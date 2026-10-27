# Pick Values
# expected value of every rookie draft pick the grades and team pages use, written to
# Data/model_output/all_draft_pick_exp_values.csv (read by Trade Grades and Fantasy Teams):
#   used picks:    picks already made in a completed rookie draft, valued by pick number
#   future picks:  by pick number once the draft order is set; until then, averaged over
#                  the simulated final standings (Future Standings.R)
# Columns:
#   exp_total_value  expected total value of the pick (mean of the rookie pick model,
#                    Draft Pick Value.R)
#   future_value     exp_total_value discounted .95 per season until the pick's draft: a
#                    pick in the next rookie draft is worth .95, two drafts out .95^2.
#                    Used picks are not discounted.
# Also writes the same values for each past date (pick_values_time.csv, see the end).
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

source(here("Scripts/Over Time Support.R")) # draft_finished_dates()

message("begin computing Pick Values...")

discount_rate <- .95

# load data
users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE)
num_teams <- nrow(users)

drafts <- read_csv(
  here("Data/derived/drafts.csv"),
  show_col_types = FALSE,
  col_types = cols(draft_id = col_character())
)
draft_picks <- readRDS(here("Data/derived/draft_picks.rds")) |>
  bind_rows() |>
  select(draft_id, round, pick_no, draft_slot)
draft_order <- read_csv(here("Data/derived/draft_order.csv"), show_col_types = FALSE)
future_draft_picks <- read_csv(
  here("Data/derived/future_draft_picks.csv"),
  show_col_types = FALSE
)
final_standings_odds <- read_csv(
  here("Data/model_output/final_standings_odds.csv"),
  show_col_types = FALSE
)

# mean value of each pick number (mean of the model's simulated quantiles)
pick_values <- read_csv(
  here("Data/model_output/rookie_draft_values.csv"),
  show_col_types = FALSE
) |>
  filter(metric == "total value") |>
  mutate(exp_total_value = rowMeans(pick(starts_with("proj_tva_")))) |>
  select(pick_no, exp_total_value)

completed_rookie_drafts <- drafts |>
  filter(type == "rookie", status == "complete")
last_rookie_draft <- max(completed_rookie_drafts$season)

# Used picks ---------------------------------------------------------------
# pick_slot is the team the pick originally belonged to

used_picks <- draft_picks |>
  inner_join(
    completed_rookie_drafts |> select(draft_id, season),
    by = join_by(draft_id)
  ) |>
  left_join(
    draft_order |>
      filter(type == "rookie") |>
      select(season, draft_slot = draft_order, pick_slot = roster_id),
    by = join_by(season, draft_slot)
  ) |>
  left_join(pick_values, by = join_by(pick_no)) |>
  select(season, round, pick_slot, exp_total_value)

# Future picks ---------------------------------------------------------------

# fallback for a draft the standings don't reach: the round's average pick
round_values <- pick_values |>
  mutate(round = (pick_no - 1) %/% num_teams + 1) |>
  summarize(round_value = mean(exp_total_value), .by = round)

# expected value of future picks (season, round, pick_slot, draft_order) given the odds of
# each final standing: by pick number once the draft order is set, otherwise averaged over
# the standings (the last-place team picks first; standings in season s set the order of
# the s + 1 draft). Picks beyond the simulated standings get their round's average pick.
future_pick_values <- function(picks, standings_odds, quiet = FALSE) {
  standings_values <- standings_odds |>
    filter(type == "rank") |>
    expand_grid(round = seq_len(max(picks$round))) |>
    mutate(
      pick_no = (round - 1) * num_teams + (num_teams + 1 - result),
      season = season + 1
    ) |>
    left_join(pick_values, by = join_by(pick_no)) |>
    summarize(
      standings_value = sum(perc * exp_total_value),
      .by = c(season, round, roster_id)
    ) |>
    rename(pick_slot = roster_id)

  future_picks <- picks |>
    distinct(season, round, pick_slot, draft_order) |>
    mutate(pick_no = (round - 1) * num_teams + draft_order) |>
    left_join(pick_values, by = join_by(pick_no)) |> # draft order set
    left_join(standings_values, by = join_by(season, round, pick_slot)) |>
    left_join(round_values, by = join_by(round))

  n_fallback <- sum(is.na(future_picks$exp_total_value) & is.na(future_picks$standings_value))
  if (n_fallback > 0 && !quiet) {
    message(
      "Note: ", n_fallback, " future picks are beyond the simulated standings and are ",
      "valued at their round's average pick"
    )
  }

  future_picks |>
    mutate(exp_total_value = coalesce(exp_total_value, standings_value, round_value)) |>
    select(season, round, pick_slot, exp_total_value)
}

# future_value: exp_total_value discounted .95 per season after the last rookie draft
discount_picks <- function(pick_values, last_rookie_draft) {
  pick_values |>
    mutate(
      future_value = exp_total_value *
        discount_rate^pmax(season - last_rookie_draft, 0)
    )
}

# dfs to save -------------------------------------------------------------

all_draft_pick_exp_values <- bind_rows(
  used_picks,
  future_pick_values(future_draft_picks, final_standings_odds)
) |>
  discount_picks(last_rookie_draft) |>
  arrange(season, round, pick_slot)

write_csv(
  all_draft_pick_exp_values,
  here("Data/model_output/all_draft_pick_exp_values.csv")
)

# Pick values over time ------------------------------------------------------
# every unused pick on each date with future standings odds (Future Standings.R), valued
# with that date's odds and draft order (Scraping.R's picks_time.csv) and discounted from
# the last rookie draft finished by then. Written to Data/model_output/pick_values_time.csv
# with the pick's owner that date (roster_id); the latest date equals today's values.
# Rebuilt every run (a few seconds).

standings_time_path <- here("Data/model_output/final_standings_odds_time.csv")

if (file.exists(standings_time_path)) {
  standings_time <- read_csv(standings_time_path, show_col_types = FALSE)
  picks_time <- read_csv(
    here("Data/derived/picks_time.csv"),
    col_types = cols(date = col_date(), season = col_double(), .default = col_integer())
  )
  rookie_drafts_finished <- draft_finished_dates() |>
    filter(type == "rookie")

  dates <- sort(unique(standings_time$date))
  dates <- dates[dates %in% picks_time$date]

  pick_values_time <- map(dates, \(d) {
    picks <- picks_time |> filter(date == d)
    last_draft <- max(c(
      rookie_drafts_finished$season[rookie_drafts_finished$finished <= d],
      min(drafts$season) - 1
    ))

    future_pick_values(picks, standings_time |> filter(date == d), quiet = TRUE) |>
      discount_picks(last_draft) |>
      left_join(
        picks |> select(season, round, pick_slot, roster_id),
        by = join_by(season, round, pick_slot)
      ) |>
      transmute(date = d, season, round, pick_slot, roster_id, exp_total_value, future_value)
  }) |>
    bind_rows()

  write_csv(pick_values_time, here("Data/model_output/pick_values_time.csv"))
}
