# Draft Grades
# grades every completed draft by each pick's value (realized value added plus future
# value) against the value expected from that pick:
#   rookie drafts: the rookie pick values (mean of Draft Pick Value.R's model)
#   startup draft: an expected-value curve over pick number, fitted below
# Writes
#   Shiny/Saved Files/picks_df.csv          every graded pick (app)
#   Data/derived/draft_grades_by_team.csv   each team's grade in each draft (Fantasy Teams)
#   Data/derived/draft_selections.csv       who drafted each player (Fantasy Teams)

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

source(here("Modeling/MCMC Samplers.R")) # reg_gibbs_sampler()

message("begin computing Draft Grades...")

mcmc_seed <- 2024 # seed for the startup draft's expected-value sampler

# trades made right after the startup draft: these players are graded for the team that
# traded for them
startup_draft_day_trades <- tibble(
  player_id = c("8138", "8228", "10236", "5857"), # James Cook, Jaylen Warren, Dalton Kincaid, Noah Fant
  roster_id = c(4L, 4L, 7L, 7L)
)

# load data
users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, display_name)
num_teams <- nrow(users)

drafts <- read_csv(
  here("Data/derived/drafts.csv"),
  show_col_types = FALSE,
  col_types = cols(draft_id = col_character())
)
draft_order <- read_csv(here("Data/derived/draft_order.csv"), show_col_types = FALSE)

player_info <- read_csv(
  here("Data/raw/player_info.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  distinct(player_id, .keep_all = TRUE) |>
  select(player_id, name, position)

# total value = value added to date + future value (already discounted)
player_total_value <- read_csv(
  here("Shiny/Saved Files/player_total_value.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  mutate(
    realized_value = rowSums(pick(matches("^sva_\\d{4}$"))),
    total_value = realized_value + future_value
  ) |>
  select(player_id, realized_value, future_value, total_value)

# mean value of each rookie pick number
pick_values <- read_csv(
  here("Data/model_output/rookie_draft_values.csv"),
  show_col_types = FALSE
) |>
  filter(metric == "total value") |>
  mutate(exp_total_value = rowMeans(pick(starts_with("proj_tva_")))) |>
  select(pick_no, exp_total_value)

# every pick in a completed draft, with the drafted player's value
draft_values <- readRDS(here("Data/derived/draft_picks.rds")) |>
  bind_rows() |>
  select(draft_id, pick_no, round, draft_slot, roster_id, player_id) |>
  inner_join(
    drafts |> filter(status == "complete") |> select(draft_id, season, type),
    by = join_by(draft_id)
  ) |>
  rows_update(startup_draft_day_trades, by = "player_id", unmatched = "ignore") |>
  left_join(player_info, by = join_by(player_id)) |>
  left_join(player_total_value, by = join_by(player_id)) |>
  mutate(across(c(realized_value, future_value, total_value), ~ replace_na(.x, 0))) |>
  arrange(season, desc(type), pick_no)

# Startup draft ---------------------------------------------------------------
# In the startup draft, each team's first kicker pick also set its spot in that season's
# rookie draft, so that pick is credited with the value of the rookie draft slot.

startup <- draft_values |> filter(type == "veteran")
startup_season <- unique(startup$season)

# value of each rookie draft slot: the sum of its picks over every round
slot_values <- pick_values |>
  mutate(draft_slot = (pick_no - 1) %% num_teams + 1) |>
  summarize(total_value = sum(exp_total_value), .by = draft_slot)

rookie_slot_rows <- draft_order |>
  filter(season == startup_season, type == "rookie") |>
  left_join(slot_values, by = join_by(draft_order == draft_slot)) |>
  left_join(
    startup |>
      filter(position == "K") |>
      summarize(pick_no = min(pick_no), .by = roster_id),
    by = join_by(roster_id)
  ) |>
  transmute(
    roster_id,
    pick_no,
    name = "rookie draft slot",
    position = as.character(draft_order),
    total_value
  )

startup_values <- startup |>
  select(
    roster_id,
    pick_no,
    player_id,
    name,
    position,
    realized_value,
    future_value,
    total_value
  ) |>
  bind_rows(rookie_slot_rows)

# value at each pick, with a rookie slot counted above the worst slot
startup_pick_values <- startup_values |>
  mutate(
    total_value = if_else(
      name == "rookie draft slot",
      total_value - min(slot_values$total_value),
      total_value
    )
  ) |>
  summarize(total_value = sum(total_value), .by = pick_no) |>
  arrange(pick_no)

# expected value by pick: linear + square-root regression on pick number
X_startup <- startup_pick_values |>
  transmute(
    pick_no = as.numeric(scale(pick_no)),
    pn_2 = as.numeric(scale(sqrt(startup_pick_values$pick_no))),
    intercept = 1
  ) |>
  as.matrix()

set.seed(mcmc_seed)
samples_startup <- reg_gibbs_sampler(
  Y = startup_pick_values$total_value,
  X = X_startup,
  iter = 7000,
  thin = 1,
  burn_in = 5000
)

# posterior mean of the regression line at each pick
startup_expectations <- tibble(
  pick_no = startup_pick_values$pick_no,
  exp_total_value = colMeans(samples_startup$beta %*% t(X_startup))
)

# each pick, labeled with its most valuable player (or the rookie slot)
startup_grades <- startup_values |>
  arrange(pick_no, desc(total_value)) |>
  distinct(pick_no, .keep_all = TRUE) |>
  select(-total_value) |>
  left_join(startup_pick_values, by = join_by(pick_no)) |>
  left_join(startup_expectations, by = join_by(pick_no)) |>
  mutate(
    value_over_expected = total_value - exp_total_value,
    season = startup_season,
    draft = "initial draft"
  )

# Rookie drafts ---------------------------------------------------------------

rookie_grades <- draft_values |>
  filter(type == "rookie") |>
  left_join(pick_values, by = join_by(pick_no)) |>
  mutate(
    value_over_expected = total_value - exp_total_value,
    draft = str_c(season, " rookie draft")
  ) |>
  select(
    roster_id,
    pick_no,
    player_id,
    name,
    position,
    realized_value,
    future_value,
    total_value,
    exp_total_value,
    value_over_expected,
    season,
    draft
  )

# dfs to save -------------------------------------------------------------

# best picks
picks_df <- bind_rows(startup_grades, rookie_grades) |>
  relocate(draft, season, .after = last_col())

write_csv(picks_df, here("Shiny/Saved Files/picks_df.csv"))

# each team's grade in each draft. The startup draft is graded against the league
# average; rookie drafts against each pick's expected value
draft_grades_by_team <- bind_rows(
  startup_values |>
    summarize(
      total_value = sum(total_value),
      total_realized_value = sum(realized_value, na.rm = TRUE),
      total_future_value = sum(future_value, na.rm = TRUE),
      .by = roster_id
    ) |>
    mutate(
      value_over_expected = total_value - mean(total_value),
      avenue = "initial draft"
    ),
  rookie_grades |>
    summarize(
      value_over_expected = sum(value_over_expected),
      total_realized_value = sum(realized_value),
      total_future_value = sum(future_value),
      .by = c(season, roster_id)
    ) |>
    mutate(avenue = str_c("rookie draft ", season))
) |>
  select(
    roster_id,
    value_over_expected,
    total_realized_value,
    total_future_value,
    avenue
  )

write_csv(
  draft_grades_by_team,
  here("Data/derived/draft_grades_by_team.csv")
)

# who drafted each player
draft_selections <- draft_values |>
  transmute(
    roster_id,
    player_id,
    season,
    avenue = if_else(
      type == "veteran",
      str_c("initial draft pick ", pick_no),
      str_c(season, " rookie draft pick ", pick_no)
    )
  )

write_csv(draft_selections, here("Data/derived/draft_selections.csv"))

# Examples (app functions in Shiny/Script Support.R) ------------------------------
# draft_rankings("2025 rookie draft", shiny = TRUE)
# best_picks("initial draft", shiny = TRUE)
# worst_picks("2024 rookie draft", shiny = TRUE)
