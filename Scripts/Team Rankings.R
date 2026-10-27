# Team Rankings
# weekly Elo ratings for every team, each team's peak and low, and the total future value
# of each team's assets, for the Team Rankings page. Uses the completed games from
# Matchups.R, so it runs after it.
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Team Rankings...")

team_records_df <- read_csv(
  here("Shiny/Saved Files/team_records_df.csv"),
  show_col_types = FALSE
)
grab_team_assets_df <- read_csv(
  here("Shiny/Saved Files/grab_team_assets_df.csv"),
  show_col_types = FALSE
)
users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, display_name)

# Elo Rankings ------------------------------------------------------------
# Each week every team is compared with every other team that played that week (as if it
# had played the whole league), so the schedule doesn't matter. Against one opponent the
# rating changes by
#   K * mov * (S - E)
#   S   = 1 for a win, 0 for a loss (.5 for a tie)
#   E   = 1 / (1 + 10^((opp_elo - elo) / 400)), the expected result given the ratings
#   mov = log(|point difference| + 1) * 2.2 / ((winner's elo - loser's elo) * .001 + 2.2),
#         larger for bigger wins, damped when the favorite wins (538's NFL Elo)
# and a team's weekly change is the average over its opponents. A team that wins as
# expected gains little and an upset moves both ratings a lot, so ratings settle around
# each team's true strength. At the start of each season ratings regress toward 1500.

elo_k <- 20 # how fast ratings move
elo_carryover <- .75 # share of a team's distance from 1500 kept into the next season

# one week: ratings is a named vector (names = roster_id); week_points has roster_id, points
elo_week <- function(ratings, week_points, K) {
  changes <- week_points |>
    cross_join(week_points |> rename(opponent_id = roster_id, opp_points = points)) |>
    filter(roster_id != opponent_id) |>
    mutate(
      elo = ratings[as.character(roster_id)],
      opp_elo = ratings[as.character(opponent_id)],
      point_diff = points - opp_points,
      result = case_when(point_diff > 0 ~ 1, point_diff < 0 ~ 0, .default = .5),
      expected = 1 / (1 + 10^((opp_elo - elo) / 400)),
      winner_gap = if_else(point_diff >= 0, elo - opp_elo, opp_elo - elo),
      mov = log(abs(point_diff) + 1) * 2.2 / (winner_gap * .001 + 2.2),
      change = K * mov * (result - expected)
    ) |>
    summarize(change = mean(change), .by = roster_id)

  ids <- as.character(changes$roster_id)
  ratings[ids] <- ratings[ids] + changes$change
  ratings
}

# ratings after every completed week, plus week 0 (the rating entering each season), in
# long format: season, week, roster_id, elo
compute_elo <- function(team_records_df, init_table, K = elo_k, lambda = elo_carryover) {
  team_weeks <- team_records_df |>
    distinct(season, week, roster_id, points) |>
    arrange(season, week)

  ratings <- set_names(init_table$elo, init_table$roster_id)
  snapshot <- function(ratings, season, week) {
    tibble(season, week, roster_id = as.numeric(names(ratings)), elo = unname(ratings))
  }

  history <- list()
  for (s in sort(unique(team_weeks$season))) {
    if (length(history) > 0) {
      ratings <- lambda * ratings + (1 - lambda) * 1500 # new season: regress toward 1500
    }
    history <- c(history, list(snapshot(ratings, s, 0)))

    season_weeks <- team_weeks |> filter(season == s)
    for (w in sort(unique(season_weeks$week))) {
      ratings <- elo_week(ratings, season_weeks |> filter(week == w), K)
      history <- c(history, list(snapshot(ratings, s, w)))
    }
  }

  bind_rows(history)
}

elo_init <- users |> transmute(roster_id, elo = 1500)

weekly_elo <- compute_elo(team_records_df, elo_init) |>
  left_join(users, by = join_by(roster_id)) |>
  transmute(
    team = display_name,
    date = str_c(season %% 100, "w", week),
    elo,
    date_hide = season + (week - 1) / 18 # x position: weeks in order, week 0 before week 1
  )

# example
# graph_elo(weekly_elo)

# peak and low ratings and the week each happened, and the composite rating: the average
# distance from 1500 after each played week (the week 0 starting ratings are left out).
# peak_order and low_order (the week's position on the Elo chart) let the app sort the
# week columns by when they happened; the app hides them.
when <- function(date) {
  str_c("20", str_sub(date, 1, 2), " week ", str_remove(date, "^\\d+w"))
}

total_success <- weekly_elo |>
  summarize(
    composite_rating = round(mean(elo[!str_detect(date, "w0$")] - 1500), digits = 2),
    peak = round(max(elo), 1),
    peak_week = when(date[which.max(elo)]),
    low = round(min(elo), 1),
    low_week = when(date[which.min(elo)]),
    peak_order = date_hide[which.max(elo)],
    low_order = date_hide[which.min(elo)],
    .by = team
  ) |>
  arrange(desc(composite_rating))

# Future Value Assets -----------------------------------------------------

all_assets_summary_df <- grab_team_assets_df |>
  summarize(total_future_value = sum(future_value), .by = roster_id) |>
  left_join(users, by = join_by(roster_id)) |>
  arrange(desc(total_future_value)) |>
  select(team = display_name, total_future_value)

# dfs to save -------------------------------------------------------------

write_csv(weekly_elo, here("Shiny/Saved Files/weekly_elo.csv"))
write_csv(
  all_assets_summary_df,
  here("Shiny/Saved Files/all_assets_summary_df.csv")
)
write_csv(total_success, here("Shiny/Saved Files/total_success.csv"))
