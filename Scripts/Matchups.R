# Matchups
# every completed game with team names and the outcome, for the Matchups page
# (team_matchup_record() in Shiny/Script Support.R builds the head-to-head table), and
# each team's Elo rating after every week (the Team Rankings page, and the Elo half of the
# Current Matchups win probability, so this runs before Future Standings)
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Matchups...")

matchups_table <- read_csv(
  here("Data/derived/matchups_table.csv"),
  show_col_types = FALSE
)

users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, display_name)

# Completed weeks ---------------------------------------------------------------
# a fantasy week is complete once every NFL game of that week has a final score
# (Scraping.R decides), so a run in the middle of a week never scores games in progress

complete_weeks <- read_csv(
  here("Data/derived/completed_weeks.csv"),
  show_col_types = FALSE
)

# Records -----------------------------------------------------------------------

team_records_df <- matchups_table |>
  semi_join(complete_weeks, by = join_by(season, week)) |>
  filter(points > 0 | opp_points > 0) |> # unplayed playoff games score 0
  left_join(users, by = join_by(roster_id)) |>
  rename(team = display_name) |>
  left_join(users, by = join_by(opponent_id == roster_id)) |>
  rename(opponent = display_name) |>
  mutate(outcome = if_else(points > opp_points, "win", "loss"))

# Example (app function in Shiny/Script Support.R) ------------------------------
# team_matchup_record(team_records_df, "Arvs", "regular season", shiny = TRUE)

# upcoming matchups: not sure this is worth it yet
# yearly_record <- team_records_df |>
#   summarize(
#     wins = sum(outcome == "win"),
#     losses = sum(outcome == "loss"),
#     .by = c(season, team)
#   )
#
# matchups_table_names |> filter(points == 0) |>
#   slice_min(week) |>
#   rename(team1 = team,
#          team2 = opponent) |>
#   left_join(yearly_record, by = join_by(season, team1 == team))

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

# ratings after every completed week, plus week 0 (the rating entering each season, for
# each of seasons), in long format: season, week, roster_id, elo
compute_elo <- function(
  team_records_df,
  init_table,
  seasons = NULL,
  K = elo_k,
  lambda = elo_carryover
) {
  team_weeks <- team_records_df |>
    distinct(season, week, roster_id, points) |>
    arrange(season, week)

  ratings <- set_names(init_table$elo, init_table$roster_id)
  snapshot <- function(ratings, season, week) {
    tibble(season, week, roster_id = as.numeric(names(ratings)), elo = unname(ratings))
  }

  history <- list()
  for (s in sort(unique(c(team_weeks$season, seasons)))) {
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

# every season on the schedule gets its week 0 rating, even before its first game, so the
# ratings going into any week are known (the Current Matchups win probability)
weekly_elo <- compute_elo(team_records_df, elo_init, seasons = unique(matchups_table$season)) |>
  left_join(users, by = join_by(roster_id)) |>
  transmute(
    team = display_name,
    date = str_c(season %% 100, "w", week),
    elo,
    date_hide = season + (week - 1) / 18, # x position: weeks in order, week 0 before week 1
    roster_id,
    season,
    week
  )

# example
# graph_elo(weekly_elo)

# dfs to save -------------------------------------------------------------

write_csv(team_records_df, here("Shiny/Saved Files/team_records_df.csv"))
write_csv(weekly_elo, here("Shiny/Saved Files/weekly_elo.csv"))
