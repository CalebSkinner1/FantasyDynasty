# Current Matchups
# tables behind the Current Matchups page: this week's games, each team's chance to win
# (the future standings model and Elo), and its playoff odds now, with a win, and with a
# loss; and how the season finishes after each combination of the week's results (the
# page's Playoff Machine). From Modeling/Future Standings.R's simulations of this week
# (week_games.csv, week_scenarios.csv) and Team Rankings.R's Elo ratings, so it runs
# after both. Both tables are empty when no regular-season week is left.
#   current_matchups.csv    one row per team: season, week, game, side (a or b; scenario bit j is 1
#                           when side a won game j + 1), team, opponent, model_win, elo_win,
#                           playoffs_now, playoffs_win, playoffs_loss (percents), stakes (the
#                           game's playoff swing, by which the games are ordered)
#   matchup_scenarios.csv   scenario, team, finish, n (simulations)
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Current Matchups...")

users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, team = display_name)
week_games <- read_csv(
  here("Data/model_output/week_games.csv"),
  col_types = cols(.default = col_double())
)
week_scenarios <- read_csv(
  here("Data/model_output/week_scenarios.csv"),
  col_types = cols(.default = col_integer())
) |>
  left_join(users, by = join_by(roster_id))
weekly_elo <- read_csv(here("Shiny/Saved Files/weekly_elo.csv"), show_col_types = FALSE)

# playoff spots (from the latest cached Sleeper league settings)
league_files <- list.files(
  here("Data/raw/sleeper"),
  pattern = "^league_\\d{4}\\.rds$",
  full.names = TRUE
)
playoff_teams <- if (length(league_files) > 0) {
  as.numeric(readRDS(max(league_files))$data$settings$playoff_teams %||% 6)
} else {
  6
}

# Games -------------------------------------------------------------------------
# one row per team, with its side of the game (a or b)

latest_elo <- weekly_elo |>
  slice_max(date_hide, n = 1, with_ties = FALSE, by = team) |>
  select(team, elo)

sides <- bind_rows(
  week_games |>
    transmute(season, week, game, side = "a", roster_id = a, opponent_id = b, model_win = a_win),
  week_games |>
    transmute(season, week, game, side = "b", roster_id = b, opponent_id = a, model_win = 1 - a_win)
) |>
  left_join(users, by = join_by(roster_id)) |>
  left_join(users |> rename(opponent = team), by = join_by(opponent_id == roster_id)) |>
  left_join(latest_elo, by = join_by(team)) |>
  left_join(latest_elo |> rename(opponent = team, opponent_elo = elo), by = join_by(opponent)) |>
  # Elo's expected result (Team Rankings.R)
  mutate(elo_win = 1 / (1 + 10^((opponent_elo - elo) / 400)))

# Playoff odds ------------------------------------------------------------------
# share of simulations in which the team made the playoffs: in all of them, in those it
# won its game, and in those it lost. A scenario's bit for the game says whether side a won.

team_won <- function(scenario, game, side) {
  a_won <- bitwAnd(scenario, 2L^(game - 1)) > 0
  if_else(side == "a", a_won, !a_won)
}

playoff_odds <- week_scenarios |>
  inner_join(sides |> select(team, game, side), by = join_by(team)) |>
  mutate(won = team_won(scenario, game, side), playoffs = finish <= playoff_teams) |>
  summarize(
    playoffs_now = sum(n[playoffs]) / sum(n),
    playoffs_win = sum(n[playoffs & won]) / sum(n[won]),
    playoffs_loss = sum(n[playoffs & !won]) / sum(n[!won]),
    .by = team
  )

current_matchups <- sides |>
  left_join(playoff_odds, by = join_by(team)) |>
  transmute(
    season,
    week,
    game,
    side,
    team,
    opponent,
    # a result no simulation had (0 / 0) is left blank
    across(
      c(model_win, elo_win, playoffs_now, playoffs_win, playoffs_loss),
      \(x) if_else(is.nan(x), NA, round(100 * x, 1))
    )
  ) |>
  # games with the most at stake first: the two teams' playoff swings (win minus loss)
  mutate(stakes = round(sum(playoffs_win - playoffs_loss, na.rm = TRUE), 1), .by = game) |>
  arrange(desc(stakes), game, side)

# dfs to save -------------------------------------------------------------

write_csv(current_matchups, here("Shiny/Saved Files/current_matchups.csv"))
write_csv(
  week_scenarios |> select(scenario, team, finish, n),
  here("Shiny/Saved Files/matchup_scenarios.csv")
)
