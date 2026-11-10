# Current Matchups
# tables behind the Current Matchups page: this week's games, each team's chance to win,
# and its playoff odds now, with a win, and with a loss; and how the season finishes after
# each combination of the week's results (the page's Playoff Machine). From
# Modeling/Future Standings.R's simulations of this week (week_games.csv,
# week_scenarios.csv), so it runs after it. Both tables are empty when no regular-season
# week is left.
#
# The chance to win is the blend of the Future Standings model and Elo (Win Probability
# Blend.R). Each combination of this week's results (a scenario) carries a weight so the
# weighted simulations win each game at that blended chance (Future Standings.R), and the
# playoff odds count the simulations with those weights.
#   current_matchups.csv    one row per team: season, week, game, side (a or b; scenario bit
#                           j is 1 when side a won game j + 1), team, opponent, win (the
#                           blended chance shown), model_win and elo_win (its two parts),
#                           playoffs_now, playoffs_win, playoffs_loss (percents), stakes
#                           (the game's playoff swing, by which the games are ordered)
#   matchup_scenarios.csv   scenario, team, finish, n (simulations), weight (the scenario's)
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
  col_types = cols(weight = col_double(), .default = col_integer())
) |>
  left_join(users, by = join_by(roster_id))

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

sides <- bind_rows(
  week_games |>
    transmute(
      season, week, game, side = "a", roster_id = a, opponent_id = b,
      win = a_win, model_win = model_a_win, elo_win = elo_a_win
    ),
  week_games |>
    transmute(
      season, week, game, side = "b", roster_id = b, opponent_id = a,
      win = 1 - a_win, model_win = 1 - model_a_win, elo_win = 1 - elo_a_win
    )
) |>
  left_join(users, by = join_by(roster_id)) |>
  left_join(users |> rename(opponent = team), by = join_by(opponent_id == roster_id))

# Playoff odds ------------------------------------------------------------------
# weighted share of simulations in which the team made the playoffs: in all of them, in
# those it won its game, and in those it lost. A scenario's bit for the game says whether
# side a won.

team_won <- function(scenario, game, side) {
  a_won <- bitwAnd(scenario, 2L^(game - 1)) > 0
  if_else(side == "a", a_won, !a_won)
}

playoff_odds <- week_scenarios |>
  inner_join(sides |> select(team, game, side), by = join_by(team)) |>
  mutate(
    won = team_won(scenario, game, side),
    playoffs = finish <= playoff_teams,
    mass = n * weight
  ) |>
  summarize(
    playoffs_now = sum(mass[playoffs]) / sum(mass),
    playoffs_win = sum(mass[playoffs & won]) / sum(mass[won]),
    playoffs_loss = sum(mass[playoffs & !won]) / sum(mass[!won]),
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
      c(win, model_win, elo_win, playoffs_now, playoffs_win, playoffs_loss),
      \(x) if_else(is.nan(x), NA, round(100 * x, 1))
    )
  ) |>
  # games with the most at stake first: the two teams' playoff swings (win minus loss)
  mutate(stakes = round(sum(playoffs_win - playoffs_loss, na.rm = TRUE), 1), .by = game) |>
  arrange(desc(stakes), game, side)

# dfs to save -------------------------------------------------------------

write_csv(current_matchups, here("Shiny/Saved Files/current_matchups.csv"))
write_csv(
  week_scenarios |> select(scenario, team, finish, n, weight),
  here("Shiny/Saved Files/matchup_scenarios.csv")
)
