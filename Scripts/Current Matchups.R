# Current Matchups
# tables behind the Current Matchups page: this week's games, each team's chance to win,
# and its playoff odds, championship odds, and expected draft pick now, with a win, and
# with a loss; and how the season finishes after each combination of the week's results
# (the page's Playoff Machine). From
# Modeling/Future Standings.R's simulations of this week (week_games.csv,
# week_scenarios.csv), so it runs after it. Both tables are empty when no regular-season
# week is left.
#
# The chance to win is the blend of the Future Standings model and Elo (Win Probability
# Blend.R). Each combination of this week's results (a scenario) carries a weight so the
# weighted simulations win each game at that blended chance (Future Standings.R), and the
# playoff odds count the simulations with those weights.
#
# The games are ordered by what is at stake (stakes, also the home page's Matchup of the
# Week). A game's swing in each of three things is the sum over its two teams of the gap
# between winning and losing it:
#   championship odds   50%
#   playoff odds        30%
#   draft pick value    20%  the expected value of the team's picks in next year's rookie
#                            draft (its slot in each round, valued as in Pick Values.R:
#                            the mean of the rookie pick model), which counts a pick moving
#                            near the top of the draft for more than one near the bottom
# Each swing is divided by its total over this week's games, so a game's stakes are its
# weighted share of the week's: the stakes of the week's games add to 100.
#   current_matchups.csv    one row per team: season, week, game, side (a or b; scenario bit
#                           j is 1 when side a won game j + 1), team, opponent, win (the
#                           blended chance shown), model_win and elo_win (its two parts),
#                           playoffs_now, playoffs_win, playoffs_loss, champion_now,
#                           champion_win, champion_loss (percents), pick_now, pick_win,
#                           pick_loss (expected draft slot in each round, 1 = first),
#                           stakes
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
n_teams <- nrow(users)

# the value of each draft slot: its picks in every round of the rookie draft (pick_no is
# the overall pick), valued at the mean of the rookie pick model as in Pick Values.R
slot_value <- read_csv(
  here("Data/model_output/rookie_draft_values.csv"),
  show_col_types = FALSE
) |>
  filter(metric == "total value") |>
  transmute(
    slot = (pick_no - 1) %% n_teams + 1,
    value = rowMeans(pick(starts_with("proj_tva_")))
  ) |>
  summarize(value = sum(value), .by = slot)

# what counts toward the stakes, and how much
stakes_weights <- c(champion = 0.5, playoffs = 0.3, pick = 0.2)

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

# Odds ------------------------------------------------------------------------
# weighted share of simulations in which the team made the playoffs and won the
# championship, and its expected draft slot and pick value: in all of them, in those it
# won its game, and in those it lost. A scenario's bit for the game says whether side a
# won. The draft goes in reverse order of the final finish (the champion picks last).

team_won <- function(scenario, game, side) {
  a_won <- bitwAnd(scenario, 2L^(game - 1)) > 0
  if_else(side == "a", a_won, !a_won)
}

playoff_odds <- week_scenarios |>
  inner_join(sides |> select(team, game, side), by = join_by(team)) |>
  mutate(
    won = team_won(scenario, game, side),
    playoffs = finish <= playoff_teams,
    champion = finish == 1,
    slot = n_teams + 1 - finish,
    mass = n * weight
  ) |>
  left_join(slot_value, by = join_by(slot)) |>
  summarize(
    playoffs_now = sum(mass[playoffs]) / sum(mass),
    playoffs_win = sum(mass[playoffs & won]) / sum(mass[won]),
    playoffs_loss = sum(mass[playoffs & !won]) / sum(mass[!won]),
    champion_now = sum(mass[champion]) / sum(mass),
    champion_win = sum(mass[champion & won]) / sum(mass[won]),
    champion_loss = sum(mass[champion & !won]) / sum(mass[!won]),
    pick_now = sum(mass * slot) / sum(mass),
    pick_win = sum((mass * slot)[won]) / sum(mass[won]),
    pick_loss = sum((mass * slot)[!won]) / sum(mass[!won]),
    value_win = sum((mass * value)[won]) / sum(mass[won]),
    value_loss = sum((mass * value)[!won]) / sum(mass[!won]),
    .by = team
  )

# a share of the week's total (none when nothing is at stake anywhere)
share <- function(x) if (sum(x) > 0) x / sum(x) else 0 * x

# each game's swings (a result no simulation had counts as no swing), then its stakes
stakes <- sides |>
  left_join(playoff_odds, by = join_by(team)) |>
  summarize(
    champion = sum(abs(champion_win - champion_loss), na.rm = TRUE),
    playoffs = sum(abs(playoffs_win - playoffs_loss), na.rm = TRUE),
    pick = sum(abs(value_win - value_loss), na.rm = TRUE),
    .by = game
  ) |>
  mutate(
    stakes = round(
      100 * (
        stakes_weights[["champion"]] * share(champion) +
          stakes_weights[["playoffs"]] * share(playoffs) +
          stakes_weights[["pick"]] * share(pick)
      ),
      1
    )
  ) |>
  select(game, stakes)

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
      c(
        win, model_win, elo_win, playoffs_now, playoffs_win, playoffs_loss,
        champion_now, champion_win, champion_loss
      ),
      \(x) if_else(is.nan(x), NA, round(100 * x, 1))
    ),
    across(c(pick_now, pick_win, pick_loss), \(x) if_else(is.nan(x), NA, round(x, 1)))
  ) |>
  left_join(stakes, by = join_by(game)) |>
  # games with the most at stake first
  arrange(desc(stakes), game, side)

# dfs to save -------------------------------------------------------------

write_csv(current_matchups, here("Shiny/Saved Files/current_matchups.csv"))
write_csv(
  week_scenarios |> select(scenario, team, finish, n, weight),
  here("Shiny/Saved Files/matchup_scenarios.csv")
)
