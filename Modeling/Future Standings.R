# This is a modest attempt to predict the future standings of the league for the next three years based
# on the expected value added from players on the roster. It is by no means comprehensive and mainly necessary
# for valuing future draft picks.
#
# Each simulation:
#   1. samples a fresh quantile of every rostered player's (and draft pick's) projected value added
#      for the season and sums each team's top 12,
#   2. plays out the season week by week (games already played keep their real result), with
#      weekly points ~ Normal(R + team VA / season weeks, sigma) (see Future Standings Support.R),
#   3. plays the playoffs, and
#   4. uses the final standings to set the next draft's order before moving on to the next season.

suppressPackageStartupMessages(library("here"))

message("begin computing Future Standings...")

source(here("Modeling/Future Standings Support.R"))

# Settings ----------------------------------------------------------------

seed <- 123
n_sims <- 50000
n_years <- 3
n_top <- 12 # players counted per team (starting lineup plus depth for byes, injuries, and trades)

set.seed(seed)

# Load Data ---------------------------------------------------------------

load(here("Modeling/player_simulations.RData"))

current_roster <- read_csv(
  here("Data/current_roster.csv"),
  col_types = cols(player_id = "c", roster_id = "i", .default = "c")
) |>
  select(player_id, roster_id)

future_draft_picks <- read_csv(
  here("Data/future_draft_picks.csv"),
  show_col_types = FALSE
)
rookie_draft_values <- read_csv(
  here("Data/rookie_draft_values.csv"),
  show_col_types = FALSE
)
va <- read_csv(here("Data/va.csv"), show_col_types = FALSE)
matchups_table <- read_csv(
  here("Data/matchups_table.csv"),
  show_col_types = FALSE
)
replacement_levels <- read_csv(
  here("Data/replacement_levels.csv"),
  show_col_types = FALSE
)

# simulated seasons: the current season (the next one in the offseason) and the two after it
seasons <- as.numeric(names(player_simulations))[seq_len(n_years)]

# league settings of the first simulated season (or the latest cached one)
league_seasons <- list.files(
  here("Data/cache/sleeper"),
  pattern = "^league_\\d{4}\\.rds$"
) |>
  str_extract("\\d{4}") |>
  as.numeric()
fmt <- read_league_cache(max(league_seasons[league_seasons <= seasons[1]])) |>
  league_format()

# Points Model -------------------------------------------------------------

points_model <- fit_points_model(matchups_table, replacement_levels, fmt)
report_points_model(points_model, matchups_table, va, fmt, n_top)

# Simulation Inputs --------------------------------------------------------

rookie <- rookie_matrices(rookie_draft_values)

picks <- future_draft_picks |>
  filter(season %in% seasons)
if (
  nrow(picks) > 0 &&
    max(picks$round) * fmt$num_teams > nrow(rookie[["first year"]])
) {
  stop("rookie_draft_values.csv does not cover round ", max(picks$round))
}

# order of the first simulated draft if it is not set yet (offseason)
orders <- list()
if (any(is.na(picks$draft_order[picks$season == seasons[1]]))) {
  orders[[as.character(seasons[1])]] <- realized_draft_order(
    matchups_table,
    seasons[1] - 1,
    fmt
  )
}

inputs <- list(
  seasons = seasons,
  fmt = fmt,
  model = points_model,
  base = season_base(points_model, seasons),
  n_top = n_top,
  schedules = build_schedules(matchups_table, va, seasons, fmt),
  players = map(
    player_simulations[seq_len(n_years)],
    \(d) player_assets(d, current_roster)
  ),
  picks = picks,
  rookie = rookie,
  orders = orders
)

# Run Seasons --------------------------------------------------------------

message("begin ", n_sims, " future standings simulations...")

plan(multisession, workers = max(1, parallelly::availableCores(omit = 1)))

sims <- future_map(
  seq_len(n_sims),
  \(i) simulate_future(inputs),
  .options = furrr_options(seed = seed)
)

plan(sequential)

final_standings_odds <- summarize_standings(sims, seasons, fmt)

write_csv(final_standings_odds, here("Data/final_standings_odds.csv"))
