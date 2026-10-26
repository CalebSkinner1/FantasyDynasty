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
#
# future_standings_at() (Future Standings Support.R) runs these simulations for the league as it
# stood on any date. It runs for today (final_standings_odds.csv, used by Pick Values and the app)
# and for each past date with saved rosters and picks (Scraping.R) and player simulations
# (Player Total Value.R), in final_standings_odds_time.csv (the same columns plus the date).

suppressPackageStartupMessages(library("here"))

message("begin computing Future Standings...")

source(here("Modeling/Future Standings Support.R"))

# Settings ----------------------------------------------------------------

seed <- 123
n_sims <- 50000
n_years <- 3
n_top <- 12 # players counted per team (starting lineup plus depth for byes, injuries, and trades)

# past dates: FALSE adds only the dates missing from final_standings_odds_time.csv (normally
# just the latest); TRUE recomputes every date (about 15 seconds each), e.g. after changing
# the model or recomputing the player simulations. The points model itself has nothing to
# retrain: it is two averages over the games played, fit every run in a fraction of a second.
recompute_standings_time <- getOption("fantasy.recompute_standings_time", FALSE)

set.seed(seed)

# Load Data ---------------------------------------------------------------

player_simulations <- readRDS(here("Modeling/models/player_simulations.rds"))

current_roster <- read_csv(
  here("Data/derived/current_roster.csv"),
  col_types = cols(player_id = "c", roster_id = "i", .default = "c")
) |>
  select(player_id, roster_id)

future_draft_picks <- read_csv(
  here("Data/derived/future_draft_picks.csv"),
  show_col_types = FALSE
)
rookie_draft_values <- read_csv(
  here("Data/model_output/rookie_draft_values.csv"),
  show_col_types = FALSE
)
va <- read_csv(here("Data/derived/va.csv"), show_col_types = FALSE)
matchups_table <- read_csv(
  here("Data/derived/matchups_table.csv"),
  show_col_types = FALSE
)
replacement_levels <- read_csv(
  here("Data/derived/replacement_levels.csv"),
  show_col_types = FALSE
)
season_dates <- read_csv(
  here("Shiny/Saved Files/season_dates.csv"),
  show_col_types = FALSE
)

# Points Model -------------------------------------------------------------
# fit on every game played so far and used for every date

fmt <- league_format_for(as.numeric(names(player_simulations))[1])
points_model <- fit_points_model(matchups_table, replacement_levels, fmt)
report_points_model(points_model, matchups_table, va, fmt, n_top)

# inputs shared by every date
league <- list(
  model = points_model,
  rookie = rookie_matrices(rookie_draft_values),
  matchups_table = matchups_table,
  va = va,
  season_dates = season_dates
)

plan(multisession, workers = max(1, parallelly::availableCores(omit = 1)))

# Today --------------------------------------------------------------------

message("begin ", n_sims, " future standings simulations...")

final_standings_odds <- future_standings_at(
  today(),
  list(roster = current_roster, picks = future_draft_picks),
  player_simulations,
  league,
  n_sims = n_sims,
  seed = seed,
  n_years = n_years,
  n_top = n_top
)

write_csv(final_standings_odds, here("Data/model_output/final_standings_odds.csv"))

# Over Time ----------------------------------------------------------------
# every date with both its rosters and picks and its player simulations. Saved dates are
# kept unless recompute_standings_time is TRUE (today's is always redone).

rosters_time <- read_csv(
  here("Data/derived/rosters_time.csv"),
  col_types = cols(date = col_date(), player_id = col_character(), .default = col_integer())
)
picks_time <- read_csv(
  here("Data/derived/picks_time.csv"),
  col_types = cols(date = col_date(), season = col_double(), .default = col_integer())
)

simulations_dir <- here("Modeling/models/simulations")
simulation_dates <- list.files(simulations_dir, pattern = "^\\d{4}-\\d{2}-\\d{2}\\.rds$") |>
  str_remove("\\.rds$") |>
  ymd()

roster_dates <- sort(unique(rosters_time$date))
dates <- roster_dates[roster_dates %in% simulation_dates]
if (length(dates) < length(roster_dates)) {
  message(
    length(roster_dates) - length(dates), " date(s) have rosters but no saved player ",
    "simulations (run Player Total Value.R with fantasy.recompute_future_value_time = TRUE)"
  )
}

odds_time_path <- here("Data/model_output/final_standings_odds_time.csv")
standings_time <- if (!recompute_standings_time && file.exists(odds_time_path)) {
  read_csv(odds_time_path, show_col_types = FALSE) |>
    filter(date != today())
} else {
  NULL
}
new_dates <- dates[!dates %in% standings_time$date]

for (i in seq_along(new_dates)) {
  date <- new_dates[i]
  message("standings on ", date, " (", i, " of ", length(new_dates), ")")

  odds <- if (date == today()) {
    final_standings_odds # same rosters, picks, and simulations as today's run above
  } else {
    future_standings_at(
      date,
      list(
        roster = rosters_time |> filter(date == !!date) |> select(player_id, roster_id),
        picks = picks_time |> filter(date == !!date) |> select(-date)
      ),
      readRDS(file.path(simulations_dir, str_c(date, ".rds"))),
      league,
      n_sims = n_sims,
      seed = seed,
      n_years = n_years,
      n_top = n_top
    )
  }

  standings_time <- bind_rows(standings_time, mutate(odds, date = date, .before = 1))
}

plan(sequential)

if (!is.null(standings_time)) {
  standings_time |>
    arrange(date, season, type, roster_id, result) |>
    write_csv(odds_time_path)
}
