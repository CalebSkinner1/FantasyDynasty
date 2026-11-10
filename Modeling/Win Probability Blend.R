# Win Probability Blend
# learns w, the weight on the Future Standings model in this week's win probability on the
# Current Matchups page:
#   p = w * Future Standings model + (1 - w) * Elo
# w (between 0 and 1) is the one that best predicted every past regular-season game both
# models could have predicted before kickoff: the lowest log loss, the average of
# -log(the chance given to what happened).
#
# Each game's two predictions:
#   Elo    each team's rating going into the week (weekly_elo.csv, Matchups.R)
#   model  the share of simulations team a won, simulated as the league stood on the
#          latest date before kickoff with saved rosters, picks, and player simulations
#          (first_season_at(), Future Standings Support.R). Future Standings.R records each
#          week's prediction as made before kickoff (win_prob_live.csv), which is used
#          instead once it exists; the rest are rebuilt here once and saved
#          (win_prob_rebuilt.csv). The rebuilt ones use today's points model (fit on every
#          game played), so they have seen a little of the games they predict; the
#          recorded ones haven't.
#
# Writes to Data/model_output:
#   win_prob_rebuilt.csv  season, week, a, b, model_a_win, date: the rebuilt predictions
#   win_prob_games.csv    season, week, a, b, model_a_win, elo_a_win, a_won, source
#                         (rebuilt or recorded): every game used
#   win_prob_blend.csv  w, its 90% range (refit on the weeks resampled), the number of
#                       games, and each model's log loss alone and blended
# Runs after Matchups.R (Elo) and Player Total Value.R (today's player simulations). For
# now Future Standings.R uses w = 0.5 rather than this fit, which isn't stable with only two
# seasons of games; this keeps the predictions and the fit up to date for when there are
# more. Only weeks not yet saved are simulated, so after the first run (a minute or two) it
# takes seconds.

suppressPackageStartupMessages(library("here"))

message("begin computing the Win Probability Blend...")

source(here("Modeling/Future Standings Support.R"))

seed <- 123
n_sims <- 20000 # per rebuilt week: enough for each game's chance to within about half a point
n_boot <- 1000

# Load Data ---------------------------------------------------------------

weekly_elo <- read_csv(here("Shiny/Saved Files/weekly_elo.csv"), show_col_types = FALSE)
completed_weeks <- read_csv(here("Data/derived/completed_weeks.csv"), show_col_types = FALSE)

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
dates <- intersect(unique(rosters_time$date), simulation_dates) |>
  as_date() |>
  sort()

league <- standings_league(year(today()))
fmt <- league$fmt

prediction_cols <- cols(date = col_date(), .default = col_double())
no_predictions <- tibble(
  season = double(), week = double(), a = double(), b = double(),
  model_a_win = double(), date = as_date(character())
)
rebuilt_path <- here("Data/model_output/win_prob_rebuilt.csv")
live_path <- here("Data/model_output/win_prob_live.csv")
saved <- if (file.exists(rebuilt_path)) {
  read_csv(rebuilt_path, col_types = prediction_cols)
} else {
  no_predictions
}
live <- if (file.exists(live_path)) {
  read_csv(live_path, col_types = prediction_cols)
} else {
  no_predictions
}

# Weeks to predict ---------------------------------------------------------------
# completed regular-season weeks, each predicted on the latest saved date before its
# kickoff on which the week before it was complete (its Tuesday had come). Weeks with no
# such date (e.g. the 2024 season, which has no saved player simulations) are left out.

results <- league$matchups_table |>
  semi_join(completed_weeks, by = join_by(season, week)) |>
  filter(week < fmt$playoff_week_start, roster_id < opponent_id) |>
  transmute(season, week, a = roster_id, b = opponent_id, a_won = as.numeric(points > opp_points))

weeks <- results |>
  distinct(season, week) |>
  rowwise() |>
  mutate(
    kickoff = week_kickoff(league$season_dates, season, week),
    previous_done = if (week == 1) {
      as_date("1900-01-01")
    } else {
      start <- league$season_dates$season_start[year(league$season_dates$season_start) == season]
      start + (2 - wday(start)) %% 7 + 1 + 7 * (week - 2)
    },
    date = {
      candidates <- dates[dates < kickoff & dates >= previous_done]
      if (length(candidates) > 0) max(candidates) else as_date(NA)
    }
  ) |>
  ungroup() |>
  filter(!is.na(date))

# weeks recorded live, or already rebuilt, aren't simulated again
to_rebuild <- weeks |>
  anti_join(distinct(live, season, week), by = join_by(season, week)) |>
  anti_join(distinct(saved, season, week), by = join_by(season, week))

plan(multisession, workers = max(1, parallelly::availableCores(omit = 1)))

rebuilt <- map(seq_len(nrow(to_rebuild)), \(i) {
  wk <- to_rebuild[i, ]
  message("rebuilding ", wk$season, " week ", wk$week, " (as of ", wk$date, ")")
  first <- first_season_at(
    wk$date,
    list(
      roster = rosters_time |> filter(date == wk$date) |> select(player_id, roster_id),
      picks = picks_time |> filter(date == wk$date) |> select(-date)
    ),
    readRDS(file.path(simulations_dir, str_c(wk$date, ".rds"))),
    league,
    n_sims = n_sims,
    seed = seed
  )
  if (is.null(first$games) || first$games$week[1] != wk$week || first$season != wk$season) {
    message("  skipped: the simulations on ", wk$date, " weren't for that week")
    return(NULL)
  }
  first$games |>
    transmute(season, week, a, b, model_a_win, date = wk$date)
}) |>
  bind_rows()

plan(sequential)

# saved right away, so these weeks are never simulated again
saved <- bind_rows(saved, rebuilt) |>
  arrange(season, week, a)
write_csv(saved, rebuilt_path)

# Every game's predictions ----------------------------------------------------------

elo_before <- weeks |>
  distinct(season, week) |>
  pmap(\(season, week) {
    pregame_elo(weekly_elo, season, week) |>
      mutate(season = season, week = week)
  }) |>
  bind_rows()

# a game's two teams in either order (Sleeper's matchup order isn't the roster order)
by_pair <- function(df) {
  df |>
    mutate(lo = pmin(a, b), hi = pmax(a, b), model_lo_win = if_else(a == lo, model_a_win, 1 - model_a_win)) |>
    select(season, week, lo, hi, model_lo_win)
}

model_predictions <- bind_rows(
  live |>
    by_pair() |>
    mutate(source = "recorded"),
  saved |>
    by_pair() |>
    mutate(source = "rebuilt")
) |>
  distinct(season, week, lo, hi, .keep_all = TRUE) # recorded first

win_prob_games <- results |>
  inner_join(model_predictions, by = join_by(season, week, a == lo, b == hi)) |>
  left_join(elo_before |> rename(elo_a = elo), by = join_by(season, week, a == roster_id)) |>
  left_join(elo_before |> rename(elo_b = elo), by = join_by(season, week, b == roster_id)) |>
  transmute(
    season,
    week,
    a,
    b,
    model_a_win = model_lo_win,
    elo_a_win = elo_win_chance(elo_a, elo_b),
    a_won,
    source
  ) |>
  filter(!is.na(elo_a_win)) |>
  arrange(season, week, a)

# Fit w -------------------------------------------------------------------------

log_loss <- function(p, y) {
  p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

fit_w <- function(games) {
  optimize(
    \(w) log_loss(w * games$model_a_win + (1 - w) * games$elo_a_win, games$a_won),
    c(0, 1)
  )$minimum
}

w <- fit_w(win_prob_games)

# the range of w: refit on the weeks drawn with replacement
set.seed(seed)
week_keys <- distinct(win_prob_games, season, week)
boot_w <- map_dbl(seq_len(n_boot), \(i) {
  week_keys |>
    slice_sample(prop = 1, replace = TRUE) |>
    inner_join(win_prob_games, by = join_by(season, week), relationship = "many-to-many") |>
    fit_w()
})

win_prob_blend <- tibble(
  w = w,
  w_low = quantile(boot_w, .05),
  w_high = quantile(boot_w, .95),
  games = nrow(win_prob_games),
  weeks = nrow(week_keys),
  log_loss_model = log_loss(win_prob_games$model_a_win, win_prob_games$a_won),
  log_loss_elo = log_loss(win_prob_games$elo_a_win, win_prob_games$a_won),
  log_loss_blend = log_loss(
    w * win_prob_games$model_a_win + (1 - w) * win_prob_games$elo_a_win,
    win_prob_games$a_won
  )
)

message(
  "win probability blend: w = ", round(w, 2), " (90% range ", round(win_prob_blend$w_low, 2),
  " to ", round(win_prob_blend$w_high, 2), ") from ", win_prob_blend$games, " games in ",
  win_prob_blend$weeks, " weeks; log loss model ", round(win_prob_blend$log_loss_model, 3),
  ", Elo ", round(win_prob_blend$log_loss_elo, 3), ", blend ",
  round(win_prob_blend$log_loss_blend, 3), " (a coin flip is 0.693)"
)

# dfs to save -------------------------------------------------------------

write_csv(win_prob_games, here("Data/model_output/win_prob_games.csv"))
write_csv(win_prob_blend, here("Data/model_output/win_prob_blend.csv"))
