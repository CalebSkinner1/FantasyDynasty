# Future Standings Support
# this holds functions utilized in Future Standings.R
#
# Points model: a team's score in a week is the league's replacement-level lineup plus the
# team's season value added spread over the weeks of the season, plus weekly noise,
#   points ~ Normal(R_season + team_va / season_weeks, sigma)
# team_va: sum of the team's top 12 sampled projected value added (players and draft picks)
# R_season: summed replacement level of the starting lineup (Data/derived/replacement_levels.csv)
# sigma: week-to-week standard deviation of a team's score within a season

suppressPackageStartupMessages({
  library("tidyverse")
  library("furrr")
})

# league format ------------------------------------------------------------

read_league_cache <- function(season, cache_dir = here("Data/raw/sleeper")) {
  path <- file.path(cache_dir, str_c("league_", season, ".rds"))
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)$data
}

# number of teams, playoff weeks, and lineup from the cached Sleeper league settings
league_format <- function(league) {
  setting <- function(name, default) {
    value <- league$settings[[name]]
    if (is.null(value) || is.na(value)) default else as.numeric(value)
  }

  fmt <- list(
    num_teams = setting("num_teams", 12),
    playoff_week_start = setting("playoff_week_start", 15),
    playoff_teams = setting("playoff_teams", 6),
    roster_positions = league$roster_positions
  )

  if (fmt$playoff_teams != 6) {
    stop(
      "simulate_season() plays a 6-team bracket (top 2 seeds on bye), but the league has ",
      fmt$playoff_teams, " playoff teams"
    )
  }
  if (setting("playoff_round_type", 0) != 0) {
    stop("playoff rounds longer than one week are not handled")
  }

  # last fantasy week: regular season + one week per playoff round
  fmt$last_week <- fmt$playoff_week_start - 1 + ceiling(log2(fmt$playoff_teams))
  fmt
}

# points model -------------------------------------------------------------

fit_points_model <- function(matchups_table, replacement_levels, fmt) {
  # starting lineup, named as in replacement_levels.csv
  slot_names <- c(DEF = "DST", FLEX = "flex", SUPER_FLEX = "super_flex")
  lineup <- tibble(slot = fmt$roster_positions) |>
    filter(!slot %in% c("BN", "IR", "TAXI")) |>
    mutate(slot = coalesce(slot_names[slot], slot)) |>
    count(slot)

  missing_slots <- setdiff(lineup$slot, replacement_levels$slot)
  if (length(missing_slots) > 0) {
    warning(
      "no replacement level for lineup slot(s): ",
      str_flatten_comma(missing_slots)
    )
  }

  # R_season: mean over regular-season weeks of the lineup's summed replacement level
  base <- replacement_levels |>
    filter(week < fmt$playoff_week_start) |>
    inner_join(lineup, by = join_by(slot)) |>
    summarize(lineup_replacement = sum(weighted_mean * n), .by = c(season, week)) |>
    summarize(base = mean(lineup_replacement), .by = season) |>
    arrange(season)

  # sigma: pooled within team-season standard deviation of regular-season scores, over
  # completed weeks only (the weeks with replacement levels), so a run in the middle of a
  # week leaves out its partial scores
  sigma <- matchups_table |>
    semi_join(distinct(replacement_levels, season, week), by = join_by(season, week)) |>
    filter(week < fmt$playoff_week_start, points != 0) |>
    mutate(dev = points - mean(points), .by = c(season, roster_id)) |>
    summarize(
      sigma = sqrt(sum(dev^2) / (n() - n_distinct(season, roster_id)))
    ) |>
    pull(sigma)

  list(
    base = set_names(base$base, base$season),
    sigma = sigma,
    weeks = fmt$last_week
  )
}

# R_season for each simulated season (latest known season when not yet played)
season_base <- function(model, seasons) {
  known <- as.numeric(names(model$base))
  map_dbl(seasons, \(s) model$base[[as.character(max(known[known <= s], min(known)))]])
}

# check against seasons already played: team mean score vs realized top-12 value added per week
# (intercept should be near R_season and slope near 1)
report_points_model <- function(model, matchups_table, va, fmt, n_top = 12) {
  team_va <- va |>
    filter(!is.na(roster_id)) |>
    summarize(va = sum(value_added), .by = c(season, roster_id, player_id)) |>
    slice_max(va, n = n_top, with_ties = FALSE, by = c(season, roster_id)) |>
    summarize(team_va = sum(va), .by = c(season, roster_id))

  weeks <- va |>
    distinct(season, week) |>
    count(season, name = "weeks")

  check <- matchups_table |>
    semi_join(distinct(va, season, week), by = join_by(season, week)) |>
    filter(week < fmt$playoff_week_start, points != 0) |>
    summarize(points = mean(points), .by = c(season, roster_id)) |>
    inner_join(team_va, by = join_by(season, roster_id)) |>
    inner_join(weeks, by = join_by(season)) |>
    filter(weeks >= fmt$last_week) # completed seasons only

  message(
    "points model: weekly points ~ Normal(R + team VA / ", model$weeks,
    ", ", round(model$sigma, 1), "), R = ",
    str_flatten_comma(str_c(round(model$base, 1), " (", names(model$base), ")"))
  )

  if (nrow(check) > 2) {
    check_fit <- lm(points ~ I(team_va / weeks), data = check) |> coef()
    message(
      "  completed seasons: team mean points = ", round(check_fit[[1]], 1),
      " + ", round(check_fit[[2]], 2), " x (top-", n_top, " VA per week)"
    )
  }
  invisible(check)
}

# schedules ----------------------------------------------------------------

# one row per game (a vs b); played = final score known
season_schedule <- function(matchups_table, yr, fmt, completed_weeks) {
  games <- matchups_table |>
    filter(season == yr, roster_id < opponent_id) |>
    transmute(
      week,
      a = as.integer(roster_id),
      b = as.integer(opponent_id),
      a_points = points,
      b_points = opp_points,
      played = week %in% completed_weeks & (points != 0 | opp_points != 0)
    )

  schedule <- list(
    regular = games |> filter(week < fmt$playoff_week_start) |> arrange(week),
    playoffs = games |> filter(week >= fmt$playoff_week_start, played)
  )

  games_per_week <- count(schedule$regular, week)$n
  if (any(games_per_week != fmt$num_teams / 2)) {
    stop("season ", yr, " does not have ", fmt$num_teams / 2, " games every regular-season week")
  }
  schedule
}

# an unplayed season on the same regular-season schedule
unplayed_schedule <- function(schedule) {
  list(
    regular = schedule$regular |> mutate(a_points = 0, b_points = 0, played = FALSE),
    playoffs = schedule$playoffs[0, ]
  )
}

# weeks of a season finished by the end of date: Player Value Added.R counted the week
# (only complete weeks) and the Tuesday after it, when its Monday game is over, had come
# (week 1's Tuesday is the first after the opener, season_start). The same rule sets the
# share of the season played (season_context()) and the realized value over time
# (Scripts/Over Time Support.R).
completed_weeks_on <- function(va, season_dates, yr, date) {
  start <- season_dates$season_start[year(season_dates$season_start) == yr]
  if (length(start) != 1) {
    return(numeric())
  }
  va |>
    filter(season == yr) |>
    distinct(week) |>
    filter(start + (2 - wday(start)) %% 7 + 1 + 7 * (week - 1) <= date) |>
    pull(week)
}

# schedules for the simulated seasons: the first season with its completed weeks played,
# then future seasons on the current (or most recent) regular-season schedule
build_schedules <- function(matchups_table, seasons, fmt, completed_weeks) {
  if (seasons[1] %in% matchups_table$season) {
    first <- season_schedule(matchups_table, seasons[1], fmt, completed_weeks)
  } else {
    latest <- max(matchups_table$season)
    first <- season_schedule(matchups_table, latest, fmt, NULL) |> unplayed_schedule()
  }

  c(list(first), rep(list(unplayed_schedule(first)), length(seasons) - 1))
}

# assets -------------------------------------------------------------------

# quantile matrix and owner for every rostered player in one season's simulation
player_assets <- function(season_simulation, current_roster) {
  assets <- season_simulation |>
    inner_join(current_roster, by = join_by(player_id))

  values <- assets |>
    select(starts_with("proj_tva_")) |>
    as.matrix()
  complete <- complete.cases(values)
  if (any(!complete)) {
    message(sum(!complete), " rostered player(s) without simulations are skipped")
  }

  list(values = values[complete, , drop = FALSE], owners = assets$roster_id[complete])
}

# quantile matrix per metric ("first year", ...), one row per pick_no
rookie_matrices <- function(rookie_draft_values) {
  rookie_draft_values |>
    arrange(pick_no) |>
    split(~metric) |>
    map(\(d) d |> select(starts_with("proj_tva_")) |> as.matrix())
}

# quantile matrix and owner for the draft picks held for season yr
# orders: list by draft season of draft order indexed by the original team (pick_slot)
pick_assets <- function(picks, rookie, yr, orders, fmt) {
  metrics <- c("first year", "second year", "third year")
  picks <- picks[picks$season <= yr & yr - picks$season < length(metrics), ]

  if (nrow(picks) == 0) {
    return(list(values = NULL, owners = integer()))
  }

  draft_order <- picks$draft_order
  unknown <- which(is.na(draft_order))
  for (i in unknown) {
    draft_order[i] <- orders[[as.character(picks$season[i])]][picks$pick_slot[i]]
  }

  pick_no <- (picks$round - 1) * fmt$num_teams + draft_order
  metric <- metrics[yr - picks$season + 1]

  values <- matrix(NA_real_, nrow(picks), ncol(rookie[[1]]))
  for (m in unique(metric)) {
    rows <- metric == m
    values[rows, ] <- rookie[[m]][pick_no[rows], , drop = FALSE]
  }

  list(values = values, owners = picks$roster_id)
}

# one fresh quantile draw per asset, then each team's top n_top draws summed
sample_team_va <- function(values, owners, num_teams, n_top = 12) {
  draw <- values[cbind(
    seq_len(nrow(values)),
    sample.int(ncol(values), nrow(values), replace = TRUE)
  )]

  ord <- order(owners, -draw)
  owners <- owners[ord]
  draw <- draw[ord]
  within_team <- seq_along(owners) - match(owners, owners) + 1
  keep <- within_team <= n_top

  team_va <- numeric(num_teams)
  sums <- rowsum(draw[keep], owners[keep])
  team_va[as.integer(rownames(sums))] <- sums
  team_va
}

# season simulation --------------------------------------------------------

# plays the unplayed games of one season; returns final rank and bye by roster_id, and
# with watch_week, whether team a won each regular-season game that week (in schedule order)
simulate_season <- function(schedule, team_va, base, sigma, weeks, fmt, watch_week = NULL) {
  n <- fmt$num_teams
  mu <- base + team_va / weeks

  # regular season
  reg <- schedule$regular
  a_points <- reg$a_points
  b_points <- reg$b_points
  todo <- !reg$played
  a_points[todo] <- rnorm(sum(todo), mu[reg$a[todo]], sigma)
  b_points[todo] <- rnorm(sum(todo), mu[reg$b[todo]], sigma)

  a_result <- (a_points > b_points) + 0.5 * (a_points == b_points)
  teams <- factor(c(reg$a, reg$b), levels = seq_len(n))
  wins <- tapply(c(a_result, 1 - a_result), teams, sum)
  points_for <- tapply(c(a_points, b_points), teams, sum)

  # seeds: wins, then points for (Sleeper's default tiebreaker)
  seed <- order(-wins, -points_for)

  # playoffs: realized result if the game has been played, otherwise simulated
  playoffs <- schedule$playoffs
  play <- function(t1, t2, wk) {
    i <- which(
      playoffs$week == wk &
        ((playoffs$a == t1 & playoffs$b == t2) | (playoffs$a == t2 & playoffs$b == t1))
    )
    if (length(i) > 0) {
      i <- i[1]
      return(if (playoffs$a_points[i] >= playoffs$b_points[i]) playoffs$a[i] else playoffs$b[i])
    }
    if (rnorm(1, mu[t1], sigma) > rnorm(1, mu[t2], sigma)) t1 else t2
  }
  other <- function(winner, t1, t2) if (winner == t1) t2 else t1

  # Sleeper's 6-team bracket (no reseeding): 3 v 6 and 4 v 5, then 1 v the 4/5 winner and
  # 2 v the 3/6 winner, then the championship
  wk <- fmt$playoff_week_start + 0:2
  w36 <- play(seed[3], seed[6], wk[1])
  w45 <- play(seed[4], seed[5], wk[1])
  w1 <- play(seed[1], w45, wk[2])
  w2 <- play(seed[2], w36, wk[2])
  champion <- play(w1, w2, wk[3])
  runner_up <- other(champion, w1, w2)

  # final order: champion, runner-up, then everyone else by regular-season record
  # (the league places the 3rd place game, 5th place game, and losers bracket by record,
  # so those games don't change the standings or the draft order)
  final_order <- c(champion, runner_up, setdiff(seed, c(champion, runner_up)))

  rank <- integer(n)
  rank[final_order] <- seq_len(n)
  bye <- logical(n)
  bye[seed[1:2]] <- TRUE
  out <- list(rank = rank, bye = bye)
  if (!is.null(watch_week)) {
    out$a_won <- (a_points > b_points)[reg$week == watch_week]
  }
  out
}

# draft order (indexed by the original team) from a season's final ranks
draft_order_from_rank <- function(rank, fmt) fmt$num_teams + 1 - rank

# one simulation of every season: team value added is sampled each season, and each season's
# final standings set the draft order of the next draft
simulate_future <- function(inputs) {
  seasons <- inputs$seasons
  orders <- inputs$orders
  fmt <- inputs$fmt
  model <- inputs$model

  results <- vector("list", length(seasons))
  for (i in seq_along(seasons)) {
    picks <- pick_assets(inputs$picks, inputs$rookie, seasons[i], orders, fmt)
    team_va <- sample_team_va(
      rbind(inputs$players[[i]]$values, picks$values),
      c(inputs$players[[i]]$owners, picks$owners),
      fmt$num_teams,
      inputs$n_top
    )

    results[[i]] <- simulate_season(
      inputs$schedules[[i]],
      team_va,
      inputs$base[i],
      model$sigma,
      model$weeks,
      fmt,
      watch_week = if (i == 1) inputs$watch_week
    )
    orders[[as.character(seasons[i] + 1)]] <- draft_order_from_rank(results[[i]]$rank, fmt)
  }
  results
}

# draft order of the first simulated draft when it is not set yet (offseason): the final
# standings of the last completed season
realized_draft_order <- function(matchups_table, yr, fmt) {
  schedule <- season_schedule(matchups_table, yr, fmt, unique(matchups_table$week))
  if (!all(schedule$regular$played) || nrow(schedule$playoffs) == 0) {
    stop("season ", yr, " is not complete, so the next draft order is unknown")
  }
  # every game is realized, so the scoring inputs are irrelevant
  result <- simulate_season(schedule, numeric(fmt$num_teams), 0, 1, 1, fmt)
  draft_order_from_rank(result$rank, fmt)
}

# league settings of a season (or the latest cached season before it)
league_format_for <- function(yr) {
  cached <- list.files(here("Data/raw/sleeper"), pattern = "^league_\\d{4}\\.rds$") |>
    str_extract("\\d{4}") |>
    as.numeric()
  read_league_cache(max(cached[cached <= yr])) |>
    league_format()
}

# future standings at a date -------------------------------------------------

# odds of each final rank and of a bye in the next n_years seasons, as the league stood at
# the end of date. Games finished by then keep their real result; the rest are simulated.
#   state   the rosters and draft picks on that date: list(roster = player_id and
#           roster_id, picks = as in future_draft_picks.csv)
#   sims    that date's player simulations by season, the first being the season under way
#           (the next one in the offseason): player_simulations.rds, or a date's file in
#           Modeling/models/simulations
#   league  inputs shared by every date: the points model, rookie pick values (rookie
#           matrices), every game (matchups_table), the weeks with value added (va), and
#           season_dates
# Each date uses the same seed, so the odds move only with the league, not with
# simulation noise. Run inside a future plan (Future Standings.R sets one).
future_standings_at <- function(
  date,
  state,
  sims,
  league,
  n_sims = 50000,
  seed = 123,
  n_years = 3,
  n_top = 12
) {
  inputs <- standings_inputs(date, state, sims, league, n_years, n_top)

  results <- future_map(
    seq_len(n_sims),
    \(i) simulate_future(inputs),
    .options = furrr_options(seed = seed)
  )

  summarize_standings(results, inputs$seasons, inputs$fmt)
}

# everything simulate_future() needs for the league as it stood at the end of date (see
# future_standings_at())
standings_inputs <- function(date, state, sims, league, n_years, n_top) {
  seasons <- as.numeric(names(sims))[seq_len(n_years)]
  fmt <- league_format_for(seasons[1])

  picks <- state$picks |>
    filter(season %in% seasons)
  if (
    nrow(picks) > 0 &&
      max(picks$round) * fmt$num_teams > nrow(league$rookie[["first year"]])
  ) {
    stop("rookie_draft_values.csv does not cover round ", max(picks$round))
  }

  # order of the first simulated draft if it was not set yet (offseason)
  orders <- list()
  if (any(is.na(picks$draft_order[picks$season == seasons[1]]))) {
    orders[[as.character(seasons[1])]] <- realized_draft_order(
      league$matchups_table,
      seasons[1] - 1,
      fmt
    )
  }

  completed_weeks <- completed_weeks_on(league$va, league$season_dates, seasons[1], date)

  list(
    seasons = seasons,
    fmt = fmt,
    model = league$model,
    base = season_base(league$model, seasons),
    n_top = n_top,
    schedules = build_schedules(league$matchups_table, seasons, fmt, completed_weeks),
    players = map(sims[seq_len(n_years)], \(d) player_assets(d, state$roster)),
    picks = picks,
    rookie = league$rookie,
    orders = orders
  )
}

# the first simulated season ---------------------------------------------------

# the first simulated season alone (the season under way, or the next one in the
# offseason), simulated n_sims times as the league stood at the end of date: its odds
# (more precise than future_standings_at()'s, which Future Standings.R keeps for the later
# seasons) and, for the Current Matchups page, the week being played and how the season
# finishes after each combination of that week's results. The week is the first
# regular-season week not finished by date; there is none in the playoffs or the
# offseason. The simulations run in chunks that each return counts, so memory stays small.
# A scenario numbers a combination of results: bit j (from 0) is 1 when team a won game
# j + 1 (with no week, there is one scenario, 0). Returns the counts, which
# first_season_results() turns into odds (optionally reweighting this week's results,
# scenario_weights()):
#   season   the season simulated
#   n_sims   the number of simulations
#   games    season, week, game, a, b (roster_ids), model_a_win (share of simulations a
#            won); NULL with no week
#   tally    simulations by scenario, team (roster_id), and final rank
#   byes     simulations by scenario and team with a first-round bye
first_season_at <- function(
  date,
  state,
  sims,
  league,
  n_sims = 500000,
  seed = 123,
  n_top = 12,
  chunk = 10000
) {
  inputs <- standings_inputs(date, state, sims, league, n_years = 1, n_top = n_top)
  season <- inputs$seasons[1]
  n_teams <- inputs$fmt$num_teams
  reg <- inputs$schedules[[1]]$regular

  has_week <- season %in% league$matchups_table$season && !all(reg$played)
  games <- NULL
  if (has_week) {
    inputs$watch_week <- min(reg$week[!reg$played])
    games <- reg |>
      filter(week == inputs$watch_week) |>
      transmute(season = season, week, game = row_number(), a, b)
  }
  n_games <- NROW(games)

  sizes <- diff(c(seq(0, n_sims, by = chunk), n_sims)) |> discard(\(x) x == 0)
  counts <- future_map(
    sizes,
    \(size) {
      tally <- array(0L, c(2^n_games, n_teams, n_teams))
      byes <- matrix(0L, 2^n_games, n_teams)
      teams <- seq_len(n_teams)
      for (i in seq_len(size)) {
        result <- simulate_future(inputs)[[1]]
        scenario <- sum(result$a_won * 2^(seq_len(n_games) - 1)) + 1
        cells <- cbind(scenario, teams, result$rank)
        tally[cells] <- tally[cells] + 1L
        byes[scenario, ] <- byes[scenario, ] + result$bye
      }
      list(tally = tally, byes = byes)
    },
    .options = furrr_options(seed = seed)
  )

  first <- list(
    season = season,
    n_sims = n_sims,
    games = games,
    tally = map(counts, "tally") |> reduce(`+`),
    byes = map(counts, "byes") |> reduce(`+`)
  )
  if (has_week) {
    first$games$model_a_win <- game_shares(first, rep(1, 2^n_games))
  }
  first
}

# simulations in each scenario (each simulation has one final rank for team 1)
scenario_counts <- function(first) {
  apply(first$tally[, 1, , drop = FALSE], 1, sum)
}

# which scenarios have team a winning each game: a scenario by game matrix
scenario_bits <- function(first) {
  n_games <- NROW(first$games)
  vapply(
    seq_len(n_games),
    \(j) bitwAnd(seq_len(2^n_games) - 1L, 2L^(j - 1)) > 0,
    logical(2^n_games)
  ) |>
    matrix(ncol = n_games)
}

# share of the (weighted) simulations in which team a won each game
game_shares <- function(first, weights) {
  mass <- weights * scenario_counts(first)
  colSums(scenario_bits(first) * mass) / sum(mass)
}

# a weight for each scenario so that team a wins game j in a share target[j] of the
# weighted simulations, changing the simulated chances as little as possible otherwise
# (iterative proportional fitting over the games). Each scenario's simulations keep their
# finishes, so the season's odds follow the new chances for this week's games. The weights
# average 1 over the simulations.
scenario_weights <- function(first, target) {
  counts <- scenario_counts(first)
  bits <- scenario_bits(first)
  target <- pmin(pmax(target, 1e-6), 1 - 1e-6)
  weights <- rep(1, length(counts))

  for (iteration in 1:500) {
    for (j in seq_along(target)) {
      share <- sum((weights * counts)[bits[, j]]) / sum(weights * counts)
      weights[bits[, j]] <- weights[bits[, j]] * target[j] / share
      weights[!bits[, j]] <- weights[!bits[, j]] * (1 - target[j]) / (1 - share)
    }
    if (max(abs(game_shares(first, weights) - target)) < 1e-10) {
      break
    }
  }
  weights * sum(counts) / sum(weights * counts)
}

# first_season_at()'s counts as odds, with each scenario's simulations weighted (all 1, the
# model's own chances, by default):
#   standings  as future_standings_at() returns, for the first season only
#   games      first_season_at()'s, with a_win, the weighted share of simulations a won
#   scenarios  scenario, roster_id, finish, n (simulations), weight (the scenario's)
first_season_results <- function(first, weights = NULL) {
  weights <- weights %||% rep(1, dim(first$tally)[1])
  n_teams <- dim(first$tally)[2]
  n_sims <- first$n_sims

  rank_counts <- apply(first$tally * weights, c(2, 3), sum)
  bye_counts <- colSums(first$byes * weights)
  ranks <- which(rank_counts > 0, arr.ind = TRUE)
  standings <- bind_rows(
    tibble(
      roster_id = ranks[, 1],
      result = ranks[, 2],
      n = rank_counts[ranks],
      type = "rank"
    ),
    tibble(
      roster_id = rep(seq_len(n_teams), 2),
      result = rep(c(0, 1), each = n_teams),
      n = c(n_sims - bye_counts, bye_counts),
      type = "bye"
    ) |>
      filter(n > 1e-9)
  ) |>
    arrange(type != "rank", roster_id, result) |>
    transmute(season = first$season, roster_id, result, perc = n / n_sims, type)

  if (is.null(first$games)) {
    return(list(standings = standings, games = NULL, scenarios = NULL))
  }

  games <- first$games
  games$a_win <- game_shares(first, weights)

  cells <- which(first$tally > 0, arr.ind = TRUE)
  scenarios <- tibble(
    scenario = cells[, 1] - 1L,
    roster_id = cells[, 2],
    finish = cells[, 3],
    n = first$tally[cells],
    weight = weights[cells[, 1]]
  )

  list(standings = standings, games = games, scenarios = scenarios)
}

# Elo and the blend -------------------------------------------------------------

# each team's Elo rating going into a week: its rating after the latest earlier week of
# that season (week 0 is the rating entering the season), from weekly_elo.csv (Matchups.R)
pregame_elo <- function(weekly_elo, season, week) {
  weekly_elo |>
    filter(season == !!season, week < !!week) |>
    slice_max(week, n = 1, with_ties = TRUE) |>
    select(roster_id, elo)
}

# the chance team a beats team b from their Elo ratings (as in Matchups.R)
elo_win_chance <- function(elo_a, elo_b) {
  1 / (1 + 10^((elo_b - elo_a) / 400))
}

# this week's games with each model's chance that team a wins and the blend shown in the
# app: w * the Future Standings model + (1 - w) * Elo (w from Win Probability Blend.R)
blend_games <- function(games, weekly_elo, w) {
  elo <- pregame_elo(weekly_elo, games$season[1], games$week[1])
  games |>
    left_join(elo |> rename(a = roster_id, elo_a = elo), by = join_by(a)) |>
    left_join(elo |> rename(b = roster_id, elo_b = elo), by = join_by(b)) |>
    mutate(
      elo_a_win = coalesce(elo_win_chance(elo_a, elo_b), model_a_win),
      blend_a_win = w * model_a_win + (1 - w) * elo_a_win
    ) |>
    select(-elo_a, -elo_b)
}

# the first day of a week's games, or earlier: the opener's weekday, a week per week
# (games are Thursday to Monday; the opener is a Wednesday or Thursday)
week_kickoff <- function(season_dates, season, week) {
  start <- season_dates$season_start[year(season_dates$season_start) == season]
  start + 7 * (week - 1)
}

# Inputs ------------------------------------------------------------------------------

# the inputs shared by every date (Future Standings.R and Win Probability Blend.R): the
# points model, fit on every game played so far, and the tables the simulations read
standings_league <- function(first_season) {
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
    here("Data/derived/season_dates.csv"),
    show_col_types = FALSE
  )

  fmt <- league_format_for(first_season)
  list(
    fmt = fmt,
    model = fit_points_model(matchups_table, replacement_levels, fmt),
    rookie = rookie_matrices(rookie_draft_values),
    matchups_table = matchups_table,
    va = va,
    season_dates = season_dates
  )
}

# share of simulations with each final rank and with a bye, by season and roster
summarize_standings <- function(sims, seasons, fmt) {
  n_sims <- length(sims)
  map(seq_along(seasons), \(i) {
    rank <- map(sims, \(s) s[[i]]$rank) |> unlist()
    bye <- map(sims, \(s) s[[i]]$bye) |> unlist()
    roster_id <- rep(seq_len(fmt$num_teams), n_sims)

    bind_rows(
      tibble(roster_id, result = rank) |> count(roster_id, result) |> mutate(type = "rank"),
      tibble(roster_id, result = as.numeric(bye)) |> count(roster_id, result) |> mutate(type = "bye")
    ) |>
      transmute(season = seasons[i], roster_id, result, perc = n / n_sims, type)
  }) |>
    bind_rows()
}
