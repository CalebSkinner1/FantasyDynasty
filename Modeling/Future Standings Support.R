# Future Standings Support
# this holds functions utilized in Future Standings.R
#
# Points model: a team's score in a week is the league's replacement-level lineup plus the
# team's season value added spread over the weeks of the season, plus weekly noise,
#   points ~ Normal(R_season + team_va / season_weeks, sigma)
# team_va: sum of the team's top 12 sampled projected value added (players and draft picks)
# R_season: summed replacement level of the starting lineup (Data/replacement_levels.csv)
# sigma: week-to-week standard deviation of a team's score within a season

suppressPackageStartupMessages({
  library("tidyverse")
  library("furrr")
})

# league format ------------------------------------------------------------

read_league_cache <- function(season, cache_dir = here("Data/cache/sleeper")) {
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

  # sigma: pooled within team-season standard deviation of regular-season scores
  sigma <- matchups_table |>
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

# schedules for the simulated seasons: the current season as played so far, then future
# seasons on the current (or most recent) regular-season schedule
build_schedules <- function(matchups_table, va, seasons, fmt) {
  if (seasons[1] %in% matchups_table$season) {
    # weeks Player Value Added.R counted as complete (guards against a mid-week run)
    completed_weeks <- va |>
      filter(season == seasons[1]) |>
      distinct(week) |>
      pull(week)
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

# plays the unplayed games of one season; returns final rank and bye by roster_id
simulate_season <- function(schedule, team_va, base, sigma, weeks, fmt) {
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
  list(rank = rank, bye = bye)
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
      fmt
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
