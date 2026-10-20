# Player Value Added
# A player's value added (VA) in a week is his fantasy points minus the points of a
# replacement-level player. Players only earn value added while in the starting lineup.
#
# Replacement level, for each week and lineup slot type (QB, RB, WR, TE, FLEX,
# SUPER_FLEX, K, DST):
#   1. for each team, the hypothetical substitute if one starter were removed: the
#      higher-projected of the team's best bench player and the best free agent
#   2. a bench substitute is worth his actual points. A free-agent substitute is worth
#      the mean actual points of the top `n_fa_avg` projected free agents (a single
#      player's game is too noisy, since most teams share the same top free agent)
#   3. average across teams, then blend with the season mean for that slot type
# A starter is compared with the slot he would vacate. For example, if a team starts
# three RBs, removing any one of them empties the FLEX, so all three are compared
# with FLEX replacement.
#
# Inputs (written by Data Manipulation/Scraping.R, which must run first):
#   Data/raw/sleeper/{matchups,league,stats_points}_<season>.rds
#   Data/raw/projections/projections<yy>.csv, Data/raw/player_info.csv
#   Data/raw/box_scores/box_score_off<yy>.csv, Data/raw/box_scores/box_score_def<yy>.csv (fallback scoring only)
# Outputs:
#   Data/derived/va.csv, Shiny/Saved Files/season_value_added.csv, Data/derived/sleeper_points.csv, Data/derived/replacement_levels.csv

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

source(here("Data Manipulation/Scrape Support.R")) # name_correction(), %!in%

message("begin computing Player Value Added...")

# Settings ----------------------------------------------------------------

n_fa_avg <- 3 # free-agent replacement = mean points of the top-3 projected free agents
week_weight <- 0.7 # weight on the weekly replacement level (the rest on the season mean)
min_stats_match <- 0.95 # share of starters that Sleeper stats must reproduce to be used
fantasy_positions <- c("QB", "RB", "WR", "TE", "K", "DST")
cache_dir <- here("Data/raw/sleeper")

# lineup slot types and the positions eligible for each
slot_map <- tribble(
  ~slot        , ~position ,
  "QB"         , "QB"      ,
  "RB"         , "RB"      ,
  "WR"         , "WR"      ,
  "TE"         , "TE"      ,
  "K"          , "K"       ,
  "DST"        , "DST"     ,
  "flex"       , "RB"      ,
  "flex"       , "WR"      ,
  "flex"       , "TE"      ,
  "super_flex" , "QB"      ,
  "super_flex" , "RB"      ,
  "super_flex" , "WR"      ,
  "super_flex" , "TE"
)

# Load Data ---------------------------------------------------------------

read_cache <- function(type, season) {
  path <- file.path(cache_dir, str_c(type, "_", season, ".rds"))
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)$data
}

seasons <- list.files(cache_dir, pattern = "^matchups_\\d{4}\\.rds$") |>
  str_extract("\\d{4}") |>
  as.numeric() |>
  sort()

if (length(seasons) == 0) {
  stop("No cached matchups. Run Data Manipulation/Scraping.R first.")
}
current_season <- max(seasons)

player_info <- read_csv(
  here("Data/raw/player_info.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)

# one row per player_id (a few retired players are listed at two positions)
player_lookup <- player_info |>
  distinct(player_id, .keep_all = TRUE) |>
  select(player_id, name, position)

# nflreadr's id crosswalk (gsis_id, sleeper_id, ...), loaded once and only if needed
load_ff_ids <- local({
  ids <- NULL
  function() {
    if (is.null(ids)) {
      ids <<- tryCatch(
        nflreadr::load_ff_playerids() |>
          mutate(
            sleeper_id = as.character(sleeper_id),
            position = recode(position, "PK" = "K")
          ) |>
          filter(!is.na(sleeper_id)),
        error = function(e) {
          warning("Could not load nflreadr::load_ff_playerids(): ", conditionMessage(e))
          tibble(
            gsis_id = character(),
            sleeper_id = character(),
            name = character(),
            position = character()
          )
        }
      )
    }
    ids
  }
})

# Rosters, Starters, and Points -------------------------------------------
# one row per rostered player per week, with his exact league points from Sleeper
# (starters_points for starters, players_points for the bench)

matchup_week_long <- function(m, season, week) {
  if (NROW(m) == 0 || sum(m$points) == 0) {
    return(NULL) # week not played yet
  }

  bench_pts <- as_tibble(m$players_points) |>
    mutate(roster_id = m$roster_id) |>
    pivot_longer(
      -roster_id,
      names_to = "player_id",
      values_to = "pts",
      values_drop_na = TRUE
    )

  starters <- tibble(
    roster_id = m$roster_id,
    player_id = m$starters,
    starter_pts = m$starters_points
  ) |>
    unnest(cols = c(player_id, starter_pts)) |>
    filter(player_id != "0") |> # empty lineup slot
    mutate(starter = TRUE)

  tibble(roster_id = m$roster_id, player_id = m$players) |>
    unnest(cols = player_id) |>
    left_join(bench_pts, by = join_by(roster_id, player_id)) |>
    left_join(starters, by = join_by(roster_id, player_id)) |>
    mutate(
      season = season,
      week = week,
      starter = coalesce(starter, FALSE),
      sleeper_points = coalesce(if_else(starter, starter_pts, pts), 0)
    ) |>
    select(season, week, roster_id, player_id, starter, sleeper_points)
}

rostered <- map(
  seasons,
  \(s) {
    read_cache("matchups", s) |>
      imap(\(m, wk) matchup_week_long(m, s, wk)) |>
      bind_rows()
  }
) |>
  bind_rows()

# only count weeks in which every NFL game is final (guards against a mid-week run)
completed_weeks <- tryCatch(
  nflreadr::load_schedules(current_season) |>
    filter(game_type == "REG") |>
    summarize(done = all(!is.na(result)), .by = week) |>
    filter(done) |>
    pull(week),
  error = function(e) {
    warning(
      "Could not load the NFL schedule, so every week with fantasy points ",
      "is treated as complete: ",
      conditionMessage(e)
    )
    NULL
  }
)

if (!is.null(completed_weeks)) {
  in_progress <- rostered |>
    filter(season == current_season, week %!in% completed_weeks) |>
    distinct(week) |>
    pull(week)

  if (length(in_progress) > 0) {
    message("skipping week(s) still in progress: ", str_flatten_comma(in_progress))
  }

  rostered <- rostered |>
    filter(season < current_season | week %in% completed_weeks)
}

rostered <- rostered |>
  left_join(player_lookup, by = join_by(player_id))

weeks_played <- rostered |> distinct(season, week)

# league lineup: how many dedicated QB/RB/WR/TE slots, and how many starters in total
lineup <- map(
  seasons,
  \(s) tibble(season = s, slot = read_cache("league", s)$roster_positions)
) |>
  bind_rows()

dedicated_slots <- lineup |>
  filter(slot %in% c("QB", "RB", "WR", "TE")) |>
  count(season, position = slot, name = "n_dedicated")

qb_slots <- dedicated_slots |>
  filter(position == "QB") |>
  select(season, n_qb_slots = n_dedicated)

starting_slots <- lineup |>
  filter(slot %!in% c("BN", "IR", "TAXI")) |>
  count(season, name = "n_slots")

# Projections ---------------------------------------------------------------
# FFToday projections have no ids, so they are joined to Sleeper ids on name and
# position, with nflreadr's cleaned names as a second pass

projections <- map(
  seasons,
  \(s) {
    read_csv(
      here(str_c("Data/raw/projections/projections", s %% 100, ".csv")),
      col_types = cols(.default = col_character())
    ) |>
      mutate(season = s)
  }
) |>
  # older files have no player_id or source columns
  bind_rows(tibble(player_id = character(), source = character())) |>
  mutate(
    projection = as.numeric(projection),
    week = as.numeric(week),
    # Sleeper projections carry ids and K/DST points; older FFToday rows have neither
    source = coalesce(source, "fftoday")
  ) |>
  name_correction()

if (!"position" %in% names(projections)) {
  stop("Projection files need a position column (see combine_week()).")
}

# Sleeper projections already have ids; FFToday rows are matched on name and position
proj_known <- projections |>
  filter(!is.na(player_id))

proj_ids <- projections |>
  filter(is.na(player_id)) |>
  select(-player_id) |>
  left_join(
    player_info |> distinct(name, position, player_id, years_exp),
    by = join_by(name, position),
    relationship = "many-to-many"
  ) |>
  # rare: two Sleeper ids share a name and position, keep the more recent player
  arrange(years_exp) |>
  distinct(season, week, name, position, .keep_all = TRUE) |>
  select(-years_exp)

unmatched <- proj_ids |>
  filter(is.na(player_id)) |>
  distinct(name, position)

if (nrow(unmatched) > 0) {
  ff_ids <- load_ff_ids()

  if (nrow(ff_ids) > 0) {
    second_pass <- unmatched |>
      mutate(clean_name = nflreadr::clean_player_names(name, lowercase = TRUE)) |>
      inner_join(
        ff_ids |>
          transmute(
            clean_name = nflreadr::clean_player_names(name, lowercase = TRUE),
            position,
            ff_player_id = sleeper_id
          ) |>
          distinct(clean_name, position, .keep_all = TRUE),
        by = join_by(clean_name, position)
      ) |>
      select(name, position, ff_player_id)

    proj_ids <- proj_ids |>
      left_join(second_pass, by = join_by(name, position)) |>
      mutate(player_id = coalesce(player_id, ff_player_id)) |>
      select(-ff_player_id)
  }

  still_unmatched <- proj_ids |>
    filter(is.na(player_id)) |>
    distinct(name, position)

  if (nrow(still_unmatched) > 0) {
    message(
      "projection names with no Sleeper id (add to name_correction()): ",
      str_flatten_comma(str_c(still_unmatched$name, " (", still_unmatched$position, ")"))
    )
  }
}

proj_ids <- proj_ids |>
  bind_rows(proj_known) |>
  filter(!is.na(player_id)) |>
  # a player listed on two position pages keeps his higher projection
  slice_max(projection, n = 1, with_ties = FALSE, by = c(season, week, player_id)) |>
  select(season, week, player_id, projection, proj_position = position, source)

# Fantasy Points for Everyone ----------------------------------------------
# rostered players already have exact points from the matchups. Free agents get
# exact points from Sleeper's weekly stats scored with the league's settings
# (Scraping.R), unless those fail to reproduce the starters' points, in which case
# they are recomputed from nflverse box scores.

stats_points <- map(seasons, \(s) read_cache("stats_points", s)) |>
  bind_rows()

if (nrow(stats_points) == 0) {
  stats_points <- tibble(
    season = double(),
    week = double(),
    player_id = character(),
    pts = double()
  )
}

# does Sleeper's stats endpoint reproduce the points Sleeper gave our starters?
stats_check <- rostered |>
  filter(starter, position %in% fantasy_positions) |>
  left_join(stats_points, by = join_by(season, week, player_id)) |>
  summarize(
    match = mean(abs(coalesce(pts, 0) - sleeper_points) < 0.015),
    .by = c(season, position)
  ) |>
  mutate(trusted = match >= min_stats_match)

# nflverse box scores scored with the league's settings (fallback only)
nflverse_points <- function(fallback_seasons) {
  read_box <- function(type, s) {
    path <- here(str_c("Data/raw/box_scores/box_score_", type, s %% 100, ".csv"))
    if (!file.exists(path)) {
      return(NULL)
    }
    read_csv(path, show_col_types = FALSE, guess_max = 10000) |>
      mutate(across(where(is.numeric), \(x) coalesce(x, 0)))
  }

  off <- map(fallback_seasons, \(s) read_box("off", s)) |> bind_rows()
  def <- map(fallback_seasons, \(s) read_box("def", s)) |> bind_rows()

  off_pts <- off |>
    transmute(
      season,
      week,
      gsis_id = player_id,
      name = player_display_name,
      position,
      pts = .04 *
        passing_yards +
        4 * passing_tds -
        passing_interceptions +
        2 * passing_2pt_conversions +
        .1 * rushing_yards +
        6 * rushing_tds +
        2 * rushing_2pt_conversions +
        receptions +
        .1 * receiving_yards +
        6 * receiving_tds +
        2 * receiving_2pt_conversions -
        # only lost fumbles count, at -2 each
        2 * (sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost) +
        6 * (special_teams_tds + fumble_recovery_tds) +
        # kicking: 50+ yards is everything made beyond 49 (60-yarders fall outside
        # nflverse's distance buckets), and blocked kicks count as misses
        3 * (fg_made_0_19 + fg_made_20_29 + fg_made_30_39) +
        4 * fg_made_40_49 +
        5 *
          (fg_made -
            fg_made_0_19 -
            fg_made_20_29 -
            fg_made_30_39 -
            fg_made_40_49) -
        fg_missed -
        fg_blocked +
        pat_made -
        pat_missed -
        pat_blocked
    ) |>
    # map nflverse ids to Sleeper ids, then fall back to name and position
    left_join(
      load_ff_ids() |>
        filter(!is.na(gsis_id)) |>
        distinct(gsis_id, .keep_all = TRUE) |>
        select(gsis_id, player_id = sleeper_id),
      by = join_by(gsis_id)
    ) |>
    name_correction() |>
    left_join(
      player_info |>
        distinct(name, position, .keep_all = TRUE) |>
        select(name, position, name_player_id = player_id),
      by = join_by(name, position)
    ) |>
    mutate(player_id = coalesce(player_id, name_player_id))

  # points allowed by each defense = points scored by its opponent's offense
  opp <- def |>
    transmute(
      season,
      week,
      team = opponent_team,
      pa = 6 *
        (passing_tds + rushing_tds + fumble_recovery_tds + special_teams_tds) +
        2 * (passing_2pt_conversions + rushing_2pt_conversions) +
        3 * fg_made +
        pat_made,
      opp_fumbles_lost = sack_fumbles_lost +
        rushing_fumbles_lost +
        receiving_fumbles_lost,
      opp_blocks = fg_blocked + pat_blocked
    )

  def_pts <- def |>
    left_join(opp, by = join_by(season, week, team)) |>
    transmute(
      season,
      week,
      player_id = recode(team, "LA" = "LAR"),
      pts = case_when(
        pa == 0 ~ 10,
        pa < 7 ~ 7,
        pa < 14 ~ 4,
        pa < 21 ~ 1,
        pa < 28 ~ 0,
        pa < 35 ~ -1,
        .default = -4
      ) +
        6 * (special_teams_tds + def_tds + fumble_recovery_tds) +
        def_sacks +
        2 * def_interceptions +
        2 * opp_fumbles_lost +
        def_fumbles_forced +
        2 * def_safeties +
        2 * opp_blocks
    )

  bind_rows(
    off_pts |> select(season, week, player_id, pts),
    def_pts
  ) |>
    filter(!is.na(player_id)) |>
    distinct(season, week, player_id, .keep_all = TRUE)
}

points <- stats_points |>
  left_join(player_lookup |> select(player_id, position), by = join_by(player_id)) |>
  semi_join(stats_check |> filter(trusted), by = join_by(season, position)) |>
  mutate(source = "sleeper stats")

untrusted <- stats_check |> filter(!trusted)

if (nrow(untrusted) > 0) {
  message(
    "Sleeper stats did not reproduce starters' points for ",
    str_flatten_comma(str_c(
      untrusted$season,
      " ",
      untrusted$position,
      " (",
      scales::percent(untrusted$match, accuracy = 1),
      ")"
    )),
    ". Using nflverse box scores for those free agents."
  )

  points <- nflverse_points(unique(untrusted$season)) |>
    left_join(
      player_lookup |> select(player_id, position),
      by = join_by(player_id)
    ) |>
    semi_join(untrusted, by = join_by(season, position)) |>
    mutate(source = "nflverse") |>
    bind_rows(points)
}

points <- points |>
  select(season, week, player_id, pts, source)

# K/DST projections are rankings, convert them to points ----------------------
# FFToday only ranks kickers and defenses (stored as 33 - rank). Map rank to
# expected points with a linear fit of actual points on rank, which keeps the order
# (and so every replacement choice) unchanged. Sleeper K/DST projections are already
# points and are left as they are.

all_points <- bind_rows(
  rostered |>
    transmute(season, week, player_id, pts = sleeper_points, source = "sleeper matchup"),
  points
) |>
  distinct(season, week, player_id, .keep_all = TRUE)

kdst_fit <- proj_ids |>
  filter(proj_position %in% c("K", "DST"), source == "fftoday") |>
  mutate(rank = 33 - projection) |>
  semi_join(weeks_played, by = join_by(season, week)) |>
  left_join(all_points, by = join_by(season, week, player_id)) |>
  mutate(pts = coalesce(pts, 0)) |>
  nest(.by = proj_position) |>
  mutate(
    coef = map(data, \(d) coef(lm(pts ~ rank, data = d))),
    intercept = map_dbl(coef, 1),
    slope = map_dbl(coef, 2)
  ) |>
  select(proj_position, intercept, slope)

if (any(kdst_fit$slope >= 0)) {
  warning("K/DST points do not fall with rank, keeping the raw rank scale.")
  kdst_fit <- kdst_fit |> mutate(intercept = 33, slope = -1)
}

proj_ids <- proj_ids |>
  left_join(kdst_fit, by = join_by(proj_position)) |>
  mutate(
    projection = if_else(
      is.na(slope) | source != "fftoday",
      projection,
      intercept + slope * (33 - projection)
    )
  ) |>
  select(season, week, player_id, projection)

rostered <- rostered |>
  left_join(proj_ids, by = join_by(season, week, player_id))

# guard: a week whose projections are missing (e.g. a failed scrape) would be
# computed with every projection set to 0, which makes its replacement choices
# arbitrary. Flag any week where most starters have no projection.
missing_projections <- rostered |>
  filter(starter) |>
  summarize(share_missing = mean(is.na(projection)), .by = c(season, week)) |>
  filter(share_missing > 0.5)

if (nrow(missing_projections) > 0) {
  warning(
    "No projections for ",
    str_flatten_comma(str_c(missing_projections$season, " week ", missing_projections$week)),
    ": value added for these weeks is computed with every projection set to 0 and ",
    "is not reliable. Rerun Scraping.R once projections are available."
  )
}

rostered <- rostered |>
  mutate(projection = coalesce(projection, 0)) # unprojected bench players

# Replacement Level --------------------------------------------------------

# the top projected free agents at each slot type (eligibility uses Sleeper positions)
free_agents <- proj_ids |>
  semi_join(weeks_played, by = join_by(season, week)) |>
  anti_join(rostered, by = join_by(season, week, player_id)) |>
  inner_join(player_lookup |> select(player_id, position), by = join_by(player_id)) |>
  left_join(points |> select(-source), by = join_by(season, week, player_id)) |>
  mutate(pts = coalesce(pts, 0)) |> # projected but did not play
  inner_join(slot_map, by = join_by(position), relationship = "many-to-many") |>
  # players tied at the cutoff are all included, so the result does not depend on row order
  slice_max(projection, n = n_fa_avg, with_ties = TRUE, by = c(season, week, slot)) |>
  summarize(
    fa_proj = max(projection),
    fa_pts = mean(pts),
    n_fa = n(),
    .by = c(season, week, slot)
  )

# each team's best bench option at each slot type
bench_best <- rostered |>
  filter(!starter) |>
  inner_join(slot_map, by = join_by(position), relationship = "many-to-many") |>
  arrange(desc(projection), player_id) |> # ties broken by player_id, not row order
  slice_head(n = 1, by = c(season, week, roster_id, slot)) |>
  select(season, week, roster_id, slot, bench_proj = projection, bench_pts = sleeper_points)

# each team's substitute: its bench player if projected at least as high as the
# best free agent, otherwise the free agents
team_replacement <- rostered |>
  distinct(season, week, roster_id) |>
  cross_join(distinct(slot_map, slot)) |>
  left_join(bench_best, by = join_by(season, week, roster_id, slot)) |>
  left_join(free_agents, by = join_by(season, week, slot)) |>
  mutate(
    from_bench = !is.na(bench_proj) & (is.na(fa_proj) | bench_proj >= fa_proj),
    replacement_pts = if_else(from_bench, bench_pts, coalesce(fa_pts, 0))
  )

replacement_levels <- team_replacement |>
  summarize(
    mean_replacement = mean(replacement_pts),
    share_from_bench = mean(from_bench),
    .by = c(season, week, slot)
  ) |>
  # smooth the weekly level toward the season mean for that slot type
  mutate(
    weighted_mean = week_weight *
      mean_replacement +
      (1 - week_weight) * mean(mean_replacement),
    .by = c(season, slot)
  ) |>
  arrange(season, week, slot)

# Value Added --------------------------------------------------------------
# a starter beyond his position's dedicated slots competes for a FLEX or SUPER_FLEX;
# it is SUPER_FLEX unless the team's QBs already fill it

starters <- rostered |>
  filter(starter) |>
  left_join(dedicated_slots, by = join_by(season, position)) |>
  left_join(qb_slots, by = join_by(season)) |>
  mutate(n_pos = n(), .by = c(season, week, roster_id, position)) |>
  mutate(n_qb = sum(position == "QB", na.rm = TRUE), .by = c(season, week, roster_id)) |>
  mutate(
    slot = case_when(
      position == "QB" & n_qb > n_qb_slots ~ "super_flex",
      position %in% c("RB", "WR", "TE") & n_pos > n_dedicated & n_qb <= n_qb_slots ~ "super_flex",
      position %in% c("RB", "WR", "TE") & n_pos > n_dedicated ~ "flex",
      .default = position
    )
  ) |>
  left_join(
    replacement_levels |> select(season, week, slot, weighted_mean),
    by = join_by(season, week, slot)
  ) |>
  mutate(value_added = sleeper_points - weighted_mean, type = "starter")

value_added <- bind_rows(
  starters,
  rostered |> filter(!starter) |> mutate(value_added = 0, type = "bench")
) |>
  arrange(season, week, desc(type), roster_id) |>
  select(
    roster_id,
    name,
    player_id,
    position,
    week,
    sleeper_points,
    value_added,
    type,
    season,
    projection
  )

season_value_added <- value_added |>
  summarize(
    name = first(name),
    position = first(position),
    total_value_added = sum(value_added),
    total_points = sum(sleeper_points),
    .by = c(season, player_id)
  ) |>
  arrange(desc(total_value_added)) |>
  relocate(season, name, player_id, position)

# points for every fantasy-relevant player (fantasy positions only)
sleeper_points <- all_points |>
  inner_join(player_lookup, by = join_by(player_id)) |>
  filter(position %in% fantasy_positions) |>
  transmute(name, player_id, position, season, week, sleeper_points = pts, source) |>
  arrange(season, week, desc(sleeper_points))

# Sanity Checks ------------------------------------------------------------

dupes <- value_added |> count(season, week, player_id) |> filter(n > 1)
if (nrow(dupes) > 0) {
  stop("Duplicate player-weeks in value added, e.g. player ", dupes$player_id[1])
}

missing_info <- value_added |> filter(is.na(position)) |> distinct(player_id)
if (nrow(missing_info) > 0) {
  warning(
    "Rostered players missing from player_info (rerun it with run_player_info <- TRUE): ",
    str_flatten_comma(missing_info$player_id)
  )
}

missing_va <- value_added |> filter(type == "starter", is.na(value_added))
if (nrow(missing_va) > 0) {
  warning(nrow(missing_va), " starter rows have no value added.")
}

short_lineups <- starters |>
  count(season, week, roster_id) |>
  left_join(starting_slots, by = join_by(season)) |>
  filter(n < n_slots)
if (nrow(short_lineups) > 0) {
  message(nrow(short_lineups), " team-weeks started fewer than a full lineup (empty slots).")
}

thin_pool <- free_agents |> filter(n_fa < n_fa_avg)
if (nrow(thin_pool) > 0) {
  message(
    nrow(thin_pool),
    " slot-weeks had fewer than ",
    n_fa_avg,
    " projected free agents."
  )
}

odd_levels <- replacement_levels |>
  filter(is.na(weighted_mean) | weighted_mean < -5 | weighted_mean > 40)
if (nrow(odd_levels) > 0) {
  warning(nrow(odd_levels), " replacement levels are missing or implausible.")
}

# Save ---------------------------------------------------------------------

write_csv(sleeper_points, here("Data/derived/sleeper_points.csv"))
write_csv(replacement_levels, here("Data/derived/replacement_levels.csv"))
write_csv(value_added, here("Data/derived/va.csv"))
write_csv(season_value_added, here("Shiny/Saved Files/season_value_added.csv"))

# remove objects and functions to declutter environment
rm(list = intersect(
  c(
    "read_cache", "load_ff_ids", "matchup_week_long", "nflverse_points",
    "rostered", "weeks_played", "completed_weeks", "in_progress", "lineup",
    "dedicated_slots", "qb_slots", "starting_slots", "projections", "proj_ids",
    "proj_known", "missing_projections",
    "unmatched", "ff_ids", "second_pass", "still_unmatched", "stats_points",
    "stats_check", "untrusted", "points", "all_points", "kdst_fit", "free_agents",
    "bench_best", "team_replacement", "starters", "dupes", "missing_info",
    "missing_va", "short_lineups", "thin_pool", "odd_levels"
  ),
  ls()
))
