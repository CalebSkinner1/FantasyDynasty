# Activity Rating
# how actively each manager runs his team, week by week through the regular season (the
# league's activity sheet, automated). A team's weekly score:
#   efficiency    10 * points / max points, max points being the best lineup that week
#                 from the actual points of the players on its roster (players on IR or the
#                 taxi squad left out once Scraping.R has saved who they were; earlier
#                 weeks count everyone)
#   - 7 * ERS     empty roster spots: a starting slot left empty, a starter whose NFL team
#                 was on bye, or a starter who wasn't active for his team's game (inactive,
#                 on reserve, or on no team; late scratches included). A starter who was
#                 active but recorded no stats isn't one.
#   - 3 * QB      questionable benchings: a starter compared with the slot he would leave
#                 open, as in Player Value Added.R (a third starting RB competes for the
#                 FLEX), and a bench player eligible for it projected higher by more than
#                 qb_threshold for the starter's position. Each bench player counts against
#                 one starter at most, and a starter already counted as an ERS isn't
#                 counted again.
#   - 2 * ODN     outcome determined: the game was lost, and filling each penalized slot
#                 with the bench player behind the penalty (for an ERS, the best-projected
#                 eligible bench player) would have won it, by actual points. Also an
#                 adjusted win.
#   + 5 * trades
#   + waivers     waiver claims and free agent pickups whose player was projected above 0
#                 that week (on an NFL roster and expected to play)
#   + 0.5 * bids  failed waiver claims
# Transactions count from September 1 (the FAAB reset), in their Sleeper week; those before
# week 1 count in week 1. The rolling score is the sum of the last rolling_weeks weekly
# scores, carried across seasons.
#
# Reads Sleeper's matchups, league settings, and transactions (Data/raw/sleeper), the
# projections (Data/raw/projections), the box scores (Data/raw/box_scores) with the id
# crosswalk (Data/derived/player_ids.csv), nflverse's game-day rosters
# (Data/raw/rosters_weekly), and the IR and taxi lists (Data/derived/roster_status_time.csv),
# all from Scraping.R.
#   activity_weekly.csv     season, week, roster_id, team, points, max_points, efficiency,
#                           ers, qb, odn, trades, waivers, bids, weekly_score, won,
#                           adjusted_won, rolling_score
#   activity_penalties.csv  season, week, team, penalty (ERS or QB), slot, started,
#                           started_projection, started_points, reason, bench,
#                           bench_projection, bench_points, decided (whether the game
#                           turned on the team's penalties)

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing the Activity Rating...")

rolling_weeks <- 18
qb_threshold <- c(QB = 3.5, RB = 2.5, WR = 2.5, TE = 1.5, K = 2, DST = 1.5)
penalties <- c(ers = 7, qb = 3, odn = 2)
transaction_points <- c(trades = 5, waivers = 1, bids = 0.5)

# lineup slot types and the positions eligible for each (as in Player Value Added.R)
slot_map <- tribble(
  ~slot, ~position,
  "QB", "QB",
  "RB", "RB",
  "WR", "WR",
  "TE", "TE",
  "K", "K",
  "DST", "DST",
  "flex", "RB",
  "flex", "WR",
  "flex", "TE",
  "super_flex", "QB",
  "super_flex", "RB",
  "super_flex", "WR",
  "super_flex", "TE"
)
# Sleeper's slot names
slot_names <- c(QB = "QB", RB = "RB", WR = "WR", TE = "TE", K = "K", DEF = "DST",
                FLEX = "flex", SUPER_FLEX = "super_flex")

# the sum of each value and the n - 1 before it
rolling_sum <- function(x, n) map_dbl(seq_along(x), \(i) sum(x[max(1, i - n + 1):i]))

# Load Data ---------------------------------------------------------------

users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, team = display_name)
player_info <- read_csv(
  here("Data/raw/player_info.csv"),
  col_types = cols(player_id = col_character(), .default = col_guess())
) |>
  distinct(player_id, .keep_all = TRUE) |>
  select(player_id, name, position)
completed_weeks <- read_csv(here("Data/derived/completed_weeks.csv"), show_col_types = FALSE)
season_dates <- read_csv(here("Data/derived/season_dates.csv"), show_col_types = FALSE)
matchups_table <- read_csv(here("Data/derived/matchups_table.csv"), show_col_types = FALSE)

sleeper_file <- function(type, season) here("Data/raw/sleeper", str_c(type, "_", season, ".rds"))
seasons <- list.files(here("Data/raw/sleeper"), pattern = "^matchups_\\d{4}\\.rds$") |>
  str_extract("\\d{4}") |>
  as.numeric() |>
  keep(\(s) file.exists(sleeper_file("league", s)) && file.exists(sleeper_file("transactions", s)))

projections <- map(seasons, \(s) {
  path <- here(str_c("Data/raw/projections/projections", s %% 100, ".csv"))
  if (!file.exists(path)) {
    return(NULL)
  }
  read_csv(path, col_types = cols(player_id = col_character(), .default = col_guess())) |>
    filter(!is.na(player_id)) |>
    transmute(season = s, week, player_id, projection)
}) |>
  bind_rows() |>
  distinct(season, week, player_id, .keep_all = TRUE)

# who played: a box score line (by Sleeper id), and every team's games
player_ids <- read_csv(here("Data/derived/player_ids.csv"), col_types = cols(.default = col_character()))
box <- map(seasons, \(s) {
  path <- here(str_c("Data/raw/box_scores/box_score_off", s %% 100, ".csv"))
  if (!file.exists(path)) {
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE, col_select = c(player_id, season, week, season_type, team))
}) |>
  bind_rows() |>
  filter(season_type == "REG") |>
  mutate(team = recode(team, "LA" = "LAR")) # Sleeper's id for the Rams defense
team_games <- box |>
  distinct(season, week, team)
played_box <- box |>
  inner_join(player_ids, by = join_by(player_id == gsis_id), suffix = c("_gsis", "")) |>
  distinct(season, week, player_id, team)

# game-day status and team (by Sleeper id, through the crosswalk where nflverse has none)
weekly_rosters <- map(seasons, \(s) {
  path <- here(str_c("Data/raw/rosters_weekly/roster_weekly", s %% 100, ".csv"))
  if (!file.exists(path)) {
    return(NULL)
  }
  read_csv(path, col_types = cols(season = col_double(), week = col_double(), .default = col_character()))
}) |>
  bind_rows()
if (nrow(weekly_rosters) > 0) {
  weekly_rosters <- weekly_rosters |>
    left_join(player_ids |> rename(crosswalk_id = player_id), by = join_by(gsis_id)) |>
    transmute(
      season,
      week,
      player_id = coalesce(sleeper_id, crosswalk_id),
      roster_team = recode(team, "LA" = "LAR"),
      active = status == "ACT"
    ) |>
    filter(!is.na(player_id)) |>
    # a player listed twice in a week (a trade) counts as active if either listing is
    summarize(roster_team = first(roster_team[active], default = first(roster_team)), active = any(active), .by = c(season, week, player_id))
} else {
  weekly_rosters <- tibble(season = double(), week = double(), player_id = character(), roster_team = character(), active = logical())
}

# IR and taxi squads, as saved by Scraping.R (none before it began saving them)
status_path <- here("Data/derived/roster_status_time.csv")
roster_status <- if (file.exists(status_path)) {
  read_csv(status_path, col_types = cols(date = col_date(), .default = col_character())) |>
    mutate(roster_id = as.integer(roster_id))
} else {
  tibble(date = as_date(character()), roster_id = integer(), player_id = character(), status = character())
}

# Lineups -----------------------------------------------------------------------------
# every rostered player each completed regular-season week: his slot if he started (in
# the order of the league's lineup), his actual points, and his projection

league_settings <- map(seasons, \(s) {
  league <- readRDS(sleeper_file("league", s))$data
  positions <- unlist(league$roster_positions)
  list(
    starting = unname(slot_names[positions[positions != "BN"]]),
    last_week = as.numeric(league$settings$playoff_week_start %||% 15) - 1
  )
}) |>
  set_names(seasons)

regular_weeks <- completed_weeks |>
  filter(season %in% seasons) |>
  filter(week <= map_dbl(as.character(season), \(s) league_settings[[s]]$last_week))

lineups <- map(seasons, \(s) {
  weeks <- readRDS(sleeper_file("matchups", s))$data
  starting <- league_settings[[as.character(s)]]$starting
  done <- regular_weeks$week[regular_weeks$season == s]
  map(done[done <= length(weeks)], \(w) {
    week_df <- weeks[[w]]
    map(seq_len(nrow(week_df)), \(i) {
      starters <- unlist(week_df$starters[[i]])
      players <- unlist(week_df$players[[i]])
      pts <- week_df$players_points[i, , drop = FALSE]
      bench <- setdiff(players, starters)
      tibble(
        season = s,
        week = w,
        roster_id = week_df$roster_id[i],
        team_points = week_df$points[i],
        player_id = c(starters, bench),
        slot = c(starting[seq_along(starters)], rep(NA, length(bench))),
        starter = c(rep(TRUE, length(starters)), rep(FALSE, length(bench)))
      ) |>
        mutate(points = map_dbl(player_id, \(p) as.numeric(pts[[p]] %||% 0)))
    }) |>
      bind_rows()
  }) |>
    bind_rows()
}) |>
  bind_rows() |>
  left_join(player_info, by = join_by(player_id)) |>
  left_join(projections, by = join_by(season, week, player_id)) |>
  mutate(projection = coalesce(projection, 0), points = coalesce(points, 0))

# the IR and taxi players each week: the latest saved list from the week's Tuesday
# through its Monday
week_span <- regular_weeks |>
  left_join(season_dates |> mutate(season = year(season_start)), by = join_by(season)) |>
  mutate(
    week_end = season_start + (2 - wday(season_start)) %% 7 + 7 * (week - 1),
    week_start = week_end - 6
  ) |>
  select(season, week, week_start, week_end)
inactive <- roster_status |>
  inner_join(week_span, by = join_by(between(date, week_start, week_end))) |>
  filter(date == suppressWarnings(max(date)), .by = c(season, week)) |>
  distinct(season, week, roster_id, player_id) |>
  mutate(roster_id = as.integer(roster_id))

lineups <- lineups |>
  left_join(
    inactive |> mutate(inactive = TRUE),
    by = join_by(season, week, roster_id, player_id)
  ) |>
  mutate(inactive = coalesce(inactive, FALSE) & !starter)

# Empty roster spots ----------------------------------------------------------------
# a starter's NFL team that week: his team in the box score, or on the game-day roster, or
# else in the nearest week he played that season. A defense's team is its id.

player_teams <- lineups |>
  filter(starter, player_id != "0", position != "DST") |>
  distinct(season, player_id) |>
  inner_join(distinct(team_games, season, week), by = join_by(season), relationship = "many-to-many") |>
  left_join(played_box, by = join_by(season, week, player_id)) |>
  mutate(played = !is.na(team)) |>
  arrange(player_id, season, week) |>
  group_by(player_id, season) |>
  fill(team, .direction = "downup") |>
  ungroup()

starters <- lineups |>
  filter(starter) |>
  left_join(player_teams, by = join_by(season, week, player_id)) |>
  left_join(weekly_rosters, by = join_by(season, week, player_id)) |>
  mutate(
    team = case_when(position == "DST" ~ player_id, played ~ team, .default = coalesce(roster_team, team)),
    team_played = paste(season, week, team) %in% paste(team_games$season, team_games$week, team_games$team),
    played = coalesce(played, FALSE) | points != 0 | (position == "DST" & team_played),
    # on no game-day roster: not active
    active = played | coalesce(active, FALSE),
    reason = case_when(
      player_id == "0" ~ "empty slot",
      played ~ NA,
      !is.na(team) & !team_played ~ "bye week",
      active ~ NA, # dressed, no stats
      .default = "not active"
    )
  )

# Questionable benching -----------------------------------------------------------------
# the slot a starter would leave open, as in Player Value Added.R: one beyond his
# position's dedicated slots competes for the FLEX, or the SUPER_FLEX unless the team's QBs
# already fill it

dedicated <- map(seasons, \(s) {
  starting <- league_settings[[as.character(s)]]$starting
  tibble(season = s, position = c("QB", "RB", "WR", "TE", "K", "DST")) |>
    mutate(n_dedicated = map_int(position, \(p) sum(starting == p)))
}) |>
  bind_rows()

starters <- starters |>
  left_join(dedicated, by = join_by(season, position)) |>
  mutate(n_pos = n(), .by = c(season, week, roster_id, position)) |>
  mutate(n_qb = sum(position == "QB", na.rm = TRUE), .by = c(season, week, roster_id)) |>
  left_join(
    dedicated |> filter(position == "QB") |> select(season, n_qb_slots = n_dedicated),
    by = join_by(season)
  ) |>
  mutate(
    open_slot = case_when(
      player_id == "0" ~ slot,
      position == "QB" & n_qb > n_qb_slots ~ "super_flex",
      position %in% c("RB", "WR", "TE") & n_pos > n_dedicated & n_qb <= n_qb_slots ~ "super_flex",
      position %in% c("RB", "WR", "TE") & n_pos > n_dedicated ~ "flex",
      .default = position
    )
  )

bench <- lineups |>
  filter(!starter, !inactive, !is.na(position))

# each team-week's benchings: pairs of a starter (not an ERS) and an eligible bench player
# projected higher by more than the threshold, taken largest gap first, each player once
questionable <- starters |>
  filter(is.na(reason)) |>
  select(season, week, roster_id, player_id, position, open_slot, projection, points) |>
  inner_join(slot_map, by = join_by(open_slot == slot), suffix = c("", "_eligible"), relationship = "many-to-many") |>
  inner_join(
    bench |> select(season, week, roster_id, bench_id = player_id, position_eligible = position, bench_projection = projection, bench_points = points),
    by = join_by(season, week, roster_id, position_eligible),
    relationship = "many-to-many"
  ) |>
  mutate(gap = bench_projection - projection) |>
  filter(gap > qb_threshold[position]) |>
  arrange(desc(gap)) |>
  group_by(season, week, roster_id) |>
  group_modify(\(pairs, key) {
    used_starters <- character()
    used_bench <- character()
    keep <- logical(nrow(pairs))
    for (i in seq_len(nrow(pairs))) {
      if (!pairs$player_id[i] %in% used_starters && !pairs$bench_id[i] %in% used_bench) {
        keep[i] <- TRUE
        used_starters <- c(used_starters, pairs$player_id[i])
        used_bench <- c(used_bench, pairs$bench_id[i])
      }
    }
    pairs[keep, ]
  }) |>
  ungroup()

# for each ERS, the best-projected eligible bench player not already behind a benching
ers_fill <- starters |>
  filter(!is.na(reason)) |>
  select(season, week, roster_id, player_id, slot, points) |>
  inner_join(slot_map, by = join_by(slot), relationship = "many-to-many") |>
  inner_join(
    bench |> select(season, week, roster_id, bench_id = player_id, position, bench_projection = projection, bench_points = points),
    by = join_by(season, week, roster_id, position),
    relationship = "many-to-many"
  ) |>
  anti_join(questionable |> select(season, week, roster_id, bench_id), by = join_by(season, week, roster_id, bench_id)) |>
  arrange(desc(bench_projection)) |>
  group_by(season, week, roster_id) |>
  group_modify(\(pairs, key) {
    used_starters <- character()
    used_bench <- character()
    keep <- logical(nrow(pairs))
    for (i in seq_len(nrow(pairs))) {
      if (!pairs$player_id[i] %in% used_starters && !pairs$bench_id[i] %in% used_bench) {
        keep[i] <- TRUE
        used_starters <- c(used_starters, pairs$player_id[i])
        used_bench <- c(used_bench, pairs$bench_id[i])
      }
    }
    pairs[keep, ]
  }) |>
  ungroup()

# Max points -----------------------------------------------------------------------------
# the best lineup by actual points: each position's dedicated slots, then the FLEX, then
# the SUPER_FLEX, from the players not on IR or the taxi squad

best_lineup_points <- function(players, starting) {
  remaining <- players |> arrange(desc(points))
  total <- 0
  take <- function(eligible, n) {
    picked <- remaining |> filter(position %in% eligible) |> slice_head(n = n)
    remaining <<- remaining |> filter(!player_id %in% picked$player_id)
    total <<- total + sum(picked$points)
  }
  for (p in c("QB", "RB", "WR", "TE", "K", "DST")) take(p, sum(starting == p))
  take(c("RB", "WR", "TE"), sum(starting == "flex"))
  take(c("QB", "RB", "WR", "TE"), sum(starting == "super_flex"))
  total
}

max_points <- lineups |>
  filter(!inactive, player_id != "0", !is.na(position)) |>
  group_by(season, week, roster_id) |>
  group_modify(\(players, key) {
    tibble(max_points = best_lineup_points(players, league_settings[[as.character(key$season)]]$starting))
  }) |>
  ungroup()

# Results -------------------------------------------------------------------------------

results <- matchups_table |>
  filter(round == "regular season") |>
  select(season, week, roster_id, opp_points)

swings <- bind_rows(
  questionable |> transmute(season, week, roster_id, swing = bench_points - points),
  ers_fill |> transmute(season, week, roster_id, swing = bench_points - points)
) |>
  summarize(swing = sum(swing), .by = c(season, week, roster_id))

# Transactions ------------------------------------------------------------------------

transactions <- map(seasons, \(s) {
  t <- readRDS(sleeper_file("transactions", s))$data
  last_week <- league_settings[[as.character(s)]]$last_week
  t |>
    mutate(
      season = s,
      time = as_datetime(created / 1000),
      week = pmax(as.numeric(leg), 1)
    ) |>
    filter(time >= as_datetime(str_c(s, "-09-01"), tz = "America/Chicago"), week <= last_week)
}) |>
  bind_rows()

added <- function(t) {
  if (nrow(t) == 0) {
    return(tibble(transaction_id = character(), player_id = character(), roster_id = integer()))
  }
  t |>
    select(transaction_id, adds) |>
    unpack(adds) |>
    pivot_longer(-transaction_id, names_to = "player_id", values_to = "roster_id", values_drop_na = TRUE)
}

moves <- bind_rows(
  # trades: one for each team in the trade
  transactions |>
    filter(type == "trade", status == "complete") |>
    select(season, week, roster_ids) |>
    unnest(roster_ids) |>
    transmute(season, week, roster_id = as.integer(roster_ids), kind = "trades"),
  # waiver claims and free agent pickups whose player was projected that week
  transactions |>
    filter(type %in% c("waiver", "free_agent"), status == "complete") |>
    (\(t) {
      added(t) |>
        distinct(transaction_id, .keep_all = TRUE) |>
        left_join(t |> select(transaction_id, season, week), by = join_by(transaction_id))
    })() |>
    left_join(projections, by = join_by(season, week, player_id)) |>
    filter(coalesce(projection, 0) > 0) |>
    transmute(season, week, roster_id = as.integer(roster_id), kind = "waivers"),
  # failed waiver claims
  transactions |>
    filter(type == "waiver", status == "failed") |>
    select(season, week, roster_ids) |>
    unnest(roster_ids) |>
    transmute(season, week, roster_id = as.integer(roster_ids), kind = "bids")
) |>
  count(season, week, roster_id, kind) |>
  pivot_wider(names_from = kind, values_from = n, values_fill = 0)
for (kind in names(transaction_points)) {
  if (!kind %in% names(moves)) moves[[kind]] <- 0L
}

# Weekly scores -------------------------------------------------------------------------

activity_weekly <- lineups |>
  distinct(season, week, roster_id, points = team_points) |>
  left_join(users, by = join_by(roster_id)) |>
  left_join(max_points, by = join_by(season, week, roster_id)) |>
  left_join(
    starters |> filter(!is.na(reason)) |> count(season, week, roster_id, name = "ers"),
    by = join_by(season, week, roster_id)
  ) |>
  left_join(
    questionable |> count(season, week, roster_id, name = "qb"),
    by = join_by(season, week, roster_id)
  ) |>
  left_join(swings, by = join_by(season, week, roster_id)) |>
  left_join(results, by = join_by(season, week, roster_id)) |>
  left_join(moves, by = join_by(season, week, roster_id)) |>
  mutate(
    across(c(ers, qb, swing, trades, waivers, bids), \(x) coalesce(as.numeric(x), 0)),
    efficiency = if_else(max_points > 0, 10 * points / max_points, 10),
    won = points > opp_points,
    # a loss the penalized slots, filled, would have won
    odn = as.numeric(!won & ers + qb > 0 & points + swing > opp_points),
    adjusted_won = won | odn == 1,
    weekly_score = efficiency -
      penalties[["ers"]] * ers -
      penalties[["qb"]] * qb -
      penalties[["odn"]] * odn +
      transaction_points[["trades"]] * trades +
      transaction_points[["waivers"]] * waivers +
      transaction_points[["bids"]] * bids
  ) |>
  arrange(season, week, roster_id) |>
  mutate(
    rolling_score = rolling_sum(weekly_score, rolling_weeks),
    .by = roster_id
  )

# Penalty log ----------------------------------------------------------------------------

name_of <- function(id) {
  coalesce(player_info$name[match(id, player_info$player_id)], if_else(id == "0", "(empty)", id))
}
slot_label <- c(QB = "QB", RB = "RB", WR = "WR", TE = "TE", K = "K", DST = "DEF",
                flex = "FLEX", super_flex = "SUPER_FLEX")

activity_penalties <- bind_rows(
  starters |>
    filter(!is.na(reason)) |>
    left_join(
      ers_fill |> select(season, week, roster_id, player_id, bench_id, bench_projection, bench_points),
      by = join_by(season, week, roster_id, player_id)
    ) |>
    transmute(
      season, week, roster_id, penalty = "ERS", slot = slot_label[slot], started = name_of(player_id),
      started_projection = projection, started_points = points, reason,
      bench = if_else(is.na(bench_id), NA, name_of(bench_id)), bench_projection, bench_points
    ),
  questionable |>
    left_join(starters |> select(season, week, roster_id, player_id, slot), by = join_by(season, week, roster_id, player_id)) |>
    transmute(
      season, week, roster_id, penalty = "QB", slot = slot_label[slot], started = name_of(player_id),
      started_projection = projection, started_points = points,
      reason = str_c(round(gap, 1), " below his bench option"),
      bench = name_of(bench_id), bench_projection, bench_points
    )
) |>
  left_join(
    activity_weekly |> select(season, week, roster_id, team, decided = odn),
    by = join_by(season, week, roster_id)
  ) |>
  mutate(decided = decided == 1) |>
  select(season, week, team, penalty, slot, started, started_projection, started_points,
         reason, bench, bench_projection, bench_points, decided) |>
  arrange(desc(season), desc(week), team, penalty)

activity_weekly <- activity_weekly |>
  select(season, week, roster_id, team, points, max_points, efficiency, ers, qb, odn,
         trades, waivers, bids, weekly_score, won, adjusted_won, rolling_score)

# dfs to save -------------------------------------------------------------

write_csv(activity_weekly, here("Shiny/Saved Files/activity_weekly.csv"))
write_csv(activity_penalties, here("Shiny/Saved Files/activity_penalties.csv"))
