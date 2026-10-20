# Scraping
# Pulls every external input: NFL box scores (nflverse), Sleeper league data,
# FFToday projections, and KeepTradeCut values.
# Past Sleeper seasons are cached in Data/raw/sleeper and never re-pulled.

message("begin Scraping...")

suppressPackageStartupMessages({
  library("here")
})

options(nflreadr.verbose = FALSE)

source(here("Data Manipulation/Scrape Support.R"))

# Settings ----------------------------------------------------------------

# one Sleeper league id per season: add the new season's id here at rollover
league_ids <- c(
  "2024" = "1066207868321370112",
  "2025" = "1180629816344895488",
  "2026" = "1312620216915595264"
)
all_league_ids <- unname(league_ids)
league_seasons <- as.numeric(names(league_ids))
current_season <- max(league_seasons)
current_league_id <- league_ids[[as.character(current_season)]]

n_future_draft_seasons <- 3 # tradeable future rookie drafts (picks can be traded three years out)

# force a player_info refresh (it also refreshes on its own when a rostered
# player is missing). Sleeper asks that /players/nfl be called at most once a day.
run_player_info <- FALSE

# Sleeper cache ------------------------------------------------------------
# past seasons never change, so they are pulled once and stored in Data/raw/sleeper.
# the current season is re-pulled every run and saved there as well, so that
# Player Value Added.R can read it from disk (complete = FALSE until the season is over).
sleeper_cache_dir <- here("Data/raw/sleeper")
dir.create(sleeper_cache_dir, recursive = TRUE, showWarnings = FALSE)

# default completeness check: something non-empty came back
has_content <- function(data) {
  if (is.data.frame(data)) {
    return(nrow(data) > 0)
  }
  length(data) > 0 && any(map_lgl(data, \(d) NROW(d) > 0))
}

# every week of a weekly list returned data (used for matchups)
all_weeks_present <- function(data) {
  length(data) > 0 && all(map_lgl(data, \(d) NROW(d) > 0))
}

cached_season <- function(type, season, fetch, is_complete = has_content) {
  path <- file.path(sleeper_cache_dir, str_c(type, "_", season, ".rds"))
  if (file.exists(path)) {
    cache <- readRDS(path)
    # a frozen past season is reused only if it still passes the completeness check
    if (isTRUE(cache$complete) && is_complete(cache$data)) {
      return(cache$data)
    }
  }
  data <- fetch()
  saveRDS(
    list(
      data = data,
      complete = season < current_season && is_complete(data),
      fetched = Sys.time()
    ),
    path
  )
  data
}

# League settings ---------------------------------------------------------
# scoring, lineup slots, and settings for each season (past seasons cached)

league_info <- map(
  seq_along(all_league_ids),
  ~ cached_season(
    "league",
    league_seasons[[.x]],
    \() fetch_sleeper_league(all_league_ids[[.x]]),
    is_complete = \(d) !is.null(d$settings) && !is.null(d$roster_positions)
  )
)

walk2(league_info, league_seasons, \(info, s) {
  if (!is.null(info$season) && !isTRUE(info$season == s)) {
    warning("league id listed for ", s, " is Sleeper's ", info$season, " league")
  }
})

current_settings <- league_info[[length(league_info)]]$settings
setting <- function(name, default) {
  value <- current_settings[[name]]
  if (is.null(value) || is.na(value)) default else as.numeric(value)
}

playoff_week_start <- setting("playoff_week_start", 15)
playoff_teams <- setting("playoff_teams", 6)
draft_rounds <- setting("draft_rounds", 3)
if (setting("playoff_round_type", 0) != 0) {
  warning("playoff rounds longer than one week are not handled, check league_weeks")
}
# weeks with fantasy matchups: regular season + one week per playoff round
league_weeks <- seq_len(playoff_week_start - 1 + ceiling(log2(playoff_teams)))

# NFL box scores -----------------------------------------------------------
# https://www.nflfastr.com

box_score_off_current <- tryCatch(
  nflreadr::load_player_stats(seasons = current_season) |>
    select(-contains("_list")),
  error = function(e) {
    message("no ", current_season, " player stats yet: ", conditionMessage(e))
    tibble()
  }
)

# columns Player Value Added.R (fallback scoring) and player_info rely on
team_stat_cols <- c(
  "season", "week", "team", "opponent_team", "passing_tds", "rushing_tds",
  "fumble_recovery_tds", "special_teams_tds", "passing_2pt_conversions",
  "rushing_2pt_conversions", "fg_made", "pat_made", "sack_fumbles_lost",
  "rushing_fumbles_lost", "receiving_fumbles_lost", "fg_blocked", "pat_blocked",
  "def_tds", "def_sacks", "def_interceptions", "def_fumbles_forced", "def_safeties"
)

# precomputed team stats are much faster than aggregating play-by-play
box_score_def_current <- tryCatch(
  nflreadr::load_team_stats(seasons = current_season, summary_level = "week") |>
    filter(season_type == "REG"),
  error = function(e) NULL
)

if (is.null(box_score_def_current) || !all(team_stat_cols %in% names(box_score_def_current))) {
  box_score_def_current <- tryCatch(
    nflfastR::calculate_stats(
      seasons = current_season,
      summary_level = "week",
      stat_type = "team",
      season_type = "REG"
    ),
    error = function(e) {
      message("no ", current_season, " team stats yet: ", conditionMessage(e))
      tibble()
    }
  )
}
box_score_def_current <- box_score_def_current |> select(-contains("_list"))

# keep last saved files if nothing has been played yet
if (nrow(box_score_off_current) > 0) {
  write_csv(
    box_score_off_current,
    here(str_c("Data/raw/box_scores/box_score_off", current_season %% 100, ".csv"))
  )
}
if (nrow(box_score_def_current) > 0) {
  write_csv(
    box_score_def_current,
    here(str_c("Data/raw/box_scores/box_score_def", current_season %% 100, ".csv"))
  )
}

# NFL weeks with stats so far (empty before week 1)
nfl_weeks_with_stats <- if (nrow(box_score_off_current) > 0) {
  sort(unique(box_score_off_current$week))
} else {
  numeric()
}

# Sleeper API ----------------------------------------------------
# https://docs.sleeper.com

# rosters
rosters <- parse_api(str_c(
  "https://api.sleeper.app/v1/league/",
  current_league_id,
  "/rosters"
)) |>
  select(roster_id, owner_id, players) |>
  unnest(cols = c(players)) |>
  rename(player_id = players)

write_csv(rosters, here("Data/derived/current_roster.csv"))

# users (one call), joined to rosters by user id
league_users <- parse_api(str_c(
  "https://api.sleeper.app/v1/league/",
  current_league_id,
  "/users"
))

users <- rosters |>
  select(owner_id, roster_id) |>
  distinct() |>
  left_join(
    league_users |> select(display_name, user_id),
    by = join_by(owner_id == user_id)
  )

write_csv(users, here("Shiny/Saved Files/users.csv"))

avatar <- league_users |>
  mutate(avatar_url = str_c("https://sleepercdn.com/avatars/", avatar)) |>
  left_join(users |> select(owner_id, roster_id), by = join_by(user_id == owner_id)) |>
  select(roster_id, display_name, avatar_url)

write_csv(avatar, here("Shiny/Saved Files/avatar.csv"))

# Player Information --------------------------------------------------------
# refreshed when forced, or when a rostered player is missing from the saved file

player_info_path <- here("Data/raw/player_info.csv")

player_info <- if (file.exists(player_info_path)) {
  read_csv(
    player_info_path,
    show_col_types = FALSE,
    col_types = cols(player_id = col_character())
  )
} else {
  NULL
}

missing_players <- setdiff(rosters$player_id, player_info$player_id)

if (run_player_info || is.null(player_info) || length(missing_players) > 0) {
  message(
    "refreshing player_info",
    if (length(missing_players) > 0) {
      str_c(" (missing rostered ids: ", str_flatten_comma(missing_players), ")")
    }
  )

  sleeper_players <- parse_api_list("https://api.sleeper.app/v1/players/nfl")
  fantasy_pos <- c("QB", "RB", "WR", "TE", "K")

  # one row per player: primary position if it is a fantasy position, otherwise
  # his first fantasy-eligible position (e.g. a FB listed as RB)
  player_information <- tibble(
    name = map_chr(sleeper_players, "full_name", .default = NA_character_),
    player_id = map_chr(sleeper_players, "player_id", .default = NA_character_),
    position = map_chr(sleeper_players, \(p) {
      eligible <- intersect(c(p$position, unlist(p$fantasy_positions)), fantasy_pos)
      if (length(eligible) > 0) eligible[[1]] else NA_character_
    }),
    birth_date = map_chr(sleeper_players, "birth_date", .default = NA_character_),
    years_exp = map_dbl(sleeper_players, "years_exp", .default = NA_real_)
  )

  # all 32 defenses (from the box scores, or the saved file before week 1)
  defenses <- if (nrow(box_score_def_current) > 0) {
    box_score_def_current |>
      distinct(name = team) |>
      mutate(
        name = recode(name, "LA" = "LAR"),
        player_id = name,
        position = "DST"
      )
  } else {
    player_info |>
      filter(position == "DST") |>
      select(name, player_id, position)
  }

  player_info <- player_information |>
    filter(position %in% fantasy_pos) |>
    # remove duplicate names
    filter(player_id %!in% c("4634", "748", "232", "7437", "638")) |>
    mutate(
      position = case_when(
        name == "Taysom Hill" ~ "TE",
        .default = position
      ),
      birth_date = case_when(
        name == "Efton Chism" ~ "2001-10-26",
        name == "Isaiah Bond" ~ "2004-03-15",
        name == "Jam Miller" & is.na(birth_date) ~ "2004-04-29",
        name == "Le'Veon Moss" & is.na(birth_date) ~ "2002-11-15",
        .default = birth_date
      )
    ) |>
    distinct(player_id, .keep_all = TRUE) |>
    bind_rows(defenses) |>
    name_correction() |>
    mutate(birth_date = ymd(birth_date))

  still_missing <- setdiff(rosters$player_id, player_info$player_id)
  if (length(still_missing) > 0) {
    warning(
      "rostered players with no fantasy position in Sleeper's data: ",
      str_flatten_comma(still_missing)
    )
  }

  write_csv(player_info, player_info_path)
  rm(sleeper_players, player_information, defenses, fantasy_pos, still_missing)
}

# Headshots -----------------------------------------------------------------
# keyed on Sleeper id so every player in player_info gets one: nflverse headshots
# (via nflreadr's id crosswalk), then this season's box scores by name, then
# Sleeper's own image

nflverse_headshots <- tryCatch(
  {
    ids <- nflreadr::load_ff_playerids() |>
      transmute(gsis_id, player_id = as.character(sleeper_id)) |>
      filter(!is.na(gsis_id), !is.na(player_id)) |>
      distinct(gsis_id, .keep_all = TRUE)
    players <- nflreadr::load_players()
    headshot_col <- intersect(c("headshot", "headshot_url"), names(players))[[1]]

    players |>
      transmute(gsis_id, headshot_url = .data[[headshot_col]]) |>
      filter(!is.na(headshot_url), headshot_url != "") |>
      inner_join(ids, by = join_by(gsis_id)) |>
      distinct(player_id, .keep_all = TRUE) |>
      select(player_id, headshot_url)
  },
  error = function(e) {
    message("nflverse headshots unavailable: ", conditionMessage(e))
    tibble(player_id = character(), headshot_url = character())
  }
)

box_score_headshots <- if (nrow(box_score_off_current) > 0) {
  box_score_off_current |>
    transmute(name = player_display_name, position, box_url = headshot_url) |>
    filter(!is.na(box_url)) |>
    name_correction() |>
    distinct(name, position, .keep_all = TRUE)
} else {
  tibble(name = character(), position = character(), box_url = character())
}

player_headshot <- player_info |>
  filter(position != "DST") |>
  left_join(nflverse_headshots, by = join_by(player_id)) |>
  left_join(box_score_headshots, by = join_by(name, position)) |>
  mutate(
    headshot_url = coalesce(
      headshot_url,
      box_url,
      str_c("https://sleepercdn.com/content/nfl/players/", player_id, ".jpg")
    )
  ) |>
  distinct(player_id, .keep_all = TRUE) |>
  select(player_id, name, headshot_url)

player_headshot |> write_csv(here("Shiny/Saved Files/player_headshot.csv"))

# matchups (raw weekly output, cached by season)
matchups <- map(
  seq_along(all_league_ids),
  ~ {
    league_id <- all_league_ids[[.x]]
    cached_season(
      "matchups",
      league_seasons[[.x]],
      \() {
        map(
          league_weeks,
          \(wk) {
            str_c(
              "https://api.sleeper.app/v1/league/",
              league_id,
              "/matchups/",
              wk
            ) |>
              parse_api()
          }
        )
      },
      is_complete = all_weeks_present
    )
  }
)

matchups_temp <- map(matchups, ~ bind_rows(.x, .id = "week")) |>
  bind_rows(.id = "season") |>
  mutate(
    season = league_seasons[as.numeric(season)],
    week = as.numeric(week)
  )

# exact league points for every NFL player (rostered or not), from Sleeper's weekly
# stats and the league's scoring settings. Only weeks with fantasy points are pulled.
walk(
  seq_along(all_league_ids),
  ~ {
    season <- league_seasons[[.x]]
    scoring <- league_info[[.x]]$scoring
    weeks_played <- matchups[[.x]] |>
      keep(\(m) NROW(m) > 0 && sum(m$points) != 0) |>
      length() |>
      seq_len()

    cached_season(
      "stats_points",
      season,
      \() {
        map(weeks_played, \(wk) {
          stats <- fetch_sleeper_stats(season, wk)
          if (is.null(stats)) {
            message("Sleeper stats unavailable for ", season, " week ", wk)
            return(NULL)
          }
          score_sleeper_stats(stats, scoring) |>
            mutate(season = season, week = wk)
        }) |>
          bind_rows()
      },
      # every played week must be present before a past season is frozen
      is_complete = \(d) NROW(d) > 0 && all(weeks_played %in% d$week)
    )
  }
)

# playoff bracket (cached by season)
playoff_bracket <- map(
  seq_along(all_league_ids),
  ~ cached_season(
    "winners_bracket",
    league_seasons[[.x]],
    \() {
      parse_api(str_c(
        "https://api.sleeper.app/v1/league/",
        all_league_ids[[.x]],
        "/winners_bracket"
      ))
    }
  )
) |>
  map(\(bracket) {
    if (!"w" %in% names(bracket)) {
      return(tibble()) # bracket not set yet
    }
    bracket |>
      filter(!is.na(w)) |>
      transmute(
        roster_id = t1,
        opponent_id = t2,
        round = case_when(
          r == 1 ~ "1st round",
          r == 2 & is.na(p) ~ "2nd round",
          p == 5 ~ "5th place",
          p == 1 ~ "Championship",
          p == 3 ~ "3rd place",
          .default = NA
        ),
        week = r + playoff_week_start - 1
      )
  }) |>
  bind_rows(.id = "season") |>
  mutate(season = league_seasons[as.numeric(season)])

matchups_table <- matchups_temp |>
  mutate(
    opponent_id = roster_id,
    opp_points = points
  ) |>
  select(season, week, opp_points, opponent_id, matchup_id) |>
  left_join(
    matchups_temp |> select(season, week, points, roster_id, matchup_id),
    by = join_by(season, week, matchup_id),
    relationship = "many-to-many"
  ) |>
  filter(opponent_id != roster_id, !is.na(matchup_id)) |>
  left_join(
    playoff_bracket |> select(-roster_id),
    by = join_by(season, week, roster_id == opponent_id)
  ) |>
  left_join(
    playoff_bracket |> select(-opponent_id),
    by = join_by(season, week, roster_id == roster_id)
  ) |>
  mutate(
    round = case_when(
      !is.na(round.x) ~ round.x,
      !is.na(round.y) ~ round.y,
      # playoff weeks not played yet (bracket unknown)
      week >= playoff_week_start & points == 0 & opp_points == 0 ~ "playoffs (not played)",
      week >= playoff_week_start ~ "loser's bracket",
      .default = "regular season"
    )
  ) |>
  select(season, week, round, roster_id, points, opponent_id, opp_points)

write_csv(matchups_table, here("Data/derived/matchups_table.csv"))

# transactions (this includes trades), cached by season
# (offseason transactions are stored by Sleeper under week 1)
transactions <- map(
  seq_along(all_league_ids),
  ~ {
    league_id <- all_league_ids[[.x]]
    cached_season("transactions", league_seasons[[.x]], \() {
      map(
        league_weeks,
        \(wk) {
          str_c(
            "https://api.sleeper.app/v1/league/",
            league_id,
            "/transactions/",
            wk
          ) |>
            parse_api()
        }
      ) |>
        bind_rows(.id = "week")
    })
  }
) |>
  bind_rows(.id = "season") |>
  mutate(season = league_seasons[as.numeric(season)])

saveRDS(transactions, here("Data/derived/transactions.rds"))

# Drafts --------------------------------------------------------------------
# each draft is identified by its own season and number of rounds (not by its
# position in Sleeper's list). Past seasons' drafts are cached.

fetch_league_drafts <- function(league_id) {
  parse_api_raw(str_c("https://api.sleeper.app/v1/league/", league_id, "/drafts")) |>
    map(\(d) {
      detail <- parse_api_raw(str_c("https://api.sleeper.app/v1/draft/", d$draft_id))
      list(
        draft_id = d$draft_id,
        season = as.numeric(d$season),
        status = d$status,
        rounds = as.numeric(d$settings$rounds),
        slot_to_roster_id = detail$slot_to_roster_id,
        draft_order = detail$draft_order,
        picks = parse_api(str_c(
          "https://api.sleeper.app/v1/draft/",
          d$draft_id,
          "/picks"
        ))
      )
    })
}

drafts <- map(
  seq_along(all_league_ids),
  ~ cached_season(
    "drafts",
    league_seasons[[.x]],
    \() fetch_league_drafts(all_league_ids[[.x]]),
    is_complete = \(d) length(d) > 0 && all(map_chr(d, "status") == "complete")
  )
) |>
  list_flatten()

draft_meta <- tibble(
  index = seq_along(drafts),
  season = map_dbl(drafts, "season"),
  rounds = map_dbl(drafts, "rounds"),
  status = map_chr(drafts, "status")
) |>
  mutate(type = if_else(rounds > draft_rounds, "veteran", "rookie")) |>
  # order downstream scripts expect: by season, the rookie draft before the startup
  arrange(season, rounds)

drafts <- drafts[draft_meta$index]

# season and type of each draft, in the same order as draft_picks.rds (draft_id links the two)
draft_meta |>
  transmute(draft_id = map_chr(drafts, "draft_id"), season, type, rounds, status) |>
  write_csv(here("Data/derived/drafts.csv"))

# Get picks in a draft
draft_picks <- map(drafts, "picks")
saveRDS(draft_picks, here("Data/derived/draft_picks.rds"))

# draft slot of each roster (slot_to_roster_id, or draft_order by user if not set)
draft_order <- map2(drafts, draft_meta$type, \(d, type) {
  if (length(d$slot_to_roster_id) > 0) {
    order <- tibble(
      draft_order = as.numeric(names(d$slot_to_roster_id)),
      roster_id = map_dbl(d$slot_to_roster_id, \(r) r %||% NA_real_)
    )
  } else if (length(d$draft_order) > 0) {
    order <- tibble(
      owner_id = names(d$draft_order),
      draft_order = as.numeric(unlist(d$draft_order))
    ) |>
      left_join(users, by = join_by(owner_id))
  } else {
    return(tibble())
  }
  order |>
    filter(!is.na(roster_id)) |>
    transmute(roster_id, draft_order, season = d$season, type = type)
}) |>
  bind_rows() |>
  arrange(season, desc(type == "rookie"), draft_order)

write_csv(draft_order, here("Data/derived/draft_order.csv"))

# Now, I want to compile each players total assets

# future rookie drafts start the season after the last completed rookie draft
first_future_draft <- draft_meta |>
  filter(type == "rookie", status == "complete") |>
  pull(season) |>
  c(min(league_seasons) - 1) |>
  max() +
  1

# assigned picks
assigned_picks <- expand_grid(
  roster_id = sort(unique(rosters$roster_id)),
  round = seq_len(draft_rounds),
  season = first_future_draft:(first_future_draft + n_future_draft_seasons - 1)
)

# traded picks (past seasons cached)
traded_picks <- map(
  seq_along(all_league_ids),
  ~ cached_season(
    "traded_picks",
    league_seasons[[.x]],
    \() {
      str_c(
        "https://api.sleeper.app/v1/league/",
        all_league_ids[[.x]],
        "/traded_picks"
      ) |>
        parse_api()
    }
  )
) |>
  keep(\(tp) nrow(tp) > 0) |>
  map(\(tp) mutate(tp, season = as.numeric(season))) |>
  bind_rows(
    # typed template, so the joins below work even if no picks were ever traded
    tibble(
      season = double(),
      round = integer(),
      roster_id = integer(),
      owner_id = integer(),
      previous_owner_id = integer()
    )
  ) |>
  mutate(index = row_number()) |>
  arrange(desc(index)) |>
  distinct(round, season, roster_id, .keep_all = TRUE) |>
  select(-index)

lost_picks <- assigned_picks |>
  inner_join(traded_picks, by = join_by(round, season, roster_id)) |>
  select(roster_id, round, season)

gained_picks <- assigned_picks |>
  inner_join(
    traded_picks,
    by = join_by(round, season, roster_id == owner_id)
  ) |>
  rename(pick_slot = roster_id.y) |>
  select(-previous_owner_id)

future_draft_picks <- assigned_picks |>
  anti_join(lost_picks, by = join_by(roster_id, round, season)) |>
  mutate(pick_slot = roster_id) |>
  bind_rows(gained_picks) |>
  left_join(
    draft_order |> select(-type),
    by = join_by(season, pick_slot == roster_id)
  ) |>
  arrange(season, round, roster_id)

write_csv(future_draft_picks, here("Data/derived/future_draft_picks.csv"))

# Scrape Projections ----------------------------------------------------
# need weekly player ranking for above replacement metric
# https://www.fftoday.com/rankings/playerwkproj.php?Season=2026&GameWeek=1&PosID=10&LeagueID=208518

# only scrape fantasy weeks with box scores that are not already saved (past
# projections don't change). Nothing is scraped before week 1.
proj_path <- here(str_c("Data/raw/projections/projections", current_season %% 100, ".csv"))
existing <- if (file.exists(proj_path)) {
  read_csv(proj_path, show_col_types = FALSE)
} else {
  tibble(name = character(), projection = double(), week = double(), position = character())
}

weeks_needed <- intersect(nfl_weeks_with_stats, league_weeks) |>
  setdiff(existing$week)

# minimum players per position for a scraped week to be saved (bye weeks included)
min_projected <- c(QB = 20, RB = 35, WR = 50, TE = 20, K = 18, DST = 18)

new_proj <- map(weeks_needed, \(wk) {
  week_proj <- tryCatch(
    combine_week(wk, current_season) |> name_correction(),
    error = function(e) {
      warning("projection scrape failed for week ", wk, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(week_proj)) {
    return(NULL)
  }

  counts <- table(factor(week_proj$position, levels = names(min_projected)))
  short <- names(min_projected)[counts < min_projected]
  if (length(short) > 0) {
    warning(
      "week ", wk, " projections look incomplete (", str_flatten_comma(short),
      "), not saved, will retry next run"
    )
    return(NULL)
  }
  week_proj
}) |>
  bind_rows()

if (nrow(new_proj) > 0) {
  write_csv(bind_rows(existing, new_proj), proj_path)
}

# older seasons' projections (Data/raw/projections/projections24.csv, ...) are read directly by
# Player Value Added.R

# Scrape Future Value -----------------------------------------------------
# https://keeptradecut.com/dynasty-rankings

# # keep_trade_cut value 8/23 (only top 50 players)
# t50 <- player_value("http://web.archive.org/web/20240823002052/https://keeptradecut.com/dynasty-rankings")
#
# # need to add more than just top 50
# b51 <- tibble(
#   name = c("James Cook", "Tee Higgins", "Mark Andrews", "Jordan Addison", "Drake Maye", "Rashee Rice",
#            "Kyle Pitts", "J.J. McCarthy", "Trey Benson", "Bryce Young", "Jayden Reed", "Xavier Worthy", "Joe Mixon",
#            "T.J. Hockenson", "Derrick Henry", "David Montgomery", "Stefon Diggs", "Cooper Kupp", "Javonte Williams",
#            "Amari Cooper", "Tony Pollard", "Terry McLaurin", "Bo Nix", "Zamir White", "George Kittle", "Keon Coleman",
#            "Brian Robinson Jr.", "Blake Corum", "Christian Watson", "Aaron Jones", "Calvin Ridley", "Jake Ferguson",
#            "Jameson Williams", "Ricky Pearsall", "James Conner", "Adonai Mitchell", "MarShawn Lloyd", "Xavier Legette",
#            "Aaron Rodgers", "Austin Ekeler", "Kendre Miller", "DeAndre Hopkins", "Zack Moss", "Luke Musgrave", "Romeo Doubs",
#            "Jerry Jeudy", "Raheem Mostert", "Jerome Ford", "Ray Davis", "Ja'Lynn Polk", "Jakobi Meyers", "Dontayvion Wicks",
#            "Derek Carr", "Marvin Mims Jr.", "Isaiah Likely", "Roschon Johnson", "Joshua Palmer", "Michael Mayer",
#            "Kimani Vidal", "Mike Williams", "Gus Edwards", "Rashid Shaheed", "Tyler Allgeier", "Antonio Gibson",
#            "Chigoziem Okonkwo", "Khalil Herbert", "Jaleel McLaughlin", "DeMario Douglas", "Brandin Cooks", "Rashod Bateman",
#            "Jalen McMillan", "Darnell Mooney", "Braelon Allen", "Ty Chandler", "Jonathan Mingo", "Greg Dulcich",
#            "Treylon Burks", "Juwan Johnson", "Adam Thielen", "Tyrone Tracy Jr.", "Michael Pittman Jr.", "Kyren Williams",
#            "DK Metcalf", "Isiah Pacheco", "Rachaad White", "Kenneth Walker III", "Zay Flowers", "Jonathon Brooks", "Jared Goff",
#            "Josh Jacobs", "George Pickens", "Deebo Samuel Sr.", "Jaxon Smith-Njigba", "D'Andre Swift",
#            "Deshaun Watson", "Rhamondre Stevenson", "Davante Adams", "Brian Thomas Jr.", "Mike Evans", "Travis Kelce",
#            "Christian Kirk", "Ladd McConkey", "Will Levis", "Tyjae Spears", "Alvin Kamara", "Najee Harris",
#            "Diontae Johnson", "Baker Mayfield", "Chris Godwin", "Jaylen Warren", "Kirk Cousins", "Evan Engram",
#            "David Njoku", "Matthew Stafford", "Hollywood Brown", "Keenan Allen", "Zach Charbonnet", "Nick Chubb",
#            "Courtland Sutton", "Dallas Goedert", "Josh Downs", "Chase Brown", "Pat Freiermuth", "Michael Penix Jr.",
#            "Devin Singletary", "Jahan Dotson", "Jaylen Wright", "Ben Sinnott", "Cole Kmet", "Geno Smith",
#            "Dalton Schultz", "Roman Wilson", "Justin Fields", "Daniel Jones", "Jermaine Burton", "Tyler Lockett",
#            "Troy Franklin", "Chuba Hubbard", "Khalil Shakir", "Quentin Johnson", "Michael Wilson", "Gabe Davis",
#            "Malachi Corley", "Tucker Kraft", "Bucky Irving", "Russell Wilson", "Cade Otton", "Javon Baker",
#            "Curtis Samuel", "Ja'Tavion Sanders", "Wan'Dale Robinson", "Audric Estime", "Elijah Moore", "Jalin Hyatt",
#            "Dameon Pierce", "Keaton Mitchell", "Noah Fant", "Will Shipley", "Ezekiel Elliot", "J.K. Dobbins",
#            "Luke McCaffrey", "Hunter Henry", "Rico Dowdle", "Theo Johnson", "Jelani Woods", "Tank Bigsby", "Elijah Mitchell",
#            "A.J. Dillon", "Devontez Walker", "Isaac Guerendo", "Dylan Laube", "Alexander Mattison", "Trey Palmer", "Tyler Boyd",
#            "Cedric Tillman", "Miles Sanders", "Zay Jones", "Dawson Knox", "Taysom Hill", "Andrei Iosivas", "Gerald Everett",
#            "Kenneth Gainwell", "Mike Gesicki", "Alec Pierce", "Tyler Higbee", "D.J. Chark Jr.", "Odell Beckham Jr.",
#            "Tyler Conklin", "D'Onta Foreman", "Rondale Moore", "Jacob Cowing", "A.T. Perry", "K.J. Osborn",
#            "Clyde Edwards-Helaire", "Erick All Jr.", "Greg Dortch", "Jamaal Williams", "Evan Hull", "Rasheen Ali",
#            "Kendrick Bourne", "Malik Washington", "Israel Abanikanda", "Darius Slayton", "Brenden Rice", "Jonnu Smith",
#            "Noah Brown", "Samaje Perine", "John Metchie III", "Michael Carter", "Josh Reynolds", "Tutu Atwell", "Jalen Tolbert",
#            "Isaiah Davis", "Cade Stover", "Skyy Moore", "Justice Hill", "Eric Gray", "Chris Rodriguez Jr.", "Sam Darnold",
#            "Jeff Wilson Jr.", "Hayden Hurst", "Daniel Bellinger", "Kadarius Toney", "Zach Ertz", "Colby Parkinson",
#            "Juju Smith-Schuster", "Michael Thomas", "Demarcus Robinson", "Tre Tucker", "Jaheim Bell", "Johnny Wilson",
#            "Luke Schoonmaker", "Sean Tucker", "Kareem Hunt", "Bo Melton", "Tyler Scott", "Sam Howell", "Pierre Strong Jr.",
#            "Cam Akers", "Aidan O'Connell", "D'Ernest Johnson", "Jamari Thrash", "Emari Demercado", "Kylen Granson",
#            "Jared Wiley", "Donovan Peoples-Jones", "Jordan Mason", "Isaiah Spiller", "Parker Washington",
#            "Calvin Austin III", "Jerick McKinnon", "Darnell Washington", "Irv Smith Jr.", "Allen Lazard", "Deuce Vaughn",
#            "Noah Gray", "Jawhar Jordan", "Spencer Rattler", "Trey Sermon", "Blake Watson", "Brevin Jordan", "Tyquan Thornton",
#            "Ainias Smith", "Donald Parham Jr.", "Jauan Jennings", "Bub Means", "Jase McClellan", "Kenny Pickett",
#            "Mac Jones", "Dalvin Cook", "Ryan Flournoy", "Zach Evans", "Xavier Hutchinson", "Jimmy Garoppolo",
#            "Jacoby Brissett", "Frank Gore Jr.", "Trey Lance", "Terrace Marshall Jr.", "Desmond Ridder", "Will Mallory",
#            "Jeremy Ruckert", "Charlie Jones", "Hunter Renfrow", "Casey Washington", "Adam Trautman", "Jordan Whittington",
#            "Logan Thomas", "Tim Patrick", "Dillon Johnson", "Cordarrelle Patterson", "Robert Woods", "Parris Campbell",
#            "Marquez Valdes-Scantling", "Will Dissly", "Tip Reiman", "Joshua Kelley", "Hendon Hooker", "Deneric Prince",
#            "Tanner Hudson", "Davis Allen", "Isaiah Hodgins", "Cody Schrader"),
#   value = c(4734, 4796, 4519, 4153, 5014, 4682,
#             4986, 4199, 3652, 4013, 4061, 4627, 3301,
#             4049, 3528, 3392, 3249, 3954, 3313,
#             3401, 2915, 3688, 4315, 3255, 3597, 3926,
#             3205, 3169, 3414, 2827, 3215, 3330,
#             3188, 3317, 2925, 3286, 2770, 3184,
#             2704, 2146, 2245, 2523, 2645, 2595, 3172,
#             2629, 2807, 2780, 2164, 3117, 2641, 3043,
#             2315, 2247, 2641, 2027, 2661, 2218,
#             2069, 1987, 2328, 2511, 2351, 1738,
#             1722, 2073, 2450, 2508, 1743, 2118,
#             2410, 2141, 2694, 1981, 1842, 2032,
#             1537, 1291, 1449, 2188, 4942, 5134,
#             5078, 4846, 4015, 4784, 4726, 4625, 4733,
#             4049, 4783, 4595, 4609, 3450,
#             3271, 3401, 3929, 4791, 3916, 4063,
#             3406, 4083, 3646, 3408, 3202, 3176,
#             3285, 3500, 3349, 3284, 3290, 3147,
#             3206, 3160, 3011, 2862, 2864, 2730,
#             2741, 2636, 2920, 3163, 2821, 4059,
#             2647, 2332, 2955, 2820, 2681, 2624,
#             2583, 2634, 2828, 2606, 2770, 2191,
#             2384, 2164, 2880, 2069, 2530, 2311,
#             2290, 2027, 2347, 2041, 2147, 2447,
#             2387, 2081, 2152, 2236, 1703, 2039,
#             1667, 2198, 1648, 2061, 1768, 2290,
#             2468, 1556, 2341, 1762, 1177, 2063, 1571,
#             1290, 1543, 1417, 1685, 1297, 1657, 1507,
#             1394, 1228, 1457, 1604, 1478, 1708, 926,
#             1315, 1248, 1647, 1022, 1471, 596,
#             1384, 1114, 1300, 1338, 1549, 1281,
#             1225, 1419, 1797, 1063, 1252, 1423,
#             1452, 1688, 991, 1235, 1662, 1415,
#             1041, 789, 1408, 1203.8, 1173, 1256, 1556,
#             923, 1447, 1062, 1584, 1608, 1119, 2451,
#             541, 833, 1480, 748, 933, 1421,
#             169, 930.3, 1255, 1625, 179, 1346,
#             1252, 1301, 176, 1342, 961, 2072, 809,
#             1111, 1626, 559, 879, 998, 731,
#             1401, 983, 1657, 1012, 1426,
#             1305, 1146.5, 1145, 1187.7, 770, 1221,
#             1259, 1270.0, 1883, 1430, 1268.5, 1057, 1047,
#             935, 1044.7, 1375, 919, 935, 890,
#             889, 988.2, 760, 1047.5, 1074, 1124.5,
#             1381, 1291, 1352, 856, 764, 431,
#             800, 688, 881.2, 892.8, 612, 1992,
#             789.2, 889, 795.2, 376, 178, 575.0,
#             724, 790, 493, 425, 1439, 731.8,
#             172, 960, 902, 678.0))
#
# bind_rows(t50, b51) |> write_csv(here("Data/ktc_value082324"))

# instantaneous keep_trade_cut value: 10 pages of 50 players, retried a few times.
# If KTC cannot be scraped, today's file is not written and the latest saved
# values are used instead.
ktc_expected <- 500
ktc_attempts <- 3
keep_trade_cut <- NULL
ktc_players <- 0

for (attempt in seq_len(ktc_attempts)) {
  scraped <- tryCatch(
    map_dfr(
      0:9,
      ~ str_c(
        "https://keeptradecut.com/dynasty-rankings?page=",
        .x,
        "&filters=QB|WR|RB|TE|RDP&format=2"
      ) |>
        player_value()
    ) |>
      select(name, position, ktc_value) |>
      name_correction(),
    error = function(e) {
      message("KTC scrape failed (attempt ", attempt, "): ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(scraped)) {
    n_players <- scraped |> distinct(name, position) |> nrow()
    # keep the most complete attempt
    if (n_players > ktc_players) {
      keep_trade_cut <- scraped
      ktc_players <- n_players
    }
    if (n_players >= ktc_expected) {
      break
    }
    message("KTC returned ", n_players, " players (attempt ", attempt, ")")
  }
}

# periodically save
ktc_date <- format(today(), "%Y-%m-%d")

if (ktc_players >= 0.99 * ktc_expected) {
  keep_trade_cut <- keep_trade_cut |>
    left_join(
      select(player_info, name, position, player_id),
      by = join_by(name, position)
    )

  write_csv(
    keep_trade_cut,
    here(paste0("Data/raw/ktc/ktc_", ktc_date, ".csv"))
  )
} else {
  ktc_files <- list.files(
    here("Data/raw/ktc"),
    pattern = "^ktc_\\d{4}-\\d{2}-\\d{2}\\.csv$",
    full.names = TRUE
  )
  latest_ktc <- ktc_files[which.max(ymd(str_extract(basename(ktc_files), "\\d{4}-\\d{2}-\\d{2}")))]

  warning(
    "KTC returned ", ktc_players, " of ", ktc_expected,
    " players, so today's values were not saved. Using ", basename(latest_ktc)
  )

  keep_trade_cut <- read_csv(
    latest_ktc,
    show_col_types = FALSE,
    col_types = cols(player_id = col_character())
  )
}

# Season dates --------------------------------------------------------------
# first game of week 1 to the last game of the final fantasy week

season_dates <- tryCatch(
  nflreadr::load_schedules(league_seasons) |>
    filter(game_type == "REG") |>
    mutate(gameday = as.Date(gameday)) |>
    summarize(
      season_start = min(gameday),
      season_end = max(gameday[week == max(league_weeks)]),
      .by = season
    ) |>
    arrange(season) |>
    select(season_start, season_end),
  error = function(e) {
    warning("NFL schedule unavailable, keeping saved season dates: ", conditionMessage(e))
    read_csv(here("Shiny/Saved Files/season_dates.csv"), show_col_types = FALSE)
  }
)
write_csv(season_dates, here("Shiny/Saved Files/season_dates.csv"))

# remove objects and functions to declutter environment
rm(list = intersect(
  c(
    "combine_week", "grab_projection", "grab_rankings", "player_value",
    "parse_api", "parse_api_list", "parse_api_raw", "sleeper_get",
    "fetch_league_drafts", "has_content", "all_weeks_present", "setting",
    "current_settings", "missing_players", "nflverse_headshots",
    "box_score_headshots", "drafts", "existing", "new_proj", "weeks_needed",
    "min_projected", "scraped", "n_players", "attempt", "ktc_attempts",
    "ktc_expected", "ktc_players", "ktc_files", "latest_ktc", "ktc_date",
    "player_info_path", "team_stat_cols", "league_users"
  ),
  ls()
))
