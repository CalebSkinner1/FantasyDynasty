# Individual Players
# tables behind the Individual Players page: each valued player's basic info and value to
# date, his weekly production (points per game, volatility, and missed games), his
# projected value added by season (for the future value chart), and the weekly value added
# table used across the app, and the players the app's selectors offer
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Individual Players...")

# load data
player_simulations <- readRDS(here("Modeling/models/player_simulations.rds"))
season_value_added <- read_csv(
  here("Shiny/Saved Files/season_value_added.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(-owner_id)
value_added <- read_csv(here("Data/derived/va.csv"), show_col_types = FALSE) |>
  left_join(users, by = join_by(roster_id)) |>
  arrange(season, week, desc(type), display_name)
# a few retired players are listed twice (two positions under one id)
player_info <- read_csv(here("Data/raw/player_info.csv"), show_col_types = FALSE) |>
  distinct(player_id, .keep_all = TRUE)
player_total_value <- read_csv(
  here("Shiny/Saved Files/player_total_value.csv"),
  show_col_types = FALSE
)
# season and share of it elapsed (p), written by Player Total Value.R
season_context <- read_csv(
  here("Data/model_output/season_context.csv"),
  show_col_types = FALSE
)

# Basic info ----------------------------------------------------------------
# only players with a future value (the ones the app lets you select), so each name is
# one player. Seasons of value added are in order, and total value is value added to
# date plus future value (which is already discounted)

sva_cols <- sort(str_subset(names(player_total_value), "^sva_\\d{4}$"))

basic_info_df <- player_total_value |>
  select(player_id, name, position, birth_date, all_of(sva_cols), future_value) |>
  left_join(
    player_info |> select(player_id, years_exp),
    by = join_by(player_id)
  ) |>
  mutate(
    across(c(all_of(sva_cols), future_value), ~ replace_na(.x, 0)),
    total_value = rowSums(pick(all_of(sva_cols))) + future_value,
    age = time_length(interval(birth_date, today()), "years")
  ) |>
  select(
    player_id, # for the app's lookups (not shown)
    name,
    position,
    birth_date,
    age,
    years_exp,
    all_of(sva_cols),
    future_value,
    total_value
  ) |>
  rename_with(~ str_replace(.x, "sva_", "total value added "), all_of(sva_cols))

write_csv(basic_info_df, here("Shiny/Saved Files/basic_info_df.csv"))

# Weekly production --------------------------------------------------------------------
# from the NFL box scores (Data/raw/box_scores, regular season), for each player with a
# future value:
#   points_mean   his league points per game played
#   points_sd     their standard deviation (his volatility)
#   missed_rate   the share of his team's games he missed
# A game counts as played when he has a box score line, and as missed when his team
# played without him (injured, inactive, suspended, or off a roster), so bye weeks don't
# count either way. His team in a week he missed is his team in the nearest week he
# played that season; seasons he never played aren't counted. A player who was active but
# recorded no stat counts as missed (rare for fantasy-relevant players).
# Every game is weighted by recency: production_decay^k for the kth most recent game (team
# games for missed_rate), so a game 17 games back counts about half as much (0.96^17).
#   points        Sleeper's league points where the app has them (sleeper_points.csv),
#                 otherwise scored from the box score with the league's settings (as in
#                 Player Value Added.R's fallback scoring)
# Box score ids are matched to Sleeper ids with nflreadr's crosswalk, saved by Scraping.R
# (Data/derived/player_ids.csv). Each stat's rank at the position is among the players
# with at least min_ranked_games games played.
#   player_production.csv  player_id, games, team_games, points_mean, points_sd,
#                          missed_rate, points_rank and sd_rank (1 = highest), ranked
#                          (players ranked at the position; NA ranks for players with
#                          too few games)

production_decay <- 0.96
min_ranked_games <- 8

box_paths <- list.files(
  here("Data/raw/box_scores"),
  pattern = "^box_score_off\\d{2}\\.csv$",
  full.names = TRUE
)
ids_path <- here("Data/derived/player_ids.csv")
sleeper_points <- read_csv(
  here("Data/derived/sleeper_points.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)

no_production <- tibble(
  player_id = character(), games = integer(), team_games = integer(),
  points_mean = double(), points_sd = double(), missed_rate = double(),
  points_rank = integer(), sd_rank = integer(), ranked = integer()
)

player_production <- if (length(box_paths) == 0 || !file.exists(ids_path)) {
  warning("No box scores or player id crosswalk: player production left empty")
  no_production
} else {
  player_ids <- read_csv(ids_path, col_types = cols(.default = col_character()))

  box <- map(box_paths, \(path) {
    read_csv(path, show_col_types = FALSE, guess_max = 10000)
  }) |>
    bind_rows() |>
    filter(season_type == "REG") |>
    mutate(across(where(is.numeric), \(x) coalesce(x, 0)))

  # every team's games (any player of the team has a line)
  team_games <- box |>
    distinct(season, week, team)

  played <- box |>
    filter(position %in% c("QB", "RB", "WR", "TE")) |>
    inner_join(player_ids, by = join_by(player_id == gsis_id), suffix = c("_gsis", "")) |>
    transmute(
      season,
      week,
      player_id,
      team,
      box_points = .04 * passing_yards + 4 * passing_tds - passing_interceptions +
        2 * passing_2pt_conversions + .1 * rushing_yards + 6 * rushing_tds +
        2 * rushing_2pt_conversions + receptions + .1 * receiving_yards +
        6 * receiving_tds + 2 * receiving_2pt_conversions -
        2 * (sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost) +
        6 * (special_teams_tds + fumble_recovery_tds)
    ) |>
    semi_join(player_total_value |> mutate(player_id = as.character(player_id)), by = join_by(player_id)) |>
    distinct(season, week, player_id, .keep_all = TRUE) |>
    left_join(
      sleeper_points |> select(season, week, player_id, sleeper_points),
      by = join_by(season, week, player_id)
    ) |>
    mutate(points = coalesce(sleeper_points, box_points))

  # each player's team games in the seasons he played, most recent first
  games <- played |>
    distinct(player_id, season) |>
    inner_join(
      distinct(team_games, season, week),
      by = join_by(season),
      relationship = "many-to-many"
    ) |>
    left_join(
      played |> select(player_id, season, week, team, points),
      by = join_by(player_id, season, week)
    ) |>
    mutate(played = !is.na(points)) |>
    arrange(player_id, season, week) |>
    group_by(player_id, season) |>
    fill(team, .direction = "downup") |>
    ungroup() |>
    semi_join(team_games, by = join_by(season, week, team)) |>
    arrange(player_id, desc(season), desc(week)) |>
    mutate(
      team_weight = production_decay^(row_number() - 1),
      .by = player_id
    )

  # weighted standard deviation (reliability weights; NA with fewer than two games)
  weighted_sd <- function(x, w) {
    if (length(x) < 2) {
      return(NA_real_)
    }
    m <- sum(w * x) / sum(w)
    sqrt(sum(w * (x - m)^2) / (sum(w) - sum(w^2) / sum(w)))
  }

  missed <- games |>
    summarize(
      team_games = n(),
      missed_rate = sum(team_weight * !played) / sum(team_weight),
      .by = player_id
    )

  games |>
    filter(played) |>
    mutate(weight = production_decay^(row_number() - 1), .by = player_id) |>
    summarize(
      games = n(),
      points_mean = sum(weight * points) / sum(weight),
      points_sd = weighted_sd(points, weight),
      .by = player_id
    ) |>
    left_join(missed, by = join_by(player_id)) |>
    left_join(
      player_total_value |>
        transmute(player_id = as.character(player_id), position),
      by = join_by(player_id)
    ) |>
    mutate(
      eligible = games >= min_ranked_games,
      ranked = sum(eligible),
      points_rank = if_else(eligible, min_rank(desc(if_else(eligible, points_mean, NA))), NA),
      sd_rank = if_else(eligible, min_rank(desc(if_else(eligible, points_sd, NA))), NA),
      .by = position
    ) |>
    select(all_of(names(no_production)))
}

write_csv(player_production, here("Shiny/Saved Files/player_production.csv"))

# Projected value added by season ----------------------------------------------
# one row per player and season with expected_tva, the mean of the projection quantiles
# floored at 0 (a player below replacement would be benched), as future value uses, and
# the quantiles the app's chart shades (2.5, 10, 90, and 97.5; sorted, so the intervals
# are never inverted). During the
# season the current season is the realized value added so far plus the projection
# scaled by the share left (1 - p), the same split future value uses.

realized_now <- season_value_added |>
  filter(season == season_context$season) |>
  select(player_id, total_value_added)

plot_future_value_df <- player_simulations |>
  imap(\(sims, season) {
    season <- as.numeric(season)
    q_cols <- str_subset(names(sims), "^proj_tva_")

    q <- as.matrix(sims[q_cols])
    q[] <- t(apply(q, 1, sort, na.last = TRUE))
    expected <- rowMeans(pmax(q, 0))

    if (season == season_context$season) {
      realized <- realized_now$total_value_added[
        match(as.character(sims$player_id), realized_now$player_id)
      ] |>
        replace_na(0) # not started this season
      remaining <- 1 - season_context$p
      q <- q * remaining + realized
      expected <- expected * remaining + realized
    }

    sims |>
      select(name, player_id) |>
      bind_cols(as_tibble(q)) |>
      mutate(expected_tva = expected, season = season)
  }) |>
  list_rbind() |>
  select(
    name,
    player_id,
    proj_tva_2.5,
    proj_tva_10,
    proj_tva_90,
    proj_tva_97.5,
    expected_tva,
    season
  )

write_csv(
  plot_future_value_df,
  here("Shiny/Saved Files/plot_future_value_df.csv")
)

write_csv(value_added, here("Shiny/Saved Files/value_added.csv"))

# Player selectors --------------------------------------------------------------
# the players the app's selectors offer (QB, RB, WR, and TE with a future value), by
# future value, so the first three are where Player Rankings starts

player_total_value |>
  filter(position %in% c("QB", "WR", "RB", "TE")) |>
  arrange(desc(future_value)) |>
  transmute(name, player_id = as.character(player_id)) |>
  write_csv(here("Shiny/Saved Files/player_choices.csv"))

# Examples (app functions in Shiny/Script Support.R) -----------------------------
# plot_future_value("4984") # Josh Allen (functions take player_ids)
# value_added |> tabulate_realized_value("11560", 2024, shiny = FALSE) # Caleb Williams
# value_added |> weekly_results("8138", 2024) # James Cook
