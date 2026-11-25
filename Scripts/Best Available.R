# Best Available
# the table behind the Best Available page: every player with a future value who is on no
# roster (the latest Sleeper rosters, Data/derived/current_roster.csv), with
#   value          in season, his projected value added for the rest of the season: the
#                  current season's projection (Individual Players.R's
#                  plot_future_value_df, floored at 0 as there) less what he has realized
#                  so far; in the offseason, his projection for next season (value_label
#                  says which)
#   projection     Sleeper's projection for the next fantasy week, in league points
#                  (Data/raw/projections/upcoming_projections.csv, Scraping.R; NA when
#                  there is none)
#   recent_points  his average points over the last three finished weeks of the season,
#                  in the weeks he has points (none on a bye), with recent_games
#   future_value   today's future value (player_total_value.csv)
#   change_7, change_30  the change in his smoothed future value over the last 7 and 30
#                  days (future_value_time.csv), NA when he has no value that far back
# Runs after Individual Players.R (plot_future_value_df.csv) and Trade Machine.R
# (assets_df.csv, for the season label).
#   best_available.csv  name, player_id, position, age, value, value_label, projection,
#                       projection_week, recent_points, recent_games, future_value,
#                       change_7, change_30

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Best Available...")

ids <- cols(player_id = col_character())
read_saved <- function(file, ...) {
  read_csv(here("Shiny/Saved Files", file), show_col_types = FALSE, ...)
}

player_total_value <- read_saved("player_total_value.csv", col_types = ids)
plot_future_value_df <- read_saved("plot_future_value_df.csv", col_types = ids)
season_value_added <- read_saved("season_value_added.csv", col_types = ids)
future_value_time <- read_saved("future_value_time.csv", col_types = ids)
assets_df <- read_saved("assets_df.csv", col_types = ids)
rostered <- read_csv(
  here("Data/derived/current_roster.csv"),
  col_types = cols(.default = col_character())
)
sleeper_points <- read_csv(
  here("Data/derived/sleeper_points.csv"),
  show_col_types = FALSE,
  col_types = ids
)
completed_weeks <- read_csv(here("Data/derived/completed_weeks.csv"), show_col_types = FALSE)

upcoming_path <- here("Data/raw/projections/upcoming_projections.csv")
upcoming <- if (file.exists(upcoming_path)) {
  read_csv(
    upcoming_path,
    col_types = cols(projection = col_double(), week = col_double(), .default = col_character())
  )
} else {
  tibble(player_id = character(), projection = double(), week = double())
}

# Rest of season (or next season) -----------------------------------------------------
# the first season projected: the current one in season (Current Season), the next one in
# the offseason (Next Season)

in_season <- first(assets_df$upcoming_label, default = "Current Season") == "Current Season"
first_season <- min(plot_future_value_df$season)

season_value <- plot_future_value_df |>
  filter(season == first_season) |>
  left_join(
    season_value_added |>
      filter(season == first_season) |>
      select(player_id, realized = total_value_added),
    by = join_by(player_id)
  ) |>
  transmute(
    player_id,
    # expected_tva is realized + the rest of the season's (floored) projection
    value = pmax(expected_tva - if (in_season) coalesce(realized, 0) else 0, 0)
  )

# Recent form --------------------------------------------------------------------------

last_season <- max(completed_weeks$season)
recent_weeks <- completed_weeks |>
  filter(season == last_season) |>
  slice_max(week, n = 3)

recent <- sleeper_points |>
  semi_join(recent_weeks, by = join_by(season, week)) |>
  summarize(
    recent_points = mean(sleeper_points),
    recent_games = n(),
    .by = player_id
  )

# Future value trend -------------------------------------------------------------------
# the smoothed value on the latest date, less the smoothed value on the latest date at
# least 7 (or 30) days earlier

fv_dates <- sort(unique(future_value_time$date))
latest <- max(fv_dates)
date_back <- function(days) max(fv_dates[fv_dates <= latest - days], as_date(-Inf))
smoothed_on <- function(d) {
  future_value_time |>
    filter(date == d) |>
    select(player_id, smooth = future_value_smooth)
}

trend <- smoothed_on(latest) |>
  left_join(smoothed_on(date_back(7)) |> rename(smooth_7 = smooth), by = join_by(player_id)) |>
  left_join(smoothed_on(date_back(30)) |> rename(smooth_30 = smooth), by = join_by(player_id)) |>
  transmute(player_id, change_7 = smooth - smooth_7, change_30 = smooth - smooth_30)

# Best available -----------------------------------------------------------------------

best_available <- player_total_value |>
  filter(!is.na(future_value), !player_id %in% rostered$player_id) |>
  transmute(
    name,
    player_id,
    position,
    age = round(time_length(interval(birth_date, today()), "years"), 1),
    future_value
  ) |>
  left_join(season_value, by = join_by(player_id)) |>
  left_join(
    upcoming |> select(player_id, projection, projection_week = week),
    by = join_by(player_id)
  ) |>
  left_join(recent, by = join_by(player_id)) |>
  left_join(trend, by = join_by(player_id)) |>
  mutate(
    value = coalesce(value, 0),
    value_label = if_else(in_season, "Rest of season", "Next season"),
    projection_week = first(na.omit(upcoming$week), default = NA)
  ) |>
  select(
    name, player_id, position, age, value, value_label, projection, projection_week,
    recent_points, recent_games, future_value, change_7, change_30
  ) |>
  arrange(desc(value), desc(future_value))

# dfs to save -------------------------------------------------------------

write_csv(best_available, here("Shiny/Saved Files/best_available.csv"))
