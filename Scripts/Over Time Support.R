# Over Time Support
# helpers for the values over time in Pick Values.R and the grades over time in Draft
# Grades.R, Transaction Grades.R, and Trade Grades.R. A graded asset (a player, a pick, or
# a fixed value such as a roster spot) is valued on every KTC date from the first one on or
# after its move through today:
#   realized value  the value added that counts for the grade, in the weeks finished by
#                   that date (a week finishes with its Monday game)
#   future value    a player's future value that date (future_value_time.csv); an unused
#                   pick's value that date (pick_values_time.csv), and after its draft the
#                   fixed value of its pick number; anything else keeps its value
# On the latest date these equal today's grades.

# the date each completed draft finished (its last pick, in Central time), from Scraping.R
draft_finished_dates <- function() {
  read_csv(
    here("Data/derived/draft_times.csv"),
    col_types = cols(draft_id = col_character(), .default = col_double())
  ) |>
    inner_join(
      read_csv(
        here("Data/derived/drafts.csv"),
        show_col_types = FALSE,
        col_types = cols(draft_id = col_character())
      ) |>
        filter(status == "complete"),
      by = join_by(draft_id)
    ) |>
    transmute(
      draft_id,
      season,
      type,
      finished = as_date(as_datetime(coalesce(last_picked, start_time) / 1000, tz = "America/Chicago"))
    )
}

# a Sleeper time (milliseconds) as a date in Central time
sleeper_date <- function(ms) {
  as_date(as_datetime(ms / 1000, tz = "America/Chicago"))
}

# the date each week with value added finished: its Monday game (season_start is the
# Thursday of week 1)
week_end_dates <- function(value_added, season_dates) {
  value_added |>
    distinct(season, week) |>
    inner_join(
      season_dates |> transmute(season = year(season_start), season_start),
      by = join_by(season)
    ) |>
    transmute(season, week, week_end = season_start + 7 * (week - 1) + 4)
}

# each asset's realized and future value on every date on or after its start
#   assets      asset (a unique id), start (date of the move), sign (1 gained, -1 lost),
#               player_id, pick_season, pick_round, pick_slot (NA unless a pick),
#               pick_drafted (date its draft finished, NA if not yet), fixed_realized and
#               fixed_future (the value of anything that isn't a player or unused pick,
#               already signed)
#   counted_va  asset, season, week, value_added: the value added rows that count for it
#   dates       the dates to value on
#   pick_values_time  needed only when there are picks
value_over_time <- function(
  assets,
  counted_va,
  dates,
  future_value_time,
  week_ends,
  pick_values_time = NULL
) {
  pick_values_time <- pick_values_time %||%
    tibble(season = double(), round = integer(), pick_slot = integer(), date = as_date(character()), future_value = double())

  # realized value through each finished week, per asset
  realized <- counted_va |>
    inner_join(week_ends, by = join_by(season, week)) |>
    summarize(value_added = sum(value_added), .by = c(asset, week_end)) |>
    arrange(week_end) |>
    mutate(realized = cumsum(value_added), .by = asset) |>
    select(asset, week_end, realized)

  assets |>
    select(
      asset, start, sign, player_id, pick_season, pick_round, pick_slot, pick_drafted,
      fixed_realized, fixed_future
    ) |>
    cross_join(tibble(date = dates)) |>
    filter(date >= start) |>
    left_join(realized, by = join_by(asset, closest(date >= week_end))) |>
    left_join(
      future_value_time |> select(player_id, date, player_future = future_value),
      by = join_by(player_id, date),
      na_matches = "never"
    ) |>
    left_join(
      pick_values_time |>
        select(pick_season = season, pick_round = round, pick_slot, date, pick_future = future_value),
      by = join_by(pick_season, pick_round, pick_slot, date),
      na_matches = "never"
    ) |>
    mutate(
      kind = case_when(
        !is.na(player_id) ~ "player",
        !is.na(pick_season) & (is.na(pick_drafted) | date < pick_drafted) ~ "unused pick",
        .default = "fixed"
      ),
      realized_value = case_when(
        kind == "player" ~ sign * coalesce(realized, 0),
        kind == "unused pick" ~ 0,
        .default = fixed_realized
      ),
      future_value = case_when(
        kind == "player" ~ sign * coalesce(player_future, 0),
        kind == "unused pick" ~ sign * pick_future, # NA if that date has no pick values
        .default = fixed_future
      )
    ) |>
    select(asset, date, realized_value, future_value)
}
