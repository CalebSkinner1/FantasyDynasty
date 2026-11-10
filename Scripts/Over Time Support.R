# Over Time Support
# helpers for the values over time in Pick Values.R and the grades over time in Draft
# Grades.R, Transaction Grades.R, and Trade Grades.R. A graded asset (a player, a pick, or
# a fixed value such as a roster spot) is valued on every KTC date from the first one on or
# after its move through today:
#   realized value  the value added that counts for the grade, in the weeks finished by
#                   that date (a week counts from the Tuesday after it, once its Monday
#                   game is over, as in the share of the season played)
#   future value    a player's smoothed future value that date (future_value_smooth in
#                   future_value_time.csv, see smooth_future_value()); an unused pick's
#                   value that date (pick_values_time.csv), and after its draft the fixed
#                   value of its pick number; anything else keeps its value
# The latest date is close to today's grades but not equal: the grades use today's future
# values, the curves the smoothed ones.
#
# Also the rule for which dates every over-time file keeps (kept_dates()), used by
# Scraping.R, Player Total Value.R, and Future Standings.R.

# Dates kept --------------------------------------------------------------------------

recent_days <- 30

# the dates kept from dates: every date in the last recent_days days, and one a week
# before that: the week's Tuesday, or its first date when it has none (weeks run Tuesday
# to Monday, so a week's Tuesday is its first day). The same dates are kept whenever this
# runs, so a date dropped once is never needed again.
kept_dates <- function(dates, today = lubridate::today(), recent = recent_days) {
  dates <- sort(unique(as_date(dates)))
  old <- dates[dates < today - recent]
  week_tuesday <- old - (wday(old) - 3) %% 7
  c(old[!duplicated(week_tuesday)], dates[dates >= today - recent])
}

# Smoothing ---------------------------------------------------------------------------

# each player's future value smoothed over time, for every over-time chart and grade (not
# today's values): the median over the dates within half_window days on either side, which
# drops dips and spikes shorter than about a week but keeps a lasting change where it
# happened, then an average of those medians over the same window weighted by a normal
# curve (sd_days) to round off the steps. Windows are in days, so daily and weekly stretches
# are smoothed alike. Near the newest date the window only reaches forward to it (and back
# at least 2 days), so the newest value is the median of its last 3 days.
#   fvt       player_id, date, future_value (every saved date)
#   targets   the dates to smooth (others keep their future_value_smooth)
# Returns fvt with future_value_smooth filled in for targets.
smooth_future_value <- function(fvt, targets, half_window = 7, sd_days = 3) {
  latest <- max(fvt$date)
  window_of <- function(df) {
    df |>
      mutate(
        ahead = pmin(half_window, as.numeric(latest - date)),
        lo = date - pmax(ahead, 2),
        hi = date + ahead
      )
  }
  values <- fvt |>
    select(player_id, n_date = date, value = future_value)

  # medians for the targets and every date in their windows
  median_dates <- fvt |>
    filter(date >= min(targets) - half_window) |>
    distinct(player_id, date)
  medians <- median_dates |>
    window_of() |>
    inner_join(values, by = join_by(player_id, lo <= n_date, hi >= n_date)) |>
    summarize(med = median(value), .by = c(player_id, date))

  smoothed <- fvt |>
    filter(date %in% targets) |>
    distinct(player_id, date) |>
    window_of() |>
    inner_join(
      medians |> rename(n_date = date),
      by = join_by(player_id, lo <= n_date, hi >= n_date)
    ) |>
    mutate(kernel = exp(-0.5 * (as.numeric(n_date - date) / sd_days)^2)) |>
    summarize(smooth = sum(kernel * med) / sum(kernel), .by = c(player_id, date))

  fvt |>
    left_join(smoothed, by = join_by(player_id, date)) |>
    mutate(future_value_smooth = coalesce(smooth, future_value_smooth)) |>
    select(-smooth)
}

# Grades over time ----------------------------------------------------------------------

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

# each team's total value gained from all its graded moves on every date (the Winners
# over Time charts): grades_time has team_name, date, realized_value, and future_value,
# one row per move and team. Before a team's first move it has gained 0, and the latest
# date equals the Winners table. Rounded for the app.
team_totals_time <- function(grades_time) {
  grades_time |>
    summarize(
      realized_value = sum(realized_value),
      future_value = sum(future_value),
      .by = c(team_name, date)
    ) |>
    complete(team_name, date, fill = list(realized_value = 0, future_value = 0)) |>
    transmute(
      team = team_name,
      date,
      realized_value = round(realized_value, 1),
      future_value = round(future_value, 1)
    ) |>
    arrange(team, date)
}

# a Sleeper time (milliseconds) as a date in Central time
sleeper_date <- function(ms) {
  as_date(as_datetime(ms / 1000, tz = "America/Chicago"))
}

# the date each week with value added counts from: the Tuesday after it, once its Monday
# game is over (week 1's is the first Tuesday after the opener, season_start)
week_end_dates <- function(value_added, season_dates) {
  value_added |>
    distinct(season, week) |>
    inner_join(
      season_dates |> transmute(season = year(season_start), season_start),
      by = join_by(season)
    ) |>
    transmute(
      season,
      week,
      week_end = season_start + (2 - wday(season_start)) %% 7 + 1 + 7 * (week - 1)
    )
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
      future_value_time |> select(player_id, date, player_future = future_value_smooth),
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
