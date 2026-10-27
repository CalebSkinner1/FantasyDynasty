# Fantasy Teams
# team tables for the Fantasy Teams page: current assets (players and picks) and how each
# was acquired, position outlook, avenue grades, and every acquisition graded.
# Also writes every player's team changes (player_moves.csv) and each team's future value
# on every past date (team_value_time.csv). Reads the outputs of Pick Values, Draft
# Grades, Transaction Grades and Trade Grades, so it runs after them.

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Fantasy Teams...")

position_levels <- c("QB", "RB", "WR", "TE", "K", "DST")

# load data
users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, display_name)

player_info <- read_csv(
  here("Data/raw/player_info.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  distinct(player_id, .keep_all = TRUE) |>
  select(player_id, name, position)

player_total_value <- read_csv(
  here("Shiny/Saved Files/player_total_value.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  select(player_id, future_value)

current_roster <- read_csv(
  here("Data/derived/current_roster.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
future_draft_picks <- read_csv(
  here("Data/derived/future_draft_picks.csv"),
  show_col_types = FALSE
)
pick_values <- read_csv(
  here("Data/model_output/all_draft_pick_exp_values.csv"),
  show_col_types = FALSE
) |>
  select(season, round, pick_slot, future_value)
value_added <- read_csv(
  here("Data/derived/va.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)

# grades (Draft Grades, Trade Grades, Transaction Grades)
picks_df <- read_csv(
  here("Shiny/Saved Files/picks_df.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
draft_grades_by_team <- read_csv(
  here("Data/derived/draft_grades_by_team.csv"),
  show_col_types = FALSE
)
draft_selections <- read_csv(
  here("Data/derived/draft_selections.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
trade_adds <- read_csv(
  here("Data/derived/trade_adds.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
transaction_adds <- read_csv(
  here("Data/derived/transaction_adds.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
individual_trades <- read_csv(
  here("Data/derived/individual_trades.csv"),
  show_col_types = FALSE
)
individual_transactions <- read_csv(
  here("Data/derived/individual_transactions.csv"),
  show_col_types = FALSE
)
overall_trade_winners <- read_csv(
  here("Shiny/Saved Files/overall_trade_winners.csv"),
  show_col_types = FALSE
)
overall_transaction_winners <- read_csv(
  here("Shiny/Saved Files/overall_transaction_winners.csv"),
  show_col_types = FALSE
)
trade_moves <- read_csv(
  here("Data/derived/trade_moves.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character(), trade_id = col_character())
)
transaction_moves <- read_csv(
  here("Data/derived/transaction_moves.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character(), transaction_id = col_character())
)

# Current assets ----------------------------------------------------------------

player_assets <- current_roster |>
  select(roster_id, player_id) |>
  left_join(player_info, by = join_by(player_id)) |>
  left_join(player_total_value, by = join_by(player_id))

# picks are named by season and round; position is the team the pick originally belonged to
pick_assets <- future_draft_picks |>
  left_join(pick_values, by = join_by(season, round, pick_slot)) |>
  left_join(users, by = join_by(pick_slot == roster_id)) |>
  transmute(
    roster_id,
    player_id = NA_character_,
    name = str_c(season, " ", round, "R Draft Pick"),
    position = display_name,
    future_value
  )

total_assets <- bind_rows(player_assets, pick_assets) |>
  mutate(future_value = replace_na(future_value, 0))

# Acquisition avenue ------------------------------------------------------------
# the latest way the team acquired each asset: a draft, trade, or transaction. Drafts
# come before any trade or transaction of the same season; within a week, trades and
# transactions are ordered by when they happened. Players are matched by player_id and
# picks by name and original team.

asset_key <- function(player_id, name, position) {
  coalesce(player_id, str_c(name, " - ", position))
}

acquisition_events <- bind_rows(
  draft_selections |> mutate(week = 0),
  trade_adds,
  transaction_adds
) |>
  mutate(key = asset_key(player_id, name, position)) |>
  arrange(roster_id, key, season, week, created) |>
  slice_tail(n = 1, by = c(roster_id, key)) |>
  select(roster_id, key, avenue)

player_avenues <- total_assets |>
  mutate(key = asset_key(player_id, name, position)) |>
  left_join(acquisition_events, by = join_by(roster_id, key)) |>
  mutate(
    avenue = if_else(
      is.na(avenue) & str_detect(name, "Draft Pick"),
      "own pick",
      avenue
    )
  ) |>
  select(roster_id, player_id, name, position, future_value, avenue)

# Avenue grades -----------------------------------------------------------------

value_avenues <- bind_rows(
  draft_grades_by_team,
  overall_transaction_winners |>
    left_join(users, by = join_by(team_name == display_name)) |>
    transmute(
      roster_id,
      value_over_expected = total_transaction_value,
      total_realized_value,
      total_future_value,
      avenue = "transaction"
    ),
  overall_trade_winners |>
    left_join(users, by = join_by(team_name == display_name)) |>
    transmute(
      roster_id,
      value_over_expected = total_trade_value,
      total_realized_value,
      total_future_value,
      avenue = "trade"
    )
) |>
  arrange(roster_id)

# acquisitions ------------------------------------------------------------------

acquisitions <- bind_rows(
  picks_df |> # drafts
    mutate(
      avenue = if_else(
        draft == "initial draft",
        str_c("initial draft pick ", pick_no),
        str_c(season, " rookie draft pick ", pick_no)
      )
    ),
  individual_trades,
  individual_transactions
) |>
  select(
    roster_id,
    name,
    position,
    realized_value,
    future_value,
    total_value,
    value_over_expected,
    avenue
  )

# Position outlook --------------------------------------------------------------
# realized value: all value added at the position while on this roster, including players
# no longer there. future value: current roster only.

realized_value <- value_added |>
  summarize(realized_value = sum(value_added), .by = c(roster_id, position))

position_outlook_df <- player_assets |>
  summarize(
    future_value = sum(future_value, na.rm = TRUE),
    .by = c(roster_id, position)
  ) |>
  full_join(realized_value, by = join_by(roster_id, position)) |>
  mutate(
    position = factor(position, position_levels),
    across(c(future_value, realized_value), ~ replace_na(.x, 0))
  ) |>
  select(roster_id, position, realized_value, future_value) |>
  arrange(position, roster_id)

# Player moves ------------------------------------------------------------------
# every time a player changed teams (traded, added, or dropped), with when, for the
# dashed lines on the Individual Players page. tab and target_id open the trade or
# transaction grade.

team_name <- function(id) users$display_name[match(id, users$roster_id)]
move_date <- function(created) {
  as_date(as_datetime(created / 1000, tz = "America/Chicago"))
}

player_moves <- bind_rows(
  trade_moves |>
    transmute(
      player_id,
      created,
      label = str_c(
        "Traded from ", team_name(from_roster_id), " to ", team_name(to_roster_id),
        " (", season, " W", week, " trade)"
      ),
      tab = "trade",
      target_id = trade_id
    ),
  transaction_moves |>
    transmute(
      player_id,
      created,
      label = str_c(
        if_else(type == "add", "Added by ", "Dropped by "),
        team_name(roster_id),
        " (", season, " W", week, " transaction)"
      ),
      tab = "transaction",
      target_id = transaction_id
    )
) |>
  mutate(date = move_date(created)) |>
  arrange(player_id, created) |>
  select(player_id, date, label, tab, target_id)

# Team value over time ----------------------------------------------------------
# each team's future value on every date with pick values (Pick Values.R), by position and
# picks: the future value of its players that date (future_value_time.csv, rosters from
# Scraping.R's rosters_time.csv) and of its unused picks that date. A year of new picks
# joins every team's assets when each rookie draft finishes, which is when Sleeper adds
# them. Kickers and defenses have no future value. The latest date's total equals today's
# total future assets (Team Rankings). Written for the app as team_value_time.csv: date,
# roster_id, team, asset (QB, RB, WR, TE, Picks), and future_value.

pick_values_time_path <- here("Data/model_output/pick_values_time.csv")

if (file.exists(pick_values_time_path)) {
  pick_values_time <- read_csv(pick_values_time_path, show_col_types = FALSE)
  rosters_time <- read_csv(
    here("Data/derived/rosters_time.csv"),
    col_types = cols(date = col_date(), player_id = col_character(), .default = col_integer())
  )
  future_value_time <- read_csv(
    here("Shiny/Saved Files/future_value_time.csv"),
    show_col_types = FALSE,
    col_types = cols(player_id = col_character())
  )

  value_dates <- intersect(unique(pick_values_time$date), unique(future_value_time$date)) |>
    as_date()
  asset_levels <- c("QB", "RB", "WR", "TE", "Picks")

  team_value_time <- bind_rows(
    rosters_time |>
      filter(date %in% value_dates) |>
      left_join(player_info |> select(player_id, asset = position), by = join_by(player_id)) |>
      filter(asset %in% asset_levels) |>
      left_join(
        future_value_time |> select(player_id, date, future_value),
        by = join_by(player_id, date)
      ) |>
      summarize(future_value = sum(future_value, na.rm = TRUE), .by = c(date, roster_id, asset)),
    pick_values_time |>
      filter(date %in% value_dates) |>
      summarize(future_value = sum(future_value), .by = c(date, roster_id)) |>
      mutate(asset = "Picks")
  ) |>
    # every team and asset on every date, so the app's stacked areas line up
    complete(date, roster_id, asset = asset_levels, fill = list(future_value = 0)) |>
    left_join(users, by = join_by(roster_id)) |>
    transmute(
      date,
      roster_id,
      team = display_name,
      asset = factor(asset, asset_levels),
      future_value = round(future_value, 1)
    ) |>
    arrange(date, roster_id, asset)

  write_csv(team_value_time, here("Shiny/Saved Files/team_value_time.csv"))
}

# dfs to save -------------------------------------------------------------

grab_team_assets_df <- player_avenues |>
  arrange(desc(future_value)) |>
  select(roster_id, player_id, name, position, future_value, avenue)

write_csv(
  grab_team_assets_df,
  here("Shiny/Saved Files/grab_team_assets_df.csv")
)
write_csv(
  position_outlook_df,
  here("Shiny/Saved Files/position_outlook_df.csv")
)
write_csv(value_avenues, here("Shiny/Saved Files/value_avenues.csv"))
write_csv(acquisitions, here("Shiny/Saved Files/acquisitions.csv"))
write_csv(player_avenues, here("Shiny/Saved Files/player_avenues.csv"))
write_csv(player_moves, here("Shiny/Saved Files/player_moves.csv"))

# Examples (app functions in Shiny/Script Support.R) ------------------------------
# grab_team_assets(4, shiny = TRUE)
# position_outlook(4, shiny = TRUE)
# grab_team_contributors(4, enter_season = 2024, shiny = TRUE)
# grab_team_contributors_weekly(enter_roster_id = 4, enter_season = 2024, enter_week = 5, shiny = TRUE)
# value_avenues |> overall_grades(enter_roster_id = 6, shiny = TRUE)
# acquisitions |> top_acquisitions(4, "trade", shiny = TRUE)
# acquisitions |> worst_acquisitions(4, "rookie draft")
# player_avenues |> team_composition(4)
