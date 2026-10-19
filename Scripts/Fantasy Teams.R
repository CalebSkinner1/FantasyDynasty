# Fantasy Teams
# team tables for the Fantasy Teams page: current assets (players and picks) and how each
# was acquired, position outlook, avenue grades, and every acquisition graded.
# Reads the outputs of Pick Values, Draft Grades, Transaction Grades and Trade Grades, so
# it runs after them.

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

# Examples (app functions in Shiny/Script Support.R) ------------------------------
# grab_team_assets(4, shiny = TRUE)
# position_outlook(4, shiny = TRUE)
# grab_team_contributors(4, enter_season = 2024, shiny = TRUE)
# grab_team_contributors_weekly(enter_roster_id = 4, enter_season = 2024, enter_week = 5, shiny = TRUE)
# value_avenues |> overall_grades(enter_roster_id = 6, shiny = TRUE)
# acquisitions |> top_acquisitions(4, "trade", shiny = TRUE)
# acquisitions |> worst_acquisitions(4, "rookie draft")
# player_avenues |> team_composition(4)
