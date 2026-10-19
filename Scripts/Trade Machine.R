# Trade Machine
# tables behind the Trade Machine page:
#   assets_df.csv          every rostered player and pick with its future value and its
#                          expected value added for the rest of the upcoming season
#   roster_spot_value.csv  the value of gaining or losing a roster spot in a trade
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Trade Machine...")

# load data
plot_future_value_df <- read_csv(
  here("Shiny/Saved Files/plot_future_value_df.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
season_value_added <- read_csv(
  here("Shiny/Saved Files/season_value_added.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
# season and share of it elapsed, written by Player Total Value.R
season_context <- read_csv(
  here("Data/model_output/season_context.csv"),
  show_col_types = FALSE
)
grab_team_assets_df <- read_csv(
  here("Shiny/Saved Files/grab_team_assets_df.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, display_name)
marginal_transaction_value <- read_csv(
  here("Shiny/Saved Files/marginal_transaction_value.csv"),
  show_col_types = FALSE
)

# Upcoming season value -----------------------------------------------------------
# The first simulated season is the current season while one is under way, and the next
# season during the offseason. Its expected value added (expected_tva) includes value
# already realized this season, which stays with the old team, so that is removed: what
# remains is the expected value for the rest of the season.

upcoming_season <- min(plot_future_value_df$season)
upcoming_label <- if_else(
  upcoming_season == season_context$season,
  "Current Season",
  "Next Season"
)

realized_this_season <- season_value_added |>
  filter(season == upcoming_season) |>
  select(player_id, realized = total_value_added)

upcoming_production <- plot_future_value_df |>
  filter(season == upcoming_season) |>
  left_join(realized_this_season, by = join_by(player_id)) |>
  mutate(upcoming_year = expected_tva - replace_na(realized, 0)) |>
  select(player_id, upcoming_year)

# picks have no value in the upcoming season
assets_df <- grab_team_assets_df |>
  mutate(
    name_code = if_else(
      str_detect(name, "Draft Pick"),
      str_c(name, " - ", position),
      name
    )
  ) |>
  left_join(upcoming_production, by = join_by(player_id)) |>
  mutate(
    upcoming_year = replace_na(upcoming_year, 0),
    upcoming_label = upcoming_label
  ) |>
  left_join(users, by = join_by(roster_id)) |>
  select(
    display_name,
    player_id,
    name_code,
    name,
    position,
    future_value,
    upcoming_year,
    upcoming_label
  )

# Roster spot value ---------------------------------------------------------------
# A team that receives more players than it sends must drop players, and a team that
# sends more can add free agents. A spot is worth the average add (gained) or drop (lost,
# negative) in the latest season, and half of that in the upcoming season.

roster_spot_value <- marginal_transaction_value |>
  filter(season == max(season)) |>
  transmute(
    type,
    future_value = total_value_added,
    upcoming_year = .5 * future_value
  )

# dfs to save -------------------------------------------------------------

write_csv(assets_df, here("Shiny/Saved Files/assets_df.csv"))
write_csv(roster_spot_value, here("Shiny/Saved Files/roster_spot_value.csv"))

# Example (app functions in Shiny/Script Support.R) ------------------------------
# trade_valuation <- grade_trade_wrapper(
#   assets_df, roster_spot_value,
#   "caskinner", c("2027 1R Draft Pick - caskinner"),
#   "Arvs", c("Josh Allen")
# )
