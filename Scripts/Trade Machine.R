# Trade Machine
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("janitor")
})

message("begin computing Trade Machine...")

plot_future_value_df <- read_csv(
  here("Shiny/Saved Files/plot_future_value_df.csv"),
  show_col_types = FALSE
)

next_year_production <- plot_future_value_df |>
  filter(season == year(today() - days(10))) |>
  select(player_id, proj_tva_50)

grab_team_assets_df <- read_csv(
  here("Shiny/Saved Files/grab_team_assets_df.csv"),
  show_col_types = FALSE
)

users <- read_csv(
  here("Shiny/Saved Files/users.csv"),
  show_col_types = FALSE
) |>
  select(roster_id, display_name)

assets_df <- grab_team_assets_df |>
  mutate(
    name_code = if_else(
      str_detect(name, "Draft Pick"),
      str_c(name, " - ", position),
      name
    )
  ) |>
  left_join(next_year_production, by = join_by(player_id)) |>
  mutate(upcoming_year = replace_na(proj_tva_50, 0)) |>
  left_join(users, by = join_by(roster_id)) |>
  select(
    display_name,
    player_id,
    name_code,
    name,
    position,
    future_value,
    upcoming_year
  )

write_csv(assets_df, here("Shiny/Saved Files/assets_df.csv"))

# examples
# marginal_transaction_value <- read_csv(here("Data/marginal_transaction_value.csv"), show_col_types = FALSE)

# team1_players <- assets_df |> filter(display_name == "caskinner") |>
#   slice(19, 22) |> pull(name_code)
#
# team2_players <- assets_df |> filter(display_name == "Arvs") |>
#   slice(9) |> pull(name_code)
#
# trade_valuation <- grade_trade_wrapper(assets_df, marginal_transaction_value, team2_players, team1_players)
#
# team1_assets_received <- trade_valuation$team1
#
# team2_assets_received <- trade_valuation$team2
