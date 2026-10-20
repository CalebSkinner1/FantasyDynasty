# Player Rankings Script
# exploratory: the app's Player Rankings functions, called by name for a quick look

library("here")

source(here("Shiny/Script Support.R"))

player_total_value <- read_csv(
  here("Shiny/Saved Files/player_total_value.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
future_value_time <- read_csv(
  here("Shiny/Saved Files/future_value_time.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)

# the app's functions take player_ids
player_id_of <- function(names) {
  player_total_value$player_id[match(names, player_total_value$name)]
}

# current future value
# top_future_value_player(player_total_value, "WR")

# future value over time
plot_over_time(
  future_value_time,
  player_id_of(c("Josh Allen", "Lamar Jackson", "Jayden Daniels", "Patrick Mahomes"))
)

# single player with comparable players
comparable_players(future_value_time, player_total_value, player_id_of("Rico Dowdle"))
comparable_players(future_value_time, player_total_value, player_id_of("Garrett Wilson"))
comparable_players(future_value_time, player_total_value, player_id_of("Aaron Rodgers"))
comparable_players(future_value_time, player_total_value, player_id_of("Ashton Jeanty"))
