# Player Rankings Script

library("here")

source(here("Shiny/Script Support.R"))

player_total_value <- read_csv(here("Data/player_total_value.csv"))
future_value_time <- read_csv(here("Shiny/Saved Files/future_value_time.csv"))

# current future value
# top_future_value_player(player_total_value, "WR")

# future value over time
# plot_over_time(future_value_time, c("Josh Allen", "Lamar Jackson", "Jayden Daniels"))

# single player with comparable players
# comparable_players(future_value_time, player_total_value, "Rico Dowdle")
# comparable_players(future_value_time, player_total_value, "Garrett Wilson")
# comparable_players(future_value_time, player_total_value, "Aaron Rodgers")


