# call data from csvs

library("tidyverse")

script_data_path <- "Saved Files/"

# load necessary functions 
source("Script Support.R")

# load data files

# load csv from script

# fantasy teams
position_outlook_df <- read_csv(str_c(script_data_path, "position_outlook_df.csv"), show_col_types = FALSE)
acquisitions <- read_csv(str_c(script_data_path, "acquisitions.csv"), show_col_types = FALSE)
grab_team_assets_df <- read_csv(str_c(script_data_path, "grab_team_assets_df.csv"), show_col_types = FALSE)
player_avenues <- read_csv(str_c(script_data_path, "player_avenues.csv"), show_col_types = FALSE)
value_avenues <- read_csv(str_c(script_data_path, "value_avenues.csv"), show_col_types = FALSE)

# individual players
basic_info_df <- read_csv(str_c(script_data_path, "basic_info_df.csv"), show_col_types = FALSE)
plot_future_value_df <- read_csv(str_c(script_data_path, "plot_future_value_df.csv"), show_col_types = FALSE)
value_added <- read_csv(str_c(script_data_path, "value_added.csv"), show_col_types = FALSE)

# drafts
picks_df <- read_csv(str_c(script_data_path, "picks_df.csv"), show_col_types = FALSE)

# trades
overall_trade_winners <- read_csv(str_c(script_data_path, "overall_trade_winners.csv"), show_col_types = FALSE)
lopsided_trades <- read_csv(str_c(script_data_path, "lopsided_trades.csv"), show_col_types = FALSE)
comparison <- read_csv(str_c(script_data_path, "comparison.csv"), show_col_types = FALSE)

# transactions
overall_transaction_winners <- read_csv(str_c(script_data_path, "overall_transaction_winners.csv"), show_col_types = FALSE)
top_transactions <- read_csv(str_c(script_data_path, "top_transactions.csv"), show_col_types = FALSE)
transaction_comparison <- read_csv(str_c(script_data_path, "transaction_comparison.csv"), show_col_types = FALSE)

# load Rdata from script
# if (interactive()) {
  load(str_c(script_data_path, "total_trade_value.Rdata"))
  load(str_c(script_data_path, "total_transaction_value.Rdata"))
# }

# matchups
team_records_df <- read_csv(str_c(script_data_path, "team_records_df.csv"), show_col_types = FALSE)
  
# future standings
most_common_finish_df <- read_csv(str_c(script_data_path, "most_common_finish_df.csv"), show_col_types = FALSE)
champion_odds <- read_csv(str_c(script_data_path, "champion_odds.csv"), show_col_types = FALSE)

# load from data folder
player_total_value <- read_csv(str_c(script_data_path, "player_total_value.csv"), show_col_types = FALSE)
users <- read_csv(str_c(script_data_path, "users.csv"), show_col_types = FALSE)
season_value_added <- read_csv(str_c(script_data_path, "season_value_added.csv"), show_col_types = FALSE)
avatar <- read_csv(str_c(script_data_path, "avatar.csv"), show_col_types = FALSE)
