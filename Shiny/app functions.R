# app functions

library("here")
data_path <- "FantasyDynasty/"
script_data_path <- "FantasyDynasty/Scripts/Saved Files/"

# load necessary functions 
source(here(data_path, "Scripts/Script Support.R"))

# load data files

# load csv from script

# fantasy teams
position_outlook_df <- read_csv(here(script_data_path, "position_outlook_df.csv"), show_col_types = FALSE)
acquisitions <- read_csv(here(script_data_path, "acquisitions.csv"), show_col_types = FALSE)
grab_team_assets_df <- read_csv(here(script_data_path, "grab_team_assets_df.csv"), show_col_types = FALSE)
player_avenues <- read_csv(here(script_data_path, "player_avenues.csv"), show_col_types = FALSE)
value_avenues <- read_csv(here(script_data_path, "value_avenues.csv"), show_col_types = FALSE)

# individual players
basic_info_df <- read_csv(here(script_data_path, "basic_info_df.csv"), show_col_types = FALSE)
plot_future_value_df <- read_csv(here(script_data_path, "plot_future_value_df.csv"), show_col_types = FALSE)
value_added <- read_csv(here(script_data_path, "value_added.csv"), show_col_types = FALSE)

# drafts
picks_df <- read_csv(here(script_data_path, "picks_df.csv"), show_col_types = FALSE)

# trades
overall_trade_winners <- read_csv(here(script_data_path, "overall_trade_winners.csv"), show_col_types = FALSE)
lopsided_trades <- read_csv(here(script_data_path, "lopsided_trades.csv"), show_col_types = FALSE)
comparison <- read_csv(here(script_data_path, "comparison.csv"), show_col_types = FALSE)

# transactions
overall_transaction_winners <- read_csv(here(script_data_path, "overall_trade_winners.csv"), show_col_types = FALSE)
top_transactions <- read_csv(here(script_data_path, "lopsided_trades.csv"), show_col_types = FALSE)
transaction_comparison <- read_csv(here(script_data_path, "transaction_comparison.csv"), show_col_types = FALSE)

# load Rdata from script
load(here(script_data_path, "total_trade_value.Rdata"))
load(here(script_data_path, "total_transaction_value.Rdata"))

# load from data folder
player_total_value <- read_csv(here(data_path, "Data", "player_total_value.csv"), show_col_types = FALSE)
users <- read_csv(here(data_path, "Data", "users.csv"), show_col_types = FALSE)
season_value_added <- read_csv(here(data_path, "Data/sva.csv"), show_col_types = FALSE)
