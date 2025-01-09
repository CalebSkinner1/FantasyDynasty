# Fantasy Teams
# this page is for interesting summaries of fantasy teams- eventually I'd like this to be a page in
# an RShiny

library("here")
library("gt")
library("gtExtras")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

# load data
season_value_added <- read_csv(here(data_path, "Data/sva_2024.csv"))
player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"))
player_info <- read_csv(here(data_path, "Data/player_info.csv"))

users <- read_csv(here(data_path, "Data/users.csv")) %>%
  select(-owner_id)

future_draft_picks <- read_csv(here(data_path, "Data/future_draft_picks.csv"))

# future assets
current_roster <- read_csv(here(data_path, "Data/current_roster.csv")) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  left_join(player_total_value, by = join_by(player_id, name, position, birth_date)) %>%
  select(roster_id, player_id, name, position, future_value, sva_2024)

# top contributors

# week by week

# recent transactions (and grades)