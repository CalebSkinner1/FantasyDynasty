# League History Script
library("here")

source(here("Shiny/Script Support.R"))

# load data
matchups_table <- read_csv(here("Data/matchups_table.csv"))
users <- read_csv(here("Data/users.csv")) %>% select(-owner_id)
value_added <- read_csv(here("Shiny/Saved Files/value_added.csv"))

# most championships
championships_df <- matchups_table %>% filter(round == "Championship", points > opp_points) %>%
  left_join(users, by = join_by(roster_id)) %>%
  rename(team = display_name) %>%
  group_by(team) %>%
  summarize(championships = n(),
            most_recent = max(season))

# most finals appearances
finals_df <- matchups_table %>% filter(round == "Championship") %>%
  left_join(users, by = join_by(roster_id)) %>%
  rename(team = display_name) %>%
  group_by(team) %>%
  summarize(finals_appearances = n(),
            most_recent = max(season))

# most playoff berths
playoffs_df <- matchups_table %>% filter(round %in% c("1st round", "2nd round")) %>%
  left_join(users, by = join_by(roster_id)) %>%
  rename(team = display_name) %>%
  distinct(season, team, .keep = "all") %>%
  group_by(team) %>%
  summarize(playoff_berths = n(),
            most_recent = max(season))

# most wins
wins_df <- matchups_table %>%
  left_join(users, by = join_by(roster_id)) %>%
  rename(team = display_name) %>%
  left_join(users, by = join_by(opponent_id == roster_id)) %>%
  rename(opponent = display_name) %>%
  mutate(
    result = if_else(points > opp_points, "win", "loss"),
    type = if_else(round == "regular season" | round == "loser's bracket", round, "winner's bracket")) %>%
  filter(points != 0)

# compute_most_wins(wins_df, "All", 2024)

# total points scored
# compute_total_points(wins_df, "All", 2024)

# highest total in a game
# highest_team_total(wins_df, "All", 2024)

# highest season total by a player
# compute_total_points_player(value_added, wins_df, "All", 2024, "All")

# total value added by a player
# compute_value_added_player(value_added, wins_df, "All", 2024, "All")

# highest fantasy points in a game by a player
# highest_player_total(value_added, wins_df, "All", 2024, "All")

# dfs to save -------------------------------------------------------------

write_csv(championships_df, here("Shiny/Saved Files/championships_df.csv"))
write_csv(finals_df, here("Shiny/Saved Files/finals_df.csv"))
write_csv(playoffs_df, here("Shiny/Saved Files/playoffs_df.csv"))
write_csv(wins_df, here("Shiny/Saved Files/wins_df.csv"))

