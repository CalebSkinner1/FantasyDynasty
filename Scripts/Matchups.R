# Matchups
library("here")

# this hosts a function that spits out the matchups on a certain day
source(here("Shiny/Script Support.R"))


matchups_table <- read_csv(here("Data/matchups_table.csv"))

users <- read_csv(here("Data/users.csv"))

matchups_table_names <- matchups_table %>%
  left_join(users %>% select(-owner_id), by = join_by(roster_id)) %>%
  rename(team = display_name) %>%
  left_join(users %>% select(-owner_id), by = join_by(opponent_id == roster_id)) %>%
  rename(opponent = display_name)

# enter team: records
team_records_df <- matchups_table_names %>%
  filter(points > 0) %>%
  mutate(outcome = if_else(points > opp_points, "win", "loss"))
  
yearly_record <- team_records_df %>%
  group_by(season, team) %>%
  summarize(wins = sum(outcome == "win"),
            losses = sum(outcome == "loss")) %>%
  ungroup()


# team_matchup_record(team_records_df, "Arvs", "regular season", shiny = TRUE)
# upcoming matchups

# not sure this is worth it yet

# matchups_table_names %>% filter(points == 0) %>%
#   slice_min(week) %>%
#   rename(team1 = team,
#          team2 = opponent) %>%
#   left_join(yearly_record, by = join_by(season, team1 == team))
  

# dfs to save -------------------------------------------------------------

write_csv(team_records_df, here("Shiny/Saved Files/team_records_df.csv"))

  
  
  