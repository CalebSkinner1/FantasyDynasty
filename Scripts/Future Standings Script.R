# Future Standings Script

source(here("Shiny/Script Support.R"))

users <- read_csv(here("Data/users.csv"), show_col_types = FALSE) %>%
  select(-owner_id)

final_standings_odds <- read_csv(here("Data/final_standings_odds.csv"), show_col_types = FALSE) %>%
  left_join(users, by = join_by(roster_id)) %>%
  select(-roster_id)

# championship odds
champion_odds <- final_standings_odds %>%
  filter(rank == 1) %>%
  pivot_wider(names_from = season, values_from = perc) %>%
  select(-rank) %>%
  arrange(desc(`2025`)) %>%
  mutate(across(contains("20"), ~scales::percent(.x))) %>%
  rename(team = display_name)

# most common finish

most_common_finish_df <- final_standings_odds %>%
  group_by(display_name, season) %>%
  slice_max(perc) %>%
  slice_head(n = 1) %>%
  arrange(rank, desc(perc)) %>%
  ungroup() %>%
  mutate(probability = scales::percent(perc)) %>%
  rename(finish = rank, team = display_name)

# dfs to save -------------------------------------------------------------

write_csv(most_common_finish_df, here("Shiny/Saved Files/most_common_finish_df.csv"))
write_csv(champion_odds, here("Shiny/Saved Files/champion_odds.csv"))
  
