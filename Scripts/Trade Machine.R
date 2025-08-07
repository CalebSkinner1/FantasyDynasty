# Trade Machine
library("here")

source(here("Scripts/Trade Grades.R"))

plot_future_value_df <- read_csv(here("Shiny/Saved Files/plot_future_value_df.csv"))

next_year_production <- plot_future_value_df %>% filter(season == year(today() - days(10))) %>%
  select(name, proj_tva_50)

assets_df <- grab_team_assets_df %>% mutate(
  name = if_else(str_detect(name, "Draft Pick"), str_c(name, " - ", position), name)) %>%
  left_join(next_year_production, by = join_by(name)) %>%
  mutate(upcoming_year = replace_na(proj_tva_50, 0)) %>%
  left_join(users, by =join_by(roster_id)) %>%
  select(display_name, name, position, future_value, upcoming_year)

write_csv(assets_df, here("Shiny/Saved Files/assets_df.csv"))

# examples
# team1_players <- assets_df %>% filter(display_name == "caskinner") %>%
#   slice(1, 3, 6) %>% pull(name)
# 
# team2_players <- assets_df %>% filter(display_name == "DepressedBroncosFan") %>%
#   slice(1, 3, 6) %>% pull(name)
# 
# grade_trade_wrapper(assets_df, team1_players, team2_players)
