# Future Standings Granular

# this is a more intense and specific approach to the future standings question. Here,
# I'll simulate a season week-by-week.

library("here")
library("tictoc")
library("tidyverse"); theme_set(theme_minimal())
library("furrr")
library("tidymodels")

source("Modeling/Future Standings.R")

# load data
load(here("Modeling/player_simulations.RData"))
player_info <- read_csv(here("Data/player_info.csv"))

future_draft_picks <- read_csv(here("Data/future_draft_picks.csv"))
rookie_draft_values <- read_csv(here("Data/rookie_draft_values.csv"))

current_roster <- read_csv(here("Data/current_roster.csv")) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  select(name, position, roster_id)

va <- read_csv(here("Data/va.csv"))

draft_order <- read_csv(here("Data/draft_order.csv"))

matchups_table <- read_csv(here("Data/matchups_table.csv"))

# first, estimate win probability of matchup based on value added ---------
team_va <- va %>% group_by(roster_id) %>% summarize(total_va = sum(value_added))
  
matchup_va_data <- matchups_table %>% filter(points != 0) %>%
  left_join(team_va, by = join_by(roster_id)) %>%
  left_join(team_va %>% rename(opp_va = total_va), by = join_by(opponent_id == roster_id)) %>%
  mutate(
    victory = as.factor(if_else(points > opp_points, 1, 0)),
    va_diff = total_va - opp_va)

matchup_fit <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(
    victory ~ va_diff - 1,
    data = matchup_va_data)

# functions for season on season --------------------------------------------
win_probability <- function(table, fit){
  n <- nrow(table)
  odds <- runif(n)
  
  results <- augment(fit, new_data = table, type = "prob") %>%
    mutate(winner = if_else(odds > .pred_1, opponent_id, roster_id))
}

playoffs <- function(standings, table){
  
}

# Run Season --------------------------------------------------------------

year_sim <- function(team_tva, current_table){
  proj_tva <- team_tva %>% slice_sample(by = roster_id) %>% select(roster_id, va)
  current_table %>% left_join(proj_tva, by = join_by(roster_id)) %>%
    rename(team_tva = va) %>% 
    left_join(proj_tva, by = join_by(opponent_id == roster_id)) %>%
    rename(opp_tva = va) %>%
    mutate(diff = team_tva - opp_tva)
}

current_table





