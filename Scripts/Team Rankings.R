# Team Rankings Script
library("here")

source(here("Shiny/Script Support.R"))

matchups_table <- read_csv(here("Data/matchups_table.csv"))
grab_team_assets_df <- read_csv(here("Shiny/Saved Files/grab_team_assets_df.csv"))
users <- read_csv(here("Data/users.csv")) %>%
  select(-owner_id)


# ELO Rankings ------------------------------------------------------------

# functions
# computes elo rating after game (margin of victory modifier)
movm <- function(point_diff, elo_winner, elo_loser, K = 10){
  K*log(abs(point_diff)+1) * (2.2/((elo_winner-elo_loser)*.001+2.2))
}

compute_week <- function(matchups, elo_table, K){
  # takes matchup table and current elo rankings and gives new elo rankings
  df <- matchups %>%
    left_join(elo_table, by = join_by(roster_id)) %>%
    left_join(elo_table %>% rename(opp_elo = elo), by = join_by(opponent_id == roster_id)) %>%
    mutate(
      point_diff = points - opp_points,
      elo_winner = if_else(points > opp_points, elo, opp_elo),
      elo_loser = if_else(points > opp_points, opp_elo, elo),
      elo = elo + movm(point_diff, elo_winner, elo_loser, K)*sign(point_diff)) %>%
    select(roster_id, elo)
  
   anti_join(elo_table, df, by = join_by(roster_id)) %>%
    select(roster_id, elo) %>%
    bind_rows(df)
}

# initialize
elo_init <- users %>% mutate(elo = 1500)

# weeks
matchup_list <- matchups_table %>%
  filter(points != 0) %>%
  group_split(season, week)

# function computes elo over time
compute_elo <- function(matchup_list, init_table, K = 10, lambda = .75){
  
  master_table <- elo_init %>% rename(start = elo)
  
  prev_elo <- elo_init
  
  # compute elo over time
  for(i in 1:length(matchup_list)){
    new_elo <- compute_week(matchup_list[[i]], prev_elo, K)
    
    names <- colnames(master_table)
    master_table <- master_table %>% left_join(new_elo, by = join_by(roster_id))
    colnames(master_table) <- c(names, paste0(matchup_list[[i]]$season[1],
                                              "_week", matchup_list[[i]]$week[1]))
    
    if(matchup_list[[i]]$week[1] == 17){ #reset elo after end of season
      new_elo <- new_elo %>% mutate(elo = elo * lambda + (1-lambda)*1500)
      
      names <- colnames(master_table)
      master_table <- master_table %>% left_join(new_elo, by = join_by(roster_id))
      colnames(master_table) <- c(names, paste0(matchup_list[[i]]$season[1] + 1,
                                                "_week", 0))
    }
    
    prev_elo <- new_elo
  }
  
  master_table %>% select(-roster_id)
}

weekly_elo <- compute_elo(matchup_list, elo_init) %>%
  rename("2024_week0" = start) %>%
  pivot_longer(cols = contains("week"), names_to = "date", values_to = "elo") %>%
  rename(team = display_name) %>%
  mutate(season = str_sub(date, start = 1, end = 4),
         week = str_remove(date, str_c(season, "_week")),
         date_hide = as.numeric(season) + (as.numeric(week)-1)/18,
         date = str_c(str_sub(season, 3, 4), "w", week)) %>%
  select(-season, -week)

graph_elo(weekly_elo)

# Future Value Assets -----------------------------------------------------

all_assets_summary_df <- grab_team_assets_df %>%
  group_by(roster_id) %>%
  summarize(total_future_value = sum(future_value)) %>%
  left_join(users, by = join_by(roster_id)) %>%
  arrange(desc(total_future_value)) %>%
  rename(team = display_name) %>%
  select(team, total_future_value)


# dfs to save -------------------------------------------------------------

write_csv(weekly_elo, here("Shiny/Saved Files/weekly_elo.csv"))
write_csv(all_assets_summary_df, here("Shiny/Saved Files/all_assets_summary_df.csv"))







