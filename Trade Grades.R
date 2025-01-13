# Trade Grades

# alright this is what this was all for amiright
library("here")
library("gt")
library("gtExtras")
library("tidymodels")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

load(here(data_path, "Data/transactions.RData"))
future_draft_pick_values <- read_csv(here(data_path, "Data/future_draft_pick_values.csv"))
player_total_value <- 

temp <- transactions %>%
  filter(type == "trade") %>%
  split(seq_len(nrow(.)))

map(temp, ~{
  adds <- temp[[1]]$adds %>%
    pivot_longer(cols = everything(),
                 names_to = "player_id",
                 values_to = "roster_id") %>%
    drop_na() %>%
    left_join() #need to add future value with unrealized realized value
  
  draft_picks <- temp[[3]]$draft_picks[[1]]
  
  if(nrow(draft_picks) > 0){
    draft_picks <- draft_picks %>%
      select(-league_id, -previous_owner_id) %>%
      mutate(season = as.numeric(season)) %>%
      left_join(future_draft_pick_values,
                by = join_by(roster_id == pick_slot, season, round))
  }

    
  
})



