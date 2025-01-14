# Trade Grades

# alright this is what this was all for amiright
library("here")
library("gt")
library("gtExtras")
library("tidymodels")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

load(here(data_path, "Data/transactions.RData")) #transactions, including trades
future_draft_pick_values <- read_csv(here(data_path, "Data/future_draft_pick_values.csv")) #future draft pick values

load(here(data_path, "Data/draft_picks.RData")) # draft results

player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"))
value_added <- read_csv(here(data_path, "Data/va_2024.csv"))
draft_order <- read_csv(here(data_path, "Data/draft_order.csv"))


player_info <- read_csv(here(data_path, "Data/player_info.csv")) %>%
  select(-birth_date)

temp <- transactions %>%
  filter(type == "trade") %>%
  split(seq_len(nrow(.)))

realized_rookie_picks <- bind_rows(draft_picks, .id = "draft_id") %>%
  group_by(draft_id) %>%
  mutate(
    draft_id = as.numeric(draft_id),
    max_round = max(round)) %>%
  filter(max_round < 4) %>%
  mutate(season = dense_rank(draft_id) + 2023) %>%
  ungroup() %>%
  left_join(draft_order %>% filter(type == "rookie"), by = join_by(season, draft_slot == draft_order)) %>%
  rename(original_owner = roster_id.y) %>%
  select(season, round, player_id, original_owner)
  


total_trade_value <- map(temp, ~{
  this_week <- .x$week %>% as.numeric() #week of transaction
  
  # gained
  adds <- .x$adds %>%
    pivot_longer(cols = everything(),
                 names_to = "player_id",
                 values_to = "roster_id") %>%
    drop_na()
  
  # need to add future value with realized value
  realized_value_gained <- adds %>%
    left_join(player_info, by = join_by(player_id)) %>% 
    left_join(value_added, by = join_by(name, position)) %>%
    filter(week > this_week | (week == this_week & roster_id.y == roster_id.x)) %>%
    group_by(name, position) %>%
    summarize(realized_value = sum(value_added),
              .groups = "keep") %>%
    ungroup()
    
  # total value after trade
  total_player_value_gained <- adds %>%
    left_join(player_total_value, by = join_by(player_id)) %>% # future value
    select(roster_id, name, position, future_value) %>%
    left_join(realized_value_gained, by = join_by(name, position))
  
  # lost
  drops <- .x$drops %>%
    pivot_longer(cols = everything(),
                 names_to = "player_id",
                 values_to = "roster_id") %>%
    drop_na()
  
  # need to add future value with realized value
  realized_value_lost <- drops %>%
    left_join(player_info, by = join_by(player_id)) %>% 
    left_join(value_added, by = join_by(name, position)) %>%
    filter(week > this_week | (week == this_week & roster_id.y == roster_id.x)) %>%
    group_by(name, position) %>%
    summarize(realized_value = sum(value_added),
              .groups = "keep") %>%
    ungroup()
  
  # total value after trade
  total_player_value_lost <- drops %>%
    left_join(player_total_value, by = join_by(player_id)) %>% # future value
    select(roster_id, name, position, future_value) %>%
    left_join(realized_value_lost, by = join_by(name, position))
  
  traded_picks0 <- .x$draft_picks[[1]]
  
  if(length(traded_picks0) != 0){
    traded_picks <- traded_picks0 %>%
      select(-league_id) %>%
      mutate(season = as.numeric(season)) %>%
      left_join(future_draft_pick_values, #future draft pick
                by = join_by(roster_id == pick_slot, season, round)) %>%
      left_join(realized_rookie_picks, #realized draft pick
                by = join_by(season, round, roster_id == original_owner)) %>%
      left_join(player_total_value, #gain value of realized draft pick
                by = join_by(player_id)) %>%
      left_join(users,
                by = join_by(roster_id)) %>%
      mutate(
        name = case_when(
          is.na(name) ~ str_c(season, " ", round, "R Draft Pick"),
          .default = name),
        position = case_when(
          is.na(position) ~ display_name,
          .default = position),
        future_value = case_when(
          is.na(future_value) ~ exp_total_value,
          .default = future_value),
        realized_value = replace_na(sva_2024, 0))
    
    traded_picks_gained <- traded_picks %>%
      select(owner_id, name, position, future_value, realized_value) %>%
      rename(roster_id = owner_id)
    
    traded_picks_lost <- traded_picks %>%
      select(previous_owner_id, name, position, future_value, realized_value) %>%
      rename(roster_id = previous_owner_id)
    
    # players and picks gained
    total_trade_value_gained <- total_player_value_gained %>%
      bind_rows(traded_picks_gained) %>%
      left_join(users, by = join_by(roster_id)) %>%
      rename(team_name = display_name) %>%
      select(-roster_id) %>%
      relocate(team_name) %>%
      mutate(total_value = realized_value + .95*future_value) %>%
      arrange(desc(team_name))
    
    # players and picks lost
    total_trade_value_lost <- total_player_value_lost %>%
      bind_rows(traded_picks_lost) %>%
      left_join(users, by = join_by(roster_id)) %>%
      rename(team_name = display_name) %>%
      select(-roster_id) %>%
      relocate(team_name) %>%
      mutate(
        realized_value = -realized_value, #lost
        future_value = -future_value, #lost
        total_value = realized_value + .95*future_value) %>%
      arrange(desc(team_name))
    
    total_trade_value <- bind_rows(total_trade_value_gained, total_trade_value_lost)
  }
  else{ #if no traded picks
    total_trade_value_gained <- total_player_value_gained %>%
      left_join(users, by = join_by(roster_id)) %>%
      rename(team_name = display_name) %>%
      select(-roster_id) %>%
      relocate(team_name) %>%
      mutate(total_value = realized_value + .95*future_value) %>%
      arrange(desc(team_name))
    
    # players and picks lost
    total_trade_value_lost <- total_player_value_lost %>%
      left_join(users, by = join_by(roster_id)) %>%
      rename(team_name = display_name) %>%
      select(-roster_id) %>%
      relocate(team_name) %>%
      mutate(
        realized_value = -realized_value, #lost
        future_value = -future_value, #lost
        total_value = realized_value + .95*future_value) %>%
      arrange(desc(team_name))
    
    total_trade_value <- bind_rows(total_trade_value_gained, total_trade_value_lost)
  }
  total_trade_value
})

comparison <- map(total_trade_value,
                  ~.x %>%
                    group_by(team_name) %>%
                    summarize(total_trade_value = sum(total_value))) %>%
  bind_rows(.id = "trade_id")

# most egregiously favorable trades
comparison %>%
  arrange(desc(total_trade_value))

# what is trade 9
total_trade_value[[9]]

# what is trade 20
total_trade_value[[20]]

# by fantasy owner
overall_trade_winners <- comparison %>%
  group_by(team_name) %>%
  summarize(total_trade_value = sum(total_trade_value)) %>%
  arrange(desc(total_trade_value))

