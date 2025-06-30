# Trade Grades

# alright this is what this was all for amiright

library("here")
library("gt")
library("gtExtras")
library("tidymodels")
library("janitor")
library("tidyverse"); theme_set(theme_minimal())
library("tictoc")
data_path <- "FantasyDynasty/"

load(here(data_path, "Data/transactions.RData")) #transactions, including trades
future_draft_pick_values <- read_csv(here(data_path, "Data/future_draft_pick_values.csv"), show_col_types = FALSE) #future draft pick values

load(here(data_path, "Data/draft_picks.RData")) # draft results

player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"), show_col_types = FALSE)
value_added <- read_csv(here(data_path, "Data/va.csv"), show_col_types = FALSE)
draft_order <- read_csv(here(data_path, "Data/draft_order.csv"), show_col_types = FALSE)

marginal_transaction_value <- read_csv(here(data_path, "Data/marginal_transaction_value.csv"), show_col_types = FALSE)

# references
player_info <- read_csv(here(data_path, "Data/player_info.csv"), show_col_types = FALSE) %>%
  select(-birth_date)
users <- read_csv(here(data_path, "Data/users.csv"), show_col_types = FALSE) %>%
  select(-owner_id)

# function to edit tables for shiny
shiny_edit_tables <- function(df){
  df %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    rename_with(~str_replace_all(.x, "_", " "), everything()) %>%
    rename_with(~str_to_title(.x), everything())
}

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

total_trade_value <- transactions %>%
  filter(type == "trade") %>%
  split(seq_len(nrow(.))) %>%
  map(., ~{
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
    left_join(realized_value_gained, by = join_by(name, position)) %>%
    mutate(
      season = .x$season[1],
      week = this_week,
      type = "add",
      realized_value = replace_na(realized_value, 0)) # if no realized value
  
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
    left_join(realized_value_lost, by = join_by(name, position)) %>%
    mutate(
      season = .x$season[1],
      week = this_week,
      type = "drop",
      realized_value = replace_na(realized_value, 0))# if no realized value
  
  # value adjustment (if add extra player, must drop player from roster)
  adjustment_needed <- drops %>% group_by(roster_id) %>%
    summarize(drops = n()) %>%
    left_join(
      adds %>% group_by(roster_id) %>%
        summarize(adds = n()),
      by = join_by(roster_id)) %>%
    mutate(deficit = drops - adds) %>%
    filter(deficit != 0) %>%
    nrow()
  
  if(adjustment_needed != 0){
    value_adjustment <- total_player_value_lost %>%
      bind_rows(total_player_value_gained) %>%
      left_join(marginal_transaction_value, by = join_by(season, type)) %>% #join with marginal transacational value
      group_by(roster_id, season, week) %>%
      summarize(
        spots_lost = if_else(sum(type == "add") - sum(type == "drop") > 0, sum(type == "add") - sum(type == "drop"), 0),
        spots_opened = if_else(sum(type == "drop") - sum(type == "add") > 0, sum(type == "drop") - sum(type == "add"), 0),
        va_lost = spots_lost*min(total_value_added),
        va_gained = spots_opened*max(total_value_added),
        .groups = "keep") %>%
      ungroup() %>%
      select(roster_id, season, week, contains("va")) %>%
      pivot_longer(cols = c(contains("va")), names_to = "position", values_to = "total_value", names_prefix = "va_") %>%
      filter(total_value !=0) %>%
      mutate(
        type = if_else(position == "gained", "add", "drop"),
        name = "roster size adjustment",
        future_value = 0,
        realized_value = 0)
  }
  else{
    value_adjustment <- tibble()
  }
  
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
        realized_value = replace_na(sva_2024, 0),
        season = .x$season[1],
        week = this_week)
    
    traded_picks_gained <- traded_picks %>%
      select(owner_id, name, position, future_value, realized_value, season, week) %>%
      rename(roster_id = owner_id) %>%
      mutate(type = "add")
    
    traded_picks_lost <- traded_picks %>%
      select(previous_owner_id, name, position, future_value, realized_value, season, week) %>%
      rename(roster_id = previous_owner_id) %>%
      mutate(type = "drop")
    
    # players and picks gained
    total_trade_value_gained <- total_player_value_gained %>%
      bind_rows(traded_picks_gained) %>%
      mutate(total_value = realized_value + .95*future_value)
    
    # players and picks lost
    total_trade_value_lost <- total_player_value_lost %>%
      bind_rows(traded_picks_lost) %>%
      mutate(
        realized_value = -realized_value, #lost
        future_value = -future_value, #lost
        total_value = realized_value + .95*future_value)
    
    total_trade_value <- bind_rows(total_trade_value_gained, total_trade_value_lost, value_adjustment) %>%
      left_join(users, by = join_by(roster_id)) %>%
      rename(team_name = display_name) %>%
      select(-roster_id) %>%
      relocate(team_name) %>%
      arrange(desc(team_name))
  }
  else{ #if no traded picks
    total_trade_value_gained <- total_player_value_gained %>%
      mutate(total_value = realized_value + .95*future_value)
      
    # players and picks lost
    total_trade_value_lost <- total_player_value_lost %>%
      mutate(
        realized_value = -realized_value, #lost
        future_value = -future_value, #lost
        total_value = realized_value + .95*future_value)
    
    total_trade_value <- bind_rows(total_trade_value_gained, total_trade_value_lost, value_adjustment) %>%
      left_join(users, by = join_by(roster_id)) %>%
      rename(team_name = display_name) %>%
      select(-roster_id) %>%
      relocate(team_name) %>%
      arrange(desc(team_name))
  }
  total_trade_value
})

comparison <- map(total_trade_value,
                  ~.x %>%
                    group_by(team_name, season) %>%
                    summarize(
                      total_trade_value = sum(total_value),
                      total_future_value = sum(future_value),
                      total_realized_value = sum(realized_value),
                      .groups = "keep") %>%
                    ungroup()) %>%
  bind_rows(.id = "trade_id")

# most egregiously favorable trades
individual_trades <- comparison %>%
  rename(
    realized_value = "total_realized_value",
    total_value = "total_trade_value",
    future_value = "total_future_value") %>%
  left_join(users, by = join_by(team_name == display_name)) %>%
  group_by(trade_id) %>%
  mutate(other_teams = list(setdiff(team_name, pick(team_name)))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    other_teams = list(setdiff(other_teams, team_name))) %>%
  ungroup() %>%
  mutate(
    other_teams2 = map_chr(other_teams, ~str_c(.x, collapse = ", "))) %>%
  left_join(bind_rows(total_trade_value, .id = "trade_id") %>% #add info about this trade
              filter(type == "add") %>%
              group_by(trade_id, team_name) %>%
              slice_max(total_value) %>%
              select(trade_id, team_name, name, position, week),
            by = join_by(trade_id, team_name)) %>%
  mutate(
    value_over_expected = total_value,
    avenue = str_c(season, " W", week, " trade with ", other_teams2)) %>%
  select(trade_id, roster_id, name, position, realized_value, future_value, total_value, value_over_expected, avenue)
  
lopsided_trades <- individual_trades %>%
  arrange(desc(total_value)) %>%
  left_join(users, by = join_by(roster_id)) %>%
  select(-value_over_expected, -roster_id) %>%
  rename(
    team_name = display_name,
    trade_details = "avenue",
    top_asset_acquired = name) %>%
  relocate(c(team_name, trade_details))

inspect_individual_trade <- function(trade_id, shiny = FALSE){
  title <- total_trade_value[[trade_id]] %>% select(team_name) %>% distinct() %>%
    map_chr(~str_c(.x, collapse = ", ")) %>%
    str_c(total_trade_value[[trade_id]]$season[1], " W",
          total_trade_value[[trade_id]]$week[1], " Trade between ", .)
  
  df <- total_trade_value[[trade_id]] %>%
    filter(type == "add" | name == "roster size adjustment") %>%
    select(-season, -week, -type) %>%
    group_by(team_name) %>%
    group_map(~ .x %>% adorn_totals("row"),
              .keep = TRUE) %>%
    bind_rows()
  
  if(shiny){
    df %>%
      shiny_edit_tables() %>%
      return()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = c(future_value, realized_value, total_value), decimals = 2) %>%
      cols_label(team_name = "Team", future_value = "Future Value", realized_value = "Realized Value",
                 total_value = "Total Value") %>%
      tab_header(title = title)
  }
    
}

# inspect_individual_trade(9) # what is trade 9
# inspect_individual_trade(20) # what is trade 20
inspect_individual_trade(7) # what is trade 7
# inspect_individual_trade(4) # what is trade 4
# inspect_individual_trade(19) # what is trade 19

# by fantasy owner, totals don't add up because of value adjustment and future devaluation
overall_trade_winners <- comparison %>%
  group_by(team_name) %>%
  summarize(
    trades = n(),
    total_trade_value = sum(total_trade_value),
    total_future_value = sum(total_future_value),
    total_realized_value = sum(total_realized_value)) %>%
  arrange(desc(total_trade_value)) %>%
  left_join(users, by = join_by(team_name == display_name)) %>%
  select(team_name, trades, total_realized_value, total_future_value, total_trade_value)

rm(value_added)
