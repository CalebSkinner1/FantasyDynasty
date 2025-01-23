# Transaction Grades
# similar to Draft Grades.R and Trade Grades.R, this page grades each transaction by a fantasy user

library("here")
library("gt")
library("gtExtras")
library("tidymodels")
library("janitor")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

# load data
tic()
load(here(data_path, "Data/transactions.RData")) #transactions, including trades
player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"), show_col_types = FALSE)
value_added <- read_csv(here(data_path, "Data/va_2024.csv"), show_col_types = FALSE)

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

total_transaction_value <- transactions %>%
  filter(type %in% c("waiver", "free_agent")) %>%
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
    left_join(player_info, by = join_by(player_id)) %>%
    left_join(player_total_value, by = join_by(player_id, position, name)) %>% # future value
    select(roster_id, name, position, future_value) %>%
    left_join(realized_value_gained, by = join_by(name, position)) %>%
    mutate(
      realized_value = replace_na(realized_value, 0), # if no realized value
      future_value = replace_na(future_value, 0), # if no future value
      )
  
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
    left_join(player_info, by = join_by(player_id)) %>%
    left_join(player_total_value, by = join_by(player_id, position, name)) %>% # future value
    select(roster_id, name, position, future_value) %>%
    left_join(realized_value_lost, by = join_by(name, position)) %>%
    mutate(
      realized_value = replace_na(realized_value, 0), # if no realized value
      future_value = replace_na(future_value, 0)) # if no future value
      
  total_transaction_value_gained <- total_player_value_gained %>%
    left_join(users, by = join_by(roster_id)) %>%
    rename(team_name = display_name) %>%
    select(-roster_id) %>%
    relocate(team_name) %>%
    mutate(
      total_value = realized_value + .95*future_value,
      type = "add") %>%
    arrange(desc(team_name))
  
  # players lost
  total_transaction_value_lost <- total_player_value_lost %>%
    left_join(users, by = join_by(roster_id)) %>%
    rename(team_name = display_name) %>%
    select(-roster_id) %>%
    relocate(team_name) %>%
    mutate(
      realized_value = -realized_value, #lost
      future_value = -future_value, #lost
      total_value = realized_value + .95*future_value,
      type = "drop") %>%
    arrange(desc(team_name))
  
  total_transaction_value <- bind_rows(total_transaction_value_gained, total_transaction_value_lost) %>%
    mutate(
      week = .x$week[1],
      season = .x$season[1],
    )
  return(total_transaction_value)
})

transaction_comparison <- map(total_transaction_value,
                  ~.x %>%
                    group_by(team_name) %>%
                    summarize(
                      total_transaction_value = sum(total_value),
                      total_future_value = sum(future_value),
                      total_realized_value = sum(realized_value)
                    )) %>%
  bind_rows(.id = "transaction_id")

individual_transactions <- transaction_comparison %>% # transactions
  rename(
    realized_value = "total_realized_value",
    total_value = "total_transaction_value",
    future_value = "total_future_value") %>%
  left_join(users, by = join_by(team_name == display_name)) %>%
  left_join(
    bind_rows(total_transaction_value, .id = "transaction_id") %>%
      left_join(users, by = join_by(team_name == display_name)) %>%
      filter(type == "add") %>%
      group_by(transaction_id) %>%
      slice_max(total_value) %>%
      select(transaction_id, team_name, name, position, season, week),
    by = join_by(transaction_id, team_name)) %>%
  filter(!is.na(name)) %>%
  mutate(
    avenue = str_c(season, " Week ", week, " transaction"),
    value_over_expected = total_value) %>%
  select(transaction_id, roster_id, name, position, realized_value, future_value, total_value, value_over_expected, avenue)

# most valuable transactions
top_transactions <- individual_transactions %>%
  arrange(desc(total_value)) %>%
  left_join(users, by = join_by(roster_id)) %>%
  select(-value_over_expected, -roster_id) %>%
  rename(
    team_name = display_name,
    transaction_details = "avenue") %>%
  relocate(c(team_name, transaction_details))

inspect_individual_transaction <- function(transaction_id, shiny = FALSE){
  title <- total_transaction_value[[transaction_id]]$team_name[1] %>%
    str_c(., "'s ", total_transaction_value[[transaction_id]]$season[1], " W",
          total_transaction_value[[transaction_id]]$week[1], " Transaction")
  
  df <- total_transaction_value[[transaction_id]] %>%
    mutate(action = if_else(type == "add", "+", "-")) %>%
    select(action, name, position, realized_value, future_value, total_value) %>%
    adorn_totals("row")
  
  if(shiny){
    df %>%
      shiny_edit_tables() %>%
      return()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538() %>%
      fmt_number(columns = c(future_value, realized_value, total_value), decimals = 2) %>%
      cols_label(future_value = "Future Value", realized_value = "Realized Value",
                 total_value = "Total Value") %>%
      tab_header(title = title)
  }
}

# inspect_individual_transaction(32) # Sam Darnold
# inspect_individual_transaction(48) # Tyrone Tracy
# inspect_individual_transaction(201) # Khalil Shakir
# inspect_individual_transaction(193) # Jalen McMillan

# 
# inspect_individual_transaction(49) # Sam Darnold
# inspect_individual_transaction(108) # Michael Penix
# inspect_individual_transaction(359) # Jalen McMillan
# inspect_individual_transaction(16) # Cade Otton

# mean add/drop value (mean value gained from adding/dropping a player)
marginal_transaction_value <- total_transaction_value %>%
  bind_rows() %>%
  filter(position != "K", position != "DST") %>%
  group_by(season, type) %>%
  summarize(total_value_added = mean(total_value),
            .groups = "keep")

# write_csv(marginal_transaction_value, here(data_path, "Data/marginal_transaction_value.csv"))

# by fantasy owner
overall_transaction_winners <- transaction_comparison %>%
  group_by(team_name) %>%
  summarize(
    transactions = n(),
    total_transaction_value = sum(total_transaction_value),
    total_future_value = sum(total_future_value),
    total_realized_value = sum(total_realized_value)) %>%
  arrange(desc(total_transaction_value)) %>%
  left_join(users, by = join_by(team_name == display_name)) %>%
  select(team_name, transactions, total_realized_value, total_future_value, total_transaction_value)

rm(value_added)


