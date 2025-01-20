# Transaction Grades
# similar to Draft Grades.R and Trade Grades.R, this page grades each transaction by a fantasy user

library("here")
library("gt")
library("gtExtras")
library("tidymodels")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

# load data
load(here(data_path, "Data/transactions.RData")) #transactions, including trades
player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"))
value_added <- read_csv(here(data_path, "Data/va_2024.csv"))

player_info <- read_csv(here(data_path, "Data/player_info.csv")) %>%
  select(-birth_date)

temp <- 

users <- read_csv(here(data_path, "Data/users.csv")) %>%
  select(-owner_id)

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
    mutate(total_value = realized_value + .95*future_value) %>%
    arrange(desc(team_name))
  
  # players and picks lost
  total_transaction_value_lost <- total_player_value_lost %>%
    left_join(users, by = join_by(roster_id)) %>%
    rename(team_name = display_name) %>%
    select(-roster_id) %>%
    relocate(team_name) %>%
    mutate(
      realized_value = -realized_value, #lost
      future_value = -future_value, #lost
      total_value = realized_value + .95*future_value) %>%
    arrange(desc(team_name))
  
  total_transaction_value <- bind_rows(total_transaction_value_gained, total_transaction_value_lost)
  total_transaction_value
})


transaction_comparison <- map(total_transaction_value,
                  ~.x %>%
                    group_by(team_name) %>%
                    summarize(total_transaction_value = sum(total_value))) %>%
  bind_rows(.id = "trade_id")

# most valuable transactions
transaction_comparison %>%
  arrange(desc(total_transaction_value))

total_transaction_value[[32]] # Sam Darnold
total_transaction_value[[48]] # Tyrone Tracy
total_transaction_value[[201]] # Khalil Shakir
total_transaction_value[[193]] # Jalen McMillan


# wrost transactions
transaction_comparison %>%
  arrange(total_transaction_value)

total_transaction_value[[49]] # Sam Darnold
total_transaction_value[[108]] # Michael Penix
total_transaction_value[[359]] # Jalen McMillan
total_transaction_value[[16]] # Cade Otton

# by fantasy owner
overall_transaction_winners <- transaction_comparison %>%
  group_by(team_name) %>%
  summarize(total_transaction_value = sum(total_transaction_value)) %>%
  arrange(desc(total_transaction_value))







