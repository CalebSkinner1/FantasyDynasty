# Transaction Grades
# similar to Draft Grades.R and Trade Grades.R, this page grades each transaction by a fantasy user

library("here")
library("gt")
library("gtExtras")
library("tidymodels")
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

# most valuable transactions
# transaction_comparison %>%
#   arrange(desc(total_transaction_value))
# 
# total_transaction_value[[32]] # Sam Darnold
# total_transaction_value[[48]] # Tyrone Tracy
# total_transaction_value[[201]] # Khalil Shakir
# total_transaction_value[[193]] # Jalen McMillan


# wrost transactions
# transaction_comparison %>%
#   arrange(total_transaction_value)
# 
# total_transaction_value[[49]] # Sam Darnold
# total_transaction_value[[108]] # Michael Penix
# total_transaction_value[[359]] # Jalen McMillan
# total_transaction_value[[16]] # Cade Otton

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
    total_transaction_value = sum(total_transaction_value),
    total_future_value = sum(total_future_value),
    total_realized_value = sum(total_realized_value)) %>%
  arrange(desc(total_transaction_value)) %>%
  left_join(users, by = join_by(team_name == display_name))

rm(value_added)


