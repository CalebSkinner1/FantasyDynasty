# Transaction Grades
# similar to Draft Grades.R and Trade Grades.R, this page grades each transaction by a fantasy user

library("here")

# load data
source(here("Shiny/Script Support.R"))
load(here("Data/transactions.RData")) #transactions, including trades
player_total_value <- read_csv(here("Data/player_total_value.csv"), show_col_types = FALSE)
value_added <- read_csv(here("Data/va.csv"), show_col_types = FALSE)

player_info <- read_csv(here("Data/player_info.csv"), show_col_types = FALSE) %>%
  select(-birth_date)

users <- read_csv(here("Data/users.csv"), show_col_types = FALSE) %>%
  select(-owner_id)

total_transaction_value <- transactions %>%
  filter(type %in% c("waiver", "free_agent")) %>%
  split(seq_len(nrow(.))) %>%
  map(., ~{
  this_season <- .x$season %>% as.numeric()
    
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
    filter(season >= this_season) %>%
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
      future_value = replace_na(future_value, 0)) # if no future value
  
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

# mean add/drop value (mean value gained from adding/dropping a player)
marginal_transaction_value <- total_transaction_value %>%
  bind_rows() %>%
  filter(position != "K", position != "DST") %>%
  group_by(season, type) %>%
  summarize(total_value_added = mean(total_value),
            .groups = "keep")

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

# dfs to save -------------------------------------------------------------

write_csv(overall_transaction_winners, here("Shiny/Saved Files/overall_transaction_winners.csv"))

write_csv(top_transactions, here("Shiny/Saved Files/top_transactions.csv"))

write_csv(transaction_comparison, here("Shiny/Saved Files/transaction_comparison.csv"))

write_csv(marginal_transaction_value, here("Data/marginal_transaction_value.csv"))

save(total_transaction_value, file = here("Shiny/Saved Files/total_transaction_value.Rdata"))

# Examples ----------------------------------------------------------------

# inspect_individual_transaction(32) # Sam Darnold
# inspect_individual_transaction(48) # Tyrone Tracy
# inspect_individual_transaction(201) # Khalil Shakir
# inspect_individual_transaction(193) # Jalen McMillan

# inspect_individual_transaction(49) # Sam Darnold
# inspect_individual_transaction(108) # Michael Penix
# inspect_individual_transaction(359) # Jalen McMillan
# inspect_individual_transaction(16) # Cade Otton
