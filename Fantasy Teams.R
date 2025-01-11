# Fantasy Teams
# this page is for interesting summaries of fantasy teams- eventually I'd like this to be a page in
# an RShiny

library("here")
library("gt")
library("gtExtras")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

# load data
season_value_added <- read_csv(here(data_path, "Data/sva_2024.csv"))
player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"))
player_info <- read_csv(here(data_path, "Data/player_info.csv"))
final_standings_odds <- read_csv(here(data_path, "Data/final_standings_odds.csv"))
rookie_draft_values <- read_csv(here(data_path, "Data/rookie_draft_values.csv")) %>%
  select(pick_no, exp_total_value)

users <- read_csv(here(data_path, "Data/users.csv")) %>%
  select(-owner_id)

#position_levels
position_levels <- c("QB", "RB", "WR", "TE", "K", "DST")


future_draft_picks <- read_csv(here(data_path, "Data/future_draft_picks.csv"))

# picks that we known the draft order
known_draft_picks <- future_draft_picks %>%
  filter(!is.na(draft_order)) %>%
  mutate(pick_no = (round-1)*12 + draft_order) %>%
  select(roster_id, season, round, pick_no, pick_slot) %>%
  left_join(rookie_draft_values, by = join_by(pick_no)) %>%
  select(-pick_no)

# but! we also need to value all of the draft picks of which we don't know the order
exp_draft_values <- final_standings_odds %>%
  expand_grid(round = 1:3) %>%
  mutate(pick_no = (round-1)*12 + (13-rank)) %>%
  left_join(rookie_draft_values, by = join_by(pick_no)) %>%
  # weight each potential draft pick by odds of receiving it
  mutate(weight_value = perc*exp_total_value) %>%
  # for each roster and season and round
  group_by(season, roster_id, round) %>%
  summarize(exp_total_value = sum(weight_value)) %>%
  ungroup() %>%
  # but draft in next season
  mutate(season = season + 1)

unknown_draft_picks <- future_draft_picks %>%
  filter(is.na(draft_order)) %>%
  left_join(exp_draft_values, by = join_by(season, round, pick_slot == roster_id)) %>%
  select(roster_id, season, round, exp_total_value, pick_slot)

# write_csv(unknown_draft_picks, here(data_path, "Data/unknown_draft_picks.csv"))

# draft picks
draft_assets <- bind_rows(known_draft_picks, unknown_draft_picks) %>%
  mutate(
    player_id = NA,
    name = str_c(season, " ", round, "R Draft Pick"),
    future_value = exp_total_value * .95^(season - 2025), # slowly devalue draft picks
    future_value = future_value*.85 # totally arbitrary uncertainty penalty
    ) %>%
  left_join(users, by = join_by(pick_slot == roster_id)) %>%
  rename(position = display_name) %>%
  select(roster_id, player_id, name, position, future_value) %>%
  left_join(users, by = join_by(roster_id)) %>%
  rename(team_name = display_name)
  
# future assets
current_roster <- read_csv(here(data_path, "Data/current_roster.csv")) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  left_join(player_total_value, by = join_by(player_id, name, position, birth_date)) %>%
  select(roster_id, player_id, name, position, future_value, sva_2024) %>%
  left_join(users, by = join_by(roster_id)) %>%
  rename(team_name = display_name)

total_assets <- current_roster %>%
  select(-sva_2024) %>%
  bind_rows(draft_assets)

# total_assets %>% arrange(desc(future_value)) %>%
#   print(n=30)

grab_team_assets <- function(enter_roster_id){
  team_name <- total_assets %>%
    filter(roster_id == enter_roster_id) %>%
    select(team_name) %>%
    slice(1) %>%
    pull()
  
  total_assets %>%
    filter(roster_id == enter_roster_id) %>%
    arrange(desc(future_value)) %>%
    select(name, position, future_value) %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_number(columns = future_value, decimals = 2) %>%
    cols_label(future_value = "Future Value") %>%
    tab_header(title = str_c(team_name, " Future Value"))
}

# grab_team_assets(7)

# position outlook
position_outlook <- function(enter_roster_id){
  team_name <- current_roster %>%
    left_join(users, by = join_by(roster_id)) %>%
    filter(roster_id == enter_roster_id) %>%
    select(team_name) %>%
    slice(1) %>%
    pull()
  
  realized_value <- value_added %>%
    filter(roster_id == enter_roster_id) %>%
    group_by(position) %>%
    summarize(
      realized_value = sum(value_added)) %>%
    ungroup()
  
  current_roster %>%
    filter(roster_id == enter_roster_id) %>%
    group_by(position) %>%
    summarize(
      future_value = sum(future_value)) %>%
    ungroup() %>%
    left_join(realized_value, by = join_by(position)) %>%
    mutate(position = factor(position, position_levels)) %>%
    relocate(realized_value, .after = position) %>%
    arrange(position) %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_number(columns = c(future_value, realized_value), decimals = 2) %>%
    cols_label(future_value = "Future Value", realized_value = "Realized Value") %>%
    tab_header(title = str_c(team_name, " Position Outlook"))
}

position_outlook(11)

# top contributors
value_added <- read_csv(here(data_path, "Data/va_2024.csv")) %>%
  mutate(season = 2024) %>%
  left_join(users, by = join_by(roster_id)) %>%
  rename(team_name = display_name)

grab_team_contributors <- function(enter_roster_id, enter_season){
  team_name <- value_added %>%
    filter(roster_id == enter_roster_id) %>%
    select(team_name) %>%
    slice(1) %>%
    pull()
  
  value_added %>% 
    filter(roster_id == enter_roster_id, season == enter_season) %>%
    group_by(name, position) %>%
    summarize(
      weeks = n(),
      healthy = sum(sleeper_points > 0),
      starts = sum(type == "starter"),
      total_value_added = sum(value_added),
      fantasy_points = sum(sleeper_points),
      .groups = "keep") %>%
    arrange(desc(total_value_added)) %>%
    ungroup() %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_number(columns = c(total_value_added, fantasy_points), decimals = 2) %>%
    cols_label(total_value_added = "Total Value Added", fantasy_points = "Fantasy Points") %>%
    tab_header(title = str_c(team_name, ": ", enter_season, " Season"))
}

# grab_team_contributors(4, 2024)

# week by week
grab_team_contributors_weekly <- function(enter_roster_id, enter_season, enter_week){
  team_name <- value_added %>%
    filter(roster_id == enter_roster_id) %>%
    select(team_name) %>%
    slice(1) %>%
    pull()
  
  value_added %>% 
    filter(roster_id == enter_roster_id, season == enter_season, week == enter_week,
           type == "starter") %>%
    select(name, position, projection, sleeper_points, value_added) %>%
    mutate(position = factor(position, position_levels)) %>%
    arrange(position, desc(value_added)) %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_number(columns = c(value_added, sleeper_points, projection), decimals = 2) %>%
    cols_label(value_added = "Value Added", sleeper_points = "Fantasy Points") %>%
    tab_header(title = str_c(team_name, ": ", enter_season, " Season Week ", enter_week))
}

grab_team_contributors_weekly(4, 2024, 5)

# recent transactions (and grades)





