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

# Grades
source(here(data_path, "Draft Grades.R")) #Draft Grades, less than one second
source(here(data_path, "Trade Grades.R")) #Trade Grades, less than one second
source(here(data_path, "Transaction Grades.R")) #Transaction Grades, nine seconds


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

future_draft_pick_values <- known_draft_picks %>% select(-roster_id) %>%
  bind_rows(unknown_draft_picks)

# write_csv(future_draft_pick_values, here(data_path, "Data/future_draft_pick_values.csv"))

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

value_added <- read_csv(here(data_path, "Data/va_2024.csv")) %>%
  mutate(season = 2024) %>%
  left_join(users, by = join_by(roster_id)) %>%
  rename(team_name = display_name)

player_avenues <- total_assets %>%
  left_join(
    bind_rows(total_transaction_value) %>% #transactions
      filter(type == "add") %>%
      mutate(transaction_avenue = str_c(season, " W", week, " transaction")) %>%
      group_by(name, position) %>%
      slice_max((season + as.numeric(week)/20), n = 1) %>%
      select(-contains("value"), -type, -season, -week),
    by = join_by(name, position, team_name)) %>%
  left_join(
    bind_rows(total_trade_value) %>% # trades
      filter(type == "add") %>%
      mutate(trade_avenue = str_c(season, " W", week, " trade")) %>%
      group_by(name, position) %>%
      slice_max(season + as.numeric(week)/20, n = 1) %>%
      select(-contains("value"), -type, -season, -week),
    by = join_by(name, position, team_name)) %>%
  left_join(
    bind_rows(rookie_vs_expecations, .id = "season") %>% # rookie draft
      mutate(rookie_draft_avenue = str_c(as.numeric(season) + 2023, " rookie draft pick ", pick_no)) %>%
      select(name, position, roster_id, rookie_draft_avenue),
    by = join_by(name, position, roster_id)) %>%
  left_join(
    init_vs_expectation %>% # initial draft
      mutate(initial_draft_avenue = str_c("initial draft pick ", pick_no)) %>%
      select(name, position, roster_id, initial_draft_avenue),
    by = join_by(name, position, roster_id)) %>%
  mutate(
    t_avenue = case_when(
      is.na(transaction_avenue) ~ trade_avenue,
      is.na(trade_avenue) ~ transaction_avenue,
      as.numeric(str_extract(trade_avenue, "\\d{4}")) + as.numeric(str_extract(trade_avenue, "(?<=W)\\d+"))/20 >
        as.numeric(str_extract(transaction_avenue, "\\d{4}")) + as.numeric(str_extract(transaction_avenue, "(?<=W)\\d+"))/20 ~ trade_avenue,
      .default = transaction_avenue),
    avenue = coalesce(t_avenue, rookie_draft_avenue, initial_draft_avenue)) %>%
  select(roster_id, player_id, name, position, future_value, avenue) %>%
  mutate(avenue = case_when(
    is.na(avenue) & position == "K" ~ "initial draft",
    is.na(avenue) & str_detect(name, "Draft Pick") ~ "own pick",
    .default = avenue
  ))

grab_team_assets <- function(enter_roster_id){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    select(display_name) %>%
    slice(1)
  
  total_assets %>%
    left_join(player_avenues %>%
                select(name, position, roster_id, avenue),
              by = join_by(roster_id, name, position)) %>%
    filter(roster_id == enter_roster_id) %>%
    arrange(desc(future_value)) %>%
    select(name, position, future_value, avenue) %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_number(columns = future_value, decimals = 2) %>%
    cols_label(future_value = "Future Value", avenue = "Acquired") %>%
    tab_header(title = str_c(team_name, " Future Value"))
}

grab_team_assets(7)

# position outlook
position_outlook <- function(enter_roster_id){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    select(display_name) %>%
    slice(1)
  
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

# position_outlook(4)

# top contributors
grab_team_contributors <- function(enter_roster_id, enter_season){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    select(display_name) %>%
    slice(1)
  
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

# grab_team_contributors(4, enter_season = 2024)

# week by week
grab_team_contributors_weekly <- function(enter_roster_id, enter_season, enter_week){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    select(display_name) %>%
    slice(1)
  
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

# grab_team_contributors_weekly(4, 2024, 5)

# overall draft, trade, transaction grades

value_avenues <- bind_rows(
  bind_rows(rookie_draft_value, .id = "season") %>%
    mutate(avenue = str_c("rookie draft ", as.numeric(season) + 2023)) %>%
    rename(
      total_value = "total_draft_value") %>%
    select(-season, -display_name),
  initial_draft_value %>%
    mutate(
      value_over_expected = total_draft_value - mean(total_draft_value),
      avenue = "initial draft") %>%
    rename(
      total_value = "total_draft_value") %>%
    select(-display_name),
  overall_trade_winners %>%
    mutate(
      value_over_expected = total_trade_value,
      avenue = "trades") %>%
    rename(total_value = "total_trade_value") %>%
    select(-team_name),
  overall_transaction_winners %>%
    mutate(
      value_over_expected = total_transaction_value,
      avenue = "transactions") %>%
    rename(total_value = "total_transaction_value") %>%
    select(-team_name)) %>%
  mutate(avenue = factor(avenue, levels = c("initial draft", "rookie draft 2024", "transactions", "trades"))) %>%
  select(-total_value)

overall_grades <- function(value_avenues_df, enter_roster_id){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    select(display_name) %>%
    slice(1)
  
  value_avenues_df %>%
    filter(roster_id == enter_roster_id) %>%
    relocate(c(avenue, total_realized_value, total_future_value), value_over_expected) %>%
    select(-roster_id) %>%
    arrange(avenue) %>%
    janitor::adorn_totals() %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_number(columns = c(total_realized_value, total_future_value, value_over_expected), decimals = 2) %>%
    cols_label(total_realized_value = "Realized Value", total_future_value = "Future Value", value_over_expected = "Value Over Expected") %>%
    tab_header(title = str_c(team_name, " Avenue Grades"))
}

# value_avenues %>% overall_grades(11)

# acquisitions

acquisitions <- bind_rows(
  init_vs_expectation %>% # initial draft
    mutate(avenue = str_c("initial draft pick ", pick_no)) %>%
    select(-pick_no),
  bind_rows(rookie_vs_expecations, .id = "season") %>% # rookie drafts
    mutate(avenue = str_c(as.numeric(season) + 2023, " rookie draft pick ", pick_no)) %>%
    select(-exp_total_value, -pick_no, -season),
  
  comparison %>% # trades
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
    select(roster_id, name, position, realized_value, future_value, total_value, value_over_expected, avenue),
  
  transaction_comparison %>% # transactions
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
    select(roster_id, name, position, realized_value, future_value, total_value, value_over_expected, avenue)
)

# top acquisitions
top_acquisitions <- function(acquisitions_df, enter_roster_id, enter_avenue = " ") {
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    select(display_name) %>%
    slice(1)
  
  acquisitions %>%
    filter(str_detect(avenue, enter_avenue), roster_id == enter_roster_id) %>%
    slice_max(value_over_expected, n = 5) %>%
    select(-roster_id, -total_value) %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_number(columns = c(realized_value, future_value, value_over_expected), decimals = 2) %>%
    cols_label(realized_value = "Realized Value", future_value = "Future Value", value_over_expected = "Value Over Expected") %>%
    tab_header(title = str_c(team_name, " Top Acquisitions"))
}

# acquisitions %>% top_acquisitions(4)
# acquisitions %>% top_acquisitions(4, "initial draft")
# acquisitions %>% top_acquisitions(4, "transaction")
# acquisitions %>% top_acquisitions(4, "trade")
# acquisitions %>% top_acquisitions(4, "rookie draft")

# worst acquisitions
worst_acquisitions <- function(acquisitions_df, enter_roster_id, enter_avenue = " ") {
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    select(display_name) %>%
    slice(1)
  
  acquisitions %>%
    filter(str_detect(avenue, enter_avenue), roster_id == enter_roster_id) %>%
    slice_min(value_over_expected, n = 5) %>%
    select(-roster_id, -total_value) %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_number(columns = c(realized_value, future_value, value_over_expected), decimals = 2) %>%
    cols_label(realized_value = "Realized Value", future_value = "Future Value", value_over_expected = "Value Over Expected") %>%
    tab_header(title = str_c(team_name, " Worst Acquisitions"))
}

acquisitions %>% worst_acquisitions(4)
# acquisitions %>% worst_acquisitions(4, "initial draft")
# acquisitions %>% worst_acquisitions(4, "transaction")
# acquisitions %>% worst_acquisitions(4, "trade")
# acquisitions %>% worst_acquisitions(4, "rookie draft")

# team composition

team_composition <- function(player_avenues_df, enter_roster_id){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    select(display_name) %>%
    slice(1)
  
  player_avenues %>%
    filter(roster_id == enter_roster_id) %>%
    mutate(
      avenue_type = case_when(
        str_detect(avenue, "initial draft") ~ "initial draft",
        str_detect(avenue, "rookie draft") ~ "rookie draft",
        str_detect(avenue, "transaction") ~ "transaction",
        str_detect(avenue, "trade") ~ "trade",
        str_detect(avenue, "own pick") ~ "own pick",
        .default = NA)) %>%
    group_by(avenue_type) %>%
    summarize(future_value = sum(future_value)) %>%
    mutate(proportion = scales::percent(future_value/sum(future_value), accuracy = .01)) %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_number(columns = c(future_value), decimals = 2) %>%
    cols_label(avenue_type = "Avenue", future_value = "Future Value") %>%
    tab_header(title = str_c(team_name, " Team Composition"))
}

player_avenues %>% team_composition(7)
