# Fantasy Teams
# this page is for interesting summaries of fantasy teams- eventually I'd like this to be a page in
# an RShiny

library("here")
data_path <- "FantasyDynasty/"

# load data
source(here(data_path, "Scripts/Script Support.R"))

# Grades
source(here(data_path, "Scripts/Draft Grades.R")) #Draft Grades, about one second
source(here(data_path, "Scripts/Trade Grades.R")) #Trade Grades, less than one second
source(here(data_path, "Scripts/Transaction Grades.R")) #Transaction Grades, nine seconds

# load data
season_value_added <- read_csv(here(data_path, "Data/sva.csv"), show_col_types = FALSE)
player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"), , show_col_types = FALSE)
player_info <- read_csv(here(data_path, "Data/player_info.csv"), , show_col_types = FALSE)
final_standings_odds <- read_csv(here(data_path, "Data/final_standings_odds.csv"), , show_col_types = FALSE)
rookie_draft_values <- read_csv(here(data_path, "Data/rookie_draft_values.csv"), , show_col_types = FALSE) %>%
  filter(metric == "total value") %>%
  select(pick_no, proj_tva_50)

users <- read_csv(here(data_path, "Data/users.csv"), show_col_types = FALSE) %>%
  select(-owner_id)

future_draft_picks <- read_csv(here(data_path, "Data/future_draft_picks.csv"), show_col_types = FALSE)

load(here(data_path, "Data/draft_picks.RData"))

# picks that we known the draft order
known_draft_picks <- 
  if(nrow(future_draft_picks %>% filter(!is.na(draft_order))) == 0){
    tibble()
  }else{
    future_draft_picks %>%
      filter(!is.na(draft_order)) %>%
      mutate(pick_no = (round-1)*12 + draft_order) %>%
      select(roster_id, season, round, pick_no, pick_slot) %>%
      left_join(rookie_draft_values, by = join_by(pick_no)) %>%
      select(-pick_no)
  }

# but! we also need to value all of the draft picks of which we don't know the order
exp_draft_values <- final_standings_odds %>%
  expand_grid(round = 1:3) %>%
  mutate(pick_no = (round-1)*12 + (13-rank)) %>%
  left_join(rookie_draft_values, by = join_by(pick_no)) %>%
  # weight each potential draft pick by odds of receiving it
  mutate(weight_value = perc*proj_tva_50) %>%
  # for each roster and season and round
  group_by(season, roster_id, round) %>%
  summarize(exp_total_value = sum(weight_value),
            .groups = "keep") %>%
  ungroup() %>%
  # but draft in next season
  mutate(season = season + 1)

unknown_draft_picks <- future_draft_picks %>%
  filter(is.na(draft_order)) %>%
  left_join(exp_draft_values, by = join_by(season, round, pick_slot == roster_id)) %>%
  select(roster_id, season, round, exp_total_value, pick_slot)

future_draft_pick_exp_values <- unknown_draft_picks %>%
  bind_rows(known_draft_picks) %>%
  select(-roster_id)

realized_draft_pick_exp_values <- bind_rows(draft_picks, .id = "draft_id") %>% filter(draft_id != 2) %>%
  left_join(rookie_draft_values, by = join_by(pick_no)) %>%
  mutate(season = if_else(as.numeric(draft_id) == 1, 2023 + as.numeric(draft_id), 2022 + as.numeric(draft_id))) %>%
  left_join(draft_order %>% filter(type == "rookie"), by = join_by(season, draft_slot == draft_order)) %>%
  rename(pick_slot = roster_id.y, exp_total_value = proj_tva_50) %>%
  select(season, round, exp_total_value, pick_slot)

all_draft_pick_exp_values <- bind_rows(realized_draft_pick_exp_values, future_draft_pick_exp_values)

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
current_roster <- read_csv(here(data_path, "Data/current_roster.csv"), show_col_types = FALSE) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  left_join(player_total_value, by = join_by(player_id, name, position)) %>%
  select(roster_id, player_id, name, position, future_value, sva_2024) %>%
  left_join(users, by = join_by(roster_id)) %>%
  rename(team_name = display_name)

total_assets <- current_roster %>%
  select(-sva_2024) %>%
  bind_rows(draft_assets) %>%
  mutate(future_value = replace_na(future_value, 0))

value_added <- read_csv(here(data_path, "Data/va.csv"), show_col_types = FALSE) %>%
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
    avenue = coalesce(rookie_draft_avenue, initial_draft_avenue, t_avenue)) %>%
  select(roster_id, player_id, name, position, future_value, avenue) %>%
  mutate(avenue = case_when(
    is.na(avenue) & position == "K" ~ "initial draft",
    is.na(avenue) & str_detect(name, "Draft Pick") ~ "own pick",
    .default = avenue
  ))

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
      avenue = "trade") %>%
    rename(total_value = "total_trade_value") %>%
    left_join(users, by = join_by(team_name == display_name)) %>%
    select(-team_name, -trades),
  overall_transaction_winners %>%
    mutate(
      value_over_expected = total_transaction_value,
      avenue = "transaction") %>%
    rename(total_value = "total_transaction_value") %>%
    left_join(users, by = join_by(team_name == display_name)) %>%
    select(-team_name, -transactions)) %>%
  mutate(avenue = factor(avenue, levels = c("initial draft", "rookie draft 2024", "rookie draft 2025", "transaction", "trade"))) %>%
  select(-total_value)

# acquisitions
acquisitions <- bind_rows(
  init_vs_expectation %>% # initial draft
    mutate(avenue = str_c("initial draft pick ", pick_no)) %>%
    select(-pick_no),
  bind_rows(rookie_vs_expecations, .id = "season") %>% # rookie drafts
    mutate(avenue = str_c(as.numeric(season) + 2023, " rookie draft pick ", pick_no)) %>%
    select(-proj_tva_50, -pick_no, -season),
  individual_trades %>%  # trades
    select(-trade_id),
  individual_transactions %>% # transactions
    select(-transaction_id)
  )

# dfs to save -------------------------------------------------------------

grab_team_assets_df <- total_assets %>%
  left_join(player_avenues %>%
              select(name, position, roster_id, avenue),
            by = join_by(roster_id, name, position)) %>%
  arrange(desc(future_value)) %>%
  select(roster_id, name, position, future_value, avenue)

write_csv(grab_team_assets_df, here(data_path, "Scripts/Saved Files/grab_team_assets_df.csv"))

realized_value <- value_added %>%
  # filter(roster_id == enter_roster_id) %>%
  group_by(roster_id, position) %>%
  summarize(
    realized_value = sum(value_added)) %>%
  ungroup()

position_outlook_df <- current_roster %>%
  group_by(roster_id, position) %>%
  summarize(
    future_value = sum(future_value)) %>%
  ungroup() %>%
  left_join(realized_value, by = join_by(position, roster_id)) %>%
  mutate(
    position = factor(position, position_levels),
    across(future_value:realized_value, ~replace_na(.x, 0))) %>%
  relocate(realized_value, .after = position) %>%
  arrange(position)

write_csv(position_outlook_df, here(data_path, "Scripts/Saved Files/position_outlook_df.csv"))
  
write_csv(value_avenues, here(data_path, "Scripts/Saved Files/value_avenues.csv"))
write_csv(acquisitions, here(data_path, "Scripts/Saved Files/acquisitions.csv"))
write_csv(player_avenues, here(data_path, "Scripts/Saved Files/player_avenues.csv"))

write_csv(all_draft_pick_exp_values, here(data_path, "Data/all_draft_pick_exp_values.csv"))

# Examples ----------------------------------------------------------------
# grab_team_assets(4, shiny = TRUE)
# 
# position_outlook(4, shiny = TRUE)
# 
# grab_team_contributors(4, enter_season = 2024, shiny = TRUE)
# 
# grab_team_contributors_weekly(enter_roster_id = 4, enter_season = 2024, enter_week = 5, shiny = TRUE)
# 
# value_avenues %>% overall_grades(enter_roster_id = 6, shiny = TRUE)

acquisitions %>% top_acquisitions(4, "trade", shiny = TRUE)
# acquisitions %>% top_acquisitions(4, "initial draft")
# acquisitions %>% top_acquisitions(4, "transaction")
# acquisitions %>% top_acquisitions(4, "trade")
# acquisitions %>% top_acquisitions(4, "rookie draft")

# acquisitions %>% worst_acquisitions(4)
# acquisitions %>% worst_acquisitions(4, "initial draft")
# acquisitions %>% worst_acquisitions(1, "transaction")
# acquisitions %>% worst_acquisitions(4, "trade")
# acquisitions %>% worst_acquisitions(4, "rookie draft")

# player_avenues %>% team_composition(4)
