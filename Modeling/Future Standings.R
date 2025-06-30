# This is a modest attempt to predict the future standings of the league for the next three years based
# on the expected value added from players on the roster. It is by no means comprehensive and mainly necessary
# for valuing future draft picks

library("here")
library("tictoc")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

# load data
load(here(data_path, "Modeling/player_simulations.RData"))
player_info <- read_csv(here(data_path, "Data/player_info.csv"))

future_draft_picks <- read_csv(here(data_path, "Data/future_draft_picks.csv"))
rookie_draft_values <- read_csv(here(data_path, "Data/rookie_draft_values.csv"))

assign_draft_pick_value <- function(dp_df, years_in_advance = "total value"){
  dp_df <- dp_df %>% filter(!is.na(draft_order))
  
  if(nrow(dp_df) == 0){
    tibble()
  }else{
    data2 %>%
      mutate(pick_no = (round-1)*12 + draft_order) %>%
      select(roster_id, season, pick_no) %>%
      left_join(rookie_draft_values %>% filter(metric == years_in_advance), by = join_by(pick_no)) %>%
      select(-season, -pick_no, -metric)
  }
}

# this is deprecated
known_draft_picks <- future_draft_picks %>%
  assign_draft_pick_value(0)

current_roster <- read_csv(here(data_path, "Data/current_roster.csv")) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  select(name, position, roster_id)

# find year, this is needed for mapping below
year <- names(player_simulations)[1] %>% as.numeric()

# the idea here, is for each player, I randomly sample from one of their quartiles, this accounts for the variation
# in their season, but also keeps the mean where it should be. It trims the variance, slightly,
# but this isn't a major concern right now.




year2 <- player_simulations[[2]] %>% select(-name) %>% apply(1, function(row) sample(row, 1))
year3 <- player_simulations[[3]] %>% select(-name) %>% apply(1, function(row) sample(row, 1))

sample_quantiles <- function(data){
  data %>%
    select(-roster_id) %>%
    apply(1, function(row) sample(row, 1)) %>%
    as_tibble() %>%
    mutate(roster_id = data1$roster_id) %>%
    group_by(roster_id) %>%
    summarize(
      va_1 = sum(value)) %>%
    filter(!is.na(roster_id)) %>%
    transmute(
      roster_id = roster_id,
      rank1 = rank(desc(va_1)),
      draft_order = rank(va_1))
}

prep_draft_picks <- function(prev_year, years_ahead = "first year"){
  future_draft_picks %>%
    filter(season == year + 1) %>%
    select(-draft_order) %>%
    left_join(prev_year %>% select(roster_id, draft_order), by = join_by(pick_slot == roster_id)) %>%
    select(roster_id, draft_order) %>%
    assign_draft_pick_value(years_ahead)
}


standings <- map(1:n_sim, ~{
  
  # draft order from year 1
  data1 <- player_simulations[[1]] %>% 
    left_join(current_roster, by = join_by(name)) %>%
    select(roster_id, contains("proj")) %>%
    bind_rows(known_draft_picks)
  
  
  year1 <- data1 %>% sample_quantiles()
    
  draft_picks2 <- prep_draft_picks(year1, "first year")
  
  # HERE
  
  data2 <- player_simulations[[]]
  
  
  # data <- .x %>%
  #   rename_with(~{
  #     years <- as.numeric(str_extract(.x, "\\d{4}"))
  #     seq_numbers <- match(years, sort(unique(years)))
  #     str_replace(.x, "\\d{4}", as.character(seq_numbers))}) %>%
  #   left_join(current_roster, by = join_by(name, position)) %>%
  #   bind_rows(known_draft_picks)
  # 
  # ny <- data %>%
  #   group_by(roster_id) %>%
  #   summarize(
  #     va_1 = sum(proj_tva_1)) %>%
  #   filter(!is.na(roster_id)) %>%
  #   transmute(
  #     roster_id = roster_id,
  #     rank1 = rank(desc(va_1)),
  #     draft_order = rank(va_1))
  
  data2 <- future_draft_picks %>%
    filter(season == year + 2) %>%
    select(-draft_order) %>%
    left_join(ny %>% select(-contains("rank")), by = join_by(pick_slot == roster_id)) %>%
    assign_draft_pick_value(1) %>%
    bind_rows(data, .)
  
  ny2 <- data2 %>%
    group_by(roster_id) %>%
    summarize(
      va_2 = sum(proj_tva_2)) %>%
    filter(!is.na(roster_id)) %>%
    transmute(
      roster_id = roster_id,
      rank2 = rank(desc(va_2)),
      draft_order = rank(va_2))
  
  data3 <- future_draft_picks %>%
    filter(season == year + 3) %>%
    select(-draft_order) %>%
    left_join(ny2 %>% select(-contains("rank")), by = join_by(pick_slot == roster_id)) %>%
    assign_draft_pick_value(2) %>%
    bind_rows(data2, .)
  
  ny3 <- data3 %>%
    group_by(roster_id) %>%
    summarize(
      va_3 = sum(proj_tva_3)) %>%
    filter(!is.na(roster_id)) %>%
    transmute(
      roster_id = roster_id,
      rank3 = rank(desc(va_3)),
      draft_order = rank(va_3))
  
  ny %>%
    left_join(ny2, by = join_by(roster_id)) %>%
    left_join(ny3, by = join_by(roster_id)) %>%
    select(roster_id, contains("rank"))
  }) %>%
  bind_rows(.id = "simulation_id")
toc()

final_standings_odds <- map(c(3, 4, 5), 
    ~standings[,c(2,.x)] %>%
      rename_with(~paste0("rank"), starts_with("rank")) %>%
      group_by(roster_id, rank) %>%
      summarize(perc = n()/10000) %>%
      ungroup()) %>%
  bind_rows(.id = "season") %>%
  mutate(season = year + as.numeric(season) - 1)

# write_csv(final_standings_odds, here(data_path, "Data/final_standings_odds.csv"))
