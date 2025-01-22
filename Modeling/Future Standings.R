# This is a modest attempt to predict the future standings of the league for the next three years based
# on the expected value added from players on the roster. It is by no means comprehensive and mainly necessary
# for valuing future draft picks

library("here")
library("tictoc")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

# load data
load(here(data_path, "Data/simulations.RData"))
player_info <- read_csv(here(data_path, "Data/player_info.csv"))

future_draft_picks <- read_csv(here(data_path, "Data/future_draft_picks.csv"))
rookie_draft_values <- read_csv(here(data_path, "Data/rookie_draft_values.csv"))

assign_draft_pick_value <- function(dp_df, place_marker){
  dp_df %>%
    filter(!is.na(draft_order)) %>%
    mutate(pick_no = (round-1)*12 + draft_order) %>%
    select(roster_id, season, pick_no) %>%
    left_join(rookie_draft_values, by = join_by(pick_no)) %>%
    select(roster_id, contains("ny")) %>%
    rename_with(~{
      # Create a sequence starting from input_value + 1
      seq_numbers <- seq(place_marker + 1, place_marker + length(.x))
      # Replace column names with proj_tva_<number>
      str_replace(.x, ".*", paste0("proj_tva_", seq_numbers))},
    starts_with("exp_value")
    )
}

known_draft_picks <- future_draft_picks %>%
  assign_draft_pick_value(0)

current_roster <- read_csv(here(data_path, "Data/current_roster.csv")) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  select(name, position, roster_id)

# find year, this is needed for mapping below
year <- simulations[[1]] %>%
  select(contains("proj_tva")) %>%
  colnames() %>%
  pluck(1) %>%
  str_remove("proj_tva_") %>%
  as.numeric()

# this is kinda a lot, but it finds the standings for each of the next three years following the simulations
# also, it unfortunately doesn't account for variation in rookie draft values pooey
tic()
standings <- map(simulations, ~{
  data <- .x %>%
    rename_with(~{
      years <- as.numeric(str_extract(.x, "\\d{4}"))
      seq_numbers <- match(years, sort(unique(years)))
      str_replace(.x, "\\d{4}", as.character(seq_numbers))}) %>%
    left_join(current_roster, by = join_by(name, position)) %>%
    bind_rows(known_draft_picks)
  
  ny <- data %>%
    group_by(roster_id) %>%
    summarize(
      va_1 = sum(proj_tva_1)) %>%
    filter(!is.na(roster_id)) %>%
    transmute(
      roster_id = roster_id,
      rank1 = rank(desc(va_1)),
      draft_order = rank(va_1))
  
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
