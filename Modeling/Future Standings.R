# This is a modest attempt to predict the future standings of the league for the next three years based
# on the expected value added from players on the roster. It is by no means comprehensive and mainly necessary
# for valuing future draft picks

library("here")
library("tictoc")
library("tidyverse"); theme_set(theme_minimal())
library("furrr")
data_path <- "FantasyDynasty/"

# load data
load(here(data_path, "Modeling/player_simulations.RData"))
player_info <- read_csv(here(data_path, "Data/player_info.csv"))

future_draft_picks <- read_csv(here(data_path, "Data/future_draft_picks.csv"))
rookie_draft_values <- read_csv(here(data_path, "Data/rookie_draft_values.csv"))

current_roster <- read_csv(here(data_path, "Data/current_roster.csv")) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  select(name, position, roster_id)

va <- read_csv(here(data_path, "Data/va.csv"))

draft_order <- read_csv(here(data_path, "Data/draft_order.csv"))

assign_draft_pick_value <- function(dp_df, years_in_advance = "total value"){
  dp_df <- dp_df %>% filter(!is.na(draft_order))
  
  if(nrow(dp_df) == 0){
    tibble()
  }else{
    dp_df %>%
      mutate(pick_no = (round-1)*12 + draft_order) %>%
      select(roster_id, pick_no) %>%
      left_join(rookie_draft_values %>% filter(metric == years_in_advance), by = join_by(pick_no)) %>%
      select(-pick_no, -metric)
  }
}

sample_quantiles <- function(data){
  data %>%
    select(-roster_id) %>%
    apply(1, function(row) sample(row, 1)) %>%
    as_tibble() %>%
    mutate(roster_id = data$roster_id) %>%
    group_by(roster_id) %>%
    summarize(
      va = sum(value)) %>%
    filter(!is.na(roster_id)) %>%
    transmute(
      roster_id = roster_id,
      rank = rank(desc(va)),
      draft_order = rank(va))
}

prep_draft_picks <- function(prev_year, year, years_ahead = "first year"){
  draft_picks_year <- future_draft_picks %>%
    filter(season == year)
  
  # if draft order is not set, find it from previous year standings
  if(is.na(draft_picks_year$draft_order[1])){
    draft_picks_year <- draft_picks_year %>%
      select(-draft_order) %>%
      left_join(prev_year %>% select(roster_id, draft_order), by = join_by(pick_slot == roster_id))
  }
  draft_picks_order <- draft_picks_year %>%
    select(roster_id, draft_order, round) %>%
    assign_draft_pick_value(years_ahead)
  
}


# the idea here, is for each player, I randomly sample from one of their quartiles, this accounts for the variation
# in their season, but also keeps the mean where it should be. It trims the variance, slightly,
# but this isn't a major concern right now.

# find year, this is needed for mapping below
this_year <- names(player_simulations)[1] %>% as.numeric()

# find draft picks to come this season
known_draft_picks_year1 <- future_draft_picks %>%
  prep_draft_picks(this_year, "first year")

known_draft_picks_year2 <- future_draft_picks %>%
  prep_draft_picks(this_year, "second year")

known_draft_picks_year3 <- future_draft_picks %>%
  prep_draft_picks(this_year, "third year")

n_sim <- 5000
tic()
team_tva_ranking <- future_map(1:n_sim, ~{
  # year 1 all assets with quantiles
  data1 <- player_simulations[[1]] %>%
    left_join(current_roster, by = join_by(name)) %>%
    select(roster_id, contains("proj")) %>%
    bind_rows(known_draft_picks_year1)
  
  # sample quantiles from year 1 for all assets and compute season rank
  standings_year1 <- data1 %>% sample_quantiles()
  
  # find quantiles of draft picks for year 2
  draft_picks2_year1 <- prep_draft_picks(standings_year1, year = this_year + 1, "first year")
  draft_picks2_year2 <- prep_draft_picks(standings_year1, year = this_year + 1, "second year")
  
  # combine all of year 2 assets with quantiles
  data2 <- player_simulations[[2]] %>%
    left_join(current_roster, by = join_by(name)) %>%
    select(roster_id, contains("proj")) %>%
    bind_rows(known_draft_picks_year2, draft_picks2_year1)
  
  # sample quantiles from year 2 for all assets and compute season rank
  standings_year2 <- data2 %>% sample_quantiles()
  
  # find quantiles of draft picks for year 3 (based on year 2 standings)
  draft_picks3_year1 <- prep_draft_picks(standings_year2, year = this_year + 2, "first year")
  
  # combine all of year 3 assets with quantiles
  data3 <- player_simulations[[3]] %>%
    left_join(current_roster, by = join_by(name)) %>%
    select(roster_id, contains("proj")) %>%
    bind_rows(known_draft_picks_year3, draft_picks2_year2, draft_picks3_year1)
  
  # sample quantiles from year 3 for all assets and compute season rank
  standings_year3 <- data3 %>% sample_quantiles()
  
  # compute final standings df
  list(year1 = standings_year1, year2 = standings_year2, year3 = standings_year3)
  },
  .progress = TRUE,
  .options=furrr_options(seed=TRUE)) %>%
  transpose() %>%
  map(bind_rows)
toc()

# convert team_tva_ranking to win standings odds ------------------------------------
# need data on previous years gap between difference in tva_ranking and standing

resid_va_fs <- va %>% group_by(season, roster_id) %>%
  summarize(va = sum(value_added)) %>%
  mutate(va_rank = rank(desc(va))) %>%
  ungroup() %>%
  # join with final standings
  left_join(draft_order %>%
              mutate(
                final_standings = 13 - draft_order,
                season = season - 1),
            by = join_by(season, roster_id)) %>%
  mutate(resid = va_rank - final_standings) %>%
  pull(resid)

# find mle of var (assuming one-to-one relationship)
var_va_fs <- sum(resid_va_fs^2)/(length(resid_va_fs) - 1)

# add variance to final standings
standings <- map(team_tva_ranking, ~{
  rand <- rnorm(nrow(.x), 0, sd = sqrt(var_va_fs))
  
  .x %>% mutate(
    sim = rep(1:ceiling(nrow(.x)/12), each = 12),
    rand_rank = rank + rand) %>%
    group_by(sim) %>%
    mutate(rank = rank(rand_rank),
           draft_order = 13 - rank) %>%
    ungroup() %>%
    select(roster_id, rank, draft_order)
})

# compute final_standings_odds
final_standings_odds <- map(standings, ~{
  .x %>% group_by(roster_id, rank) %>%
    summarize(perc = n()/n_sim,
              .groups = "keep") %>%
    ungroup()}) %>%
  bind_rows(.id = "season") %>%
  mutate(season = this_year + as.numeric(str_remove(season, "year")) - 1)

write_csv(final_standings_odds, here(data_path, "Data/final_standings_odds.csv"))
