# This is a modest attempt to predict the future standings of the league for the next three years based
# on the expected value added from players on the roster. It is by no means comprehensive and mainly necessary
# for valuing future draft picks

library("here")

source(here("Modeling/Future Standings Support.R"))
# load data ---------------------------------------------------------------
load(here("Modeling/player_simulations.RData"))
player_info <- read_csv(here("Data/player_info.csv"))

future_draft_picks <- read_csv(here("Data/future_draft_picks.csv"))
rookie_draft_values <- read_csv(here("Data/rookie_draft_values.csv"))

current_roster <- read_csv(here("Data/current_roster.csv")) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  select(name, position, roster_id)

va <- read_csv(here("Data/va.csv"))

draft_order <- read_csv(here("Data/draft_order.csv"))

matchups_table <- read_csv(here("Data/matchups_table.csv"))

season_dates <- read_csv(here("Data/season_dates.csv"))

# the idea here, is for each player, I randomly sample from one of their quartiles, this accounts for the variation
# in their season, but also keeps the mean where it should be. It trims the variance, slightly,
# but this isn't a major concern right now.

# team tva ranking --------------------------------------------------------

# find year, this is needed for mapping below
this_year <- names(player_simulations)[1] %>% as.numeric()

# find draft picks to come this season
known_draft_picks_year1 <- future_draft_picks %>%
  prep_draft_picks(this_year, "first year")

known_draft_picks_year2 <- future_draft_picks %>%
  prep_draft_picks(this_year, "second year")

known_draft_picks_year3 <- future_draft_picks %>%
  prep_draft_picks(this_year, "third year")

n_sims <- 5000
tic()
team_tva_ranking <- future_map(1:n_sims, ~{
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

save(team_tva_ranking, file = here("Modeling/team_tva_ranking.RData"))
load(here("Modeling/team_tva_ranking.RData"))

# Here, I'll simulate a season week-by-week.
# this is a more intense and specific approach to the future standings question. 

# first, estimate win probability of matchup based on value added ---------
team_va <- va %>% group_by(roster_id) %>% summarize(total_va = sum(value_added))

matchup_va_data <- matchups_table %>% filter(points != 0) %>%
  left_join(team_va, by = join_by(roster_id)) %>%
  left_join(team_va %>% rename(opp_va = total_va), by = join_by(opponent_id == roster_id)) %>%
  mutate(
    victory = as.factor(if_else(points > opp_points, 1, 0)),
    va_diff = total_va - opp_va)

matchup_fit <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(
    victory ~ va_diff - 1,
    data = matchup_va_data)

matchup_fit_coef <- matchup_fit$fit %>% coef() %>% as.data.frame()
write_csv(matchup_fit_coef, here("Modeling/matchup_fit_coef.csv"))
# Run Season --------------------------------------------------------------

team_tva_list <- map(team_tva_ranking, ~.x %>% mutate(group = (row_number() - 1)%/% 12) %>% group_split(group, .keep = FALSE)) %>%
  transpose()

current_table <- construct_table(matchups_table, season_dates, today())

tic()
final_standings_odds <- compute_final_standings_odds(current_table, team_tva_list, matchup_fit_coef, 3, n_sims = 50)
toc()

write_csv(final_standings_odds, here("Data/final_standings_odds.csv"))



