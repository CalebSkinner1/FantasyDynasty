# Future Value and Total Value

library("here")

# source(here("Modeling/Player Value Added.R")) # grab updated value added ~50 seconds
source(here("Data Manipulation/Scrape Support.R")) # grab functions
source(here("Modeling/Player Total Value Functions.R")) # grab functions
season_value_added <- read_csv(here("Data/sva.csv")) # shortcut
player_info <- read_csv(here("Data/player_info.csv")) # shortcut 

keep_trade_cut <- read_csv(here("Data/ktc values/ktc_value070825.csv")) # shortcut
sleeper_points <- read_csv(here("Data/sleeper_points24.csv")) # shortcut

# organize data sets

# get ktc value from beginning of 2024 season
historical_ktc <- read_csv(here("Data/ktc values/ktc_value082324.csv")) %>%
  filter(!str_detect(name, "Early"), !str_detect(name, "Mid"), !str_detect(name, "Late")) %>%
  name_correction() %>%
  group_by(name) %>%
  summarize(value = max(value)) %>%
  arrange(desc(value))

# create data matrix
hktc_data <- historical_ktc %>%
  rename("historical_value" = "value") %>%
  left_join(season_value_added, by = join_by(name)) %>%
  select(-total_points) %>%
  mutate(
    total_value_added = replace_na(total_value_added, 0),
    # adjust total value added to account for two missing games
    tva_adj = case_when(
      total_value_added < 0 ~ total_value_added,
      .default = total_value_added * 17/max(sleeper_points$week))) %>%
  left_join(keep_trade_cut, by = join_by(name)) %>%
  select(-position) %>%
  left_join(player_info, by = join_by(name)) %>%
  select(-player_id) %>%
  mutate(
    age = interval(birth_date, ymd("2024-08-23"))/years(1))

# hktc_data_list <- hktc_data %>%
#   group_by(position) %>%
#   reframe(position = list(tibble(name, historical_value, total_value_added, tva_adj, ktc_value, position, age))) %>%
#   deframe()

# Model Total Value Added for next season-------------------------------------------------------------------------

# I use a BART (Bayesian Additive Regression Tree) Model

# means and standard deviations that I used to scale the predictors
tva_scales <- hktc_data %>% compute_tva_scales()

# prep data
tva_data <- hktc_data %>% prep_data_tva(tva_scales)

# run model ~95 seconds
tic()
tva_fit <- fit_bart(tva_data$full_data)
# tva_fit <- fit_bart(tva_data$train_data)
toc()

# save(tva_fit, file = here("Modeling/tva_fit.RData"))
# load(here("Modeling/tva_fit.RData"))

# compute accuracy (RMSE)
# model_accuracy(tva_fit, tva_data$test_data)

# graph residuals
# graph_residuals(tva_fit, tva_data$test_data)

# save(tva_fit, file = here("Modeling/tva_fit.RData"))

tva_resid_fit <- model_residuals(tva_fit, tva_data$full_data)
# save(tva_resid_fit, file = here("Modeling/tva_resid_fit.RData"))
# load(here("Modeling/tva_resid_fit.RData"))

# tva_samples <- generate_samples(tva_fit, tva_data$train_data)

# compute_coverage(tva_fit, tva_data$test_data, confidence = .95)

# Model KTC Value for next season -----------------------------------------

# means and standard deviations that I used to scale the predictors
ktc_scales <- hktc_data %>% compute_ktc_scales()

# prep data
ktc_data <- hktc_data %>% prep_data_ktc(ktc_scales)

# run model ~98 seconds
tic()
# # ktc_fit <- fit_bart(ktc_data$train_data)
ktc_fit <- fit_bart(ktc_data$full_data)
toc()

# save(ktc_fit, file = here("Modeling/ktc_fit.RData"))
# load(here("Modeling/ktc_fit.RData"))

ktc_resid_fit <- model_residuals(ktc_fit, ktc_data$full_data)
# save(ktc_resid_fit, file = here("Modeling/ktc_resid_fit.RData"))
# load(here("Modeling/ktc_resid_fit.RData"))

# compute accuracy (RMSE)
# model_accuracy(ktc_fit, ktc_data$test_data)

# graph residuals
# graph_residuals(ktc_fit, ktc_data$test_data)

# ktc_samples <- generate_samples(ktc_fit, ktc_data$full_data)

# origin data set, set at beginning of last year
sim_df <- keep_trade_cut %>%
  left_join(player_info, by = join_by(name)) %>%
  mutate(
    age = interval(birth_date, ymd("2024-08-23"))/years(1),  # just because age needs to be age at this point in the season
    season = 2024) %>%
  select(name, position, ktc_value, birth_date, age, season) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"))

tic() # ~4 mins
player_simulations <- next_years(origin_data = sim_df, n_years = 15, tva_scales = tva_scales, ktc_scales = ktc_scales,
                                 tva_fit = tva_fit, ktc_fit = ktc_fit, tva_resid_fit = tva_resid_fit, ktc_resid_fit = ktc_resid_fit)
toc()

# save(player_simulations, file = here("Modeling/player_simulations.RData"))

future_value <- compute_future_value(player_simulations, years = 8, weight = .95)

player_total_value <- future_value %>%
  full_join(season_value_added, by = join_by(name)) %>%
  select(name, total_value_added, future_value) %>%
  left_join(player_info, by = join_by(name)) %>%
  left_join(keep_trade_cut, by = join_by(name)) %>%
  mutate(
    sva_2024 = replace_na(total_value_added, 0),
    ktc_value = case_when(
      position %in% c("K", "DST") ~ 0,
      .default = ktc_value),
    future_value = case_when(
      position %in% c("K", "DST") ~ 0,
      is.na(future_value) ~ 0,
      .default = future_value)) %>%
  select(name, player_id, birth_date, position, ktc_value, sva_2024, future_value)

# write_csv(player_total_value, here("Data/player_total_value.csv"))
