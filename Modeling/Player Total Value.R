# Future Value and Total Value

library("here")
data_path <- "FantasyDynasty/"

# source(here(data_path, "Modeling/Player Value Added.R")) # grab updated value added ~50 seconds
source(here(data_path, "Data Manipulation/Scrape Support.R")) # grab functions
source(here(data_path, "Modeling/Player Total Value Functions.R")) # grab functions
season_value_added <- read_csv(here(data_path, "Data/sva.csv")) # shortcut
player_info <- read_csv(here(data_path, "Data/player_info.csv")) # shortcut 

keep_trade_cut <- read_csv(here(data_path, "Data/ktc values/ktc_value061725.csv")) # shortcut
sleeper_points <- read_csv(here(data_path, "Data/sleeper_points24.csv")) # shortcut

# organize data sets

# get ktc value from beginning of 2024 season
historical_ktc <- read_csv(here(data_path,"Data/ktc values/ktc_value082324.csv")) %>%
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
  rename("ktc_value" = value) %>%
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

# run model
tic()
tva_fit <- fit_bart(tva_data$train_data)
toc()

# compute accuracy (RMSE)
augment(tva_fit, tva_data$test_data)

# Model KTC Value for next season -----------------------------------------

# means and standard deviations that I used to scale the predictors
ktc_scales <- hktc_data %>% compute_ktc_scales()

# prep data
ktc_data <- hktc_data %>% prep_data_ktc(ktc_scales)

# run model
tic()
ktc_fit <- fit_bart(ktc_data$train_data)
toc()

# compute accuracy (RMSE)
augment(ktc_fit, ktc_data$test_data)

dbarts_model <- extract_fit_engine(ktc_fit)

# Generate posterior predictive samples
posterior_samples <- predict(dbarts_model, newdata = test_data)
dim(posterior_samples)

# HERE


# Posterior mean and 95% CI for first observation
posterior_1 <- posterior_samples[, 1]
mean(posterior_1)
quantile(posterior_1, probs = c(0.025, 0.975))






X_tva <- hktc_data %>% prep_data_tva(means, sds)
Y_tva <- hktc_data$tva_adj
group_tva <- hktc_data$position %>% factor() %>% as.numeric()



# tva model parameter values sample (can rerun or load from files)

# tva_parameter_values <- find_tva_parameters(hktc_data) # OR
# save(tva_parameter_values, file = here(data_path, "Data/tva_parameter_values.RData"))
load(here(data_path, "Data/tva_parameter_values.RData"))

# ktc model parameter values sample
# ktc_parameter_values <- find_ktc_parameters(hktc_data) #OR
# save(ktc_parameter_values, file = here(data_path, "Data/ktc_parameter_values.RData"))
load(here(data_path, "Data/ktc_parameter_values.RData"))

# this value is the group (position) for each player in the data set
# constant_group <- hktc_data %>%
#   transmute(position = as.factor(position) %>% as.numeric) %>%
#   pull()

# takes about 27 minutes for 10000 observations

# NEED TO FIX THESE

sim_df <- keep_trade_cut %>%
  rename(ktc_value = value) %>%
  left_join(player_info, by = join_by(name)) %>%
  mutate(
    age = interval(birth_date, ymd("2025-08-23"))/years(1)) %>% # just because age needs to be age at this point in the season
  select(name, position, ktc_value, birth_date, age) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"))

# simulations <- sim_df %>% simulate_future_value(15, 10000)
# save(simulations, file = here(data_path, "Data/simulations.RData"))
load(here(data_path, "Data/simulations.RData"))

# now, I need to convert this list of each simulation into a list of each player with all their simulations
player_simulations <- bind_rows(simulations, .id = "simulation_id") %>%
  group_by(name) %>%
  group_split()

# compute median future value, only keep next 8 years
median_values <- map_dfr(player_simulations, ~{
  # weights for devaluing future
  weights <- .95^(1:8)
  data <- .x %>%
    # select(contains("proj_tva")) %>%
    select(proj_tva_2025:proj_tva_2032)
  
  .x %>%
    # weighted future_value (each year is valued .95 of previous year)
    mutate(
      future_value = rowSums(data * weights)) %>%
  group_by(name) %>%
    summarize(
      median = median(future_value),
      sd = sd(future_value),
      mean = mean(future_value),
      q2.5 = quantile(future_value, .025),
      q97.5 = quantile(future_value, .975)) %>%
    mutate(
      ny = median(data[,1] %>% pull()),
      ny2 = median(data[,2] %>% pull()))
    }) %>%
  arrange(desc(median))

player_total_value <- median_values %>%
  full_join(season_value_added, by = join_by(name)) %>%
  rename(future_value = median) %>%
  select(name, total_value_added, future_value, contains("ny")) %>%
  left_join(player_info, by = join_by(name)) %>%
  left_join(keep_trade_cut, by = join_by(name)) %>%
  mutate(
    sva_2024 = replace_na(total_value_added, 0),
    ktc_value = case_when(
      position %in% c("K", "DST") ~ 0,
      .default = value),
    future_value = case_when(
      position %in% c("K", "DST") ~ 0,
      is.na(future_value) ~ 0,
      .default = future_value)) %>%
  select(name, player_id, birth_date, position, sva_2024, ktc_value, future_value, contains("ny"))

# write_csv(player_total_value, here(data_path, "Data/player_total_value.csv"))

