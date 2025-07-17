# Future Value and Total Value

library("here")

# source(here("Modeling/Player Value Added.R")) # grab updated value added ~50 seconds
source(here("Data Manipulation/Scrape Support.R")) # grab functions
source(here("Modeling/Player Total Value Functions.R")) # grab functions
season_value_added <- read_csv(here("Data/sva.csv")) # shortcut
player_info <- read_csv(here("Data/player_info.csv")) # shortcut 

keep_trade_cut <- read_csv(here("Data/ktc values/ktc_value071025.csv")) # shortcut
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
    # adjust total value added to account for missing games
    # tva_adj = case_when(
    #   total_value_added < 0 ~ total_value_added,
    #   .default = total_value_added * 17/max(sleeper_points$week)),
    ) %>% rename(tva_adj = total_value_added) %>%
  left_join(keep_trade_cut, by = join_by(name)) %>%
  select(-position) %>%
  left_join(player_info, by = join_by(name)) %>%
  select(-player_id, -years_exp) %>%
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

season_dates <- read_csv(here("Data/season_dates.csv"))

# origin data set, set at beginning of last year
sim_df <- compile_data_set(keep_trade_cut, future_value_names, today(), season_dates$season_start[2], season_dates$season_end[2])

tic() # ~7 mins
player_simulations <- next_years(origin_data = sim_df, n_years = 15, tva_scales = tva_scales, ktc_scales = ktc_scales,
                                 tva_fit = tva_fit, ktc_fit = ktc_fit, tva_resid_fit = tva_resid_fit, ktc_resid_fit = ktc_resid_fit)
toc()

save(player_simulations, file = here("Modeling/player_simulations.RData"))

# Future Value over Time --------------------------------------------------

ktc_list <- list.files(
  path = here("Data/ktc values"),
  full.names = T) %>%
  set_names(basename(.)) %>%
  map(~read_csv(.x, show_col_types = FALSE))

# these are the names of the dudes that I'll compute the future value of repetitively
future_value_names <- map_dfr(ktc_list, name_correction) %>% distinct(name) %>%
  left_join(player_info, by = join_by(name)) %>%
  select(-player_id) %>%
  filter(!str_detect(name, c("Mid")), !str_detect(name, c("Early")), !str_detect(name, c("Late")))

write_csv(future_value_names, here("Data/future_value_names.csv"))

# compute future value over time
last_date_fvt <- read_csv(here("Data/last_date_fvt.csv")) %>% pull(value)

reduced_ktc_list <- select_ktc_list(ktc_list, last_date_fvt)

future_value_time <- read_csv(here("Shiny/Saved Files/future_value_time.csv"))

# can't figure out how to parallelize this. Takes ~ 4 minutes for one run
tic()
future_value_time <- map_future_value_time(future_value_names, reduced_ktc_list, tva_scales, ktc_scales,
                                           tva_fit, ktc_fit, tva_resid_fit, ktc_resid_fit, season_start, season_end) %>%
  bind_rows(future_value_time)
toc()

write_csv(future_value_time, here("Shiny/Saved Files/future_value_time.csv"))
# make list of the dates already computed, so I don't have to compute them again
last_date_fvt <- max(future_value_time$date) %>% as_tibble() %>% write_csv(here("Data/last_date_fvt.csv"))

# ensure future value is the same as most recent future_value_over_time

player_total_value <- future_value_time %>% filter(date == max(date)) %>%
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

write_csv(player_total_value, here("Data/player_total_value.csv"))


