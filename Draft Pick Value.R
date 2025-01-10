# Estimate Rookie Draft Slot Value (from previous years)
# one downside of this, is we can't actually peer into the future and see
# how good the rookies are, but we can see how good previous rookies turned out to be

library("here")
library("tidyverse")
library("tidymodels")
data_path <- "FantasyDynasty/"

# load data of interest
load(here(data_path, "Data/draft_picks.RData"))
player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv")) %>%
  mutate(
    total_value = sva_2024 + future_value*.95, # devalue the future
    player_id = as.character(player_id)) %>%
  select(name, player_id, position, total_value, sva_2024, contains("ny"))
player_info <- read_csv(here(data_path, "Data/player_info.csv"))

# find value of all players in rookie drafts
rookie_drafts <- bind_rows(draft_picks, .id = "draft_id") %>%
  group_by(draft_id) %>%
  mutate(max_rounds = max(round)) %>%
  ungroup() %>%
  # only keep rookie drafts (for now)
  filter(max_rounds == 3) %>%
  select(pick_no, roster_id, player_id) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  select(pick_no, roster_id, name, player_id, position) %>%
  left_join(player_total_value %>% select(-player_id, -position), by = join_by(name)) %>%
  mutate(
    total_value = replace_na(total_value, 0),
    sva_2024 = replace_na(sva_2024, 0),
    ny = replace_na(ny, 0),
    ny2 = replace_na(ny2, 0))

rookie_drafts %>%
  ggplot() +
  geom_point(aes(x = pick_no, y = total_value))

# Polynomial Regression on total value ---------------------------------------------------
lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

rec_tv <- recipe(total_value ~ pick_no, data = rookie_drafts) %>%
  step_mutate(pick_no2 = sqrt(pick_no))

tv_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(rec_tv)

tv_fit <- fit(tv_wf, data = rookie_drafts)

# plot fit, looks pretty good
tv_fit %>% augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = total_value)) +
  geom_line(aes(y = .pred))

# plot residuals against pick_no, looks ok enough
tv_fit %>%
  augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)

# Polynomial Regression - Next Year Value Added ---------------------------

rec_va <- recipe(sva_2024 ~ pick_no, data = rookie_drafts) %>%
  step_mutate(pick_no2 = sqrt(pick_no))

va_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(rec_va)

va_fit <- fit(va_wf, data = rookie_drafts)

# plot fit, looks meh but whatever at this point
va_fit %>% augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = sva_2024)) +
  geom_line(aes(y = .pred))

# plot residuals against pick_no
va_fit %>%
  augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)

# probably would be better with a zero-inflated model

# Polynomial Regression - 2 Years Value Added -----------------------------
# this is kinda stretching it. Using draft pick to predict the bayesian hierarchical model's
# prediction for va in the following year.
# this is essentially a proxy for predicting output in 2 years with draft slot

# this is kinda confusing, from perspective of rookie_drafts, ny is two years after drafted
rec_ny2 <- recipe(ny ~ pick_no, data = rookie_drafts) %>%
  step_mutate(pick_no2 = sqrt(pick_no))

ny2_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(rec_ny2)

ny2_fit <- fit(ny2_wf, data = rookie_drafts)

# plot fit, looks meh but whatever at this point
ny2_fit %>% augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = ny)) +
  geom_line(aes(y = .pred))

# plot residuals against pick_no
ny2_fit %>%
  augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)


# Polynomial Regression - 3 Years Value Added -----------------------------

# this is kinda confusing, from perspective of rookie_drafts, ny2 is three years after drafted
rec_ny3 <- recipe(ny2 ~ pick_no, data = rookie_drafts) %>%
  step_mutate(pick_no2 = sqrt(pick_no))

ny3_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(rec_ny3)

ny3_fit <- fit(ny3_wf, data = rookie_drafts)

# plot fit, looks meh but whatever at this point
ny3_fit %>% augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = ny2)) +
  geom_line(aes(y = .pred))

# plot residuals against pick_no
ny3_fit %>%
  augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)

# Write to CSV ------------------------------------------------------------

rookie_draft_values <- augment(tv_fit, new_data = tibble(pick_no = 1:36)) %>%
  rename(exp_total_value = .pred) %>%
  left_join(augment(va_fit, new_data = tibble(pick_no = 1:36)), by = join_by(pick_no)) %>%
  rename(exp_value_added_ny = .pred) %>%
  left_join(augment(ny2_fit, new_data = tibble(pick_no = 1:36)), by = join_by(pick_no)) %>%
  rename(exp_value_added_ny2 = .pred) %>%
  left_join(augment(ny3_fit, new_data = tibble(pick_no = 1:36)), by = join_by(pick_no)) %>%
  rename(exp_value_added_ny3 = .pred) %>%
  relocate(pick_no)

# write_csv(rookie_draft_values, here(data_path, "Data/rookie_draft_values.csv"))




