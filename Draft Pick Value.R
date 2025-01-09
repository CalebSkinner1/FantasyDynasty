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
    total_value = sva_2024 + future_value,
    player_id = as.character(player_id)) %>%
  select(name, player_id, position, total_value)

# find value of all players in rookie drafts
rookie_drafts <- bind_rows(draft_picks, .id = "draft_id") %>%
  group_by(draft_id) %>%
  mutate(max_rounds = max(round)) %>%
  ungroup() %>%
  # only keep rookie drafts (for now)
  filter(max_rounds == 3) %>%
  select(pick_no, roster_id, player_id) %>%
  left_join(player_total_value, by = join_by(player_id))

rookie_drafts %>%
  ggplot() +
  geom_point(aes(x = pick_no, y = total_value))


# Polynomial Regression ---------------------------------------------------
lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

rec_poly <- recipe(total_value ~ pick_no, data = rookie_drafts) %>%
  step_mutate(pick_no2 = sqrt(pick_no))

poly_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(rec_poly)

poly_fit <- fit(poly_wf, data = rookie_drafts)

# plot fit, looks pretty good
poly_fit %>% augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = total_value)) +
  geom_line(aes(y = .pred))

# plot residuals against pick_no
poly_fit %>%
  augment(rookie_drafts) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)

rookie_draft_values <- augment(poly_fit, new_data = tibble(pick_no = 1:36)) %>%
  relocate(pick_no)

write_csv(rookie_draft_values, here(data_path, "Data/rookie_draft_values.csv"))






