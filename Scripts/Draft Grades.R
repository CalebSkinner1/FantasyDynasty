# Draft Grades!

# Ok now this is fun
library("here")
library("gt")
library("gtExtras")
library("tidymodels")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

# load data
load(here(data_path, "Data/draft_picks.RData"))

users <- read_csv(here(data_path, "Data/users.csv")) %>%
  select(-owner_id)

player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv")) %>%
  select(name, player_id, birth_date, position, sva_2024, future_value) %>%
  mutate(total_value = sva_2024 + .95*future_value) # devalue future
player_info <- read_csv(here(data_path, "Data/player_info.csv"))

# because drafting a kicker gave rookie draft order, I need to account for the rookie draft order
# when grading the initial draft
# rookie expected values per pick
rookie_draft_pick_values <- read_csv(here(data_path, "Data/rookie_draft_values.csv")) %>%
  select(pick_no, exp_total_value)
  
rookie_draft_values <- rookie_draft_pick_values %>%
  mutate(
    draft_slot = pick_no%%12,
    draft_slot = if_else(draft_slot == 0, 12, draft_slot)) %>%
  group_by(draft_slot) %>%
  summarize(total_value = sum(exp_total_value))

# this tibble holds the expected value gained from rookie draft order
rookie_draft_order_value <- read_csv(here(data_path, "Data/draft_order.csv")) %>%
  filter(season == 2024, type == "rookie") %>%
  # now compute expected value gained from draft order only
  left_join(rookie_draft_values, by = join_by(draft_order == draft_slot)) %>%
  mutate(
    name = "rookie draft slot",
    position = as.character(draft_order)) %>%
  select(roster_id, name, position, total_value)

draft_values <- map(draft_picks, ~{
  draft_values <- .x %>%
    select(player_id, roster_id, draft_slot) %>%
    left_join(player_info, by = join_by(player_id)) %>%
    select(player_id, roster_id, draft_slot, name, position) %>% 
    left_join(player_total_value, by = join_by(player_id, name, position)) %>%
    rename(realized_value = sva_2024) %>%
    mutate(
      pick_no = row_number(),
      across(contains("_value"), ~replace_na(., 0))) %>%
    select(roster_id, pick_no, name, position, realized_value, future_value, total_value)
  
  if(nrow(draft_values) > 50){
    kicker_selected <- draft_values %>% filter(position == "K") %>%
      group_by(roster_id) %>%
      summarize(pick_no = min(pick_no))
    
    draft_values <- draft_values %>%
      # draft day trade
      mutate(
        roster_id = case_when(
          name == "James Cook" ~ 4,
          name == "Jaylen Warren" ~ 4,
          name == "Dalton Kincaid" ~ 7,
          name == "Noah Fant" ~ 7,
          .default = roster_id))
    
    draft_values <- rookie_draft_order_value %>%
      left_join(kicker_selected, by = join_by(roster_id)) %>%
      bind_rows(draft_values, .)
  }
  draft_values
})

# Expected Value per pick slot -------------------------------------------------
initial_draft_data <- draft_values[[2]] %>%
  mutate(
    total_value = case_when(
      name == "rookie draft slot" ~ total_value - min(rookie_draft_values$total_value),
      .default = total_value)) %>%
  group_by(pick_no) %>%
  summarize(total_value = sum(total_value))

initial_draft_data %>%
  ggplot() +
  geom_point(aes(x = pick_no, y = total_value))

lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

rec_init <- recipe(total_value ~ pick_no, data = initial_draft_data) %>%
  step_mutate(pick_no2 = sqrt(pick_no))

init_wf <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(rec_init)

init_fit <- fit(init_wf, data = initial_draft_data)

# plot fit, looks pretty good
init_fit %>% augment(initial_draft_data) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = total_value)) +
  geom_line(aes(y = .pred))

# plot residuals against pick_no, looks ok enough, may want to make non homoscedastic but whatevs
init_fit %>%
  augment(initial_draft_data) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)

initial_draft_expectations <- init_fit %>% augment(initial_draft_data) %>%
  select(pick_no, .pred) %>%
  rename(exp_total_value = .pred)

# Draft Rankings ----------------------------------------------------------

# best picks - lol we vastly undervalued rookie picks
init_vs_expectation <- draft_values[[2]] %>%
  group_by(pick_no) %>%
  summarize(
    total_value = sum(total_value)) %>%
  left_join(
    draft_values[[2]] %>%
      group_by(pick_no) %>%
      mutate(max_value = rank(desc(total_value))) %>%
      filter(max_value == 1) %>%
      select(-max_value, -total_value),
    by = join_by(pick_no)) %>%
  mutate(
    total_value = case_when(
      name == "rookie draft slot" ~ total_value - min(rookie_draft_values$total_value),
      .default = total_value)) %>%
  left_join(initial_draft_expectations, by = join_by(pick_no)) %>%
  mutate(value_over_expected = total_value - exp_total_value) %>%
  select(roster_id, pick_no, name, position, realized_value, future_value, total_value, value_over_expected)

# best picks- lol we vastly undervalued rookie picks
init_vs_expectation %>% 
  arrange(desc(value_over_expected)) %>%
  slice(1:30) %>%
  left_join(users, by = join_by(roster_id)) %>%
  select(-roster_id) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_number(columns = c(realized_value, future_value, value_over_expected, total_value), decimals = 2) %>%
  cols_label(display_name = "Team Name", pick_no = "Pick", total_value = "total value",
             value_over_expected = "Value Over Expected") %>%
  tab_header(title = "Best Picks")

# worst picks - some massive first round busts
init_vs_expectation %>% 
  arrange(value_over_expected) %>%
  slice(1:30) %>%
  left_join(users, by = join_by(roster_id)) %>%
  select(-roster_id) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_number(columns = c(value_over_expected, total_value), decimals = 2) %>%
  cols_label(display_name = "Team Name", pick_no = "Pick", total_value = "total value",
             value_over_expected = "Value Over Expected") %>%
  tab_header(title = "Worst Picks")

# initial draft rankings
initial_draft_value <- draft_values[[2]] %>%
  group_by(roster_id) %>%
  summarize(
    total_draft_value = sum(total_value),
    total_realized_value = sum(realized_value, na.rm = TRUE),
    total_future_value = sum(future_value, na.rm = TRUE)) %>%
  left_join(users, by = join_by(roster_id)) %>%
  arrange(desc(total_draft_value))

# rookie draft individual picks vs expectation
rookie_vs_expecations <- draft_values[-2] %>%
  map(~{
    .x %>%
      left_join(rookie_draft_pick_values, by = join_by(pick_no)) %>%
      mutate(value_over_expected = total_value - exp_total_value)})

# rookie draft rankings
rookie_draft_value <- rookie_vs_expecations %>%
  map(~{
    .x %>%
      group_by(roster_id) %>%
      summarize(
        value_over_expected = sum(value_over_expected),
        total_draft_value = sum(total_value),
        total_realized_value = sum(realized_value),
        total_future_value = sum(future_value)) %>%
      left_join(users, by = join_by(roster_id)) %>%
      arrange(desc(value_over_expected))
  })

# total rookie draft rankings
bind_rows(rookie_draft_value) %>%
  group_by(display_name) %>%
  summarize(value_over_expected = sum(value_over_expected)) %>%
  arrange(desc(value_over_expected)) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_number(columns = value_over_expected, decimals = 2) %>%
  cols_label(display_name = "Team Name", value_over_expected = "Value Over Expected") %>%
  tab_header(title = "Draft Grades")
