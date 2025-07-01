# Draft Grades!

# Ok now this is fun
library("here")
library("gt")
library("gtExtras")
library("tidymodels")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

source(here(data_path, "Modeling/MCMC Samplers.R"))
source(here(data_path, "Modeling/Player Total Value Functions.R"))

# load data
load(here(data_path, "Data/draft_picks.RData"))

users <- read_csv(here(data_path, "Data/users.csv"), show_col_types = FALSE) %>%
  select(-owner_id)

player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"), show_col_types = FALSE) %>%
  select(name, player_id, birth_date, position, sva_2024, future_value) %>%
  mutate(total_value = sva_2024 + .95*future_value) # devalue future
player_info <- read_csv(here(data_path, "Data/player_info.csv"), show_col_types = FALSE)

# because drafting a kicker gave rookie draft order, I need to account for the rookie draft order
# when grading the initial draft
# rookie expected values per pick
rookie_draft_pick_values <- read_csv(here(data_path, "Data/rookie_draft_values.csv"), show_col_types = FALSE) %>%
  filter(metric == "total value") %>%
  select(pick_no, proj_tva_50)
  
# function to edit tables for shiny
shiny_edit_tables <- function(df){
  df %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    rename_with(~str_replace_all(.x, "_", " "), everything()) %>%
    rename_with(~str_to_title(.x), everything())
}

rookie_draft_values <- rookie_draft_pick_values %>%
  mutate(
    draft_slot = pick_no%%12,
    draft_slot = if_else(draft_slot == 0, 12, draft_slot)) %>%
  group_by(draft_slot) %>%
  summarize(total_value = sum(proj_tva_50))

# this tibble holds the expected value gained from rookie draft order
rookie_draft_order_value <- read_csv(here(data_path, "Data/draft_order.csv"), show_col_types = FALSE) %>%
  filter(season == 2024, type == "rookie") %>%
  # now compute expected value gained from draft order only
  left_join(rookie_draft_values, by = join_by(draft_order == draft_slot)) %>%
  mutate(
    name = "rookie draft slot",
    position = as.character(draft_order)) %>%
  select(roster_id, name, position, total_value)

# realized value from each draft pick
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

X_id <- initial_draft_data %>% select(pick_no) %>%
  mutate(
    # pn_1 = pick_no^(.25),
    pn_2 = pick_no^(.5),
    # pn_3 = pick_no^(.75),
    across(everything(), ~scale(.x))) %>%
  mutate(intercept = 1) %>%
  as.matrix()

Y_id <- initial_draft_data$total_value

#note that X_tv[c(1:36)] can actually stay the same
samples_id <- reg_gibbs_sampler(Y = Y_id, X = X_id,
                                iter = 7000, thin = 1, burn_in = 5000)

# quantiles of each pick
quantiles_id <- compute_quantiles(samples_id$new_y)

# plot fit, looks pretty good
# tibble(.pred = quantiles_id$`10`,
#        pick_no = seq_along(Y_id)) %>%
#   right_join(initial_draft_data, by = join_by(pick_no)) %>%
#   ggplot(aes(x = pick_no)) +
#   geom_point(aes(y = total_value)) +
#   geom_line(aes(y = .pred))

# plot residuals against pick_no, certainly heteroscedasticity
# tibble(.pred = quantiles_id$`10`,
#        pick_no = seq_along(Y_id)) %>%
#   right_join(initial_draft_data, by = join_by(pick_no)) %>%
#   mutate(.resid = total_value  - .pred) %>%
#   ggplot(aes(x = pick_no)) +
#   geom_point(aes(y = .resid)) +
#   geom_hline(yintercept = 0)

initial_draft_expectations <- tibble(.pred = quantiles_id$`10`,
                                     pick_no = seq_along(Y_id)) %>%
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
      mutate(value_over_expected = total_value - proj_tva_50)})

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

# best picks- lol we vastly undervalued rookie picks
best_picks <- function(enter_draft, shiny){
  enter_draft <- if_else(enter_draft == "All", " ", enter_draft)
  
  df <- init_vs_expectation %>%
    bind_rows(.,rookie_vs_expecations, .id = "draft_id") %>%
    mutate(
      draft = case_when(
        draft_id == "1" ~ "initial draft",
        .default = str_c(as.numeric(draft_id) + 2022, " rookie draft"))) %>%
    filter(str_detect(draft, enter_draft)) %>%
    left_join(users, by = join_by(roster_id)) %>%
    select(display_name, pick_no, name, position, realized_value, future_value, value_over_expected) %>%
    arrange(desc(value_over_expected))
  
  if(shiny){
    df %>%
      rename(
        team_name = display_name,
        pick = pick_no) %>%
      shiny_edit_tables()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538() %>%
      fmt_number(columns = c(value_over_expected, realized_value, future_value), decimals = 2) %>%
      cols_label(display_name = "Team Name", value_over_expected = "Value Over Expected",
                 realized_value = "Realized Value", future_value = "Future Value", pick_no = "Pick") %>%
      tab_header(title = str_c(str_to_title(enter_draft), " Best Picks"))
  }
}

# best_picks("initial draft", shiny = TRUE)
# best_picks("2024 rookie draft", shiny = TRUE)
# best_picks("2025 rookie draft", shiny = TRUE)

# worst picks - some massive first round busts
worst_picks <- function(enter_draft, shiny){
  enter_draft <- if_else(enter_draft == "All", " ", enter_draft)
  
  df <- init_vs_expectation %>%
    bind_rows(.,rookie_vs_expecations, .id = "draft_id") %>%
    mutate(
      draft = case_when(
        draft_id == "1" ~ "initial draft",
        .default = str_c(as.numeric(draft_id) + 2022, " rookie draft"))) %>%
    filter(str_detect(draft, enter_draft)) %>%
    left_join(users, by = join_by(roster_id)) %>%
    select(display_name, pick_no, name, position, realized_value, future_value, value_over_expected) %>%
    arrange(value_over_expected)
  
  if(shiny){
    df %>%
      rename(
        team_name = display_name,
        pick = pick_no) %>%
      shiny_edit_tables()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538() %>%
      fmt_number(columns = c(value_over_expected, realized_value, future_value), decimals = 2) %>%
      cols_label(display_name = "Team Name", value_over_expected = "Value Over Expected",
                 realized_value = "Realized Value", future_value = "Future Value", pick_no = "Pick") %>%
      tab_header(title = str_c(str_to_title(enter_draft), " Worst Picks"))
  }
}

# worst_picks("initial draft", shiny = TRUE)
# worst_picks("2024 rookie draft", shiny = TRUE)
# worst_picks("2025 rookie draft", shiny = TRUE)

draft_rankings <- function(enter_draft, shiny){
  enter_draft <- if_else(enter_draft == "All", " ", enter_draft)
  
  df <- init_vs_expectation %>%
    bind_rows(.,rookie_vs_expecations, .id = "draft_id") %>%
    mutate(
      draft = case_when(
        draft_id == "1" ~ "initial draft",
        .default = str_c(as.numeric(draft_id) + 2022, " rookie draft"))) %>%
    filter(str_detect(draft, enter_draft)) %>%
    left_join(users, by = join_by(roster_id)) %>%
    group_by(display_name) %>%
    summarize(
      realized_value = sum(realized_value, na.rm = TRUE),
      future_value = sum(future_value, na.rm = TRUE),
      value_over_expected = sum(value_over_expected, na.rm = TRUE)) %>%
    arrange(desc(value_over_expected))
  
  if(shiny){
    df %>%
      rename(team_name = display_name) %>%
      shiny_edit_tables()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538() %>%
      fmt_number(columns = c(value_over_expected, realized_value, future_value), decimals = 2) %>%
      cols_label(display_name = "Team Name", value_over_expected = "Value Over Expected",
                 realized_value = "Realized Value", future_value = "Future Value") %>%
      tab_header(title = str_c(str_to_title(enter_draft), " Draft Grades"))
  }
}

# draft_rankings("initial draft", shiny = TRUE)
# draft_rankings("2024 rookie draft", shiny = TRUE)
# draft_rankings("2025 rookie draft", shiny = TRUE)
