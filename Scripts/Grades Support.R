# Grades Support

# libraries
library("gt")
library("gtExtras")
library("tidymodels")
library("tidyverse"); theme_set(theme_minimal())


# samplers
source(here(data_path, "Modeling/MCMC Samplers.R"))
source(here(data_path, "Modeling/Player Total Value Functions.R"))


# All Grades --------------------------------------------------------------
# function to edit tables for shiny
shiny_edit_tables <- function(df){
  df %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    rename_with(~str_replace_all(.x, "_", " "), everything()) %>%
    rename_with(~str_to_title(.x), everything())
}



# Draft Grades ------------------------------------------------------------

# best picks- lol we vastly undervalued rookie picks
best_picks <- function(enter_draft, shiny){
  enter_draft <- if_else(enter_draft == "All", " ", enter_draft)
  
  df <- best_picks_df %>%
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

# worst picks - some massive first round busts
worst_picks <- function(enter_draft, shiny){
  enter_draft <- if_else(enter_draft == "All", " ", enter_draft)
  
  df <- picks_df %>%
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
