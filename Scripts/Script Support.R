# Grades Support

# libraries
library("gt")
library("gtExtras")
library("tidymodels")
library("tidyverse"); theme_set(theme_minimal())
library("janitor")
library("tictoc")


# All Scripts --------------------------------------------------------------
# function to edit tables for shiny
shiny_edit_tables <- function(df){
  df %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    rename_with(~str_replace_all(.x, "_", " "), everything()) %>%
    rename_with(~str_to_title(.x), everything())
}

#position_levels
position_levels <- c("QB", "RB", "WR", "TE", "K", "DST")

# Individual Players ------------------------------------------------------

basic_info <- function(enter_name){
  basic_info_df %>%
    filter(name == enter_name) %>%
    select(-name) %>%
    shiny_edit_tables()
}

# plots players median future value over next fifteen years
plot_future_value <- function(enter_name){
  plot_future_value_df %>%
    filter(name == enter_name) %>%
    ggplot(aes(x = season)) +
    geom_line(aes(y = proj_tva_50), color = "indianred3") +
    geom_ribbon(aes(ymin = proj_tva_10, ymax = proj_tva_90), fill = "cadetblue4", alpha = .5) +
    geom_ribbon(aes(ymin = proj_tva_5, ymax = proj_tva_95), fill = "cadetblue1", alpha = .5) +
    labs(title = str_c(enter_name, " Projected Total Value Added"), x = "", y = "") +
    geom_point(data = season_value_added %>% filter(name == enter_name),
               aes(x = 2024, y = total_value_added))
}

tabulate_realized_value <- function(va_data, enter_name, enter_season, shiny = FALSE){
  df <- va_data %>%
    filter(name == enter_name) %>%
    filter(season == enter_season) %>%
    group_by(display_name) %>%
    summarize(
      start_week = min(week),
      weeks = n(),
      healthy = sum(sleeper_points > 0),
      starts = sum(type == "starter"),
      `total value added` = sum(value_added),
      `mean fantasy points` = sum(sleeper_points)/healthy) %>%
    arrange(start_week) %>%
    select(-start_week)
  
  if(shiny){
    df %>%
      rename("Team" = display_name) %>%
      shiny_edit_tables()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = c(`total value added`, `mean fantasy points`), decimals = 2) %>%
      cols_label(display_name = "Team") %>%
      tab_header(title = str_c(enter_name, ": ", enter_season, " Season"))
  }
}


# Fantasy Teams -----------------------------------------------------------
# grab team assets
grab_team_assets <- function(enter_roster_id, shiny = FALSE){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    pull(display_name)
  
  df <- grab_team_assets_df %>%
    filter(roster_id == enter_roster_id) %>%
    select(-roster_id)
  
  if(shiny){
    df %>%
      rename("Acquired" = "avenue") %>%
      shiny_edit_tables()
  }
  else{
    gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = future_value, decimals = 2) %>%
      cols_label(future_value = "Future Value", avenue = "Acquired") %>%
      tab_header(title = str_c(team_name, " Future Value"))
  }
}

# position outlook
position_outlook <- function(enter_roster_id, shiny = FALSE){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    pull(display_name)
  
  df <- position_outlook_df %>%
    filter(roster_id == enter_roster_id) %>%
    select(-roster_id)
  
  if(shiny){
    df %>%
      shiny_edit_tables()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = c(future_value, realized_value), decimals = 2) %>%
      cols_label(future_value = "Future Value", realized_value = "Realized Value") %>%
      tab_header(title = str_c(team_name, " Position Outlook"))
  }
}

# top contributors
grab_team_contributors <- function(enter_roster_id, enter_season, shiny = FALSE){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    pull(display_name)
  
  df <- value_added %>% 
    filter(roster_id == enter_roster_id, season == enter_season) %>%
    group_by(name, position) %>%
    summarize(
      weeks = n(),
      healthy = sum(sleeper_points > 0),
      starts = sum(type == "starter"),
      total_value_added = sum(value_added),
      fantasy_points = sum(sleeper_points),
      .groups = "keep") %>%
    arrange(desc(total_value_added)) %>%
    ungroup()
  
  if(shiny){
    df %>% shiny_edit_tables() %>% return()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = c(total_value_added, fantasy_points), decimals = 2) %>%
      cols_label(total_value_added = "Total Value Added", fantasy_points = "Fantasy Points") %>%
      tab_header(title = str_c(team_name, ": ", enter_season, " Season"))
  }
  
}

# week by week
grab_team_contributors_weekly <- function(enter_roster_id, enter_season, enter_week, shiny = FALSE){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    pull(display_name)
  
  df <- value_added %>% 
    filter(roster_id == enter_roster_id, season == enter_season, week == enter_week,
           type == "starter") %>%
    select(name, position, projection, sleeper_points, value_added) %>%
    mutate(position = factor(position, position_levels)) %>%
    arrange(position, desc(value_added))
  
  if(shiny){
    df %>%
      rename(fantasy_points = sleeper_points) %>%
      shiny_edit_tables()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538() %>%
      fmt_number(columns = c(value_added, sleeper_points, projection), decimals = 2) %>%
      cols_label(value_added = "Value Added", sleeper_points = "Fantasy Points") %>%
      tab_header(title = str_c(team_name, ": ", enter_season, " Season Week ", enter_week))
  }
}

overall_grades <- function(value_avenues, enter_roster_id, shiny = FALSE){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    pull(display_name)
  
  df <- value_avenues %>%
    filter(roster_id == enter_roster_id) %>%
    relocate(c(avenue, total_realized_value, total_future_value), value_over_expected) %>%
    select(-roster_id) %>%
    arrange(avenue) %>%
    janitor::adorn_totals()
  
  if(shiny){
    df %>%
      rename(
        realized_value = "total_realized_value",
        future_value = "total_future_value") %>%
      shiny_edit_tables() %>%
      return()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538() %>%
      fmt_number(columns = c(total_realized_value, total_future_value, value_over_expected), decimals = 2) %>%
      cols_label(total_realized_value = "Realized Value", total_future_value = "Future Value", value_over_expected = "Value Over Expected") %>%
      tab_header(title = str_c(team_name, " Avenue Grades"))
  }
}

# top acquisitions
top_acquisitions <- function(acquisitions, enter_roster_id, enter_avenue = "All", shiny = FALSE) {
  enter_avenue <- if_else(enter_avenue == "All", " ", enter_avenue)
  
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    pull(display_name)
  
  df <- acquisitions %>%
    filter(str_detect(avenue, enter_avenue), roster_id == enter_roster_id) %>%
    slice_max(value_over_expected, n = 5) %>%
    select(-roster_id, -total_value)
  
  if(shiny){
    df %>%
      shiny_edit_tables() %>%
      return()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = c(realized_value, future_value, value_over_expected), decimals = 2) %>%
      cols_label(realized_value = "Realized Value", future_value = "Future Value", value_over_expected = "Value Over Expected") %>%
      tab_header(title = str_c(team_name, " Top Acquisitions"))
  }
  
}


# worst acquisitions
worst_acquisitions <- function(acquisitions, enter_roster_id, enter_avenue = "All", shiny = FALSE) {
  enter_avenue <- if_else(enter_avenue == "All", " ", enter_avenue)
  
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    pull(display_name)
  
  df <- acquisitions %>%
    filter(str_detect(avenue, enter_avenue), roster_id == enter_roster_id) %>%
    slice_min(value_over_expected, n = 5) %>%
    select(-roster_id, -total_value)
  
  if(shiny){
    df %>%
      shiny_edit_tables() %>%
      return()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = c(realized_value, future_value, value_over_expected), decimals = 2) %>%
      cols_label(realized_value = "Realized Value", future_value = "Future Value", value_over_expected = "Value Over Expected") %>%
      tab_header(title = str_c(team_name, " Worst Acquisitions"))
  }
}

# team composition
team_composition <- function(player_avenues, enter_roster_id, shiny = FALSE){
  team_name <- users %>%
    filter(roster_id == enter_roster_id) %>%
    pull(display_name)
  
  df <- player_avenues %>%
    filter(roster_id == enter_roster_id) %>%
    mutate(
      avenue_type = case_when(
        str_detect(avenue, "initial draft") ~ "initial draft",
        str_detect(avenue, "rookie draft") ~ "rookie draft",
        str_detect(avenue, "transaction") ~ "transaction",
        str_detect(avenue, "trade") ~ "trade",
        str_detect(avenue, "own pick") ~ "own pick",
        .default = NA)) %>%
    group_by(avenue_type) %>%
    summarize(future_value = sum(future_value)) %>%
    mutate(proportion = scales::percent(future_value/sum(future_value), accuracy = .01))
  
  if(shiny){
    df %>%
      rename(avenue = avenue_type) %>%
      shiny_edit_tables()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = c(future_value), decimals = 2) %>%
      cols_label(avenue_type = "Avenue", future_value = "Future Value") %>%
      tab_header(title = str_c(team_name, " Team Composition"))
  }
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
  
  df <- picks_df %>%
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

# points by week compared with projected
weekly_results <- function(va_data, enter_name, enter_season){
  va_data %>%
    filter(name == enter_name) %>%
    filter(season == enter_season) %>%
    select(week, sleeper_points, projection, value_added) %>%
    pivot_longer(cols = c(sleeper_points, projection, value_added),
                 names_to = "type", values_to = "value") %>%
    ggplot() +
    geom_line(aes(x = week, y = value, color = type)) +
    geom_hline(yintercept = 0) +
    labs(x = "", y = "", title = str_c(enter_name, ": Weekly Sleeper Points")) +
    scale_color_manual(
      values = c("forestgreen", "cadetblue2", "indianred3"),
      labels = c("Projection", "Sleeper Points", "Value Added"),
      name = "")
}

# Trade Grades ------------------------------------------------------------

inspect_individual_trade <- function(trade_id, shiny = FALSE){
  title <- total_trade_value[[trade_id]] %>% select(team_name) %>% distinct() %>%
    map_chr(~str_c(.x, collapse = ", ")) %>%
    str_c(total_trade_value[[trade_id]]$season[1], " W",
          total_trade_value[[trade_id]]$week[1], " Trade between ", .)
  
  df <- total_trade_value[[trade_id]] %>%
    filter(type == "add" | name == "roster size adjustment") %>%
    select(-season, -week, -type) %>%
    group_by(team_name) %>%
    group_map(~ .x %>% adorn_totals("row"),
              .keep = TRUE) %>%
    bind_rows()
  
  if(shiny){
    df %>%
      shiny_edit_tables() %>%
      return()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = c(future_value, realized_value, total_value), decimals = 2) %>%
      cols_label(team_name = "Team", future_value = "Future Value", realized_value = "Realized Value",
                 total_value = "Total Value") %>%
      tab_header(title = title)
  }
  
}

# Transaction Grades ------------------------------------------------------

inspect_individual_transaction <- function(transaction_id, shiny = FALSE){
  title <- total_transaction_value[[transaction_id]]$team_name[1] %>%
    str_c(., "'s ", total_transaction_value[[transaction_id]]$season[1], " W",
          total_transaction_value[[transaction_id]]$week[1], " Transaction")
  
  df <- total_transaction_value[[transaction_id]] %>%
    mutate(action = if_else(type == "add", "+", "-")) %>%
    select(action, name, position, realized_value, future_value, total_value) %>%
    adorn_totals("row")
  
  if(shiny){
    df %>%
      shiny_edit_tables() %>%
      return()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE) %>%
      fmt_number(columns = c(future_value, realized_value, total_value), decimals = 2) %>%
      cols_label(future_value = "Future Value", realized_value = "Realized Value",
                 total_value = "Total Value") %>%
      tab_header(title = title)
  }
}
