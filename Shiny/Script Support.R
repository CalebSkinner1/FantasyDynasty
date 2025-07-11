# Grades Support

# libraries
library("gt")
library("gtExtras")
library("tidyverse"); theme_set(theme_minimal())
library("janitor")
library("tictoc")
library("plotly")

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
    geom_ribbon(aes(ymin = proj_tva_2.5, ymax = proj_tva_97.5), fill = "cadetblue1", alpha = .5) +
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
    mutate(
      position = factor(position, position_levels),
      projection = if_else(position %in% c("K", "DST"), 8, projection)) %>%
    arrange(position, desc(value_added)) %>%
    adorn_totals()
  
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
    arrange(desc(value_over_expected)) %>%
    # slice_max(value_over_expected, n = 5) %>%
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
    # slice_min(value_over_expected, n = 5) %>%
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
  
  df <- picks_df %>%
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

# Matchups ----------------------------------------------------------------
# function that gets matchup records

team_matchup_record <- function(team_records_df, enter_team, enter_round, enter_seasons, shiny){
  if("All" %in% enter_round){
    enter_round <- unique(team_records_df$round)}
  
  if(missing(enter_seasons)){
    enter_seasons <- team_records_df %>% pull(season) %>% unique()
  }
  
  df <- team_records_df %>%
    filter(str_detect(team, enter_team), round %in% enter_round, season %in% enter_seasons) %>%
    group_by(opponent) %>%
    summarize(
      games = n(),
      wins = sum(outcome == "win"),
      losses = sum(outcome == "loss"),
      `points for` = sum(points),
      `points against` = sum(opp_points)) %>%
    arrange(desc(wins), losses) %>%
    janitor::adorn_totals()
  
  if(shiny){
    df %>%
      shiny_edit_tables()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE)
  }
}

# Future Standings Script -------------------------------------------------

most_common_finish <- function(most_common_finish_df, enter_season, shiny = TRUE){
  df <- most_common_finish_df %>%
    filter(season == enter_season) %>%
    select(team, finish, probability)
  
  if(shiny){
    df %>% shiny_edit_tables()
  }else{
    df %>%
      gt() %>%
      gt_theme_538(quiet = TRUE)
  }

}

# Player Rankings -----------------------------------------------------------

top_future_value_player <- function(player_total_value, enter_position){
  if("All" %in% enter_position){
    enter_position <- unique(player_total_value$position)}
  
  player_total_value %>%
    filter(position %in% enter_position) %>%
    rowwise() %>%
    mutate(
      realized_value = sum(c_across(contains("sva_"))),
      age = time_length(interval(birth_date, today()), unit = "year")) %>%
    ungroup() %>%
    select(name, position, age, realized_value, future_value) %>%
    arrange(desc(future_value)) %>%
    shiny_edit_tables()
}

plot_over_time <- function(future_value_time, enter_names){
  p <- future_value_time %>%
    filter(name %in% enter_names) %>%
    ggplot() +
    geom_line(aes(x = date, y = future_value, color = name)) +
    labs(x = "Date", y = "Future Value") +
    theme(legend.position = "none")
  
  ggplotly(p)
}

comparable_players <- function(future_value_time, player_total_value, enter_name){
  player_position <- player_total_value %>% filter(name == enter_name) %>% pull(position)
  
  similar_players <- player_total_value %>%
    filter(position == player_position, !is.na(future_value)) %>%
    arrange(desc(future_value)) %>%
    mutate(index = row_number())
  
  n <- similar_players %>% nrow()
  
  player_row <- similar_players %>%
    filter(name == enter_name) %>%
    pull(index)
  
  player_row <- case_when(
    player_row < 3 ~ 3,
    player_row > n - 2 ~ n - 2,
    .default = player_row)
  
  names <- similar_players %>%
    slice((player_row - 2):(player_row + 2)) %>%
    pull(name)
  
  p <- future_value_time %>%
    filter(name %in% names) %>% # get range of players
    ggplot() +
    geom_line(aes(x = date, y = future_value, color = name)) +
    labs(x = "Date", y = "Future Value") +
    theme(legend.position = "none")
  
  ggplotly(p)
}

# Team Rankings -----------------------------------------------------------

graph_elo <- function(weekly_elo){
  p <- weekly_elo %>%
    ggplot() +
    geom_line(aes(x = date_hide, y = elo, color = team)) +
    scale_x_continuous(breaks = weekly_elo$date_hide, labels = weekly_elo$date) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "ELO", title = "Team ELO") +
    theme(legend.position = "none")
  
  ggplotly(p, tooltip = c("color", "y"))
}

# compute win probability of two teams
elo_win_probability <- function(team1_elo_df, team2_elo_df){
  elo_diff <- team1_elo_df$elo - team2_elo_df$elo
  
  perc <- (1 / (10^(-elo_diff/400) + 1))
  
  bind_rows(team1_elo_df, team2_elo_df) %>%
    ungroup() %>%
    mutate(win_probability = scales::percent(c(perc, 1 - perc), accuracy = .01)) %>%
    select(team, elo, win_probability) %>%
    shiny_edit_tables()
  
  # %>% scales::percent(accuracy = .01)
}

# History -----------------------------------------------------------------

# compute most wins
compute_most_wins <- function(wins_df, enter_round, enter_season){
  if("All" %in% enter_round){
    enter_round <- unique(wins_df$type)}
  
  wins_df %>% filter(type %in% enter_round, season %in% enter_season) %>%
    group_by(team) %>%
    summarize(wins = sum(result == "win"),
              losses = sum(result == "loss"),
              games = n()) %>%
    mutate(win_percentage = scales::percent(wins/games, accuracy = .01)) %>%
    select(team, wins, losses, win_percentage) %>%
    arrange(desc(wins)) %>%
    shiny_edit_tables()
}

# compute total points scored
compute_total_points <- function(wins_df, enter_round, enter_season){
  if("All" %in% enter_round){
    enter_round <- unique(wins_df$type)}
  
  wins_df %>% filter(type %in% enter_round, season %in% enter_season) %>%
    group_by(team) %>%
    summarize(points_for = sum(points),
              points_against = sum(opp_points),
              average_points = mean(points)) %>%
    select(team, points_for, average_points, points_against) %>%
    arrange(desc(average_points)) %>%
    shiny_edit_tables()
}

# compute most points scored in a game
highest_team_total <- function(wins_df, enter_round, enter_season){
  if("All" %in% enter_round){
    enter_round <- unique(wins_df$type)}
  
  wins_df %>% arrange(desc(points)) %>%
    filter(type %in% enter_round, season %in% enter_season) %>%
    mutate(date = str_c(season, " Week ", week)) %>%
    select(team, points, date, opponent) %>%
    slice_max(points, n = 100) %>%
    shiny_edit_tables()
}

# compute fantasy points scored in season
compute_total_points_player <- function(value_added, wins_df, enter_round, enter_season, enter_position){
  if("All" %in% enter_round){
    enter_round <- unique(wins_df$type)}
  
  if("All" %in% enter_position){
    enter_position <- unique(value_added$position)}
  
  value_added %>% left_join(wins_df %>% select(season, week, type, roster_id),
                            by = join_by(season, week, roster_id)) %>%
    filter(type.y %in% enter_round, season %in% enter_season, position %in% enter_position) %>%
    group_by(name) %>%
    summarize(total_fantasy_points = sum(sleeper_points)) %>%
    arrange(desc(total_fantasy_points)) %>%
    slice_max(total_fantasy_points, n = 100) %>% 
    shiny_edit_tables()
}

# compute total value added in season
compute_value_added_player <- function(value_added, wins_df, enter_round, enter_season, enter_position){
  if("All" %in% enter_round){
    enter_round <- unique(wins_df$type)}
  
  if("All" %in% enter_position){
    enter_position <- unique(value_added$position)}
  
  value_added %>% left_join(wins_df %>% select(season, week, type, roster_id),
                            by = join_by(season, week, roster_id)) %>%
    filter(type.y %in% enter_round, season %in% enter_season, position %in% enter_position) %>%
    group_by(name) %>%
    summarize(realized_value = sum(value_added)) %>%
    arrange(desc(realized_value)) %>%
    slice_max(realized_value, n = 100) %>% 
    shiny_edit_tables()
}

# highest fantasy total in game
# compute fantasy points scored in season
highest_player_total <- function(value_added, wins_df, enter_round, enter_season, enter_position){
  if("All" %in% enter_round){
    enter_round <- unique(wins_df$type)}
  
  if("All" %in% enter_position){
    enter_position <- unique(value_added$position)}
  
  value_added %>% left_join(wins_df %>% select(season, week, type, roster_id),
                            by = join_by(season, week, roster_id)) %>%
    filter(type.y %in% enter_round, season %in% enter_season, position %in% enter_position) %>%
    arrange(desc(sleeper_points)) %>%
    mutate(date = str_c(season, " Week ", week)) %>%
    rename(fantasy_points = sleeper_points, team = display_name) %>%
    select(name, fantasy_points, team, date) %>%
    slice_max(fantasy_points, n = 100) %>% 
    shiny_edit_tables()
}

