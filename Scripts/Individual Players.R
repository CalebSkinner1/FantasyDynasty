# Player Page
# this is essentially a group of functions that allow one to call any player
# and see their past tva or future value
library("here")
library("gt")
library("gtExtras")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

load(here(data_path, "Data/simulations.RData"))
season_value_added <- read_csv(here(data_path, "Data/sva_2024.csv"), show_col_types = FALSE)
users <- read_csv(here(data_path, "Data/users.csv"), show_col_types = FALSE) %>%
  select(-owner_id)
player_info <- read_csv(here(data_path, "Data/player_info.csv"), show_col_types = FALSE)
player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"), show_col_types = FALSE) %>%
  select(name, player_id, birth_date, position, sva_2024, future_value) %>%
  mutate(total_value = sva_2024 + .95*future_value) # devalue future

# function to edit tables for shiny
shiny_edit_tables <- function(df){
  df %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    rename_with(~str_replace_all(.x, "_", " "), everything()) %>%
    rename_with(~str_to_title(.x), everything())
}

basic_info <- function(enter_name){
  player_info %>%
    left_join(player_total_value %>% select(-name, -position, -birth_date),
              by = join_by(player_id)) %>%
    filter(name == enter_name) %>%
    mutate(age = interval(birth_date, today())%/% years(1)) %>%
    select(position, age, sva_2024, future_value, total_value) %>%
    rename(value_added_2024 = sva_2024) %>%
    shiny_edit_tables()
}

# plots players median future value over next fifteen years
plot_future_value <- function(enter_name){
  bind_rows(simulations, .id = "simulation_id") %>%
    filter(name == enter_name) %>%
    select(contains("tva")) %>%
    rename_with(~str_remove(.x, "proj_tva_"), starts_with("proj_tva")) %>%
    # pivot_longer(
    #   cols = everything(),
    #   names_to = "year",
    #   values_to = "tva") %>%
    group_by() %>%
    summarize(
      across(everything(), list(
        mean = ~ mean(.x),
        median = ~ median(.x),
        p2.5 = ~quantile(.x, .025),
        p97.5 = ~quantile(.x, .975),
        p10 = ~quantile(.x, .1),
        p90 = ~quantile(.x, .9)))) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("year", "stat"),
      names_sep = "_",
      values_to = "tva") %>%
    pivot_wider(names_from = stat, values_from = tva) %>%
    mutate(year = as.numeric(year)) %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = mean), color = "indianred3") +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = "cadetblue4", alpha = .5) +
    geom_ribbon(aes(ymin = p2.5, ymax = p97.5), fill = "cadetblue1", alpha = .5) +
    labs(title = str_c(enter_name, " Projected Total Value Added"), x = "", y = "") +
    geom_point(data = season_value_added %>% filter(name == enter_name),
               aes(x = 2024, y = total_value_added))
}

# examples:

# plot_future_value("Josh Allen")
# plot_future_value("Amon-Ra St. Brown")
# plot_future_value("Caleb Williams")
# plot_future_value("Trey Benson")
# plot_future_value("Bijan Robinson")
# plot_future_value("Malik Nabers")
# plot_future_value("Lamar Jackson")
# plot_future_value("Josh Allen")
# plot_future_value("Jalen Hurts")
# plot_future_value("Justin Fields")
# plot_future_value("Matthew Stafford")
# plot_future_value("Aaron Rodgers")
# plot_future_value("Derrick Henry")

# points for each team table
# data, in future add more seasons here
value_added_24 <- read_csv(here(data_path, "Data/va_2024.csv"), show_col_types = FALSE) %>%
  mutate(season = 2024) %>%
  left_join(users, by = join_by(roster_id)) %>%
  select(-roster_id) %>%
  arrange(season, week, desc(type), display_name)

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
      shiny_edit_tables() %>%
      rename("Team" = display_name) %>%
      return()
  }
  else{
    df %>%
      gt() %>%
      gt_theme_538() %>%
      fmt_number(columns = c(`total value added`, `mean fantasy points`), decimals = 2) %>%
      cols_label(display_name = "Team") %>%
      tab_header(title = str_c(enter_name, ": ", enter_season, " Season"))
  }
}

# value_added_24 %>% tabulate_realized_value("Caleb Williams", 2024, shiny = TRUE)

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

# value_added_24 %>% weekly_results("James Cook", 2024)
