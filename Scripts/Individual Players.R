# Player Page
# this is essentially a group of functions that allow one to call any player
# and see their past tva and/or future value
library("here")

# load data
source(here("Shiny/Script Support.R"))

load(here("Modeling/player_simulations.RData"))
season_value_added <- read_csv(here("Data/sva.csv"), show_col_types = FALSE)
users <- read_csv(here("Data/users.csv"), show_col_types = FALSE) %>%
  select(-owner_id)
value_added <- read_csv(here("Data/va.csv"), show_col_types = FALSE) %>%
  left_join(users, by = join_by(roster_id)) %>%
  arrange(season, week, desc(type), display_name)
player_info <- read_csv(here("Data/player_info.csv"), show_col_types = FALSE)
player_total_value <- read_csv(here("Data/player_total_value.csv"), show_col_types = FALSE) %>%
  select(name, player_id, birth_date, position, contains("sva"), future_value) %>%
  rowwise() %>%
  mutate(total_value = sum(c_across(contains("sva"))) + .95*future_value) %>%# devalue future
  ungroup()
player_headshot <- read_csv(here("Shiny/Saved Files/player_headshot.csv"))
  
# dfs to save -------------------------------------------------------------

basic_info_df <- player_info %>%
  left_join(player_total_value %>% select(-name, -position, -birth_date),
            by = join_by(player_id)) %>%
  mutate(
    age = time_length(lubridate::interval(birth_date, today()), "years"),
    across(sva_2024:total_value, ~replace_na(.x, 0))) %>%
  rename_with(~str_replace(.x, "sva_", "total value added "), starts_with("sva")) %>%
  select(-player_id) %>%
  relocate(age, .after = birth_date)

write_csv(basic_info_df, here("Shiny/Saved Files/basic_info_df.csv"))

# want points for each team table?

# data, in future add more seasons here
sn <- max(value_added$season)

games <- value_added %>% filter(season == sn) %>% slice_max(week) %>% slice(1) %>% pull(week)

plot_future_value_df <- imap_dfr(seq_along(player_simulations), ~{
  df <- player_simulations[[.x]] %>%
    mutate(season = as.numeric(names(player_simulations)[.x]))
  
  if(.x == 1){
    df <- df %>%
      left_join(
        season_value_added %>% select(name, season, total_value_added), by = join_by(name, season)) %>%
      mutate(
        across(contains("proj_tva"), ~.*(17-games)/17 + total_value_added)) %>%
      select(-total_value_added)
  }else{
    names <- colnames(df %>% select(contains("proj_tva")))
    
    df[names] <- t(apply(df[names], 1, sort)) #sort to ensure credible intervals aren't inverted
  }
  
  df
})


write_csv(plot_future_value_df, here("Shiny/Saved Files/plot_future_value_df.csv"))

write_csv(value_added, here("Shiny/Saved Files/value_added.csv"))

# Examples ----------------------------------------------------------------
# plot_future_value("Kaleb Johnson")
# plot_future_value("Josh Allen")
# plot_future_value("Amon-Ra St. Brown")
# plot_future_value("Caleb Williams")
# plot_future_value("Trey Benson")
# plot_future_value("Bijan Robinson")
# plot_future_value("Malik Nabers")
# plot_future_value("Lamar Jackson")
# plot_future_value("Jalen Hurts")
# plot_future_value("Justin Fields")
# plot_future_value("Matthew Stafford")
# plot_future_value("Aaron Rodgers")
# plot_future_value("Derrick Henry")

# value_added %>% tabulate_realized_value("Caleb Williams", 2024, shiny = FALSE)

# value_added %>% weekly_results("James Cook", 2024)
