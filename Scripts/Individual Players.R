# Player Page
# this is essentially a group of functions that allow one to call any player
# and see their past tva and/or future value
library("here")
data_path <- "FantasyDynasty/"

# load data
source(here(data_path, "Scripts/Script Support.R"))

load(here(data_path, "Modeling/player_simulations.RData"))
season_value_added <- read_csv(here(data_path, "Data/sva.csv"), show_col_types = FALSE)
users <- read_csv(here(data_path, "Data/users.csv"), show_col_types = FALSE) %>%
  select(-owner_id)
player_info <- read_csv(here(data_path, "Data/player_info.csv"), show_col_types = FALSE)
player_total_value <- read_csv(here(data_path, "Data/player_total_value.csv"), show_col_types = FALSE) %>%
  select(name, player_id, birth_date, position, sva_2024, future_value) %>%
  mutate(total_value = sva_2024 + .95*future_value) # devalue future

# dfs to save -------------------------------------------------------------

basic_info_df <- player_info %>%
  left_join(player_total_value %>% select(-name, -position, -birth_date),
            by = join_by(player_id)) %>%
  mutate(
    age = time_length(lubridate::interval(birth_date, today()), "years"),
    across(sva_2024:total_value, ~replace_na(.x, 0))) %>%
  rename(value_added_2024 = sva_2024)

write_csv(basic_info_df, here(data_path, "Scripts/Saved Files/basic_info_df.csv"))

# points for each team table
# data, in future add more seasons here
plot_future_value_df <- imap_dfr(seq_along(player_simulations), ~{
  player_simulations[[.x]] %>%
    mutate(season = as.numeric(names(player_simulations)[.x])) %>%
    relocate(season)
})

write_csv(plot_future_value_df, here(data_path, "Scripts/Saved Files/plot_future_value_df.csv"))

value_added <- read_csv(here(data_path, "Data/va.csv"), show_col_types = FALSE) %>%
  left_join(users, by = join_by(roster_id)) %>%
  arrange(season, week, desc(type), display_name)

write_csv(value_added, here(data_path, "Scripts/Saved Files/value_added.csv"))

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

# value_added_24 %>% tabulate_realized_value("Caleb Williams", 2024, shiny = FALSE)

# value_added_24 %>% weekly_results("James Cook", 2024)
