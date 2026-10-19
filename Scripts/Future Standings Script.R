# Future Standings Script
# odds tables for the Future Standings page from the simulated final standings
# (Modeling/Future Standings.R). Odds are numbers from 0 to 100 (percent), so the app
# sorts them correctly, and each table is sorted by the first simulated season.
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Future Standings Script...")

users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, display_name)

final_standings_odds <- read_csv(
  here("Data/model_output/final_standings_odds.csv"),
  show_col_types = FALSE
) |>
  left_join(users, by = join_by(roster_id)) |>
  select(-roster_id) |>
  rename(team = display_name)

# league size and playoff spots (from the latest cached Sleeper league settings)
num_teams <- n_distinct(final_standings_odds$team)
league_files <- list.files(
  here("Data/raw/sleeper"),
  pattern = "^league_\\d{4}\\.rds$",
  full.names = TRUE
)
league_settings <- if (length(league_files) > 0) {
  readRDS(max(league_files))$data$settings
} else {
  list()
}
playoff_teams <- as.numeric(league_settings$playoff_teams %||% 6)

# share of simulations meeting a condition, one column per season, as a percent
odds_table <- function(odds) {
  odds |>
    summarize(perc = 100 * sum(perc), .by = c(season, team)) |>
    arrange(season) |>
    pivot_wider(names_from = season, values_from = perc, values_fill = 0) |>
    arrange(desc(pick(2)[[1]])) # first season's odds
}

# championship odds
champion_odds <- final_standings_odds |>
  filter(type == "rank", result == 1) |>
  odds_table()

# playoff odds
playoff_odds <- final_standings_odds |>
  filter(type == "rank", result <= playoff_teams) |>
  odds_table()

# first round bye odds
bye_odds <- final_standings_odds |>
  filter(type == "bye", result == 1) |>
  odds_table()

# number one pick odds: last place in a season picks first in the next season's draft, so
# the columns are the draft seasons
n1_pick_odds <- final_standings_odds |>
  filter(type == "rank", result == num_teams) |>
  mutate(season = season + 1) |>
  odds_table()

# most common finish
most_common_finish_df <- final_standings_odds |>
  filter(type == "rank") |>
  slice_max(perc, n = 1, with_ties = FALSE, by = c(team, season)) |>
  arrange(result, desc(perc)) |>
  mutate(probability = 100 * perc) |>
  rename(finish = result) |>
  select(season, finish, perc, team, probability)

# dfs to save -------------------------------------------------------------

write_csv(
  most_common_finish_df,
  here("Shiny/Saved Files/most_common_finish_df.csv")
)
write_csv(champion_odds, here("Shiny/Saved Files/champion_odds.csv"))
write_csv(playoff_odds, here("Shiny/Saved Files/playoff_odds.csv"))
write_csv(bye_odds, here("Shiny/Saved Files/bye_odds.csv"))
write_csv(n1_pick_odds, here("Shiny/Saved Files/n1_pick_odds.csv"))

# final_standings_odds |>
#   filter(season == 2026, type == "rank") |>
#   mutate(
#     lottery = case_when(
#       result == 1 ~ 1,
#       result == 2 ~ 2,
#       result == 3 ~ 3,
#       result == 4 ~ 4,
#       result == 5 ~ 5,
#       result == 6 ~ 6,
#       result == 7 ~ 7,
#       result == 8 ~ 7,
#       result == 9 ~ 10,
#       result == 10 ~ 17,
#       result == 11 ~ 17,
#       result == 12 ~ 22,
#       .default = NA
#     ) *
#       0.01 *
#       perc
#   ) |>
#   group_by(display_name) |>
#   summarize(
#     lottery_odds = sum(lottery)
#   ) |>
#   arrange(desc(lottery_odds)) |>
#   mutate(
#     lottery_odds = lottery_odds/sum(lottery_odds)
#   )
