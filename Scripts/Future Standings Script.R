# Future Standings Script
suppressPackageStartupMessages(library("here"))

message("begin computing Future Standings Script...")

source(here("Shiny/Script Support.R"))

users <- read_csv(here("Data/users.csv"), show_col_types = FALSE) |>
  select(-owner_id)

final_standings_odds <- read_csv(
  here("Data/final_standings_odds.csv"),
  show_col_types = FALSE
) |>
  left_join(users, by = join_by(roster_id)) |>
  select(-roster_id)

# championship odds
champion_odds <- final_standings_odds |>
  filter(type == "rank", result == 1) |>
  pivot_wider(names_from = season, values_from = perc) |>
  select(-result, -type) |>
  arrange(desc(`2026`)) |>
  mutate(across(contains("20"), ~ scales::percent(.x))) |>
  rename(team = display_name)

# playoff odds
playoff_odds <- final_standings_odds |>
  filter(type == "rank", result < 7) |>
  group_by(season, display_name) |>
  summarize(playoff_perc = sum(perc), .groups = "keep") |>
  pivot_wider(names_from = season, values_from = playoff_perc) |>
  arrange(desc(`2026`)) |>
  mutate(across(contains("20"), ~ scales::percent(.x, accuracy = .01))) |>
  rename(team = display_name)

# first round bye odds
bye_odds <- final_standings_odds |>
  filter(type == "bye", result == 1) |>
  group_by(season, display_name) |>
  summarize(bye_perc = sum(perc), .groups = "keep") |>
  pivot_wider(names_from = season, values_from = bye_perc) |>
  arrange(desc(`2026`)) |>
  mutate(across(contains("20"), ~ scales::percent(.x, accuracy = .01))) |>
  rename(team = display_name)

# number one pick odds
n1_pick_odds <- final_standings_odds |>
  filter(type == "rank", result == 12) |>
  group_by(season, display_name) |>
  summarize(n1_pick_perc = sum(perc), .groups = "keep") |>
  pivot_wider(names_from = season, values_from = n1_pick_perc) |>
  arrange(desc(`2026`)) |>
  mutate(across(contains("20"), ~ scales::percent(.x, accuracy = .01))) |>
  rename(team = display_name) |>
  ungroup()

# most common finish
most_common_finish_df <- final_standings_odds |>
  filter(type == "rank") |>
  group_by(display_name, season) |>
  slice_max(perc) |>
  slice_head(n = 1) |>
  arrange(result, desc(perc)) |>
  ungroup() |>
  mutate(probability = scales::percent(perc)) |>
  rename(finish = result, team = display_name) |>
  select(-type)

# dfs to save -------------------------------------------------------------

write_csv(
  most_common_finish_df,
  here("Shiny/Saved Files/most_common_finish_df.csv")
)
write_csv(champion_odds, here("Shiny/Saved Files/champion_odds.csv"))
write_csv(playoff_odds, here("Shiny/Saved Files/playoff_odds.csv"))
write_csv(bye_odds, here("Shiny/Saved Files/bye_odds.csv"))
write_csv(n1_pick_odds, here("Shiny/Saved Files/n1_pick_odds.csv"))
