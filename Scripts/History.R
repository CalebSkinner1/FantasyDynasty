# League History
# tables for the History page: championships, finals and playoff appearances for every
# team, and every completed game by type (regular season, winner's bracket, loser's
# bracket) for the records. Reuses the game table from Matchups.R, so it runs after it.
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing History...")

# load data
team_records_df <- read_csv(
  here("Shiny/Saved Files/team_records_df.csv"),
  show_col_types = FALSE
)
users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(team = display_name)

# every completed game. The winner's bracket is the championship path; the consolation
# games (3rd and 5th place) count with the loser's bracket
wins_df <- team_records_df |>
  rename(result = outcome) |>
  mutate(
    type = case_when(
      round == "regular season" ~ "regular season",
      round %in% c("1st round", "2nd round", "Championship") ~ "winner's bracket",
      .default = "loser's bracket"
    )
  )

# count per team (teams with none get 0) and the latest season it happened
team_counts <- function(games, count_name) {
  games |>
    distinct(season, team) |>
    summarize(n = n(), most_recent = max(season), .by = team) |>
    right_join(users, by = join_by(team)) |>
    mutate(n = replace_na(n, 0L)) |>
    arrange(desc(n), desc(most_recent), team) |>
    select(team, n, most_recent) |>
    rename(!!count_name := n)
}

# most championships
championships_df <- wins_df |>
  filter(round == "Championship", result == "win") |>
  team_counts("championships")

# most finals appearances
finals_df <- wins_df |>
  filter(round == "Championship") |>
  team_counts("finals_appearances")

# most playoff berths (teams with a bye start in the 2nd round)
playoffs_df <- wins_df |>
  filter(round %in% c("1st round", "2nd round")) |>
  team_counts("playoff_berths")

# Examples (app functions in Shiny/Script Support.R) ------------------------------
# compute_most_wins(wins_df, "All", 2024)
# compute_total_points(wins_df, "All", 2024)
# highest_team_total(wins_df, "All", c(2024, 2025))
# compute_total_points_player(value_added, wins_df, "All", 2024, "All")
# compute_value_added_player(value_added, wins_df, "All", 2024, "All")
# highest_player_total(value_added, wins_df, "All", 2024, "All")

# dfs to save -------------------------------------------------------------

write_csv(championships_df, here("Shiny/Saved Files/championships_df.csv"))
write_csv(finals_df, here("Shiny/Saved Files/finals_df.csv"))
write_csv(playoffs_df, here("Shiny/Saved Files/playoffs_df.csv"))
write_csv(wins_df, here("Shiny/Saved Files/wins_df.csv"))
