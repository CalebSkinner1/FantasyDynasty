# Matchups
# every completed game with team names and the outcome, for the Matchups page
# (team_matchup_record() in Shiny/Script Support.R builds the head-to-head table)
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Matchups...")

matchups_table <- read_csv(
  here("Data/derived/matchups_table.csv"),
  show_col_types = FALSE
)

users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, display_name)

# Completed weeks ---------------------------------------------------------------
# a fantasy week is complete once every NFL game of that week has a final score, so a run
# in the middle of a week never scores games still in progress. If the NFL schedule
# can't be loaded, every week with points counts as complete.

nfl_weeks <- tryCatch(
  nflreadr::load_schedules(unique(matchups_table$season)) |>
    filter(game_type == "REG") |>
    summarize(complete = all(!is.na(home_score)), .by = c(season, week)),
  error = function(e) {
    warning(
      "NFL schedule unavailable, so every week with points counts as complete: ",
      conditionMessage(e)
    )
    NULL
  }
)

complete_weeks <- if (is.null(nfl_weeks)) {
  matchups_table |> distinct(season, week)
} else {
  nfl_weeks |> filter(complete) |> select(season, week)
}

# Records -----------------------------------------------------------------------

team_records_df <- matchups_table |>
  semi_join(complete_weeks, by = join_by(season, week)) |>
  filter(points > 0 | opp_points > 0) |> # unplayed playoff games score 0
  left_join(users, by = join_by(roster_id)) |>
  rename(team = display_name) |>
  left_join(users, by = join_by(opponent_id == roster_id)) |>
  rename(opponent = display_name) |>
  mutate(outcome = if_else(points > opp_points, "win", "loss"))

# Example (app function in Shiny/Script Support.R) ------------------------------
# team_matchup_record(team_records_df, "Arvs", "regular season", shiny = TRUE)

# upcoming matchups: not sure this is worth it yet
# yearly_record <- team_records_df |>
#   summarize(
#     wins = sum(outcome == "win"),
#     losses = sum(outcome == "loss"),
#     .by = c(season, team)
#   )
#
# matchups_table_names |> filter(points == 0) |>
#   slice_min(week) |>
#   rename(team1 = team,
#          team2 = opponent) |>
#   left_join(yearly_record, by = join_by(season, team1 == team))

# dfs to save -------------------------------------------------------------

write_csv(team_records_df, here("Shiny/Saved Files/team_records_df.csv"))
