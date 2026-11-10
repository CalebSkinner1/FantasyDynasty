# Build App Data
# bundles everything the app shows into one file, Shiny/app_data.rds: a named list of the
# tables the scripts write to Shiny/Saved Files, which the app unpacks at startup
# (Shiny/app_call_data.R). Reading one .rds is several times faster than parsing each
# CSV, and it keeps column types (dates, character ids), so the app sets none. Runs after
# every script that writes to Saved Files, just before deploying (Frequent Updates.R).
# Run it by hand after rerunning any script, to see the change in the app.
#
# Also here, since they draw on several scripts' results: the home page headlines and the
# date the data was updated. The large over-time tables are rounded to the precision the
# app shows (the CSVs keep full precision for the scripts that read them).

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin building the app data...")

read_saved <- function(file, ...) {
  read_csv(here("Shiny/Saved Files", file), show_col_types = FALSE, ...)
}
ids <- cols(player_id = col_character())

# a .Rdata file's one object
load_saved <- function(file) {
  env <- new.env()
  load(here("Shiny/Saved Files", file), envir = env)
  get(ls(env), envir = env)
}

app_data <- list(
  users = read_saved("users.csv"),
  avatar = read_saved("avatar.csv"),

  # fantasy teams
  position_outlook_df = read_saved("position_outlook_df.csv"),
  acquisitions = read_saved("acquisitions.csv"),
  grab_team_assets_df = read_saved("grab_team_assets_df.csv", col_types = ids),
  player_avenues = read_saved("player_avenues.csv"),
  value_avenues = read_saved("value_avenues.csv"),
  team_value_time = read_saved("team_value_time.csv"),

  # individual players and player rankings
  player_total_value = read_saved("player_total_value.csv", col_types = ids),
  basic_info_df = read_saved("basic_info_df.csv", col_types = ids),
  plot_future_value_df = read_saved("plot_future_value_df.csv", col_types = ids) |>
    mutate(across(c(starts_with("proj_tva_"), expected_tva), \(x) round(x, 1))),
  season_value_added = read_saved("season_value_added.csv", col_types = ids),
  value_added = read_saved("value_added.csv", col_types = ids) |>
    mutate(across(c(sleeper_points, value_added, projection), \(x) round(x, 2))),
  player_headshot = read_saved("player_headshot.csv", col_types = ids),
  # the smoothed values, which the app's over-time charts show
  future_value_time = read_saved("future_value_time.csv", col_types = ids) |>
    transmute(name, player_id, future_value = round(future_value_smooth, 1), date),
  player_moves = read_saved(
    "player_moves.csv",
    col_types = cols(player_id = col_character(), target_id = col_character())
  ),

  # drafts
  picks_df = read_saved("picks_df.csv"),
  draft_grades_time = read_saved("draft_grades_time.csv"),

  # transactions
  overall_transaction_winners = read_saved("overall_transaction_winners.csv"),
  top_transactions = read_saved("top_transactions.csv"),
  total_transaction_value = load_saved("total_transaction_value.Rdata"),
  transaction_grades_time = read_saved(
    "transaction_grades_time.csv",
    col_types = cols(transaction_id = col_character())
  ),
  transaction_winners_time = read_saved("transaction_winners_time.csv"),

  # trades
  overall_trade_winners = read_saved("overall_trade_winners.csv"),
  lopsided_trades = read_saved("lopsided_trades.csv"),
  total_trade_value = load_saved("total_trade_value.Rdata"),
  trade_grades_time = read_saved(
    "trade_grades_time.csv",
    col_types = cols(trade_id = col_character())
  ),
  trade_winners_time = read_saved("trade_winners_time.csv"),

  # trade machine
  assets_df = read_saved("assets_df.csv", col_types = ids),
  roster_spot_value = read_saved("roster_spot_value.csv"),

  # matchups and history
  team_records_df = read_saved("team_records_df.csv"),
  championships_df = read_saved("championships_df.csv"),
  finals_df = read_saved("finals_df.csv"),
  playoffs_df = read_saved("playoffs_df.csv"),
  wins_df = read_saved("wins_df.csv"),

  # future standings
  champion_odds = read_saved("champion_odds.csv"),
  playoff_odds = read_saved("playoff_odds.csv"),
  bye_odds = read_saved("bye_odds.csv"),
  n1_pick_odds = read_saved("n1_pick_odds.csv"),
  odds_time = read_saved("odds_time.csv"),
  finish_odds = read_saved("finish_odds.csv"),

  # current matchups
  current_matchups = read_saved(
    "current_matchups.csv",
    col_types = cols(side = "c", team = "c", opponent = "c", .default = "d")
  ),
  matchup_scenarios = read_saved(
    "matchup_scenarios.csv",
    col_types = cols(team = "c", weight = "d", .default = "i")
  ),

  # team rankings
  weekly_elo = read_saved("weekly_elo.csv"),
  all_assets_summary_df = read_saved("all_assets_summary_df.csv"),
  total_success = read_saved("total_success.csv"),

  # modeling
  toy_tva_plot_data = read_saved("toy_tva_plot_data.csv"),
  toy_ktc_plot_data = read_saved("toy_ktc_plot_data.csv"),
  draft_fit_plot = read_saved("draft_fit_plot.csv")
)

# headshots only for the players the app can show
app_data$player_headshot <- app_data$player_headshot |>
  semi_join(app_data$player_total_value, by = join_by(player_id))

# player selectors: names shown, player_id returned (names can repeat), by future value
# (Individual Players.R); Player Rankings starts with the top three
player_choices <- read_saved("player_choices.csv", col_types = cols(.default = "c"))
app_data$player_choices <- deframe(player_choices)
app_data$top_players <- head(player_choices$player_id, 3)

# the upcoming season's name for charts: "Current Season" in season, "Next Season" in the
# offseason (set by Trade Machine.R)
app_data$upcoming_season_label <- first(
  app_data$assets_df$upcoming_label,
  default = "Current Season"
)

# the date shown on the home page
app_data$data_updated_label <- str_squish(format(today(), "%B %e, %Y"))

# Home page headlines --------------------------------------------------------------
# the tiles at the top of the home page. The home page leaves out a tile whose table has
# no rows.

# championship odds in the latest simulations, for the first season simulated
app_data$home_champion <- app_data$odds_time |>
  filter(odds == "champion", date == max(date)) |>
  filter(season == min(season)) |>
  arrange(desc(perc)) |>
  slice_head(n = 3)

# the top three Elo ratings after the latest week
app_data$home_elo <- app_data$weekly_elo |>
  filter(date_hide == max(date_hide)) |>
  arrange(desc(elo)) |>
  slice_head(n = 3)

# the most recent trade, with the team winning it so far (Lopsided Trades)
app_data$home_trade <- app_data$trade_grades_time |>
  distinct(trade_id, trade_date) |>
  arrange(desc(trade_date), desc(as.numeric(trade_id))) |>
  slice_head(n = 1) |>
  inner_join(
    app_data$lopsided_trades |> mutate(trade_id = as.character(trade_id)),
    by = join_by(trade_id)
  )

# this week's game with the most at stake in the playoff race (current_matchups is in
# that order; none when no regular-season week is left)
app_data$home_matchup <- app_data$current_matchups |>
  filter(game == first(game))

# the player with the most future value, and his team
app_data$home_player <- app_data$player_total_value |>
  slice_max(future_value, n = 1, with_ties = FALSE) |>
  left_join(
    app_data$assets_df |> distinct(player_id, team = display_name),
    by = join_by(player_id)
  )

# Save -------------------------------------------------------------------------------

saveRDS(app_data, here("Shiny/app_data.rds"))

message(
  "app data: ", length(app_data), " objects, ",
  round(file.size(here("Shiny/app_data.rds")) / 1e6, 1), " MB"
)
