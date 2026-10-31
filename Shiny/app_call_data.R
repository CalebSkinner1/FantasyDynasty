# call data from csvs

# app.R sets app_dir (the Shiny folder); default to the working directory
if (!exists("app_dir")) {
  app_dir <- "."
}
script_data_path <- file.path(app_dir, "Saved Files", "")

# load necessary functions
source(file.path(app_dir, "Script Support.R"))

# load data files

# load csv from script

# fantasy teams
position_outlook_df <- read_csv(
  str_c(script_data_path, "position_outlook_df.csv"),
  show_col_types = FALSE
)
acquisitions <- read_csv(
  str_c(script_data_path, "acquisitions.csv"),
  show_col_types = FALSE
)
grab_team_assets_df <- read_csv(
  str_c(script_data_path, "grab_team_assets_df.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
player_avenues <- read_csv(
  str_c(script_data_path, "player_avenues.csv"),
  show_col_types = FALSE
)
value_avenues <- read_csv(
  str_c(script_data_path, "value_avenues.csv"),
  show_col_types = FALSE
)

# individual players
basic_info_df <- read_csv(
  str_c(script_data_path, "basic_info_df.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
plot_future_value_df <- read_csv(
  str_c(script_data_path, "plot_future_value_df.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
value_added <- read_csv(
  str_c(script_data_path, "value_added.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
player_headshot <- read_csv(
  str_c(script_data_path, "player_headshot.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)

# drafts
picks_df <- read_csv(
  str_c(script_data_path, "picks_df.csv"),
  show_col_types = FALSE
)

# transactions
overall_transaction_winners <- read_csv(
  str_c(script_data_path, "overall_transaction_winners.csv"),
  show_col_types = FALSE
)
top_transactions <- read_csv(
  str_c(script_data_path, "top_transactions.csv"),
  show_col_types = FALSE
)
transaction_comparison <- read_csv(
  str_c(script_data_path, "transaction_comparison.csv"),
  show_col_types = FALSE
)
load(str_c(script_data_path, "total_transaction_value.Rdata"))

# trades
overall_trade_winners <- read_csv(
  str_c(script_data_path, "overall_trade_winners.csv"),
  show_col_types = FALSE
)
lopsided_trades <- read_csv(
  str_c(script_data_path, "lopsided_trades.csv"),
  show_col_types = FALSE
)
comparison <- read_csv(
  str_c(script_data_path, "comparison.csv"),
  show_col_types = FALSE
)
load(str_c(script_data_path, "total_trade_value.Rdata"))

# trade grades
assets_df <- read_csv(
  str_c(script_data_path, "assets_df.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
roster_spot_value <- read_csv(
  str_c(script_data_path, "roster_spot_value.csv"),
  show_col_types = FALSE
)

# matchups
team_records_df <- read_csv(
  str_c(script_data_path, "team_records_df.csv"),
  show_col_types = FALSE
)

# future standings
most_common_finish_df <- read_csv(
  str_c(script_data_path, "most_common_finish_df.csv"),
  show_col_types = FALSE
)
champion_odds <- read_csv(
  str_c(script_data_path, "champion_odds.csv"),
  show_col_types = FALSE
)
playoff_odds <- read_csv(
  str_c(script_data_path, "playoff_odds.csv"),
  show_col_types = FALSE
)
bye_odds <- read_csv(
  str_c(script_data_path, "bye_odds.csv"),
  show_col_types = FALSE
)
n1_pick_odds <- read_csv(
  str_c(script_data_path, "n1_pick_odds.csv"),
  show_col_types = FALSE
)

# team rankings
weekly_elo <- read_csv(
  str_c(script_data_path, "weekly_elo.csv"),
  show_col_types = FALSE
)
all_assets_summary_df <- read_csv(
  str_c(script_data_path, "all_assets_summary_df.csv"),
  show_col_types = FALSE
)
total_success <- read_csv(
  str_c(script_data_path, "total_success.csv"),
  show_col_types = FALSE
)

# history
championships_df <- read_csv(
  str_c(script_data_path, "championships_df.csv"),
  show_col_types = FALSE
)
finals_df <- read_csv(
  str_c(script_data_path, "finals_df.csv"),
  show_col_types = FALSE
)
playoffs_df <- read_csv(
  str_c(script_data_path, "playoffs_df.csv"),
  show_col_types = FALSE
)
wins_df <- read_csv(
  str_c(script_data_path, "wins_df.csv"),
  show_col_types = FALSE
)

# every player's team changes (Fantasy Teams.R), marked on the future value chart. Empty
# until that script has run once with this version
moves_path <- str_c(script_data_path, "player_moves.csv")
player_moves <- if (file.exists(moves_path)) {
  read_csv(
    moves_path,
    show_col_types = FALSE,
    col_types = cols(player_id = col_character(), target_id = col_character())
  )
} else {
  tibble(
    player_id = character(),
    date = as.Date(character()),
    label = character(),
    tab = character(),
    target_id = character()
  )
}

# values and grades over time (Fantasy Teams, Draft Grades, Transaction Grades, and
# Trade Grades scripts): empty until those scripts have run once with this version
read_over_time <- function(file, col_types = cols(), empty = tibble()) {
  path <- str_c(script_data_path, file)
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE, col_types = col_types)
  } else {
    empty
  }
}

team_value_time <- read_over_time(
  "team_value_time.csv",
  empty = tibble(date = as.Date(character()), roster_id = integer(), team = character(),
                 asset = character(), future_value = double())
)
draft_grades_time <- read_over_time(
  "draft_grades_time.csv",
  empty = tibble(draft = character(), roster_id = integer(), date = as.Date(character()),
                 value_over_expected = double())
)
transaction_grades_time <- read_over_time(
  "transaction_grades_time.csv",
  col_types = cols(transaction_id = col_character()),
  empty = tibble(transaction_id = character(), transaction_date = as.Date(character()),
                 date = as.Date(character()), realized_value = double(), future_value = double())
)
trade_grades_time <- read_over_time(
  "trade_grades_time.csv",
  col_types = cols(trade_id = col_character()),
  empty = tibble(trade_id = character(), trade_date = as.Date(character()), team_name = character(),
                 date = as.Date(character()), realized_value = double(), future_value = double())
)

# each team's total value gained from all its trades and from all its transactions on every
# date (Trade and Transaction Grades pages). Before a team's first one, it has gained 0. On
# the latest date these match the Winners tables, up to rounding.
winners_time <- function(grades_time) {
  grades_time |>
    summarize(
      realized_value = sum(realized_value),
      future_value = sum(future_value),
      .by = c(team_name, date)
    ) |>
    complete(team_name, date, fill = list(realized_value = 0, future_value = 0)) |>
    rename(team = team_name) |>
    arrange(team, date)
}
trade_winners_time <- winners_time(trade_grades_time)
transaction_winners_time <- transaction_grades_time |>
  inner_join(
    transaction_comparison |>
      transmute(transaction_id = as.character(transaction_id), team_name),
    by = join_by(transaction_id)
  ) |>
  winners_time()

# Future Standings: the odds on every past date and each team's chance at every finish
odds_time <- read_over_time(
  "odds_time.csv",
  empty = tibble(odds = character(), season = double(), date = as.Date(character()),
                 team = character(), perc = double())
)
finish_odds <- read_over_time(
  "finish_odds.csv",
  empty = tibble(season = double(), team = character(), finish = integer(), probability = double())
)

# player rankings
future_value_time <- read_csv(
  str_c(script_data_path, "future_value_time.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)

# load from data folder
player_total_value <- read_csv(
  str_c(script_data_path, "player_total_value.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
users <- read_csv(str_c(script_data_path, "users.csv"), show_col_types = FALSE)
season_value_added <- read_csv(
  str_c(script_data_path, "season_value_added.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
avatar <- read_csv(
  str_c(script_data_path, "avatar.csv"),
  show_col_types = FALSE
)

# modeling
toy_tva_plot_data <- read_csv(
  str_c(script_data_path, "toy_tva_plot_data.csv"),
  show_col_types = FALSE
)
toy_ktc_plot_data <- read_csv(
  str_c(script_data_path, "toy_ktc_plot_data.csv"),
  show_col_types = FALSE
)
draft_fit_plot <- read_csv(
  str_c(script_data_path, "draft_fit_plot.csv"),
  show_col_types = FALSE
)

# files written before player_id was added (Scraping.R and Individual Players.R now
# include it): match by name
if (!"player_id" %in% names(player_headshot)) {
  player_headshot <- player_headshot |>
    inner_join(select(player_total_value, name, player_id), by = join_by(name))
}
if (!"player_id" %in% names(basic_info_df)) {
  basic_info_df <- basic_info_df |>
    inner_join(select(player_total_value, name, player_id), by = join_by(name))
}

# player selectors: names shown, player_id returned (names can repeat)
player_choices <- player_total_value |>
  filter(position %in% c("QB", "WR", "RB", "TE")) |>
  select(name, player_id) |>
  deframe()

# the three players with the most future value (Player Rankings starts with them)
top_players <- player_total_value |>
  filter(position %in% c("QB", "WR", "RB", "TE")) |>
  slice_max(future_value, n = 3, with_ties = FALSE) |>
  pull(player_id)

# the upcoming season's name for charts: "Current Season" in season, "Next Season" in the
# offseason (Trade Machine.R)
upcoming_season_label <- first(assets_df$upcoming_label, default = "Current Season")

# when the data was last updated: written by Workflow/Frequent Updates.R before each
# deploy (otherwise the newest data file's date)
updated_path <- str_c(script_data_path, "last_updated.csv")
data_updated <- if (file.exists(updated_path)) {
  as.Date(read_csv(updated_path, show_col_types = FALSE)$updated[1])
} else {
  as.Date(max(file.mtime(list.files(script_data_path, full.names = TRUE))))
}
data_updated_label <- str_squish(format(data_updated, "%B %e, %Y"))

# Home page headlines ------------------------------------------------------------
# the tiles at the top of the home page, from data loaded above. The home page leaves out a
# tile whose data has no rows.

# playoff odds in the latest simulations, for the first season simulated
home_playoffs <- odds_time |>
  filter(odds == "playoffs", date == max(date)) |>
  filter(season == min(season)) |>
  arrange(desc(perc))

# Elo ratings after the latest week
home_elo <- weekly_elo |>
  filter(date_hide == max(date_hide)) |>
  arrange(desc(elo))

# the most recent trade, with the team winning it so far (Lopsided Trades)
home_trade <- trade_grades_time |>
  distinct(trade_id, trade_date) |>
  arrange(desc(trade_date), desc(as.numeric(trade_id))) |>
  slice_head(n = 1) |>
  inner_join(
    lopsided_trades |> mutate(trade_id = as.character(trade_id)),
    by = join_by(trade_id)
  )

# the player with the most future value, and his team
home_player <- player_total_value |>
  slice_max(future_value, n = 1, with_ties = FALSE) |>
  left_join(
    assets_df |> distinct(player_id, team = display_name),
    by = join_by(player_id)
  )
