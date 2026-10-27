# Transaction Grades
# grades each waiver and free agent transaction by the value the team gained and lost,
# assessed today: realized value added since the transaction plus current future value.
# Writes the app's transaction tables, marginal_transaction_value.csv (the value of a
# roster spot, used by Trade Grades), and Data/derived/individual_transactions.csv,
# transaction_adds.csv, and transaction_moves.csv (used by Fantasy Teams). Also grades
# each transaction on every past date (transaction_grades_time.csv).

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

source(here("Scripts/Over Time Support.R")) # value_over_time()

message("begin computing Transaction Grades...")

# load data
transactions <- readRDS(here("Data/derived/transactions.rds")) #transactions, including trades
player_total_value <- read_csv(
  here("Shiny/Saved Files/player_total_value.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
value_added <- read_csv(
  here("Data/derived/va.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)

player_info <- read_csv(
  here("Data/raw/player_info.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  distinct(player_id, .keep_all = TRUE) |>
  select(-birth_date)

users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(-owner_id)

completed_transactions <- transactions |>
  filter(type %in% c("waiver", "free_agent")) |>
  filter(status == "complete") |>
  mutate(transaction_id = as.character(row_number()))

# every player added or dropped: one row per transaction and player
transaction_moves <- function(col, move_type) {
  completed_transactions |>
    select(transaction_id, season, week, players = all_of(col)) |>
    unpack(players) |>
    pivot_longer(
      cols = -c(transaction_id, season, week),
      names_to = "player_id",
      values_to = "roster_id",
      values_drop_na = TRUE
    ) |>
    mutate(type = move_type)
}

moves <- bind_rows(
  transaction_moves("adds", "add"),
  transaction_moves("drops", "drop")
) |>
  mutate(season = as.numeric(season), week = as.numeric(week))

# value added since the transaction. In the transaction's own week, an added player's
# value counts only if he played for the adding team that week (and a dropped player's
# only if he didn't play for the dropping team)
counted_va <- moves |>
  inner_join(
    value_added |>
      select(
        player_id,
        va_roster_id = roster_id,
        va_season = season,
        va_week = week,
        value_added
      ),
    by = join_by(player_id),
    relationship = "many-to-many"
  ) |>
  filter(
    va_season > season |
      (va_season == season & va_week > week) |
      (va_season == season &
        va_week == week &
        (va_roster_id == roster_id) == (type == "add"))
  )

realized_since <- counted_va |>
  summarize(
    realized_value = sum(value_added),
    .by = c(transaction_id, player_id, type)
  )

# value gained (adds) and lost (drops) in every transaction: realized value since the
# transaction plus current future value, negative for players dropped
transaction_values <- moves |>
  left_join(realized_since, by = join_by(transaction_id, player_id, type)) |>
  left_join(
    player_info |> select(player_id, name, position),
    by = join_by(player_id)
  ) |>
  left_join(
    player_total_value |> select(player_id, future_value),
    by = join_by(player_id)
  ) |>
  left_join(users, by = join_by(roster_id)) |>
  mutate(
    across(c(realized_value, future_value), ~ replace_na(.x, 0)),
    sign = if_else(type == "drop", -1, 1),
    realized_value = sign * realized_value,
    future_value = sign * future_value,
    total_value = realized_value + future_value
  ) |>
  arrange(as.numeric(transaction_id), type) |> # adds before drops
  select(
    transaction_id,
    team_name = display_name,
    player_id,
    name,
    position,
    future_value,
    realized_value,
    total_value,
    type,
    week,
    season
  )

# one table per transaction (the app's breakdown), named by transaction number
total_transaction_value <- transaction_values |>
  select(-transaction_id) |>
  split(factor(
    transaction_values$transaction_id,
    levels = unique(transaction_values$transaction_id)
  ))

transaction_comparison <- transaction_values |>
  summarize(
    total_transaction_value = sum(total_value),
    total_future_value = sum(future_value),
    total_realized_value = sum(realized_value),
    .by = c(transaction_id, team_name)
  )

individual_transactions <- transaction_comparison |> # transactions
  rename(
    realized_value = "total_realized_value",
    total_value = "total_transaction_value",
    future_value = "total_future_value"
  ) |>
  left_join(users, by = join_by(team_name == display_name)) |>
  left_join(
    transaction_values |>
      filter(type == "add") |>
      group_by(transaction_id) |>
      slice_max(total_value, n = 1, with_ties = FALSE) |>
      select(transaction_id, team_name, name, position, season, week),
    by = join_by(transaction_id, team_name)
  ) |>
  filter(!is.na(name)) |>
  mutate(
    avenue = str_c(season, " Week ", week, " transaction"),
    value_over_expected = total_value
  ) |>
  select(
    transaction_id,
    roster_id,
    name,
    position,
    realized_value,
    future_value,
    total_value,
    value_over_expected,
    avenue
  )

# most valuable transactions
top_transactions <- individual_transactions |>
  arrange(desc(total_value)) |>
  left_join(users, by = join_by(roster_id)) |>
  select(-value_over_expected, -roster_id) |>
  rename(
    team_name = display_name,
    transaction_details = "avenue"
  ) |>
  relocate(c(team_name, transaction_details))

# mean add/drop value (mean value gained from adding/dropping a player), with its realized
# and future parts (total_value_added = realized_value + future_value)
marginal_transaction_value <- transaction_values |>
  filter(position != "K", position != "DST") |>
  summarize(
    total_value_added = mean(total_value),
    realized_value = mean(realized_value),
    future_value = mean(future_value),
    .by = c(season, type)
  ) |>
  arrange(season, type)

# by fantasy owner
overall_transaction_winners <- transaction_comparison |>
  group_by(team_name) |>
  summarize(
    transactions = n(),
    total_transaction_value = sum(total_transaction_value),
    total_future_value = sum(total_future_value),
    total_realized_value = sum(total_realized_value)
  ) |>
  arrange(desc(total_transaction_value)) |>
  # left_join(users, by = join_by(team_name == display_name)) |>
  select(
    team_name,
    transactions,
    total_realized_value,
    total_future_value,
    total_transaction_value
  )

# Transaction grades over time --------------------------------------------------
# each transaction's grade on every KTC date from the first one on or after it went
# through: realized value from the transaction through the weeks finished by that date,
# plus future value that date (see Scripts/Over Time Support.R). The latest date equals
# today's grade. Written to Data/model_output/transaction_grades_time.csv, one row per
# transaction, team, and date, and rounded for the app (Shiny/Saved Files).

future_value_time <- read_csv(
  here("Shiny/Saved Files/future_value_time.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
season_dates <- read_csv(here("Shiny/Saved Files/season_dates.csv"), show_col_types = FALSE)

transaction_dates <- completed_transactions |>
  transmute(transaction_id, transaction_date = sleeper_date(status_updated))

transaction_assets <- transaction_values |>
  left_join(transaction_dates, by = join_by(transaction_id)) |>
  transmute(
    asset = str_c(transaction_id, player_id, type, sep = "|"),
    transaction_id,
    transaction_date,
    team_name,
    start = transaction_date,
    sign = if_else(type == "drop", -1, 1),
    player_id,
    pick_season = NA_real_,
    pick_round = NA_integer_,
    pick_slot = NA_integer_,
    pick_drafted = as_date(NA),
    fixed_realized = 0,
    fixed_future = 0
  )

transaction_grades_time <- value_over_time(
  transaction_assets,
  counted_va |>
    transmute(
      asset = str_c(transaction_id, player_id, type, sep = "|"),
      season = va_season,
      week = va_week,
      value_added
    ),
  sort(unique(future_value_time$date)),
  future_value_time,
  week_end_dates(value_added, season_dates)
) |>
  left_join(
    transaction_assets |> select(asset, transaction_id, transaction_date, team_name),
    by = join_by(asset)
  ) |>
  summarize(
    realized_value = sum(realized_value),
    future_value = sum(future_value),
    .by = c(transaction_id, transaction_date, team_name, date)
  ) |>
  mutate(total_value = realized_value + future_value) |>
  arrange(as.numeric(transaction_id), date)

write_csv(
  transaction_grades_time,
  here("Data/model_output/transaction_grades_time.csv")
)

# the app's copy: one team per transaction (top_transactions.csv names it), and the total
# is realized + future
transaction_grades_time |>
  transmute(
    transaction_id,
    transaction_date,
    date,
    realized_value = round(realized_value, 1),
    future_value = round(future_value, 1)
  ) |>
  write_csv(here("Shiny/Saved Files/transaction_grades_time.csv"))

# dfs to save -------------------------------------------------------------

write_csv(
  overall_transaction_winners,
  here("Shiny/Saved Files/overall_transaction_winners.csv")
)

write_csv(top_transactions, here("Shiny/Saved Files/top_transactions.csv"))

write_csv(
  transaction_comparison,
  here("Shiny/Saved Files/transaction_comparison.csv")
)

write_csv(
  marginal_transaction_value,
  here("Shiny/Saved Files/marginal_transaction_value.csv")
)

save(
  total_transaction_value,
  file = here("Shiny/Saved Files/total_transaction_value.Rdata")
)

# for Fantasy Teams: graded transactions (acquisitions) and every player added, with
# when (for the latest way each team acquired each player)
write_csv(
  individual_transactions,
  here("Data/derived/individual_transactions.csv")
)

users_by_name <- users |> select(roster_id, team_name = display_name)
transaction_adds <- transaction_values |>
  filter(type == "add") |>
  left_join(users_by_name, by = join_by(team_name)) |>
  left_join(
    completed_transactions |> select(transaction_id, created),
    by = join_by(transaction_id)
  ) |>
  transmute(
    roster_id,
    player_id,
    season,
    week,
    created,
    avenue = str_c(season, " W", week, " transaction")
  )

write_csv(transaction_adds, here("Data/derived/transaction_adds.csv"))

# every add and drop with when it happened (team changes on the Individual Players page)
transaction_moves_dated <- moves |>
  left_join(
    completed_transactions |> select(transaction_id, created),
    by = join_by(transaction_id)
  ) |>
  select(transaction_id, roster_id, player_id, type, season, week, created)

write_csv(
  transaction_moves_dated,
  here("Data/derived/transaction_moves.csv")
)

# Examples ----------------------------------------------------------------

# inspect_individual_transaction(32) # Sam Darnold
# inspect_individual_transaction(48) # Tyrone Tracy
# inspect_individual_transaction(201) # Khalil Shakir
# inspect_individual_transaction(193) # Jalen McMillan

# inspect_individual_transaction(49) # Sam Darnold
# inspect_individual_transaction(108) # Michael Penix
# inspect_individual_transaction(359) # Jalen McMillan
# inspect_individual_transaction(16) # Cade Otton
