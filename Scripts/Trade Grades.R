# Trade Grades
# grades every trade by the value each team gained and lost, assessed today:
#   players: realized value added since the trade (on any roster) plus future value
#   picks:   expected pick value, discounted .95 per season until the draft (Pick Values.R)
#   roster spots: a team that receives more players than it sends must drop players, and
#            one that sends more can add free agents; each spot is worth the average add
#            or drop in that season (marginal_transaction_value.csv, Transaction Grades.R).
#            Like the Trade Machine, a team's grade is what it received minus what its
#            partners received, so a partner's roster spot counts against the team and
#            every trade sums to zero.
# Runs after Transaction Grades and Pick Values. Writes the app's trade tables and
# Data/derived/individual_trades.csv, trade_adds.csv, and trade_moves.csv (used by Fantasy
# Teams).

suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Trade Grades...")

# load data
transactions <- readRDS(here("Data/derived/transactions.rds")) # transactions, including trades

pick_values <- read_csv(
  here("Data/model_output/all_draft_pick_exp_values.csv"),
  show_col_types = FALSE
) |>
  select(season, round, pick_slot, future_value)

player_total_value <- read_csv(
  here("Shiny/Saved Files/player_total_value.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  select(player_id, future_value)

value_added <- read_csv(
  here("Data/derived/va.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  select(player_id, roster_id, season, week, value_added)

marginal_transaction_value <- read_csv(
  here("Shiny/Saved Files/marginal_transaction_value.csv"),
  show_col_types = FALSE
)

player_info <- read_csv(
  here("Data/raw/player_info.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  distinct(player_id, .keep_all = TRUE) |>
  select(player_id, name, position)

users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, display_name)

# Grade each trade ------------------------------------------------------------

# players moved in a trade (adds or drops) as one row per player and roster
trade_players <- function(x) {
  if (!is.data.frame(x) || ncol(x) == 0) {
    return(tibble(player_id = character(), roster_id = integer()))
  }
  x |>
    pivot_longer(
      cols = everything(),
      names_to = "player_id",
      values_to = "roster_id"
    ) |>
    drop_na()
}

# value added since the trade. In the trade's own week, a player's value counts for the
# receiving team only if he played for it that week (and against the sending team only
# if he didn't play for it)
realized_since <- function(players, this_season, this_week, gained) {
  players |>
    inner_join(
      value_added,
      by = join_by(player_id),
      suffix = c("", "_va"),
      relationship = "many-to-many"
    ) |>
    filter(
      season > this_season |
        (season == this_season & week > this_week) |
        (season == this_season &
          week == this_week &
          (roster_id_va == roster_id) == gained)
    ) |>
    summarize(realized_value = sum(value_added), .by = player_id)
}

player_values <- function(players, this_season, this_week, type) {
  players |>
    left_join(player_info, by = join_by(player_id)) |>
    left_join(player_total_value, by = join_by(player_id)) |>
    left_join(
      realized_since(players, this_season, this_week, gained = type == "add"),
      by = join_by(player_id)
    ) |>
    mutate(
      future_value = replace_na(future_value, 0),
      realized_value = replace_na(realized_value, 0),
      type = type
    )
}

# value of one roster spot in a season: the average add (gained) or drop (lost, negative),
# as realized, future, and total value
spot_value <- function(this_season, this_type) {
  mtv <- marginal_transaction_value |> filter(type == this_type)
  if (!this_season %in% mtv$season) {
    this_season <- max(mtv$season)
  }
  mtv |>
    filter(season == this_season) |>
    transmute(realized_value, future_value, total_value = total_value_added)
}

compute_trade_value <- function(trade) {
  this_week <- as.numeric(trade$week)
  this_season <- as.numeric(trade$season)

  adds <- trade_players(trade$adds)
  drops <- trade_players(trade$drops)

  players <- bind_rows(
    player_values(adds, this_season, this_week, "add"),
    player_values(drops, this_season, this_week, "drop")
  )

  # picks: the new owner gains the pick and the previous owner loses it. position is the
  # team the pick originally belonged to
  picks <- trade$draft_picks[[1]]
  if (is.data.frame(picks) && nrow(picks) > 0) {
    picks <- picks |>
      mutate(season = as.numeric(season)) |>
      left_join(pick_values, by = join_by(roster_id == pick_slot, season, round)) |>
      left_join(users, by = join_by(roster_id)) |>
      mutate(
        player_id = NA_character_,
        name = str_c(season, " ", round, "R Draft Pick"),
        position = display_name,
        future_value = replace_na(future_value, 0),
        realized_value = 0
      )
    picks <- bind_rows(
      picks |> mutate(roster_id = owner_id, type = "add"),
      picks |> mutate(roster_id = previous_owner_id, type = "drop")
    ) |>
      select(roster_id, player_id, name, position, future_value, realized_value, type)
  } else {
    picks <- tibble()
  }

  # roster size adjustment: net players received (picks take no roster spot). A team that
  # receives more players must drop the extra (lost spots, negative); one that sends more
  # can add free agents (gained spots). Each team's own adjustment is something it
  # received ("add"); a partner's adjustment counts against the team ("drop"), split
  # evenly among the partners, so the trade sums to zero
  trade_teams <- unique(c(players$roster_id, if (nrow(picks) > 0) picks$roster_id))
  roster_spots <- full_join(
    count(adds, roster_id, name = "received"),
    count(drops, roster_id, name = "sent"),
    by = join_by(roster_id)
  ) |>
    mutate(
      across(c(received, sent), ~ replace_na(.x, 0L)),
      net = received - sent
    ) |>
    filter(net != 0)

  adjustment <- roster_spots |>
    pmap(\(roster_id, net, ...) {
      spot <- spot_value(this_season, if_else(net > 0, "drop", "add"))
      own <- spot |>
        mutate(
          across(everything(), ~ .x * abs(net)),
          roster_id = roster_id,
          position = if_else(net > 0, "lost", "gained"),
          type = "add"
        )
      partners <- setdiff(trade_teams, roster_id)
      partner_rows <- own |>
        mutate(across(c(realized_value, future_value, total_value), ~ -.x / length(partners))) |>
        select(-roster_id) |>
        expand_grid(roster_id = partners) |>
        mutate(position = str_c("partner ", position), type = "drop")
      bind_rows(own, partner_rows)
    }) |>
    bind_rows() |>
    mutate(name = "roster size adjustment", player_id = NA_character_)

  bind_rows(players, picks) |>
    mutate(
      # value lost counts against the team
      realized_value = if_else(type == "drop", -realized_value, realized_value),
      future_value = if_else(type == "drop", -future_value, future_value),
      total_value = realized_value + future_value
    ) |>
    bind_rows(adjustment) |>
    mutate(season = this_season, week = this_week) |>
    left_join(users, by = join_by(roster_id)) |>
    select(
      team_name = display_name,
      player_id,
      name,
      position,
      future_value,
      realized_value,
      season,
      week,
      type,
      total_value
    ) |>
    arrange(desc(team_name))
}

trades <- transactions |>
  filter(type == "trade")

# named by trade number; trades that moved no players or picks (e.g. FAAB only) are dropped
total_trade_value <- trades |>
  split(seq_len(nrow(trades))) |>
  map(compute_trade_value) |>
  discard(\(x) nrow(x) == 0)

comparison <- total_trade_value |>
  map(
    ~ .x |>
      summarize(
        total_trade_value = sum(total_value),
        total_future_value = sum(future_value),
        total_realized_value = sum(realized_value),
        .by = c(team_name, season, week)
      )
  ) |>
  bind_rows(.id = "trade_id")

# the other teams in each trade
trade_partners <- comparison |>
  distinct(trade_id, team_name) |>
  inner_join(
    comparison |> distinct(trade_id, partner = team_name),
    by = join_by(trade_id),
    relationship = "many-to-many"
  ) |>
  filter(team_name != partner) |>
  summarize(other_teams = str_flatten(partner, ", "), .by = c(trade_id, team_name))

# most valuable asset each team received (a player or pick, not a roster spot)
top_asset <- bind_rows(total_trade_value, .id = "trade_id") |>
  filter(type == "add", name != "roster size adjustment") |>
  slice_max(total_value, n = 1, with_ties = FALSE, by = c(trade_id, team_name)) |>
  select(trade_id, team_name, name, position)

# most egregiously favorable trades
individual_trades <- comparison |>
  rename(
    realized_value = "total_realized_value",
    total_value = "total_trade_value",
    future_value = "total_future_value"
  ) |>
  left_join(users, by = join_by(team_name == display_name)) |>
  left_join(trade_partners, by = join_by(trade_id, team_name)) |>
  left_join(top_asset, by = join_by(trade_id, team_name)) |>
  mutate(
    # a team that received only a roster spot (it sent players for nothing)
    name = coalesce(name, "roster spot"),
    value_over_expected = total_value,
    avenue = str_c(season, " W", week, " trade with ", other_teams)
  ) |>
  select(
    trade_id,
    roster_id,
    name,
    position,
    realized_value,
    future_value,
    total_value,
    value_over_expected,
    avenue
  )

lopsided_trades <- individual_trades |>
  filter(total_value > 0) |>
  arrange(desc(total_value)) |>
  left_join(users, by = join_by(roster_id)) |>
  select(-value_over_expected, -roster_id) |>
  rename(
    team_name = display_name,
    trade_details = "avenue",
    top_asset_acquired = name
  ) |>
  relocate(c(team_name, trade_details))

# by fantasy owner
overall_trade_winners <- comparison |>
  group_by(team_name) |>
  summarize(
    trades = n(),
    total_trade_value = sum(total_trade_value),
    total_future_value = sum(total_future_value),
    total_realized_value = sum(total_realized_value)
  ) |>
  arrange(desc(total_trade_value)) |>
  select(
    team_name,
    trades,
    total_realized_value,
    total_future_value,
    total_trade_value
  )

# trade grade over time ---------------------------------------------------
#
# # realized value over time
# date <- map_vec(value_added$season, ~season_start[year(season_start) == .x]) + days(4) + weeks(value_added$week - 1)
# realized_value_time <- value_added |> mutate(date = date) |>
#   select(name, value_added, date)
#
# # future player value over time
# future_value_time
#
# # draft pick value over time
# load(here("Modeling/team_tva_ranking.RData"))
# matchup_fit_coef <- read_csv(here("Modeling/matchup_fit_coef.csv"))
# matchups_table <- read_csv(here("Data/derived/matchups_table.csv"))
# season_dates <- read_csv(here("Shiny/Saved Files/season_dates.csv"))
#
# season_end <- season_dates$season_end[season_dates$season_end > today()] |> min()
# season_start <- season_dates$season_start[season_dates$season_start < season_end] |> max()
#
# team_tva_list <- map(team_tva_ranking, ~.x |> mutate(group = (row_number() - 1)%/% 12) |> group_split(group, .keep = FALSE)) |>
#   transpose()
#
# dates <- unique(future_value_time$date)
#
# tables_list <- map(dates, ~construct_table(matchups_table, season_dates, .x))
#
# tic()
# final_standings_time <- map(tables_list, ~compute_final_standings_odds(.x, team_tva_list, matchup_fit_coef, 3, n_sims = 50))
# toc()
#
# exp_draft_values_time <- map(final_standings_time, ~.x |>
#   expand_grid(round = 1:3) |>
#   mutate(pick_no = (round-1)*12 + (13-rank)) |>
#   left_join(rookie_draft_values, by = join_by(pick_no)) |>
#   # weight each potential draft pick by odds of receiving it
#   mutate(weight_value = perc*proj_tva_50) |>
#   # for each roster and season and round
#   group_by(season, roster_id, round) |>
#   summarize(exp_total_value = sum(weight_value),
#             .groups = "keep") |>
#   ungroup() |>
#   # but draft in next season
#   mutate(season = season + 1))
#
# # compute value over time
# total_trade_value_time

rm(value_added)

# dfs to save -------------------------------------------------------------

write_csv(
  overall_trade_winners,
  here("Shiny/Saved Files/overall_trade_winners.csv")
)

write_csv(lopsided_trades, here("Shiny/Saved Files/lopsided_trades.csv"))

write_csv(comparison, here("Shiny/Saved Files/comparison.csv"))

save(
  total_trade_value,
  file = here("Shiny/Saved Files/total_trade_value.Rdata")
)

# for Fantasy Teams: graded trades (acquisitions) and every player or pick received, with
# when (for the latest way each team acquired each asset)
write_csv(individual_trades, here("Data/derived/individual_trades.csv"))

trade_adds <- bind_rows(total_trade_value, .id = "trade_id") |>
  filter(type == "add", name != "roster size adjustment") |>
  left_join(users, by = join_by(team_name == display_name)) |>
  left_join(
    trades |> transmute(trade_id = as.character(row_number()), created),
    by = join_by(trade_id)
  ) |>
  transmute(
    roster_id,
    player_id,
    name,
    position,
    season,
    week,
    created,
    avenue = str_c(season, " W", week, " trade")
  )

write_csv(trade_adds, here("Data/derived/trade_adds.csv"))

# every player traded, from and to which team, and when (team changes on the Individual
# Players page)
trade_player_rows <- bind_rows(total_trade_value, .id = "trade_id") |>
  filter(!is.na(player_id)) |>
  left_join(users, by = join_by(team_name == display_name))

trade_moves <- trade_player_rows |>
  filter(type == "add") |>
  select(trade_id, player_id, to_roster_id = roster_id, season, week) |>
  left_join(
    trade_player_rows |>
      filter(type == "drop") |>
      select(trade_id, player_id, from_roster_id = roster_id),
    by = join_by(trade_id, player_id)
  ) |>
  left_join(
    trades |> transmute(trade_id = as.character(row_number()), created),
    by = join_by(trade_id)
  )

write_csv(trade_moves, here("Data/derived/trade_moves.csv"))

# Examples (app functions in Shiny/Script Support.R) ------------------------------
# inspect_individual_trade(9) # what is trade 9
# inspect_individual_trade(20) # what is trade 20
