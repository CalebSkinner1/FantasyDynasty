# Individual Players
# tables behind the Individual Players page: each valued player's basic info and value to
# date, his projected value added by season (for the future value chart), and the weekly
# value added table used across the app
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Individual Players...")

# load data
player_simulations <- readRDS(here("Modeling/models/player_simulations.rds"))
season_value_added <- read_csv(
  here("Shiny/Saved Files/season_value_added.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(-owner_id)
value_added <- read_csv(here("Data/derived/va.csv"), show_col_types = FALSE) |>
  left_join(users, by = join_by(roster_id)) |>
  arrange(season, week, desc(type), display_name)
# a few retired players are listed twice (two positions under one id)
player_info <- read_csv(here("Data/raw/player_info.csv"), show_col_types = FALSE) |>
  distinct(player_id, .keep_all = TRUE)
player_total_value <- read_csv(
  here("Shiny/Saved Files/player_total_value.csv"),
  show_col_types = FALSE
)
# season and share of it elapsed (p), written by Player Total Value.R
season_context <- read_csv(
  here("Data/model_output/season_context.csv"),
  show_col_types = FALSE
)

# Basic info ----------------------------------------------------------------
# only players with a future value (the ones the app lets you select), so each name is
# one player. Seasons of value added are in order, and total value is value added to
# date plus future value (which is already discounted)

sva_cols <- sort(str_subset(names(player_total_value), "^sva_\\d{4}$"))

basic_info_df <- player_total_value |>
  select(player_id, name, position, birth_date, all_of(sva_cols), future_value) |>
  left_join(
    player_info |> select(player_id, years_exp),
    by = join_by(player_id)
  ) |>
  mutate(
    across(c(all_of(sva_cols), future_value), ~ replace_na(.x, 0)),
    total_value = rowSums(pick(all_of(sva_cols))) + future_value,
    age = time_length(interval(birth_date, today()), "years")
  ) |>
  select(
    player_id, # for the app's lookups (not shown)
    name,
    position,
    birth_date,
    age,
    years_exp,
    all_of(sva_cols),
    future_value,
    total_value
  ) |>
  rename_with(~ str_replace(.x, "sva_", "total value added "), all_of(sva_cols))

write_csv(basic_info_df, here("Shiny/Saved Files/basic_info_df.csv"))

# Projected value added by season ----------------------------------------------
# one row per player and season with the projection quantiles (sorted, so credible
# intervals are never inverted) and expected_tva, the mean of the quantiles floored at
# 0 (a player below replacement would be benched), as future value uses. During the
# season the current season is the realized value added so far plus the projection
# scaled by the share left (1 - p), the same split future value uses.

realized_now <- season_value_added |>
  filter(season == season_context$season) |>
  select(player_id, total_value_added)

plot_future_value_df <- player_simulations |>
  imap(\(sims, season) {
    season <- as.numeric(season)
    q_cols <- str_subset(names(sims), "^proj_tva_")

    q <- as.matrix(sims[q_cols])
    q[] <- t(apply(q, 1, sort, na.last = TRUE))
    expected <- rowMeans(pmax(q, 0))

    if (season == season_context$season) {
      realized <- realized_now$total_value_added[
        match(as.character(sims$player_id), realized_now$player_id)
      ] |>
        replace_na(0) # not started this season
      remaining <- 1 - season_context$p
      q <- q * remaining + realized
      expected <- expected * remaining + realized
    }

    sims |>
      select(name, player_id) |>
      bind_cols(as_tibble(q)) |>
      mutate(expected_tva = expected, season = season)
  }) |>
  list_rbind()

write_csv(
  plot_future_value_df,
  here("Shiny/Saved Files/plot_future_value_df.csv")
)

write_csv(value_added, here("Shiny/Saved Files/value_added.csv"))

# Examples (app functions in Shiny/Script Support.R) -----------------------------
# plot_future_value("4984") # Josh Allen (functions take player_ids)
# value_added |> tabulate_realized_value("11560", 2024, shiny = FALSE) # Caleb Williams
# value_added |> weekly_results("8138", 2024) # James Cook
