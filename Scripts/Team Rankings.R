# Team Rankings
# each team's peak and low Elo ratings and composite rating, and the total future value of
# each team's assets, for the Team Rankings page. The weekly Elo ratings come from
# Matchups.R, so it runs after it.
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
})

message("begin computing Team Rankings...")

grab_team_assets_df <- read_csv(
  here("Shiny/Saved Files/grab_team_assets_df.csv"),
  show_col_types = FALSE
)
users <- read_csv(here("Shiny/Saved Files/users.csv"), show_col_types = FALSE) |>
  select(roster_id, display_name)

# Elo Rankings ------------------------------------------------------------
# weekly ratings from Matchups.R (see there for how they're computed)

weekly_elo <- read_csv(here("Shiny/Saved Files/weekly_elo.csv"), show_col_types = FALSE)

# peak and low ratings and the week each happened, and the composite rating: the average
# distance from 1500 after each played week (the week 0 starting ratings are left out).
# peak_order and low_order (the week's position on the Elo chart) let the app sort the
# week columns by when they happened; the app hides them.
when <- function(date) {
  str_c("20", str_sub(date, 1, 2), " week ", str_remove(date, "^\\d+w"))
}

total_success <- weekly_elo |>
  summarize(
    composite_rating = round(mean(elo[!str_detect(date, "w0$")] - 1500), digits = 2),
    peak = round(max(elo), 1),
    peak_week = when(date[which.max(elo)]),
    low = round(min(elo), 1),
    low_week = when(date[which.min(elo)]),
    peak_order = date_hide[which.max(elo)],
    low_order = date_hide[which.min(elo)],
    .by = team
  ) |>
  arrange(desc(composite_rating))

# Future Value Assets -----------------------------------------------------

all_assets_summary_df <- grab_team_assets_df |>
  summarize(total_future_value = sum(future_value), .by = roster_id) |>
  left_join(users, by = join_by(roster_id)) |>
  arrange(desc(total_future_value)) |>
  select(team = display_name, total_future_value)

# dfs to save -------------------------------------------------------------

write_csv(
  all_assets_summary_df,
  here("Shiny/Saved Files/all_assets_summary_df.csv")
)
write_csv(total_success, here("Shiny/Saved Files/total_success.csv"))
