# Relocating Files

# this script moves some files from data folder in Saved Files folder, so it can be inside the app
suppressPackageStartupMessages(library("here"))

message("begin Relocating Files...")

load_path <- "Shiny/Saved Files"

# load from data folder
player_total_value <- read_csv(
  here("Data", "player_total_value.csv"),
  show_col_types = FALSE
)
users <- read_csv(here("Data", "users.csv"), show_col_types = FALSE)
season_value_added <- read_csv(here("Data/sva.csv"), show_col_types = FALSE)
avatar <- read_csv(here("Data", "avatar.csv"), show_col_types = FALSE)
marginal_transaction_value <- read_csv(
  here("Data/marginal_transaction_value.csv"),
  show_col_types = FALSE
)
toy_tva_plot_data <- read_csv(
  here("Data/toy_tva_plot_data.csv"),
  show_col_types = FALSE
)
toy_ktc_plot_data <- read_csv(
  here("Data/toy_ktc_plot_data.csv"),
  show_col_types = FALSE
)
draft_fit_plot <- read_csv(
  here("Data/draft_fit_plot.csv"),
  show_col_types = FALSE
)
season_dates <- read_csv(here("Data/season_dates.csv"), show_col_types = FALSE)

# load into Saved Files folder
write_csv(player_total_value, here(load_path, "player_total_value.csv"))
write_csv(users, here(load_path, "users.csv"))
write_csv(season_value_added, here(load_path, "season_value_added.csv"))
write_csv(avatar, here(load_path, "avatar.csv"))
write_csv(
  marginal_transaction_value,
  here(load_path, "marginal_transaction_value.csv")
)
write_csv(toy_tva_plot_data, here(load_path, "toy_tva_plot_data.csv"))
write_csv(toy_ktc_plot_data, here(load_path, "toy_ktc_plot_data.csv"))
write_csv(draft_fit_plot, here(load_path, "draft_fit_plot.csv"))
write_csv(season_dates, here(load_path, "season_dates.csv"))
