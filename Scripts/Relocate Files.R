# Relocating Files

# this script moves some files from data folder in Saved Files folder, so it can be inside the app
library("here")

load_path <- "Shiny/Saved Files"

# load from data folder
player_total_value <- read_csv(here("Data", "player_total_value.csv"), show_col_types = FALSE)
users <- read_csv(here("Data", "users.csv"), show_col_types = FALSE)
season_value_added <- read_csv(here("Data/sva.csv"), show_col_types = FALSE)
avatar <- read_csv(here("Data", "avatar.csv"), show_col_types = FALSE)

# load into Saved Files folder
write_csv(player_total_value, here(load_path, "player_total_value.csv"))
write_csv(users, here(load_path, "users.csv"))
write_csv(season_value_added, here(load_path, "season_value_added.csv"))
write_csv(avatar, here(load_path, "avatar.csv"))
