# Load the app's data
# everything the app shows, bundled into one file by Workflow/Build App Data.R (see there
# for each table and the script that writes it), unpacked here as one object per table

# app.R sets app_dir (the Shiny folder); default to the working directory
if (!exists("app_dir")) {
  app_dir <- "."
}

# load necessary functions
source(file.path(app_dir, "Script Support.R"))

list2env(readRDS(file.path(app_dir, "app_data.rds")), envir = environment())
