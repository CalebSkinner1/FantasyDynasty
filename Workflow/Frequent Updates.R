# Frequent Updates
# Weekly refresh: scrape, update the models, rebuild the app's tables, and deploy.
# Every Tuesday at 10:00 this runs unattended (Workflow/Scheduled/run_frequent_updates.sh),
# and it can still be run by hand. Each step runs in its own R process (see run_step() in
# Workflow Support.R) and the run stops at the first failure, so a broken step is never deployed.
# Times below are rough; each step's time is printed in the log.

source(here::here("Workflow", "Workflow Support.R"))

# Data and models ---------------------------------------------------------

# Scraping: box scores, Sleeper data, projections, KTC (~40 seconds)
run_step("Data Manipulation/Scraping.R")

# Player Value Added (reads the files Scraping.R writes) (~5 seconds)
run_step("Modeling/Player Value Added.R")

# Player Total Value: today's future values from the saved models (~20 seconds)
run_step("Modeling/Player Total Value.R")

# Future Standings: 50,000 simulations of the next three seasons (~15 seconds)
run_step("Modeling/Future Standings.R")

# App tables (written to Shiny/Saved Files) ----------------------------------

# Individual Players
run_step("Scripts/Individual Players.R")

# Pick Values (expected value of every draft pick; needs Future Standings)
run_step("Scripts/Pick Values.R")

# Draft Grades
run_step("Scripts/Draft Grades.R")

# Transaction Grades (also the value of a roster spot, used by Trade Grades)
run_step("Scripts/Transaction Grades.R")

# Trade Grades (needs Pick Values and Transaction Grades)
run_step("Scripts/Trade Grades.R")

# Fantasy Teams (needs Pick Values and the three grades)
run_step("Scripts/Fantasy Teams.R")

# Trade Machine
run_step("Scripts/Trade Machine.R")

# Matchups
run_step("Scripts/Matchups.R")

# Future Standings Script
run_step("Scripts/Future Standings Script.R")

# Team Rankings
run_step("Scripts/Team Rankings.R")

# History
run_step("Scripts/History.R")

# Deploy -------------------------------------------------------------------

# the date shown on the app's home page
write.csv(
  data.frame(updated = as.character(Sys.Date())),
  here::here("Shiny/Saved Files/last_updated.csv"),
  row.names = FALSE
)

message("\n== deploying the app")
rsconnect::deployApp(
  appDir = here::here("Shiny"),
  appName = "fantasy-dashboard",
  account = "calebskinner",
  forceUpdate = TRUE,
  appPrimaryDoc = "app.R",
  appFiles = NULL,
  launch.browser = FALSE
)
