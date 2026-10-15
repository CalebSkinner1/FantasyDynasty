# Frequent Updates
# Weekly refresh: scrape, update the models, rebuild the app's tables, and deploy.
# Every Tuesday at 10:00 this runs unattended (Workflow/Scheduled/run_frequent_updates.sh),
# and it can still be run by hand. Each step runs in its own R process (see run_step() in
# Workflow Support.R) and the run stops at the first failure, so a broken step is never deployed.
# Times below are rough; each step's time is printed in the log.

source(here::here("Workflow", "Workflow Support.R"))

# Data and models ---------------------------------------------------------

# Scraping: box scores, Sleeper data, projections, KTC (~1 minute)
run_step("Data Manipulation/Scraping.R")

# Player Value Added (reads the files Scraping.R writes) (~5 seconds)
run_step("Modeling/Player Value Added.R")

# Player Total Value: today's future values from the saved models (a few minutes)
run_step("Modeling/Player Total Value.R")

# Future Standings: 50,000 simulations of the next three seasons (~1-2 minutes)
run_step("Modeling/Future Standings.R")

# App tables (written to Shiny/Saved Files) ----------------------------------

# Individual Players
run_step("Scripts/Individual Players.R")

# Fantasy Teams (also runs Draft Grades, Trade Grades, and Transaction Grades, ~15 seconds)
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
