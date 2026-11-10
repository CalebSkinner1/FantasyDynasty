# Frequent Updates
# Daily refresh: scrape, update the models, rebuild the app's tables, and deploy.
# Every day at 10:00 this runs unattended (Workflow/Scheduled/run_frequent_updates.sh),
# and it can still be run by hand. Each step runs in its own R process (see run_step() in
# Workflow Support.R) and the run stops at the first failure, so a broken step is never deployed.
# Times below are rough; each step's time is printed in the log.

source(here::here("Workflow", "Workflow Support.R"))

# Data and models ---------------------------------------------------------

# Scraping: box scores, Sleeper data, projections, KTC (~40 seconds)
run_step("Data Manipulation/Scraping.R")

# Player Value Added (reads the files Scraping.R writes) (~5 seconds)
run_step("Modeling/Player Value Added.R")

# Matchups: records and Elo ratings (Elo is needed by the Win Probability Blend and
# Future Standings)
run_step("Scripts/Matchups.R")

# Player Total Value: today's future values from the saved models (~20 seconds)
run_step("Modeling/Player Total Value.R")

# Win Probability Blend: the weight on the model vs. Elo in this week's win probabilities
# (seconds; a minute or two the first time)
run_step("Modeling/Win Probability Blend.R")

# Future Standings: 50,000 simulations of the next three seasons, and 500,000 of this
# season for this week's games (~1 minute)
run_step("Modeling/Future Standings.R")

# App tables (written to Shiny/Saved Files, then bundled for the app) ---------------

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

# Future Standings Script
run_step("Scripts/Future Standings Script.R")

# Team Rankings
run_step("Scripts/Team Rankings.R")

# Current Matchups (needs Future Standings)
run_step("Scripts/Current Matchups.R")

# History
run_step("Scripts/History.R")

# Deploy -------------------------------------------------------------------

# bundle the app's tables into Shiny/app_data.rds, all the app loads at startup (~5 seconds)
run_step("Workflow/Build App Data.R")

deploy_app <- function() {
  rsconnect::deployApp(
    appDir = here::here("Shiny"),
    appName = "fantasy-dashboard",
    account = "calebskinner",
    forceUpdate = TRUE,
    appPrimaryDoc = "app.R",
    # only what the app runs on: its code and the bundled data (not Saved Files)
    appFiles = c("app.R", "Script Support.R", "app_call_data.R", "app_data.rds"),
    launch.browser = FALSE
  )
}

# a deploy that fails (most often a timeout while shinyapps.io builds the app) is tried once
# more after two minutes, enough for a build already under way to finish; a second failure
# stops the run
message("\n== deploying the app")
tryCatch(
  deploy_app(),
  error = function(e) {
    message(
      "\n== deploy failed (", conditionMessage(e), "); ",
      "trying once more in 2 minutes"
    )
    Sys.sleep(120)
    deploy_app()
  }
)
