# Yearly Updates
# Two checklists, one after the season and one before the next. Do the manual steps first,
# then set `checklist` and source this file to run that checklist's scripts in order. Each
# script runs in its own R process (see run_step() in Workflow Support.R), and the run stops
# at the first failure.

source(here::here("Workflow", "Workflow Support.R"))

checklist <- "none" # "after season" or "before season"

# A. After the season ----------------------------------------------------------
# When: January, after the championship week has been scored.
#
# Manual:
#   [ ] The last Frequent Updates run happened after the championship (final week, playoff
#       results, and end-of-season transactions are in).
#   [ ] If the league has set next year's rookie draft order in Sleeper, Scraping.R picks it up
#       automatically (Data/derived/future_draft_picks.csv). Otherwise Future Standings uses
#       the final standings (last place picks first).
#
# Scripts:
#   1. Draft Pick Value: the latest rookie class has finished its first season, so its realized
#      value replaces the in-season projection. Refits the pick values.
#   2. Frequent Updates: scrape, value added, total value (now simulating from next season),
#      Future Standings (now starting from next season), app tables, and deploy.
after_season <- function() {
  run_step("Modeling/Draft Pick Value.R")
  source(here::here("Workflow", "Frequent Updates.R"))
}

# B. Before the season -----------------------------------------------------------
# When: August, after the rookie draft and once Sleeper has renewed the league.
#
# Manual:
#   [ ] Data Manipulation/Scraping.R: add the new season's league id to `league_ids`
#       (Sleeper gives each renewed season a new id). Everything else rolls over from it.
#   [ ] Modeling/Player Total Value.R: add the new training year to `ktc_begin_end_dates`
#       (pre = last year's post date, post = the KTC file closest to the new season's start;
#       files are in Data/raw/ktc).
#   [ ] Check that this year's rookies (drafted in Sleeper) appear in
#       Data/derived/current_roster.csv after scraping.
#
# Scripts:
#   1. Scraping and Player Value Added for the new season.
#   2. Player Total Value: retrain and re-tune the BART models with last season added, then
#      recompute future value over time for every KTC date with the new models (slow).
#   3. Draft Pick Value: refit with the new draft class.
#   4. Frequent Updates: Future Standings, app tables, and deploy.
before_season <- function() {
  run_step("Data Manipulation/Scraping.R")
  run_step("Modeling/Player Value Added.R")
  run_step(
    "Modeling/Player Total Value.R",
    options = list(
      fantasy.train_models = TRUE,
      fantasy.tune_models = TRUE,
      fantasy.recompute_future_value_time = TRUE
    )
  )
  run_step("Modeling/Draft Pick Value.R")
  source(here::here("Workflow", "Frequent Updates.R"))
}

# Run ----------------------------------------------------------------------

switch(
  checklist,
  "after season" = after_season(),
  "before season" = before_season(),
  "none" = message("Set `checklist` to \"after season\" or \"before season\" to run one."),
  stop("unknown checklist: ", checklist)
)
