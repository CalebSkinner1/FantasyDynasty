# Workflow for updating data

# This R script details an orderly procedure for updating the data
library("here")

# First Data Acquisition and Manipulation
# source(here("Data Manipulation", "Scraping.R"))

# misty::restart() #clear memory

# Player Value Added (this file runs scraping.R automatically)
source(here("Modeling", "Player Value Added.R"))

misty::restart() #clear memory

# with relative frequency, will need to rerun player_simulations to account for new ktc value
# fair warning it will take 14+ minutes
library("here")
source(here("Modeling", "Player Total Value.R"))

# when future standings is allowed to vary with season, add that here
misty::restart() #clear memory

# Individual Players
library("here")
source(here("Scripts", "Individual Players.R"))

# Fantasy Teams
source(here("Scripts", "Fantasy Teams.R"))

# Matchups
source(here("Scripts", "Matchups.R"))

# Future Stqndings Script
source(here("Scripts", "Future Standings Script.R"))

# Team Rankings
source(here("Scripts", "Team Rankings.R"))

# History
source(here("Scripts", "History.R"))

misty::restart() #clear memory

if (interactive()) {
  setwd(here::here("Shiny"))
}

# run app
rsconnect::deployApp(appName = "fantasy-dashboard",
                     account = "calebskinner",
                     forceUpdate = TRUE,
                     appPrimaryDoc = "app.R",
                     appFiles = NULL)


