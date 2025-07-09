# Workflow for updating data

# This R script details an orderly procedure for updating the data
library("here")

# First Data Acquisition and Manipulation
# source(here("FantasyDynasty", "Data Manipulation", "Scraping.R"))

# misty::restart() #clear memory

# Player Value Added (this file runs scraping.R automatically)
source(here("Modeling", "Player Value Added.R"))

misty::restart() #clear memory

# with relative frequency, will need to rerun player_simulations to account for new ktc value
# fair warning it will take 7+ minutes
source(here("Modeling", "Player Total Value Functions.R"))

# when future standings is allowed to vary with season, add that here

# Individual Players
library("here")
source(here("Scripts", "Individual Players.R"))

# Fantasy Teams
source(here("Scripts", "Fantasy Teams.R"))

misty::restart() #clear memory


# run app
rsconnect::deployApp(appName = "fantasy-dashboard",
                     account = "calebskinner",
                     forceUpdate = TRUE,
                     appPrimaryDoc = "app.R",
                     appFiles = NULL)


