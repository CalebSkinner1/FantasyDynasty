# Workflow for updating data

# This R script details an orderly procedure for updating the data
library("here")

# First Data Acquisition and Manipulation
# source(here("FantasyDynasty", "Data Manipulation", "Scraping.R"))

# misty::restart() #clear memory

# Player Value Added (this file runs scraping.R automatically)
source(here("FantasyDynasty", "Modeling", "Player Value Added.R"))

misty::restart() #clear memory

# when future standings is allowed to vary with season, add that here

# Individual Players
library("here")
source(here("FantasyDynasty", "Scripts", "Individual Players.R"))

# Fantasy Teams
source(here("FantasyDynasty", "Scripts", "Fantasy Teams.R"))

misty::restart() #clear memory