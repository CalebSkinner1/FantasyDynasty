# Yearly Updates
library("here")
# this file specifies some of the tasks that may need to happen intermittently or yearly

# 1. load player_info and commented out files in Scrape.R
# 2. rerun models
# future value
source(here("FantasyDynasty", "Modeling", "Player Total Value.R"))

# draft pick value
source(here("FantasyDynasty", "Modeling", "Draft Pick Value.R"))

# future standings
source(here("FantasyDynasty", "Modeling", "Future Standings.R"))

# 3. ensure draft picks are loaded properly