# Yearly Updates
library("here")
# this file specifies some of the tasks that may need to happen intermittently or yearly

# 1. load player_info and commented out files in Scrape.R account for league reset
# 2. rerun models
# future value
source(here("Modeling", "Player Total Value.R"))

# 3. ensure draft picks are loaded properly

# draft pick value
source(here("Modeling", "Draft Pick Value.R"))

# future standings
source(here("Modeling", "Future Standings.R"))

