# Workflow for updating data

# This R script details an orderly procedure for updating the data

# Player Value Added (this file runs scraping.R automatically) ~45 seconds
source(here::here("Modeling", "Player Value Added.R"))
misty::restart() #clear memory

# with relative frequency, will need to rerun player_simulations to account for new ktc value
# fair warning it will take ~5 minutes
source(here::here("Modeling", "Player Total Value.R"))
misty::restart() #clear memory

# with relative frequency, will need to rerun Future Standings.R to update projections of the end of season results
# fair warning, it will take ~8 minutes
tictoc::tic()
source(here::here("Modeling", "Future Standings.R"))
tictoc::toc()
misty::restart()

# Individual Players
source(here::here("Scripts", "Individual Players.R"))

# Fantasy Teams
source(here::here("Scripts", "Fantasy Teams.R"))

# Trade Machine
source(here::here("Scripts", "Trade Machine.R"))

# Matchups
source(here::here("Scripts", "Matchups.R"))

# Future Stqndings Script
source(here::here("Scripts", "Future Standings Script.R"))

# Team Rankings
source(here::here("Scripts", "Team Rankings.R"))

# History
source(here::here("Scripts", "History.R"))

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


