# Shiny App

# doesn't work for current version of R, so I backed up to 4.4.2

library("shiny")
library("shinydashboard")
library("DT")

# only if running on local machine
print("App starting...") # for logs
if (interactive()) {
  setwd(here::here("Shiny"))
}

# Source the function files in the folder
source("app_call_data.R")
print("Loaded functions") # for logs

source("ui.R")
source("server.R")

# Run the App
shinyApp(ui, server)


