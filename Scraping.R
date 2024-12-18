# Scraping

library("nflfastR")
library("tidyverse")
library("jsonlite")
library("httr")

# load box score data from NFL 2024
box_score <- load_player_stats(seasons = 2024)


# Scrape Sleeper Stuff ----------------------------------------------------
# https://docs.sleeper.com

# enter url, return parsed object 
parse_api <- function(url){
  response <- GET(url)
  content <- content(response, "text")
  parsed <- fromJSON(content) %>%
    as_tibble()
  
  return(parsed)
}

# each team's players and starters


# matchups



# expected scores for each player



# trades



# drafts

# Get all drafts urls for a league
draft_urls <- parse_api("https://api.sleeper.app/v1/league/1066207868321370112/drafts") %>%
  transmute(url = str_c("https://api.sleeper.app/v1/draft/", draft_id, "/picks")) %>%
  pull()

# Get all draft trade urls for a league
draft_trades <- draft_urls %>% str_replace("/picks", "/traded_picks")

# Get picks in a draft
draft_picks <- map(draft_urls, parse_api)

# Get trades in a draft
draft_trades <- map(draft_trades, parse_api)


