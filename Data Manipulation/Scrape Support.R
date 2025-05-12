# Scraping Functions

library("tidyverse"); theme_set(theme_minimal())
library("jsonlite")
library("httr")
library("rvest")
library("data.table")

# Misc --------------------------------------------------------------------
`%!in%` = Negate(`%in%`)

# syncs up name styles
name_correction <- function(df){
  df %>%
    mutate(
      name = case_when(
        name == "Brian Robinson" ~ "Brian Robinson Jr.",
        name == "Marvin Harrison" ~ "Marvin Harrison Jr.",
        name == "Michael Pittman" ~ "Michael Pittman Jr.",
        name == "DJ Moore" ~ "D.J. Moore",
        name == "Josh Palmer" ~ "Joshua Palmer",
        name == "Demario Douglas" ~ "DeMario Douglas",
        name == "Brian Thomas" ~ "Brian Thomas Jr.",
        name == "Chig Okonkwo" ~ "Chigoziem Okonkwo",
        name == "ARI" ~ "Arizona Cardinals",
        name == "ATL" ~ "Atlanta Falcons",
        name == "BAL" ~ "Baltimore Ravens",
        name == "BUF" ~ "Buffalo Bills",
        name == "CAR" ~ "Carolina Panthers",
        name == "CHI" ~ "Chicago Bears",
        name == "CIN" ~ "Cincinnati Bengals",
        name == "CLE" ~ "Cleveland Browns",
        name == "DAL" ~ "Dallas Cowboys",
        name == "DEN" ~ "Denver Broncos",
        name == "DET" ~ "Detroit Lions",
        name == "GB" ~ "Green Bay Packers",
        name == "HOU" ~ "Houston Texans",
        name == "IND" ~ "Indianapolis Colts",
        name == "JAX" ~ "Jacksonville Jaguars",
        name == "KC" ~ "Kansas City Chiefs",
        name == "LAR" ~ "Los Angeles Rams",
        name == "LAC" ~ "Los Angeles Chargers",
        name == "LV" ~ "Las Vegas Raiders",
        name == "MIA" ~ "Miami Dolphins",
        name == "MIN" ~ "Minnesota Vikings",
        name == "NE" ~ "New England Patriots",
        name == "NO" ~ "New Orleans Saints",
        name == "NYG" ~ "New York Giants",
        name == "NYJ" ~ "New York Jets",
        name == "PHI" ~ "Philadelphia Eagles",
        name == "PIT" ~ "Pittsburgh Steelers",
        name == "SEA" ~ "Seattle Seahawks",
        name == "SF" ~ "San Francisco 49ers",
        name == "TB" ~ "Tampa Bay Buccaneers",
        name == "TEN" ~ "Tennessee Titans",
        name == "WAS" ~ "Washington Commanders",
        name == "Tyrone Tracy" ~ "Tyrone Tracy Jr.",
        name == "Pierre Strong" ~ "Pierre Strong Jr.",
        name == "Chris Rodriguez" ~ "Chris Rodriguez Jr.",
        name == "Marvin Mims" ~ "Marvin Mims Jr.",
        name == "Kenneth Walker" ~ "Kenneth Walker III",
        name == "Deebo Samuel" ~ "Deebo Samuel Sr.",
        name == "Michael Penix" ~ "Michael Penix Jr.",
        name == "Quentin Johnson" ~ "Quentin Johnston",
        name == "Ezekiel Elliot" ~ "Ezekiel Elliott",
        name == "AJ Dillon" ~ "A.J. Dillon",
        name == "DJ Chark" ~ "D.J. Chark Jr.",
        name == "D.J. Chark" ~ "D.J. Chark Jr.",
        name == "Odell Beckham" ~ "Odell Beckham Jr.",
        name == "Erick All" ~ "Erick All Jr.",
        name == "John Metchie" ~ "John Metchie III",
        name == "Jeff Wilson" ~ "Jeff Wilson Jr.",
        name == "Juju Smith-Schuster" ~ "JuJu Smith-Schuster",
        name == "Calvin Austin" ~ "Calvin Austin III",
        name == "Irv Smith" ~ "Irv Smith Jr.",
        name == "Donald Parham" ~ "Donald Parham Jr.",
        name == "Frank Gore" ~ "Frank Gore Jr.", #player_id == "11573"
        name == "Terrace Marshall" ~ "Terrace Marshall Jr.",
        name == "Marquise Brown" ~ "Hollywood Brown",
        name == "Gabriel Davis" ~ "Gabe Davis",
        name == "Jeffery Wilson" ~ "Jeff Wilson Jr.",
        .default = name))}

# Sleeper API -------------------------------------------------------------
# enter url, return parsed object 
parse_api <- function(url){
  response <- GET(url)
  content <- content(response, "text")
  parsed <- fromJSON(content) %>%
    as_tibble()
  
  return(parsed)
}

# leave as list
parse_api_list <- function(url){
  response <- GET(url)
  content <- content(response, "text")
  parsed <- fromJSON(content)
  
  return(parsed)
}

# Scrape Projections ------------------------------------------------------

# player projections (qb (10), rb (20), wr (30), te (40))
grab_projection <- function(fftoday_page_html, wk){
  table <- read_html(fftoday_page_html) %>%
    html_element("body") %>%
    html_element("center") %>%
    html_elements("table") %>%
    pluck(2) %>%
    html_element(".bodycontent") %>%
    html_elements("table") %>% 
    pluck(6) %>%
    html_elements("table") %>%
    html_table()
  
  table[[1]][,-1][-c(1:2),] %>%
    as_tibble() %>%
    select(X2, last_col()) %>%
    rename(name = "X2") %>%
    rename_with(~paste0("projection"), .cols = last_col()) %>%
    mutate(
      projection = as.numeric(projection),
      week = wk) %>%
    return()
}

# grab_projection("https://www.fftoday.com/rankings/playerwkproj.php?Season=2024&GameWeek=1&PosID=20&LeagueID=208518", 1)
# grab_projection("https://www.fftoday.com/rankings/playerwkproj.php?Season=2024&GameWeek=1&PosID=20&LeagueID=208518&order_by=FFPts&sort_order=DESC&cur_page=1", 1)

# position rankings (k, dst)
grab_rankings <- function(fftoday_page_html, wk){
  all <- read_html(fftoday_page_html) %>%
    html_element("body") %>%
    html_element("center") %>%
    html_elements("table") %>%
    pluck(2) %>%
    html_element(".bodycontent") %>%
    html_elements("table") %>% 
    pluck(4)
  
  kickers <- all %>%
    html_elements("table") %>%
    pluck(2) %>%
    html_table() %>%
    select(X2, X3) %>%
    slice(-c(1,2)) %>%
    filter(!str_detect(X2, "Tier")) %>%
    rename(projection = "X2",
           name = "X3") %>%
    relocate(name) %>%
    mutate(
      week = wk,
      projection = 33 - as.numeric(projection))
  
  defenses <- all %>%
    html_elements("table") %>%
    pluck(4) %>%
    html_table() %>%
    select(X2, X3) %>%
    slice(-c(1,2)) %>%
    filter(!str_detect(X2, "Tier")) %>%
    rename(projection = "X2",
           name = "X3") %>%
    relocate(name) %>%
    mutate(
      week = wk,
      projection = 33 - as.numeric(projection))
  
  list(kickers, defenses) %>%
    return()
}

# grab_rankings("https://www.fftoday.com/rankings/playerwkrank.php?GameWeek=1&PosID=80&LeagueID=208518", 1)

# combine them into one function
# make sure url doesn't change for some reason
# QB URL: "https://www.fftoday.com/rankings/playerwkproj.php?Season=2024&GameWeek=1&PosID=10&LeagueID=208518"
combine_week <- function(week, season){
  kick_def_rankings <- str_c("https://www.fftoday.com/rankings/playerwkrank.php?Season=", season, "&GGameWeek=",
                             week, "&PosID=80&LeagueID=208518") %>%
    grab_rankings(week)
  
  
  rb_wr <- map(c(20,30), ~str_c("https://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=",
                                week, "&PosID=", .x, "&LeagueID=208518&order_by=FFPts&sort_order=DESC&cur_page=1") %>%
                 grab_projection(week)) %>%
    append(kick_def_rankings)
  
  map(c(10, 20, 30, 40), ~str_c("https://www.fftoday.com/rankings/playerwkproj.php?Season=", season, "&GameWeek=",
                                week, "&PosID=", .x, "&LeagueID=208518") %>%
        grab_projection(week)) %>%
    append(rb_wr) %>%
    rbindlist() %>%
    as_tibble() %>%
    return()
}

# Scrape Future Value -----------------------------------------------------

# scrape future value from ktc
player_value <- function(ktc_page_html){
  players <- read_html(ktc_page_html) %>%
    html_element("body") %>%
    html_element("main") %>%
    html_elements("div") %>%
    html_elements(".onePlayer") %>%
    html_element(".single-ranking-wrapper") %>%
    html_element(".single-ranking")
  
  # grabs player name
  name <- players %>%
    html_element(".player-name") %>%
    html_node("p") %>%
    html_node("a") %>%
    html_text2()
  
  # grabs player value
  value <- players %>%
    html_element(".value") %>%
    html_node("p") %>%
    html_text2()
  
  # return in tibble
  tibble(
    name = name,
    value = as.numeric(value)) %>%
    return()
  
}

