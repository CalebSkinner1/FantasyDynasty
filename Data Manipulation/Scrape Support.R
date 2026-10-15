# Scraping Functions

suppressPackageStartupMessages({
  library("tidyverse")
  theme_set(theme_minimal())
  library("jsonlite")
  library("httr")
  library("rvest")
  library("data.table")
})

# Misc --------------------------------------------------------------------
`%!in%` <- Negate(`%in%`)

# syncs up name styles
name_correction <- function(df) {
  df |>
    mutate(
      name = case_when(
        name == "Nyheim Miller-Hines" ~ "Nyheim Hines",
        name == "Kayton Allen" ~ "Kaytron Allen",
        name == "Kenneth Gainwell" ~ "Kenny Gainwell",
        name == "Ezekiel Elliot" ~ "Ezekiel Elliott",
        name == "Juju Smith-Schuster" ~ "JuJu Smith-Schuster",
        name == "Quentin Johnson" ~ "Quentin Johnston",
        name == "Matthew Hibner" ~ "Matt Hibner",
        name == "Chris Brazzell" ~ "Chris Brazzell II",
        name == "Andrew Ogletree" ~ "Drew Ogletree",
        name == "Omar Cooper" ~ "Omar Cooper Jr.",
        name == "Mike Washington" ~ "Mike Washington Jr.",
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
        name == "Cam Ward" ~ "Cameron Ward",
        name == "Ollie Gordon II" ~ "Ollie Gordon",
        name == "Luther Burden III" ~ "Luther Burden",
        name == "Dont'e Thornton Jr." ~ "Dont'e Thornton",
        name == "Harold Fannin Jr." ~ "Harold Fannin",
        name == "Andres Borregales" ~ "Andy Borregales",
        name == "Tre' Harris" ~ "Tre Harris",
        name == "Zonovan Knight" ~ "Bam Knight", # bruh
        name == "Oronde Gadsden II" ~ "Oronde Gadsden",
        name == "D.J. Turner" ~ "DJ Turner",
        name == "John Parker Romo" ~ "Parker Romo",
        name == "Kevin Austin Jr." ~ "Kevin Austin",
        name == "Mike Badgley" ~ "Michael Badgley",
        name == "Mitchell Tinsley" ~ "Mitch Tinsley",
        .default = name
      )
    )
}

# Sleeper API -------------------------------------------------------------
# enter url, return parsed object
parse_api <- function(url) {
  response <- GET(url)
  content <- content(response, "text")
  parsed <- fromJSON(content) |>
    as_tibble()

  return(parsed)
}

# leave as list
parse_api_list <- function(url) {
  response <- GET(url)
  content <- content(response, "text")
  parsed <- fromJSON(content)

  return(parsed)
}

# league settings: scoring weights and starting lineup slots
fetch_sleeper_league <- function(league_id) {
  league <- parse_api_list(str_c("https://api.sleeper.app/v1/league/", league_id))

  list(
    scoring = tibble(
      stat = names(league$scoring_settings),
      weight = as.numeric(unlist(league$scoring_settings))
    ),
    roster_positions = unlist(league$roster_positions)
  )
}

# weekly stats for every NFL player (unofficial Sleeper endpoint), returned long:
# player_id, stat, value. Returns NULL if the endpoint is unavailable.
fetch_sleeper_stats <- function(season, week) {
  urls <- c(
    str_c("https://api.sleeper.app/v1/stats/nfl/regular/", season, "/", week),
    str_c(
      "https://api.sleeper.com/stats/nfl/",
      season,
      "/",
      week,
      "?season_type=regular"
    )
  )

  for (url in urls) {
    parsed <- tryCatch(
      {
        resp <- httr::RETRY(
          "GET",
          url,
          times = 3,
          terminate_on = c(400, 403, 404),
          quiet = TRUE
        )
        if (httr::status_code(resp) != 200) {
          NULL
        } else {
          fromJSON(
            httr::content(resp, "text", encoding = "UTF-8"),
            simplifyVector = FALSE
          )
        }
      },
      error = function(e) NULL
    )

    if (length(parsed) == 0) {
      next
    }

    # api.sleeper.app: object keyed by player_id
    # api.sleeper.com: array of records with player_id and stats
    if (!is.null(names(parsed))) {
      ids <- names(parsed)
      stat_lists <- parsed
    } else {
      ids <- map_chr(parsed, ~ as.character(.x$player_id %||% NA_character_))
      stat_lists <- map(parsed, ~ .x$stats)
    }

    # keep scalar numeric stats only
    stat_lists <- map(
      stat_lists,
      ~ keep(.x, \(v) is.numeric(v) && length(v) == 1)
    )

    out <- tibble(
      player_id = rep(ids, lengths(stat_lists)),
      stat = unlist(map(stat_lists, names), use.names = FALSE),
      value = unlist(stat_lists, use.names = FALSE)
    ) |>
      filter(!is.na(player_id))

    if (nrow(out) > 0) {
      return(out)
    }
  }
  NULL
}

# league points from long stats and the league's scoring weights
score_sleeper_stats <- function(stats_long, scoring) {
  stats_long |>
    inner_join(scoring, by = join_by(stat)) |>
    summarize(pts = round(sum(value * weight), 2), .by = player_id)
}

# Scrape Projections ------------------------------------------------------

fetch_html <- function(url, pause = 1.5, tries = 4) {
  Sys.sleep(pause)
  resp <- httr::RETRY(
    "GET",
    url,
    httr::user_agent("Mozilla/5.0 (personal fantasy league script)"),
    times = tries,
    pause_base = 2,
    terminate_on = 404,
    quiet = TRUE
  )
  httr::stop_for_status(resp, task = paste("fetch", url))
  read_html(httr::content(resp, "text", encoding = "UTF-8"))
}

# player projections (qb (10), rb (20), wr (30), te (40))
# pos is the position of the page (known from PosID), stored so projections can be
# joined to player_info on name and position
grab_projection <- function(fftoday_page_html, wk, pos) {
  table <- fetch_html(fftoday_page_html) |>
    html_element("body") |>
    html_element("center") |>
    html_elements("table") |>
    pluck(2) |>
    html_element(".bodycontent") |>
    html_elements("table") |>
    pluck(6) |>
    html_elements("table") |>
    html_table()

  table[[1]][, -1][-c(1:2), ] |>
    as_tibble() |>
    select(X2, last_col()) |>
    rename(name = "X2") |>
    rename_with(~ paste0("projection"), .cols = last_col()) |>
    mutate(
      projection = as.numeric(projection),
      week = wk,
      position = pos
    )
}

# grab_projection("https://www.fftoday.com/rankings/playerwkproj.php?Season=2024&GameWeek=1&PosID=20&LeagueID=208518", 1)
# grab_projection("https://www.fftoday.com/rankings/playerwkproj.php?Season=2024&GameWeek=1&PosID=20&LeagueID=208518&order_by=FFPts&sort_order=DESC&cur_page=1", 1)

# position rankings (k, dst)
grab_rankings <- function(fftoday_page_html, wk) {
  all <- fetch_html(fftoday_page_html) |>
    html_element("body") |>
    html_element("center") |>
    html_elements("table") |>
    pluck(2) |>
    html_element(".bodycontent") |>
    html_elements("table") |>
    pluck(4)

  kickers <- all |>
    html_elements("table") |>
    pluck(2) |>
    html_table() |>
    select(X2, X3) |>
    slice(-c(1, 2)) |>
    filter(!str_detect(X2, "Tier")) |>
    rename(projection = "X2", name = "X3") |>
    relocate(name) |>
    mutate(
      week = wk,
      projection = 33 - as.numeric(projection), # rank, not points (calibrated in Player Value Added.R)
      position = "K"
    )

  defenses <- all |>
    html_elements("table") |>
    pluck(4) |>
    html_table() |>
    select(X2, X3) |>
    slice(-c(1, 2)) |>
    filter(!str_detect(X2, "Tier")) |>
    rename(projection = "X2", name = "X3") |>
    relocate(name) |>
    mutate(
      week = wk,
      projection = 33 - as.numeric(projection), # rank, not points (calibrated in Player Value Added.R)
      position = "DST"
    )

  list(kickers, defenses)
}

# grab_rankings("https://www.fftoday.com/rankings/playerwkrank.php?GameWeek=1&PosID=80&LeagueID=208518", 1)

# combine them into one function
# make sure url doesn't change for some reason
# QB URL: "https://www.fftoday.com/rankings/playerwkproj.php?Season=2024&GameWeek=1&PosID=10&LeagueID=208518"
combine_week <- function(week, season) {
  kick_def_rankings <- str_c(
    "https://www.fftoday.com/rankings/playerwkrank.php?Season=",
    season,
    "&GameWeek=",
    week,
    "&PosID=80&LeagueID=208518"
  ) |>
    grab_rankings(week)

  page_positions <- c("10" = "QB", "20" = "RB", "30" = "WR", "40" = "TE")

  rb_wr <- map(
    c(20, 30),
    ~ str_c(
      "https://www.fftoday.com/rankings/playerwkproj.php?Season=",
      season,
      "&GameWeek=",
      week,
      "&PosID=",
      .x,
      "&LeagueID=208518&order_by=FFPts&sort_order=DESC&cur_page=1"
    ) |>
      grab_projection(week, page_positions[[as.character(.x)]])
  ) |>
    append(kick_def_rankings)

  map(
    c(10, 20, 30, 40),
    ~ str_c(
      "https://www.fftoday.com/rankings/playerwkproj.php?Season=",
      season,
      "&GameWeek=",
      week,
      "&PosID=",
      .x,
      "&LeagueID=208518"
    ) |>
      grab_projection(week, page_positions[[as.character(.x)]])
  ) |>
    append(rb_wr) |>
    rbindlist() |>
    as_tibble() |>
    # the RB/WR pages are pulled twice (default + sorted by points)
    distinct(name, position, week, .keep_all = TRUE)
}

# Scrape Future Value -----------------------------------------------------

# scrape future value from ktc
player_value <- function(ktc_page_html) {
  players <- fetch_html(ktc_page_html) |>
    html_element("body") |>
    html_element("main") |>
    html_elements("div") |>
    html_elements(".onePlayer") |>
    html_element(".single-ranking-wrapper") |>
    html_element(".single-ranking")

  # grabs player name
  name <- players |>
    html_element(".player-name") |>
    html_node("p") |>
    html_node("a") |>
    html_text2()

  # grabs player value
  value <- players |>
    html_element(".value") |>
    html_node("p") |>
    html_text2()

  position_team_raw <- players |>
    html_element(".position-team") |>
    html_text2()

  position <- str_extract(position_team_raw, "^[A-Z]+")
  age <- str_extract(position_team_raw, "\\d+\\.\\d+") |> as.numeric()

  # return in tibble
  tibble(
    name = name,
    ktc_value = as.numeric(value),
    position = position,
    age = age
  )
}
