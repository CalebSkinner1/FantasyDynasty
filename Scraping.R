# Scraping

library("nflfastR")
library("here")
data_path <- "FantasyDynasty/"

source(here(data_path, "Scrape Functions.R"))



# load box score data from NFL 2024
# https://www.nflfastr.com
box_score_off <- load_player_stats(seasons = 2024, stat_type = "offense")
box_score_kicking <- load_player_stats(seasons = 2024, stat_type = "kicking")
box_score_def <- calculate_stats(summary_level = "week", stat_type = "team")

# Sleeper API ----------------------------------------------------
# https://docs.sleeper.com

league_id <- "1066207868321370112"

# matchups CHECK
matchups <- map(1:15, ~str_c("https://api.sleeper.app/v1/league/", league_id, "/matchups/", .x) %>%
                  parse_api())
# matchups[[1]]$players
# matchups[[1]]$starters

# transactions (this includes trades)
transactions <- map(1:15, ~str_c("https://api.sleeper.app/v1/league/", league_id, "/transactions/", .x) %>%
                      parse_api())
# transactions[[10]] %>% filter(type == "trade") %>% select(draft_picks)

# traded picks
traded_picks <- str_c("https://api.sleeper.app/v1/league/", league_id, "/traded_picks") %>%
  parse_api()

# drafts CHECK

# Get all drafts urls for a league
draft_urls <- str_c("https://api.sleeper.app/v1/league/", league_id, "/drafts") %>%
  parse_api() %>%
  transmute(url = str_c("https://api.sleeper.app/v1/draft/", draft_id, "/picks")) %>%
  pull()

# Get all draft trade urls for a league
draft_trades <- draft_urls %>% str_replace("/picks", "/traded_picks")

# Get picks in a draft
draft_picks <- map(draft_urls, parse_api)

# Get trades in a draft
draft_trades <- map(draft_trades, parse_api)

# # player information
# player_info2 <- parse_api_list("https://api.sleeper.app/v1/players/nfl")
# 
# player_information <- map(player_info2, ~{
#   tibble(
#     name = .x$full_name,
#     player_id = .x$player_id,
#     position = .x$fantasy_positions,
#     age = .x$age,
#     height = .x$height,
#     weight = .x$weight)}) %>%
#   rbindlist(fill = TRUE) %>%
#   as_tibble()

# write_csv(player_information, here(data_path, "player_info.csv"))

rm(draft_urls)

# Scrape Projections ----------------------------------------------------
# need weekly player ranking for above replacement metric
# https://www.fftoday.com/rankings/playerwkproj.php?Season=2024&GameWeek=1&PosID=10&LeagueID=208518

projections <- map(1:15, ~combine_week(.x)) %>%
  rbindlist() %>%
  as_tibble() %>%
  name_correction()

# Scrape Future Value -----------------------------------------------------
# https://keeptradecut.com/dynasty-rankings

# keep_trade_cut value 8/23 (only top 50 players)
# t50 <- player_value("http://web.archive.org/web/20240823002052/https://keeptradecut.com/dynasty-rankings")
# 
# # need to add more than just top 50
# b51 <- tibble(
#   name = c("James Cook", "Tee Higgins", "Mark Andrews", "Jordan Addison", "Dalton Kincaid", "Drake Maye", "Rashee Rice",
#            "Kyle Pitts", "J.J. McCarthy", "Trey Benson", "Bryce Young", "Jayden Reed", "Xavier Worthy", "Joe Mixon",
#            "T.J. Hockenson", "Derrick Henry", "David Montgomery", "Stefon Diggs", "Cooper Kupp", "Javonte Williams",
#            "Amari Cooper", "Tony Pollard", "Terry McLaurin", "Bo Nix", "Zamir White", "George Kittle", "Keon Coleman",
#            "Brian Robinson Jr.", "Blake Corum", "Christian Watson", "Aaron Jones", "Calvin Ridley", "Jake Ferguson",
#            "Jameson Williams", "Ricky Pearsall", "James Conner", "Adonai Mitchell", "MarShawn Lloyd", "Xavier Legette",
#            "Aaron Rodgers", "Austin Ekeler", "Kendre Miller", "DeAndre Hopkins", "Zack Moss", "Luke Musgrave", "Romeo Doubs",
#            "Jerry Jeudy", "Raheem Mostert", "Jerome Ford", "Ray Davis", "Ja'Lynn Polk", "Jakobi Meyers", "Dontayvion Wicks",
#            "Derek Carr", "Marvin Mims Jr.", "Isaiah Likely", "Roschon Johnson", "Joshua Palmer", "Michael Mayer",
#            "Kimani Vidal", "Mike Williams", "Gus Edwards", "Rashid Shaheed", "Tyler Allgeier", "Antonio Gibson",
#            "Chigoziem Okonkwo", "Khalil Herbert", "Jaleel McLaughlin", "DeMario Douglas", "Brandin Cooks", "Rashod Bateman",
#            "Jalen McMillan", "Darnell Mooney", "Braelon Allen", "Ty Chandler", "Jonathan Mingo", "Greg Dulcich",
#            "Treylon Burks", "Juwan Johnson", "Adam Thielen", "Tyrone Tracy Jr."),
#   value = c(4734, 4796, 4519, 4153, 5365, 5014, 4682,
#             4986, 4199, 3652, 4013, 4061, 4627, 3301,
#             4049, 3528, 3392, 3249, 3954, 3313,
#             3401, 2915, 3688, 4315, 3255, 3597, 3926,
#             3205, 3169, 3414, 2827, 3215, 3330,
#             3188, 3317, 2925, 3286, 2770, 3184,
#             2704, 2146, 2245, 2523, 2645, 2595, 3172,
#             2629, 2807, 2780, 2164, 3117, 2641, 3043,
#             2315, 2247, 2641, 2027, 2661, 2218,
#             2069, 1987, 2328, 2511, 2351, 1738,
#             1722, 2073, 2450, 2508, 1743, 2118,
#             2410, 2141, 2694, 1981, 1842, 2032,
#             1537, 1291, 1449, 2188))


# instantaneous keep_trade_cut value
keep_trade_cut <- map(0:9, ~str_c("https://keeptradecut.com/dynasty-rankings?page=", .x, "&filters=QB|WR|RB|TE|RDP&format=2") %>%
      player_value()) %>%
  rbindlist() %>%
  as_tibble() %>%
  mutate(value = as.numeric(value))


