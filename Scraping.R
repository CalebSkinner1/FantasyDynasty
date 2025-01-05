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
matchups <- map(1:17, ~str_c("https://api.sleeper.app/v1/league/", league_id, "/matchups/", .x) %>%
                  parse_api())
# matchups[[1]]$players
# matchups[[1]]$starters

# transactions (this includes trades)
transactions <- map(1:17, ~str_c("https://api.sleeper.app/v1/league/", league_id, "/transactions/", .x) %>%
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
#     birth_date = .x$birth_date,
#     height = .x$height,
#     weight = .x$weight)}) %>%
#   rbindlist(fill = TRUE) %>%
#   as_tibble()

# write_csv(player_information, here(data_path, "player_info.csv"))

rm(draft_urls)

# Scrape Projections ----------------------------------------------------
# need weekly player ranking for above replacement metric
# https://www.fftoday.com/rankings/playerwkproj.php?Season=2024&GameWeek=1&PosID=10&LeagueID=208518

projections <- map(1:17, ~combine_week(.x)) %>%
  rbindlist() %>%
  as_tibble() %>%
  name_correction()

# Scrape Future Value -----------------------------------------------------
# https://keeptradecut.com/dynasty-rankings

# # keep_trade_cut value 8/23 (only top 50 players)
# t50 <- player_value("http://web.archive.org/web/20240823002052/https://keeptradecut.com/dynasty-rankings")
# 
# # need to add more than just top 50
# b51 <- tibble(
#   name = c("James Cook", "Tee Higgins", "Mark Andrews", "Jordan Addison", "Drake Maye", "Rashee Rice",
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
#            "Treylon Burks", "Juwan Johnson", "Adam Thielen", "Tyrone Tracy Jr.", "Michael Pittman Jr.", "Kyren Williams",
#            "DK Metcalf", "Isiah Pacheco", "Rachaad White", "Kenneth Walker III", "Zay Flowers", "Jonathon Brooks", "Jared Goff",
#            "Josh Jacobs", "George Pickens", "Brock Bowers", "Deebo Samuel Sr.", "Jaxon Smith-Njigba", "D'Andre Swift",
#            "Deshaun Watson", "Rhamondre Stevenson", "Davante Adams", "Brian Thomas Jr.", "Mike Evans", "Travis Kelce",
#            "Christian Kirk", "Ladd McConkey", "Will Levis", "Tyjae Spears", "Alvin Kamara", "Najee Harris",
#            "Diontae Johnson", "Baker Mayfield", "Chris Godwin", "Jaylen Warren", "Kirk Cousins", "Evan Engram",
#            "David Njoku", "Matthew Stafford", "Hollywood Brown", "Keenan Allen", "Zach Charbonnet", "Nick Chubb",
#            "Courtland Sutton", "Dallas Goedert", "Josh Downs", "Chase Brown", "Pat Freiermuth", "Michael Penix Jr.",
#            "Devin Singletary", "Jahan Dotson", "Jaylen Wright", "Ben Sinnott", "Cole Kmet", "Geno Smith",
#            "Dalton Schultz", "Roman Wilson", "Justin Fields", "Daniel Jones", "Jermaine Burton", "Tyler Lockett",
#            "Troy Franklin", "Chuba Hubbard", "Khalil Shakir", "Quentin Johnson", "Michael Wilson", "Gabe Davis",
#            "Malachi Corley", "Tucker Kraft", "Bucky Irving", "Russell Wilson", "Cade Otton", "Javon Baker",
#            "Curtis Samuel", "Ja'Tavion Sanders", "Wan'Dale Robinson", "Audric Estime", "Elijah Moore", "Jalin Hyatt",
#            "Dameon Pierce", "Keaton Mitchell", "Noah Fant", "Will Shipley", "Ezekiel Elliot", "J.K. Dobbins",
#            "Luke McCaffrey", "Hunter Henry", "Rico Dowdle", "Theo Johnson", "Jelani Woods", "Tank Bigsby", "Elijah Mitchell",
#            "A.J. Dillon", "Devontez Walker", "Isaac Guerendo", "Dylan Laube", "Alexander Mattison", "Trey Palmer", "Tyler Boyd",
#            "Cedric Tillman", "Miles Sanders", "Zay Jones", "Dawson Knox", "Taysom Hill", "Andrei Iosivas", "Gerald Everett",
#            "Kenneth Gainwell", "Mike Gesicki", "Alec Pierce", "Tyler Higbee", "D.J. Chark Jr.", "Odell Beckham Jr.",
#            "Tyler Conklin", "D'Onta Foreman", "Rondale Moore", "Jacob Cowing", "A.T. Perry", "K.J. Osborn",
#            "Clyde Edwards-Helaire", "Erick All Jr.", "Greg Dortch", "Jamaal Williams", "Evan Hull", "Rasheen Ali",
#            "Kendrick Bourne", "Malik Washington", "Israel Abanikanda", "Darius Slayton", "Brenden Rice", "Jonnu Smith",
#            "Noah Brown", "Samaje Perine", "John Metchie III", "Michael Carter", "Josh Reynolds", "Tutu Atwell", "Jalen Tolbert",
#            "Isaiah Davis", "Cade Stover", "Skyy Moore", "Justice Hill", "Eric Gray", "Chris Rodriguez Jr.", "Sam Darnold",
#            "Jeff Wilson Jr.", "Hayden Hurst", "Daniel Bellinger", "Kadarius Toney", "Zach Ertz", "Colby Parkinson",
#            "Juju Smith-Schuster", "Michael Thomas", "Demarcus Robinson", "Tre Tucker", "Jaheim Bell", "Johnny Wilson",
#            "Luke Schoonmaker", "Sean Tucker", "Kareem Hunt", "Bo Melton", "Tyler Scott", "Sam Howell", "Pierre Strong Jr.",
#            "Cam Akers", "Aidan O'Connell", "D'Ernest Johnson", "Jamari Thrash", "Emari Demercado", "Kylen Granson",
#            "Jared Wiley", "Donovan Peoples-Jones", "Jordan Mason", "Isaiah Spiller", "Parker Washington",
#            "Calvin Austin III", "Jerick McKinnon", "Darnell Washington", "Irv Smith Jr.", "Allen Lazard", "Deuce Vaughn",
#            "Noah Gray", "Jawhar Jordan", "Spencer Rattler", "Trey Sermon", "Blake Watson", "Brevin Jordan", "Tyquan Thornton",
#            "Ainias Smith", "Donald Parham Jr.", "Jauan Jennings", "Bub Means", "Jase McClellan", "Kenny Pickett",
#            "Mac Jones", "Dalvin Cook", "Ryan Flournoy", "Zach Evans", "Xavier Hutchinson", "Jimmy Garoppolo",
#            "Jacoby Brissett", "Frank Gore Jr.", "Trey Lance", "Terrace Marshall Jr.", "Desmond Ridder", "Will Mallory",
#            "Jeremy Ruckert", "Charlie Jones", "Hunter Renfrow", "Casey Washington", "Adam Trautman", "Jordan Whittington",
#            "Logan Thomas", "Tim Patrick", "Dillon Johnson", "Cordarrelle Patterson", "Robert Woods", "Parris Campbell",
#            "Marquez Valdes-Scantling", "Will Dissly", "Tip Reiman", "Joshua Kelley", "Hendon Hooker", "Deneric Prince",
#            "Tanner Hudson", "Davis Allen", "Isaiah Hodgins", "Cody Schrader"),
#   value = c(4734, 4796, 4519, 4153, 5014, 4682,
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
#             1537, 1291, 1449, 2188, 4942, 5134,
#             5078, 4846, 4015, 4784, 4726, 4625, 4733,
#             4049, 4783, 5199, 4595, 4609, 3450,
#             3271, 3401, 3929, 4791, 3916, 4063,
#             3406, 4083, 3646, 3408, 3202, 3176,
#             3285, 3500, 3349, 3284, 3290, 3147,
#             3206, 3160, 3011, 2862, 2864, 2730,
#             2741, 2636, 2920, 3163, 2821, 4059,
#             2647, 2332, 2955, 2820, 2681, 2624,
#             2583, 2634, 2828, 2606, 2770, 2191,
#             2384, 2164, 2880, 2069, 2530, 2311,
#             2290, 2027, 2347, 2041, 2147, 2447,
#             2387, 2081, 2152, 2236, 1703, 2039,
#             1667, 2198, 1648, 2061, 1768, 2290,
#             2468, 1556, 2341, 1762, 1177, 2063, 1571,
#             1290, 1543, 1417, 1685, 1297, 1657, 1507,
#             1394, 1228, 1457, 1604, 1478, 1708, 926,
#             1315, 1248, 1647, 1022, 1471, 596,
#             1384, 1114, 1300, 1338, 1549, 1281,
#             1225, 1419, 1797, 1063, 1252, 1423,
#             1452, 1688, 991, 1235, 1662, 1415,
#             1041, 789, 1408, 1203.8, 1173, 1256, 1556,
#             923, 1447, 1062, 1584, 1608, 1119, 2451,
#             541, 833, 1480, 748, 933, 1421,
#             169, 930.3, 1255, 1625, 179, 1346,
#             1252, 1301, 176, 1342, 961, 2072, 809,
#             1111, 1626, 559, 879, 998, 731,
#             1401, 983, 1657, 1012, 1426,
#             1305, 1146.5, 1145, 1187.7, 770, 1221,
#             1259, 1270.0, 1883, 1430, 1268.5, 1057, 1047,
#             935, 1044.7, 1375, 919, 935, 890,
#             889, 988.2, 760, 1047.5, 1074, 1124.5,
#             1381, 1291, 1352, 856, 764, 431,
#             800, 688, 881.2, 892.8, 612, 1992, 
#             789.2, 889, 795.2, 376, 178, 575.0,
#             724, 790, 493, 425, 1439, 731.8,
#             172, 960, 902, 678.0))

# bind_rows(t50, b51) %>% write_csv(here(data_path, "ktc_value082324"))









# instantaneous keep_trade_cut value
keep_trade_cut <- map(0:9, ~str_c("https://keeptradecut.com/dynasty-rankings?page=", .x, "&filters=QB|WR|RB|TE|RDP&format=2") %>%
      player_value()) %>%
  rbindlist() %>%
  as_tibble() %>%
  mutate(value = as.numeric(value)) %>%
  name_correction()

# periodically save
keep_trade_cut %>% write_csv(here(data_path, "ktc_value010525"))

# remove objects and functions to declutter environment
rm(league_id, combine_week, grab_projection, grab_rankings, parse_api, parse_api_list, player_value)
