# Player Total Value
# player's value is composed of realized value and future value
# realized value is computed as fantasy points earned above replacement
# players only earn realized value when they are in the starting lineup
# replacement level is the average score of each team's top replacement player
# if a team doesn't have a replacement player, then the top waiver pick is selected

# load data
library("here")
library("tidymodels")
data_path <- "FantasyDynasty/"
source(here(data_path, "Scraping.R")) #run data

# Sleeper Score -----------------------------------------------------------

# compute fantasy score for non kickers and defenses
off_sleeper_points <- box_score_off %>%
  transmute(
    name = player_display_name,
    season = season, 
    week = week,
    sleeper_points = .04*passing_yards + 4*passing_tds - interceptions - sack_fumbles - sack_fumbles_lost + 2*passing_2pt_conversions +
      .1*rushing_yards + 6*rushing_tds - rushing_fumbles - rushing_fumbles_lost + 2*rushing_2pt_conversions +
      receptions + .1*receiving_yards + 6*receiving_tds - receiving_fumbles -receiving_fumbles_lost + 6*special_teams_tds)

# kicking
kick_sleeper_points <- box_score_kicking %>%
  transmute(
    name = player_display_name,
    season = season,
    week = week,
    fg_points = case_when(
      fg_att == 0 ~ 0,
      .default = 3*(fg_made_0_19 + fg_made_20_29 + fg_made_30_39) + 4*fg_made_40_49 + 5*fg_made_50_59 + 5*fg_made_60_ - fg_missed),
    pat_points = case_when(
      pat_att == 0 ~ 0,
      .default = pat_made - pat_missed),
    sleeper_points = fg_points + pat_points) %>%
  select(-fg_points, -pat_points)

# defense
opp_points_scored <- box_score_def %>%
  mutate(
    pa = 6*(passing_tds + rushing_tds + fumble_recovery_tds + special_teams_tds) + 2*passing_2pt_conversions +
      2*rushing_2pt_conversions + 3*fg_made + pat_made, #not including defensive tds
    defense = opponent_team,
    blocks = fg_blocked + pat_blocked,
    fumbles = receiving_fumbles_lost + rushing_fumbles_lost + sack_fumbles_lost) %>%
  select(season, week, defense, pa, blocks, fumbles)

# note: does not include special teams forced and recovered fumbles... oh well
def_sleeper_points <- box_score_def %>%
  left_join(opp_points_scored, by = join_by(season, week, team == defense)) %>%
  transmute(
    name = team,
    season = season,
    week = week,
    pa_points = case_when(
      pa == 0 ~ 10,
      pa < 7 ~ 7,
      pa < 14 ~ 4,
      pa < 21 ~ 1,
      pa < 28 ~ 0,
      pa < 35 ~ -1,
      .default = -4),
    sleeper_points = pa_points + 6*(special_teams_tds + def_tds) + def_sacks + 2*def_interceptions + 2*fumbles +
      def_fumbles_forced + 2*def_safeties + 2*blocks) %>%
  select(-pa_points) %>%
  mutate(name = recode(name, "LA" = "LAR"))

sleeper_points <- bind_rows(def_sleeper_points, off_sleeper_points, kick_sleeper_points) %>%
  name_correction() %>%
  filter(week != 18)

# all 32 defenses
defenses <- def_sleeper_points %>%
  select(name) %>%
  distinct() %>%
  mutate(player_id = name,
         position = "DST")

# load player info
player_info <- read_csv(here(data_path, "player_info.csv")) %>%
  select(name, player_id, position, birth_date) %>%
  filter(position %in% c("TE", "RB", "WR", "QB", "K")) %>%
  # remove duplicate names
  filter(player_id != 4634, player_id != 748, player_id != 232) %>%
  mutate(
    position = case_when(
      name == "Taysom Hill" ~ "TE",
      .default = position)) %>%
  distinct() %>%
  bind_rows(defenses) %>%
  name_correction()

# Find Value at Replacement -----------------------------------------------

# get projections in list format
projections_list <- projections %>%
  group_by(week) %>%
  reframe(week = list(tibble(name, projection, week))) %>%
  deframe()

# first find each player that started in the week
starters <- map(matchups, ~.x %>%
                  select(roster_id, starters, starters_points) %>%
                  unnest(cols = c(starters, starters_points)) %>%
                  filter(starters != 0) %>%
                  left_join(player_info, by = join_by(starters == player_id)) %>%
                  select(-starters, -birth_date))

# find bench players
bench <- pmap(list(matchups, starters, projections_list), function(m, s, p){
  m %>%
    select(roster_id, players) %>%
    unnest(cols = players) %>%
    left_join(player_info, by = join_by(players == player_id)) %>%
    anti_join(s %>% select(name), by = join_by(name)) %>%
    left_join(p %>% select(-week), by = join_by(name)) %>%
    mutate(projection = replace_na(projection, 0),
           type = "bench") %>%
    select(-players, -birth_date)
})

# find top waiver picks

waiver <- pmap(list(projections_list, bench, starters),
               function(p, b, s){
                 p %>% left_join(player_info %>% select(-player_id), by = join_by(name),
                                 relationship = "many-to-many") %>%
                   anti_join(b %>% select(name), by = join_by(name)) %>%
                   anti_join(s %>% select(name), by = join_by(name)) %>%
                   mutate(type = "waiver") %>%
                   arrange(position, desc(projection)) %>%
                   select(-birth_date)
               })

# replacement
top_replacement <- function(bench, waiver, pos, roster){
  bench %>% filter(roster_id == roster) %>%
    select(-roster_id) %>%
    bind_rows(waiver) %>%
    filter(position %in% pos) %>%
    slice_max(order_by = projection) %>%
    slice(1) %>%
    select(name)
}

clean_replacements <- function(bench, waiver, pos, wk){
  replacements <- map(1:12, ~top_replacement(bench, waiver, pos, .x)) %>%
    rbindlist() %>%
    as_tibble() %>%
    distinct() %>%
    pull()
  
  if(length(replacements) != 12){
    replacements <- waiver %>% filter(position %in% pos, name %!in% replacements) %>%
      slice(1:(12-length(replacements))) %>%
      select(name) %>%
      pull() %>%
      append(replacements)
  }
  if(length(pos) > 3){
    pos <- "super_flex"
  }else if(length(pos) > 2){
    pos <- "flex"
  }
  
  list(replacements, pos, wk) %>%
    return()
}

flex <- c("TE", "WR", "RB")
super_flex <- append(flex, "QB")
all_positions <- list("QB", "RB", "WR", "TE", flex, super_flex, "K", "DST")

all_replacements <- map2(bench, waiver, ~{
  b <- .x
  w <- .y
  wk <- .y$week[[1]]
  map(all_positions, ~clean_replacements(b, w, .x, wk))})

# replacement's fantasy score
find_score <- function(player, wk){
  sleeper_points %>%
    filter(week == wk, name == player) %>%
    select(sleeper_points) %>%
    pull()
}

mean_replacements <- map(all_replacements, ~map(.x, ~{
  r <- .x[[1]]
  p <- .x[[2]]
  wk <- .x[[3]]
  tibble(mean_replacement = map(r, ~find_score(.x, wk)) %>%
           unlist() %>% sum()/12, pos = p, week = wk)}) %>%
    rbindlist() %>%
    as_tibble())

# tone down outliers
overall_mean <- mean_replacements %>%
  rbindlist() %>%
  as_tibble() %>%
  group_by(pos) %>%
  summarize(overall_mean = mean(mean_replacement))

weighted_mean_replacements <- map(mean_replacements, ~.x %>%
                                    left_join(overall_mean, by = join_by(pos)) %>%
                                    mutate(weighted_mean = overall_mean*.3 + mean_replacement*.7) %>%
                                    select(pos, week, weighted_mean))

# find value above mean replacement
starters_revamp <- map(starters, ~.x %>%
                         group_by(roster_id) %>%
                         reframe(
                           roster_id = list(tibble(starters_points, name, position, roster_id))) %>%
                         deframe())

# compute value added
value_added <- map2(starters_revamp, weighted_mean_replacements, ~{
  s <- .x
  mr <- .y
  map(s, ~{
    
    qb <- sum(.x$position == "QB")
    wr <- sum(.x$position == "WR")
    rb <- sum(.x$position == "RB")
    te <- sum(.x$position == "TE")
    
    .x %>%
      mutate(
        replacement_need = case_when(
          position == "QB" & qb == 2 ~ "super_flex",
          position == "RB" & rb > 2 & qb == 1 ~ "super_flex",
          position == "RB" & rb > 2 ~ "flex",
          position == "WR" & wr > 2 & qb == 1 ~ "super_flex",
          position == "WR" & wr > 2 ~ "flex",
          position == "TE" & te > 1 & qb == 1 ~ "super_flex",
          position == "TE" & te > 1 ~ "flex",
          .default = position)) %>%
      left_join(mr, by = join_by(replacement_need == pos)) %>%
      mutate(
        value_added = starters_points - weighted_mean)}) %>%
    rbindlist() %>%
    as_tibble() %>%
    rename(points = "starters_points") %>%
    select(roster_id, name, position, week, points, value_added)}) %>%
  rbindlist() %>%
  as_tibble()

season_value_added <- value_added %>%
  group_by(position, name) %>%
  summarize(
    total_value_added = sum(value_added),
    total_points = sum(points)) %>%
  arrange(desc(total_value_added))

season_value_added %>% print(n=30)

# value_added %>%
#   group_by(roster_id, position, name) %>%
#   summarize(total_value_added = sum(value_added)) %>%
#   ggplot() +
#   geom_jitter(aes(x = roster_id, y = total_value_added, color = position), width = .1, height = 0)

# season_value_added %>%
#   ggplot() +
#   geom_violin(aes(position, total_value_added))

# season_value_added %>%
#   ggplot() +
#   geom_point(aes(x = total_points, y = total_value_added, color = position))


# remove objects and functions to declutter environment
rm(top_replacement, find_score, clean_replacements, super_flex, flex,
   weighted_mean_replacements, waiver, starters, starters_revamp, projections_list, projections, overall_mean,
   opp_points_scored, off_sleeper_points, kick_sleeper_points, defenses, def_sleeper_points,
   box_score_off, box_score_kicking, box_score_def, bench, all_replacements, all_positions, mean_replacements)

