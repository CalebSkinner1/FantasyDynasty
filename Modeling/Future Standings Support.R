# Future Standings Support
# this holds functions utilized in Future Standings.R
library("tictoc")
library("tidyverse"); theme_set(theme_minimal())
library("furrr")
library("tidymodels")


# team rva ranking --------------------------------------------------------

assign_draft_pick_value <- function(dp_df, years_in_advance = "total value"){
  dp_df <- dp_df %>% filter(!is.na(draft_order))
  
  if(nrow(dp_df) == 0){
    tibble()
  }else{
    dp_df %>%
      mutate(pick_no = (round-1)*12 + draft_order) %>%
      select(roster_id, pick_no) %>%
      left_join(rookie_draft_values %>% filter(metric == years_in_advance), by = join_by(pick_no)) %>%
      select(-pick_no, -metric)
  }
}

sample_quantiles <- function(data){
  data %>%
    select(-roster_id) %>%
    apply(1, function(row) sample(row, 1)) %>%
    as_tibble() %>%
    mutate(roster_id = data$roster_id) %>%
    group_by(roster_id) %>%
    summarize(
      va = sum(value)) %>%
    filter(!is.na(roster_id)) %>%
    transmute(
      roster_id = roster_id,
      rank = rank(desc(va)),
      draft_order = rank(va),
      va = va)
}

prep_draft_picks <- function(prev_year, year, years_ahead = "first year"){
  draft_picks_year <- future_draft_picks %>%
    filter(season == year)
  
  # if draft order is not set, find it from previous year standings
  if(is.na(draft_picks_year$draft_order[1])){
    draft_picks_year <- draft_picks_year %>%
      select(-draft_order) %>%
      left_join(prev_year %>% select(roster_id, draft_order), by = join_by(pick_slot == roster_id))
  }
  draft_picks_order <- draft_picks_year %>%
    select(roster_id, draft_order, round) %>%
    assign_draft_pick_value(years_ahead)
  
}

# functions for season on season --------------------------------------------
construct_table <- function(matchups_table, season_dates, date){
  # find season_end and season_start
  season_end <- season_dates$season_end[season_dates$season_end > date] %>% min()
  season_start <- season_dates$season_start[season_dates$season_start < season_end] %>% max()
  
  current_table <- matchups_table %>%
    filter(season == year(season_start)) %>%
    mutate(
      game_date = season_start + weeks(week - 1) + days(4),
      points = if_else(game_date > date, 0, points),
      opp_points = if_else(game_date > date, 0, opp_points)) %>%
    rowwise() %>%
    mutate(
      matchup = if_else(roster_id > opponent_id, str_c(roster_id, "_", opponent_id), str_c(opponent_id, "_", roster_id))) %>%
    ungroup() %>%
    distinct(season, week, matchup, .keep_all = TRUE) %>%
    select(season, week, round, roster_id, opponent_id, points, opp_points)
}


win_probability <- function(table, fit_coef){
  n <- nrow(table)
  odds <- runif(n)
  
  table %>%
    mutate(
      .pred_1 = exp(va_diff*fit_coef[[1]])/(1+ exp(va_diff*fit_coef[[1]])),
      winner = if_else(odds > .pred_1, opponent_id, roster_id),
           loser = if_else(winner == roster_id, opponent_id, roster_id)) %>%
    select(-contains(".pred"), -contains("points"), -contains("va"))
}

sim_playoffs <- function(standings, team_tva, fit_coef, current_table){
  
  if(current_table %>% filter(week == 15) %>% slice(1) %>% pull(points) == 0){
    # first round
    first_round <- tibble(
      roster_id = standings[standings$rank %in% c(3,4),]$winner,
      opponent_id = standings[standings$rank %in% c(6,5),]$winner) %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
    
    # fifth place
    fifth_place <- tibble(
      roster_id = first_round$loser[1],
      opponent_id = first_round$loser[2]) %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
    
    # second round
    second_round <- tibble(
      roster_id = first_round$winner,
      opponent_id = standings[standings$rank %in% c(2,1),]$winner) %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
  }else{
    # fifth place
    fifth_place <- current_table %>% filter(round == "5th place") %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
    
    # second round
    second_round <- current_table %>% filter(round == "2nd round") %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
  }
  
  if(current_table %>% filter(week == 16) %>% slice(1) %>% pull(points) == 0){
    # third place
    third_place <- tibble(
      roster_id = second_round$loser[1],
      opponent_id = second_round$loser[2]) %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
    
    # championship
    championship <- tibble(
      roster_id = second_round$winner[1],
      opponent_id = second_round$winner[2]) %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
  }else{
    # third place
    third_place <- current_table %>% filter(round == "3rd place") %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
    
    # championship
    championship <- current_table %>% filter(round == "Championship") %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
  }

  # final standings for season
  tibble(rank = c(1:6),
         roster_id = c(championship$winner[1], championship$loser[1],
                       third_place$winner[1], third_place$loser[1],
                       fifth_place$winner[1], fifth_place$loser[1])) %>%
    bind_rows(filter(standings, rank > 6) %>% select(winner, rank) %>% rename(roster_id = winner)) %>%
    arrange(rank)
}

prep_table_tva <- function(current_table, team_tva){
  current_table %>% left_join(team_tva, by = join_by(roster_id)) %>%
    rename(team_tva = va) %>% 
    left_join(team_tva, by = join_by(opponent_id == roster_id)) %>%
    rename(opp_tva = va) %>%
    mutate(va_diff = team_tva - opp_tva)
}

year_sim <- function(current_table, team_tva, fit_coef){
  completed_results <- current_table %>% filter(points != 0) %>%
    mutate(winner = if_else(points > opp_points, roster_id, opponent_id),
           loser = if_else(winner == roster_id, opponent_id, roster_id)) %>%
    select(season, week, roster_id, opponent_id, winner, loser)
  
  results <- current_table %>%
    prep_table_tva(team_tva) %>%
    filter(week <= 14) %>%
    win_probability(fit_coef) %>%
    bind_rows(., completed_results)
  
  end_season_standings <- results %>% group_by(winner) %>%
    summarize(wins = n()) %>%
    mutate(rank = rank(desc(wins), ties.method = "random")) #random ties is enough for this I think
  
  end_season_standings %>% sim_playoffs(team_tva, fit_coef, current_table)
}

multi_year_sim <- function(current_table, team_tva, fit_coef, years = 3){
  year_results <- list()
  year_table <- list()
  year_table[[1]] <- current_table
  
  if(length(team_tva) != years){
    break("team_tva list is not the correct length")
  }
  
  for(i in 1:years){
    year_results[[i]] <- year_sim(year_table[[i]], team_tva[[i]], fit_coef)
    
    year_table[[i + 1]] <- year_table[[i]] %>%
      slice(c(19:98)) %>%
      bind_rows(slice(year_table[[i]], c(19:36))) %>%
      mutate(
        season = year_table[[i]]$season[1] + 1,
        week = c(rep(1:14, each = 6), rep(15, each = 4), rep(16, each = 6), rep(17, each = 4)),
        points = 0,
        opp_points = 0)
  }
  year_results
}

compute_final_standings_odds <- function(current_table, team_tva_list, fit_coef, years = 3, n_sims = 5000){
  sim_standings <- future_map(team_tva_list[c(1:n_sims)], ~multi_year_sim(current_table, .x, fit_coef),
                              .progress = TRUE,
                              .options = furrr_options(seed = TRUE)) %>%
    transpose()
  
  # compute final_standings_odds
  final_standings_odds <- map(sim_standings, ~{
    bind_rows(.x) %>% group_by(roster_id, rank) %>%
      summarize(perc = n()/n_sims,
                .groups = "keep") %>%
      ungroup()}) %>%
    bind_rows(.id = "season") %>%
    mutate(season = current_table$season[1] + as.numeric(str_remove(season, "year")) - 1)
  
  final_standings_odds
}
