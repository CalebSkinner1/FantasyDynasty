# Future Standings Support
# this holds functions utilized in Future Standings.R

suppressPackageStartupMessages({
  library("tictoc")
  library("tidyverse")
  theme_set(theme_minimal())
  library("furrr")
  library("tidymodels")
})

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
    mutate(rank = rank(-value)) %>%
    filter(rank < 13) %>% #only keep top 12 valuable players (this caps it, realistic because teams can only start 8 players each week)
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

  current_table <- tibble(
      season = current_table$season[1] + 1,
      round = c(
        rep("1st round", each = 2), rep("loser's bracket", each = 2),
        rep("2nd round", each = 2), "5th place", rep("loser's bracket", each = 3),
        "Championship", "3rd place", rep("loser's bracket", each = 2)),
      week = c(rep(15, each = 4), rep(16, each = 6), rep(17, each = 4)),
      points = 0,
      opp_points = 0) |>
    filter(week > max(current_table$week)) %>%
    bind_rows(current_table, .)
}

win_probability <- function(table, fit_coef, points_method = TRUE){
  if(points_method){
    sigma_1 <-  rnorm(nrow(table), 0, fit_coef[[3]])
    sigma_2 <-  rnorm(nrow(table), 0, fit_coef[[3]])

    table %>%
      mutate(
        # mean and add normal error
        points = team_tva*fit_coef[[1]] + sqrt(team_tva)*fit_coef[[2]] + sigma_1,
        opp_points = opp_tva*fit_coef[[1]] + sqrt(opp_tva)*fit_coef[[2]] + sigma_2,
        winner = if_else(points > opp_points, roster_id, opponent_id),
        loser = if_else(winner == roster_id, opponent_id, roster_id)) %>%
      select(-contains(".pred"), -contains("va"), -contains("draft_order"), -contains("rank")) %>%
      return()
  }else{
    table %>%
      mutate(
        .pred_1 = exp(va_diff*fit_coef[[1]])/(1+ exp(va_diff*fit_coef[[1]])),
        winner = if_else(odds > .pred_1, opponent_id, roster_id),
        loser = if_else(winner == roster_id, opponent_id, roster_id)) %>%
      select(-contains(".pred"), -contains("points"), -contains("va")) %>%
      return()
  }
}

# need to build up empty current_table... this should fix issues.
sim_playoffs <- function(standings, team_tva, fit_coef, current_table){
  
  if(current_table %>% filter(week == 15) %>% slice(1) %>% pull(points) == 0){
    # first round
    first_round <- tibble(
      roster_id = standings[standings$rank %in% c(3,4),]$roster_id,
      opponent_id = standings[standings$rank %in% c(6,5),]$roster_id) %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
  }else{
    # first round
    first_round <- current_table %>%
      filter(week == 15, roster_id %in% standings[standings$rank %in% c(3, 4, 5, 6),]$roster_id) %>%
      mutate(
        winner = if_else(points > opp_points, roster_id, opponent_id),
        loser = if_else(winner == roster_id, opponent_id, roster_id)
      )
  }
  if(current_table %>% filter(week == 16) %>% slice(1) %>% pull(points) == 0){
    # fifth place
    fifth_place <- tibble(
      roster_id = first_round$loser[1],
      opponent_id = first_round$loser[2]) %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
    
    # second round
    second_round <- tibble(
      roster_id = first_round$winner,
      opponent_id = standings[standings$rank %in% c(2,1),]$roster_id) %>%
      prep_table_tva(team_tva) %>%
      win_probability(fit_coef)
  }else{
    # fifth place
    fifth_place <- current_table %>%
      filter(week == 16, roster_id %in% first_round$loser) %>%
      mutate(
        winner = if_else(points > opp_points, roster_id, opponent_id),
        loser = if_else(winner == roster_id, opponent_id, roster_id))
    
    # second round
    second_round <- current_table %>%
      filter(week == 16, roster_id %in% c(first_round$winner, standings[standings$rank %in% c(2,1),]$roster_id)) %>%
      mutate(
        winner = if_else(points > opp_points, roster_id, opponent_id),
        loser = if_else(winner == roster_id, opponent_id, roster_id))
  }
  
  if(current_table %>% filter(week == 17) %>% slice(1) %>% pull(points) == 0){
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
    third_place <- current_table %>%
      filter(week == 17, roster_id %in% second_round$loser) %>%
      mutate(
        winner = if_else(points > opp_points, roster_id, opponent_id),
        loser = if_else(winner == roster_id, opponent_id, roster_id))
    
    # championship
    championship <- current_table %>%
      filter(week == 17, roster_id %in% second_round$winner) %>%
      mutate(
        winner = if_else(points > opp_points, roster_id, opponent_id),
        loser = if_else(winner == roster_id, opponent_id, roster_id))
  }

  # final standings for season
  tibble(rank = c(1:6),
         roster_id = c(championship$winner[1], championship$loser[1],
                       third_place$winner[1], third_place$loser[1],
                       fifth_place$winner[1], fifth_place$loser[1])) %>%
    bind_rows(filter(standings, rank > 6) %>% select(roster_id, rank)) %>%
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
    select(season, week, roster_id, opponent_id, winner, loser, points, opp_points)
  
  furthest_week <- max(c(0,completed_results$week))
  
  results <- current_table %>%
    prep_table_tva(team_tva) %>%
    filter(week <= 14, week > furthest_week) %>%
    win_probability(fit_coef) %>%
    bind_rows(., completed_results)
  
  tiebreaker <- bind_rows(
    results %>% filter(week < 15) |>
      group_by(roster_id) %>%
    summarize(total_points = sum(points)),
    results %>% filter(week < 15) |>
      group_by(opponent_id) %>%
      summarize(total_points = sum(opp_points)) %>%
      rename("roster_id" = opponent_id)) %>%
    group_by(roster_id) %>%
    summarize(total_points = sum(total_points)) %>%
    mutate(rank = rank(total_points))
  
  end_season_standings <- results %>%
    filter(week < 15) |> 
    pivot_longer(cols = c(roster_id, opponent_id), names_to = "position", values_to = "roster_id") |>
    mutate(win = if_else(roster_id == winner, 1, 0)) |>
    group_by(roster_id) %>%
    summarize(wins = sum(win)) %>%
    left_join(tiebreaker, by = join_by(roster_id)) %>%
    mutate(wins = wins + rank * 0.01) %>%
    mutate(rank = rank(desc(wins))) #shouldn't need any ties (tiebreaker accounted for it)
  
  end_season_standings %>% sim_playoffs(team_tva, fit_coef, current_table) %>%
    left_join(end_season_standings %>% rename("season_rank" = rank), by = join_by(roster_id)) %>%
    mutate(bye = if_else(season_rank < 3, TRUE, FALSE)) %>%
    select(roster_id, rank, bye)
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
    rank <- bind_rows(.x) %>% group_by(roster_id, rank) %>%
      summarize(perc = n()/n_sims,
                .groups = "keep") %>%
      ungroup() %>%
      rename("result" = rank) %>%
      mutate(type = "rank")
    bye <- bind_rows(.x) %>% group_by(roster_id, bye) %>%
      summarize(perc = n()/n_sims,
                .groups = "keep") %>%
      ungroup() %>%
      rename("result" = bye) %>%
      mutate(type = "bye")
    bind_rows(rank, bye)
    }) %>%
    bind_rows(.id = "season") %>%
    mutate(season = current_table$season[1] + as.numeric(str_remove(season, "year")) - 1)
  
  final_standings_odds
}
