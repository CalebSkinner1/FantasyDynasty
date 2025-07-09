# Server Script

# Define Server
server <- function(input, output, session) {
  # Reactivity for Page 1
  
  output$player_basic_info <- renderDT({ #table 1
    req(input$player_name)
    basic_info(input$player_name) %>%
      datatable(
        options = list(
          pageLength = 1,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$plot_future_value <- renderPlot({ #first plot
    req(input$player_name)
    p <- plot_future_value(input$player_name)
    plot(p)
  })
  
  output$tabulate_realized_value_title <- renderUI({ #title
    req(input$player_name, input$player_season) #require input
    h3(str_c(input$player_name, " Seasons"))
  })
  
  output$tabulate_realized_value <- renderDT({ #table 2
    req(input$player_season)
    tabulate_realized_value(value_added, input$player_name, input$player_season, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$weekly_results <- renderPlot({ #second plot
    req(input$player_season) #require input
    weekly_results(value_added, input$player_name, input$player_season)
  })
  
  # Reactivity for Page 2
  
  output$image_ui <- renderUI({ # user image
    req(input$team_name)
    image_url <- avatar %>% filter(display_name == input$team_name) %>% pull(avatar_url)
    
    tags$img(src = image_url, height = "150px")
  })
  
  output$team_assets_title <- renderUI({ #title
    req(input$team_name) #require input
    h3(str_c(input$team_name, " Future Value"))
  })
  
  output$team_assets <- renderDT({ #table 1
    req(input$team_name) # require input
    users %>% filter(display_name == input$team_name) %>%
      select(roster_id) %>%
      pull() %>% grab_team_assets(shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$position_outlook_title <- renderUI({ #title
    req(input$team_name) #require input
    h3(str_c(input$team_name, " Position Outlook"))
  })
  
  output$position_outlook <- renderDT({ #table 2
    req(input$team_name) # require input
    users %>% filter(display_name == input$team_name) %>%
      select(roster_id) %>%
      pull() %>% position_outlook(shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$team_contributors_title <- renderUI({ #title
    req(input$team_name, input$team_season) #require input
    h3(str_c(input$team_name, ": ", input$team_season, " Season"))
  })
  
  output$team_contributors <- renderDT({ #table 3
    req(input$team_name, input$team_season) # require input
    users %>% filter(display_name == input$team_name) %>%
      pull(roster_id) %>% grab_team_contributors(input$team_season, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$team_contributors_weekly_title <- renderUI({ #title
    req(input$team_name, input$team_season) #require input
    h3(str_c(input$team_name, ": ", input$team_season, " Season Week ", input$team_week))
  })
  
  output$team_contributors_weekly <- renderDT({ #table 4
    req(input$team_name, input$team_season, input$team_week) # require input
    users %>% filter(display_name == input$team_name) %>%
      select(roster_id) %>%
      pull() %>% grab_team_contributors_weekly(input$team_season, input$team_week, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 11,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$avenue_grades_title <- renderUI({ #title
    req(input$team_name) #require input
    h3(str_c(input$team_name, " Avenue Grades"))
  })
  
  output$avenue_grades <- renderDT({ #table 5
    req(input$team_name) # require input
    users %>% filter(display_name == input$team_name) %>%
      select(roster_id) %>%
      pull() %>% overall_grades(value_avenues, ., shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$top_acquisitions_title <- renderUI({ #title
    req(input$team_name, input$team_avenue) #require input
    h3(str_c(input$team_name, " Top Acquisitions"))
  })
  
  output$top_acquisitions <- renderDT({ #table 6
    req(input$team_name, input$team_avenue) # require input
    users %>% filter(display_name == input$team_name) %>%
      select(roster_id) %>%
      pull() %>% top_acquisitions(acquisitions, ., enter_avenue = input$team_avenue, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$worst_acquisitions_title <- renderUI({ #title
    req(input$team_name, input$team_avenue) #require input
    h3(str_c(input$team_name, " Worst Acquisitions"))
  })
  
  output$worst_acquisitions <- renderDT({ #table 7
    req(input$team_name, input$team_avenue) # require input
    users %>% filter(display_name == input$team_name) %>%
      select(roster_id) %>%
      pull() %>% worst_acquisitions(acquisitions, ., enter_avenue = input$team_avenue, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$team_composition_title <- renderUI({ #title
    req(input$team_name) #require input
    h3(str_c(input$team_name, " Team Composition"))
  })
  
  output$team_composition <- renderDT({ #table 8
    req(input$team_name) # require input
    users %>% filter(display_name == input$team_name) %>%
      select(roster_id) %>%
      pull() %>% team_composition(player_avenues, ., shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  # Reactivity for Page 3
  
  output$draft_ranking_title <- renderUI({ #title
    req(input$draft_selection) #require input
    h3(str_c(str_to_title(input$draft_selection), " Draft Grades"))
  })
  
  output$draft_ranking <- renderDT({ #table 1
    req(input$draft_selection) # require input
    draft_rankings(input$draft_selection, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 12,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$best_picks_title <- renderUI({ #title
    req(input$draft_selection) #require input
    h3(str_c(str_to_title(input$draft_selection), " Best Picks"))
  })
  
  output$best_picks <- renderDT({ #table 2
    req(input$draft_selection) # require input
    best_picks(input$draft_selection, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$worst_picks_title <- renderUI({ #title
    req(input$draft_selection) #require input
    h3(str_c(str_to_title(input$draft_selection), " Worst Picks"))
  })
  
  output$worst_picks <- renderDT({ #table 3
    req(input$draft_selection) # require input
    worst_picks(input$draft_selection, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  # Reactivity for Page 4
  
  output$trade_winners_title <- renderUI({ #title
    h3("Trade Winners")
  })
  
  output$trade_winners <- renderDT({ #table 1
    overall_trade_winners %>%
      shiny_edit_tables() %>%
      datatable(
        options = list(
          pageLength = 12,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$trade_lopsided_title <- renderUI({ #title
    h3("Lopsided Trades")
  })
  
  output$trade_lopsided <- renderDT({ #table 2
    lopsided_trades %>%
      shiny_edit_tables() %>%
      rename("Trade ID" = "Trade Id") %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$individual_trade_title <- renderUI({ #title
    req(input$trade_selection) #require input
    h3(total_trade_value[[input$trade_selection]] %>% select(team_name) %>% distinct() %>%
         map_chr(~str_c(.x, collapse = ", ")) %>%
         str_c(total_trade_value[[input$trade_selection]]$season[1], " W",
               total_trade_value[[input$trade_selection]]$week[1], " Trade between ", .))
  })
  
  output$individual_trade <- renderDT({ #table 3
    req(input$trade_selection) #require input
    inspect_individual_trade(input$trade_selection, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 11,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  # Reactivity for Page 5
  
  output$transaction_winners_title <- renderUI({ #title
    h3("Transaction Winners")
  })
  
  output$transaction_winners <- renderDT({ #table 1
    overall_transaction_winners %>%
      shiny_edit_tables() %>%
      datatable(
        options = list(
          pageLength = 12,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$top_transaction_title <- renderUI({ #title
    h3("Top Transactions")
  })
  
  output$top_transaction <- renderDT({ #table 2
    top_transactions %>%
      shiny_edit_tables() %>%
      rename("Transaction ID" = "Transaction Id") %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$individual_transaction_title <- renderUI({ #title
    req(input$transaction_selection) #require input
    h3(total_transaction_value[[input$transaction_selection]]$team_name[1] %>%
         str_c(., "'s ", total_transaction_value[[input$transaction_selection]]$season[1], " W",
               total_transaction_value[[input$transaction_selection]]$week[1], " Transaction"))
  })
  
  output$individual_transaction <- renderDT({ #table 3
    req(input$transaction_selection) #require input
    inspect_individual_transaction(input$transaction_selection, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 2,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  # Reactivity for Page 6
  
  output$matchups_title <- renderUI({ #title
    req(input$team_name)
    h3(str_c(input$team_name, "'s Matchup Record"))
  })
  
  output$matchup_history <- renderDT({ #table 1
    req(input$team_name, input$enter_round, input$team_seasons) #require input
    team_records_df %>% team_matchup_record(input$team_name, input$enter_round, input$team_seasons, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 12,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  # Reactivity for Page 8
  
  output$championship_odds_title <- renderUI({ #title
    h3("Championship Odds")
  })
  
  output$championship_odds <- renderDT({ #table 1
    champion_odds %>% shiny_edit_tables() %>% 
      datatable(
        options = list(
          pageLength = 12,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$most_common_finish_title <- renderUI({ #title
    req(input$standings_season)
    h3(str_c(input$standings_season, " Most Likely Finish"))
  })
  
  output$most_common_finish <- renderDT({ #table 2
    req(input$standings_season) #require input
    most_common_finish_df %>% most_common_finish(input$standings_season, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 12,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
}
