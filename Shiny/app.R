# Shiny App
library("shiny")
library("shinydashboard")
library("here")
library("DT")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty"

# Specify the folder containing the R scripts
folder_path <- "FantasyDynasty/Scripts/"

# Source the script files in the folder
source(here(data_path, "Scripts", "Fantasy Teams.R")) # Fantasy Teams
source(here(data_path, "Scripts/Individual Players.R")) # Individual Players

# Define UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Baylor Seniors"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Individual Players", tabName = "players", icon = icon("helmet-un")),
      menuItem("Fantasy Teams", tabName = "teams", icon = icon("user-plus")),
      menuItem("Draft Grades", tabName = "draft", icon = icon("calendar")),
      menuItem("Trade Grades", tabName = "trade", icon = icon("handshake")),
      menuItem("Transaction Grades", tabName = "transaction", icon = icon("right-left")),
      menuItem("Matchups", tabName = "matchups", icon = icon("calendar-days")),
      menuItem("Current Standings", tabName = "current_standings", icon = icon("ranking-star")),
      menuItem("Future Standings", tabName = "future_standings", icon = icon("circle-question")),
      menuItem("Player Rankings", tabName = "rankings", icon = icon("arrow-trend-up")),
      menuItem("Modeling", tabName = "modeling", icon = icon("chart-simple")))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        # Home Tab
        tabName = "home",
        titlePanel("Fantasy Football Dynasty League"),
        p("Hello! This website hosts results and analysis for the Baylor Seniors Fantasy Football Dynasty League. The foundation of the analysis
        rests on the novel", strong("value added"), "metric. From here, I model player's future success and organize data in an (hopefully)
        accessible method. This website is intended to contain relevant and niche information that informs the twelve users within
        the league. It is not intended to compete with popular fantasy player valuing websites like Keep Trade Cut or Dynasty Daddy.
        In fact, several of my models are derived from these popular ratings. That being said, I firmly take credit for any success
        or newfound knowledge any fantasy users derive from this website. You're Welcome."),
        h4("Value Added"),
        p("A fantasy players' value added is intended to represent a fantasy player's actual value contributed to an user. Players yield
        no benefits on the bench; a team with two quality QBs needs a third less than a team with one quality QB. I measure a players' value
        added as the difference in his fantasy output with the output of a replacement level player of
        their position. Players only contribute (or lose) value added if started in the lineup. Thus, a fantasy player in the Baylor Seniors
        league may have a different value added than the same fantasy player in a different league."),
        p("Each position's replacement level player is defined as the top bench player or free agent at every position. The replacement level
           output is the average of these twelve players. Ultimately, a players' value added can change drastically depending on the 
          player's position and the team's composition. For example, if a user starts three RBs in one particular week, then all three RB's
          replacement level output is actually the top bench or free agent FLEX player. However, if the following week, the user starts only two RBs,
          then the RB's replacement level output is the top bench or free agent RB. This is intentional; this dynamic rating system aims to adjust
          players' value to the fantasy team's composition."),
        h4("Future Value"),
        p("Put simply, a player's ", strong("future value"), " is his expected value added in future seasons. It's difficult to project this
        directly, so I measure a player's future value as a function of his position, age, and current keep trade cut value. I employ a
        bayesian hierarchical polynomial regression model to predict each player's value added in future years.
        (It's much more complicated than this. If you want to know more please see my ",
        a("GitHub", href = "https://github.com/CalebSkinner1/FantasyDynasty", target = "_blank"), "for more information).
        With this model, I am able to draw samples from future years and simulate each player's career. I total the results from these
        simulations to compute a player's future value. In line with common financial and economic models,
        I discount the future value of each successive year by 5%.")
        
      ),
      tabItem( # Page 1 Tab
        tabName = "players",
        titlePanel("Individual Players"),
        selectizeInput(
          inputId = "player_name", 
          label = "Enter a Player's Name", 
          choices = player_total_value$name,
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5  # Limit the number of suggestions shown
          )),
        DTOutput("player_basic_info"),
        plotOutput("plot_future_value"),
        selectizeInput(
          inputId = "player_season", 
          label = "Enter Season", 
          choices = c(2024),
          multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 2  # Limit the number of suggestions shown
          )),
        uiOutput("tabulate_realized_value_title"),
        DTOutput("tabulate_realized_value"),
        plotOutput("weekly_results")
      ),
      tabItem( # Page 2 Tab
        tabName = "teams",
        titlePanel("Fantasy Teams"),
        selectizeInput(
          inputId = "team_name", 
          label = "Enter a Team's Name", 
          choices = users$display_name,
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5  # Limit the number of suggestions shown
          )),
        uiOutput("team_assets_title"),
        DTOutput("team_assets"),
        uiOutput("position_outlook_title"),
        DTOutput("position_outlook"),
        selectizeInput(
          inputId = "team_season", 
          label = "Enter Season", 
          choices = "2024",
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 2  # Limit the number of suggestions shown
          )),
        uiOutput("team_contributors_title"),
        DTOutput("team_contributors"),
        selectizeInput(
          inputId = "team_week", 
          label = "Enter Week", 
          choices = c(1:17),
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 3  # Limit the number of suggestions shown
          )),
        uiOutput("team_contributors_weekly_title"),
        DTOutput("team_contributors_weekly"),
        uiOutput("avenue_grades_title"),
        DTOutput("avenue_grades"),
        selectizeInput(
          inputId = "team_avenue", 
          label = "Enter Acquisition Avenue", 
          choices = c("rookie draft", "initial draft", "trade", "transaction", "All"),
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5  # Limit the number of suggestions shown
          )),
        uiOutput("top_acquisitions_title"),
        DTOutput("top_acquisitions"),
        uiOutput("worst_acquisitions_title"),
        DTOutput("worst_acquisitions"),
        uiOutput("team_composition_title"),
        DTOutput("team_composition"),
      ),
      tabItem( # Page 3 Tab
        tabName = "draft",
        titlePanel("Draft Grades"),
        selectizeInput(
          inputId = "draft_selection", 
          label = "Enter Draft", 
          choices = c("2024 rookie draft", "initial draft", "All"),
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 3  # Limit the number of suggestions shown
          )),
        uiOutput("draft_ranking_title"),
        DTOutput("draft_ranking"),
        uiOutput("best_picks_title"),
        DTOutput("best_picks"),
        uiOutput("worst_picks_title"),
        DTOutput("worst_picks")
      ),
      tabItem( # Page 4 Tab
        tabName = "trade",
        h2("Trade Grades"),
        uiOutput("trade_winners_title"),
        DTOutput("trade_winners"),
        uiOutput("trade_lopsided_title"),
        DTOutput("trade_lopsided"),
        selectizeInput(
          inputId = "trade_selection", 
          label = "Enter Trade ID", 
          choices = unique(comparison$trade_id),
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 3  # Limit the number of suggestions shown
          )),
        uiOutput("individual_trade_title"),
        DTOutput("individual_trade")
      ),
      tabItem( # Page 5 Tab
        tabName = "transaction",
        titlePanel("Transaction Grades"),
        uiOutput("transaction_winners_title"),
        DTOutput("transaction_winners"),
        uiOutput("top_transaction_title"),
        DTOutput("top_transaction"),
        selectizeInput(
          inputId = "transaction_selection", 
          label = "Enter Transaction ID", 
          choices = unique(transaction_comparison$transaction_id),
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 3  # Limit the number of suggestions shown
          )),
        uiOutput("individual_transaction_title"),
        DTOutput("individual_transaction")
      ),
      tabItem( # Page 6 Tab
        tabName = "standings",
        titlePanel("Matchups"),
        p("This is a static page that will be completed at a later date.")
      ),
      tabItem( # Page 7 Tab
        tabName = "current_standings",
        titlePanel("Current Standings"),
        p("This is a static page that will be completed at a later date.")
      ),
      tabItem( # Page 8 Tab
        tabName = "future_standings",
        titlePanel("Future Standings"),
        p("This is a static page that will be completed at a later date.")
      ),
      tabItem( # Page 9 Tab
        tabName = "rankings",
        titlePanel("Player Rankings"),
        p("This is a static page that will be completed at a later date.")
      ),
      tabItem( # Page 10 Tab
        tabName = "modeling",
        titlePanel("Model Explanations and Fit"),
        p("This is a static page that will be completed at a later date.")
      )
    )
  )
)

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
    plot_future_value(input$player_name)
  })
  
  output$tabulate_realized_value_title <- renderUI({ #title
    req(input$player_name, input$player_season) #require input
    h3(str_c(input$player_name, " Seasons"))
  })
  
  output$tabulate_realized_value <- renderDT({ #table 2
    req(input$player_season)
    tabulate_realized_value(value_added_24, input$player_name, input$player_season, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$weekly_results <- renderPlot({ #second plot
    req(input$player_season) #require input
    weekly_results(value_added_24, input$player_name, input$player_season)
  })
  
  # Reactivity for Page 2
  
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
      select(roster_id) %>%
      pull() %>% grab_team_contributors(input$team_season, shiny = TRUE) %>%
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
          pageLength = 5,         # Set the initial number of rows per page
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
  
}

# Run the App
shinyApp(ui, server)

