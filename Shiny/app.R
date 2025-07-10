# Shiny App

# doesn't work for current version of R, so I backed up to 4.4.2

library("shiny")
library("shinydashboard")
library("DT")

# only if running on local machine
print("App starting...") # for logs
if (interactive()) {
  setwd(here::here("Shiny"))
}

# Source the function files in the folder
source("app_call_data.R")
print("Loaded functions") # for logs

# Define UI
ui <- dashboardPage(
  skin = "green",
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
      menuItem("Future Standings", tabName = "future_standings", icon = icon("ranking-star")),
      menuItem("Player Rankings", tabName = "rankings", icon = icon("arrow-trend-up")),
      menuItem("Team Rankings", tabName = "team_rankings", icon = icon("rocket")),
      menuItem("Modeling", tabName = "modeling", icon = icon("chart-simple")),
      menuItem("History", tabName = "history", icon = icon("bank")))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        # Home Tab
        tabName = "home",
        titlePanel("Fantasy Football Dynasty League"),
        p("Hello! This website hosts results and analysis for the Baylor Seniors Fantasy Football Superflex Dynasty League. The foundation of the analysis
        rests on the novel", strong("value added"), "metric. I model player's future success and organize data in an (hopefully)
        accessible method. This website contains relevant and niche information that informs the twelve users of
        the league. It should supplement- and not replace- the league information you find on Sleeper and the popular fantasy player valuing
          websites like", a("Keep Trade Cut", href = "https://keeptradecut.com/dynasty-rankings", target = "_blank"), "or",
          a("Dynasty Daddy.", href = "https://dynasty-daddy.com", target = "blank"), "In fact, several of my models leverage
          these popular ratings. That said, I believe users will find insights unique to this website, and I firmly take
          credit for any success or newfound knowledge any fantasy users derive. As for any mistakes or poor
          advice -- well -- thank you forsupporting your local statistican."),
        h4("Value Added"),
        p("A fantasy players'", strong("value added"), "measures a fantasy player's actual value contributed to a user. I measure a players' value
        added as the difference in his fantasy output with the output of a replacement level player of
        the same position. Players only contribute (or lose) value added if started in the lineup. Thus, a fantasy player in the Baylor Seniors
        league may have a different value added than the same fantasy player in a different league. This is intentional. A player's value is relative
        to the team in which he resides. Players yield no benefits on the bench; a team with two quality QBs needs another less than a
        team with only one quality QB."),
        p("The replacement level value is the same for all players of the same position and is defined as the top bench player
        or free agent at that position. In essence, the replacement level value is the hypothetical substitute if a particular starter
        is removed from the lineup. Each team has a replacement level player for each position, and I compute the replacement level value with an
         average of their output."),
         p("A players' value added can change drastically depending on the player's position and the team's composition. For example, a user starts
         three RBs in one particular week, so their replacement level output is the top bench or free agent FLEX player.
         However, the following week, the user starts only two RBs, so the RB's replacement level output is the top bench
         or free agent RB. This adjustment impacts a players' value added, just like it impacts a player's value to his fantasy owner.
          This dynamic rating system adjusts players' value to the fantasy team's composition."),
        h4("Future Value"),
        p("Put simply, a player's ", strong("future value"), " is his expected value added in future seasons. It's difficult to project this
        directly, so I model a player's future value as a function of his position, age, and current keep trade cut value. I employ a
        bayesian additive regression tree to predict each player's value added in future years.
        (If you want to know the full details please see my ",
          a("GitHub", href = "https://github.com/CalebSkinner1/FantasyDynasty", target = "_blank"), "for more information).
        With this model, I am able to draw samples from future years and simulate each player's career. I total the results from these
        simulations to compute a player's future value. In line with common financial and economic models,
        I discount the future value of each successive year by 5%."),
        h4("Other Models"),
        p("The simulations of players' future value open the door for other models and insights. I use the players' realized value and future
          value to esimate the value of rookie draft picks. I use these values to grade users' drafts, trades, and transactions and detail
          a team's fantasy assets. The simulations even faciliate the creation of championship odds for the next three years.
          There's a lot of possibilities, and in many ways, we're just getting started."),
        h4("A Disclaimer"),
        p("As with any model, these projections have their fair share of deficiencies. First, the model is trained on only a single year of data. This means
        the model struggles to anticipate large career altering changes in a players' career. Football, of course, is filled injuries and unpredictable events.
        This model underestimates the volatility of the players' careers, and, in turn, overvalues youth. I anticipate significant
        improvements as time progresses, and I am able to account for the entire trajectory of a players' career.
        Honestly, it is remarkable that this single year of data can be extrapolated to model a players' entire career.")),
      
      tabItem( # Page 1 Tab
        tabName = "players",
        titlePanel("Individual Players"),
        p("Select any player and this page will report the players' value added and future value. In general, the future value for each year is the
          median of 1000 samples, while the colored regions are the 80% and 95% credible regions."),
        selectizeInput(
          inputId = "player_name", 
          label = "Enter a Player's Name", 
          choices = player_total_value$name,
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5  # Limit the number of suggestions shown
          )),
        DTOutput("player_basic_info"),
        plotOutput("plot_future_value", height = "400px", width = "100%"),
        p("Enter a season to see the player's value added and fantasy points scored."),
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
        fluidRow(
          column(9,
                 titlePanel("Fantasy Teams"),
                 p("Select a user to view the fantasy teams' top assets and acquisitions."),
                 
                 selectizeInput(
                   inputId = "team_name", 
                   label = "Enter a Team's Name", 
                   choices = users$display_name,
                   options = list(
                     placeholder = "Start typing...",
                     maxOptions = 5  # Limit the number of suggestions shown
                   ))),
          column(3, uiOutput("image_ui"))
        ),
        uiOutput("team_assets_title"),
        p("Top future assets on the team."),
        DTOutput("team_assets"),
        uiOutput("position_outlook_title"),
        p("Asset value by position."),
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
        p("Top contributors in this season."),
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
        p("Weekly Value"),
        DTOutput("team_contributors_weekly"),
        uiOutput("avenue_grades_title"),
        p("Performance across acquisition avenues."),
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
        p("Filter by avenue to see top trade, draft pick, transaction, etc."),
        DTOutput("top_acquisitions"),
        uiOutput("worst_acquisitions_title"),
        p("Filter by avenue to see worst trade, draft pick, transaction, etc."),
        DTOutput("worst_acquisitions"),
        uiOutput("team_composition_title"),
        p("Proportion of total value on roster acquired from various avenues."),
        DTOutput("team_composition"),
      ),
      
      tabItem( # Page 3 Tab
        tabName = "draft",
        titlePanel("Draft Grades"),
        p("Select a draft to view the top picks and best overall drafts. The users' expected value is the
        anticipated value accrued from the users' set of picks. This provides a standard in which to compare drafts."),
        selectizeInput(
          inputId = "draft_selection", 
          label = "Enter Draft", 
          choices = c("2024 rookie draft", "2025 rookie draft", "initial draft", "All"),
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
        p("The total value gained or lost from all trades."),
        DTOutput("trade_winners"),
        uiOutput("trade_lopsided_title"),
        p("The most unfair trades in league history. This is assessed now, not the time of the trade."),
        DTOutput("trade_lopsided"),
        p("Select a single trade to see a full breakdown of the value gained and lost for each user."),
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
        p("The total value gained or lost from all transactions."),
        DTOutput("transaction_winners"),
        uiOutput("top_transaction_title"),
        p("The most successful transactions in league history. This is assessed now, not the time of the transaction."),
        DTOutput("top_transaction"),
        p("Select a single transaction to see a full breakdown of the value gained and lost from added and dropped players."),
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
        tabName = "matchups",
        titlePanel("Matchups History"),
        p("Select a team to see historical records against opponents."),
        selectizeInput(
          inputId = "team_name",
          label = "Enter a Team's Name",
          choices = users$display_name,
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5  # Limit the number of suggestions shown
          )),
        selectizeInput(
          inputId = "team_seasons",
          label = "Enter Season",
          choices = unique(team_records_df$season),
          multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 2  # Limit the number of suggestions shown
          )),
        selectizeInput(
          inputId = "enter_round",
          label = "Enter Round",
          choices = c("All", unique(team_records_df$round)),
          multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 2  # Limit the number of suggestions shown
          )),
        uiOutput("matchups_title"),
        
        DTOutput("matchup_history"),
      ),
      tabItem( # Page 7 Tab
        tabName = "future_standings",
        titlePanel("Future Standings"),
        p("I simulate final standings over the next three years 5000 times. In each simulation,
          I project the value added for each player and find the total value added for each fantasy team.
          I inject a bit of randomness to account for the randomness of seeding and playoffs. The championship odds and
          most common finish are below."),
        
        uiOutput("championship_odds_title"),
        DTOutput("championship_odds"),
        
        selectizeInput(
          inputId = "standings_season",
          label = "Enter Season",
          choices = unique(most_common_finish_df$season),
          options = list(
            placeholder = "Start typing...",
            maxOptions = 3  # Limit the number of suggestions shown
          )),
        uiOutput("most_common_finish_title"),
        p("The most likely finish for each Fantasy Team."),
        DTOutput("most_common_finish")
      ),
      tabItem( # Page 8 Tab
        tabName = "rankings",
        titlePanel("Player Rankings"),
        p("This is a static page that will be completed at a later date. Here, I plan to show a player's future value over time.")
      ),
      tabItem( # Page 9 Tab
        tabName = "team_rankings",
        titlePanel("Team Rankings"),
        p("Team rankings by ELO and total future value of assets."),
        
        uiOutput("elo_rankings_title"),
        p("ELO is a simple rating system developed by physicist Dr. Arpad Elo. Teams gain points for winning and lose points
          for losing. The strength of opponent and margin of victory are taken into account. The average score is 1500 and ratings
          are discounted by 25% at the end of each season. ELO gives more weight to recent games, but all games have non-zero impact
          on the rating. ELO is simple to compute and interpret, but it is not a perfect fit for Fantasy sports. Teams have no direct
          impact on their opponent's score, but their 'Points Allowed' are factored into the score. Nevertheless, ELO is an useful tool
          to understand team strength over time."),
        plotlyOutput("elo_rankings", height = "400px", width = "100%"),
        p("ELO also gives implied win probabilities. Select two teams to compare their win probability."),
        fluidRow(
          column(6,
                 selectizeInput(
                   inputId = "team1", 
                   label = "Enter a Team's Name", 
                   choices = users$display_name,
                   options = list(
                     placeholder = "Start typing...",
                     maxOptions = 5  # Limit the number of suggestions shown
                   ))),
          column(6,
                 selectizeInput(
                   inputId = "team2", 
                   label = "Enter a Team's Name", 
                   choices = users$display_name,
                   options = list(
                     placeholder = "Start typing...",
                     maxOptions = 5  # Limit the number of suggestions shown
                   )))
        ),
        DTOutput("elo_win_prob"),
        
        uiOutput("future_assets_title"),
        DTOutput("future_assets")
      ),
      tabItem( # Page 10 Tab
        tabName = "modeling",
        titlePanel("Model Explanations and Fit"),
        p("This is a static page that will be completed at a later date. Here, I'll explain my models and show their fit.")
      ),
      tabItem( # Page 11 Tab
        tabName = "history",
        titlePanel("League History"),
        p("This is a static page that will be completed at a later date. Here, I'll list the league champions and notable records set in the league.")
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
    h3(str_c(input$team_name, " Top Assets"))
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
  
  # Reactivity for Page 7
  
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
  
  # Reactivity for Page 9
  
  output$elo_rankings_title <- renderUI({ #title
    h3("ELO Rankings")
  })
  
  output$elo_rankings <- renderPlotly({ #first plot
    graph_elo(weekly_elo)
  })
  
  output$elo_win_prob <- renderDT({ # text 1
    req(input$team1, input$team2)
    elos <- weekly_elo %>% group_by(team) %>% slice_max(date_hide)
    
    elo_win_probability(filter(elos, team == input$team1),
                        filter(elos, team == input$team2)) %>%
      datatable(
        options = list(
          pageLength = 2,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$future_assets_title <- renderUI({ #title
    h3("Total Future Assets")
  })
  
  output$future_assets <- renderDT({ #table 2
    all_assets_summary_df %>% shiny_edit_tables() %>%
      datatable(
        options = list(
          pageLength = 12,         # Set the initial number of rows per page
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
}

# Run the App
shinyApp(ui, server)


