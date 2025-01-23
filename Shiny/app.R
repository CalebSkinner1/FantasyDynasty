# Shiny App
library("shiny")
library("shinydashboard")
library("here")
library("DT")
library("tidyverse"); theme_set(theme_minimal())

# Specify the folder containing the R scripts
folder_path <- "FantasyDynasty/Scripts/"

# Source the script files in the folder
source(here(data_path, "Scripts/Fantasy Teams.R")) # Fantasy Teams
source(here(data_path, "Scripts/Individual Players.R")) # Individual Players

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Fantasy Football Dynasty League"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Individual Players", tabName = "players", icon = icon("chart-line")),
      menuItem("Fantasy Teams", tabName = "teams", icon = icon("sliders-h")),
      menuItem("Draft Grades", tabName = "draft", icon = icon("sliders-h")),
      menuItem("Trade Grades", tabName = "trade", icon = icon("sliders-h")),
      menuItem("Transaction Grades", tabName = "transaction", icon = icon("sliders-h")),
      menuItem("Future Standings", tabName = "standings", icon = icon("sliders-h")),
      menuItem("Player Rankings", tabName = "rankings", icon = icon("sliders-h")))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        # Home Tab
        tabName = "home",
        h2("Welcome to the Home Page"),
        p("This is a static page with general information.")
      ),
      tabItem( # Page 1 Tab
        tabName = "players",
        h2("Individual Players"),
        selectizeInput(
          inputId = "player_name", 
          label = "Enter a Player's Name", 
          choices = player_total_value$name,
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5  # Limit the number of suggestions shown
          )),
        plotOutput("plot_future_value"),
        selectizeInput(
          inputId = "player_season", 
          label = "Enter Season", 
          choices = "2024",
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
        h2("Fantasy Teams"),
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
        h2("Draft Grades"),
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
        
        
        
        selectizeInput(
          inputId = "trade_selection", 
          label = "Enter Trade Number", 
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
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactivity for Page 1
  
  output$plot_future_value <- renderPlot({ #first plot
    plot_future_value(input$player_name)
  })
  
  
  output$tabulate_realized_value_title <- renderUI({ #title
    req(input$player_name, input$player_season) #require input
    h3(str_c(input$player_name, " Seasons"))
  })
  
  output$tabulate_realized_value <- renderDT({ #table
    req(input$player_season)
    tabulate_realized_value(value_added_24, input$player_name, input$player_season, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          autoWidth = TRUE,       # Adjust column width automatically
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
          autoWidth = TRUE,       # Adjust column width automatically
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
          autoWidth = TRUE,       # Adjust column width automatically
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
          autoWidth = TRUE,       # Adjust column width automatically
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
          autoWidth = TRUE,       # Adjust column width automatically
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
          autoWidth = TRUE,       # Adjust column width automatically
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
          autoWidth = TRUE,       # Adjust column width automatically
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
          autoWidth = TRUE,       # Adjust column width automatically
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
          autoWidth = TRUE,       # Adjust column width automatically
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
          autoWidth = TRUE,       # Adjust column width automatically
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$best_picks_title <- renderUI({ #title
    req(input$draft_selection) #require input
    h3(str_c(str_to_title(input$draft_selection), " Best Picks"))
  })
  
  output$best_picks <- renderDT({ #table 1
    req(input$draft_selection) # require input
    best_picks(input$draft_selection, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          autoWidth = TRUE,       # Adjust column width automatically
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
  output$worst_picks_title <- renderUI({ #title
    req(input$draft_selection) #require input
    h3(str_c(str_to_title(input$draft_selection), " Worst Picks"))
  })
  
  output$worst_picks <- renderDT({ #table 1
    req(input$draft_selection) # require input
    worst_picks(input$draft_selection, shiny = TRUE) %>%
      datatable(
        options = list(
          pageLength = 5,         # Set the initial number of rows per page
          autoWidth = TRUE,       # Adjust column width automatically
          ordering = TRUE,        # Enable column sorting
          scrollX = TRUE          # Allow horizontal scrolling if columns exceed width
        ))
  })
  
}

# Run the App
shinyApp(ui, server)
