# Shiny App

# doesn't work for current version of R, so I backed up to 4.4.2

print("Loading packages...")
library("shiny")
library("DT")
library("shinydashboard")

# only if running on local machine
print("App starting...") # for logs

print("Source app_call_data.R")
source("app_call_data.R")
print("functions are loaded") # for logs

# Define UI ---------------------------------------------------------------
print("Run UI")
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
      menuItem("Current Standings", tabName = "current_standings", icon = icon("ranking-star")),
      menuItem("Future Standings", tabName = "future_standings", icon = icon("circle-question")),
      menuItem("Player Rankings", tabName = "rankings", icon = icon("arrow-trend-up")),
      menuItem("Team Rankings", tabName = "team_rankings", icon = icon("arrow-trend-up")),
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
        accessible method. This website is intended to contain relevant and niche information that informs the twelve users of
        the league. It is not intended to compete with popular fantasy player valuing websites like",
          a("Keep Trade Cut", href = "https://keeptradecut.com/dynasty-rankings", target = "_blank"), "or",
          a("Dynasty Daddy", href = "https://dynasty-daddy.com", target = "blank"),". In fact, several of my models leverage these popular ratings.
        That said, I firmly take credit for any success or newfound knowledge any fantasy users derive from this website.
        As for any mistakes or poor advice -- well -- thank you for supporting your local statistican."),
        h4("Value Added"),
        p("A fantasy players' value added represents a fantasy player's actual value contributed to an user. I measure a players' value
        added as the difference in his fantasy output with the output of a replacement level player of
        the same position. Players only contribute (or lose) value added if started in the lineup. Thus, a fantasy player in the Baylor Seniors
        league may have a different value added than the same fantasy player in a different league. This is intentional. A players value is inherently relative
        to the team in which he resides. Players yield no benefits on the bench; a team with two quality QBs needs a third less than a
        team with only one quality QB."),
        p("Each position's replacement level player is defined as the top bench player or free agent at every position. The replacement level
           output is the average of these twelve players. Ultimately, a players' value added can change drastically depending on the 
          player's position and the team's composition. For example, if a user starts three RBs in one particular week, then their
          replacement level output is actually the top bench or free agent FLEX player. However, if the following week, the user starts only two RBs,
          then the RB's replacement level output is the top bench or free agent RB. This adjustment can have a dramatic impact on players' value.
          This dynamic rating system aims to adjust players' value to the fantasy team's composition."),
        h4("Future Value"),
        p("Put simply, a player's ", strong("future value"), " is his expected value added in future seasons. It's difficult to project this
        directly, so I model a player's future value as a function of his position, age, and current keep trade cut value. I employ a
        bayesian additive regression tree to predict each player's value added in future years.
        (It's much more complicated than this. If you want to know more please see my ",
          a("GitHub", href = "https://github.com/CalebSkinner1/FantasyDynasty", target = "_blank"), "for more information).
        With this model, I am able to draw samples from future years and simulate each player's career. I total the results from these
        simulations to compute a player's future value. In line with common financial and economic models,
        I discount the future value of each successive year by 5%."),
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
          choices = unique(value_added$season),
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
        tabName = "current_standings",
        titlePanel("Current Standings"),
        p("This is a static page that will be completed at a later date.")
      ),
      tabItem( # Page 8 Tab
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
      tabItem( # Page 9 Tab
        tabName = "rankings",
        titlePanel("Player Rankings"),
        p("This is a static page that will be completed at a later date.")
      ),
      tabItem( # Page 10 Tab
        tabName = "team_rankings",
        titlePanel("Team Rankings"),
        p("This is a static page that will be completed at a later date.")
      ),
      tabItem( # Page 11 Tab
        tabName = "modeling",
        titlePanel("Model Explanations and Fit"),
        p("This is a static page that will be completed at a later date.")
      ),
      tabItem( # Page 12 Tab
        tabName = "history",
        titlePanel("League History"),
        p("This is a static page that will be completed at a later date.")
      )
    )
  )
)

# Server ------------------------------------------------------------------

print("Sourcing server.R")
source("server.R")

print("Launching shinyApp...")
shinyApp(ui, server)


