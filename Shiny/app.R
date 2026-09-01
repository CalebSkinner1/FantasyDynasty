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
      id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("house")),

      menuItem(
        "Lookup",
        icon = icon("magnifying-glass"),
        startExpanded = TRUE,
        menuSubItem("Individual Players", tabName = "players"),
        menuSubItem("Fantasy Teams", tabName = "teams"),
        menuSubItem("Team Rankings", tabName = "team_rankings")
      ),

      menuItem(
        "Retrospective",
        icon = icon("clock-rotate-left"),
        menuSubItem("Draft Grades", tabName = "draft"),
        menuSubItem("Transaction Grades", tabName = "transaction"),
        menuSubItem("Trade Grades", tabName = "trade")
      ),

      menuItem(
        "Projections",
        icon = icon("arrow-trend-up"),
        menuSubItem("Trade Machine", tabName = "trade_machine"),
        menuSubItem("Future Standings", tabName = "future_standings"),
        menuSubItem("Player Rankings", tabName = "rankings")
      ),

      menuItem(
        "Archives",
        icon = icon("box-archive"),
        menuSubItem("Matchups", tabName = "matchups"),
        menuSubItem("History", tabName = "history")
      ),

      menuItem("Modeling", tabName = "modeling", icon = icon("chart-simple"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        # Home Tab
        tabName = "home",
        titlePanel("Fantasy Football Dynasty League"),
        p(
          "Hello! This site hosts results and analysis for the Baylor Seniors Fantasy Football Superflex Dynasty League. The analysis
        is built on the",
          strong("value added"),
          "metric, a measure of what a player actually contributes to the fantasy team that starts him. I model future player value and organize the data in a way
        that I hope is genuinely useful for the league's twelve managers. Treat this as a supplement to Sleeper and popular player valuation sites like",
          a(
            "KeepTradeCut",
            href = "https://keeptradecut.com/dynasty-rankings",
            target = "_blank"
          ),
          "and",
          a(
            "Dynasty Daddy.",
            href = "https://dynasty-daddy.com",
            target = "_blank"
          ),
          "Several of my models lean on these ratings. Even so, you'll find insights unique to this website, and I take
          full credit for any success or newfound knowledge they produce. :)"
        ),
        h3("Where should I start?"),
        p("The tabs fall into four groups."),

        p(
          strong("Lookup."),
          "Individual Players and Fantasy Teams are the lookup tabs, one player or one roster at a time.
        Each shows value added to date alongside projected value going forward. Team Rankings orders the league
        by Elo rating and by the total future value of their assets."
        ),

        p(
          strong("Retrospective."),
          "Draft Grades, Transaction Grades, and Trade Grades assign present-day
        value to completed decisions. This is useful for identifying championship patterns."
        ),

        p(
          strong("Projections."),
          "Trade Machine, Future Standings, and Player Rankings forecast future output
        and results. Start here if you're weighing a move."
        ),
        p(
          strong("Archives."),
          "Matchups and History chronicle past results so managers can recall their exploits."
        ),
        h3("How does this all work?"),
        h4("Value Added"),
        p(
          "A player's",
          strong("value added"),
          "is the difference between his fantasy output and the output of a replacement-level player at the same position.
          Only players in the starting lineup accumulate value added. That means the same player can be worth more
          in this league than in another. This is intentional. A player's value is relative to the roster on which he resides. A team with two quality quarterbacks
          needs a third less than a team with one."
        ),
        p(
          "Replacement level is shared by every player at a position and is defined as the best available bench player
        or free agent at that position. It is the hypothetical substitute if a particular starter
        is removed from the lineup. Each team has a replacement level player for each position, and I estimate the global replacement level value by averaging
        the performance of the hypothetical replacement for all twelve teams."
        ),
        p(
          "A player's value added can change drastically depending on the player's position and the team's composition. For example, a user starts
         three RBs in one particular week, so their replacement level output is the top bench or free agent FLEX player.
         However, the following week, the user starts only two RBs, so the RB's replacement level output is the top bench
         or free agent RB. This adjustment impacts a player's value added, just like it impacts a player's value to his fantasy owner.
          This dynamic rating system adjusts player's value to the fantasy team's composition."
        ),
        h4("Future Value"),
        p(
          "A player's ",
          strong("future value"),
          "is his expected value added in the seasons to come. I model a player's future value as a function of his position, age, and current KeepTradeCut value,
          using a Bayesian additive regression tree to predict player value added in each future year. The full details live in the",
          strong("Modeling"),
          "tab and on my",
          a(
            "GitHub.",
            href = "https://github.com/CalebSkinner1/FantasyDynasty",
            target = "_blank"
          ),
          "The model generates samples from those future years which simulate a player's career from end to end.
        Summing the results over the next eight years gives the player's future value. Following standard practice
        in finance and economics, I discount each successive year by 5%."
        ),
        h4("Other Models"),
        p(
          "The simulations of players' entire careers open the door for other models and insights.
        Combining realized value with future value estimates what rookie draft picks are worth, which
        in turn permits draft, trade, and transaction grades.
        Altogether, these grades and valuations give inventory of a team's assets and produce championship odds for the next three seasons.
        There's plenty left to build, and we're just getting started."
        ),
        h4("A Disclaimer"),
        p(
          "The future value models are trained on only two years of data, so they may struggle to anticipate career-altering
          turns that football produces constantly. The practical effect is that they understate volatility and, in turn,
          may underestimate the risk of young unproven players.
          I expect real improvement as seasons accumulate and the full arc of a career may be modeled."
        )
      ),

      tabItem(
        # Page 1 Tab
        tabName = "players",
        fluidRow(
          column(
            9,
            titlePanel("Individual Players"),
            p(
              "Select any player and this page will report the player's value added and future value. The future value for each year is the
          median of 5000 samples, while the colored regions are the 80% and 95% credible regions."
            ),
            selectizeInput(
              inputId = "player_name",
              label = "Enter a Player's Name",
              choices = filter(
                player_total_value,
                position %in% c("QB", "WR", "RB", "TE")
              ) |>
                pull(name),
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            )
          ),
          column(3, uiOutput("player_image"))
        ),
        DTOutput("player_basic_info"),
        plotOutput("plot_future_value", height = "400px", width = "100%"),
        p(
          "Enter a season to see the player's value added and fantasy points scored."
        ),
        selectizeInput(
          inputId = "player_season",
          label = "Enter Season",
          choices = unique(season_value_added$season),
          multiple = TRUE, #enable multiple selections
          selected = max(season_value_added$season),
          options = list(
            placeholder = "Start typing...",
            maxOptions = length(unique(season_value_added$season)) # Limit the number of suggestions shown
          )
        ),
        uiOutput("tabulate_realized_value_title"),
        DTOutput("tabulate_realized_value"),
        p(
          "This visual will show the player's weekly output, projected output, and value added over the course of a season.
          It defaults to the most recent season selected of the choices above."
        ),
        plotOutput("weekly_results")
      ),

      tabItem(
        # Page 2 Tab
        tabName = "teams",
        fluidRow(
          column(
            9,
            titlePanel("Fantasy Teams"),
            p(
              "Select a user to view the team's top assets and acquisitions."
            ),

            selectizeInput(
              inputId = "team_name",
              label = "Enter a Team's Name",
              choices = users$display_name,
              selected = users$display_name[1],
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            )
          ),
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
          choices = unique(season_value_added$season),
          multiple = TRUE, #enable multiple selections
          selected = max(season_value_added$season),
          options = list(
            placeholder = "Start typing...",
            maxOptions = length(unique(season_value_added$season)) # Limit the number of suggestions shown
          )
        ),
        uiOutput("team_contributors_title"),
        p("Top contributors in this season."),
        DTOutput("team_contributors"),
        selectizeInput(
          inputId = "team_week",
          label = "Enter Week",
          choices = c(1:17),
          # multiple = TRUE, #enable multiple selections
          selected = 1,
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5 # Limit the number of suggestions shown
          )
        ),
        uiOutput("team_contributors_weekly_title"),
        p(
          "Weekly starters. This will default to the most recent season selected."
        ),
        DTOutput("team_contributors_weekly"),
        uiOutput("avenue_grades_title"),
        p("Performance across acquisition avenues."),
        DTOutput("avenue_grades"),
        selectizeInput(
          inputId = "team_avenue",
          label = "Enter Acquisition Avenue",
          choices = c(
            "rookie draft",
            "initial draft",
            "trade",
            "transaction",
            "All"
          ),
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5 # Limit the number of suggestions shown
          )
        ),
        uiOutput("top_acquisitions_title"),
        p("Filter by avenue to see top trade, draft pick, transaction, etc."),
        DTOutput("top_acquisitions"),
        uiOutput("worst_acquisitions_title"),
        p("Filter by avenue to see worst trade, draft pick, transaction, etc."),
        DTOutput("worst_acquisitions"),
        uiOutput("team_composition_title"),
        p("Proportion of total value on roster acquired from various avenues."),
        DTOutput("team_composition")
      ),

      tabItem(
        # Page 3 Tab
        tabName = "draft",
        titlePanel("Draft Grades"),
        p(
          "Select a draft to view the top picks and best overall drafts. The user's expected value is the
        anticipated value accrued from the user's set of picks. This provides a standard against which to compare drafts."
        ),
        selectizeInput(
          inputId = "draft_selection",
          label = "Enter Draft",
          choices = c(
            unique(picks_df$draft),
            "All"
          ),
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = length(c(unique(picks_df$draft), "All")) # Limit the number of suggestions shown
          )
        ),
        uiOutput("draft_ranking_title"),
        DTOutput("draft_ranking"),
        uiOutput("best_picks_title"),
        DTOutput("best_picks"),
        uiOutput("worst_picks_title"),
        DTOutput("worst_picks")
      ),

      tabItem(
        # Page 4 Tab
        tabName = "transaction",
        titlePanel("Transaction Grades"),
        uiOutput("transaction_winners_title"),
        p("The total value gained or lost from all transactions."),
        DTOutput("transaction_winners"),
        uiOutput("top_transaction_title"),
        p(
          "The most successful transactions in league history. This is assessed now, not at the time of the transaction."
        ),
        DTOutput("top_transaction"),
        p(
          "Select a single transaction to see a full breakdown of the value gained and lost from added and dropped players."
        ),
        selectizeInput(
          inputId = "transaction_selection",
          label = "Enter Transaction ID",
          choices = unique(transaction_comparison$transaction_id),
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5 # Limit the number of suggestions shown
          )
        ),
        uiOutput("individual_transaction_title"),
        DTOutput("individual_transaction")
      ),

      tabItem(
        # Page 5 Tab
        tabName = "trade",
        titlePanel("Trade Grades"),
        uiOutput("trade_winners_title"),
        p("The total value gained or lost from all trades."),
        DTOutput("trade_winners"),
        uiOutput("trade_lopsided_title"),
        p(
          "The most unfair trades in league history. This is assessed now, not at the time of the trade."
        ),
        DTOutput("trade_lopsided"),
        p(
          "Select a single trade to see a full breakdown of the value gained and lost for each user."
        ),
        selectizeInput(
          inputId = "trade_selection",
          label = "Enter Trade ID",
          choices = unique(comparison$trade_id),
          # multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 3 # Limit the number of suggestions shown
          )
        ),
        uiOutput("individual_trade_title"),
        DTOutput("individual_trade")
      ),

      tabItem(
        # Page 6 Tab
        tabName = "trade_machine",
        titlePanel("Trade Machine"),
        p(
          "This Trade Machine compares the projected future value of various assets in a potential trade.
        It outputs the net future value and net next year value for each fantasy team."
        ),
        p(
          "Please note that
        fantasy trades are incredibly contextual. Consider your own team composition
        and championship window carefully before making a trade. Like many trade calculators,
        Trade Machine treats player's values as additive, but, obviously, this is an approximate way to compute
        value (for example, 1 first round pick is more valuable than 3 third round picks, even though the sum of the latter's
        KTC value is higher). The ",
          strong("value added"),
          " metric helps account for this, but nothing is perfect. Also, please note
          that the model has no direct injury input. A player's future value will dynamically respond to an injury,
          but the response is indirect and lagged."
        ),
        p(
          "Enter two teams and select assets from each team to view projected trade grade."
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              inputId = "trade_machine_team1",
              label = "Enter a Team's Name",
              choices = users$display_name,
              selected = users$display_name[1],
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            )
          ),
          column(
            6,
            selectizeInput(
              inputId = "trade_machine_team2",
              label = "Enter a Team's Name",
              choices = users$display_name,
              selected = users$display_name[2],
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              inputId = "team1_assets",
              label = "Select Team 1 Assets",
              choices = NULL,
              multiple = TRUE, #enable multiple selections
              options = list(
                placeholder = "Start typing...",
                maxOptions = 12 # Limit the number of suggestions shown
              )
            )
          ),
          column(
            6,
            selectizeInput(
              inputId = "team2_assets",
              label = "Select Team 2 Assets",
              choices = NULL,
              multiple = TRUE, #enable multiple selections
              options = list(
                placeholder = "Start typing...",
                maxOptions = 12 # Limit the number of suggestions shown
              )
            )
          )
        ),
        fluidRow(
          valueBoxOutput("overall_value", width = 6),
          column(6, uiOutput("immediate_value"))
        ),
        plotOutput("plot_trade_assets", height = "300px", width = "100%"),
        fluidRow(
          column(
            6,
            uiOutput("team1_trade_outlook_title"),
            DTOutput("team1_trade_outlook")
          ),
          column(
            6,
            uiOutput("team2_trade_outlook_title"),
            DTOutput("team2_trade_outlook")
          )
        ),
      ),

      tabItem(
        # Page 7 Tab
        tabName = "future_standings",
        titlePanel("Future Standings"),
        p(
          "I simulate final standings over the next three years 5000 times. In each simulation,
          I project the value added for each player and find the total value added for each fantasy team.
          This informs the teams' win probability for each game in the season. I simulate three consecutive seasons, and
          compute the final standings odds for each team. The championship odds and most common finish are below."
        ),

        uiOutput("championship_odds_title"),
        DTOutput("championship_odds"),

        uiOutput("playoff_odds_title"),
        DTOutput("playoff_odds"),

        uiOutput("bye_odds_title"),
        DTOutput("bye_odds"),

        uiOutput("n1_pick_odds_title"),
        DTOutput("n1_pick_odds"),

        selectizeInput(
          inputId = "standings_season",
          label = "Enter Season",
          choices = unique(most_common_finish_df$season),
          selected = min(most_common_finish_df$season),
          options = list(
            placeholder = "Start typing...",
            maxOptions = length(unique(most_common_finish_df$season)) # Limit the number of suggestions shown
          )
        ),
        uiOutput("most_common_finish_title"),
        p("The most likely finish for each Fantasy Team."),
        DTOutput("most_common_finish")
      ),

      tabItem(
        # Page 8 Tab
        tabName = "rankings",
        titlePanel("Player Rankings"),
        p(
          "Player's future value, ranked and displayed over time.
          Select a position to see the top players of that position."
        ),
        selectizeInput(
          inputId = "enter_position",
          label = "Enter Position",
          choices = c("All", "QB", "RB", "WR", "TE"),
          multiple = TRUE, # enable multiple selections
          selected = "All",
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5 # Limit the number of suggestions shown
          )
        ),

        uiOutput("players_top_future_value_title"),
        DTOutput("players_top_future_value"),

        selectizeInput(
          inputId = "enter_players",
          label = "Enter Players",
          choices = filter(
            player_total_value,
            position %in% c("QB", "WR", "RB", "TE")
          ) |>
            pull(name),
          multiple = TRUE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5 # Limit the number of suggestions shown
          )
        ),

        uiOutput("future_value_over_time_title"),
        p(
          "Demonstrates a player's future value at different points in the league history. Please note that
          these estimations were conducted at irregular intervals until June 2025. Note that a player's value will
          gradually decrease over the course of a season as the player ages and has fewer expected games remaining in his career.
          There are some notable exceptions, typically when a player dramatically exceeds expectations."
        ),
        plotlyOutput("future_value_over_time"),

        uiOutput("comparable_future_value_title"),
        p(
          "Select a single player and see the trajectory of similar players of the same position."
        ),
        selectizeInput(
          inputId = "enter_player",
          label = "Enter one Player",
          choices = filter(
            player_total_value,
            position %in% c("QB", "WR", "RB", "TE")
          ) |>
            pull(name),
          multiple = FALSE, #enable multiple selections
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5 # Limit the number of suggestions shown
          )
        ),
        plotlyOutput("comparable_future_value")
      ),

      tabItem(
        # Page 9 Tab
        tabName = "team_rankings",
        titlePanel("Team Rankings"),
        p("Team rankings by Elo and total future value of assets."),

        uiOutput("elo_rankings_title"),
        p(
          "Elo is a simple rating system developed by physicist Dr. Arpad Elo. Teams gain points for winning and lose points
          for losing. The strength of opponent and margin of victory are taken into account. The average score is 1500 and ratings
          are discounted by 25% at the end of each season. Elo gives more weight to recent games, but all games have non-zero impact
          on the rating. Elo is simple to compute and interpret, but it is not a perfect fit for Fantasy sports. Teams have no direct
          impact on their opponent's score, but their 'Points Against' are factored into the score. For this reason, each
          week, I compute the adjustments as if each fantasy team had played all other teams in the league, and I average the adjustments
          together. This removes any scheduling bias from the rating. Elo is not a forward facing predictive metric, but it is useful
          to understand team strength over time."
        ),
        plotlyOutput("elo_rankings", height = "400px", width = "100%"),
        p(
          "Elo also gives implied win probabilities. Select two teams to compare their win probability."
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              inputId = "team1",
              label = "Enter a Team's Name",
              choices = users$display_name,
              selected = users$display_name[1],
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            )
          ),
          column(
            6,
            selectizeInput(
              inputId = "team2",
              label = "Enter a Team's Name",
              choices = users$display_name,
              selected = users$display_name[2],
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            )
          )
        ),
        DTOutput("elo_win_prob"),

        uiOutput("total_success_title"),
        DTOutput("total_success"),

        uiOutput("future_assets_title"),
        DTOutput("future_assets")
      ),

      tabItem(
        # Page 10 Tab
        tabName = "matchups",
        titlePanel("Matchup History"),
        p("Select a team to see historical records against opponents."),
        fluidRow(
          column(
            4,
            selectizeInput(
              inputId = "matchups_team_name",
              label = "Enter a Team's Name",
              choices = users$display_name,
              selected = users$display_name[1],
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            )
          ),
          column(
            4,
            selectizeInput(
              inputId = "team_seasons",
              label = "Enter Season",
              choices = unique(team_records_df$season),
              multiple = TRUE, #enable multiple selections
              selected = max(team_records_df$season),
              options = list(
                placeholder = "Start typing...",
                maxOptions = length(unique(team_records_df$season)) # Limit the number of suggestions shown
              )
            )
          ),
          column(
            4,
            selectizeInput(
              inputId = "enter_round",
              label = "Enter Round",
              choices = c("All", unique(team_records_df$round)),
              multiple = TRUE, #enable multiple selections
              selected = "All",
              options = list(
                placeholder = "Start typing...",
                maxOptions = length(c("All", unique(team_records_df$round))) # Limit the number of suggestions shown
              )
            )
          )
        ),

        uiOutput("matchups_title"),

        DTOutput("matchup_history"),
      ),

      tabItem(
        # Page 11 Tab
        tabName = "history",
        titlePanel("League History"),
        p(
          "This page lists the league champions and notable records set in the league."
        ),
        uiOutput("championship_title"),
        DTOutput("championship"),

        uiOutput("finalists_title"),
        DTOutput("finalists"),

        uiOutput("playoffs_title"),
        DTOutput("playoffs"),

        fluidRow(
          column(
            6,
            selectizeInput(
              inputId = "enter_season_history",
              label = "Enter Season",
              choices = unique(wins_df$season),
              multiple = TRUE, #enable multiple selections
              selected = max(wins_df$season),
              options = list(
                placeholder = "Start typing...",
                maxOptions = length(unique(wins_df$season)) # Limit the number of suggestions shown
              )
            )
          ),
          column(
            6,
            selectizeInput(
              inputId = "enter_round_history",
              label = "Enter Game Type",
              choices = c("All", unique(wins_df$type)),
              multiple = TRUE, #enable multiple selections
              selected = "All",
              options = list(
                placeholder = "Start typing...",
                maxOptions = (length(unique(wins_df$type)) + 1) # Limit the number of suggestions shown
              )
            )
          )
        ),

        uiOutput("most_wins_title"),
        DTOutput("most_wins"),

        uiOutput("most_points_title"),
        DTOutput("most_points"),

        uiOutput("highest_team_game_title"),
        p(
          "Note: reverse the sort on this column to see the lowest team total in league history."
        ),
        DTOutput("highest_team_game"),

        selectizeInput(
          inputId = "enter_position_history",
          label = "Enter Position",
          choices = c("All", unique(value_added$position)),
          multiple = TRUE, #enable multiple selections
          selected = "All",
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5 # Limit the number of suggestions shown
          )
        ),

        uiOutput("player_highest_season_title"),
        DTOutput("player_highest_season"),

        uiOutput("player_highest_va_season_title"),
        DTOutput("player_highest_va_season"),

        uiOutput("player_best_game_title"),
        DTOutput("player_best_game")
      ),

      tabItem(
        # Page 12 Tab
        tabName = "modeling",
        titlePanel("Model Explanations and Fit"),
        withMathJax(),
        p(
          "This site makes dozens of projections and draws substantial conclusions. Why should you trust them? Let me try to convince you."
        ),
        h4("Value Added"), # mathematically show
        p(
          "Debates are waged and numbers are crunched to project the future of fantasy players, but far less attention is devoted
        to understanding their realized value. As Robert Kiyosaki once said, 'the best way to predict the future is to study the past.'
        To address this, I propose the ",
          strong("value added"),
          "
        metric, which summarizes the contribution a player has made to a fantasy team relative
        to his position-specific replacement level."
        ),
        p(
          "Let \\(x_{j w}\\) denote the fantasy points scored by player \\(j\\) in week \\(w = 1, \\ldots, 17\\)
        of a season. Players earn points only when started, so let
        \\(S_w\\) denote the set of starters in week \\(w\\). A player’s position
        determines their potential replacement. There are eight lineup types:",
          "\\[P = \\{\\text{QB, RB, WR, TE, SUPERFLEX, FLEX, K, DEF} \\}.\\]"
        ),
        p(
          "Replacement players are determined using pre-game projections \\(\\hat{x}_{j w}\\).
        The replacement-level player for position \\(p\\), team \\(T\\), and week \\(w\\) is defined as",
          "\\[j^{*}_{p T w} = \\arg\\max_{j \\in p \\cap S_w^c \\cap T} \\hat{x}_{j w},\\]"
        ),
        p(
          "The realized replacement output is",
          "\\[r_{p T w} = \\sum_{T} \\mathbb{1}_{j \\in T} x_{j^{*}_{p T w} w}.\\]"
        ),
        p(
          "The realized replacement output is very volatile across each week and team. A more stable estimate of the replacement value
        is obtained by averaging across teams:",
          "\\[\\bar{r}_{p w} = \\frac{1}{12}\\sum_{T} r_{p T w}.\\]"
        ),
        p(
          "Even this estimate can fluctuate some week to week, so I smooth the weekly replacement estimate \\(\\bar{r}_{p w}\\) toward
        the season-long average at each position, yielding the weighted-weekly replacement:",
          "\\[wr_{p w} = (1- \\alpha) \\bar{r}_{p w} + \\alpha \\frac{1}{17} \\sum_{u = 1}^{17} \\bar{r}_{p u},\\]"
        ),
        p(
          "where \\(\\alpha \\in (0, 1)\\) is a mixing parameter. I set \\(\\alpha = 0.3\\).
        Finally, value added for player \\(j\\) in week \\(w\\) is defined",
          "\\[
          v_{j w} =
            \\begin{cases}
            x_{j w} - wr_{p w}, & \\text{if } j \\in S_w \\\\
            0, &\\text{otherwise,} \\end{cases}
            \\]"
        ),
        p(
          "and the value added over the course of an entire season is",
          "\\[
          sv_j = \\sum_{w = 1}^{17} v_{j w}.
          \\]"
        ),

        h4("Future Value"), # math, model
        p(
          "Now that we have measurements of each player's worth over the course of an entire season,
        it is useful to model this quantity in order to predict a player's future success prior to a season.
        At the beginning of a season \\(t\\), I observe each player's age \\(a_{j t}\\), KeepTradeCut value \\(k_{j t}\\),
        position \\(p_{j t}\\). I model the player's value added over the course of the subsequent season as",
          "\\[sv_{j, t + 1} = f_{\\text{BART}}(a_{j t}, k_{j t}, p_{j t}) + \\epsilon_{j, t + 1}, \\\\
        \\epsilon_{j, t + 1} = \\sigma_{j, t + 1} z_{j, t + 1}, \\\\
        z_{j, t + 1} \\sim N(0, 1),\\]",
          "where the conditional variance is modeled as",
          "\\[\\log \\sigma_{j, t + 1} = g_{\\text{GAM}}(a_{j t}, k_{j t}, p_{j t}) \\]",
          "The mean structure is modeled using Bayesian Additive Regression Trees (BART),
        while heteroskedasticity in the residuals is accommodated by modeling the log standard deviation as a smooth
        function of the same covariates using a generalized additive model (GAM)."
        ),
        p(
          "It is clear that seasonal value added is not linear in age or KeepTradeCut value. Players tend to peak at a certain age,
        and value added accelerates nonlinearly as KeepTradeCut value increases. While many modeling techniques would struggle to identify
        these patterns, BART is well suited to learning complex, nonlinear relationships. Moreover, younger players tend to exhibit
        greater volatility than older players, and the player's KeepTradeCut and position are informative about their uncertainty.
        Modeling the residual scale using these allows uncertainty quantification to adapt to player-specific conditions."
        ),
        p(
          "Below, I display the projected seasonal value added across a range of predictor values. The shaded portion
        denotes the 80% credible region. Each panel corresponds to a fixed player age, the x-axis
        represents the player's pre-season KeepTradeCut value, and the color indicates the player's position. The stepwise behavior
        observed along the x-axis is characteristic of the BART framework. Notably, uncertainty is substantially larger for younger players."
        ),
        plotOutput("sva_fit", height = "400px", width = "100%"),
        p(
          "To model a player's entire career, seasonal value added is predicted iteratively over multiple years.
        Age and position naturally carry forward from one season to the next, but the KeepTradeCut value requires additional modeling.
        I therefore model a player's KeepTradeCut at the beginning of the next season using an additional BART model:",
          "\\[k_{j t + 1} = f_{\\text{BART}}(a_{j t}, sv_{j t + 1}, p_{j t}, k_{j t}) + \\delta_{j t + 1}, \\\\
        \\delta_{j t + 1} = \\tau_{j t + 1} z_{j t + 1}, \\\\
        z_{j t + 1} \\sim N(0, 1),\\]",
          "where, again, the conditional variance is modeled as",
          "\\[\\log \\tau_{j t + 1} = g_{\\text{GAM}}(a_{j t}, sv_{j t + 1}, p_{j t}, k_{j t}). \\]"
        ),
        p(
          "Here, I plot the projected KeepTradeCut value for a player with a preseason KeepTradeCut value of 5,000.
        As before, the shaded areas represent 80% credible intervals, color denotes player position, and the x-axis
        is the total seasonal value added. Interestingly, the greatest uncertainty is observed among
        underperforming players, particularly at older ages."
        ),
        plotOutput("ktc_fit", height = "400px", width = "100%"),
        p(
          "By iterating these two models, a player's entire career trajectory can be simulated year by year. Posterior samples are generated
        at each season and propagated forward through subsequent models, with downsampling used to mitigate computational cost.
        Additional constraints are imposed to prevent unrealistic extrapolation (for example, predicting performance for 50-year-old players)."
        ),
        p(
          "Finally, a player's future value is defined as a discounted sum of the median predicted seasonal value over the next eight seasons:",
          "\\[f_j = \\sum_{t=1}^8 (1-\\gamma)^t \\hat{sv}_{j t}, \\]",
          "where \\(\\gamma = 0.05 \\) is the temporal discount factor."
        ),

        h4("Draft Picks"), # math, model
        p(
          "One of the most challenging assets to value in a dynasty Fantasy Football league is a rookie draft pick.
        Under this framework, however, draft pick valuation becomes a straightforward modeling task.
        The player's total value is defined as the sum of his realized value and discounted future value:",
          "\\[t_j = \\sum_{t = 1}^{\\tau} sv_{j t} + f_j,\\]",
          "where \\(\\tau\\) denotes the number of seasons the player has already completed."
        ),
        p(
          "To isolate the value of a draft pick, I model a player's total value added as a function of their
        draft position \\(d_j\\):",
          "\\[t_j =  \\beta_0 + \\beta_1 d_j + \\beta_2 \\sqrt{d_j} + \\epsilon_j, \\\\
        \\epsilon_j = \\sigma_j z_j, \\\\
        z_j \\sim N(0, 1).\\]",
          "Here, \\(\\sigma_j^2\\) represents player-specific variance,
        allowing for heteroskedasticity across draft positions."
        ),
        p(
          "I assign the regression coefficients \\(\\beta\\) a standard g-prior and place a conjugate inverse-gamma prior
        on \\(\\sigma_j^2\\) (see Sosa and Aristizabal (2021) for an accessible overview of Bayesian hierarchical linear regression).
        Posterior inference is conducted using a Gibbs sampler with an embedded Metropolis-Hastings step
        to estimate the heteroskedastic variance structure. The full MCMC implementation is available on ",
          a(
            "GitHub",
            href = "https://github.com/CalebSkinner1/FantasyDynasty/blob/main-branch/Modeling/MCMC%20Samplers.R",
            target = "_blank"
          ),
          "."
        ),
        p(
          "I plot drafted players' Total Value Added across their rookie draft position. The blue line is the median projected
        value, while the shaded region corresponds to 95% credible intervals. Hover on the point to see the player's name."
        ),
        plotlyOutput("draft_fit", height = "400px", width = "100%"),

        h4("Future Standings"), # math, model
        p(
          "Finally, I leverage the simulated player career trajectories to predict the outcomes of future
        fantasy matchups. I approximate the strength \\(s_T\\) of a fantasy team \\(T\\) by summing the projected
        season value added of the top twelve players on the roster:
          \\[\\tilde{s}_{T} = \\sum_{j \\in T}^{12} \\tilde{sv}_{j}. \\]"
        ),
        p(
          "I model the total fantasy points scored \\(o_{T w}\\) given the team's strength with
        a simple linear regression:",
          "\\[o_{T w} = \\beta_0 + \\beta_1 \\tilde{s}_{T} + \\beta_2 \\sqrt{\\tilde{s}_{T}} + \\epsilon_{T w}, \\\\
        \\epsilon_{T w} \\sim N(0, \\sigma^2)\\]"
        ),
        p(
          "To simulate each fantasy matchup within a given season, I draw new noise terms
        \\(\\epsilon_{T w} \\sim N(0, \\hat{\\sigma}^2)\\) and generate weekly outcomes:",
          "\\[\\tilde{o}_{T w} = \\hat{\\beta}_0 + \\hat{\\beta}_1 s_T + \\hat{\\beta}_2 \\sqrt{s_T} + \\tilde{\\epsilon}_{T w}. \\]"
        ),
        p(
          "Because the weekly schedule is known, simulated weekly outputs for each team are sufficient to generate
        complete season outcomes. To associate probabilities with each possible finishing position, 5,000 posterior
        samples of each player's projected seasonal value added are generated, and the full
        season simulation is repeated independently for each sample. After simulating both the regular season and playoffs,
        draft positions are determined and incorporated into each team's asset portfolio. By iterating this process,
        I generate projected league standings for the next three seasons."
        ),
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactivity for Page 1

  output$player_image <- renderUI({
    # user image
    req(input$player_name)
    image_url <- player_headshot |>
      filter(name == input$player_name) |>
      slice(1) |>
      pull(headshot_url)

    tags$img(src = image_url, height = "150px")
  })

  output$player_basic_info <- renderDT({
    #table 1
    req(input$player_name)
    basic_info(input$player_name) |> dt(page = 1)
  })

  output$plot_future_value <- renderPlot({
    #first plot
    req(input$player_name)
    p <- plot_future_value(input$player_name)
    plot(p)
  })

  output$tabulate_realized_value_title <- renderUI({
    #title
    req(input$player_name, input$player_season) #require input
    h3(str_c(input$player_name, " Seasons"))
  })

  output$tabulate_realized_value <- renderDT({
    #table 2
    req(input$player_season)
    tabulate_realized_value(
      value_added,
      input$player_name,
      input$player_season,
      shiny = TRUE
    ) |>
      dt(page = 5)
  })

  output$weekly_results <- renderPlot({
    #second plot
    req(input$player_season) #require input
    weekly_results(value_added, input$player_name, input$player_season)
  })

  # Reactivity for Page 2

  output$image_ui <- renderUI({
    # user image
    req(input$team_name)
    image_url <- avatar |>
      filter(display_name == input$team_name) |>
      pull(avatar_url)

    tags$img(src = image_url, height = "150px")
  })

  output$team_assets_title <- renderUI({
    #title
    req(input$team_name) #require input
    h3(str_c(input$team_name, " Top Assets"))
  })

  output$team_assets <- renderDT({
    #table 1
    req(input$team_name) # require input
    users |>
      filter(display_name == input$team_name) |>
      select(roster_id) |>
      pull() |>
      grab_team_assets(shiny = TRUE) |>
      dt(page = 5)
  })

  output$position_outlook_title <- renderUI({
    #title
    req(input$team_name) #require input
    h3(str_c(input$team_name, " Position Outlook"))
  })

  output$position_outlook <- renderDT({
    #table 2
    req(input$team_name) # require input
    users |>
      filter(display_name == input$team_name) |>
      select(roster_id) |>
      pull() |>
      position_outlook(shiny = TRUE) |>
      dt(page = 5)
  })

  output$team_contributors_title <- renderUI({
    #title
    req(input$team_name, input$team_season) #require input
    h3(str_c(
      input$team_name,
      ": ",
      str_flatten(input$team_season, ", "),
      " Season"
    ))
  })

  output$team_contributors <- renderDT({
    #table 3
    req(input$team_name, input$team_season) # require input
    users |>
      filter(display_name == input$team_name) |>
      pull(roster_id) |>
      grab_team_contributors(input$team_season, shiny = TRUE) |>
      dt(page = 5)
  })

  output$team_contributors_weekly_title <- renderUI({
    #title
    req(input$team_name, input$team_season) #require input
    h3(str_c(
      input$team_name,
      ": ",
      max(input$team_season),
      " Season Week ",
      input$team_week
    ))
  })

  output$team_contributors_weekly <- renderDT({
    #table 4
    req(input$team_name, input$team_season, input$team_week) # require input
    users |>
      filter(display_name == input$team_name) |>
      select(roster_id) |>
      pull() |>
      grab_team_contributors_weekly(
        input$team_season,
        input$team_week,
        shiny = TRUE
      ) |>
      dt(page = 5)
  })

  output$avenue_grades_title <- renderUI({
    #title
    req(input$team_name) #require input
    h3(str_c(input$team_name, " Avenue Grades"))
  })

  rid <- reactive({
    req(input$team_name)
    users |>
      filter(display_name == input$team_name) |>
      pull(roster_id)
  })

  output$avenue_grades <- renderDT({
    #table 5
    req(input$team_name) # require input
    overall_grades(value_avenues, rid(), shiny = TRUE) |> dt(page = 5)
  })

  output$top_acquisitions_title <- renderUI({
    #title
    req(input$team_name, input$team_avenue) #require input
    h3(str_c(input$team_name, " Top Acquisitions"))
  })

  output$top_acquisitions <- renderDT({
    #table 6
    req(input$team_name, input$team_avenue) # require input
    top_acquisitions(
      acquisitions,
      rid(),
      enter_avenue = input$team_avenue,
      shiny = TRUE
    ) |>
      dt(page = 5)
  })

  output$worst_acquisitions_title <- renderUI({
    #title
    req(input$team_name, input$team_avenue) #require input
    h3(str_c(input$team_name, " Worst Acquisitions"))
  })

  output$worst_acquisitions <- renderDT({
    #table 7
    req(input$team_name, input$team_avenue) # require input
    worst_acquisitions(
      acquisitions,
      rid(),
      enter_avenue = input$team_avenue,
      shiny = TRUE
    ) |>
      dt(page = 5)
  })

  output$team_composition_title <- renderUI({
    #title
    req(input$team_name) #require input
    h3(str_c(input$team_name, " Team Composition"))
  })

  output$team_composition <- renderDT({
    #table 8
    req(input$team_name) # require input
    team_composition(player_avenues, rid(), shiny = TRUE) |>
      dt(page = 5)
  })

  # Reactivity for Page 3

  output$draft_ranking_title <- renderUI({
    #title
    req(input$draft_selection) #require input
    h3(str_c(str_to_title(input$draft_selection), " Draft Grades"))
  })

  output$draft_ranking <- renderDT({
    #table 1
    req(input$draft_selection) # require input
    draft_rankings(input$draft_selection, shiny = TRUE) |>
      dt(page = 12)
  })

  output$best_picks_title <- renderUI({
    #title
    req(input$draft_selection) #require input
    h3(str_c(str_to_title(input$draft_selection), " Best Picks"))
  })

  output$best_picks <- renderDT({
    #table 2
    req(input$draft_selection) # require input
    best_picks(input$draft_selection, shiny = TRUE) |>
      dt(page = 5)
  })

  output$worst_picks_title <- renderUI({
    #title
    req(input$draft_selection) #require input
    h3(str_c(str_to_title(input$draft_selection), " Worst Picks"))
  })

  output$worst_picks <- renderDT({
    #table 3
    req(input$draft_selection) # require input
    worst_picks(input$draft_selection, shiny = TRUE) |>
      dt(page = 5)
  })

  # Reactivity for Page 4

  output$transaction_winners_title <- renderUI({
    #title
    h3("Transaction Winners")
  })

  output$transaction_winners <- renderDT({
    #table 1
    overall_transaction_winners |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  output$top_transaction_title <- renderUI({
    #title
    h3("Top Transactions")
  })

  output$top_transaction <- renderDT({
    #table 2
    top_transactions |>
      shiny_edit_tables() |>
      rename("Transaction ID" = "Transaction Id") |>
      dt(page = 5)
  })

  output$individual_transaction_title <- renderUI({
    req(input$transaction_selection)
    txn <- total_transaction_value[[input$transaction_selection]]
    h3(str_c(
      txn$team_name[1],
      "'s ",
      txn$season[1],
      " W",
      txn$week[1],
      " Transaction"
    ))
  })

  output$individual_transaction <- renderDT({
    #table 3
    req(input$transaction_selection) #require input
    inspect_individual_transaction(
      input$transaction_selection,
      shiny = TRUE
    ) |>
      dt(page = 12)
  })

  # Reactivity for Page 5

  output$trade_winners_title <- renderUI({
    #title
    h3("Trade Winners")
  })

  output$trade_winners <- renderDT({
    #table 1
    overall_trade_winners |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  output$trade_lopsided_title <- renderUI({
    #title
    h3("Lopsided Trades")
  })

  output$trade_lopsided <- renderDT({
    #table 2
    lopsided_trades |>
      shiny_edit_tables() |>
      rename("Trade ID" = "Trade Id") |>
      dt(page = 5)
  })

  output$individual_trade_title <- renderUI({
    req(input$trade_selection)
    trade <- total_trade_value[[input$trade_selection]]
    teams <- trade |>
      select(team_name) |>
      distinct() |>
      map_chr(~ str_c(.x, collapse = ", "))
    h3(str_c(trade$season[1], " W", trade$week[1], " Trade between ", teams))
  })

  output$individual_trade <- renderDT({
    #table 3
    req(input$trade_selection) #require input
    inspect_individual_trade(input$trade_selection, shiny = TRUE) |>
      dt(page = 12)
  })

  # Reactivity for Page 6

  observeEvent(input$trade_machine_team1, {
    # Get the choices based on selected category
    selected_choices <- filter(
      assets_df,
      display_name == input$trade_machine_team1
    ) |>
      pull(name_code)

    # Update the second selectizeInput with new choices
    updateSelectizeInput(
      session,
      "team1_assets",
      choices = selected_choices,
      selected = NULL
    )
  })

  observeEvent(input$trade_machine_team2, {
    # Get the choices based on selected category
    selected_choices <- filter(
      assets_df,
      display_name == input$trade_machine_team2
    ) |>
      pull(name_code)

    # Update the second selectizeInput with new choices
    updateSelectizeInput(
      session,
      "team2_assets",
      choices = selected_choices,
      selected = NULL
    )
  })

  output$overall_value <- renderValueBox({
    req(input$team1_assets, input$team2_assets)
    trade_valuation <- grade_trade_wrapper(
      assets_df,
      marginal_transaction_value,
      input$team1_assets,
      input$team2_assets
    )
    valueBox(
      value = trade_valuation$ov_statement,
      subtitle = "Net future value",
      icon = icon("scale-balanced"),
      color = "navy",
      width = NULL
    )
  })

  output$immediate_value <- renderUI({
    #immediate value statement
    req(input$team1_assets, input$team2_assets) #require input
    trade_valuation <- grade_trade_wrapper(
      assets_df,
      marginal_transaction_value,
      input$team1_assets,
      input$team2_assets
    )

    h3(trade_valuation$iv_statement)
  })

  output$plot_trade_assets <- renderPlot({
    #first plot
    req(input$team1_assets, input$team2_assets)
    trade_valuation <- grade_trade_wrapper(
      assets_df,
      marginal_transaction_value,
      input$team1_assets,
      input$team2_assets
    )
    p <- graph_trade(trade_valuation$team1, trade_valuation$team2)
    plot(p)
  })

  output$team1_trade_outlook_title <- renderUI({
    #table 1 title
    req(input$trade_machine_team1) # require input

    h3(str_c(input$trade_machine_team1, "'s Assets Received"))
  })

  output$team1_trade_outlook <- renderDT({
    #table 1
    req(input$team1_assets, input$team2_assets) #require input
    trade_valuation <- grade_trade_wrapper(
      assets_df,
      marginal_transaction_value,
      input$team1_assets,
      input$team2_assets
    )

    trade_valuation$team1 |>
      select(-team) |>
      shiny_edit_tables() |>
      dt(page = 5)
  })

  output$team2_trade_outlook_title <- renderUI({
    #table 2 title
    req(input$trade_machine_team2) # require input

    h3(str_c(input$trade_machine_team2, "'s Assets Received"))
  })

  output$team2_trade_outlook <- renderDT({
    #table 2
    req(input$team1_assets, input$team2_assets) #require input
    trade_valuation <- grade_trade_wrapper(
      assets_df,
      marginal_transaction_value,
      input$team1_assets,
      input$team2_assets
    )

    trade_valuation$team2 |>
      select(-team) |>
      shiny_edit_tables() |>
      dt(page = 5)
  })

  # Reactivity for Page 7

  output$championship_odds_title <- renderUI({
    #title
    h3("Championship Odds")
  })

  output$championship_odds <- renderDT({
    #table 1
    champion_odds |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  output$playoff_odds_title <- renderUI({
    #title
    h3("Playoff Odds")
  })

  output$playoff_odds <- renderDT({
    #table 2
    playoff_odds |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  output$bye_odds_title <- renderUI({
    #title
    h3("First Round Bye Odds")
  })

  output$bye_odds <- renderDT({
    #table 3
    bye_odds |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  output$n1_pick_odds_title <- renderUI({
    #title
    h3("Number 1 Pick Odds")
  })

  output$n1_pick_odds <- renderDT({
    #table 4
    n1_pick_odds |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  output$most_common_finish_title <- renderUI({
    #title
    req(input$standings_season)
    h3(str_c(input$standings_season, " Most Likely Finish"))
  })

  output$most_common_finish <- renderDT({
    #table 5
    req(input$standings_season) #require input
    most_common_finish_df |>
      most_common_finish(input$standings_season, shiny = TRUE) |>
      dt(page = 12)
  })

  # Reactivity for Page 8

  output$players_top_future_value_title <- renderUI({
    #title
    h3("Players Ranked by Future Value")
  })

  output$players_top_future_value <- renderDT({
    #table 1
    req(input$enter_position) #require input
    player_total_value |>
      top_future_value_player(input$enter_position) |>
      dt(page = 5)
  })

  output$future_value_over_time_title <- renderUI({
    #title
    h3("Future Value over Time")
  })

  output$future_value_over_time <- renderPlotly({
    # first plot
    req(input$enter_players) #require input
    future_value_time |> plot_over_time(input$enter_players)
  })

  output$comparable_future_value_title <- renderUI({
    #title
    h3("Compare Future Value with similar players")
  })

  output$comparable_future_value <- renderPlotly({
    # first plot
    req(input$enter_player) #require input
    future_value_time |>
      comparable_players(player_total_value, input$enter_player)
  })

  # Reactivity for Page 9

  output$elo_rankings_title <- renderUI({
    #title
    h3("Elo Rankings")
  })

  output$elo_rankings <- renderPlotly({
    #first plot
    graph_elo(weekly_elo)
  })

  output$elo_win_prob <- renderDT({
    # text 1
    req(input$team1, input$team2)
    elos <- weekly_elo |> group_by(team) |> slice_max(date_hide)

    elo_win_probability(
      filter(elos, team == input$team1),
      filter(elos, team == input$team2)
    ) |>
      dt(page = 2)
  })

  output$total_success_title <- renderUI({
    #title
    h3("High and Low Elo Rating")
  })

  output$total_success <- renderDT({
    #table 2
    total_success |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  output$future_assets_title <- renderUI({
    #title
    h3("Total Future Assets")
  })

  output$future_assets <- renderDT({
    #table 2
    all_assets_summary_df |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  # Reactivity for Page 10

  output$matchups_title <- renderUI({
    #title
    req(input$matchups_team_name)
    h3(str_c(input$matchups_team_name, "'s Matchup Record"))
  })

  output$matchup_history <- renderDT({
    #table 1
    req(input$matchups_team_name, input$enter_round, input$team_seasons) #require input
    team_records_df |>
      team_matchup_record(
        input$matchups_team_name,
        input$enter_round,
        input$team_seasons,
        shiny = TRUE
      ) |>
      dt(page = 12)
  })

  # Reactivity for Page 11

  output$championship_title <- renderUI({
    #title
    h3("Championships")
  })

  output$championship <- renderDT({
    # table 1
    championships_df |>
      shiny_edit_tables() |>
      dt(page = 5)
  })

  output$finalists_title <- renderUI({
    #title
    h3("Finals")
  })

  output$finalists <- renderDT({
    # table 2
    finals_df |>
      shiny_edit_tables() |>
      dt(page = 5)
  })

  output$playoffs_title <- renderUI({
    #title
    h3("Playoff Appearances")
  })

  output$playoffs <- renderDT({
    # table 3
    playoffs_df |>
      shiny_edit_tables() |>
      dt(page = 6)
  })

  output$most_wins_title <- renderUI({
    #title
    h3("Most Wins")
  })

  output$most_wins <- renderDT({
    # table 4
    req(input$enter_round_history, input$enter_season_history)
    compute_most_wins(
      wins_df,
      input$enter_round_history,
      input$enter_season_history
    ) |>
      dt(page = 5)
  })

  output$most_points_title <- renderUI({
    #title
    h3("Most Fantasy Points")
  })

  output$most_points <- renderDT({
    # table 5
    req(input$enter_round_history, input$enter_season_history)
    compute_total_points(
      wins_df,
      input$enter_round_history,
      input$enter_season_history
    ) |>
      dt(page = 5)
  })

  output$highest_team_game_title <- renderUI({
    #title
    h3("Most Fantasy Points in a Game")
  })

  output$highest_team_game <- renderDT({
    # table 6
    req(input$enter_round_history, input$enter_season_history)
    highest_team_total(
      wins_df,
      input$enter_round_history,
      input$enter_season_history
    ) |>
      dt(page = 5)
  })

  output$player_highest_season_title <- renderUI({
    #title
    h3("Player's Total Fantasy Points")
  })

  output$player_highest_season <- renderDT({
    # table 7
    req(
      input$enter_round_history,
      input$enter_season_history,
      input$enter_position_history
    )
    compute_total_points_player(
      value_added,
      wins_df,
      input$enter_round_history,
      input$enter_season_history,
      input$enter_position_history
    ) |>
      dt(page = 5)
  })

  output$player_highest_va_season_title <- renderUI({
    #title
    h3("Player's Total Realized Value")
  })

  output$player_highest_va_season <- renderDT({
    # table 8
    req(
      input$enter_round_history,
      input$enter_season_history,
      input$enter_position_history
    )
    compute_value_added_player(
      value_added,
      wins_df,
      input$enter_round_history,
      input$enter_season_history,
      input$enter_position_history
    ) |>
      dt(page = 5)
  })

  output$player_best_game_title <- renderUI({
    #title
    h3("Player's Most Fantasy Points in a Game")
  })

  output$player_best_game <- renderDT({
    # table 3
    req(
      input$enter_round_history,
      input$enter_season_history,
      input$enter_position_history
    )
    highest_player_total(
      value_added,
      wins_df,
      input$enter_round_history,
      input$enter_season_history,
      input$enter_position_history
    ) |>
      dt(page = 5)
  })

  # Reactivity for Page 12

  output$sva_fit <- renderPlot({
    #first plot
    p <- toy_tva_plot_data |>
      ggplot() +
      geom_line(aes(x = historical_value, y = median_tva, color = position)) +
      geom_ribbon(
        aes(x = historical_value, ymin = q10, ymax = q90, fill = position),
        alpha = 0.2
      ) +
      facet_wrap(~age, nrow = 3) +
      labs(x = "KeepTradeCut", y = "Season Value Added") +
      theme(
        axis.text.x = element_text(angle = 30, vjust = 1.25, hjust = 1),
        legend.title = element_blank()
      )

    plot(p)
  })

  output$ktc_fit <- renderPlot({
    #first plot
    p <- toy_ktc_plot_data |>
      ggplot() +
      geom_line(aes(x = tva_adj, y = median_ktc, color = position)) +
      geom_ribbon(
        aes(x = tva_adj, ymin = q10, ymax = q90, fill = position),
        alpha = 0.2
      ) +
      facet_wrap(~age, nrow = 3) +
      labs(x = "Total Value Added", y = "Post-season KeepTradeCut") +
      coord_cartesian(ylim = c(0, 10000)) +
      theme(legend.title = element_blank())

    plot(p)
  })

  output$draft_fit <- renderPlotly({
    #first plot
    p <- draft_fit_plot |>
      ggplot(aes(x = pick_no)) +
      geom_point(aes(y = `Total Value`, text = paste0("Name: ", name))) +
      geom_line(aes(y = median), color = "cadetblue4") +
      geom_ribbon(
        aes(ymin = lower, ymax = upper),
        fill = "cadetblue3",
        alpha = .2
      ) +
      labs(x = "Draft Pick", y = "Total Value Added")

    ggplotly(p, tooltip = c("y", "text"))
  })
}

# Run the App
shinyApp(ui, server)
