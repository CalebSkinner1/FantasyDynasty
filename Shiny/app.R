# Shiny App

# doesn't work for current version of R, so I backed up to 4.4.2

library("shiny")
library("bs4Dash")
library("fresh")
library("DT")

print("App starting...") # for logs

# the app's files live in the Shiny folder (the working directory when deployed; found
# with here() when run locally)
app_dir <- if (interactive()) here::here("Shiny") else "."
source(file.path(app_dir, "app_call_data.R"))
print("Loaded functions") # for logs

# Define UI
# bs4Dash (AdminLTE 3 / Bootstrap 4) with a fresh theme; thematic matches the ggplot
# charts to the page (see thematic_shiny() at the bottom). Boxes sit side by side on wide
# screens and stack into one column on phones.

app_theme <- create_theme(
  bs4dash_status(
    primary = "#00a65a",
    success = "#00a65a",
    info = "#2a78d6",
    warning = "#eda100",
    danger = "#e34948"
  ),
  bs4dash_layout(main_bg = "#f4f6f9"),
  bs4dash_sidebar_dark(
    bg = "#222d32",
    color = "#c2c7d0",
    hover_color = "#ffffff"
  )
)

# a page section: a card with a title (text or a uiOutput)
panel <- function(..., title = NULL, width = 12) {
  box(
    ...,
    title = title,
    width = width,
    status = "success",
    solidHeader = FALSE,
    collapsible = FALSE
  )
}

ui <- dashboardPage(
  title = "Baylor Seniors Dynasty League",
  freshTheme = app_theme,
  dark = NULL,
  help = NULL,
  scrollToTop = TRUE,
  dashboardHeader(
    title = dashboardBrand(title = "Baylor Seniors", color = "success"),
    status = "success",
    skin = "dark"
  ),
  dashboardSidebar(
    skin = "dark",
    status = "success",
    elevation = 3,
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
    # titles that come from the server (uiOutput) sit inside the card titles
    tags$head(
      tags$style(HTML(
        "
        .card-title h3 { font-size: 1.1rem; margin: 0; }

        /* stat tiles (team bio, trade verdict) */
        .stat-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
          gap: 0.75rem;
          margin-top: 0.5rem;
        }
        .stat-grid.four { grid-template-columns: repeat(4, minmax(0, 1fr)); }
        @media (max-width: 768px) {
          .stat-grid.four { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        }
        .stat-tile {
          background: #ffffff;
          border: 1px solid #e3e6ea;
          border-left: 4px solid #00a65a;
          border-radius: 0.35rem;
          padding: 0.6rem 0.8rem;
        }
        .stat-tile.muted { border-left-color: #c3c2b7; }
        .stat-label {
          font-size: 0.75rem;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: #6c757d;
        }
        .stat-value { font-size: 1.5rem; font-weight: 600; line-height: 1.3; color: #212529; }
        .stat-detail { font-size: 0.85rem; color: #6c757d; }
        .stat-tile.large .stat-value { font-size: 2rem; }

        /* player and team names in tables */
        a.player-link, a.team-link { color: #1c5fae; }
        a.player-link:hover, a.team-link:hover { text-decoration: underline; }
        "
      )),
      # a click on a player or team name opens that player's or team's page (app.R
      # server: goto_player, goto_team)
      tags$script(HTML(
        "
        $(document).on('click', 'a.player-link, a.team-link', function(e) {
          e.preventDefault();
          var input = $(this).hasClass('player-link') ? 'goto_player' : 'goto_team';
          Shiny.setInputValue(input, String($(this).data('id')), {priority: 'event'});
          window.scrollTo(0, 0);
        });
        "
      ))
    ),
    tabItems(
      tabItem(
        # Home Tab
        tabName = "home",
        h2("Fantasy Football Dynasty League"),
        p(em(str_c("Data updated ", data_updated_label, "."))),
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
          "A player's replacement is the substitute his team would have started instead. This is the team's best
          bench player at that position or the average output of the top three projected free agents.
          I average this replacement output across all twelve teams, and blend that week's level (70%)
          with the season-to-date average (30%) to stabilize the estimate."
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
          I set all negative value added predictions to zero, because the player would be benched, and sum
          over the next eight years to obtain a player's future value. Following standard practice
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
        h2("Individual Players"),
        fluidRow(
          panel(
            width = 9,
            title = "Player",
            p(
              "Select any player to see his value added to date and his projected value added in future seasons."
            ),
            selectizeInput(
              inputId = "player_name",
              label = "Select a Player",
              choices = player_choices,
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            )
          ),
          column(3, uiOutput("player_image"))
        ),
        panel(title = "Value to Date", DTOutput("player_basic_info")),
        fluidRow(
          panel(
            width = 7,
            title = "Projected Value Added",
            p(
              "The line is his expected value added each season, the average of his simulated outcomes with below-replacement
              seasons set to zero. The shaded regions are the 80% and 95% credible intervals. The current season combines value
              added so far with the projection for the rest of the season."
            ),
            plotOutput("plot_future_value", height = "400px", width = "100%")
          ),
          panel(
            width = 5,
            title = "Future Value over Time",
            p(
              "Player's future value over time as his KeepTradeCut value varies, production accumulates, and age increases.
              Dashed lines mark team changes; hover over the marker at the top for details and click it to open the grade."
            ),
            plotlyOutput("player_value_over_time", height = "400px")
          )
        ),
        panel(
          title = uiOutput("tabulate_realized_value_title", inline = TRUE),
          p(
            "Enter a season to see the player's value added and fantasy points scored."
          ),
          selectizeInput(
            inputId = "player_season",
            label = "Select Seasons",
            choices = unique(season_value_added$season),
            multiple = TRUE, #enable multiple selections
            selected = max(season_value_added$season),
            options = list(
              placeholder = "Start typing...",
              maxOptions = length(unique(season_value_added$season)) # Limit the number of suggestions shown
            )
          ),
          DTOutput("tabulate_realized_value")
        ),
        panel(
          title = "Weekly Results",
          p(
            "Each bar is the player's fantasy points in a week. In weeks he started, the gray part is the replacement level
            and the blue part is his value added above it. A red bar fell short of replacement, which is marked by the dashed
            line. Hollow bars are bench weeks, and orange dots are his projection. Hover over a week for the numbers."
          ),
          plotlyOutput("weekly_results", height = "420px")
        )
      ),

      tabItem(
        # Page 2 Tab
        tabName = "teams",
        h2("Fantasy Teams"),
        fluidRow(
          panel(
            width = 9,
            title = "Team",
            p(
              "Select a user to view the team's top assets and acquisitions."
            ),
            selectizeInput(
              inputId = "team_name",
              label = "Select a Team",
              choices = users$display_name,
              selected = users$display_name[1],
              options = list(
                placeholder = "Start typing...",
                maxOptions = nrow(users) # Limit the number of suggestions shown
              )
            ),
            uiOutput("team_bio")
          ),
          column(3, uiOutput("image_ui"))
        ),
        fluidRow(
          panel(
            width = 6,
            title = uiOutput("team_assets_title", inline = TRUE),
            p("Top future assets on the team."),
            DTOutput("team_assets")
          ),
          panel(
            width = 6,
            title = uiOutput("position_outlook_title", inline = TRUE),
            p("Asset value by position."),
            DTOutput("position_outlook")
          )
        ),
        panel(
          title = uiOutput("team_value_time_title", inline = TRUE),
          p(
            "The team's future value on each date, by position and draft picks: the future value of
            the players on its roster that day plus the value of its unused picks. A new year of picks
            joins every team's assets when each rookie draft finishes, and future value falls through
            each season as it is realized. Points are computed on each date with KeepTradeCut values,
            which were sparse in 2024. Hover over an area for the numbers."
          ),
          plotlyOutput("team_value_time", height = "380px")
        ),
        fluidRow(
          panel(
            width = 6,
            title = uiOutput("team_contributors_title", inline = TRUE),
            selectizeInput(
              inputId = "team_season",
              label = "Select Seasons",
              choices = unique(season_value_added$season),
              multiple = TRUE, #enable multiple selections
              selected = max(season_value_added$season),
              options = list(
                placeholder = "Start typing...",
                maxOptions = length(unique(season_value_added$season)) # Limit the number of suggestions shown
              )
            ),
            p("Top contributors in this season."),
            DTOutput("team_contributors")
          ),
          panel(
            width = 6,
            title = uiOutput("team_contributors_weekly_title", inline = TRUE),
            selectizeInput(
              inputId = "team_week",
              label = "Select a Week",
              choices = NULL, # completed weeks of the selected season, set in the server
              options = list(
                placeholder = "Start typing...",
                maxOptions = 18 # Limit the number of suggestions shown
              )
            ),
            p(
              "Weekly starters. This defaults to the most recent week."
            ),
            DTOutput("team_contributors_weekly")
          )
        ),
        fluidRow(
          panel(
            width = 6,
            title = uiOutput("avenue_grades_title", inline = TRUE),
            p("Performance across acquisition avenues."),
            DTOutput("avenue_grades")
          ),
          panel(
            width = 6,
            title = uiOutput("team_composition_title", inline = TRUE),
            p(
              "Proportion of total value on roster acquired from various avenues."
            ),
            DTOutput("team_composition")
          )
        ),
        panel(
          title = "Acquisitions",
          selectizeInput(
            inputId = "team_avenue",
            label = "Select an Acquisition Avenue",
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
          fluidRow(
            column(
              6,
              uiOutput("top_acquisitions_title"),
              p(
                "Filter by avenue to see top trade, draft pick, transaction, etc."
              ),
              DTOutput("top_acquisitions")
            ),
            column(
              6,
              uiOutput("worst_acquisitions_title"),
              p(
                "Filter by avenue to see worst trade, draft pick, transaction, etc."
              ),
              DTOutput("worst_acquisitions")
            )
          )
        )
      ),

      tabItem(
        # Page 3 Tab
        tabName = "draft",
        h2("Draft Grades"),
        panel(
          title = "Draft",
          p(
            "Select a draft to view the top picks and best overall drafts. Each pick is graded against the value
            expected from that draft slot. Rookie drafts use the rookie pick model, and the startup draft is
            graded against the league average. This provides a standard against which to compare drafts."
          ),
          selectizeInput(
            inputId = "draft_selection",
            label = "Select a Draft",
            choices = c(
              unique(picks_df$draft),
              "All"
            ),
            # multiple = TRUE, #enable multiple selections
            options = list(
              placeholder = "Start typing...",
              maxOptions = length(c(unique(picks_df$draft), "All")) # Limit the number of suggestions shown
            )
          )
        ),
        panel(
          title = uiOutput("draft_ranking_title", inline = TRUE),
          DTOutput("draft_ranking")
        ),
        panel(
          title = uiOutput("draft_grades_time_title", inline = TRUE),
          p(
            "Each team's draft grade (value over expected) on each date since the draft, graded the same
            way as the table above. Hover over a line for the numbers, and click a team in the legend to
            hide it."
          ),
          plotlyOutput("draft_grades_time", height = "400px")
        ),
        fluidRow(
          panel(
            width = 6,
            title = uiOutput("best_picks_title", inline = TRUE),
            DTOutput("best_picks")
          ),
          panel(
            width = 6,
            title = uiOutput("worst_picks_title", inline = TRUE),
            DTOutput("worst_picks")
          )
        ),
        panel(
          title = "Rookie Pick Value",
          p(
            "Each point is a drafted rookie's total value added (realized value plus future value) by his draft position.
            The blue line is the model's median, and the shaded region is the 95% credible interval. Rookie picks are
            graded against the model's mean, which sits above the median because a few picks become stars. Hover over
            a point to see the player. The",
            strong("Modeling"),
            "tab explains the model."
          ),
          plotlyOutput("draft_fit_grades", height = "400px", width = "100%")
        )
      ),

      tabItem(
        # Page 4 Tab
        tabName = "transaction",
        h2("Transaction Grades"),
        panel(
          title = "Top Transactions",
          p(
            "The most successful transactions in league history. This is assessed now, not at the time of the transaction.
            A player's later value counts for the team that added him even if he leaves, and transactions that only
            dropped a player aren't listed."
          ),
          DTOutput("top_transaction"),
          p(
            "Click a transaction in the table above to see a full breakdown of the value gained and lost from added and dropped players."
          )
        ),
        fluidRow(
          panel(
            width = 6,
            title = uiOutput("individual_transaction_title", inline = TRUE),
            DTOutput("individual_transaction")
          ),
          panel(
            width = 6,
            title = "Grade over Time",
            p(
              "The value the transaction gained on each date since it went through: realized value from
              the transaction through that date plus future value that date. The dashed line marks the
              transaction."
            ),
            plotlyOutput("transaction_grade_time", height = "320px")
          )
        ),
        panel(
          title = "Transaction Winners",
          p("The total value gained or lost from all transactions."),
          DTOutput("transaction_winners")
        )
      ),

      tabItem(
        # Page 5 Tab
        tabName = "trade",
        h2("Trade Grades"),
        panel(
          title = "Lopsided Trades",
          p(
            "The most unfair trades in league history. This is assessed now, not at the time of the trade.
            A player's value produced after the trade counts for the team that received him, even if he later leaves."
          ),
          DTOutput("trade_lopsided"),
          p(
            "Click a trade in the table above to see a full breakdown of the value gained and lost for each user."
          )
        ),
        fluidRow(
          panel(
            width = 6,
            title = uiOutput("individual_trade_title", inline = TRUE),
            DTOutput("individual_trade")
          ),
          panel(
            width = 6,
            title = "Grade over Time",
            p(
              "Each team's value gained on each date since the trade: realized value from the trade
              through that date plus future value that date. The dashed line marks the trade; KeepTradeCut
              values were sparse in 2024, so an early trade's first point can come months later."
            ),
            plotlyOutput("trade_grade_time", height = "320px")
          )
        ),
        panel(
          title = "Trade Winners",
          p("The total value gained or lost from all trades."),
          DTOutput("trade_winners")
        )
      ),

      tabItem(
        # Page 6 Tab
        tabName = "trade_machine",
        h2("Trade Machine"),
        panel(
          title = "Build a Trade",
          p(
            "This Trade Machine compares the projected future value of various assets in a potential trade.
        It outputs the net future value and net value for the rest of the current season."
          ),
          p(
            "Please note that
        fantasy trades are incredibly contextual. Consider your own team composition
        and championship window carefully before making a trade. Like many trade calculators,
        Trade Machine treats values as additive, but, obviously, this is an approximate way to compute
        value (for example, 1 first round pick is more valuable than 3 third round picks, even though the sum of the latter's
        KTC value is higher). The ",
            strong("value added"),
            " metric helps account for this, but nothing is perfect. Also, please note
          that the model has no direct injury input. A player's future value will dynamically respond to an injury,
          but the response is indirect and lagged."
          ),
          p(
            "Select two teams and the assets each would send (one side can be empty). If one team receives more players
            than it sends, it must drop players, so the trade is adjusted by the value of the average add or drop."
          ),
          fluidRow(
            column(
              6,
              selectizeInput(
                inputId = "trade_machine_team1",
                label = "Select Team 1",
                choices = users$display_name,
                selected = users$display_name[1],
                options = list(
                  placeholder = "Start typing...",
                  maxOptions = nrow(users) # Limit the number of suggestions shown
                )
              )
            ),
            column(
              6,
              selectizeInput(
                inputId = "trade_machine_team2",
                label = "Select Team 2",
                choices = users$display_name,
                selected = users$display_name[2],
                options = list(
                  placeholder = "Start typing...",
                  maxOptions = nrow(users) # Limit the number of suggestions shown
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
                  maxOptions = 100 # show every asset
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
                  maxOptions = 100 # show every asset
                )
              )
            )
          )
        ),
        panel(
          title = "Verdict",
          uiOutput("trade_verdict")
        ),
        panel(
          title = "Value Exchanged",
          plotOutput("plot_trade_assets", height = "300px", width = "100%")
        ),
        fluidRow(
          panel(
            width = 6,
            title = uiOutput("team1_trade_outlook_title", inline = TRUE),
            DTOutput("team1_trade_outlook")
          ),
          panel(
            width = 6,
            title = uiOutput("team2_trade_outlook_title", inline = TRUE),
            DTOutput("team2_trade_outlook")
          )
        )
      ),

      tabItem(
        # Page 7 Tab
        tabName = "future_standings",
        h2("Future Standings"),
        p(
          "I simulate final standings over the next three years 50,000 times. In each simulation,
          I project the value added for each player and find the total value added for each fantasy team.
          Then, a team's weekly score is drawn around a replacement-level lineup plus its projected value added,
          and the season, playoffs and draft order are played out. This yields final standing,
          championship, and draft pick odds for each team."
        ),
        fluidRow(
          panel(
            width = 6,
            title = "Championship Odds (%)",
            DTOutput("championship_odds")
          ),
          panel(width = 6, title = "Playoff Odds", DTOutput("playoff_odds"))
        ),
        fluidRow(
          panel(
            width = 6,
            title = "First Round Bye Odds (%)",
            DTOutput("bye_odds")
          ),
          panel(
            width = 6,
            title = "Number 1 Pick Odds by Draft (%)",
            DTOutput("n1_pick_odds")
          )
        ),
        fluidRow(
          panel(
            width = 6,
            title = uiOutput("most_common_finish_title", inline = TRUE),
            selectizeInput(
              inputId = "standings_season",
              label = "Select a Season",
              choices = unique(most_common_finish_df$season),
              selected = min(most_common_finish_df$season),
              options = list(
                placeholder = "Start typing...",
                maxOptions = length(unique(most_common_finish_df$season)) # Limit the number of suggestions shown
              )
            ),
            p("The most likely finish for each Fantasy Team."),
            DTOutput("most_common_finish")
          )
        )
      ),

      tabItem(
        # Page 8 Tab
        tabName = "rankings",
        h2("Player Rankings"),
        panel(
          title = "Players Ranked by Future Value",
          p(
            "Players' future value, ranked and displayed over time.
          Select a position to see the top players of that position."
          ),
          selectizeInput(
            inputId = "enter_position",
            label = "Select Positions",
            choices = c("All", "QB", "RB", "WR", "TE"),
            multiple = TRUE, # enable multiple selections
            selected = "All",
            options = list(
              placeholder = "Start typing...",
              maxOptions = 5 # Limit the number of suggestions shown
            )
          ),
          DTOutput("players_top_future_value")
        ),
        fluidRow(
          panel(
            width = 6,
            title = "Future Value over Time",
            selectizeInput(
              inputId = "enter_players",
              label = "Select Players",
              choices = player_choices,
              multiple = TRUE, #enable multiple selections
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            ),
            p(
              "Demonstrates a player's future value at different points in the league history. Please note that
          these estimations were conducted at irregular intervals until June 2025. Note that a player's value will
          gradually decrease over the course of a season as the player ages and has fewer expected games remaining in his career.
          There are some notable exceptions, typically when a player dramatically exceeds expectations."
            ),
            plotlyOutput("future_value_over_time")
          ),
          panel(
            width = 6,
            title = "Compare Future Value with Similar Players",
            p(
              "Select a single player and see the trajectory of similar players of the same position."
            ),
            selectizeInput(
              inputId = "enter_player",
              label = "Select a Player",
              choices = player_choices,
              multiple = FALSE, #enable multiple selections
              options = list(
                placeholder = "Start typing...",
                maxOptions = 5 # Limit the number of suggestions shown
              )
            ),
            plotlyOutput("comparable_future_value")
          )
        )
      ),

      tabItem(
        # Page 9 Tab
        tabName = "team_rankings",
        h2("Team Rankings"),
        p("Team rankings by Elo and total future value of assets."),
        panel(
          title = "Elo Rankings",
          p(
            "Elo is a simple rating system developed by physicist Dr. Arpad Elo. Each result is compared with what the ratings
            expected: beating a weaker team earns little, an upset earns a lot, and bigger margins of victory count more.
            The average Elo is 1500 and ratings are discounted by 25% at the end of each season. Elo gives more weight to recent games,
            but all games have non-zero impact on the rating. Elo is simple to compute and interpret, but it is not a
            perfect fit for Fantasy sports. Teams have no direct impact on their opponent's score, but their 'Points Against' are factored
            into the score. For this reason, each week, I compute the adjustments as if each fantasy team had played
            all other teams in the league, and I average the adjustments together. This removes any scheduling
            bias from the rating. Elo is not a forward facing predictive metric, but it is useful to understand team strength over time."
          ),
          plotlyOutput("elo_rankings", height = "400px", width = "100%")
        ),
        fluidRow(
          panel(
            width = 6,
            title = "Win Probability",
            p(
              "Elo also gives implied win probabilities. Select two teams to compare their win probability."
            ),
            fluidRow(
              column(
                6,
                selectizeInput(
                  inputId = "team1",
                  label = "Select Team 1",
                  choices = users$display_name,
                  selected = users$display_name[1],
                  options = list(
                    placeholder = "Start typing...",
                    maxOptions = nrow(users) # Limit the number of suggestions shown
                  )
                )
              ),
              column(
                6,
                selectizeInput(
                  inputId = "team2",
                  label = "Select Team 2",
                  choices = users$display_name,
                  selected = users$display_name[2],
                  options = list(
                    placeholder = "Start typing...",
                    maxOptions = nrow(users) # Limit the number of suggestions shown
                  )
                )
              )
            ),
            DTOutput("elo_win_prob")
          ),
          panel(
            width = 6,
            title = "Total Future Assets",
            DTOutput("future_assets")
          )
        ),
        panel(
          title = "High and Low Elo Rating",
          DTOutput("total_success"),
          p(
            style = "margin-top: 1rem;",
            strong("Composite rating"),
            "is a team's average Elo rating, measured as points above or below the league average of 1500,
            over every week it has played since the league began. A team that has hovered around 1550 has a composite
            rating near +50, and a team that has hovered around 1450 is near -50. Where the peak and low show a team's best
            and worst moments, the composite rating rewards a team that stays strong season after season."
          )
        )
      ),

      tabItem(
        # Page 10 Tab
        tabName = "matchups",
        h2("Matchup History"),
        panel(
          title = uiOutput("matchups_title", inline = TRUE),
          p("Select a team to see historical records against opponents."),
          fluidRow(
            column(
              4,
              selectizeInput(
                inputId = "matchups_team_name",
                label = "Select a Team",
                choices = users$display_name,
                selected = users$display_name[1],
                options = list(
                  placeholder = "Start typing...",
                  maxOptions = nrow(users) # Limit the number of suggestions shown
                )
              )
            ),
            column(
              4,
              selectizeInput(
                inputId = "team_seasons",
                label = "Select Seasons",
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
                label = "Select Rounds",
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
          DTOutput("matchup_history")
        )
      ),

      tabItem(
        # Page 11 Tab
        tabName = "history",
        h2("League History"),
        p(
          "This page lists the league champions and notable records set in the league."
        ),
        fluidRow(
          panel(width = 4, title = "Championships", DTOutput("championship")),
          panel(width = 4, title = "Finals", DTOutput("finalists")),
          panel(width = 4, title = "Playoff Appearances", DTOutput("playoffs"))
        ),
        panel(
          title = "Records",
          fluidRow(
            column(
              6,
              selectizeInput(
                inputId = "enter_season_history",
                label = "Select Seasons",
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
                label = "Select Game Types",
                choices = c("All", unique(wins_df$type)),
                multiple = TRUE, #enable multiple selections
                selected = "All",
                options = list(
                  placeholder = "Start typing...",
                  maxOptions = (length(unique(wins_df$type)) + 1) # Limit the number of suggestions shown
                )
              )
            )
          )
        ),
        fluidRow(
          panel(width = 6, title = "Most Wins", DTOutput("most_wins")),
          panel(
            width = 6,
            title = "Most Fantasy Points",
            DTOutput("most_points")
          )
        ),
        panel(
          title = "Most Fantasy Points in a Game",
          p(
            "Note: reverse the sort on this column to see the lowest team total in league history."
          ),
          DTOutput("highest_team_game")
        ),
        panel(
          title = "Player Records",
          selectizeInput(
            inputId = "enter_position_history",
            label = "Select Positions",
            choices = c("All", unique(value_added$position)),
            multiple = TRUE, #enable multiple selections
            selected = "All",
            options = list(
              placeholder = "Start typing...",
              maxOptions = length(unique(value_added$position)) + 1 # Limit the number of suggestions shown
            )
          )
        ),
        fluidRow(
          panel(
            width = 6,
            title = "Player's Total Fantasy Points (as a starter)",
            DTOutput("player_highest_season")
          ),
          panel(
            width = 6,
            title = "Player's Total Realized Value",
            DTOutput("player_highest_va_season")
          )
        ),
        panel(
          title = "Player's Most Fantasy Points in a Game (as a starter)",
          DTOutput("player_best_game")
        )
      ),

      tabItem(
        # Page 12 Tab
        tabName = "modeling",
        h2("Model Explanations and Fit"),
        withMathJax(),
        p(
          "This site makes dozens of projections and draws substantial conclusions. Why should you trust them? Let me try to convince you."
        ),
        h4("Value Added"), # mathematically show
        p(
          "Debates are waged and numbers are crunched to project the future of fantasy players, but far less attention is devoted
        to understanding their realized value. As the saying goes, 'the best way to predict the future is to study the past.'
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
          "\\[P = \\{\\text{QB, RB, WR, TE, SUPERFLEX, FLEX, K, DST} \\}.\\]"
        ),
        p(
          "Replacement players are determined using pre-game projections \\(\\hat{x}_{j w}\\).
        The replacement-level player for position \\(p\\), team \\(T\\), and week \\(w\\) is defined as",
          "\\[j^{*}_{p T w} = \\arg\\max_{j \\in p \\cap S_w^c \\cap T} \\hat{x}_{j w},\\]"
        ),
        p(
          "If the team's best bench option \\(j^{*}\\) is projected below the best free agent, the replacement output is
        instead the mean actual points of the three highest-projected free agents."
        ),
        p(
          "The realized replacement output is",
          "\\[r_{p T w} = x_{j^{*}_{p T w} w}.\\]"
        ),
        p(
          "The realized replacement output is very volatile across each week and team. A more stable estimate of the replacement value
        is obtained by averaging across teams:",
          "\\[\\bar{r}_{p w} = \\frac{1}{12}\\sum_{T} r_{p T w}.\\]"
        ),
        p(
          "Even this estimate can fluctuate some week to week, so I smooth the weekly replacement estimate \\(\\bar{r}_{p w}\\) toward
        the season-long average at each position, yielding the weighted-weekly replacement:",
          "\\[wr_{p w} = (1- \\alpha) \\bar{r}_{p w} + \\alpha \\frac{1}{W} \\sum_{u = 1}^{W} \\bar{r}_{p u},\\]",
          "where \\(W\\) is the number of weeks played so far and \\(\\alpha \\in (0, 1)\\) is a mixing parameter.
          I set \\(\\alpha = 0.3\\)."
        ),
        p(
          "Finally, value added for player \\(j\\) in week \\(w\\) is defined",
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
          \\epsilon_{j, t + 1} = \\sigma_{j, t + 1} z_{j, t + 1}\\]",
          "where \\(z_{j, t + 1}\\) is drawn from the empirical distribution of standardized out-of-fold residuals.",
          "The residual scale is modeled as",
          "\\[\\mathbb{E}|\\epsilon_{j, t + 1}| = g_{\\text{GAM}}(a_{j t}, k_{j t}, p_{j t}) \\]",
          "The mean structure is modeled using Bayesian Additive Regression Trees (BART),
        while heteroskedasticity in the residuals is accommodated by modeling the expected absolute residual
        (\\(\\sigma = \\mathbb{E}|\\epsilon|\\sqrt{\\pi/2}\\)) as a smooth
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
        \\delta_{j t + 1} = \\tau_{j t + 1} z_{j t + 1},\\]",
          "where \\(z_{j, t+1}\\) is drawn from the empirical distribution of standardized out-of-fold residuals.
        The residual scale is modeled as",
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
          "Finally, a player's future value is a discounted sum of his expected seasonal value added
          over the next eight seasons, where each season's expectation counts below-replacement outcomes as zero.
          During the season, the current season is weighted by the share remaining and is not discounted:",
          "\\[f_j = \\sum_{t=0}^7 (1-\\gamma)^t \\, \\mathbb{E}\\left[\\max(sv_{j t}, 0)\\right],\\]",
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
          "I assign the regression coefficients \\(\\beta\\) a standard g-prior and model the log variance as a linear function of
          the same pick terms, \\(\\log \\sigma_j^2 = \\gamma_0 + \\gamma_1 d_j + \\gamma_2 \\sqrt{d_j}\\),
          sampled with a Metropolis-Hastings step inside the Gibbs sampler. The full MCMC implementation is available at ",
          a(
            "GitHub",
            href = "https://github.com/CalebSkinner1/FantasyDynasty/blob/main-branch/Modeling/MCMC%20Samplers.R",
            target = "_blank"
          ),
          "."
        ),
        p(
          "I plot drafted players' Total Value Added across their rookie draft position. The blue line is the median projected
        value, while the shaded region corresponds to 95% credible intervals. Pick values used elsewhere on the site are the posterior mean,
        which sits above the median because a few picks become stars. Hover on the point to see the player's name."
        ),
        plotlyOutput("draft_fit", height = "400px", width = "100%"),

        h4("Future Standings"), # math, model
        p(
          "Finally, I leverage the simulated player career trajectories to predict the outcomes of future
        fantasy matchups. I approximate the strength \\(s_T\\) of a fantasy team \\(T\\) by summing the projected
        season value added of the top twelve players on the roster:
          \\[\\tilde{s}_{T} = \\sum_{j \\in T_{12}} \\tilde{sv}_{j}. \\]"
        ),
        p(
          "I model the team's weekly score as normally distributed around the league's replacement-level lineup
          plus the team's projected value added spread across the season's 17 weeks:",
          "\\[o_{T w} \\sim N\\left(R + \\tilde{s}_T / 17, \\sigma^2\\right),\\]",
          "where \\(R\\) is the summed replacement level of a starting lineup and \\(\\sigma \\approx 22\\) is the
          typical week-to-week standard deviation of a team's score around its own season average. I estimate
          it as the pooled within-team-season standard deviation of regular season scores,",
          "\\[\\hat{\\sigma}^2 = \\frac{1}{N - G}\\sum_{T,t}\\sum_{w}\\left(o_{T t w} - \\bar{o}_{T t}\\right)^2,\\]",
          "where \\(N\\) is the number of team-weeks and \\(G\\) the number of team-seasons.
          Currently \\(\\hat{\\sigma} \\approx 22\\) points."
        ),
        p(
          "To simulate each fantasy matchup within a given season, I draw new noise terms
        \\(\\tilde{\\epsilon}_{T w} \\sim N(0, \\hat{\\sigma}^2)\\) and generate weekly outcomes:",
          "\\[\\tilde{o}_{T w} = R + \\tilde{s}_T / 17 + \\tilde{\\epsilon}_{T w}\\]"
        ),
        p(
          "Because the weekly schedule is known, simulated weekly outputs for each team are sufficient to generate
        complete season outcomes. To associate probabilities with each possible finishing position, we generate 50,000 independent
        simulations, with each drawing a fresh projection for every player and draft pick each season. After simulating both the regular season and playoffs,
        draft positions are determined and incorporated into each team's asset portfolio. By iterating this process,
        I generate projected league standings for the next three seasons."
        ),
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Links between pages --------------------------------------------------------
  # a player or team name clicked in any table opens its page (see the script in the
  # page head)
  observeEvent(input$goto_player, {
    updateSelectizeInput(session, "player_name", selected = input$goto_player)
    updateTabItems(session, "sidebar", selected = "players")
  })

  observeEvent(input$goto_team, {
    updateSelectizeInput(session, "team_name", selected = input$goto_team)
    updateTabItems(session, "sidebar", selected = "teams")
  })

  # the trade and transaction shown in the Trade Grades and Transaction Grades breakdowns:
  # set by clicking a row in their tables or a team-change marker on a player's chart
  selected_trade <- reactiveVal(as.character(lopsided_trades$trade_id[1]))
  selected_transaction <- reactiveVal(as.character(top_transactions$transaction_id[1]))

  # Reactivity for Page 1
  # input$player_name is the player's player_id; player_label() is his name

  player_label <- reactive({
    req(input$player_name)
    names(player_choices)[match(input$player_name, player_choices)]
  })

  output$player_image <- renderUI({
    # user image
    req(input$player_name)
    image_url <- player_headshot |>
      filter(player_id == input$player_name) |>
      slice(1) |>
      pull(headshot_url)

    # Sleeper's default player avatar when there is no headshot or it fails to load (and
    # a plain silhouette if that fails too)
    player_default <- "https://sleepercdn.com/images/v2/icons/player_default.webp"
    silhouette <- str_c(
      "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>",
      "<circle cx='50' cy='36' r='20' fill='%23c3c2b7'/>",
      "<path d='M14 96c4-22 20-32 36-32s32 10 36 32z' fill='%23c3c2b7'/></svg>"
    )
    if (length(image_url) != 1 || is.na(image_url)) {
      image_url <- player_default
    }

    tags$img(
      src = image_url,
      height = "150px",
      alt = player_label(),
      onerror = str_c(
        "this.onerror = function() { this.onerror = null; this.src = \"",
        silhouette,
        "\"; };",
        "this.src = \"",
        player_default,
        "\";"
      )
    )
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

  output$player_value_over_time <- renderPlotly({
    req(input$player_name)
    plot_over_time(
      future_value_time,
      input$player_name,
      moves = player_moves,
      source = "player_moves"
    ) |>
      event_register("plotly_click")
  })

  # a click on a team-change marker opens that trade or transaction (priority "event" so
  # clicking the same marker again still opens it; event_data warns until the chart has
  # been drawn once)
  move_click <- reactive(suppressWarnings(
    event_data("plotly_click", source = "player_moves", priority = "event")
  ))
  # an error in an observer ends the session (the page then offers to reload), so a click
  # that can't be read is logged and ignored instead. The key can arrive as a list.
  observeEvent(move_click(), {
    tryCatch(
      {
        key <- as.character(unlist(move_click()$key))
        if (length(key) != 1 || !grepl("|", key, fixed = TRUE)) {
          return()
        }
        target <- strsplit(key, "|", fixed = TRUE)[[1]]
        if (target[1] == "trade") {
          selected_trade(target[2])
          row <- match(target[2], as.character(lopsided_trades$trade_id))
          if (!is.na(row)) {
            dataTableProxy("trade_lopsided") |> selectRows(row) |> selectPage((row - 1) %/% 5 + 1)
          }
          updateTabItems(session, "sidebar", selected = "trade")
        } else {
          selected_transaction(target[2])
          row <- match(target[2], as.character(top_transactions$transaction_id))
          if (!is.na(row)) {
            dataTableProxy("top_transaction") |> selectRows(row) |> selectPage((row - 1) %/% 5 + 1)
          }
          updateTabItems(session, "sidebar", selected = "transaction")
        }
      },
      error = function(e) message("team-change click failed: ", conditionMessage(e))
    )
  })

  output$tabulate_realized_value_title <- renderUI({
    #title
    req(input$player_name, input$player_season) #require input
    h3(str_c(player_label(), " Seasons"))
  })

  output$tabulate_realized_value <- renderDT({
    #table 2
    req(input$player_name, input$player_season)
    tabulate_realized_value(
      value_added,
      input$player_name,
      input$player_season,
      shiny = TRUE
    ) |>
      dt(page = 5)
  })

  output$weekly_results <- renderPlotly({
    #second plot
    req(input$player_name, input$player_season) #require input
    weekly_results(value_added, input$player_name, input$player_season)
  })

  # Reactivity for Page 2

  observeEvent(input$team_season, {
    weeks <- value_added |>
      filter(season == max(as.numeric(input$team_season))) |>
      distinct(week) |>
      arrange(week) |>
      pull(week)

    updateSelectizeInput(
      session,
      "team_week",
      choices = weeks,
      selected = max(weeks)
    )
  })

  output$image_ui <- renderUI({
    # user image
    req(input$team_name)
    image_url <- avatar |>
      filter(display_name == input$team_name) |>
      pull(avatar_url)

    tags$img(src = image_url, height = "150px")
  })

  output$team_bio <- renderUI({
    req(input$team_name)
    bio <- team_bio(input$team_name)
    div(
      class = "stat-grid four", # two rows: this season, then all time
      pmap(bio, \(label, value, detail) {
        div(
          class = "stat-tile",
          div(class = "stat-label", label),
          div(class = "stat-value", value),
          div(class = "stat-detail", detail)
        )
      })
    )
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
      dt(paging = FALSE) # all six positions
  })

  output$team_value_time_title <- renderUI({
    req(input$team_name)
    h3(str_c(input$team_name, " Asset Value over Time"))
  })

  output$team_value_time <- renderPlotly({
    req(input$team_name)
    plot_team_value_time(team_value_time, input$team_name)
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
      dt(page = 12) # a full lineup and its total on one page
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
    overall_grades(value_avenues, rid(), shiny = TRUE) |> dt(paging = FALSE)
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
      dt(paging = FALSE, percent = "Proportion")
  })

  # Reactivity for Page 3

  # "Initial Draft", "2025 Rookie Draft", or "All Draft" for titles
  draft_label <- function(draft) {
    if (draft == "All") "All Draft" else str_to_title(draft)
  }

  output$draft_ranking_title <- renderUI({
    #title
    req(input$draft_selection) #require input
    h3(str_c(draft_label(input$draft_selection), " Grades"))
  })

  output$draft_ranking <- renderDT({
    #table 1
    req(input$draft_selection) # require input
    draft_rankings(input$draft_selection, shiny = TRUE) |>
      dt(page = 12)
  })

  output$draft_grades_time_title <- renderUI({
    req(input$draft_selection)
    h3(str_c(draft_label(input$draft_selection), " Grades over Time"))
  })

  output$draft_grades_time <- renderPlotly({
    req(input$draft_selection)
    plot_draft_grades_time(draft_grades_time, input$draft_selection)
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

  output$transaction_winners <- renderDT({
    #table 1
    overall_transaction_winners |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  output$top_transaction <- renderDT({
    #table 2: click a row to see its breakdown below (the first row to start)
    top_transactions |>
      select(-transaction_id) |>
      shiny_edit_tables() |>
      dt(
        page = 5,
        dom = "ftip",
        selection = list(
          mode = "single",
          selected = row_of(isolate(selected_transaction()), top_transactions$transaction_id)
        ),
        options = list(displayStart = page_start(isolate(selected_transaction()), top_transactions$transaction_id))
      )
  })

  # transaction_id of the clicked row
  observeEvent(input$top_transaction_rows_selected, {
    selected_transaction(as.character(top_transactions$transaction_id[
      input$top_transaction_rows_selected
    ]))
  })

  output$individual_transaction_title <- renderUI({
    txn <- total_transaction_value[[selected_transaction()]]
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
    inspect_individual_transaction(selected_transaction(), shiny = TRUE) |>
      dt(page = 12)
  })

  output$transaction_grade_time <- renderPlotly({
    team <- total_transaction_value[[selected_transaction()]]$team_name[1]
    plot_transaction_grade_time(transaction_grades_time, selected_transaction(), team)
  })

  # Reactivity for Page 5

  output$trade_winners <- renderDT({
    #table 1
    overall_trade_winners |>
      shiny_edit_tables() |>
      dt(page = 12)
  })

  output$trade_lopsided <- renderDT({
    #table 2: click a row to see its breakdown below (the first row to start)
    lopsided_trades |>
      select(-trade_id) |>
      shiny_edit_tables() |>
      dt(
        page = 5,
        dom = "ftip",
        selection = list(
          mode = "single",
          selected = row_of(isolate(selected_trade()), lopsided_trades$trade_id)
        ),
        options = list(displayStart = page_start(isolate(selected_trade()), lopsided_trades$trade_id))
      )
  })

  # trade_id of the clicked row
  observeEvent(input$trade_lopsided_rows_selected, {
    selected_trade(as.character(lopsided_trades$trade_id[
      input$trade_lopsided_rows_selected
    ]))
  })

  output$individual_trade_title <- renderUI({
    trade <- total_trade_value[[selected_trade()]]
    teams <- trade |>
      select(team_name) |>
      distinct() |>
      map_chr(~ str_c(.x, collapse = ", "))
    h3(str_c(trade$season[1], " W", trade$week[1], " Trade between ", teams))
  })

  output$individual_trade <- renderDT({
    #table 3
    inspect_individual_trade(selected_trade(), shiny = TRUE) |>
      dt(page = 12)
  })

  output$trade_grade_time <- renderPlotly({
    plot_trade_grade_time(trade_grades_time, selected_trade())
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

  # graded once per change of teams or assets, shared by the outputs below
  trade_valuation <- reactive({
    req(input$trade_machine_team1, input$trade_machine_team2)
    validate(need(
      input$trade_machine_team1 != input$trade_machine_team2,
      "Select two different teams."
    ))
    validate(need(
      length(input$team1_assets) + length(input$team2_assets) > 0,
      "Select the assets each team would send to grade the trade."
    ))
    grade_trade_wrapper(
      assets_df,
      roster_spot_value,
      input$trade_machine_team1,
      input$team1_assets,
      input$trade_machine_team2,
      input$team2_assets
    )
  })

  # the net difference in future value and in the upcoming season's value, and which team
  # each favors. A tile takes the favored team's color (its color in the chart below)
  output$trade_verdict <- renderUI({
    tv <- trade_valuation()
    tile <- function(what, value, favor) {
      even <- value < .05
      color <- team_colors()[[favor]]
      div(
        class = str_c("stat-tile large", if (even) " muted"),
        # team color border with a light tint of it (hex alpha 1a = 10%)
        style = if (!even) str_c("border-left-color: ", color, "; background: ", color, "1a;"),
        div(class = "stat-label", what),
        div(class = "stat-value", if (even) "Even" else format(round(value, 1), nsmall = 1, big.mark = ",")),
        div(
          class = "stat-detail",
          if (even) "neither team gains" else tagList("favors ", strong(favor))
        )
      )
    }
    div(
      class = "stat-grid",
      style = "margin-top: 0;",
      tile("Future value", tv$ov_value, tv$ov_favor),
      tile(str_c(tv$upcoming_label, " value"), tv$iv_value, tv$iv_favor)
    )
  })

  output$plot_trade_assets <- renderPlot({
    #first plot
    p <- graph_trade(
      trade_valuation()$team1,
      trade_valuation()$team2,
      trade_valuation()$upcoming_label
    )
    plot(p)
  })

  output$team1_trade_outlook_title <- renderUI({
    #table 1 title
    req(input$trade_machine_team1) # require input

    h3(str_c(input$trade_machine_team1, "'s Assets Received"))
  })

  output$team1_trade_outlook <- renderDT({
    #table 1
    trade_valuation()$team1 |>
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
    trade_valuation()$team2 |>
      select(-team) |>
      shiny_edit_tables() |>
      dt(page = 5)
  })

  # Reactivity for Page 7

  output$championship_odds <- renderDT({
    #table 1
    champion_odds |>
      shiny_edit_tables() |>
      dt(page = 12, percent = TRUE)
  })

  output$playoff_odds <- renderDT({
    #table 2
    playoff_odds |>
      shiny_edit_tables() |>
      dt(page = 12, percent = TRUE)
  })

  output$bye_odds <- renderDT({
    #table 3
    bye_odds |>
      shiny_edit_tables() |>
      dt(page = 12, percent = TRUE)
  })

  output$n1_pick_odds <- renderDT({
    #table 4
    n1_pick_odds |>
      shiny_edit_tables() |>
      dt(page = 12, percent = TRUE)
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
      dt(page = 12, percent = "Probability")
  })

  # Reactivity for Page 8

  output$players_top_future_value <- renderDT({
    #table 1
    req(input$enter_position) #require input
    player_total_value |>
      top_future_value_player(input$enter_position) |>
      dt(page = 5)
  })

  output$future_value_over_time <- renderPlotly({
    # first plot
    req(input$enter_players) #require input
    future_value_time |> plot_over_time(input$enter_players)
  })

  output$comparable_future_value <- renderPlotly({
    # first plot
    req(input$enter_player) #require input
    future_value_time |>
      comparable_players(player_total_value, input$enter_player)
  })

  # Reactivity for Page 9

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
      dt(page = 2, percent = "Win Probability")
  })

  output$total_success <- renderDT({
    #table 2
    total_success |>
      shiny_edit_tables() |>
      dt(page = 12)
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
      dt(page = 12, percent = "Win %")
  })

  # Reactivity for Page 11

  output$championship <- renderDT({
    # table 1
    championships_df |>
      shiny_edit_tables() |>
      dt(page = 5)
  })

  output$finalists <- renderDT({
    # table 2
    finals_df |>
      shiny_edit_tables() |>
      dt(page = 5)
  })

  output$playoffs <- renderDT({
    # table 3
    playoffs_df |>
      shiny_edit_tables() |>
      dt(page = 6)
  })

  output$most_wins <- renderDT({
    # table 4
    req(input$enter_round_history, input$enter_season_history)
    compute_most_wins(
      wins_df,
      input$enter_round_history,
      input$enter_season_history
    ) |>
      dt(page = 5, percent = "Win Percentage")
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

  # rookie pick model fit, on the Modeling and Draft Grades pages
  draft_fit_chart <- function() {
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

    as_plotly(p, tooltip = c("y", "text"))
  }

  output$draft_fit <- renderPlotly(draft_fit_chart())
  output$draft_fit_grades <- renderPlotly(draft_fit_chart())
}

# Run the App
# ggplot charts take the page's colors and fonts
thematic::thematic_shiny()
shinyApp(ui, server)
