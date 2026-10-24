# Grades Support

# libraries
# gt and gtExtras are used only by the shiny = FALSE (interactive) branches, so they are
# called with gt:: / gtExtras:: and load only when needed, not at app start
suppressPackageStartupMessages({
  library("tidyverse")
  theme_set(theme_minimal())
  library("janitor")
  library("plotly")
})

# All Scripts --------------------------------------------------------------
# function to edit tables for shiny
shiny_edit_tables <- function(df) {
  df |>
    mutate(across(where(is.numeric), ~ round(.x, 1))) |>
    rename_with(~ str_replace_all(.x, "_", " "), everything()) |>
    rename_with(~ str_to_title(.x), everything())
}

# player and team names in a table become links: a team name opens Fantasy Teams and a
# player name opens Individual Players (app.R listens for these clicks). A column is
# linked when its values are names; player_choices maps names to player_id (set in
# app_call_data.R). Returns the table and which columns were linked.
link_names <- function(df) {
  linked <- character()
  link <- function(class, id, label) {
    escaped <- map_chr(label, \(x) if (is.na(x)) NA_character_ else htmltools::htmlEscape(x))
    case_when(
      is.na(label) ~ NA_character_,
      is.na(id) ~ escaped,
      TRUE ~ str_c(
        "<a href=\"#\" class=\"", class, "\" data-id=\"",
        map_chr(id, \(x) htmltools::htmlEscape(coalesce(x, ""), attribute = TRUE)),
        "\">", escaped, "</a>"
      )
    )
  }
  for (col in names(df)) {
    values <- df[[col]]
    if (!is.character(values) || all(is.na(values))) next
    if (any(values %in% users$display_name)) {
      team_id <- if_else(values %in% users$display_name, values, NA_character_)
      df[[col]] <- link("team-link", team_id, values)
      linked <- c(linked, col)
    } else if (exists("player_choices") && any(values %in% names(player_choices))) {
      player_id <- unname(player_choices[values])
      df[[col]] <- link("player-link", player_id, values)
      linked <- c(linked, col)
    }
  }
  list(df = df, linked = linked)
}

# app tables. A search box appears when the table runs past one page; paging = FALSE
# shows every row with no page buttons; percent names the columns shown with a % sign
# (TRUE for every numeric column); a "Total" row in the first column is bold. Player and
# team names link to their pages (links = FALSE to skip)
dt <- function(
  df,
  page = 5,
  paging = TRUE,
  dom = NULL,
  selection = "none",
  percent = NULL,
  links = TRUE,
  options = list()
) {
  if (is.null(dom)) {
    dom <- if (!paging) "t" else if (nrow(df) > page) "ftip" else "tip"
  }

  total_row <- nrow(df) > 0 && "Total" %in% df[[1]]
  escape <- TRUE
  if (links && nrow(df) > 0) {
    linked <- link_names(df)
    df <- linked$df
    unlinked <- setdiff(names(df), linked$linked)
    escape <- if (length(unlinked) == 0) FALSE else unlinked
  }

  table <- datatable(
    df,
    rownames = FALSE,
    selection = selection,
    escape = escape,
    class = "compact stripe hover",
    options = modifyList(
      list(
        pageLength = page,
        paging = paging,
        ordering = TRUE,
        scrollX = TRUE,
        dom = dom
      ),
      options
    )
  )

  if (isTRUE(percent)) {
    percent <- names(df)[map_lgl(df, is.numeric)]
  }
  if (length(percent) > 0) {
    table <- table |> formatString(percent, suffix = "%")
  }

  # matched by the first column's value, so the row stays bold on any page or sort
  if (total_row) {
    table <- table |>
      formatStyle(
        names(df)[1],
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  }

  table
}

# a selectable table's row for an id (NULL when it isn't in the table), and the first row
# of the page that shows it (0-based, for DT's displayStart)
row_of <- function(id, ids) {
  row <- match(id, ids)
  if (is.na(row)) NULL else row
}

page_start <- function(id, ids, page = 5) {
  row <- match(id, ids)
  if (is.na(row)) 0 else (row - 1) %/% page * page
}

#position_levels
position_levels <- c("QB", "RB", "WR", "TE", "K", "DST")

# plotly charts in the page's sans-serif font (thematic otherwise leaves them in serif)
as_plotly <- function(p, ...) {
  font_stack <- "'Source Sans Pro', -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif"
  ggplotly(p + theme(text = element_text(family = font_stack)), ...) |>
    layout(font = list(family = font_stack))
}

# Team colors ---------------------------------------------------------------
# one fixed color per team, assigned in roster_id order, so a team has the same color in
# every chart. Add scale_color_team() or scale_fill_team() to any chart that colors by team
# name. (Twelve colors that pass the dataviz palette checks for neighboring colors; with
# this many, rely on hover or labels as well as color.)
team_palette <- c(
  "#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300",
  "#6250d6", "#e34948", "#0f8fa8", "#a35f00", "#b53e9a", "#6f9d00"
)

team_colors <- function() {
  teams <- users |> arrange(roster_id)
  set_names(team_palette[seq_len(nrow(teams))], teams$display_name)
}

scale_color_team <- function(...) {
  scale_color_manual(values = team_colors(), ...)
}

scale_fill_team <- function(...) {
  scale_fill_manual(values = team_colors(), ...)
}

# Individual Players ------------------------------------------------------

basic_info <- function(enter_id) {
  basic_info_df |>
    filter(player_id == enter_id) |>
    select(-name, -player_id) |>
    shiny_edit_tables()
}

# plots a player's expected value added by season (the mean used in future value),
# with 80% and 95% credible regions
plot_future_value <- function(enter_id) {
  enter_name <- plot_future_value_df$name[match(enter_id, plot_future_value_df$player_id)]
  first_season <- min(plot_future_value_df$season)
  season_label <- if (exists("upcoming_season_label")) upcoming_season_label else "Current Season"

  plot_future_value_df |>
    filter(player_id == enter_id) |>
    ggplot(aes(x = season)) +
    geom_ribbon(
      aes(ymin = proj_tva_10, ymax = proj_tva_90),
      fill = "cadetblue4",
      alpha = .5
    ) +
    geom_ribbon(
      aes(ymin = proj_tva_2.5, ymax = proj_tva_97.5),
      fill = "cadetblue1",
      alpha = .5
    ) +
    geom_line(aes(y = expected_tva), color = "indianred3") +
    # the first projected season (the current season in season, the next one in the offseason)
    geom_vline(xintercept = first_season, linetype = "dashed", color = "grey40") +
    annotate(
      "text",
      x = first_season,
      y = Inf,
      label = season_label,
      hjust = -.05,
      vjust = 1.5,
      color = "grey30"
    ) +
    labs(
      title = str_c(enter_name, " Projected Total Value Added"),
      x = "Season",
      y = "Value added"
    ) +
    scale_x_continuous(breaks = scales::breaks_width(1)) +
    geom_point(
      data = season_value_added |> filter(player_id == enter_id),
      aes(x = season, y = total_value_added)
    )
}

tabulate_realized_value <- function(
  va_data,
  enter_id,
  enter_season,
  shiny = FALSE
) {
  player_games <- va_data |>
    filter(player_id == enter_id, season %in% enter_season)
  shiny::validate(shiny::need(
    nrow(player_games) > 0,
    "This player wasn't on a roster in the selected seasons."
  ))
  enter_name <- player_games$name[1]

  df <- player_games |>
    group_by(display_name) |>
    summarize(
      start_week = min(week),
      weeks = n(),
      healthy = sum(sleeper_points > 0),
      starts = sum(type == "starter"),
      `total value added` = sum(value_added),
      # average over the weeks he scored (blank if he never did)
      `mean fantasy points` = if_else(
        healthy > 0,
        sum(sleeper_points) / healthy,
        NA_real_
      )
    ) |>
    arrange(start_week) |>
    select(-start_week)

  if (shiny) {
    df |>
      rename("Team" = display_name) |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE) |>
      gt::fmt_number(
        columns = c(`total value added`, `mean fantasy points`),
        decimals = 2
      ) |>
      gt::cols_label(display_name = "Team") |>
      gt::tab_header(title = str_c(enter_name, ": ", enter_season, " Season"))
  }
}

# Fantasy Teams -----------------------------------------------------------
# grab team assets
grab_team_assets <- function(enter_roster_id, shiny = FALSE) {
  team_name <- users |>
    filter(roster_id == enter_roster_id) |>
    pull(display_name)

  df <- grab_team_assets_df |>
    filter(roster_id == enter_roster_id) |>
    select(-roster_id)

  if (shiny) {
    df |>
      select(-player_id) |>
      rename("Acquired" = "avenue") |>
      shiny_edit_tables()
  } else {
    df |>
      select(-player_id) |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE) |>
      gt::fmt_number(columns = future_value, decimals = 2) |>
      gt::cols_label(future_value = "Future Value", avenue = "Acquired") |>
      gt::tab_header(title = str_c(team_name, " Future Value"))
  }
}

# position outlook
position_outlook <- function(enter_roster_id, shiny = FALSE) {
  team_name <- users |>
    filter(roster_id == enter_roster_id) |>
    pull(display_name)

  df <- position_outlook_df |>
    filter(roster_id == enter_roster_id) |>
    select(-roster_id)

  if (shiny) {
    df |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE) |>
      gt::fmt_number(columns = c(future_value, realized_value), decimals = 2) |>
      gt::cols_label(
        future_value = "Future Value",
        realized_value = "Realized Value"
      ) |>
      gt::tab_header(title = str_c(team_name, " Position Outlook"))
  }
}

# top contributors
grab_team_contributors <- function(
  enter_roster_id,
  enter_season,
  shiny = FALSE
) {
  team_name <- users |>
    filter(roster_id == enter_roster_id) |>
    pull(display_name)

  df <- value_added |>
    filter(roster_id == enter_roster_id, season %in% enter_season) |>
    group_by(name, position) |>
    summarize(
      weeks = n(),
      healthy = sum(sleeper_points > 0),
      starts = sum(type == "starter"),
      total_value_added = sum(value_added),
      fantasy_points = sum(sleeper_points),
      .groups = "keep"
    ) |>
    arrange(desc(total_value_added)) |>
    ungroup()

  if (shiny) {
    df |> shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE) |>
      gt::fmt_number(
        columns = c(total_value_added, fantasy_points),
        decimals = 2
      ) |>
      gt::cols_label(
        total_value_added = "Total Value Added",
        fantasy_points = "Fantasy Points"
      ) |>
      gt::tab_header(title = str_c(team_name, ": ", enter_season, " Season"))
  }
}

# week by week
grab_team_contributors_weekly <- function(
  enter_roster_id,
  enter_season,
  enter_week,
  shiny = FALSE
) {
  enter_season <- max(enter_season)

  team_name <- users |>
    filter(roster_id == enter_roster_id) |>
    pull(display_name)

  df <- value_added |>
    filter(
      roster_id == enter_roster_id,
      season == enter_season,
      week == enter_week,
      type == "starter"
    ) |>
    select(name, position, projection, sleeper_points, value_added)
  shiny::validate(shiny::need(nrow(df) > 0, "No games for this team in this week."))

  df <- df |>
    mutate(position = factor(position, position_levels)) |>
    arrange(position, desc(value_added)) |>
    adorn_totals()

  if (shiny) {
    df |>
      rename(fantasy_points = sleeper_points) |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538() |>
      gt::fmt_number(
        columns = c(value_added, sleeper_points, projection),
        decimals = 2
      ) |>
      gt::cols_label(
        value_added = "Value Added",
        sleeper_points = "Fantasy Points"
      ) |>
      gt::tab_header(
        title = str_c(
          team_name,
          ": ",
          enter_season,
          " Season Week ",
          enter_week
        )
      )
  }
}

overall_grades <- function(value_avenues, enter_roster_id, shiny = FALSE) {
  team_name <- users |>
    filter(roster_id == enter_roster_id) |>
    pull(display_name)

  df <- value_avenues |>
    filter(roster_id == enter_roster_id) |>
    relocate(
      c(avenue, total_realized_value, total_future_value),
      value_over_expected
    ) |>
    select(-roster_id) |>
    arrange(avenue) |>
    janitor::adorn_totals()

  if (shiny) {
    df |>
      rename(
        realized_value = "total_realized_value",
        future_value = "total_future_value"
      ) |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538() |>
      gt::fmt_number(
        columns = c(
          total_realized_value,
          total_future_value,
          value_over_expected
        ),
        decimals = 2
      ) |>
      gt::cols_label(
        total_realized_value = "Realized Value",
        total_future_value = "Future Value",
        value_over_expected = "Value Over Expected"
      ) |>
      gt::tab_header(title = str_c(team_name, " Avenue Grades"))
  }
}

# top acquisitions
top_acquisitions <- function(
  acquisitions,
  enter_roster_id,
  enter_avenue = "All",
  shiny = FALSE
) {
  enter_avenue <- if_else(enter_avenue == "All", " ", enter_avenue)

  team_name <- users |>
    filter(roster_id == enter_roster_id) |>
    pull(display_name)

  df <- acquisitions |>
    filter(str_detect(avenue, enter_avenue), roster_id == enter_roster_id) |>
    arrange(desc(value_over_expected)) |>
    # slice_max(value_over_expected, n = 5) |>
    select(-roster_id, -total_value)

  if (shiny) {
    df |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE) |>
      gt::fmt_number(
        columns = c(realized_value, future_value, value_over_expected),
        decimals = 2
      ) |>
      gt::cols_label(
        realized_value = "Realized Value",
        future_value = "Future Value",
        value_over_expected = "Value Over Expected"
      ) |>
      gt::tab_header(title = str_c(team_name, " Top Acquisitions"))
  }
}


# worst acquisitions
worst_acquisitions <- function(
  acquisitions,
  enter_roster_id,
  enter_avenue = "All",
  shiny = FALSE
) {
  enter_avenue <- if_else(enter_avenue == "All", " ", enter_avenue)

  team_name <- users |>
    filter(roster_id == enter_roster_id) |>
    pull(display_name)

  df <- acquisitions |>
    filter(str_detect(avenue, enter_avenue), roster_id == enter_roster_id) |>
    # slice_min(value_over_expected, n = 5) |>
    select(-roster_id, -total_value) |>
    arrange(value_over_expected)

  if (shiny) {
    df |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE) |>
      gt::fmt_number(
        columns = c(realized_value, future_value, value_over_expected),
        decimals = 2
      ) |>
      gt::cols_label(
        realized_value = "Realized Value",
        future_value = "Future Value",
        value_over_expected = "Value Over Expected"
      ) |>
      gt::tab_header(title = str_c(team_name, " Worst Acquisitions"))
  }
}

# team composition
team_composition <- function(player_avenues, enter_roster_id, shiny = FALSE) {
  team_name <- users |>
    filter(roster_id == enter_roster_id) |>
    pull(display_name)

  df <- player_avenues |>
    filter(roster_id == enter_roster_id) |>
    mutate(
      avenue_type = case_when(
        str_detect(avenue, "initial draft") ~ "initial draft",
        str_detect(avenue, "rookie draft") ~ "rookie draft",
        str_detect(avenue, "transaction") ~ "transaction",
        str_detect(avenue, "trade") ~ "trade",
        str_detect(avenue, "own pick") ~ "own pick",
        .default = NA
      )
    ) |>
    group_by(avenue_type) |>
    summarize(future_value = sum(future_value)) |>
    mutate(proportion = 100 * future_value / sum(future_value)) # percent

  if (shiny) {
    df |>
      rename(avenue = avenue_type) |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE) |>
      gt::fmt_number(columns = c(future_value), decimals = 2) |>
      gt::cols_label(avenue_type = "Avenue", future_value = "Future Value") |>
      gt::tab_header(title = str_c(team_name, " Team Composition"))
  }
}

# a team's bio for the Fantasy Teams page, one row per stat (label, value, and a detail
# line) in display order: this season (record, future value, playoff odds, Elo), then
# all time (record, championships, playoff berths, composite rating)
team_bio <- function(enter_team) {
  ordinal <- function(n) {
    suffix <- case_when(
      n %% 100 %in% 11:13 ~ "th",
      n %% 10 == 1 ~ "st",
      n %% 10 == 2 ~ "nd",
      n %% 10 == 3 ~ "rd",
      TRUE ~ "th"
    )
    str_c(n, suffix)
  }
  rank_of <- function(df, value_col) {
    df |>
      mutate(rank = min_rank(desc(.data[[value_col]]))) |>
      filter(team == enter_team)
  }
  record <- function(games) {
    wins <- sum(games$result == "win")
    losses <- sum(games$result == "loss")
    ties <- sum(games$result == "tie")
    str_c(wins, "-", losses, if (ties > 0) str_c("-", ties))
  }
  count_for <- function(df, col) {
    n <- df[[col]][df$team == enter_team]
    if (length(n) == 0 || is.na(n[1])) 0 else n[1]
  }

  games <- wins_df |> filter(team == enter_team)
  regular <- games |> filter(type == "regular season")
  playoff_games <- games |> filter(type == "winner's bracket")
  n_teams <- nrow(users)

  # this season's record and place in the standings (wins, then points for)
  this_season <- max(wins_df$season)
  current <- regular |> filter(season == this_season)
  standing <- wins_df |>
    filter(season == this_season, type == "regular season") |>
    summarize(wins = sum(result == "win"), points = sum(points), .by = team) |>
    arrange(desc(wins), desc(points)) |>
    mutate(rank = row_number()) |>
    filter(team == enter_team)

  # playoff odds for the first simulated season (Future Standings)
  odds_season <- names(playoff_odds)[2]
  playoff <- playoff_odds |>
    select(team, odds = all_of(odds_season)) |>
    rank_of("odds")
  odds_label <- case_when(
    playoff$odds > 99 & playoff$odds < 100 ~ ">99%",
    playoff$odds > 0 & playoff$odds < 1 ~ "<1%",
    TRUE ~ str_c(round(playoff$odds), "%")
  )

  elo <- weekly_elo |>
    slice_max(date_hide, n = 1, with_ties = FALSE, by = team) |>
    rank_of("elo")
  composite <- total_success |> rank_of("composite_rating")
  future <- all_assets_summary_df |> rank_of("total_future_value")

  berths <- count_for(playoffs_df, "playoff_berths")
  finals <- count_for(finals_df, "finals_appearances")
  titles <- count_for(championships_df, "championships")

  tibble(
    label = c(
      "Current record",
      "Future value",
      "Playoff odds",
      "Elo rating",
      "All-time record",
      "Championships",
      "Playoff berths",
      "Composite rating"
    ),
    value = c(
      record(current),
      scales::comma(future$total_future_value, accuracy = 1),
      odds_label,
      scales::comma(elo$elo, accuracy = 1),
      record(regular),
      as.character(titles),
      as.character(berths),
      scales::comma(composite$composite_rating, accuracy = .1)
    ),
    detail = c(
      str_c(this_season, " season, ", ordinal(standing$rank), " of ", n_teams),
      str_c(ordinal(future$rank), " of ", n_teams, " in total assets"),
      str_c(odds_season, " season, ", ordinal(playoff$rank), " of ", n_teams),
      str_c(ordinal(elo$rank), " of ", n_teams),
      str_c("regular season, ", round(100 * mean(regular$result == "win")), "% wins"),
      str_c(finals, if_else(finals == 1, " finals appearance", " finals appearances")),
      if_else(
        nrow(playoff_games) > 0,
        str_c(record(playoff_games), " in playoff games"),
        "no playoff games yet"
      ),
      str_c(ordinal(composite$rank), " of ", n_teams)
    )
  )
}

# Draft Grades ------------------------------------------------------------

# best picks- lol we vastly undervalued rookie picks
best_picks <- function(enter_draft, shiny) {
  enter_draft <- if_else(enter_draft == "All", " ", enter_draft)

  df <- picks_df |>
    filter(str_detect(draft, enter_draft)) |>
    left_join(users, by = join_by(roster_id)) |>
    select(
      display_name,
      pick_no,
      name,
      position,
      realized_value,
      future_value,
      value_over_expected
    ) |>
    arrange(desc(value_over_expected))

  if (shiny) {
    df |>
      rename(
        team_name = display_name,
        pick = pick_no
      ) |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538() |>
      gt::fmt_number(
        columns = c(value_over_expected, realized_value, future_value),
        decimals = 2
      ) |>
      gt::cols_label(
        display_name = "Team Name",
        value_over_expected = "Value Over Expected",
        realized_value = "Realized Value",
        future_value = "Future Value",
        pick_no = "Pick"
      ) |>
      gt::tab_header(title = str_c(str_to_title(enter_draft), " Best Picks"))
  }
}

# worst picks - some massive first round busts
worst_picks <- function(enter_draft, shiny) {
  enter_draft <- if_else(enter_draft == "All", " ", enter_draft)

  df <- picks_df |>
    filter(str_detect(draft, enter_draft)) |>
    left_join(users, by = join_by(roster_id)) |>
    select(
      display_name,
      pick_no,
      name,
      position,
      realized_value,
      future_value,
      value_over_expected
    ) |>
    arrange(value_over_expected)

  if (shiny) {
    df |>
      rename(
        team_name = display_name,
        pick = pick_no
      ) |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538() |>
      gt::fmt_number(
        columns = c(value_over_expected, realized_value, future_value),
        decimals = 2
      ) |>
      gt::cols_label(
        display_name = "Team Name",
        value_over_expected = "Value Over Expected",
        realized_value = "Realized Value",
        future_value = "Future Value",
        pick_no = "Pick"
      ) |>
      gt::tab_header(title = str_c(str_to_title(enter_draft), " Worst Picks"))
  }
}

draft_rankings <- function(enter_draft, shiny) {
  enter_draft <- if_else(enter_draft == "All", " ", enter_draft)

  df <- picks_df |>
    filter(str_detect(draft, enter_draft)) |>
    left_join(users, by = join_by(roster_id)) |>
    group_by(display_name) |>
    summarize(
      realized_value = sum(realized_value, na.rm = TRUE),
      future_value = sum(future_value, na.rm = TRUE),
      value_over_expected = sum(value_over_expected, na.rm = TRUE)
    ) |>
    arrange(desc(value_over_expected))

  if (shiny) {
    df |>
      rename(team_name = display_name) |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538() |>
      gt::fmt_number(
        columns = c(value_over_expected, realized_value, future_value),
        decimals = 2
      ) |>
      gt::cols_label(
        display_name = "Team Name",
        value_over_expected = "Value Over Expected",
        realized_value = "Realized Value",
        future_value = "Future Value"
      ) |>
      gt::tab_header(title = str_c(str_to_title(enter_draft), " Draft Grades"))
  }
}

# a player's weekly results in one season, one bar per week split into replacement level
# and value added (points = replacement + value added, for weeks he started):
#   gray up to the replacement level and blue above it when he beat replacement
#   red when he fell short, with a dashed line at the replacement level
#   hollow bars for bench weeks (no value added); orange dots are his projection
weekly_results <- function(va_data, enter_id, enter_season) {
  player_games <- va_data |>
    filter(player_id == enter_id, season == max(enter_season))
  shiny::validate(shiny::need(
    nrow(player_games) > 0,
    str_c("This player wasn't on a roster in ", max(enter_season), ".")
  ))
  enter_name <- player_games$name[1]

  signed <- function(x) str_c(if_else(x >= 0, "+", ""), round(x, 1))
  weeks <- player_games |>
    mutate(
      started = type == "starter",
      replacement = sleeper_points - value_added,
      xmin = week - .35,
      xmax = week + .35,
      text = if_else(
        started,
        str_c(
          "Week ", week, " (started for ", display_name, ")",
          "<br>Points: ", round(sleeper_points, 1),
          "<br>Replacement level: ", round(replacement, 1),
          "<br>Value added: ", signed(value_added),
          "<br>Projection: ", round(projection, 1)
        ),
        str_c(
          "Week ", week, " (bench for ", display_name, ")",
          "<br>Points: ", round(sleeper_points, 1),
          "<br>Projection: ", round(projection, 1)
        )
      )
    )

  above <- weeks |> filter(started, value_added >= 0)
  below <- weeks |> filter(started, value_added < 0)
  bench <- weeks |> filter(!started)

  # layers are added only when they have weeks to draw (plotly fails on empty layers)
  p <- ggplot(weeks) +
    geom_hline(yintercept = 0, color = "grey60")

  # beat replacement: replacement level (gray) plus value added (blue)
  if (nrow(above) > 0) {
    p <- p +
      geom_rect(
        data = above,
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = replacement, text = text),
        fill = "#c3c2b7"
      ) +
      geom_rect(
        data = above,
        aes(xmin = xmin, xmax = xmax, ymin = replacement, ymax = sleeper_points, text = text),
        fill = "#2a78d6"
      )
  }
  # fell short of replacement: red bar, dashed line at the replacement level
  if (nrow(below) > 0) {
    p <- p +
      geom_rect(
        data = below,
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = sleeper_points, text = text),
        fill = "#e34948"
      ) +
      geom_segment(
        data = below,
        aes(x = xmin, xend = xmax, y = replacement, yend = replacement, text = text),
        linetype = "dashed",
        color = "#52514e",
        linewidth = .6
      )
  }
  # bench weeks
  if (nrow(bench) > 0) {
    p <- p +
      geom_rect(
        data = bench,
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = sleeper_points, text = text),
        fill = NA,
        color = "#8a8980",
        linewidth = .4
      )
  }
  # projection
  p <- p +
    geom_line(aes(x = week, y = projection), color = "#eb6834", linewidth = .4) +
    geom_point(aes(x = week, y = projection, text = text), color = "#eb6834", size = 1.8) +
    scale_x_continuous(
      breaks = scales::breaks_width(1),
      limits = c(.5, max(weeks$week) + .5),
      expand = c(0, 0)
    ) +
    labs(
      title = str_c(enter_name, ": ", max(enter_season), " Weekly Results"),
      x = "Week",
      y = "Fantasy points"
    ) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

  as_plotly(p, tooltip = "text") |>
    plotly::layout(showlegend = FALSE)
}

# Trade Grades ------------------------------------------------------------

inspect_individual_trade <- function(trade_id, shiny = FALSE) {
  title <- total_trade_value[[trade_id]] |>
    select(team_name) |>
    distinct() |>
    map_chr(~ str_c(.x, collapse = ", ")) %>%
    str_c(
      total_trade_value[[trade_id]]$season[1],
      " W",
      total_trade_value[[trade_id]]$week[1],
      " Trade between ",
      .
    )

  # what each team received: players, picks, and its own roster size adjustment (a
  # partner's adjustment counts against the team in its grade but isn't listed)
  df <- total_trade_value[[trade_id]] |>
    filter(type == "add") |>
    select(-season, -week, -type, -player_id) |>
    group_by(team_name) |>
    group_map(~ .x |> adorn_totals("row"), .keep = TRUE) |>
    bind_rows()

  if (shiny) {
    df |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE) |>
      gt::fmt_number(
        columns = c(future_value, realized_value, total_value),
        decimals = 2
      ) |>
      gt::cols_label(
        team_name = "Team",
        future_value = "Future Value",
        realized_value = "Realized Value",
        total_value = "Total Value"
      ) |>
      gt::tab_header(title = title)
  }
}

# Transaction Grades ------------------------------------------------------

inspect_individual_transaction <- function(transaction_id, shiny = FALSE) {
  title <- total_transaction_value[[transaction_id]]$team_name[1] |>
    str_c(
      "'s ",
      total_transaction_value[[transaction_id]]$season[1],
      " W",
      total_transaction_value[[transaction_id]]$week[1],
      " Transaction"
    )

  df <- total_transaction_value[[transaction_id]] |>
    mutate(action = if_else(type == "add", "+", "-")) |>
    select(
      action,
      name,
      position,
      realized_value,
      future_value,
      total_value
    ) |>
    adorn_totals("row")

  if (shiny) {
    df |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE) |>
      gt::fmt_number(
        columns = c(future_value, realized_value, total_value),
        decimals = 2
      ) |>
      gt::cols_label(
        future_value = "Future Value",
        realized_value = "Realized Value",
        total_value = "Total Value"
      ) |>
      gt::tab_header(title = title)
  }
}

# Matchups ----------------------------------------------------------------
# function that gets matchup records

team_matchup_record <- function(
  team_records_df,
  enter_team,
  enter_round,
  enter_seasons,
  shiny
) {
  if ("All" %in% enter_round) {
    enter_round <- unique(team_records_df$round)
  }

  if (missing(enter_seasons)) {
    enter_seasons <- team_records_df |> pull(season) |> unique()
  }

  games <- team_records_df |>
    filter(
      team == enter_team,
      round %in% enter_round,
      season %in% enter_seasons
    )
  shiny::validate(shiny::need(nrow(games) > 0, "No games match these filters."))

  # record and points for a set of games
  record <- function(df) {
    df |>
      summarize(
        games = n(),
        wins = sum(outcome == "win"),
        losses = sum(outcome == "loss"),
        `win %` = 100 * wins / games,
        `points for` = sum(points),
        `points against` = sum(opp_points),
        `avg points for` = mean(points),
        `avg points against` = mean(opp_points)
      )
  }

  # one row per opponent, then the total over all games
  df <- bind_rows(
    games |>
      group_by(opponent) |>
      record() |>
      arrange(desc(wins), losses),
    games |>
      record() |>
      mutate(opponent = "Total")
  )

  if (shiny) {
    df |>
      shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE)
  }
}

# Future Standings Script -------------------------------------------------

most_common_finish <- function(
  most_common_finish_df,
  enter_season,
  shiny = TRUE
) {
  df <- most_common_finish_df |>
    filter(season == enter_season) |>
    select(team, finish, probability)

  if (shiny) {
    df |> shiny_edit_tables()
  } else {
    df |>
      gt::gt() |>
      gtExtras::gt_theme_538(quiet = TRUE)
  }
}

# Player Rankings -----------------------------------------------------------

top_future_value_player <- function(player_total_value, enter_position) {
  if ("All" %in% enter_position) {
    enter_position <- unique(player_total_value$position)
  }

  player_total_value |>
    filter(position %in% enter_position) |>
    rowwise() |>
    mutate(
      realized_value = sum(c_across(contains("sva_"))),
      age = lubridate::time_length(
        lubridate::interval(birth_date, today()),
        unit = "years"
      )
    ) |>
    ungroup() |>
    select(name, position, age, realized_value, future_value) |>
    arrange(desc(future_value)) |>
    shiny_edit_tables()
}

# future value over time for the selected players (by player_id). With moves (a player's
# team changes from player_moves.csv), dashed lines mark each trade, add, and drop; hover
# the marker at the top for what happened, and click it to open that trade or
# transaction (key = "tab|id", read in app.R from the plot's source)
plot_over_time <- function(
  future_value_time,
  enter_ids,
  moves = NULL,
  source = "A"
) {
  df <- future_value_time |>
    filter(player_id %in% enter_ids, !is.na(future_value)) |>
    mutate(
      text = str_c(name, "<br>", format(date, "%b %e, %Y"), "<br>Future value: ", round(future_value, 1))
    )

  p <- ggplot(df) +
    geom_line(aes(x = date, y = future_value, color = name, group = name, text = text)) +
    labs(x = "Date", y = "Future value") +
    theme(legend.position = "none")

  if (!is.null(moves) && nrow(df) > 0) {
    top <- max(df$future_value) * 1.08
    bottom <- min(0, min(df$future_value))
    marks <- moves |>
      filter(player_id %in% enter_ids, date >= min(df$date), date <= max(df$date)) |>
      summarize(
        text = str_c(format(first(date), "%b %e, %Y"), "<br>", str_c(label, collapse = "<br>")),
        key = str_c(first(tab), "|", first(target_id)),
        .by = date
      )
    if (nrow(marks) > 0) {
      p <- p +
        geom_segment(
          data = marks,
          aes(x = date, xend = date, y = bottom, yend = top),
          linetype = "dashed",
          color = "grey55",
          linewidth = .4
        ) +
        geom_point(
          data = marks,
          aes(x = date, y = top, text = str_c(text, "<br><i>Click to open the grade</i>"), key = key),
          shape = 25,
          size = 2.6,
          fill = "#52514e",
          color = "#52514e"
        )
    }
  }

  # the dashed lines ignore the mouse: each one ends exactly at its marker, so otherwise
  # it takes the marker's hover and click (and the click has no trade or transaction)
  chart <- as_plotly(p, tooltip = "text", source = source) |> plotly_build()
  chart$x$data <- map(chart$x$data, \(trace) {
    if (identical(trace$line$dash, "dash")) trace$hoverinfo <- "skip"
    trace
  })
  chart
}

# future value over time for a player and the four players closest to him in future
# value at his position (two above and two below when possible)
comparable_players <- function(
  future_value_time,
  player_total_value,
  enter_id
) {
  player <- player_total_value |>
    filter(player_id == enter_id, !is.na(future_value))
  shiny::validate(shiny::need(
    nrow(player) == 1,
    "This player has no current future value to compare."
  ))

  similar_players <- player_total_value |>
    filter(position == player$position, !is.na(future_value)) |>
    arrange(desc(future_value))

  n <- nrow(similar_players)
  player_row <- match(enter_id, similar_players$player_id)
  first_row <- max(1, min(player_row - 2, n - 4))
  ids <- similar_players$player_id[first_row:min(n, first_row + 4)]

  plot_over_time(future_value_time, ids)
}

# Team Rankings -----------------------------------------------------------

graph_elo <- function(weekly_elo) {
  elo <- weekly_elo |>
    mutate(
      season = 2000 + as.numeric(str_extract(date, "^\\d+")),
      week = as.numeric(str_remove(date, "^\\d+w")),
      hover = str_c(team, "<br>", season, " week ", week, "<br>Elo ", round(elo))
    )
  # each season starts at its week 0 row
  season_starts <- elo |>
    filter(week == 0) |>
    distinct(season, date_hide)

  p <- elo |>
    ggplot(aes(x = date_hide, y = elo, color = team, group = team, text = hover)) +
    geom_vline(
      data = season_starts,
      aes(xintercept = date_hide),
      linetype = "dashed",
      color = "grey70"
    ) +
    geom_line() +
    scale_x_continuous(
      breaks = season_starts$date_hide,
      labels = season_starts$season
    ) +
    scale_color_team() +
    labs(x = "", y = "Elo", title = "Team Elo") +
    theme(legend.position = "none", panel.grid.minor = element_blank())

  as_plotly(p, tooltip = "text")
}

# compute win probability of two teams
elo_win_probability <- function(team1_elo_df, team2_elo_df) {
  elo_diff <- team1_elo_df$elo - team2_elo_df$elo

  perc <- (1 / (10^(-elo_diff / 400) + 1))

  bind_rows(team1_elo_df, team2_elo_df) |>
    ungroup() |>
    mutate(
      win_probability = 100 * c(perc, 1 - perc)
    ) |>
    select(team, elo, win_probability) |>
    shiny_edit_tables()
}

# History -----------------------------------------------------------------

# compute most wins
compute_most_wins <- function(wins_df, enter_round, enter_season) {
  if ("All" %in% enter_round) {
    enter_round <- unique(wins_df$type)
  }

  wins_df |>
    filter(type %in% enter_round, season %in% enter_season) |>
    group_by(team) |>
    summarize(
      wins = sum(result == "win"),
      losses = sum(result == "loss"),
      games = n()
    ) |>
    mutate(win_percentage = 100 * wins / games) |> # numeric, so it sorts correctly
    select(team, wins, losses, win_percentage) |>
    arrange(desc(wins)) |>
    shiny_edit_tables()
}

# compute total points scored
compute_total_points <- function(wins_df, enter_round, enter_season) {
  if ("All" %in% enter_round) {
    enter_round <- unique(wins_df$type)
  }

  wins_df |>
    filter(type %in% enter_round, season %in% enter_season) |>
    group_by(team) |>
    summarize(
      points_for = sum(points),
      points_against = sum(opp_points),
      average_points = mean(points)
    ) |>
    select(team, points_for, average_points, points_against) |>
    arrange(desc(average_points)) |>
    shiny_edit_tables()
}

# compute most points scored in a game
highest_team_total <- function(wins_df, enter_round, enter_season) {
  if ("All" %in% enter_round) {
    enter_round <- unique(wins_df$type)
  }

  wins_df |>
    arrange(desc(points)) |>
    filter(type %in% enter_round, season %in% enter_season) |>
    mutate(date = str_c(season, " Week ", week), index = row_number()) %>%
    filter(index < 51 | index > nrow(.) - 50) |>
    select(team, points, date, opponent) |>
    shiny_edit_tables()
}

# fantasy points scored as a starter in the selected seasons
compute_total_points_player <- function(
  value_added,
  wins_df,
  enter_round,
  enter_season,
  enter_position
) {
  if ("All" %in% enter_round) {
    enter_round <- unique(wins_df$type)
  }

  if ("All" %in% enter_position) {
    enter_position <- unique(value_added$position)
  }

  value_added |>
    left_join(
      wins_df |> select(season, week, type, roster_id),
      by = join_by(season, week, roster_id)
    ) |>
    filter(
      type.x == "starter", # points count only when the player was started
      type.y %in% enter_round,
      season %in% enter_season,
      position %in% enter_position
    ) |>
    group_by(name) |>
    summarize(total_fantasy_points = sum(sleeper_points)) |>
    arrange(desc(total_fantasy_points)) |>
    slice_max(total_fantasy_points, n = 100) |>
    shiny_edit_tables()
}

# compute total value added in season
compute_value_added_player <- function(
  value_added,
  wins_df,
  enter_round,
  enter_season,
  enter_position
) {
  if ("All" %in% enter_round) {
    enter_round <- unique(wins_df$type)
  }

  if ("All" %in% enter_position) {
    enter_position <- unique(value_added$position)
  }

  value_added |>
    left_join(
      wins_df |> select(season, week, type, roster_id),
      by = join_by(season, week, roster_id)
    ) |>
    filter(
      type.y %in% enter_round,
      season %in% enter_season,
      position %in% enter_position
    ) |>
    group_by(name) |>
    summarize(realized_value = sum(value_added)) |>
    arrange(desc(realized_value)) |>
    slice_max(realized_value, n = 100) |>
    shiny_edit_tables()
}

# most fantasy points in a game as a starter
highest_player_total <- function(
  value_added,
  wins_df,
  enter_round,
  enter_season,
  enter_position
) {
  if ("All" %in% enter_round) {
    enter_round <- unique(wins_df$type)
  }

  if ("All" %in% enter_position) {
    enter_position <- unique(value_added$position)
  }

  value_added |>
    left_join(
      wins_df |> select(season, week, type, roster_id),
      by = join_by(season, week, roster_id)
    ) |>
    filter(
      type.x == "starter", # points count only when the player was started
      type.y %in% enter_round,
      season %in% enter_season,
      position %in% enter_position
    ) |>
    arrange(desc(sleeper_points)) |>
    mutate(date = str_c(season, " Week ", week)) |>
    rename(fantasy_points = sleeper_points, team = display_name) |>
    select(name, fantasy_points, team, date) |>
    slice_max(fantasy_points, n = 100) |>
    shiny_edit_tables()
}

# Trade Machine -----------------------------------------------------------
# grades a trade: team1 sends team1_players and team2 sends team2_players (name_codes from
# assets_df). Each team's assets are looked up on its own roster. A team that receives
# more players than it sends must drop players, and one that sends more can add free
# agents, valued with roster_spot_value (picks take no roster spot).
grade_trade_wrapper <- function(
  assets_df,
  roster_spot_value,
  team1,
  team1_players,
  team2,
  team2_players
) {
  team1_assets_lost <- assets_df |>
    filter(display_name == team1, name_code %in% team1_players)
  team2_assets_lost <- assets_df |>
    filter(display_name == team2, name_code %in% team2_players)

  team1_assets_gained <- team2_assets_lost |>
    mutate(team = team1) |>
    select(team, name, position, future_value, upcoming_year)

  team2_assets_gained <- team1_assets_lost |>
    mutate(team = team2) |>
    select(team, name, position, future_value, upcoming_year)

  # roster spots: n spots of one type (add = spot gained, drop = spot lost)
  spot_rows <- function(team, spot_type, n) {
    roster_spot_value |>
      filter(type == spot_type) |>
      transmute(
        team = team,
        name = "roster size adjustment",
        position = if_else(spot_type == "add", "gained", "lost"),
        future_value = future_value * n,
        upcoming_year = upcoming_year * n
      )
  }

  # players team1 sends minus players team2 sends
  roster_size_adj <- sum(!is.na(team1_assets_lost$player_id)) -
    sum(!is.na(team2_assets_lost$player_id))

  if (roster_size_adj > 0) {
    team1_assets_gained <- bind_rows(
      team1_assets_gained,
      spot_rows(team1, "add", roster_size_adj)
    )
    team2_assets_gained <- bind_rows(
      team2_assets_gained,
      spot_rows(team2, "drop", roster_size_adj)
    )
  } else if (roster_size_adj < 0) {
    team1_assets_gained <- bind_rows(
      team1_assets_gained,
      spot_rows(team1, "drop", -roster_size_adj)
    )
    team2_assets_gained <- bind_rows(
      team2_assets_gained,
      spot_rows(team2, "add", -roster_size_adj)
    )
  }

  # the upcoming season is the current season in season and the next one in the offseason
  upcoming_label <- first(assets_df$upcoming_label, default = "Current Season")

  overall_value <- sum(team1_assets_gained$future_value) -
    sum(team2_assets_gained$future_value)
  ov_statement <- str_c(
    "Trade Total Future Value Favors: ",
    if_else(overall_value > 0, team1, team2),
    " (",
    round(abs(overall_value), digits = 2),
    ")"
  )

  immediate_value <- sum(team1_assets_gained$upcoming_year) -
    sum(team2_assets_gained$upcoming_year)
  iv_statement <- str_c(
    "Trade ",
    upcoming_label,
    " Value Favors: ",
    if_else(immediate_value > 0, team1, team2),
    " (",
    round(abs(immediate_value), digits = 2),
    ")"
  )

  # each table ends with its total
  add_total <- function(df, team) {
    bind_rows(
      df,
      tibble(
        team = team,
        name = "Total",
        future_value = sum(df$future_value),
        upcoming_year = sum(df$upcoming_year)
      )
    )
  }

  list(
    "team1" = add_total(team1_assets_gained, team1),
    "team2" = add_total(team2_assets_gained, team2),
    "ov_statement" = ov_statement,
    "iv_statement" = iv_statement,
    "ov_value" = abs(overall_value),
    "ov_favor" = if_else(overall_value > 0, team1, team2),
    "iv_value" = abs(immediate_value),
    "iv_favor" = if_else(immediate_value > 0, team1, team2),
    "upcoming_label" = upcoming_label
  )
}

graph_trade <- function(
  team1_assets_received,
  team2_assets_received,
  upcoming_label = "Current Season"
) {
  bind_rows(team1_assets_received, team2_assets_received) |>
    filter(name == "Total") |>
    select(team, future_value, upcoming_year) |>
    pivot_longer(
      cols = c(future_value, upcoming_year),
      names_to = "type",
      values_to = "value"
    ) |>
    mutate(
      type = if_else(type == "future_value", "Future Value", upcoming_label)
    ) |>
    ggplot() +
    geom_bar(
      aes(x = type, y = value, fill = team),
      stat = "identity",
      position = position_dodge(width = 1),
      width = 1
    ) +
    scale_fill_team() +
    labs(x = "", y = "") +
    theme(legend.title = element_blank())
}
