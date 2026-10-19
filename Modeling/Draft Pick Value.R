# Estimate Rookie Draft Slot Value (from previous years)
# one downside of this is we can't actually peer into the future and see
# how good the rookies are, but we can see how good previous rookies turned out to be
#
# For each rookie draft pick, four quantities are modeled as a heteroskedastic
# polynomial regression on pick number (MCMC Samplers.R):
#   total value  = realized value added + future value (future value is already discounted)
#   first year, second year, third year = value added in the player's 1st/2nd/3rd season
# A season's target is
#   finished:     realized value added (one row per player)
#   in progress:  realized value added + (1 - p) * projection, p = share of season played
#   future:       projection
# Projected targets enter as all 39 simulated quantiles with weight 1/39 each, so a
# player counts once but his projection uncertainty is kept.
# Normally run once a year, after the season.

suppressPackageStartupMessages({
  library("here")
  library("plotly")
})

demonstrate_fit <- FALSE # set to true to show the fit and residual plots
mcmc_seed <- 123 # seed for the samplers

# load MCMC Samplers
source(here("Modeling/MCMC Samplers.R"))
# quantile_probs, season_context()
source(here("Modeling/Player Total Value Functions.R"))

# Load Data ----------------------------------------------------------------

draft_picks <- readRDS(here("Data/derived/draft_picks.rds"))
player_simulations <- readRDS(here("Modeling/models/player_simulations.rds"))

player_total_value <- read_csv(
  here("Shiny/Saved Files/player_total_value.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  mutate(
    # future value only covers the rest of the current season, so the realized
    # value of that season is not counted twice
    total_value = rowSums(across(matches("^sva_\\d{4}$"))) + future_value
  ) |>
  select(player_id, total_value)

season_value_added <- read_csv(
  here("Shiny/Saved Files/season_value_added.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  select(season, player_id, realized = total_value_added)

player_info <- read_csv(
  here("Data/raw/player_info.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
) |>
  distinct(player_id, .keep_all = TRUE)

season_dates <- read_csv(
  here("Shiny/Saved Files/season_dates.csv"),
  show_col_types = FALSE
)

# draft metadata cached by Scraping.R: each draft's own season, rounds, and status
cache_dir <- here("Data/raw/sleeper")
read_cache <- function(file) readRDS(file.path(cache_dir, file))$data

draft_meta <- list.files(cache_dir, pattern = "^drafts_\\d{4}\\.rds$") |>
  map(read_cache) |>
  list_flatten() |>
  map(\(d) {
    tibble(
      draft_id = as.character(d$draft_id),
      season = d$season,
      rounds = d$rounds,
      status = d$status
    )
  }) |>
  bind_rows()

latest_league <- list.files(cache_dir, pattern = "^league_\\d{4}\\.rds$") |>
  sort() |>
  last()
rookie_rounds <- read_cache(latest_league)$settings$draft_rounds %||%
  min(draft_meta$rounds)

# season s and the share p of it that has been played
context <- season_context(today(), season_dates)

# Rookie Draft Picks -------------------------------------------------------

rookie_picks <- bind_rows(draft_picks) |>
  mutate(
    draft_id = as.character(draft_id),
    player_id = as.character(player_id)
  ) |>
  inner_join(
    draft_meta |> filter(rounds <= rookie_rounds, status == "complete"),
    by = join_by(draft_id)
  ) |>
  select(draft_season = season, pick_no, roster_id, player_id) |>
  left_join(
    player_info |> select(player_id, name, position),
    by = join_by(player_id)
  ) |>
  left_join(player_total_value, by = join_by(player_id)) |>
  mutate(total_value = replace_na(total_value, 0))

n_picks <- max(rookie_picks$pick_no)

# Season Targets -------------------------------------------------------------

# simulated projections by player, season, and quantile
projections <- imap(
  player_simulations,
  \(sims, season) {
    sims |>
      select(player_id, starts_with("proj_tva_")) |>
      pivot_longer(
        -player_id,
        names_to = "quantile",
        values_to = "projection"
      ) |>
      mutate(season = as.numeric(season))
  }
) |>
  bind_rows()

projected_seasons <- unique(projections$season)

# value added for each rookie in the k-th season after his draft
season_targets <- function(k) {
  picks <- rookie_picks |>
    mutate(target_season = draft_season + k - 1) |>
    # only seasons that have started or that the simulations cover
    filter(
      target_season <= context$season | target_season %in% projected_seasons
    ) |>
    left_join(
      season_value_added,
      by = join_by(player_id, target_season == season)
    ) |>
    mutate(realized = replace_na(realized, 0))

  finished <- picks |>
    filter(
      target_season < context$season |
        (target_season == context$season & context$p == 1)
    ) |>
    transmute(pick_no, y = realized, weight = 1)

  in_progress <- picks |>
    filter(target_season == context$season, context$p < 1)
  future <- picks |>
    filter(target_season > context$season)

  projected <- bind_rows(in_progress, future) |>
    left_join(
      projections,
      by = join_by(player_id, target_season == season),
      relationship = "one-to-many"
    ) |>
    mutate(
      # players without simulations are out of the league
      projection = replace_na(projection, 0),
      remaining = if_else(target_season == context$season, 1 - context$p, 1),
      y = realized + remaining * projection
    ) |>
    mutate(weight = 1 / n(), .by = c(player_id, target_season)) |>
    select(pick_no, y, weight)

  bind_rows(finished, projected)
}

model_data <- list(
  "total value" = rookie_picks |>
    transmute(pick_no, y = total_value, weight = 1),
  "first year" = season_targets(1),
  "second year" = season_targets(2),
  "third year" = season_targets(3)
)

# Polynomial Regression on Pick Number -----------------------------------------

# design matrix: pick number and its square root, scaled with the training data's
# center and scale (the same scaling is used for the picks we predict)
design_reference <- function(pick_no) {
  raw <- cbind(pick_no = pick_no, pn_2 = sqrt(pick_no))
  list(center = colMeans(raw), scale = apply(raw, 2, sd))
}

design_matrix <- function(pick_no, reference) {
  raw <- cbind(pick_no = pick_no, pn_2 = sqrt(pick_no))
  scaled <- sweep(sweep(raw, 2, reference$center), 2, reference$scale, "/")
  cbind(scaled, intercept = 1)
}

# posterior predictive quantiles (one row per quantile) for picks 1..n_picks
fit_pick_model <- function(data, seed) {
  reference <- design_reference(data$pick_no)

  set.seed(seed)
  samples <- het_reg_mh_sampler(
    Y = data$y,
    X = design_matrix(data$pick_no, reference),
    new_X = design_matrix(seq_len(n_picks), reference),
    weights = data$weight,
    proposal_sd = c(.1, .1, .5)
  )

  apply(samples$new_y, 2, quantile, probs = quantile_probs)
}

tic()
quantiles_list <- imap(
  model_data,
  \(data, metric) {
    fit_pick_model(data, mcmc_seed + match(metric, names(model_data)))
  }
)
toc()

# Write to CSV ------------------------------------------------------------

rookie_draft_values <- imap(
  quantiles_list,
  \(quantiles, metric) {
    df <- t(quantiles) |> as_tibble(.name_repair = "minimal")
    colnames(df) <- paste0("proj_tva_", seq_along(quantile_probs) * 2.5)

    df |>
      mutate(pick_no = seq_len(n_picks), metric = metric)
  }
) |>
  bind_rows() |>
  relocate(pick_no, metric)

write_csv(
  rookie_draft_values,
  here("Data/model_output/rookie_draft_values.csv")
)

# total value fit, shown on the Modeling page
quantiles_tv <- quantiles_list[["total value"]]

draft_fit_plot <- tibble(
  median = quantiles_tv[20, ],
  lower = quantiles_tv[1, ],
  upper = quantiles_tv[39, ],
  pick_no = seq_len(n_picks)
) |>
  right_join(rookie_picks, by = join_by(pick_no)) |>
  mutate("Total Value" = round(total_value, digits = 2)) |>
  select(name, pick_no, median, lower, upper, `Total Value`) |>
  distinct()

write_csv(draft_fit_plot, here("Shiny/Saved Files/draft_fit_plot.csv"))

# Demonstrate Model Fit ----------------------------------------------------
if (demonstrate_fit) {
  print(
    rookie_picks |>
      ggplot() +
      geom_point(aes(
        x = pick_no,
        y = total_value,
        color = as.factor(draft_season)
      ))
  )

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

  print(ggplotly(p, tooltip = c("y", "text")))

  # fit and residuals for every metric (projected rows shown at their weight)
  iwalk(model_data, \(data, metric) {
    fit <- tibble(
      pick_no = seq_len(n_picks),
      median = quantiles_list[[metric]][20, ],
      lower = quantiles_list[[metric]][1, ],
      upper = quantiles_list[[metric]][39, ]
    )

    print(
      ggplot(fit, aes(x = pick_no)) +
        geom_point(
          data = data,
          aes(y = y, alpha = weight),
          show.legend = FALSE
        ) +
        geom_line(aes(y = median)) +
        geom_errorbar(aes(ymin = lower, ymax = upper)) +
        labs(title = str_c(metric, " fit"), x = "Draft Pick", y = "")
    )

    print(
      data |>
        left_join(fit |> select(pick_no, median), by = join_by(pick_no)) |>
        ggplot(aes(x = pick_no)) +
        geom_point(aes(y = y - median, alpha = weight), show.legend = FALSE) +
        geom_hline(yintercept = 0) +
        labs(title = str_c(metric, " residuals"), x = "Draft Pick", y = "")
    )
  })
}
