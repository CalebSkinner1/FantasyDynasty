# Future Value and Total Value

suppressPackageStartupMessages({
  library("here")
  library("bundle")
  library("face")
})

message("begin computing Player Total Value...")
demonstrate_fit <- FALSE # set to true to rerun the model fit images
train_models <- FALSE # set to true to retrain BART models

source(here("Data Manipulation/Scrape Support.R")) # grab functions
source(here("Modeling/Player Total Value Functions.R")) # grab functions
season_value_added <- read_csv(here("Data/sva.csv"), show_col_types = FALSE) # shortcut
player_info <- read_csv(here("Data/player_info.csv"), show_col_types = FALSE) # shortcut
season_dates <- read_csv(here("Data/season_dates.csv"), show_col_types = FALSE)

ktc_list <- list.files(
  path = here("Data/ktc values"),
  full.names = T
) %>%
  set_names(basename(.)) %>%
  map(~ read_csv(.x, show_col_types = FALSE))

# these are the names of the dudes that I'll compute the future value of repetitively
future_value_names <- map_dfr(ktc_list, name_correction) %>%
  distinct(name) %>%
  left_join(player_info, by = join_by(name)) %>%
  select(-player_id) %>%
  filter(
    !str_detect(name, c("Mid")),
    !str_detect(name, c("Early")),
    !str_detect(name, c("Late"))
  ) |>
  bind_rows(
    player_info |>
      filter(name == "Marshawn Lynch") |>
      select(name, position, birth_date, years_exp)
  ) |>
  drop_na()

# write_csv(future_value_names, here("Data/future_value_names.csv"))

# organize data sets
ktc_begin_end_dates <- list(
  year1 = list(
    pre_ktc_date = ymd("2024-08-23"),
    post_ktc_date = ymd("2025-08-24")
  ),
  year2 = list(
    pre_ktc_date = ymd("2025-08-24"),
    post_ktc_date = ymd("2026-01-06")
  )
) # push back as far as possible

# -----------------------------------------------------------------------
# 0. Build fpca_data (Stage 1 population-shape training data)
#
# General rule: any player with a real observation (KTC entering value or
# measured VA) in some season is assumed present in every season after
# that one, through the most recent season in the data -- using their real
# total_value_added where we have it, 0 where we don't.
# -----------------------------------------------------------------------

hktc_data <- map_dfr(
  ktc_begin_end_dates,
  ~ compile_training_data(
    ktc_list,
    player_info,
    pre_ktc_date = .x$pre_ktc_date,
    post_ktc_date = .x$post_ktc_date
  )
)

all_seasons <- sort(unique(c(season_value_added$season, hktc_data$season)))

# One row per (name, season, position) with a real observed value.
# When both sources report the same player-season, season_value_added
# wins (listed first; distinct() keeps the first occurrence per group).
real_obs <- bind_rows(
  season_value_added |>
    filter(!(position %in% c("K", "DST"))) |>
    transmute(name, season, position, total_value_added),
  hktc_data |>
    filter(!(position %in% c("K", "DST"))) |>
    transmute(name, season, position, total_value_added = tva_adj)
) |>
  distinct(name, season, position, .keep_all = TRUE) |>
  left_join(player_info, by = join_by(name, position))

# Catch player_info join failures (name mismatches) before they turn into
# silent NA ages downstream.
name_mismatches <- real_obs |>
  filter(is.na(birth_date)) |>
  distinct(name, position)
if (nrow(name_mismatches) > 0) {
  warning(sprintf(
    "%d player(s) failed to match player_info (NA birth_date) -- see `name_mismatches`. These will be dropped.",
    nrow(name_mismatches)
  ))
  print(name_mismatches)
}

fpca_data <- real_obs |>
  filter(!is.na(birth_date)) |>
  group_by(name, position) |>
  complete(season = seq(min(season), max(all_seasons))) |>
  fill(birth_date, .direction = "downup") |>
  mutate(
    total_value_added = replace_na(total_value_added, 0),
    age = as.numeric(ktc_begin_end_dates$year1$pre_ktc_date - birth_date) /
      365.25 +
      season -
      2024
  ) |>
  ungroup() |>
  transmute(
    subj = name,
    argvals = age,
    y = total_value_added,
    position,
    season
  ) |>
  arrange(position, subj, argvals)


# -----------------------------------------------------------------------
# 0b. Diagnostics -- run before fitting to catch issues early
# -----------------------------------------------------------------------

fpca_data |>
  group_by(position) |>
  summarize(
    n = n(),
    n_na_age = sum(is.na(argvals)),
    n_na_y = sum(is.na(y)),
    n_unique_age = n_distinct(argvals),
    n_players = n_distinct(subj)
  )

# Flags any player/position with more rows than unique ages -- a genuine
# duplicate-age conflict that survived the dedup above.
fpca_data |>
  group_by(subj, position) |>
  filter(n() != n_distinct(argvals)) |>
  summarize(n = n(), n_unique = n_distinct(argvals), .groups = "drop")

fpca_data |> group_by(subj) |> summarize(count = n()) |> filter(count == 1)
fpca_data |> count(position)

fpca_data |>
  group_by(position) |>
  summarize(min_age = min(argvals), max_age = max(argvals))

fpca_data |>
  ggplot(aes(argvals, y, group = subj)) +
  geom_line(alpha = 0.15) +
  geom_point(alpha = 0.3, size = 0.8) +
  facet_wrap(~position) +
  labs(x = "Age", y = "VA (tva_adj)", title = "Raw career points by position")


# -----------------------------------------------------------------------
# 1. STAGE 1 -- population mean/covariance via sparse FPCA, per position
# -----------------------------------------------------------------------

positions <- sort(unique(fpca_data$position))

fpca_by_position <- positions |>
  set_names() |>
  map(~ fit_fpca_position(filter(fpca_data, position == .x)))

walk(positions, function(p) {
  fit <- fpca_by_position[[p]]
  mu_df <- tibble(age = fit$age_grid, mu = fit$mu.new)
  print(
    fpca_data |>
      filter(position == p) |>
      ggplot(aes(argvals, y)) +
      geom_point(alpha = 0.2) +
      geom_line(
        data = mu_df,
        aes(age, mu),
        color = "firebrick",
        linewidth = 1
      ) +
      labs(title = paste("Population mean curve --", p), x = "Age", y = "VA")
  )
})

# Variance explained per retained component, per position
map(fpca_by_position, ~ .x$eigenvalues / sum(.x$eigenvalues))

raw_scores_by_position <- positions |>
  set_names() |>
  map(
    ~ extract_raw_scores(
      filter(fpca_data, position == .x),
      fpca_by_position[[.x]]
    )
  )


# -----------------------------------------------------------------------
# 2. STAGE 2 -- regress raw scores on KTC + age
# -----------------------------------------------------------------------

ktc_lookup <- hktc_data |>
  transmute(subj = name, season, ktc_in = historical_value)

score_models_by_position <- positions |>
  set_names() |>
  map(
    ~ fit_score_models(
      filter(fpca_data, position == .x),
      raw_scores_by_position[[.x]],
      fpca_by_position[[.x]],
      ktc_lookup
    )
  )

# How much data actually informed each position's Stage 2 fit (KTC-matched
# subset -- expect this to be smaller than Stage 1's per-position n).
map(score_models_by_position, "n_train")

# How much does KTC explain of each mode? (1 - tau2/lambda)
map2(score_models_by_position, fpca_by_position, function(sm, fit) {
  1 - sm$tau2_const / fit$eigenvalues
})

# Sanity check the heteroskedastic variance function before trusting it --
# with only ~n_train rows feeding a regression on squared residuals (an
# inherently noisy, heavy-tailed target), confirm tau2_fun produces a
# sensible, not wildly erratic, pattern across age/KTC. Look for: does
# variance decrease with age (matching "young players are riskier")? Is it
# reasonably smooth rather than jumping around?
walk(positions, function(p) {
  sm <- score_models_by_position[[p]]
  fit <- fpca_by_position[[p]]
  check_grid <- expand_grid(
    age = quantile(fit$age_grid, c(0.1, 0.5, 0.9)),
    ktc = c(2000, 5000, 8000)
  )
  print(p)
  print(
    check_grid |>
      mutate(tau2_xi1 = map2_dbl(ktc, age, ~ sm$tau2_fun(.x, .y)[1])) |>
      arrange(ktc, age)
  )
})


# -----------------------------------------------------------------------
# 3. Example usage
#
# project_career() now takes the fit/sm objects directly rather than a
# position string, so it works the same whether you're calling it here on
# the full-data fit or later on a bootstrap resample's fit/sm objects.
# -----------------------------------------------------------------------

proj_new <- project_career(
  fpca_by_position$WR,
  score_models_by_position$WR,
  ktc_in = 8600,
  age_in = 24.5
)

hist_example <- fpca_data |>
  filter(subj == "Ty Simpson") |>
  select(age = argvals, tva_adj = y)

proj_existing <- project_career(
  fpca_by_position$QB,
  score_models_by_position$QB,
  ktc_in = 3577,
  age_in = 23.6,
  history = hist_example
)

ggplot(proj_existing, aes(age, pred_va)) +
  geom_ribbon(aes(ymin = lower80, ymax = upper80), alpha = 0.2) +
  geom_line(linewidth = 1) +
  labs(title = "Projected career VA curve", x = "Age", y = "Predicted VA")

# hktc_data_list <- hktc_data %>%
#   group_by(position) %>%
#   reframe(position = list(tibble(name, historical_value, total_value_added, tva_adj, ktc_value, position, age))) %>%
#   deframe()

# Model Total Value Added for next season-------------------------------------------------------------------------

# I use a BART (Bayesian Additive Regression Tree) Model

# means and standard deviations that I used to scale the predictors
tva_scales <- hktc_data %>% compute_tva_scales()

# prep data
tva_data <- hktc_data %>% prep_data_tva(tva_scales)

if (train_models) {
  # run model ~ 2.5 minutes
  tic()
  # tva_fit <- fit_bart(tva_data$full_data)
  tva_fit <- fit_bart(tva_data$train_data)
  toc()

  saveRDS(bundle(tva_fit), file = here("Modeling/tva_fit.rds"))

  # compute accuracy(RMSE)
  model_accuracy(tva_fit, tva_data$test_data)

  # graph residuals
  graph_residuals(tva_fit, tva_data$test_data)

  tva_samples <- generate_samples(tva_fit, tva_data$test_data)

  compute_coverage(tva_fit, tva_data$test_data, confidence = .95)
}


# Model KTC Value for next season -----------------------------------------

# means and standard deviations that I used to scale the predictors
ktc_scales <- hktc_data %>% compute_ktc_scales()

# prep data
ktc_data <- hktc_data %>% prep_data_ktc(ktc_scales)

if (train_models) {
  # run model ~2.5 minutes
  tic()
  # ktc_fit <- fit_bart(ktc_data$train_data)
  ktc_fit <- fit_bart(ktc_data$full_data)
  toc()

  saveRDS(bundle(ktc_fit), file = here("Modeling/ktc_fit.rds"))

  # compute accuracy(RMSE)
  model_accuracy(ktc_fit, ktc_data$test_data)

  # graph residuals
  graph_residuals(ktc_fit, ktc_data$test_data)

  ktc_samples <- generate_samples(ktc_fit, ktc_data$test_data)

  compute_coverage(ktc_fit, ktc_data$test_data, confidence = .95)
}

# Model Residuals --------------------------------------------------------
if (train_models) {
  tva_resid_fit <- model_residuals(tva_fit, tva_data$full_data)
  saveRDS(tva_resid_fit, file = here("Modeling/tva_resid_fit.rds"))

  ktc_resid_fit <- model_residuals(ktc_fit, ktc_data$full_data)
  saveRDS(ktc_resid_fit, file = here("Modeling/ktc_resid_fit.rds"))
}


# Load Models -------------------------------------------------------------

tva_fit <- readRDS(here("Modeling/tva_fit.rds")) %>% unbundle()
tva_resid_fit <- readRDS(here("Modeling/tva_resid_fit.rds"))

ktc_fit <- readRDS(here("Modeling/ktc_fit.rds")) %>% unbundle()
ktc_resid_fit <- readRDS(here("Modeling/ktc_resid_fit.rds"))

# Run Player Intervals ----------------------------------------------------

# compute future value over time
last_date_fvt <- read_csv(
  here("Data/last_date_fvt.csv"),
  show_col_types = FALSE
) %>%
  pull(value)
keep_trade_cut <- select_ktc_list(ktc_list, last_date_fvt)[[1]]

# origin data set, set at beginning of last year
sim_df <- select_ktc_list(ktc_list, last_date_fvt)[[1]] %>%
  compile_data_set(
    future_value_names,
    today(),
    max(season_dates$season_start),
    max(season_dates$season_end)
  )

message(
  "There are ",
  sim_df |> filter(is.na(birth_date)) |> nrow(),
  " missing players"
)

sim_df <- sim_df |>
  filter(!is.na(birth_date))

diff <- (sim_df |> nrow()) - (sim_df |> drop_na() |> nrow())
if (diff != 0) {
  message("Warning: ", diff, " missing players")
}

# ~3 mins
player_simulations <- next_years(
  origin_data = sim_df,
  n_years = 10,
  tva_scales = tva_scales,
  ktc_scales = ktc_scales,
  tva_fit = tva_fit,
  ktc_fit = ktc_fit,
  tva_resid_fit = tva_resid_fit,
  ktc_resid_fit = ktc_resid_fit
)

save(player_simulations, file = here("Modeling/player_simulations.RData"))

# Future Value over Time --------------------------------------------------
# goal is to only run one at a time, while keeping the previous models

reduced_ktc_list <- select_ktc_list(ktc_list, last_date_fvt)
# reduced_ktc_list <- ktc_list # if running all again

future_value_time <- read_csv(
  here("Shiny/Saved Files/future_value_time.csv"),
  show_col_types = FALSE
) %>%
  filter(date != today())

message("begin mapping future value over time...")

# can't figure out how to parallelize this. Takes ~ 4 minutes for one run
future_value_time <- map_future_value_time(
  future_value_names,
  reduced_ktc_list,
  tva_scales,
  ktc_scales,
  tva_fit,
  ktc_fit,
  tva_resid_fit,
  ktc_resid_fit,
  season_dates
) %>%
  bind_rows(future_value_time)

write_csv(future_value_time, here("Shiny/Saved Files/future_value_time.csv"))
# make list of the dates already computed, so I don't have to compute them again
max(future_value_time$date) %>%
  as_tibble() %>%
  write_csv(here("Data/last_date_fvt.csv"))

# ensure future value is the same as most recent future_value_over_time

player_total_value <- future_value_time %>%
  filter(date == max(date)) %>%
  full_join(
    season_value_added %>%
      select(name, season, total_value_added) %>%
      pivot_wider(
        names_from = season,
        values_from = total_value_added,
        names_prefix = "sva_"
      ),
    by = join_by(name)
  ) %>%
  select(name, contains("sva"), future_value) %>%
  left_join(player_info, by = join_by(name)) %>%
  left_join(keep_trade_cut, by = join_by(name)) %>%
  mutate(
    across(contains("sva"), ~ replace_na(., 0)),
    ktc_value = case_when(
      position %in% c("K", "DST") ~ 0,
      .default = ktc_value
    ),
    future_value = case_when(
      position %in% c("K", "DST") ~ 0,
      is.na(future_value) ~ 0,
      .default = future_value
    )
  ) %>%
  arrange(desc(future_value)) |>
  select(
    name,
    player_id,
    birth_date,
    position,
    ktc_value,
    contains("sva"),
    future_value
  )

write_csv(player_total_value, here("Data/player_total_value.csv"))

# Demonstrate Model Fit ----------------------------------------------------
if (demonstrate_fit) {
  # tva
  toy_tva_data <- tibble(
    position = c(
      rep("QB", 1200),
      rep("RB", 1200),
      rep("WR", 1200),
      rep("TE", 1200)
    ),
    age = rep(
      c(
        rep(22, 100),
        rep(23, 100),
        rep(24, 100),
        rep(25, 100),
        rep(26, 100),
        rep(27, 100),
        rep(28, 100),
        rep(29, 100),
        rep(30, 100),
        rep(31, 100),
        rep(32, 100),
        rep(33, 100)
      ),
      4
    ),
    historical_value = rep(seq(from = 100, to = 10000, length.out = 100), 48),
    tva_adj = NA
  )

  prep_toy_tva_data <- toy_tva_data |> prep_data_tva(tva_scales)

  tic()
  toy_tva_quantiles <- generate_samples(tva_fit, prep_toy_tva_data$full_data) |>
    compute_quantiles(tva_resid_fit, prep_toy_tva_data$full_data)
  toc()

  toy_tva_plot_data <- toy_tva_data |>
    bind_cols(as_tibble(t(toy_tva_quantiles))) |>
    mutate(age = factor(age)) |>
    rename(
      median_tva = "V20",
      q05 = "V2",
      q10 = "V4",
      q90 = "V36",
      q95 = "V38"
    ) |>
    select(historical_value, age, position, median_tva, q05, q10, q90, q95)

  toy_tva_plot_data |>
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

  # ktc
  toy_ktc_data <- tibble(
    position = c(
      rep("QB", 24000),
      rep("RB", 24000),
      rep("WR", 24000),
      rep("TE", 24000)
    ),
    age = rep(
      c(
        rep(23, 2000),
        rep(24, 2000),
        rep(25, 2000),
        rep(26, 2000),
        rep(27, 2000),
        rep(28, 2000),
        rep(29, 2000),
        rep(30, 2000),
        rep(31, 2000),
        rep(32, 2000),
        rep(33, 2000),
        rep(34, 2000)
      ),
      4
    ),
    historical_value = rep(seq(from = 100, to = 10000, length.out = 100), 960),
    tva_adj = rep(
      c(
        rep(-28, 100),
        rep(-16, 100),
        rep(-4, 100),
        rep(8, 100),
        rep(20, 100),
        rep(32, 100),
        rep(44, 100),
        rep(56, 100),
        rep(68, 100),
        rep(80, 100),
        rep(92, 100),
        rep(104, 100),
        rep(116, 100),
        rep(128, 100),
        rep(140, 100),
        rep(152, 100),
        rep(164, 100),
        rep(176, 100),
        rep(188, 100),
        rep(200, 100)
      ),
      48
    ),
    ktc_value = 0
  )

  prep_toy_ktc_data <- prep_data_ktc(toy_ktc_data, ktc_scales)

  tic()
  toy_ktc_quantiles <- generate_samples(ktc_fit, prep_toy_ktc_data$full_data) |>
    compute_quantiles(ktc_resid_fit, prep_toy_ktc_data$full_data)
  toc()

  toy_ktc_plot_data <- toy_ktc_data |>
    bind_cols(as_tibble(t(toy_ktc_quantiles))) |>
    mutate(age = factor(age)) |>
    rename(
      median_ktc = "V20",
      q05 = "V2",
      q10 = "V4",
      q90 = "V36",
      q95 = "V38"
    ) |>
    select(
      historical_value,
      age,
      position,
      tva_adj,
      median_ktc,
      q05,
      q10,
      q90,
      q95
    ) |>
    filter(historical_value == 5000)

  toy_ktc_plot_data |>
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

  write_csv(toy_tva_plot_data, here("Data/toy_tva_plot_data.csv"))
  write_csv(toy_ktc_plot_data, here("Data/toy_ktc_plot_data.csv"))
}
