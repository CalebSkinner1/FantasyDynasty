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

# build dataset ----------------------------------------------------------

hktc_data <- map_dfr(
  ktc_begin_end_dates,
  ~ compile_training_data(
    ktc_list,
    player_info,
    pre_ktc_date = .x$pre_ktc_date,
    post_ktc_date = .x$post_ktc_date
  )
)

players_2024 <- season_value_added |>
  filter(season == 2024, !(position %in% c("K", "DST"))) |>
  bind_rows(hktc_data |> filter(season == 2024)) |>
  distinct(name) |>
  pull(name)
players_2025 <- season_value_added |>
  filter(season == 2025, !(position %in% c("K", "DST"))) |>
  bind_rows(hktc_data |> filter(season == 2025)) |>
  distinct(name) |>
  pull(name)
only_2025 <- setdiff(players_2025, players_2024)

empty_2024 <- hktc_data |>
  filter(
    season == 2024,
    !(name %in% pull(season_value_added |> filter(season == 2024), name))
  ) |>
  rename(total_value_added = tva_adj) |>
  select(name, season, age, position, total_value_added)

new_rows <- season_value_added |>
  select(name, season, position, season, total_value_added) |>
  left_join(player_info, by = join_by(name, position)) |>
  mutate(
    age = as.numeric(ktc_begin_end_dates$year1$pre_ktc_date - birth_date) /
      365.25 +
      season -
      2024
  ) |>
  select(name, season, age, position, total_value_added) |>
  bind_rows(empty_2024) |>
  filter(
    season == 2024,
    !(name %in% players_2025),
    !position %in% c("K", "DST")
  ) |>
  mutate(
    season = season + 1,
    age = age + 1,
    position = position,
    total_value_added = 0
  )

fpca_data <- season_value_added |>
  left_join(player_info, by = join_by(name, position)) |>
  mutate(
    age = as.numeric(ktc_begin_end_dates$year1$pre_ktc_date - birth_date) /
      365.25 +
      season -
      2024
  ) |>
  select(name, season, age, position, total_value_added) |>
  bind_rows(
    hktc_data |>
      filter(season == 2025) |>
      rename(total_value_added = tva_adj) |>
      select(name, season, age, position, total_value_added)
  ) |>
  filter(!position %in% c("K", "DST")) |>
  bind_rows(empty_2024) |>
  bind_rows(new_rows) |>
  distinct(name, season, .keep_all = TRUE) |>
  transmute(
    subj = name,
    argvals = age,
    y = total_value_added,
    position,
    season
  ) |>
  arrange(position, subj, argvals)

fpca_data |> group_by(subj) |> summarize(count = n()) |> filter(count == 1)

fpca_data |> count(position)

fpca_data |>
  ggplot(aes(argvals, y, group = subj)) +
  geom_line(alpha = 0.15) +
  geom_point(alpha = 0.3, size = 0.8) +
  facet_wrap(~position) +
  labs(x = "Age", y = "VA (tva_adj)", title = "Raw career points by position")

# Stage 1 ----------------------------------------------------------------
age_grid <- seq(20, 43, length.out = 100)

fit_fpca_position <- function(pos_data, pve = 0.90, knots = 6, K_cap = 4) {
  d <- pos_data |>
    transmute(argvals, subj = factor(subj), y) |>
    as.data.frame()

  fit <- face.sparse(
    d,
    argvals.new = age_grid,
    knots = knots,
    pve = pve
  )

  K <- min(ncol(fit$eigenfunctions), K_cap)
  fit$eigenfunctions <- fit$eigenfunctions[, 1:K, drop = FALSE]
  fit$eigenvalues <- fit$eigenvalues[1:K]
  fit$K <- K
  fit
}

positions <- sort(unique(fpca_data$position))

fpca_by_position <- positions |>
  set_names() |>
  map(~ fit_fpca_position(filter(fpca_data, position == .x)))

walk(positions, function(p) {
  fit <- fpca_by_position[[p]]
  mu_df <- tibble(age = age_grid, mu = fit$mu.new)
  print(
    fpca_data %>%
      filter(position == p) %>%
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

map(fpca_by_position, ~ .x$eigenvalues / sum(.x$eigenvalues))

make_mu_fun <- function(fit) {
  function(a) approx(age_grid, fit$mu.new, xout = a, rule = 2)$y
}

make_phi_fun <- function(fit) {
  function(a) {
    apply(fit$eigenfunctions, 2, function(col) {
      approx(age_grid, col, xout = a, rule = 2)$y
    })
  }
}

blup_scores <- function(
  y_obs,
  ages_obs,
  mu_fun,
  phi_fun,
  prior_mean,
  prior_var,
  sigma2
) {
  K <- length(prior_mean)
  Tinv <- diag(1 / prior_var, K)

  if (length(y_obs) == 0) {
    return(list(mean = prior_mean, cov = diag(prior_var, K)))
  }

  Phi_i <- matrix(sapply(ages_obs, phi_fun), ncol = K, byrow = TRUE)
  mu_i <- mu_fun(ages_obs)
  resid <- y_obs - mu_i

  Sigma_inv <- Tinv + (1 / sigma2) * crossprod(Phi_i)
  Sigma <- solve(Sigma_inv)
  post_mean <- Sigma %*%
    (Tinv %*% prior_mean + (1 / sigma2) * t(Phi_i) %*% resid)

  list(mean = as.vector(post_mean), cov = Sigma)
}

extract_raw_scores <- function(pos_data, fit) {
  mu_fun <- make_mu_fun(fit)
  phi_fun <- make_phi_fun(fit)
  sigma2 <- fit$sigma2

  pos_data %>%
    group_by(subj) %>%
    group_modify(
      ~ {
        res <- blup_scores(
          y_obs = .x$y,
          ages_obs = .x$argvals,
          mu_fun = mu_fun,
          phi_fun = phi_fun,
          prior_mean = rep(0, fit$K),
          prior_var = fit$eigenvalues,
          sigma2 = sigma2
        )
        as_tibble(matrix(res$mean, nrow = 1)) %>%
          set_names(paste0("xi", seq_len(fit$K)))
      }
    ) %>%
    ungroup()
}

raw_scores_by_position <- positions |>
  set_names() |>
  map(
    ~ extract_raw_scores(
      filter(fpca_data, position == .x),
      fpca_by_position[[.x]]
    )
  )

# Stage 2 ----------------------------------------------------------------

ktc_lookup <- hktc_data |>
  transmute(subj = name, season, ktc_in = historical_value)

fit_score_models <- function(pos_data, raw_scores, fit, ktc_lookup) {
  d <- pos_data |>
    distinct(subj, season, argvals) |>
    inner_join(ktc_lookup, by = c("subj", "season")) |>
    left_join(raw_scores, by = "subj")

  score_models <- map(seq_len(fit$K), function(k) {
    lm(as.formula(paste0("xi", k, " ~ ktc_in + argvals")), data = d)
  })
  names(score_models) <- paste0("xi", seq_len(fit$K))

  tau2 <- map_dbl(seq_len(fit$K), function(k) {
    pmin(sigma(score_models[[k]])^2, fit$eigenvalues[k])
  })

  list(models = score_models, tau2 = tau2, n_train = nrow(d))
}

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

# How much does KTC explain of each mode? (1 - tau2/lambda)
map(score_models_by_position, "n_train")

# How much does KTC explain of each mode? (1 - tau2/lambda)
map2(score_models_by_position, fpca_by_position, function(sm, fit) {
  1 - sm$tau2 / fit$eigenvalues
})

project_career <- function(
  position,
  ktc_in,
  age_in,
  history = NULL,
  ages_out = seq(21, 36, by = 1)
) {
  fit <- fpca_by_position[[position]]
  sm <- score_models_by_position[[position]]
  mu_fun <- make_mu_fun(fit)
  phi_fun <- make_phi_fun(fit)

  g_mean <- map_dbl(
    sm$models,
    ~ predict(.x, newdata = tibble(ktc_in = ktc_in, argvals = age_in))
  )

  if (is.null(history) || nrow(history) == 0) {
    y_obs <- numeric(0)
    ages_obs <- numeric(0)
  } else {
    y_obs <- history$tva_adj
    ages_obs <- history$age
  }

  post <- blup_scores(
    y_obs = y_obs,
    ages_obs = ages_obs,
    mu_fun = mu_fun,
    phi_fun = phi_fun,
    prior_mean = g_mean,
    prior_var = sm$tau2,
    sigma2 = fit$sigma2
  )

  mu_out <- mu_fun(ages_out)
  Phi_out <- matrix(sapply(ages_out, phi_fun), ncol = fit$K, byrow = TRUE)
  pred_va <- as.vector(mu_out + Phi_out %*% post$mean)
  var_va <- diag(Phi_out %*% post$cov %*% t(Phi_out))

  tibble(
    age = ages_out,
    pred_va = pred_va,
    se = sqrt(pmax(var_va, 0)),
    lower80 = pred_va - 1.28 * se,
    upper80 = pred_va + 1.28 * se
  )
}

proj_new <- project_career("WR", ktc_in = 8600, age_in = 24.5)

hist_example <- fpca_data |>
  filter(subj == "Trey Benson") |>
  select(age = argvals, tva_adj = y)

proj_existing <- project_career(
  "RB",
  ktc_in = 2181,
  age_in = 24,
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
