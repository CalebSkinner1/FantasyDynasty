# Player Total Value Functions
# this file stores all the functions needed for the Player Total Value Page

suppressPackageStartupMessages({
  library("tidyverse")
  theme_set(theme_minimal())
  library("tidymodels")
  library("parsnip")
  library("dbarts")
  library("vip")
  library("here")
  library("tictoc")
  library("furrr")
})

# quantiles carried through the simulation (2.5%, 5%, ..., 97.5%)
quantile_probs <- seq(.025, .975, by = .025)

# Prep Data ---------------------------------------------------------------

compile_training_data <- function(
  ktc_list,
  player_info,
  season_value_added,
  pre_ktc_date,
  post_ktc_date
) {
  this_season <- year(pre_ktc_date)

  # compute ktc before the start of the season
  pre_ktc <- ktc_list[[str_c(
    "ktc_value",
    str_pad(month(pre_ktc_date), width = 2, side = "left", pad = 0),
    str_pad(day(pre_ktc_date), width = 2, side = "left", pad = 0),
    str_sub(year(pre_ktc_date), 3, 4),
    ".csv"
  )]] |>
    filter(
      !str_detect(name, "Early"),
      !str_detect(name, "Mid"),
      !str_detect(name, "Late")
    ) |>
    name_correction()

  post_ktc <- ktc_list[[str_c(
    "ktc_value",
    str_pad(month(post_ktc_date), width = 2, side = "left", pad = 0),
    str_pad(day(post_ktc_date), width = 2, side = "left", pad = 0),
    str_sub(year(post_ktc_date), 3, 4),
    ".csv"
  )]] |>
    filter(
      !str_detect(name, "Early"),
      !str_detect(name, "Mid"),
      !str_detect(name, "Late")
    ) |>
    name_correction()

  pre_ktc <- pre_ktc |>
    select(player_id, name, ktc_value = any_of(c("ktc_value", "value"))) |>
    filter(!is.na(player_id))
  post_ktc <- post_ktc |>
    select(player_id, ktc_value = any_of(c("ktc_value", "value"))) |>
    filter(!is.na(player_id))

  pre_ktc |>
    rename("historical_value" = "ktc_value") |>
    left_join(
      season_value_added |> filter(season == this_season) |> select(-name),
      by = join_by(player_id)
    ) |>
    select(-total_points) |>
    mutate(
      total_value_added = replace_na(total_value_added, 0)
    ) |>
    rename(tva_adj = total_value_added) |>
    left_join(post_ktc, by = join_by(player_id)) |>
    select(-position) |>
    left_join(player_info |> select(-name), by = join_by(player_id)) |>
    select(-years_exp) |>
    mutate(
      age = as.numeric(pre_ktc_date - birth_date) / 365.25
    )
}

# Season Context ------------------------------------------------------------
# Future value blends two simulations so it never jumps at a season boundary:
#   current path: starts at the current season s
#   next path:    starts at season s + 1
# with p = share of season s elapsed (p = 1 throughout the offseason),
#   FV = (1 - p) * sum_{j=0..7} d^(j - p) * current_{s+j}
#        + p     * sum_{j=1..8} d^(j - p) * next_{s+j}
# Each season uses a player's age at that season's start, so nobody ages within a
# year. At p = 1 the value equals the offseason value, which has no time terms, and
# the next season's current path at p = 0 is exactly this season's next path.

season_context <- function(date, season_dates) {
  sd <- season_dates |>
    mutate(season = year(season_start)) |>
    arrange(season_start)

  # seasons not in season_dates yet (e.g. next season before its league is added)
  # are placed a whole number of years after the latest known season, so a new
  # season still starts on time
  missing_seasons <- setdiff(seq(min(sd$season), year(date) + 1), sd$season)
  if (length(missing_seasons) > 0) {
    latest <- sd |> slice_max(season, n = 1, with_ties = FALSE)
    sd <- bind_rows(
      sd,
      tibble(
        season = missing_seasons,
        season_start = latest$season_start + years(missing_seasons - latest$season),
        season_end = latest$season_end + years(missing_seasons - latest$season)
      )
    ) |>
      arrange(season_start)
  }

  in_season <- sd |> filter(season_start <= date, date <= season_end)

  if (nrow(in_season) > 0) {
    season <- in_season$season[[1]]
    p <- as.numeric(date - in_season$season_start[[1]]) /
      as.numeric(in_season$season_end[[1]] - in_season$season_start[[1]])
  } else {
    # offseason (or preseason): the last finished season is fully elapsed
    finished <- sd |> filter(season_end < date)
    season <- if (nrow(finished) > 0) max(finished$season) else min(sd$season) - 1
    p <- 1
  }

  # start of a season (beyond the table, a whole number of years after the closest)
  season_start_of <- function(k) {
    known <- sd$season_start[sd$season == k]
    if (length(known) > 0) {
      return(known[[1]])
    }
    ref <- sd[which.min(abs(sd$season - k)), ]
    ref$season_start + years(k - ref$season)
  }

  list(
    season = season,
    p = min(max(p, 0), 1),
    current_start = season_start_of(season),
    next_start = season_start_of(season + 1)
  )
}

# simulation origin for a path whose first simulated season is first_season. Ages
# are as of that season's start; next_year() adds one year to age and season, so
# the origin sits one season earlier.
origin_data <- function(
  future_value_names,
  keep_trade_cut,
  first_season,
  first_season_start
) {
  future_value_names |>
    left_join(
      keep_trade_cut |>
        select(player_id, ktc_value) |>
        distinct(player_id, .keep_all = TRUE),
      by = join_by(player_id)
    ) |>
    mutate(
      age = time_length(
        lubridate::interval(birth_date, first_season_start),
        unit = "years"
      ) -
        1,
      season = first_season - 1,
      ktc_value = replace_na(ktc_value, 0)
    ) |>
    select(
      name,
      player_id,
      position,
      birth_date,
      age,
      ktc_value,
      season,
      years_exp
    )
}

# simulation input for one date: players who were in the league by then
prepare_sim_data <- function(
  future_value_names,
  keep_trade_cut,
  date,
  first_season,
  first_season_start
) {
  # this is my arbitrary cutoff to include rookies
  league_year <- function(d) year(d) - (month(d) < 3)
  cutoff_years <- league_year(today()) - league_year(as_date(date))

  origin_data(
    keep_trade_cut = keep_trade_cut,
    future_value_names = future_value_names,
    first_season = first_season,
    first_season_start = first_season_start
  ) |>
    filter(
      (years_exp - cutoff_years) >= 0, # remove players that shouldn't appear yet
      !is.na(birth_date)
    )
}

interaction_terms_tva <- function(data) {
  data |>
    select(historical_value, age) |>
    mutate(
      x1_2 = historical_value^2,
      x2_2 = age^2,
      x1_x2 = historical_value * age
    )
}

# identify the appropriate values to scale
compute_tva_scales <- function(data) {
  summaries <- data |> interaction_terms_tva()

  means <- colMeans(summaries)

  sds <- sapply(summaries, sd)

  list("means" = means, "sds" = sds)
}

# center and scale predictors with stored means and standard deviations
scale_predictors <- function(df, scales) {
  m <- as.matrix(df)
  m <- sweep(m, 2, scales$means[colnames(m)], "-")
  m <- sweep(m, 2, scales$sds[colnames(m)], "/")
  as_tibble(m)
}

# prep data with transformations and interactions (returns the full scaled data)
prep_data_tva <- function(data, scales) {
  data |>
    interaction_terms_tva() |>
    scale_predictors(scales) |>
    mutate(position = data$position, Y = data$tva_adj)
}

interaction_terms_ktc <- function(data) {
  data |>
    select(historical_value, age, tva_adj) |>
    mutate(
      x1_2 = historical_value^2,
      x2_2 = age^2,
      x1_x2 = historical_value * age,
      x2_x3 = age * tva_adj
    )
}

# identify the appropriate values to scale
compute_ktc_scales <- function(data) {
  summaries <- data |> interaction_terms_ktc()

  means <- colMeans(summaries)

  sds <- sapply(summaries, sd)

  list("means" = means, "sds" = sds)
}

# prep data with transformations and interactions (returns the full scaled data)
prep_data_ktc <- function(data, scales) {
  min_ktc <- min(data$ktc_value, na.rm = TRUE)

  ktc <- if_else(
    is.na(data$ktc_value),
    runif(1, min = 0, max = min_ktc),
    data$ktc_value
  )

  data |>
    interaction_terms_ktc() |>
    scale_predictors(scales) |>
    mutate(position = data$position, Y = ktc)
}

# BART --------------------------------------------------------------------
# Chipman, George, McCulloch (2005)

# posterior draws stored by the dbarts engine. Only the posterior mean is used
# downstream, so the final model keeps 200 draws thinned from 1,000 iterations
# (5x less prediction work than storing all 1,000), and tuning keeps 200 unthinned.
final_engine <- list(ndpost = 200, keepevery = 5)
tune_engine <- list(ndpost = 200, keepevery = 1)

bart_workflow <- function(
  data,
  engine,
  seed,
  trees = tune(),
  prior_terminal_node_coef = tune(),
  prior_terminal_node_expo = tune()
) {
  spec <- parsnip::bart(
    trees = !!trees,
    prior_terminal_node_coef = !!prior_terminal_node_coef,
    prior_terminal_node_expo = !!prior_terminal_node_expo
  ) |>
    set_engine(
      "dbarts",
      ndpost = !!engine$ndpost,
      keepevery = !!engine$keepevery,
      seed = !!seed
    ) |>
    set_mode("regression")

  workflow() |>
    add_model(spec) |>
    add_recipe(recipe(Y ~ ., data = data))
}

# run code in parallel with future (tune >= 1.2 and finetune use the active plan)
with_parallel <- function(code) {
  if (packageVersion("tune") < "1.2.0") {
    message("tune < 1.2 ignores future::plan(), so this runs sequentially")
  }
  old_plan <- future::plan(
    future::multisession,
    workers = max(1, parallel::detectCores() - 1)
  )
  on.exit(future::plan(old_plan), add = TRUE)
  code
}

# tune hyperparameters with 10-fold cross validation, racing out poor candidates
tune_bart <- function(data, seed, folds = 10, grid = 20) {
  if (!requireNamespace("finetune", quietly = TRUE)) {
    stop("install.packages(\"finetune\") to tune the models")
  }

  wf <- bart_workflow(data, tune_engine, seed)

  params <- wf |>
    extract_parameter_set_dials() |>
    update(
      prior_terminal_node_coef = prior_terminal_node_coef(range = c(.4, .9)),
      prior_terminal_node_expo = prior_terminal_node_expo(range = c(1, 3))
    ) |>
    finalize(data)

  set.seed(seed)
  df_folds <- vfold_cv(data, v = folds)

  tuned <- with_parallel(
    finetune::tune_race_anova(
      wf,
      resamples = df_folds,
      grid = grid,
      param_info = params,
      metrics = metric_set(rmse)
    )
  )

  select_best(tuned, metric = "rmse") |>
    select(-.config)
}

# saved hyperparameters (written whenever the models are tuned)
params_path <- function(model_name) {
  here("Modeling", str_c(model_name, "_params.rds"))
}

save_params <- function(params, model_name) {
  saveRDS(params, params_path(model_name))
  params
}

# saved hyperparameters, or the ones stored in the current model fit
load_params <- function(model_name) {
  if (file.exists(params_path(model_name))) {
    return(readRDS(params_path(model_name)))
  }

  spec_args <- readRDS(here("Modeling", str_c(model_name, "_fit.rds"))) |>
    bundle::unbundle() |>
    extract_spec_parsnip() |>
    pluck("args")

  tibble(
    trees = rlang::eval_tidy(spec_args$trees),
    prior_terminal_node_coef = rlang::eval_tidy(spec_args$prior_terminal_node_coef),
    prior_terminal_node_expo = rlang::eval_tidy(spec_args$prior_terminal_node_expo)
  ) |>
    save_params(model_name)
}

# fit the final model on all of the data
fit_bart <- function(data, params, seed) {
  bart_workflow(data, final_engine, seed) |>
    finalize_workflow(params) |>
    fit(data = data)
}

# out-of-fold posterior means (10-fold cross validation with the final settings)
oof_predictions <- function(data, params, seed, folds = 10) {
  wf <- bart_workflow(data, final_engine, seed) |>
    finalize_workflow(params)

  set.seed(seed)
  resamples <- with_parallel(
    fit_resamples(
      wf,
      vfold_cv(data, v = folds),
      metrics = metric_set(rmse),
      control = control_resamples(save_pred = TRUE)
    )
  )

  collect_predictions(resamples) |>
    arrange(.row) |>
    pull(.pred)
}

# draw posterior samples from model fit (draws x rows)
generate_samples <- function(fit, data) {
  model <- extract_fit_engine(fit)

  # Generate posterior predictive samples
  predict(model, newdata = data)
}

posterior_mean <- function(fit, data) {
  colMeans(generate_samples(fit, data))
}

# model the size of the out-of-fold residuals: log E|residual| is smooth in each
# predictor, plus a position effect
model_residuals <- function(data, oof_pred) {
  new_data <- data |>
    mutate(abs_residuals = abs(Y - oof_pred)) |>
    select(-Y)

  library("mgcv")
  smooth_vars <- intersect(c("historical_value", "age", "tva_adj"), names(new_data))
  formula_text <- str_c(
    "abs_residuals ~ ",
    str_c("s(", smooth_vars, ")", collapse = " + "),
    " + position"
  )

  gam_model <- gam(
    as.formula(formula_text),
    data = new_data,
    family = gaussian(link = "log"),
    keep_data = FALSE
  )

  gam_coef <- coef(gam_model)

  gam_model$model <- NULL
  gam_model$y <- NULL
  gam_model$residuals <- NULL

  list(
    model = gam_model,
    coef = gam_coef
  )
}

# out-of-fold accuracy and interval coverage, printed so they show when sourced
report_fit <- function(model_name, data, oof_pred, resid_fit) {
  resid <- data$Y - oof_pred
  coverage <- compute_coverage(oof_pred, resid_fit, data, confidence = c(.8, .95))

  message(sprintf(
    "%s model (out of fold): RMSE %.2f | 80%% interval covers %.0f%% | 95%% interval covers %.0f%%",
    model_name,
    sqrt(mean(resid^2)),
    100 * coverage[[1]],
    100 * coverage[[2]]
  ))

  print(
    ggplot(tibble(resid = resid)) +
      geom_density(aes(resid)) +
      labs(title = str_c(model_name, " out-of-fold residuals"), x = "", y = "")
  )
}

# share of outcomes inside the central predictive intervals
compute_coverage <- function(posterior_mean, resid_fit, data, confidence = .95) {
  quantiles <- compute_quantiles(NULL, resid_fit, data, posterior_mean = posterior_mean)

  map_dbl(confidence, \(level) {
    lower <- which.min(abs(quantile_probs - (1 - level) / 2))
    upper <- which.min(abs(quantile_probs - (1 + level) / 2))
    mean(data$Y >= quantiles[lower, ] & data$Y <= quantiles[upper, ])
  })
}

# Simulate Future Value ---------------------------------------------------------------

# this function updates the data so its ready for the next year
update_data_year <- function(data) {
  hv <- replace_na(data$ktc_value, 0)

  data |>
    mutate(
      age = age + 1,
      season = season + 1,
      historical_value = hv,
      tva_adj = 0,
      ktc_value = 0
    ) |>
    select(
      name,
      player_id, # carried through the yearly update now, was dropped before
      historical_value,
      season,
      position,
      birth_date,
      age,
      tva_adj,
      ktc_value
    )
}

# predictive quantiles (one row per quantile, one column per player): the
# posterior mean plus normal quantiles with the residual model's scale
compute_quantiles <- function(
  samples,
  resid_fit,
  data,
  posterior_mean = colMeans(samples)
) {
  # rebuild design matrix
  Xp <- mgcv::predict.gam(
    resid_fit$model,
    newdata = data |> select(-any_of("Y")),
    type = "lpmatrix"
  )

  eta <- Xp %*% resid_fit$coef #linear predictor
  # the residual model predicts E|residual|, which for normal errors is
  # sigma * sqrt(2 / pi), so rescale it to the standard deviation
  sigma_hat <- resid_fit$model$family$linkinv(eta) |>
    as.vector() *
    sqrt(base::pi / 2)

  outer(qnorm(quantile_probs), sigma_hat) +
    matrix(posterior_mean, nrow = length(quantile_probs), ncol = length(posterior_mean), byrow = TRUE)
}

# quantiles for every scenario, predicting only the players still active in it.
# Inactive players (out of KTC, too old, or at the value floor) contribute 0, which
# is what the bounds below would set them to anyway. All scenarios are predicted
# in one batch.
predict_scenarios <- function(data_list, active_list, prep, fit, resid_fit) {
  n_players <- nrow(data_list[[1]])
  out <- map(active_list, \(a) matrix(0, length(quantile_probs), n_players))

  stacked <- map2(data_list, active_list, \(d, a) d[a, , drop = FALSE]) |>
    bind_rows()

  if (nrow(stacked) == 0) {
    return(out)
  }

  prepped <- prep(stacked)
  quantiles <- compute_quantiles(
    NULL,
    resid_fit,
    prepped,
    posterior_mean = posterior_mean(fit, prepped)
  )

  scenario <- rep(seq_along(active_list), map_int(active_list, sum))
  for (k in seq_along(active_list)) {
    if (any(active_list[[k]])) {
      out[[k]][, active_list[[k]]] <- quantiles[, scenario == k, drop = FALSE]
    }
  }
  out
}

# pool every scenario's quantiles and take the overall quantiles
integrate_quantiles <- function(quantile_list) {
  matrix <- do.call(rbind, quantile_list)

  matrix[is.na(matrix)] <- 0

  if (requireNamespace("matrixStats", quietly = TRUE)) {
    t(matrixStats::colQuantiles(matrix, probs = quantile_probs, drop = FALSE))
  } else {
    apply(matrix, 2, quantile, probs = quantile_probs, na.rm = TRUE)
  }
}

# this function bounds value according to domain knowledge
bound_tva <- function(quantiles, data) {
  map(
    seq_len(nrow(quantiles)),
    ~ {
      # the origin is a single data set shared by every quantile
      d <- data[[if (length(data) == 1) 1 else .x]]

      tv <- case_when(
        d$historical_value == 0 ~ 0, # set to 0 if ktc is 0
        d$age > 43 ~ 0, # set to 0 if age is too large
        .default = pmax(quantiles[.x, ], -20)
      ) # and don't let quantile get below -20

      d |> mutate(tva_adj = tv)
    }
  )
}

# ktc_floor: about the lowest KTC value among ranked players. A projected value
# below it means the player has fallen out of the rankings, so he is out (0).
bound_ktc <- function(samples_list, tva_data_list, ktc_floor = 450) {
  imap(
    seq_along(tva_data_list),
    ~ {
      ktcv <- case_when(
        tva_data_list[[.x]]$historical_value == 0 ~ 0, #if out, then stay out
        tva_data_list[[.x]]$age > 43 ~ 0, # if over 43, be done
        tva_data_list[[.x]]$tva_adj == -20 ~ 0, # if tva_adj == - 20, then out
        samples_list[.x, ] < ktc_floor ~ 0, # fell out of the rankings
        samples_list[.x, ] > 9999 ~ 9999, # cap ktc at 9999
        .default = samples_list[.x, ]
      )

      tva_data_list[[.x]] |> mutate(ktc_value = ktcv)
    }
  )
}

next_year <- function(
  data_list,
  seasons_list,
  tva_scales,
  ktc_scales,
  tva_fit,
  ktc_fit,
  tva_resid_fit,
  ktc_resid_fit
) {
  if (is.data.frame(data_list)) {
    data_list <- list("origin" = data_list)
  }
  # update data (add year to age, shift historical value)
  updated_data <- map(data_list, update_data_year)

  # tva quantiles for each scenario, pooled, then bounded
  tva_active <- map(
    updated_data,
    \(d) coalesce(d$historical_value > 0 & d$age <= 43, FALSE)
  )

  tva_data_list <- predict_scenarios(
    updated_data,
    tva_active,
    \(d) prep_data_tva(d, tva_scales),
    tva_fit,
    tva_resid_fit
  ) |>
    integrate_quantiles() |> #combine quantiles from possible data sets into one
    bound_tva(updated_data) # add with updated data

  # ktc quantiles for each scenario, pooled, then bounded
  ktc_active <- map(
    tva_data_list,
    \(d) coalesce(d$historical_value > 0 & d$age <= 43 & d$tva_adj > -20, FALSE)
  )

  ktc_data_list <- predict_scenarios(
    tva_data_list,
    ktc_active,
    \(d) prep_data_ktc(d, ktc_scales),
    ktc_fit,
    ktc_resid_fit
  ) |>
    integrate_quantiles() |> #bind lists together and computes aggregate quantiles
    bound_ktc(tva_data_list)

  compile <- map(ktc_data_list, "tva_adj") |>
    set_names(paste0("proj_tva_", seq_along(ktc_data_list) * 2.5)) |>
    as_tibble() |>
    mutate(
      name = updated_data[[1]]$name,
      player_id = updated_data[[1]]$player_id # carried through, was absent before
    ) |>
    relocate(name, player_id)

  seasons_list <- c(
    seasons_list,
    setNames(list(compile), paste0(updated_data[[1]]$season[1]))
  )

  list("data" = ktc_data_list, "seasons_list" = seasons_list)
}

next_years <- function(
  origin_data,
  n_years,
  tva_scales,
  ktc_scales,
  tva_fit,
  ktc_fit,
  tva_resid_fit,
  ktc_resid_fit
) {
  updating_list <- list("data" = origin_data, "seasons_list" = list())

  for (i in 1:n_years) {
    updating_list <- next_year(
      data_list = updating_list$data,
      seasons_list = updating_list$seasons_list,
      tva_scales,
      ktc_scales,
      tva_fit,
      ktc_fit,
      tva_resid_fit,
      ktc_resid_fit
    )
  }

  updating_list$seasons_list
}

# discounted sum of the median projections over the first `years` seasons. Season k
# of the path is weighted discount_rate^(first_exponent + k - 1).
compute_future_value <- function(
  seasons_list,
  years = 8,
  discount_rate = .95,
  first_exponent = 0
) {
  future_value <- imap(
    1:years,
    ~ {
      pmax(seasons_list[[.x]]$proj_tva_50, 0) *
        discount_rate^(first_exponent + .x - 1)
    }
  ) |>
    as.data.frame() |>
    rowSums()

  tibble(
    name = seasons_list[[1]]$name,
    player_id = seasons_list[[1]]$player_id, # carried through, was absent before
    future_value = future_value
  ) |>
    arrange(desc(future_value))
}

# blended future value (see Season Context above)
blend_future_value <- function(
  sims_current,
  sims_next,
  p,
  years = 8,
  discount_rate = .95
) {
  parts <- list()
  if (!is.null(sims_current)) {
    parts$current <- compute_future_value(
      sims_current,
      years,
      discount_rate,
      first_exponent = -p
    ) |>
      mutate(future_value = (1 - p) * future_value)
  }
  if (!is.null(sims_next)) {
    parts$next_season <- compute_future_value(
      sims_next,
      years,
      discount_rate,
      first_exponent = 1 - p
    ) |>
      mutate(future_value = p * future_value)
  }

  bind_rows(parts) |>
    summarize(
      name = first(name),
      future_value = sum(future_value),
      .by = player_id
    ) |>
    relocate(name) |>
    arrange(desc(future_value))
}

# Future Value over Time --------------------------------------------------

# simulate both paths needed on a date and blend them. Returns the future value for
# every player in future_value_names, the simulations, and the season context.
future_value_at_date <- function(
  future_value_names,
  keep_trade_cut,
  date,
  season_dates,
  models,
  n_years = 8
) {
  context <- season_context(date, season_dates)

  simulate_from <- function(first_season, first_season_start) {
    prepare_sim_data(
      future_value_names,
      keep_trade_cut,
      date,
      first_season,
      first_season_start
    ) |>
      next_years(
        n_years = n_years,
        tva_scales = models$tva_scales,
        ktc_scales = models$ktc_scales,
        tva_fit = models$tva_fit,
        ktc_fit = models$ktc_fit,
        tva_resid_fit = models$tva_resid_fit,
        ktc_resid_fit = models$ktc_resid_fit
      )
  }

  sims_current <- if (context$p < 1) {
    simulate_from(context$season, context$current_start)
  }
  sims_next <- if (context$p > 0) {
    simulate_from(context$season + 1, context$next_start)
  }

  future_value <- future_value_names |>
    select(name, player_id) |>
    left_join(
      blend_future_value(sims_current, sims_next, context$p) |> select(-name),
      by = join_by(player_id)
    ) |>
    mutate(date = date)

  list(
    future_value = future_value,
    sims_current = sims_current,
    sims_next = sims_next,
    context = context
  )
}
