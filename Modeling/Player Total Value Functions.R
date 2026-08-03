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


# Prep Data ---------------------------------------------------------------
compile_training_data <- function(
  ktc_list,
  player_info,
  weekly_value_added,
  pre_ktc_date,
  post_ktc_date,
  season_dates
) {
  season_start <- season_dates$season_start
  season_end <- season_dates$season_end

  this_season <- year(season_start)

  # ktc names lookup
  ktc_tibble <- ktc_list |>
    bind_rows(.id = "date") |>
    mutate(
      date = str_remove(date, "ktc_value") |>
        str_remove(".csv") |>
        lubridate::mdy(),
      ktc_value = coalesce(ktc_value, value)
    ) |>
    select(-value)

  min_ktc <- min(ktc_tibble$ktc_value)

  weekly_value_added |>
    filter(season == this_season) |>
    mutate(
      date = season_start + weeks(week - 1)
    ) |>
    left_join(
      ktc_tibble |> rename(ktc_date = date),
      join_by(name, closest(date >= ktc_date))
    ) |>
    rowwise() |>
    mutate(ktc_value = if_else(is.na(ktc_value), min_ktc, ktc_value)) |>
    ungroup() |>
    left_join(player_info, by = join_by(name, position)) |>
    mutate(
      age = as.numeric(season_start - birth_date) /
        365.25 +
        as.numeric(date - season_start) /
          (as.numeric(season_end - season_start))
    ) |>
    select(season, week, name, position, age, ktc_value, value_added)
}

compile_data_set <- function(
  future_value_names,
  keep_trade_cut,
  sva_tibble,
  va_tibble,
  date,
  season_start,
  season_end
) {
  day_multiplier <- years(1) / days(season_end - season_start)

  this_season <- if_else(
    date < season_end,
    year(season_start),
    year(season_start) + 1
  )

  seasons_ago <- year(today()) - this_season

  # number of days since the season ended (in years)
  days_past_season_start <- if_else(
    date > season_start,
    time_length(lubridate::interval(season_start, date), unit = "years"),
    0
  )

  # va_tibble progress
  va_add <- va_tibble |>
    mutate(
      date = season_start + weeks(week - 1) + years(season - this_season)
    ) |>
    filter(date < current_date) |>
    arrange(name, season, week) |>
    group_by(name) |>
    mutate(
      is_active = as.numeric(value_added != 0),
      ewma_active = accumulate(
        is_active,
        ~ decay * .x + (1 - decay) * .y,
        .init = 0
      )[-1],
      ewma_value = accumulate(
        value_added,
        ~ decay * .x + (1 - decay) * .y,
        .init = 0
      )[-1],
      magnitude_given_active = ewma_value / pmax(ewma_active, 0.05)
    ) |>
    filter(season == this_season) |>
    slice_max(week, with_ties = FALSE) |>
    select(
      season,
      week,
      name,
      position,
      ewma_active,
      magnitude_given_active
    )

  history_bp <- sva_tibble |>
    filter(season < this_season) |>
    left_join(future_value_names, by = join_by(name, position)) |>
    mutate(
      age = as.numeric(season_start - birth_date) /
        365.25 +
        season -
        this_season,
      tva_adj = total_value_added
    ) |>
    select(season, name, position, age, tva_adj) |>
    nest(history = c(season, age, tva_adj))

  future_value_names |>
    filter(years_exp >= seasons_ago) |>
    left_join(va_add, by = join_by(name, position)) |>
    left_join(keep_trade_cut, by = join_by(name)) |>
    left_join(history_bp, by = join_by(name, position)) |>
    mutate(
      age = time_length(
        lubridate::interval(birth_date, season_start),
        unit = "years"
      ),
      age = age + days_past_season_start * day_multiplier,
      ktc = replace_na(ktc_value, 0),
      week = replace_na(week, 0),
      season = replace_na(season, this_season),
      ewma_active = replace_na(ewma_active, 0),
      magnitude_given_active = replace_na(magnitude_given_active, 0),
    ) |>
    select(
      season,
      week,
      name,
      position,
      ktc,
      age,
      ewma_active,
      magnitude_given_active,
      history
    )
}

select_ktc_list <- function(ktc_list, last_date_fvt) {
  dates <- names(ktc_list) |>
    str_remove("ktc_value") |>
    str_remove(".csv") |>
    mdy()

  dates <- dates[dates > last_date_fvt]
  keep_trade_cut <- ktc_list[dates > last_date_fvt]
  dates_order <- order(dates)

  list(
    date = dates[dates_order],
    keep_trade_cut = ktc_list[dates_order]
  )
}

# FPCA Stuff ---------------------------------------------------------------

fit_fpca_position <- function(pos_data, pve = 0.90, knots = 6, K_cap = 4) {
  d <- pos_data |>
    transmute(argvals, subj = factor(subj), y) |>
    filter(!is.na(argvals), !is.na(y))

  n_dropped <- nrow(pos_data) - nrow(d)
  if (n_dropped > 0) {
    warning(sprintf(
      "Dropped %d row(s) with NA argvals/y before face.sparse",
      n_dropped
    ))
  }
  if (n_distinct(d$argvals) < 2) {
    stop(
      "Fewer than 2 unique ages available -- cannot fit face.sparse for this position"
    )
  }

  pos_age_grid <- seq(min(d$argvals), max(d$argvals), length.out = 100)

  fit <- face.sparse(
    as.data.frame(d),
    argvals.new = pos_age_grid,
    knots = knots,
    pve = pve
  )
  K <- min(ncol(fit$eigenfunctions), K_cap)
  fit$eigenfunctions <- fit$eigenfunctions[, 1:K, drop = FALSE]
  fit$eigenvalues <- fit$eigenvalues[1:K]
  fit$K <- K
  fit$age_grid <- pos_age_grid
  fit
}

make_mu_fun <- function(fit) {
  function(a) approx(fit$age_grid, fit$mu.new, xout = a, rule = 2)$y
}

make_phi_fun <- function(fit) {
  function(a) {
    apply(fit$eigenfunctions, 2, function(col) {
      approx(fit$age_grid, col, xout = a, rule = 2)$y
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

  pos_data |>
    group_by(subj) |>
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
        as_tibble(matrix(res$mean, nrow = 1)) |>
          set_names(paste0("xi", seq_len(fit$K)))
      }
    ) |>
    ungroup()
}

fit_weekly_score_models <- function(
  weekly_data,
  raw_scores,
  fpca_fit,
  decay = 0.85
) {
  panel <- weekly_data |>
    rename(subj = name) |>
    arrange(subj, season, week) |>
    group_by(subj) |>
    mutate(
      is_active = as.numeric(value_added != 0),
      ewma_active = accumulate(
        is_active,
        ~ decay * .x + (1 - decay) * .y,
        .init = 0
      )[-1],
      ewma_value = accumulate(
        value_added,
        ~ decay * .x + (1 - decay) * .y,
        .init = 0
      )[-1],
      magnitude_given_active = ewma_value / pmax(ewma_active, 0.05)
    ) |>
    ungroup() |>
    left_join(raw_scores, by = "subj")

  hurdle_model <- glm(
    is_active ~ week + ktc_value + age,
    data = panel,
    family = binomial()
  )

  panel <- panel |>
    mutate(
      pred_active_rate = predict(hurdle_model, type = "response")
    )

  rhs <- "ktc_value + age + week + pred_active_rate + ewma_active * magnitude_given_active"

  score_models <- map(seq_len(fpca_fit$K), function(k) {
    xi_col <- paste0("xi", k)
    d_k <- panel |> filter(!is.na(.data[[xi_col]]))
    lm(as.formula(paste0(xi_col, " ~ ", rhs)), data = d_k)
  })
  names(score_models) <- paste0("xi", seq_len(fpca_fit$K))

  tau2_const <- map_dbl(seq_len(fpca_fit$K), function(k) {
    pmin(sigma(score_models[[k]])^2, fpca_fit$eigenvalues[k])
  })

  var_models <- map(seq_len(fpca_fit$K), function(k) {
    xi_col <- paste0("xi", k)
    resid2 <- residuals(score_models[[k]])^2
    floor_val <- fpca_fit$eigenvalues[k] * 1e-4
    d_var <- panel |>
      filter(!is.na(.data[[xi_col]])) |>
      mutate(log_resid2 = log(pmax(resid2, floor_val)))
    lm(as.formula(paste0("log_resid2 ~ ", rhs)), data = d_var)
  })
  names(var_models) <- paste0("xi", seq_len(fpca_fit$K))

  predict_inseason <- function(
    ktc_in,
    age,
    week = 0,
    ewma_active = 0,
    magnitude_given_active = 0
  ) {
    pred_active_rate <- predict(
      hurdle_model,
      newdata = tibble(week = week, ktc_value = ktc_in, age = age),
      type = "response"
    )
    newdata <- tibble(
      ktc_value = ktc_in,
      age = age,
      week = week,
      pred_active_rate = pred_active_rate,
      ewma_active = ewma_active,
      magnitude_given_active = magnitude_given_active
    )

    g_mean <- map_dbl(seq_len(fpca_fit$K), function(k) {
      predict(score_models[[k]], newdata = newdata)
    })
    prior_var <- map_dbl(seq_len(fpca_fit$K), function(k) {
      pmin(
        exp(predict(var_models[[k]], newdata = newdata)),
        fpca_fit$eigenvalues[k]
      )
    })

    list(mean = g_mean, var = prior_var)
  }

  list(
    models = score_models,
    var_models = var_models,
    hurdle_model = hurdle_model,
    predict_inseason = predict_inseason,
    tau2_const = tau2_const,
    n_train = nrow(panel)
  )
}

age_taper_weight <- function(age, position, taper_window = c(5, 2)) {
  max_age <- case_when(
    position == "QB" ~ 40,
    position == "RB" ~ 34,
    position == "TE" ~ 36,
    position == "WR" ~ 36
  )

  window <- taper_window[1] + taper_window[2]
  t <- pmin(pmax((age - max_age + taper_window[1]) / window, 0), 1)
  1 - (3 * t^2 - 2 * t^3)
}

train_models <- function(fpca_data, weekly_data) {
  positions <- sort(unique(fpca_data$position))

  fpca_by_position <- positions |>
    set_names() |>
    map(~ fit_fpca_position(filter(fpca_data, position == .x)))

  raw_scores_by_position <- positions |>
    set_names() |>
    map(
      ~ extract_raw_scores(
        filter(fpca_data, position == .x),
        fpca_by_position[[.x]]
      )
    )

  score_models_by_position <- positions |>
    set_names() |>
    map(function(pos) {
      fit_weekly_score_models(
        weekly_data = filter(weekly_data, position == pos),
        raw_scores = raw_scores_by_position[[pos]],
        fpca_fit = fpca_by_position[[pos]]
      )
    })

  list("fpca" = fpca_by_position, "score_models" = score_models_by_position)
}

.project_posterior <- function(
  fit,
  sm,
  ktc_in,
  age_in,
  history,
  ages_out,
  week = 0,
  ewma_active = 0,
  magnitude_given_active = 0
) {
  mu_fun <- make_mu_fun(fit)
  phi_fun <- make_phi_fun(fit)

  inseason <- sm$predict_inseason(
    ktc_in = ktc_in,
    age = age_in,
    week = week,
    ewma_active = ewma_active,
    magnitude_given_active = magnitude_given_active
  )
  g_mean <- inseason$mean
  prior_var <- inseason$var

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
    prior_var = prior_var,
    sigma2 = fit$sigma2
  )

  mu_out <- mu_fun(ages_out)
  Phi_out <- matrix(sapply(ages_out, phi_fun), ncol = fit$K, byrow = TRUE)

  list(
    mu_out = mu_out,
    Phi_out = Phi_out,
    post = post
  )
}

project_career <- function(
  fit,
  sm,
  ktc_in,
  age_in,
  history = NULL,
  pos,
  last_season,
  week = 0,
  ewma_active = 0,
  magnitude_given_active = 0,
  n_years_ahead = 10,
  discount_rate = 0.95,
  quantile_probs = ppoints(20),
  quantile_basis = c("predictive", "curve"),
  taper_window = c(5, 2)
) {
  quantile_basis <- match.arg(quantile_basis)
  ages_out <- age_in + seq_len(n_years_ahead)

  p <- .project_posterior(
    fit,
    sm,
    ktc_in,
    age_in,
    history,
    ages_out,
    week,
    ewma_active,
    magnitude_given_active
  )

  taper_weight <- age_taper_weight(ages_out, pos, taper_window)

  pred_va_raw <- as.vector(p$mu_out + p$Phi_out %*% p$post$mean)
  var_curve_raw <- diag(p$Phi_out %*% p$post$cov %*% t(p$Phi_out))

  pred_va <- taper_weight * pred_va_raw
  var_curve <- taper_weight^2 * var_curve_raw
  var_predictive <- taper_weight^2 * (var_curve_raw + fit$sigma2)
  se_curve <- sqrt(pmax(var_curve, 0))
  se_predictive <- sqrt(pmax(var_predictive, 0))

  expected_value <- tibble(
    season = last_season + seq_len(n_years_ahead),
    age = ages_out,
    pred_va = pred_va,
    se_curve = se_curve,
    se_predictive = se_predictive,
    taper_weight = taper_weight
  ) |>
    mutate(
      pred_va = pmax(0, pred_va)
    )

  w <- discount_rate^seq_len(n_years_ahead)
  v <- colSums(p$Phi_out * w)
  fv_estimate <- sum(w * pred_va)
  fv_var <- as.numeric(t(v) %*% p$post$cov %*% v) + fit$sigma2 * sum(w^2)
  future_value <- tibble(estimate = fv_estimate, se = sqrt(pmax(fv_var, 0)))

  se_for_quantiles <- if (quantile_basis == "predictive") {
    se_predictive
  } else {
    se_curve
  }
  quantiles <- expected_value |>
    select(season, age, pred_va) |>
    mutate(se = se_for_quantiles) |>
    crossing(quantile_prob = quantile_probs) |>
    mutate(
      quantile_value = qnorm(quantile_prob, mean = pred_va, sd = se),
      quantile_value = pmax(-20, quantile_value)
    ) |>
    select(season, age, quantile_prob, quantile_value)

  list(
    expected_value = expected_value,
    future_value = future_value,
    quantiles = quantiles
  )
}

project_careers <- function(
  players_df,
  models,
  id_col = "name",
  n_years_ahead = 10,
  discount_rate = 0.95,
  quantile_probs = ppoints(20),
  quantile_basis = c("predictive", "curve"),
  taper_window = c(5, 2)
) {
  missing_pos <- setdiff(unique(players_df$position), names(models$fpca))
  if (length(missing_pos) > 0) {
    stop(sprintf(
      "No fitted model for position(s): %s",
      paste(missing_pos, collapse = ", ")
    ))
  }

  if (!(id_col %in% names(players_df))) {
    players_df[[id_col]] <- seq_len(nrow(players_df))
  }
  has_history_col <- "history" %in% names(players_df)

  for (col in c("week", "ewma_active", "magnitude_given_active")) {
    if (!(col %in% names(players_df))) players_df[[col]] <- 0
  }

  last_season <- unique(players_df$season) - 1
  if (length(last_season) != 1) {
    stop(
      "players_df$season must be constant across all rows for a single project_careers() call"
    )
  }

  results <- map(seq_len(nrow(players_df)), function(i) {
    row <- players_df[i, ]
    pos <- row$position
    fit <- models$fpca[[pos]]
    sm <- models$score_models[[pos]]
    hist_i <- if (has_history_col) row$history[[1]] else NULL

    out <- project_career(
      fit = fit,
      sm = sm,
      ktc_in = row$ktc,
      age_in = row$age,
      history = hist_i,
      pos = pos,
      last_season = last_season,
      week = row$week,
      ewma_active = row$ewma_active,
      magnitude_given_active = row$magnitude_given_active,
      n_years_ahead = n_years_ahead,
      discount_rate = discount_rate,
      quantile_probs = quantile_probs,
      quantile_basis = quantile_basis,
      taper_window = taper_window
    )

    id_val <- row[[id_col]]
    out$expected_value <- out$expected_value |>
      mutate("{id_col}" := id_val, position = pos, .before = 1)
    out$future_value <- out$future_value |>
      mutate("{id_col}" := id_val, position = pos, .before = 1)
    out$quantiles <- out$quantiles |>
      mutate("{id_col}" := id_val, position = pos, .before = 1)
    out
  })

  list(
    expected_value = map_dfr(results, "expected_value"),
    future_value = map_dfr(results, "future_value") |> arrange(desc(estimate)),
    quantiles = map_dfr(results, "quantiles")
  )
}
