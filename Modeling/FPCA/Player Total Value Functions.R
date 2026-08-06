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
  ktc_fpca_data,
  date,
  season_start,
  season_end,
  decay = 0.85
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

  history_df <- sva_tibble |>
    filter(season < this_season) |>
    left_join(future_value_names, by = join_by(name, position)) |>
    mutate(
      age = as.numeric(season_start - birth_date) /
        365.25 +
        season -
        this_season,
      tva_adj = total_value_added
    ) |>
    left_join(
      select(ktc_fpca_data, -argvals),
      by = join_by(season, name, position)
    ) |>
    transmute(name, position, age, tva_adj, ktc = ktc_value) |>
    nest(history = c(age, tva_adj, ktc))

  future_value_names |>
    filter(years_exp >= seasons_ago) |>
    left_join(keep_trade_cut, by = join_by(name)) |>
    left_join(history_df, by = join_by(name, position)) |>
    mutate(
      age = time_length(
        lubridate::interval(birth_date, season_start),
        unit = "years"
      ),
      age = age + days_past_season_start * day_multiplier,
      ktc = replace_na(ktc_value, 0),
      season = this_season
    ) |>
    select(
      season,
      name,
      position,
      ktc,
      age,
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
    transmute(argvals, name = factor(name), y) |>
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
    d |> rename(subj = name) |> as.data.frame(),
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

fit_fpca_position_ktc <- function(pos_data, pve = 0.90, knots = 6, K_cap = 4) {
  fit_fpca_position(
    pos_data |> transmute(argvals, name, y = ktc_value),
    pve = pve,
    knots = knots,
    K_cap = K_cap
  )
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

  sigma2_vec <- if (length(sigma2) == 1) rep(sigma2, length(y_obs)) else sigma2
  W <- diag(1 / sigma2_vec, length(y_obs))

  Sigma_inv <- Tinv + t(Phi_i) %*% W %*% Phi_i
  Sigma <- solve(Sigma_inv)
  post_mean <- Sigma %*% (Tinv %*% prior_mean + t(Phi_i) %*% W %*% resid)

  list(mean = as.vector(post_mean), cov = Sigma)
}

build_observations <- function(
  history,
  sigma2_full,
  age_in,
  games_played = 0,
  tva_partial = 0,
  games_in_season = 17
) {
  ages <- if (is.null(history) || nrow(history) == 0) {
    numeric(0)
  } else {
    history$age
  }
  y <- if (is.null(history) || nrow(history) == 0) {
    numeric(0)
  } else {
    history$tva_adj
  }
  s2 <- rep(sigma2_full, length(y))

  if (games_played > 0) {
    f <- min(games_played / games_in_season, 1)
    ages <- c(ages, age_in)
    y <- c(y, tva_partial / f)
    s2 <- c(s2, sigma2_full / f)
  }

  list(ages = ages, y = y, sigma2 = s2)
}

extract_raw_scores <- function(pos_data, fit) {
  mu_fun <- make_mu_fun(fit)
  phi_fun <- make_phi_fun(fit)
  sigma2 <- fit$sigma2

  pos_data |>
    group_by(name) |>
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
          set_names(paste0("xi", seq_len(fit$K))) |>
          mutate(n_obs = nrow(.x))
      }
    ) |>
    ungroup()
}

fit_entering_score_models <- function(
  weekly_data,
  raw_scores,
  ktc_history_by_player,
  fpca_fit,
  fpca_fit_ktc
) {
  mu_fun_ktc <- make_mu_fun(fpca_fit_ktc)
  phi_fun_ktc <- make_phi_fun(fpca_fit_ktc)
  ktc_score_cols <- paste0("ktc_xi", seq_len(fpca_fit_ktc$K))

  ktc_score_asof <- function(name_i, as_of_age) {
    h <- ktc_history_by_player |> filter(name == name_i, argvals <= as_of_age)
    if (nrow(h) == 0) {
      return(setNames(rep(0, fpca_fit_ktc$K), ktc_score_cols))
    }
    res <- blup_scores(
      y_obs = h$ktc,
      ages_obs = h$argvals,
      mu_fun = mu_fun_ktc,
      phi_fun = phi_fun_ktc,
      prior_mean = rep(0, fpca_fit_ktc$K),
      prior_var = fpca_fit_ktc$eigenvalues,
      sigma2 = fpca_fit_ktc$sigma2
    )
    setNames(res$mean, ktc_score_cols)
  }

  ktc_xi_by_player_season <- weekly_data |>
    distinct(name, season, age) |>
    rowwise() |>
    mutate(ktc_xi = list(ktc_score_asof(name, age))) |>
    ungroup() |>
    unnest_wider(ktc_xi)

  season_panel <- ktc_xi_by_player_season |> left_join(raw_scores, by = "name")
  entering_rhs <- paste(c(ktc_score_cols, "age"), collapse = " + ")

  entering_models <- map(seq_len(fpca_fit$K), function(k) {
    xi_col <- paste0("xi", k)
    d_k <- season_panel |> filter(!is.na(.data[[xi_col]]))
    lm(
      as.formula(paste0(xi_col, " ~ ", entering_rhs)),
      data = d_k,
      weights = n_obs
    )
  })
  names(entering_models) <- paste0("xi", seq_len(fpca_fit$K))

  var_models <- map(seq_len(fpca_fit$K), function(k) {
    xi_col <- paste0("xi", k)
    d_k <- season_panel |> filter(!is.na(.data[[xi_col]]))
    entering_pred <- predict(entering_models[[k]], newdata = d_k)
    resid2 <- (d_k[[xi_col]] - entering_pred)^2
    floor_val <- fpca_fit$eigenvalues[k] * 1e-4
    d_var <- d_k |> mutate(log_resid2 = log(pmax(resid2, floor_val)))
    lm(as.formula(paste0("log_resid2 ~ ", entering_rhs)), data = d_var)
  })
  names(var_models) <- paste0("xi", seq_len(fpca_fit$K))

  predict_prior <- function(ktc_in, ktc_history = NULL, age) {
    ktc_xi <- if (is.null(ktc_history) || nrow(ktc_history) == 0) {
      setNames(rep(0, fpca_fit_ktc$K), ktc_score_cols)
    } else {
      res <- blup_scores(
        y_obs = ktc_history$ktc,
        ages_obs = ktc_history$age,
        mu_fun = mu_fun_ktc,
        phi_fun = phi_fun_ktc,
        prior_mean = rep(0, fpca_fit_ktc$K),
        prior_var = fpca_fit_ktc$eigenvalues,
        sigma2 = fpca_fit_ktc$sigma2
      )
      setNames(res$mean, ktc_score_cols)
    }

    newdata <- as_tibble(as.list(ktc_xi)) |> mutate(age = age)

    g_mean <- map_dbl(seq_len(fpca_fit$K), function(k) {
      predict(entering_models[[k]], newdata = newdata)
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
    entering_models = entering_models,
    var_models = var_models,
    predict_prior = predict_prior
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

train_models <- function(fpca_data, weekly_data, ktc_fpca_data) {
  positions <- sort(unique(fpca_data$position))

  fpca_by_position <- positions |>
    set_names() |>
    map(~ fit_fpca_position(filter(fpca_data, position == .x)))

  fpca_by_position_ktc <- positions |>
    set_names() |>
    map(~ fit_fpca_position_ktc(filter(ktc_fpca_data, position == .x)))

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
      fit_entering_score_models(
        weekly_data = filter(weekly_data, position == pos),
        raw_scores = raw_scores_by_position[[pos]],
        ktc_history_by_player = filter(ktc_fpca_data, position == pos) |>
          transmute(name, argvals, ktc = ktc_value),
        fpca_fit = fpca_by_position[[pos]],
        fpca_fit_ktc = fpca_by_position_ktc[[pos]]
      )
    })

  list(
    "fpca" = fpca_by_position,
    "fpca_ktc" = fpca_by_position_ktc,
    "score_models" = score_models_by_position
  )
}

.project_posterior <- function(
  fit,
  sm,
  ktc_in,
  ktc_history,
  age_in,
  history,
  ages_out,
  games_played = 0,
  tva_partial = 0,
  games_in_season = 17
) {
  mu_fun <- make_mu_fun(fit)
  phi_fun <- make_phi_fun(fit)

  prior <- sm$predict_prior(
    ktc_in = ktc_in,
    ktc_history = ktc_history,
    age = age_in
  )

  obs <- build_observations(
    history,
    fit$sigma2,
    age_in,
    games_played,
    tva_partial,
    games_in_season
  )

  post <- blup_scores(
    y_obs = obs$y,
    ages_obs = obs$ages,
    mu_fun = mu_fun,
    phi_fun = phi_fun,
    prior_mean = prior$mean,
    prior_var = prior$var,
    sigma2 = obs$sigma2
  )

  mu_out <- mu_fun(ages_out)
  Phi_out <- matrix(sapply(ages_out, phi_fun), ncol = fit$K, byrow = TRUE)

  list(mu_out = mu_out, Phi_out = Phi_out, post = post)
}

project_career <- function(
  fit,
  sm,
  ktc_in,
  age_in,
  ktc_history = NULL,
  history = NULL,
  pos,
  last_season,
  games_played = 0,
  tva_partial = 0,
  games_in_season = 17,
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
    ktc_history,
    age_in,
    history,
    ages_out,
    games_played,
    tva_partial,
    games_in_season
  )

  taper_weight <- age_taper_weight(ages_out, pos, taper_window)

  pred_va_raw <- as.vector(p$mu_out + p$Phi_out %*% p$post$mean)
  var_curve_raw <- diag(p$Phi_out %*% p$post$cov %*% t(p$Phi_out))

  pred_va <- pmax(0, taper_weight * pred_va_raw)
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
  games_in_season = 17,
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
  has_ktc_history_col <- "ktc_history" %in% names(players_df)

  for (col in c("games_played", "tva_partial")) {
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
    ktc_hist_i <- if (has_ktc_history_col) row$ktc_history[[1]] else NULL

    out <- project_career(
      fit = fit,
      sm = sm,
      ktc_in = row$ktc,
      age_in = row$age,
      ktc_history = ktc_hist_i,
      history = hist_i,
      pos = pos,
      last_season = last_season,
      games_played = row$games_played,
      tva_partial = row$tva_partial,
      games_in_season = games_in_season,
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
