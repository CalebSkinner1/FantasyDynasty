# Player Total Value Functions
# this file stores all the functions needed for the Player Total Value Page

suppressPackageStartupMessages({
  library("tidyverse")
  library("tidymodels")
  library("parsnip")
  library("dbarts")
  library("vip")
  library("here")
  library("tictoc")
  library("furrr")
})
theme_set(theme_minimal())

# Prep Data ---------------------------------------------------------------
compile_training_data <- function(
  ktc_list,
  player_info,
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
    ) %>%
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
    ) %>%
    name_correction()

  colnames(pre_ktc) <- c("name", "ktc_value")
  colnames(post_ktc) <- c("name", "ktc_value")

  # create data table
<<<<<<< Updated upstream
  pre_ktc %>%
=======
  pre_ktc |>
>>>>>>> Stashed changes
    rename("historical_value" = "ktc_value") %>%
    left_join(
      season_value_added %>% filter(season == this_season),
      by = join_by(name)
    ) %>%
    select(-total_points) %>%
    mutate(
      total_value_added = replace_na(total_value_added, 0)
    ) %>%
    rename(tva_adj = total_value_added) %>%
    left_join(post_ktc, by = join_by(name)) %>%
    select(-position) %>%
    left_join(player_info, by = join_by(name)) %>%
    select(-player_id, -years_exp) %>%
    mutate(
<<<<<<< Updated upstream
      age = as.numeric(pre_ktc_date - birth_date) / 365.25
=======
      age = as.numeric(pre_ktc_date - birth_date) / 365.25,
      season = this_season
>>>>>>> Stashed changes
    )
}

compile_data_set <- function(
  keep_trade_cut,
  future_value_names,
  sva_tibble,
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

  # number of days since the season ended (in years)
  days_past_season_start <- if_else(
    date > season_start,
    time_length(lubridate::interval(season_start, date), unit = "years"),
    0
  )

  history_bp <- sva_tibble |>
    filter(season < this_season) |>
    select(-total_points) |>
    left_join(future_value_names, by = join_by(name, position)) |>
    mutate(
      age = as.numeric(season_start - birth_date) /
        365.25 +
        season -
        this_season,
      tva_adj = total_value_added
    ) |>
    select(name, position, season, age, tva_adj) |>
    nest(history = c(season, age, tva_adj))

  future_value_names |>
    left_join(keep_trade_cut, by = join_by(name)) |>
    rename(ktc_value = value) |>
    left_join(history_bp, by = join_by(name, position)) |>
    mutate(
      age = time_length(
        lubridate::interval(birth_date, season_start),
        unit = "years"
      ),
      age = age + days_past_season_start * day_multiplier,
      ktc = replace_na(ktc_value, 0)
    ) |>
    select(name, position, ktc, age, history)
}

select_ktc_list <- function(ktc_list, last_date_fvt) {
  dates <- names(ktc_list) |>
    str_remove("ktc_value") |>
    str_remove(".csv") |>
    mdy()

  dates_order <- order(dates[dates > last_date_fvt])

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

fit_score_models <- function(pos_data, raw_scores, fit, ktc_lookup) {
  d <- pos_data |>
    distinct(subj, season, argvals) |>
    inner_join(ktc_lookup, by = c("subj", "season")) |>
    left_join(raw_scores, by = "subj")

  score_models <- map(seq_len(fit$K), function(k) {
    lm(as.formula(paste0("xi", k, " ~ ktc_in + argvals")), data = d)
  })
  names(score_models) <- paste0("xi", seq_len(fit$K))

  tau2_const <- map_dbl(seq_len(fit$K), function(k) {
    pmin(sigma(score_models[[k]])^2, fit$eigenvalues[k])
  })

  var_models <- map(seq_len(fit$K), function(k) {
    resid2 <- residuals(score_models[[k]])^2
    floor_val <- fit$eigenvalues[k] * 1e-4
    d_var <- d |> mutate(log_resid2 = log(pmax(resid2, floor_val)))
    lm(log_resid2 ~ ktc_in + argvals, data = d_var)
  })
  names(var_models) <- paste0("xi", seq_len(fit$K))

  tau2_fun <- function(ktc_in, age) {
    map_dbl(seq_len(fit$K), function(k) {
      raw <- exp(predict(
        var_models[[k]],
        newdata = tibble(ktc_in = ktc_in, argvals = age)
      ))
      pmin(raw, fit$eigenvalues[k])
    })
  }

  list(
    models = score_models,
    var_models = var_models,
    tau2_fun = tau2_fun,
    tau2_const = tau2_const,
    n_train = nrow(d)
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

compute_future_value <- function(fpca_data, hktc_data, positions) {
  # 1. STAGE 1 -- population mean/covariance via sparse FPCA, per position
  positions <- sort(unique(fpca_data$position))

  fpca_by_position <- positions |>
    set_names() |>
    map(~ fit_fpca_position(filter(fpca_data, position == .x)))

  # walk(positions, function(p) {
  #   fit <- fpca_by_position[[p]]
  #   mu_df <- tibble(age = fit$age_grid, mu = fit$mu.new)
  #   print(
  #     fpca_data |>
  #       filter(position == p) |>
  #       ggplot(aes(argvals, y)) +
  #       geom_point(alpha = 0.2) +
  #       geom_line(
  #         data = mu_df,
  #         aes(age, mu),
  #         color = "firebrick",
  #         linewidth = 1
  #       ) +
  #       labs(title = paste("Population mean curve --", p), x = "Age", y = "VA")
  #   )
  # })

  # Variance explained per retained component, per position
  # map(fpca_by_position, ~ .x$eigenvalues / sum(.x$eigenvalues))

  raw_scores_by_position <- positions |>
    set_names() |>
    map(
      ~ extract_raw_scores(
        filter(fpca_data, position == .x),
        fpca_by_position[[.x]]
      )
    )

  # 2. STAGE 2 -- regress raw scores on KTC + age
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
  # map(score_models_by_position, "n_train")

  # How much does KTC explain of each mode? (1 - tau2/lambda)
  # map2(score_models_by_position, fpca_by_position, function(sm, fit) {
  #   1 - sm$tau2_const / fit$eigenvalues
  # })

  # Sanity check the heteroskedastic variance function before trusting it --
  # with only ~n_train rows feeding a regression on squared residuals (an
  # inherently noisy, heavy-tailed target), confirm tau2_fun produces a
  # sensible, not wildly erratic, pattern across age/KTC. Look for: does
  # variance decrease with age (matching "young players are riskier")? Is it
  # reasonably smooth rather than jumping around?
  # walk(positions, function(p) {
  #   sm <- score_models_by_position[[p]]
  #   fit <- fpca_by_position[[p]]
  #   check_grid <- expand_grid(
  #     age = quantile(fit$age_grid, c(0.1, 0.5, 0.9)),
  #     ktc = c(2000, 5000, 8000)
  #   )
  #   print(p)
  #   print(
  #     check_grid |>
  #       mutate(tau2_xi1 = map2_dbl(ktc, age, ~ sm$tau2_fun(.x, .y)[1])) |>
  #       arrange(ktc, age)
  #   )
  # })

  list("fpca" = fpca_by_position, "score_models" = score_models_by_position)
}

.project_posterior <- function(
  fit,
  sm,
  ktc_in,
  age_in,
  history,
  ages_out
) {
  mu_fun <- make_mu_fun(fit)
  phi_fun <- make_phi_fun(fit)

  g_mean <- map_dbl(
    sm$models,
    ~ predict(.x, newdata = tibble(ktc_in = ktc_in, argvals = age_in))
  )
  prior_var <- sm$tau2_fun(ktc_in, age_in)

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
  n_years_ahead = 10,
  discount_rate = 0.95,
  quantile_probs = ppoints(20),
  quantile_basis = c("predictive", "curve"),
  taper_window = c(5, 2)
) {
  quantile_basis <- match.arg(quantile_basis)
  ages_out <- age_in + seq_len(n_years_ahead)

  p <- .project_posterior(fit, sm, ktc_in, age_in, history, ages_out)

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
    rowwise() |>
    mutate(
      pred_va = max(0, pred_va)
    )

  # --- Future value: discounted sum across years, with its own SE
  w <- discount_rate^seq_len(n_years_ahead)
  v <- colSums(p$Phi_out * w) # weighted sum of phi(a_t) across years
  fv_estimate <- sum(w * pred_va)
  fv_var <- as.numeric(t(v) %*% p$post$cov %*% v) + fit$sigma2 * sum(w^2)
  future_value <- tibble(estimate = fv_estimate, se = sqrt(pmax(fv_var, 0)))

  # --- Quantiles: closed-form Normal, given the Gaussian BLUP posterior ---
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

  binded_history <- bind_rows(players_df$history)
  if (nrow(binded_history) == 0) {
    last_season <- 2023
  } else {
    last_season <- bind_rows(players_df$history) |> pull(season) |> max()
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
