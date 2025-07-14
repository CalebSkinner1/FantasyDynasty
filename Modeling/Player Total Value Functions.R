# Player Total Value Functions
# this file stores all the functions needed for the Player Total Value Page

library("tidyverse"); theme_set(theme_minimal())
library("tidymodels")
library("parsnip")
library("dbarts")
library("vip")
library("here")
library("tictoc")
library("furrr")

# Prep Data ---------------------------------------------------------------

compile_data_set <- function(keep_trade_cut, future_value_names, date, season_start, season_end){
  day_multiplier <- years(1)/days(season_end - season_start)
  
  season <- if_else(date < season_end, year(season_end) - 2, year(season_end) - 1)
  
  # number of days since the season ended (in years)
  days_past_season_start <- if_else(date > season_start, time_length(interval(season_start, date), unit = "years"), 0)
  
  future_value_names %>%
    left_join(keep_trade_cut, by = join_by(name)) %>%
    mutate(
      # this is supposed to represent the values at the end of last season (hence the minus 1)
      age = time_length(interval(birth_date, season_start), unit = "years") - 1,
      age = age + days_past_season_start*day_multiplier,
      season = season,
      ktc_value = replace_na(ktc_value, 0)) %>%
    select(name, position, birth_date, age, ktc_value, season, years_exp)
}

interaction_terms_tva <- function(data){
  data %>%
    select(historical_value, age) %>%
    mutate(
      x1_2 = historical_value^2,
      x2_2 = age^2,
      x1_x2 = historical_value*age)
}

# identify the appropriate values to scale
compute_tva_scales <- function(data){
  summaries <- data %>% interaction_terms_tva()
  
  means <- colMeans(summaries)
  
  sds <- sapply(summaries, sd)
  
  list("means" = means, "sds" = sds)
}

# prep data with transformations and interactions
prep_data_tva <- function(data, scales, split_prop = .8){
  df <- data %>% interaction_terms_tva()
  
  means <- scales$means[colnames(df)]
  sds <- scales$sds[colnames(df)]
  
  scaled_data <- pmap_dfr(list(df, means, sds), function(df, means, sds){
    (df - means)/sds}) %>%
    mutate(position = data$position,
           Y = data$tva_adj)
  
  data_split <- initial_split(scaled_data, prop = split_prop)
  
  list("train_data" = training(data_split),
       "test_data" = testing(data_split),
       "full_data" = scaled_data)
}

interaction_terms_ktc <- function(data){
  data %>%
    select(historical_value, age, tva_adj) %>%
    mutate(
      x1_2 = historical_value^2,
      x2_2 = age^2,
      x1_x2 = historical_value*age,
      x2_x3 = age*tva_adj
    )
}

# identify the appropriate values to scale
compute_ktc_scales <- function(data){
  summaries <- data %>% interaction_terms_ktc()
  
  means <- colMeans(summaries)
  
  sds <- sapply(summaries, sd)
  
  list("means" = means, "sds" = sds)
}

# prep data with transformations and interactions
prep_data_ktc <- function(data, scales, split_prop = .8){
  df <- data %>% interaction_terms_ktc()
  
  min_ktc <- min(data$ktc_value, na.rm = TRUE)
  
  ktc <- if_else(is.na(data$ktc_value),
                runif(1, min = 0, max = min_ktc),
                data$ktc_value)
  
  means <- scales$means[colnames(df)]
  sds <- scales$sds[colnames(df)]
  
  scaled_data <- pmap_dfr(list(df, means, sds), function(df, means, sds){
    (df - means)/sds}) %>%
    mutate(
      position = data$position,
      Y = ktc)
  
  data_split <- initial_split(scaled_data, prop = split_prop)
  
  list("train_data" = training(data_split),
       "test_data" = testing(data_split),
       "full_data" = scaled_data)
}

# BART --------------------------------------------------------------------

fit_bart <- function(train_data, tune_grid = 20){
  # Chipman, George, McCulloch (2005)
  # cross validation
  df_folds <- vfold_cv(train_data)
  
  # Preprocessing recipe
  rec <- recipe(Y ~ ., data = train_data)
  
  # Specify BART model
  bart_spec <- parsnip::bart(
    trees = tune(),
    prior_terminal_node_coef = tune(),
    prior_terminal_node_expo = tune()) %>%
    set_engine("dbarts") %>%
               # control = dbarts::bartControl(
               #   n.samples = 200,   # posterior samples (after burn-in)
               #   n.burn = 100       # optional burn-in samples
    set_mode("regression")
  
  # parameters object
  parameters_object <- workflow() %>%
    add_model(bart_spec) %>%
    add_recipe(rec) %>%
    extract_parameter_set_dials() %>%
    update(
      prior_terminal_node_coef = prior_terminal_node_coef(range = c(.4, .9)),
      prior_terminal_node_expo = prior_terminal_node_expo(range = c(1, 3))) %>%
    finalize(train_data)
  
  # tune model parameters
  tune_object <- workflow() %>%
    add_model(bart_spec) %>%
    add_recipe(rec) %>%
    tune_grid(
      df_folds,
      grid = tune_grid,
      param_info = parameters_object,
      metrics = metric_set(rmse)
    )
  
  # select best parameters
  best_param <- select_best(tune_object, metric = "rmse") %>% 
    select(-.config)
  
  # Create a bart workflow
  workflow_object <- workflow() %>%
    add_model(bart_spec) %>%
    add_recipe(rec) %>%
    finalize_workflow(best_param)
  
  # Fit the model
  fit(workflow_object, data = train_data)
}

# draw posterior samples from model fit
generate_samples <- function(fit, data){
  model <- extract_fit_engine(fit)
  
  # Generate posterior predictive samples
  predict(model, newdata = data)
}

model_residuals <- function(fit, data){
  samples <- generate_samples(fit, data)
  
  posterior_mean <- colMeans(samples)
  
  new_data <- data %>%
    mutate(abs_residuals = abs(Y - posterior_mean)) %>%
    select(-Y)
  
  library("mgcv")
  # Create formula string: s(X1) + s(X2) + ...
  predictor_vars <- setdiff(names(new_data), c("abs_residuals", "position"))
  smoother_terms <- paste0("s(", predictor_vars, ")", collapse = " + ")
  formula_text <- paste("abs_residuals ~", smoother_terms, " + position")
  gam_formula <- as.formula(formula_text)
  
  gam(gam_formula, data = new_data,
                     family = gaussian(link = "log"))  
}

model_accuracy <- function(fit, test_data){
  augment(fit, test_data) %>%
    rmse(Y, .pred)
}

graph_residuals <- function(fit, test_data){
  augment(fit, test_data) %>%
    mutate(resid = Y - .pred) %>%
    ggplot() +
    geom_density(aes(resid))
}

compute_coverage <- function(fit, test_data, confidence = .95){
  samples <- generate_samples(fit, test_data)
  
  # compute coverage
  quantiles <- samples %>% apply(2, quantile, probs = c((1-confidence)/2, (1+confidence)/2))
  between(test_data$Y, quantiles[1,], quantiles[2,]) %>% mean()
}

# Simulate Future Value ---------------------------------------------------------------

# this function updates the data so its ready for the next year
update_data_year <- function(data){
  min_ktc <- min(data$ktc_value, na.rm = TRUE)
  
  # hv <- if_else(is.na(data$ktc_value),
  #                             runif(1, min = 0, max = min_ktc),
  #                             data$ktc_value)
  hv <- replace_na(data$ktc_value, 0)
  
  data %>%
    mutate(
      age = age + 1,
      season = season + 1,
      historical_value = hv,
      tva_adj = 0,
      ktc_value = 0) %>%
    select(name, historical_value, season, position, birth_date, age, tva_adj, ktc_value)
}

# compute quantiles from samples
compute_quantiles <- function(samples, resid_fit, data){
  sigma_hat <- predict(resid_fit, newdata = data %>% select(-Y), type = "response")
  
  # compute quantiles
  posterior_mean <- colMeans(samples)
  
  map(seq(.025, .975, by = .025), ~{
    (posterior_mean + qnorm(p = .x)*sigma_hat)}) %>% do.call(rbind, .)
}

integrate_quantiles <- function(quantile_list){
  quantile_list %>%
    do.call(rbind, .) %>%
    apply(., 2, quantile, probs = seq(.025, .975, by = .025))
}

# this function bounds value according to domain knowledge
bound_tva <- function(quantiles, data){
  if(length(data) == 1){ #i.e. origin
    map(seq_len(nrow(quantiles)), ~{
      tv <- case_when(data[[1]]$historical_value == 0 ~ 0, # set to 0 if ktc is 0 
                      data[[1]]$age > 43 ~ 0, # set to 0 if age is too large
                      .default = pmax(quantiles[.x,], -20)) # and don't let quantile get below -20
      
      data[[1]] %>% mutate(tva_adj = tv)
    })
  }else{
    imap(seq_along(data), ~{
      tv <- case_when(data[[.x]]$historical_value == 0 ~ 0, # set to 0 if ktc is 0 
                      data[[.x]]$age > 43 ~ 0, # set to 0 if age is too large
                      .default = pmax(quantiles[.x,], -20)) # and don't let quantile get below -20
      
      data[[.x]] %>% mutate(tva_adj = tv)
    })
  }
}

bound_ktc <- function(samples_list, tva_data_list){
  imap(seq_along(tva_data_list), ~{
    ktcv <- case_when(
      tva_data_list[[.x]]$historical_value == 0 ~ 0, #if out, then stay out
      tva_data_list[[.x]]$age > 43 ~ 0, # if over 43, be done
      tva_data_list[[.x]]$tva_adj == -20 ~ 0, # if tva_adj == - 20, then out
      samples_list[.x,] < 0 ~ 0,
      samples_list[.x,] > 9999 ~ 9999, # cap ktc at 9999
      .default = samples_list[.x,]
    )
    # values are not on normal 9999 to 1 scale. I need to scale them to return
    # this is ok because value is relative anyway
    # conditional min-max scaling
    
    tva_data_list[[.x]] %>% mutate(ktc_value = ktcv)
  })
}

next_year <- function(data_list, seasons_list, tva_scales, ktc_scales,
                      tva_fit, ktc_fit, tva_resid_fit, ktc_resid_fit){
  if(length(data_list) != 39){
    data_list <- list("origin" = data_list)
  }
  # update data (add year to age, shift historical value)
  updated_data <- map(data_list, update_data_year)
  
  # prep tva modeling
  tva_prep <- map(updated_data, ~prep_data_tva(.x, tva_scales)$full_data)
  
  # list of tva samples (for each quantile)
  tva_data_list <- map(tva_prep, ~{
    generate_samples(tva_fit, .x) %>%
      compute_quantiles(tva_resid_fit, .x)
  }) %>% integrate_quantiles() %>% #combine quantiles from possible data sets into one
    bound_tva(updated_data) # add with updated data
  
  # prep ktc modeling
  ktc_prep <- map(tva_data_list, ~{
     prep_data_ktc(.x, ktc_scales)$full_data
  })
  
  # list of ktc samples (for each quantile)
  ktc_data_list <- map(ktc_prep, ~{
    generate_samples(ktc_fit, .x) %>%
      compute_quantiles(ktc_resid_fit, .x)
  }) %>% integrate_quantiles() %>% #bind lists together and computes aggregate quantiles
    bound_ktc(tva_data_list)
  
  compile <- imap_dfc(seq_along(ktc_data_list), ~{
    df <- tibble(ktc_data_list[[.x]]$tva_adj)
    
    colnames(df) <- paste0("proj_tva_", .x*2.5)
    
    df
  }) %>%
    mutate(name = updated_data[[1]]$name) %>%
    relocate(name)
  
  seasons_list <- c(seasons_list, setNames(list(compile), paste0(updated_data[[1]]$season[1])))
  
  list("data" = ktc_data_list, "seasons_list" = seasons_list)
}

next_years <- function(origin_data, n_years, tva_scales, ktc_scales,
                       tva_fit, ktc_fit, tva_resid_fit, ktc_resid_fit){
  updating_list <- list("data" = origin_data, "seasons_list" = list())
  
  for(i in 1:n_years){
    updating_list <- next_year(updating_list$data, updating_list$seasons_list, tva_scales, ktc_scales,
                               tva_fit, ktc_fit, tva_resid_fit, ktc_resid_fit)
  }
  
  updating_list$seasons_list
}

compute_future_value <- function(seasons_list, years = 8, weight = .95){
  
  future_value <- imap(1:years, ~{
    pmax(seasons_list[[.x]]$proj_tva_50, 0)*weight^(.x - 1)
  }) %>% as.data.frame() %>%
    rowSums()
  
  tibble(
    name = seasons_list[[1]]$name,
    future_value = future_value) %>%
    arrange(desc(future_value))
}

# Future Value over Time --------------------------------------------------

future_value_over_time <- function(future_value_names, keep_trade_cut, date,
                                   tva_scales, ktc_scales, tva_fit, ktc_fit,
                                   tva_resid_fit, ktc_resid_fit, season_start, season_end){
  
  season_end <- season_end[date < season_end] %>% min() # identify season end for counting weeks
  season_start <- season_start[season_end > season_start] %>% min() # identify season start for counting weeks
  
  # this is my arbitrary cutoff to include rookies
  diff <- time_length(interval(date, ymd(str_c(year(today()), "-03-01"))), unit = "year") %>% floor()
  
  # compile data - age and season look to be too small, but will be added in next_year
  df <- compile_data_set(keep_trade_cut, future_value_names, date, season_start, season_end) %>%
    filter(years_exp > diff) #remove rookies or 1st years that shouldn't appear yet
    
  # compute fraction of remaining season
  # weeks_in <- time_length(interval(season_start, date), unit = "week") %>% floor()
  
  sims <- next_years(df, n_years = 8, tva_scales, ktc_scales, tva_fit, ktc_fit, tva_resid_fit, ktc_resid_fit)
  
  future_value_names %>%
    left_join(compute_future_value(sims, years = 8, weight = .95), by = join_by(name)) %>%
    select(name, future_value) %>%
    mutate(date = date)
}

map_future_value_time <- function(future_value_names, ktc_list,
                                  tva_scales, ktc_scales, tva_fit, ktc_fit,
                                  tva_resid_fit, ktc_resid_fit, season_start, season_end){
  map2_dfr(ktc_list, names(ktc_list), ~{
    date <- .y %>% str_remove("ktc_value") %>% str_remove(".csv") %>% mdy()
    
    colnames(.x) <- c("name", "ktc_value")
    
    future_value_over_time(future_value_names, .x, date, tva_scales, ktc_scales,
                           tva_fit, ktc_fit, tva_resid_fit, ktc_resid_fit, season_start, season_end)}
    # .progress = TRUE,
    # .options = furrr_options(seed = TRUE)
  ) %>% arrange(desc(date))
}

select_ktc_list <- function(ktc_list, last_date_fvt){
  dates <- names(ktc_list) %>% str_remove("ktc_value") %>% str_remove(".csv") %>% mdy()
  
  ktc_list[dates > last_date_fvt]
}



