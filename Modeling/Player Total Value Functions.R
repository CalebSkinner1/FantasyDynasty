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

# Simulate Future Value ---------------------------------------------------------------

# draw posterior samples from model fit
generate_samples <- function(fit, data){
  model <- extract_fit_engine(fit)
  
  # Generate posterior predictive samples
  predict(model, newdata = data, n.samples = samples)
}

# this function updates the data so its ready for the next year
update_data_year <- function(data){
  min_ktc <- min(data$ktc_value, na.rm = TRUE)
  
  hv <- if_else(is.na(data$ktc_value),
                              runif(1, min = 0, max = min_ktc),
                              data$ktc_value)
  
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
compute_quantiles <- function(samples){
  # compute quantiles
  quant <- apply(samples, 2, quantile, probs = seq(.05, .95, by = .05)) %>% t()
  
  # split into list
  split(quant, col(quant))
}

# this function bounds value according to domain knowledge
bound_tva <- function(samples_list, data){
  if(length(data) == 1){ #i.e. origin
    map(samples_list, ~{
      tv <- if_else(data[[1]]$historical_value == 0, 0, .x) #set to 0 if ktc is 0
      
      data[[1]] %>% mutate(tva_adj = tv)
    })
  }else{
    map2(samples_list, data, ~{
      tv <- if_else(.y$historical_value == 0, 0, .x) #set to 0 if ktc is 0
      
      .y %>% mutate(tva_adj = tv)
    })
  }
}

bound_ktc <- function(samples_list, tva_data_list){
  map2(samples_list, tva_data_list, ~{
    ktcv <- case_when(
      .y$historical_value == 0 ~ 0, #if out, then stay out
      .x < 0 ~ 0,
      .x > 9999 ~ 9999, # cap ktc at 9999
      .default = .x
    )
    # values are not on normal 9999 to 1 scale. I need to scale them to return
    # this is ok because value is relative anyway
    # conditional min-max scaling
    
    .y %>% mutate(ktc_value = ktcv)
  })
}

next_year <- function(data_list, seasons_list, tva_scales, ktc_scales, tva_fit, ktc_fit){
  if(length(data_list) != 19){
    data_list <- list("origin" = data_list)
  }
  # update data (add year to age, shift historical value)
  updated_data <- map(data_list, update_data_year)
  
  # prep tva modeling
  tva_prep <- map(updated_data, ~prep_data_tva(.x, tva_scales)$full_data)
  
  # list of tva samples (for each quantile)
  tva_data_list <- map(tva_prep, ~{
    generate_samples(tva_fit, .x)
  }) %>% do.call(rbind, .) %>% #bind lists together
    compute_quantiles() %>% #compute quantiles
    bound_tva(updated_data) # add with updated data
  
  # prep ktc modeling
  ktc_prep <- map(tva_data_list, ~{
     prep_data_ktc(.x, ktc_scales)$full_data
  })
  
  # list of ktc samples (for each quantile)
  ktc_data_list <- map(ktc_prep, ~{
    generate_samples(ktc_fit, .x)
  }) %>% do.call(rbind, .) %>% #bind lists together
    compute_quantiles() %>% # compute quantiles
    bound_ktc(tva_data_list)
  
  compile <- imap(seq_along(ktc_data_list), ~{
    df <- tibble(ktc_data_list[[.x]]$tva_adj)
    
    perc <- as.numeric(names(ktc_data_list)[.x])*5
    
    colnames(df) <- paste0("proj_tva_", perc)
    
    df
  }) %>% bind_cols() %>%
    mutate(name = updated_data[[1]]$name) %>%
    relocate(name)
  
  seasons_list <- c(seasons_list, setNames(list(compile), paste0(updated_data[[1]]$season[1])))
  
  list("data" = ktc_data_list, "seasons_list" = seasons_list)
}

next_years <- function(origin_data, n_years, tva_scales, ktc_scales, tva_fit, ktc_fit){
  updating_list <- list("data" = origin_data, "seasons_list" = list())
  
  for(i in 1:n_years){
    updating_list <- next_year(updating_list$data, updating_list$seasons_list, tva_scales, ktc_scales, tva_fit, ktc_fit)
  }
  
  updating_list$seasons_list
}

compute_future_value <- function(seasons_list, years = 15, weight = .95){
  future_value <- imap(1:years, ~{
    seasons_list[[.x]]$proj_tva_50*weight^(.x - 1)
  }) %>% as.data.frame() %>%
    rowSums()
  
  tibble(
    name = seasons_list[[1]]$name,
    future_value = future_value) %>%
    arrange(desc(future_value))
}


