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
  
  data_split <- pmap_dfr(list(df, means, sds), function(df, means, sds){
    (df - means)/sds}) %>%
    mutate(position = data$position,
           Y = data$tva_adj) %>%
    initial_split(prop = split_prop)
  
  list("train_data" = training(data_split),
       "test_data" = testing(data_split))
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
  
  means <- scales$means[colnames(df)]
  sds <- scales$sds[colnames(df)]
  
  data_split <- pmap_dfr(list(df, means, sds), function(df, means, sds){
    (df - means)/sds}) %>%
    mutate(
      position = data$position,
      Y = if_else(is.na(data$ktc_value),
                  runif(1, min = 0, max = min(data$ktc_value, na.rm = TRUE)),
                  data$ktc_value)) %>%
    initial_split(prop = split_prop)
  
  list("train_data" = training(data_split),
       "test_data" = testing(data_split))
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


# Simulation Stuff

# draw a new y value from the model with certain beta and sigma values
draw_new_y <- function(beta_samples, sigma_samples, X_new, group) {
  # Randomly select a posterior sample index
  sample_idx <- sample(seq_len(dim(beta_samples)[1]), 1)
  
  # Choose a random sample of parameters from the posterior
  beta_sample <- beta_samples[sample_idx, c(group), ] # sample row of betas
  sigma_sample <- sigma_samples[sample_idx] # sample sigma
  
  # Generate the new outcome (y_new) based on the model
  return(rnorm(1, mean = sum(X_new * beta_sample), sd = sigma_sample)) 
}

# extract new samples
extract_new_samples <- function(parameter_samples, new_data_matrix, new_data_groups){
  beta_samples <- parameter_samples$beta     # Group-level coefficients
  sigma_samples <- parameter_samples$sigma   # Observation noise
  
  N_new <- length(new_data_matrix)
  X <- t(new_data_matrix) %>% as.data.frame() %>% as.list()
  groups <- new_data_groups %>% as.list()
  
  y_new_samples <- map2(X, groups, ~draw_new_y(beta_samples, sigma_samples, .x, .y)) %>%
    unlist() %>%
    as_tibble()
  
  return(y_new_samples)
}

next_year <- function(data){
  year <- year(data$birth_date[1] + dyears(data$age[1])) - 1
  
  min_ktc <- min(data$ktc_value, na.rm = TRUE)
  
  updated_data <- data %>%
    # assign random ktc value to players with super low ktc value
    rowwise() %>% 
    mutate(
      ktc_value = case_when(
        is.na(ktc_value) ~ runif(1, min = 0, max = min_ktc),
        .default = ktc_value)) %>%
    ungroup() %>%
    mutate(
      # update values
      historical_value = ktc_value)
  
  constant_group <- updated_data %>%
    transmute(position = as.factor(position) %>% as.numeric) %>%
    pull()
  
  ny_tva <- updated_data %>%
    prep_data_tva() %>%
    extract_new_samples(tva_parameter_values, ., constant_group) %>%
    bind_cols(select(updated_data, ktc_value)) %>%
    # ktc_value = 0 if retired/out, must stay out
    mutate(value = if_else(ktc_value == 0, 0, value)) %>%
    select(value)
  
  ny_ktc <- updated_data %>%
    mutate(tva_adj = ny_tva$value) %>%
    prep_data_ktc() %>%
    extract_new_samples(ktc_parameter_values, ., constant_group) %>%
    bind_cols(select(updated_data, ktc_value)) %>%
    # ktc_value = 0 if retired/out, must stay out
    mutate(value = if_else(ktc_value == 0, 0, value)) %>%
    select(value)
  
  # values are not on normal 9999 to 1 scale. I need to scale them to return
  # this is ok because value is relative anyway
  # conditional min-max scaling
  
  new_ny_ktc <- ny_ktc %>%
    mutate(
      # return 0s to 0. Interpret these as essentially retired
      value = case_when(
        value < 0 ~ 0,
        value > 9999 ~ 9999, # cap ktc at 9999
        .default = value
        ))
  
  # force max to 9999, allows for ktc value to drop, but not exceed
  # if(max(ny_ktc) > 9999){
  #   new_ny_ktc <- new_ny_ktc %>% mutate(value = value * 9999/max(ny_ktc))}
  
  new_data <- updated_data %>%
    select(name, position, birth_date, age, contains("proj_tva")) %>%
    mutate(
      tva_adj = ny_tva$value,
      ktc_value = new_ny_ktc$value,
      proj_tva = ny_tva$value,
      age = age + 1) %>%
    rename_with(~paste0(.x, "_", (year + 1)), ends_with("proj_tva")) %>%
    relocate(contains("proj_tva"), .after = last_col())
  
  return(new_data)
}

next_years <- function(data, n_years){
  for(i in 1:n_years){
    data <- data %>% next_year()
  }
  final_df <- data %>% arrange(desc(ktc_value)) %>%
    select(-tva_adj)
  return(final_df)
}

compile_future <- function(future_years_df){
  
  future_years <- future_years_df %>%
    rowwise() %>%
    mutate(
      # cap total value added min at -20
      across(contains("proj_tva"), ~pmax(.x, -20)),
      future_value = sum(c_across(contains("proj_tva")))) %>%
    ungroup() %>%
    arrange(desc(future_value))
  
  return(future_years)
}

simulate_future_value <- function(data, n_years, simulations){
  plan(multisession)
  
  future_values_list <- future_map(
    1:simulations,
    ~data %>%
      next_years(n_years) %>%
      compile_future(),
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )
  return(future_values_list)}
