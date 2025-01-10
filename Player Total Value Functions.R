# Player Total Value Functions
# this file stores all the functions needed for the Player Total Value Page

# Prep data for and run bayesian hierarchical model

# prep data with transformations and interactions
prep_data_tva <- function(data){
  # remove tva stuff
  means <- means[-c(3, 7)]
  sds <- sds[-c(3, 7)]
  
  df <- data %>%
    select(historical_value, age) %>%
    mutate(
      x1_2 = historical_value^2,
      x2_2 = age^2,
      x1_x2 = historical_value*age)
  
  matrix <- pmap(list(df, means, sds), function(df, means, sds){
    (df - means)/sds}) %>%
    bind_rows() %>%
    mutate(intercept = 1) %>%
    relocate(intercept) %>%
    as.matrix()
  
  return(matrix)
}

# prep data with transformations and interactions
prep_data_ktc <- function(data){
  df <- data %>%
    select(historical_value, age, tva_adj) %>%
    mutate(
      x1_2 = historical_value^2,
      x2_2 = age^2,
      x1_x2 = historical_value*age,
      x2_x3 = age*tva_adj)
  
  matrix <- pmap(list(df, means, sds), function(df, means, sds){
    (df - means)/sds}) %>%
    bind_rows() %>%
    mutate(intercept = 1) %>%
    relocate(intercept) %>%
    as.matrix()
  
  return(matrix)
}

# uses hierarchical_tva.stan to estimate parameters
find_tva_parameters <- function(data){
  y <- data %>% select(tva_adj) %>% pull()
  N <- length(y)
  
  group <- data %>%
    transmute(position = as.factor(position) %>% as.numeric) %>%
    pull()
  J <- unique(group) %>% length()
  
  X <- data %>%
    prep_data_tva()
  
  K <- ncol(X)
  
  hierarchical_data <- list(N = N, K = K, J = J, X = X, y = y,
                            group = group,
                            N_new = N, X_new = X)
  
  hierarchical_fit <- stan(
    file = here(data_path, "hierarchical_tva.stan"),  # Path to Stan model file
    data = hierarchical_data,                         # Data list
    iter = 6000,                         # Number of iterations
    chains = 6,                          # Number of chains
    seed = 123)                          # Seed for reproducibility
  
  # errors, but at this point its good enough. All the individual players converge
  
  # h_summary <- summary(hierarchical_fit)
  
  # a <- h_summary$summary[, "Rhat"]
  # a[order(a)]
  
  # plot(hierarchical_fit)
  # pairs(hierarchical_fit, pars = c("beta", "sigma"))
  # traceplot(hierarchical_fit, pars = "sigma")
  
  posterior_samples <- rstan::extract(hierarchical_fit)
  
  return(posterior_samples)
}

# Predict Keep Trade Cut Value after season ends, uses hierarchical_tva.stan to estimate parameters
find_ktc_parameters <- function(data){
  min_ktc <- min(data$ktc_value, na.rm = TRUE)
  
  # fill in NA ktc values
  data <- data %>%
    rowwise() %>% 
    mutate(
      ktc_value = case_when(
        is.na(ktc_value) ~ runif(1, min = 0, max = min_ktc),
        .default = ktc_value)) %>%
    ungroup()
  
  y <- data %>% select(ktc_value) %>% pull()
  N <- length(y)
  
  group <- data %>%
    transmute(position = as.factor(position) %>% as.numeric) %>%
    pull()
  J <- unique(group) %>% length()
  
  X <- data %>%
    prep_data_ktc()
  
  K <- ncol(X)
  
  hierarchical_data <- list(N = N, K = K, J = J, X = X, y = y,
                            group = group,
                            N_new = N, X_new = X)
  
  hierarchical_fit <- stan(
    file = here(data_path, "hierarchical_ktc.stan"),  # Path to Stan model file
    data = hierarchical_data,                         # Data list
    iter = 6000,                         # Number of iterations
    chains = 6,                          # Number of chains
    seed = 123)                          # Seed for reproducibility
  
  # errors, but at this point its good enough. All the individual players converge
  
  # h_summary <- summary(hierarchical_fit)
  
  # a <- h_summary$summary[, "Rhat"]
  # a[order(a)]
  
  # plot(hierarchical_fit)
  # pairs(hierarchical_fit, pars = c("beta", "sigma"))
  # traceplot(hierarchical_fit, pars = "sigma")
  
  posterior_samples <- rstan::extract(hierarchical_fit)
  
  return(posterior_samples)
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

extract_new_samples(tva_parameter_values, updated_data %>% prep_data_tva(), constant_group)


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
      # return 0s to 0. Interpret these as essentially done
      value = if_else(value < 0, 0, value))
  
  # force max to 9999, allows for ktc value to drop, but not exceed
  if(max(ny_ktc) > 9999){
    new_ny_ktc <- new_ny_ktc %>% mutate(value = value * 9999/max(ny_ktc))}
  
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
