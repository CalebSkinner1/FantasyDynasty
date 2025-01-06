# Future Value and Total Value
library("rstan")
library("here")
data_path <- "FantasyDynasty/"

source(here(data_path, "Player Value Added.R")) # grab value added

# organize data sets

historical_ktc <- read_csv(here(data_path,"ktc_value082324")) %>%
  filter(!str_detect(name, "Early"), !str_detect(name, "Mid"), !str_detect(name, "Late")) %>%
  name_correction() %>%
  group_by(name) %>%
  summarize(value = max(value)) %>%
  arrange(desc(value))

hktc_data <- historical_ktc %>%
  rename("historical_value" = "value") %>%
  left_join(season_value_added, by = join_by(name)) %>%
  select(-position, -total_points) %>%
  mutate(
    total_value_added = replace_na(total_value_added, 0),
    # adjust total value added to account for two missing games
    tva_adj = case_when(
      total_value_added < 0 ~ total_value_added,
      .default = total_value_added * 17/max(sleeper_points$week))) %>%
  left_join(keep_trade_cut, by = join_by(name)) %>%
  rename("ktc_value" = value) %>%
  left_join(player_info, by = join_by(name)) %>%
  select(-player_id) %>%
  mutate(
    age = interval(birth_date, ymd("2024-08-23"))/years(1))

hktc_data_list <- hktc_data %>%
  group_by(position) %>%
  reframe(position = list(tibble(name, historical_value, total_value_added, tva_adj, ktc_value, position, age))) %>%
  deframe()

# Bayesian methodology, using hierarchical polynomial regression
scale_this <- function(x) as.vector(scale(x))

# First, create polynomial regression model for historical value and age on
# total value added for next season

# Predict Total Value Added for next season

# runs bayesian hierarchical model to find parameters for
# total value added polynomial regression model
prep_data_tva <- function(data){
  matrix <- data %>%
    select(historical_value, age) %>%
    mutate(
      x1_2 = historical_value^2,
      x2_2 = age^2,
      x1_x2 = historical_value*age,
      across(everything(), ~.x %>% scale_this()),
      intercept = 1) %>%
    relocate(intercept) %>%
    as.matrix()
  
  return(matrix)
}

prep_data_ktc <- function(data){
  matrix <- data %>%
    select(historical_value, age, tva_adj) %>%
    mutate(
      x1_2 = historical_value^2,
      x2_2 = age^2,
      x1_x2 = historical_value*age,
      x2_x3 = age*tva_adj,
      across(everything(), ~.x %>% scale_this()),
      intercept = 1) %>%
    relocate(intercept) %>%
    as.matrix()
  
  return(matrix)
}

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

# tva model parameter values sample
tva_parameter_values <- find_tva_parameters(hktc_data)

# Predict Keep Trade Cut Value after season ends
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
    select(historical_value, age, tva_adj) %>%
    mutate(
      x1_2 = historical_value^2,
      x2_2 = age^2,
      x1_x2 = historical_value*age,
      x2_x3 = age*tva_adj,
      across(everything(), ~.x %>% scale_this()),
      intercept = 1) %>%
    relocate(intercept) %>%
    as.matrix()
  
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

# ktc model parameter values sample
ktc_parameter_values <- find_ktc_parameters(hktc_data)

# draw a new y value from the model with certain beta and sigma values
draw_new_y <- function(beta_samples, sigma_samples, X_new, group) {
  # Choose a random sample of parameters from the posterior
  beta_sample <- sample(beta_samples[group], 1)
  sigma_sample <- sample(sigma_samples, 1)
  
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

# projected tva values for 2025 season

# this value is the group (position) for each player in the data set
constant_group <- hktc_data %>%
  transmute(position = as.factor(position) %>% as.numeric) %>%
  pull()

next_year <- function(data){
  
}

tva_2025 <- hktc_data %>%
  # assign random ktc value to players with super low ktc value
  rowwise() %>% 
  mutate(
    ktc_value = case_when(
      is.na(ktc_value) ~ runif(1, min = 0, max = min_ktc),
      .default = ktc_value)) %>%
  ungroup() %>%
  mutate(historical_value = ktc_value,
         age = age + 1) %>%
  prep_data_tva() %>%
  extract_new_samples(tva_parameter_values, ., constant_group)

# project ktc values for after 2025 season
ktc_2025 <- hktc_data %>%
  # assign random ktc value to players with super low ktc value
  rowwise() %>% 
  mutate(
    ktc_value = case_when(
      is.na(ktc_value) ~ runif(1, min = 0, max = min_ktc),
      .default = ktc_value)) %>%
  ungroup() %>%

# projected tva values
extract_new_samples(ktc_parameter_values, X, constant_group)





# Older Methods --------------------------------------------------------

# non hierarchical - estimates are much much worse

# y <- hktc_data_list[[1]] %>% select(tva_adj) %>% pull()
# N <- length(y)
# K <- 6
# 
# X <- hktc_data_list[[1]] %>%
#   select(historical_value, age) %>%
#   mutate(
#     x1_2 = historical_value^2,
#     x2_2 = age^2,
#     x1_x2 = historical_value*age,
#     across(everything(), ~.x %>% scale_this()),
#     intercept = 1) %>%
#   relocate(intercept) %>%
#   as.matrix()
# 
# polynomial_data <- list(N = N, K = K, X = X, y = y,
#                         N_new = N, X_new = X)
# 
# 
# 
# polynomial_fit <- stan(
#   file = here(data_path, "polynomial.stan"),  # Path to Stan model file
#   data = polynomial_data,                         # Data list
#   iter = 4000,                         # Number of iterations
#   chains = 4,                          # Number of chains
#   seed = 123)                          # Seed for reproducibility
# 
# 
# polynomial_fit


# # # # # # # # # # # # # # # #

# frequentist

# lm_spec <- linear_reg() %>%
#   set_mode("regression") %>%
#   set_engine("lm")
# 
# compute_fit <- function(data, recipe){
#   wf <- workflow() %>%
#     add_model(lm_spec) %>%
#     add_recipe(recipe)
#   
#   fit(wf, data = data)
# }
# 
# recipe <- recipe(tva_adj ~ age + historical_value, data = hktc_data_list[[1]]) %>%
#   step_poly(historical_value, degree = 2) %>%
#   step_poly(age, degree = 2)
# 
# visualize_fit <- function(data, fit, range, quantity){
#   quantities <- fit$pre$actions$recipe$recipe$template %>% colnames()
#   
#   regression_lines <- bind_cols(
#     augment(fit, new_data = range),
#     predict(fit, new_data = range, type = "conf_int"))
#   
#   data %>%
#     select(all_of(quantities)) %>%
#     rename_with(~paste0("X"), last_col()) %>%
#     mutate(age = round(age, digits = 0)) %>%
#     ggplot(aes(historical_value, X)) +
#     facet_wrap(~age) +
#     geom_point(color = "cadetblue3") +
#     geom_line(aes(y = .pred), color = "darkgreen",
#               data = regression_lines) +
#     geom_line(aes(y = .pred_lower), data = regression_lines, 
#               linetype = "dashed", color = "indianred4") +
#     geom_line(aes(y = .pred_upper), data = regression_lines, 
#               linetype = "dashed", color = "indianred4") +
#     labs(y = quantity, x = "Historical Value")
# }
# fit_viz <- function(data, short_quantity, quantity, type, hv_degree = 3, age_degree = 2,
#                     knots = list(knots = 2500, 5000, 7500)){
#   formula <- as.formula(str_c(short_quantity, " ~ age + historical_value"))
#   
#   # recipe
#   if(type == "poly"){
#     recipe <- recipe(formula, data = data) %>%
#       step_poly(historical_value, degree = hv_degree) %>%
#       step_poly(age, degree = age_degree)
#   }else if(type == "spline"){
#     recipe <- recipe(formula, data = data) %>%
#       step_bs(historical_value, options = knots) %>%
#       step_bs(age)}
#   
#   # value range for new data set
#   value_range <- crossing(historical_value = seq(min(data$historical_value),
#                                                  max(data$historical_value), length.out = 500),
#                           age = seq(round(min(data$age), 0), round(max(data$age), 0)))
#   
#   fit <- data %>% compute_fit(recipe)
#   
#   p <- data %>% visualize_fit(fit, value_range, quantity)
#   
#   return(list(p, fit))}
# 
# 
# # Goal 1: use historical ktc to predict season value added
# pred_va <- map(hktc_data_list, ~fit_viz(.x, "tva_adj", "Total Value Added (Adj)", "poly", age_degree = 3, hv_degree = 2)) #slightly better
# # map(hktc_data_list, ~fit_viz(.x, "tva_adj", "Total Value Added (Adj)", "spline"))
# 
# # Goal 2: use historical ktc to predict current ktc
# pred_ktc <- map(hktc_data_list, ~fit_viz(.x, "ktc_value", "Current KTC Value", "poly", age_degree = 3)) #slightly better
# # map(hktc_data_list, ~fit_viz(.x, "ktc_value", "Current KTC Value", "spline"))
# 
# # Second stage
# # simulate 5 years and average
# # goal is to convert current ktc value into estimated future points
# # don't really care about variance quantification, so for now, just take expectations.
# # In future... definitely need to draw samples
# 
# # expected values for next year
# exp_next_year <- function(player_data, fit){
#   pos0 <- player_data %>% select(position) %>% pull()
#   
#   pos <- case_when(
#     pos0 == "QB" ~ 1,
#     pos0 == "RB" ~ 2,
#     pos0 == "TE" ~ 3,
#     pos0 == "WR" ~ 4,
#     .default = NA)
#   
#   player_data %>%
#     rename_with(~paste0("historical_value"), contains("ktc")) %>%
#     select(historical_value, age) %>%
#     augment(fit[[pos]][[2]], new_data = .) %>%
#     select(.pred)
# }
# 
# complete_next_year <- function(player_data){
#   year <- player_data %>%
#     mutate(year = year(birth_date + dyears(age)) + 1) %>%
#     pull()
#   
#   #first compute next year eva
#   eva <- player_data %>% exp_next_year(pred_va) %>%
#     rename_with(~paste0("eva_", year))
#   
#   #next compute next year value
#   ektc <- player_data %>% exp_next_year(pred_ktc) %>%
#     rename_with(~paste0("ektc_", year))
#   
#   # create tibble of estimated values for next year
#   player_data %>%
#     select(name, position, birth_date, age) %>%
#     bind_cols(eva, ektc) %>%
#     mutate(age = age + 1)
# }
# 
# # list of the data by player name
# hktc_ln <- hktc_data %>%
#   distinct() %>%
#   group_by(name) %>%
#   reframe(
#     name = list(tibble(name, ktc_value, position, birth_date, age))) %>%
#   deframe()
# # here!
# map(hktc_ln, complete_next_year) %>%
#   map(., complete_next_year)
# 
# hktc_data %>% filter(name == "Cooper Kupp")
# 
# hktc_data %>% exp_next_year("Cooper Kupp", pred_va)
# 
# hktc_data %>% exp_next_year("Cooper Kupp", pred_ktc)







