# Estimate Rookie Draft Slot Value (from previous years)
# one downside of this is we can't actually peer into the future and see
# how good the rookies are, but we can see how good previous rookies turned out to be one year later

library("here")

# load MCMC Samplers
source(here("Modeling/MCMC Samplers.R"))
source(here("Modeling/Player Total Value Functions.R"))

# load data of interest
load(here("Data/draft_picks.RData"))
load(here("Modeling/player_simulations.RData"))
player_total_value <- read_csv(here("Data/player_total_value.csv")) %>%
  mutate(
    total_value = sva_2024 + future_value*.95, # devalue the future
    player_id = as.character(player_id)) %>%
  select(name, player_id, position, total_value, sva_2024, contains("ny"))
player_info <- read_csv(here("Data/player_info.csv"))

# future value projections of players
future_value_projections <- imap(1:3, ~{
  df <- player_simulations[[.x]] %>% #throw all quantiles into data (will slightly underestimate variance but oh well)
    pivot_longer(cols = contains("proj_tva"), names_to = "quantile", values_to = "proj_tva") %>%
    select(name, proj_tva)
  colnames(df) <- c("name", paste0("sva_", names(player_simulations)[.x]))
  df
}) %>% bind_cols() %>%
  rename("name" = name...1) %>%
  select(name, contains("sva"))

# find value of all players in rookie drafts
rookie_drafts <- bind_rows(draft_picks, .id = "draft_id") %>%
  group_by(draft_id) %>%
  mutate(
    max_rounds = max(round),
    season = if_else(draft_id == 1, as.numeric(draft_id) + 2023, as.numeric(draft_id) + 2022)) %>%
  ungroup() %>%
  # only keep rookie drafts (for now)
  filter(max_rounds == 3) %>%
  select(pick_no, roster_id, player_id, season) %>%
  left_join(player_info, by = join_by(player_id)) %>%
  select(season, pick_no, roster_id, name, player_id, position) %>%
  left_join(player_total_value %>% select(-player_id, -position), by = join_by(name)) %>%
  mutate(
    total_value = replace_na(total_value, 0),
    sva_2024 = replace_na(sva_2024, 0)) %>%
  left_join(future_value_projections, by = join_by(name))

rookie_drafts %>%
  ggplot() +
  geom_point(aes(x = pick_no, y = total_value, color = as.factor(season)))

# Polynomial Regression pick on total value ---------------------------------------------------
X_tv <- rookie_drafts %>%
  distinct(season, pick_no, name, total_value) %>%
  select(pick_no) %>%
  mutate(
    # pn_1 = pick_no^(.25),
    pn_2 = pick_no^(.5),
    # pn_3 = pick_no^(.75),
    across(everything(), ~scale(.x))) %>%
  mutate(intercept = 1) %>%
  as.matrix()

Y_tv <- rookie_drafts %>% distinct(season, pick_no, name, total_value) %>% pull(total_value)
Y_tv/100

tic()
samples_tv <- het_reg_mh_sampler(Y = Y_tv, X = X_tv, new_X = X_tv[c(1:36),],
                             iter = 50000, thin = 5, burn_in = 3000, proposal_sd = c(.1, .1, .5))
toc()

# quantiles of each pick
quantiles_tv <- apply(samples_tv$new_y, 2, quantile, probs = seq(.025, .975, by = .025))

# plot fit, looks pretty good
tibble(.pred = quantiles_tv[20,],
       lower = quantiles_tv[1,],
       upper = quantiles_tv[39,],
       pick_no = c(1:36)) %>%
  right_join(rookie_drafts, by = join_by(pick_no)) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = total_value)) +
  geom_line(aes(y = .pred)) +
  geom_errorbar(aes(ymin = lower, ymax = upper))

# plot residuals against pick_no, looks ok enough
tibble(.pred = quantiles_tv[20,],
       pick_no = c(1:36)) %>%
  right_join(rookie_drafts, by = join_by(pick_no)) %>%
  mutate(.resid = total_value - .pred) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)

# Polynomial Regression - Rookie Year Value Added ---------------------------

# this is more complicated, because one year ahead differs for different players
# I will need to adapt this in future years
ny_data <- rookie_drafts %>%
  mutate(first_year = case_when(
    season == 2024 ~ sva_2024,
    season == 2025 ~ sva_2025,
    .default = NA
  )) %>%
  select(pick_no, first_year) %>%
  slice_sample(n = 800, replace = FALSE) # takes a long time if n is too large

X_ny <- ny_data %>% select(pick_no) %>%
  mutate(
    # pn_1 = pick_no^(.25),
    pn_2 = pick_no^(.5),
    # pn_3 = pick_no^(.75),
    across(everything(), ~scale(.x))) %>%
  mutate(intercept = 1) %>%
  as.matrix()

Y_ny <- ny_data$first_year

tic() #note that X_tv[c(1:36)] can actually stay the same
samples_ny <- het_reg_mh_sampler(Y = Y_ny, X = X_ny, new_X = X_tv[c(1:36),],
                                 iter = 50000, thin = 5, burn_in = 3000, proposal_sd = c(.1, .1, .5))
toc()

# quantiles of each pick
quantiles_ny <- apply(samples_ny$new_y, 2, quantile, probs = seq(.025, .975, by = .025))

# plot fit, looks pretty good
tibble(.pred = quantiles_ny[20,],
       lower = quantiles_ny[1,],
       upper = quantiles_ny[39,],
       pick_no = c(1:36)) %>%
  right_join(ny_data, by = join_by(pick_no)) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = first_year)) +
  geom_line(aes(y = .pred)) +
  geom_errorbar(aes(ymin = lower, ymax = upper))

# plot residuals against pick_no, looks ok enough
tibble(.pred = quantiles_ny[20,],
       pick_no = c(1:36)) %>%
  right_join(ny_data, by = join_by(pick_no)) %>%
  mutate(.resid = first_year  - .pred) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)

# Polynomial Regression - 2nd Year Value Added -----------------------------
# this is kinda stretching it. Using draft pick to predict the bayesian hierarchical model's
# prediction for va in the following year.
# this is essentially a proxy for predicting output in 2 years with draft slot

# this is kinda confusing, from perspective of rookie_drafts, ny is two years after drafted
ny2_data <- rookie_drafts %>%
  mutate(second_year = case_when(
    season == 2024 ~ sva_2025,
    season == 2025 ~ sva_2026,
    .default = NA)) %>%
  select(pick_no, second_year) %>%
  slice_sample(n = 800, replace = FALSE) # it gets antsy if n is too large, lots of big inversions

X_ny2 <- ny2_data %>% select(pick_no) %>%
  mutate(
    # pn_1 = pick_no^(.25),
    pn_2 = pick_no^(.5),
    # pn_3 = pick_no^(.75),
    across(everything(), ~scale(.x))) %>%
  mutate(intercept = 1) %>%
  as.matrix()

Y_ny2 <- ny2_data$second_year

tic() #note that X_tv[c(1:36)] can actually stay the same
samples_ny2 <- het_reg_mh_sampler(Y = Y_ny2, X = X_ny2, new_X = X_tv[c(1:36),],
                                  iter = 50000, thin = 5, burn_in = 3000, proposal_sd = c(.1, .1, .5))
toc()

# quantiles of each pick
quantiles_ny2 <- apply(samples_ny2$new_y, 2, quantile, probs = seq(.025, .975, by = .025))

# plot fit, looks pretty good
tibble(.pred = quantiles_ny2[20,],
       lower = quantiles_ny2[1,],
       upper = quantiles_ny2[39,],
       pick_no = c(1:36)) %>%
  right_join(ny2_data, by = join_by(pick_no)) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = second_year)) +
  geom_line(aes(y = .pred))

# plot residuals against pick_no, looks ok enough
tibble(.pred = quantiles_ny2[20,],
       pick_no = c(1:36)) %>%
  right_join(ny2_data, by = join_by(pick_no)) %>%
  mutate(.resid = second_year  - .pred) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)

# Polynomial Regression - 3 Years Value Added -----------------------------

# this is kinda confusing, from perspective of rookie_drafts, ny2 is three years after drafted
ny3_data <- rookie_drafts %>%
  mutate(third_year = case_when(
    season == 2024 ~ sva_2026,
    season == 2025 ~ sva_2027,
    .default = NA)) %>%
  select(pick_no, third_year) %>%
  slice_sample(n = 800, replace = FALSE) # it gets antsy if n is too large

X_ny3 <- ny3_data %>% select(pick_no) %>%
  mutate(
    # pn_1 = pick_no^(.25),
    pn_2 = pick_no^(.5),
    # pn_3 = pick_no^(.75),
    across(everything(), ~scale(.x))) %>%
  mutate(intercept = 1) %>%
  as.matrix()

Y_ny3 <- ny3_data$third_year

tic() #note that X_tv[c(1:36)] can actually stay the same
samples_ny3 <- het_reg_mh_sampler(Y = Y_ny2, X = X_ny2, new_X = X_tv[c(1:36),],
                                  iter = 50000, thin = 5, burn_in = 3000, proposal_sd = c(.1, .1, .5))
toc()

# quantiles of each pick
quantiles_ny3 <- apply(samples_ny3$new_y, 2, quantile, probs = seq(.025, .975, by = .025))

# plot fit, looks pretty good
tibble(.pred = quantiles_ny3[20,],
       lower = quantiles_ny3[1,],
       upper = quantiles_ny3[39,],
       pick_no = c(1:36)) %>%
  right_join(ny3_data, by = join_by(pick_no)) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = third_year)) +
  geom_line(aes(y = .pred))

# plot residuals against pick_no, looks ok enough
tibble(.pred = quantiles_ny3[20,],
       pick_no = c(1:36)) %>%
  right_join(ny3_data, by = join_by(pick_no)) %>%
  mutate(.resid = third_year  - .pred) %>%
  ggplot(aes(x = pick_no)) +
  geom_point(aes(y = .resid)) +
  geom_hline(yintercept = 0)

# Write to CSV ------------------------------------------------------------

quantiles_list <- list("total value" = quantiles_tv, "first year" = quantiles_ny,
                       "second year" = quantiles_ny2, "third_year" = quantiles_ny3)

rookie_draft_values <- imap_dfr(1:4, ~{
  df <- t(quantiles_list[[.x]]) %>% as_tibble()
  colnames(df) <- paste0("proj_tva_", seq_along(colnames(df))*2.5)
  
  df %>%
    mutate(
      pick_no = c(1:36),
      metric = names(quantiles_list)[.x])}) %>%
  relocate(pick_no, metric)

# write_csv(rookie_draft_values, here("Data/rookie_draft_values.csv"))




