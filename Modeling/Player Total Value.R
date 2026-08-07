# Future Value and Total Value

suppressPackageStartupMessages({
  library("here")
  library("bundle")
})

message("begin computing Player Total Value...")
demonstrate_fit <- FALSE # set to true to rerun the model fit images
train_models <- FALSE # set to true to retrain BART models

source(here("Data Manipulation/Scrape Support.R")) # grab functions
source(here("Modeling/Player Total Value Functions.R")) # grab functions
season_value_added <- read_csv(here("Data/sva.csv"), show_col_types = FALSE) # shortcut
player_info <- read_csv(here("Data/player_info.csv"), show_col_types = FALSE) # shortcut
season_dates <- read_csv(here("Data/season_dates.csv"), show_col_types = FALSE)

ktc_list <- list.files(
  path = here("Data/ktc values"),
  full.names = T
) %>%
  set_names(basename(.)) |>
  map(~ read_csv(.x, show_col_types = FALSE))

# these are the names of the dudes that I'll compute the future value of repetitively
future_value_names <- map_dfr(ktc_list, name_correction) |>
  distinct(name) |>
  left_join(player_info, by = join_by(name)) |>
  select(-player_id) |>
  filter(
    !str_detect(name, c("Mid")),
    !str_detect(name, c("Early")),
    !str_detect(name, c("Late"))
  ) |>
  bind_rows(
    player_info |>
      filter(name == "Marshawn Lynch") |>
      select(name, position, birth_date, years_exp)
  ) |>
  filter(name %!in% c("Brendan Sorsby", "Trinidad Chambliss"))

write_csv(future_value_names, here("Data/future_value_names.csv"))

# organize data sets
ktc_begin_end_dates <- list(
  year1 = list(
    pre_ktc_date = ymd("2024-08-23"),
    post_ktc_date = ymd("2025-08-24")
  ),
  year2 = list(
    pre_ktc_date = ymd("2025-08-24"),
    post_ktc_date = ymd("2026-08-04")
  )
) # push back as far as possible

hktc_data <- map_dfr(
  ktc_begin_end_dates,
  ~ compile_training_data(
    ktc_list,
    player_info,
    pre_ktc_date = .x$pre_ktc_date,
    post_ktc_date = .x$post_ktc_date
  )
)

# hktc_data_list <- hktc_data |>
#   group_by(position) |>
#   reframe(position = list(tibble(name, historical_value, total_value_added, tva_adj, ktc_value, position, age))) |>
#   deframe()

# Model Total Value Added for next season-------------------------------------------------------------------------

# I use a BART (Bayesian Additive Regression Tree) Model

# means and standard deviations that I used to scale the predictors
tva_scales <- hktc_data |> compute_tva_scales()

# prep data
tva_data <- hktc_data |> prep_data_tva(tva_scales)

if (train_models) {
  # run model ~ 2.5 minutes
  tic()
  # tva_fit <- fit_bart(tva_data$full_data)
  tva_fit <- fit_bart(tva_data$train_data)
  toc()

  saveRDS(bundle(tva_fit), file = here("Modeling/tva_fit.rds"))

  # compute accuracy(RMSE)
  model_accuracy(tva_fit, tva_data$test_data)

  # graph residuals
  graph_residuals(tva_fit, tva_data$test_data)

  tva_samples <- generate_samples(tva_fit, tva_data$test_data)

  compute_coverage(tva_fit, tva_data$test_data, confidence = .95)
}


# Model KTC Value for next season -----------------------------------------

# means and standard deviations that I used to scale the predictors
ktc_scales <- hktc_data |> compute_ktc_scales()

# prep data
ktc_data <- hktc_data |> prep_data_ktc(ktc_scales)

if (train_models) {
  # run model ~2.5 minutes
  tic()
  # ktc_fit <- fit_bart(ktc_data$train_data)
  ktc_fit <- fit_bart(ktc_data$full_data)
  toc()

  saveRDS(bundle(ktc_fit), file = here("Modeling/ktc_fit.rds"))

  # compute accuracy(RMSE)
  model_accuracy(ktc_fit, ktc_data$test_data)

  # graph residuals
  graph_residuals(ktc_fit, ktc_data$test_data)

  ktc_samples <- generate_samples(ktc_fit, ktc_data$test_data)

  compute_coverage(ktc_fit, ktc_data$test_data, confidence = .95)
}

# Model Residuals --------------------------------------------------------
if (train_models) {
  tva_resid_fit <- model_residuals(tva_fit, tva_data$full_data)
  saveRDS(tva_resid_fit, file = here("Modeling/tva_resid_fit.rds"))

  ktc_resid_fit <- model_residuals(ktc_fit, ktc_data$full_data)
  saveRDS(ktc_resid_fit, file = here("Modeling/ktc_resid_fit.rds"))
}


# Load Models -------------------------------------------------------------

tva_fit <- readRDS(here("Modeling/tva_fit.rds")) |> unbundle()
tva_resid_fit <- readRDS(here("Modeling/tva_resid_fit.rds"))

ktc_fit <- readRDS(here("Modeling/ktc_fit.rds")) |> unbundle()
ktc_resid_fit <- readRDS(here("Modeling/ktc_resid_fit.rds"))

# Run Player Intervals ----------------------------------------------------

ktc_tibble <- ktc_list |>
  bind_rows(.id = "date") |>
  mutate(
    date = str_remove(date, "ktc_value") |>
      str_remove(".csv") |>
      lubridate::mdy(),
    ktc_value = coalesce(ktc_value, value)
  ) |>
  select(-value) |>
  name_correction()

# compute future value over time

# origin data set, set at beginning of last year
sim_df <- ktc_tibble |>
  slice_max(order_by = date, with_ties = TRUE) |>
  select(-date) |>
  compile_data_set(
    future_value_names,
    today(),
    max(season_dates$season_start),
    max(season_dates$season_end)
  )

message(
  "There are ",
  sim_df |> filter(is.na(birth_date)) |> nrow(),
  " missing players"
)

sim_df <- sim_df |>
  filter(!is.na(birth_date))

diff <- (sim_df |> nrow()) - (sim_df |> drop_na() |> nrow())
if (diff != 0) {
  message("Warning: ", diff, " missing players")
}

# ~3 mins
player_simulations <- next_years(
  origin_data = sim_df,
  n_years = 10,
  tva_scales = tva_scales,
  ktc_scales = ktc_scales,
  tva_fit = tva_fit,
  ktc_fit = ktc_fit,
  tva_resid_fit = tva_resid_fit,
  ktc_resid_fit = ktc_resid_fit
)

save(player_simulations, file = here("Modeling/player_simulations.RData"))

# Future Value over Time --------------------------------------------------
# goal is to only run one at a time, while keeping the previous models

# future_value_time <- read_csv(
#   here("Shiny/Saved Files/future_value_time.csv"),
#   show_col_types = FALSE
# ) |>
#   filter(date != today())

# last_date_fvt <- read_csv(
#   here("Data/last_date_fvt.csv"),
#   show_col_types = FALSE
# ) |>
#   pull(value)

future_value_time <- tibble()
last_date_fvt <- ymd("20240101")

message("begin mapping future value over time...")

while (last_date_fvt < max(ktc_tibble$date)) {
  keep_trade_cut <- ktc_tibble |>
    filter(date > last_date_fvt) |>
    slice_min(order_by = date, with_ties = TRUE)

  current_date <- keep_trade_cut$date[1]
  message("starting ", current_date)

  # can't figure out how to parallelize this. Takes ~ 4 minutes for one run
  future_value_time <- future_value_over_time(
    future_value_names,
    keep_trade_cut,
    date = current_date,
    tva_scales,
    ktc_scales,
    tva_fit,
    ktc_fit,
    tva_resid_fit,
    ktc_resid_fit,
    season_dates
  ) |>
    bind_rows(future_value_time)

  last_date_fvt <- max(future_value_time$date) |>
    as_tibble()
}

# coarse protection against double Antonio Williams
future_value_time <- future_value_time |>
  group_by(name, date) |>
  summarize(future_value = max(future_value, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(date), desc(future_value))

# make list of the dates already computed, so I don't have to compute them again
write_csv(last_date_fvt, here("Data/last_date_fvt.csv"))

write_csv(future_value_time, here("Shiny/Saved Files/future_value_time.csv"))

# ensure future value is the same as most recent future_value_over_time

player_total_value <- future_value_time |>
  filter(date == max(date)) |>
  full_join(
    season_value_added |>
      select(name, season, total_value_added) |>
      pivot_wider(
        names_from = season,
        values_from = total_value_added,
        names_prefix = "sva_"
      ),
    by = join_by(name)
  ) |>
  select(name, contains("sva"), future_value) |>
  left_join(
    # coarse protection against double Antonio Williams
    filter(player_info, !(name == "Antonio Williams" & position == "RB")),
    by = join_by(name)
  ) |>
  left_join(select(keep_trade_cut, -date), by = join_by(name)) |>
  mutate(
    across(contains("sva"), ~ replace_na(., 0)),
    ktc_value = case_when(
      position %in% c("K", "DST") ~ 0,
      .default = ktc_value
    ),
    future_value = case_when(
      position %in% c("K", "DST") ~ 0,
      is.na(future_value) ~ 0,
      .default = future_value
    )
  ) |>
  arrange(desc(future_value)) |>
  select(
    name,
    player_id,
    birth_date,
    position,
    ktc_value,
    contains("sva"),
    future_value
  )

write_csv(player_total_value, here("Data/player_total_value.csv"))

# Demonstrate Model Fit ----------------------------------------------------
if (demonstrate_fit) {
  # tva
  toy_tva_data <- tibble(
    position = c(
      rep("QB", 1200),
      rep("RB", 1200),
      rep("WR", 1200),
      rep("TE", 1200)
    ),
    age = rep(
      c(
        rep(22, 100),
        rep(23, 100),
        rep(24, 100),
        rep(25, 100),
        rep(26, 100),
        rep(27, 100),
        rep(28, 100),
        rep(29, 100),
        rep(30, 100),
        rep(31, 100),
        rep(32, 100),
        rep(33, 100)
      ),
      4
    ),
    historical_value = rep(seq(from = 100, to = 10000, length.out = 100), 48),
    tva_adj = NA
  )

  prep_toy_tva_data <- toy_tva_data |> prep_data_tva(tva_scales)

  tic()
  toy_tva_quantiles <- generate_samples(tva_fit, prep_toy_tva_data$full_data) |>
    compute_quantiles(tva_resid_fit, prep_toy_tva_data$full_data)
  toc()

  toy_tva_plot_data <- toy_tva_data |>
    bind_cols(as_tibble(t(toy_tva_quantiles))) |>
    mutate(age = factor(age)) |>
    rename(
      median_tva = "V20",
      q05 = "V2",
      q10 = "V4",
      q90 = "V36",
      q95 = "V38"
    ) |>
    select(historical_value, age, position, median_tva, q05, q10, q90, q95)

  toy_tva_plot_data |>
    ggplot() +
    geom_line(aes(x = historical_value, y = median_tva, color = position)) +
    geom_ribbon(
      aes(x = historical_value, ymin = q10, ymax = q90, fill = position),
      alpha = 0.2
    ) +
    facet_wrap(~age, nrow = 3) +
    labs(x = "KeepTradeCut", y = "Season Value Added") +
    theme(
      axis.text.x = element_text(angle = 30, vjust = 1.25, hjust = 1),
      legend.title = element_blank()
    )

  # ktc
  toy_ktc_data <- tibble(
    position = c(
      rep("QB", 24000),
      rep("RB", 24000),
      rep("WR", 24000),
      rep("TE", 24000)
    ),
    age = rep(
      c(
        rep(23, 2000),
        rep(24, 2000),
        rep(25, 2000),
        rep(26, 2000),
        rep(27, 2000),
        rep(28, 2000),
        rep(29, 2000),
        rep(30, 2000),
        rep(31, 2000),
        rep(32, 2000),
        rep(33, 2000),
        rep(34, 2000)
      ),
      4
    ),
    historical_value = rep(seq(from = 100, to = 10000, length.out = 100), 960),
    tva_adj = rep(
      c(
        rep(-28, 100),
        rep(-16, 100),
        rep(-4, 100),
        rep(8, 100),
        rep(20, 100),
        rep(32, 100),
        rep(44, 100),
        rep(56, 100),
        rep(68, 100),
        rep(80, 100),
        rep(92, 100),
        rep(104, 100),
        rep(116, 100),
        rep(128, 100),
        rep(140, 100),
        rep(152, 100),
        rep(164, 100),
        rep(176, 100),
        rep(188, 100),
        rep(200, 100)
      ),
      48
    ),
    ktc_value = 0
  )

  prep_toy_ktc_data <- prep_data_ktc(toy_ktc_data, ktc_scales)

  tic()
  toy_ktc_quantiles <- generate_samples(ktc_fit, prep_toy_ktc_data$full_data) |>
    compute_quantiles(ktc_resid_fit, prep_toy_ktc_data$full_data)
  toc()

  toy_ktc_plot_data <- toy_ktc_data |>
    bind_cols(as_tibble(t(toy_ktc_quantiles))) |>
    mutate(age = factor(age)) |>
    rename(
      median_ktc = "V20",
      q05 = "V2",
      q10 = "V4",
      q90 = "V36",
      q95 = "V38"
    ) |>
    select(
      historical_value,
      age,
      position,
      tva_adj,
      median_ktc,
      q05,
      q10,
      q90,
      q95
    ) |>
    filter(historical_value == 5000)

  toy_ktc_plot_data |>
    ggplot() +
    geom_line(aes(x = tva_adj, y = median_ktc, color = position)) +
    geom_ribbon(
      aes(x = tva_adj, ymin = q10, ymax = q90, fill = position),
      alpha = 0.2
    ) +
    facet_wrap(~age, nrow = 3) +
    labs(x = "Total Value Added", y = "Post-season KeepTradeCut") +
    coord_cartesian(ylim = c(0, 10000)) +
    theme(legend.title = element_blank())

  write_csv(toy_tva_plot_data, here("Data/toy_tva_plot_data.csv"))
  write_csv(toy_ktc_plot_data, here("Data/toy_ktc_plot_data.csv"))
}
