# Future Value and Total Value

suppressPackageStartupMessages({
  library("here")
  library("bundle")
})

message("begin computing Player Total Value...")
demonstrate_fit <- FALSE # set to true to rerun the model fit images
train_models <- getOption("fantasy.train_models", FALSE) # set to true to refit the BART models (and residual models)
tune_models <- getOption("fantasy.tune_models", FALSE) # with train_models, also re-tune hyperparameters (slow, offseason)
model_seed <- 123 # seed for cross validation folds and the BART samplers

set.seed(model_seed)

source(here("Data Manipulation/Scrape Support.R")) # grab functions
source(here("Modeling/Player Total Value Functions.R")) # grab functions
season_value_added <- read_csv(
  here("Shiny/Saved Files/season_value_added.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
player_info <- read_csv(
  here("Data/raw/player_info.csv"),
  show_col_types = FALSE,
  col_types = cols(player_id = col_character())
)
season_dates <- read_csv(here("Shiny/Saved Files/season_dates.csv"), show_col_types = FALSE)

ktc_list <- list.files(
  path = here("Data/raw/ktc"),
  full.names = T
) %>%
  set_names(basename(.)) |>
  map(
    ~ read_csv(
      .x,
      show_col_types = FALSE,
      col_types = cols(player_id = col_character())
    )
  )

# players to value: everyone KeepTradeCut has ranked (matched to a Sleeper id), plus
# every player who has been on a roster or in a transaction in this league. Players
# without a KTC value (e.g. Marshawn Lynch) get a future value of 0, and KTC entries
# without a Sleeper id (draft picks, college players) are left out.
ktc_player_ids <- ktc_list |>
  map(\(k) k$player_id) |>
  unlist()

league_transactions <- local({
  transactions <- readRDS(here("Data/derived/transactions.rds"))
  transactions
})

league_player_ids <- c(
  read_csv(
    here("Data/derived/va.csv"),
    show_col_types = FALSE,
    col_types = cols(player_id = col_character())
  )$player_id,
  read_csv(
    here("Data/derived/current_roster.csv"),
    show_col_types = FALSE,
    col_types = cols(player_id = col_character())
  )$player_id,
  names(league_transactions$adds),
  names(league_transactions$drops)
)

future_value_names <- player_info |>
  filter(
    player_id %in% c(ktc_player_ids, league_player_ids),
    position %in% c("QB", "RB", "WR", "TE")
  ) |>
  distinct(player_id, .keep_all = TRUE) |>
  select(name, player_id, position, birth_date, years_exp)

write_csv(future_value_names, here("Data/model_output/future_value_names.csv"))

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
    season_value_added,
    pre_ktc_date = .x$pre_ktc_date,
    post_ktc_date = .x$post_ktc_date
  )
)

# Model Total Value Added for next season ----------------------------------
# I use a BART (Bayesian Additive Regression Tree) Model

# means and standard deviations that I used to scale the predictors
tva_scales <- hktc_data |> compute_tva_scales()

# prep data
tva_data <- hktc_data |> prep_data_tva(tva_scales)

# Model KTC Value for next season -----------------------------------------

# means and standard deviations that I used to scale the predictors
ktc_scales <- hktc_data |> compute_ktc_scales()

# prep data
ktc_data <- hktc_data |> prep_data_ktc(ktc_scales)

# Train Models ------------------------------------------------------------
# Both models are fit on all of the data. Hyperparameters are re-tuned only when
# tune_models is TRUE (10-fold racing, ~minutes); otherwise the saved ones are reused
# and each fit takes seconds. The residual (variance) models use out-of-fold
# predictions, which also give honest accuracy and coverage numbers.

if (train_models) {
  for (model_name in c("tva", "ktc")) {
    model_data <- if (model_name == "tva") tva_data else ktc_data

    params <- if (tune_models) {
      tic(str_c("tuning ", model_name))
      tuned <- tune_bart(model_data, model_seed) |> save_params(model_name)
      toc()
      tuned
    } else {
      load_params(model_name)
    }
    message(
      model_name,
      " hyperparameters: ",
      str_flatten_comma(str_c(names(params), " = ", signif(unlist(params), 3)))
    )

    model_fit <- fit_bart(model_data, params, model_seed)
    saveRDS(
      bundle(model_fit),
      file = here("Modeling", "models", str_c(model_name, "_fit.rds"))
    )

    oof_pred <- oof_predictions(model_data, params, model_seed)
    resid_fit <- model_residuals(model_data, oof_pred)
    saveRDS(
      resid_fit,
      file = here("Modeling", "models", str_c(model_name, "_resid_fit.rds"))
    )

    report_fit(str_to_upper(model_name), model_data, oof_pred, resid_fit)
  }
  rm(model_name, model_data, params, model_fit, oof_pred, resid_fit)
}

# Load Models -------------------------------------------------------------

tva_fit <- readRDS(here("Modeling/models/tva_fit.rds")) |> unbundle()
tva_resid_fit <- readRDS(here("Modeling/models/tva_resid_fit.rds"))

ktc_fit <- readRDS(here("Modeling/models/ktc_fit.rds")) |> unbundle()
ktc_resid_fit <- readRDS(here("Modeling/models/ktc_resid_fit.rds"))

# Run Player Intervals ----------------------------------------------------

ktc_tibble <- ktc_list |>
  bind_rows(.id = "date") |>
  mutate(
    date = str_extract(date, "\\d{4}-\\d{2}-\\d{2}") |>
      lubridate::ymd()
  ) |>
  filter(!is.na(player_id)) |>
  name_correction()

# latest KTC values (the file Scraping.R saved, or the last good one)
keep_trade_cut <- ktc_tibble |>
  slice_max(order_by = date, with_ties = TRUE) |>
  distinct(player_id, .keep_all = TRUE)

# compute future value over time

models <- list(
  tva_scales = tva_scales,
  ktc_scales = ktc_scales,
  tva_fit = tva_fit,
  ktc_fit = ktc_fit,
  tva_resid_fit = tva_resid_fit,
  ktc_resid_fit = ktc_resid_fit
)

# today's values: during the season both the current-season and next-season paths
# are simulated and blended (see Season Context in the functions file); in the
# offseason only the next-season path is needed
today_value <- future_value_at_date(
  future_value_names,
  keep_trade_cut |> select(-date),
  today(),
  season_dates,
  models,
  n_years = 10
)

message(
  "season ",
  today_value$context$season,
  " is ",
  scales::percent(today_value$context$p, accuracy = 1),
  " complete"
)

# share of the season elapsed (p) as used above, saved so the app's scripts split the
# current season the same way future value does
today_value$context |>
  as_tibble() |>
  mutate(date = today()) |>
  write_csv(here("Data/model_output/season_context.csv"))

n_incomplete <- today_value$future_value |>
  filter(is.na(future_value)) |>
  nrow()
if (n_incomplete != 0) {
  message("Warning: ", n_incomplete, " players could not be simulated")
}

# simulated seasons starting with the current season (the next season in the offseason)
player_simulations <- today_value$sims_current %||% today_value$sims_next

saveRDS(player_simulations, here("Modeling/models/player_simulations.rds"))

# Future Value over Time --------------------------------------------------
# goal is to only run one at a time, while keeping the previous models.
# Set recompute_future_value_time to TRUE to recompute every KTC date with the
# current models (e.g. after retraining)

recompute_future_value_time <- getOption("fantasy.recompute_future_value_time", FALSE)

future_value_time <- if (recompute_future_value_time) {
  tibble(
    name = character(),
    player_id = character(),
    future_value = double(),
    date = as.Date(character())
  )
} else {
  read_csv(
    here("Shiny/Saved Files/future_value_time.csv"),
    show_col_types = FALSE,
    col_types = cols(player_id = col_character())
  ) |>
    filter(date != today())
}

last_date_fvt <- if (nrow(future_value_time) > 0) {
  max(future_value_time$date)
} else {
  min(ktc_tibble$date) - 1
}

message("begin mapping future value over time...")

while (last_date_fvt < max(ktc_tibble$date)) {
  ktc_snapshot <- ktc_tibble |>
    filter(date > last_date_fvt) |>
    slice_min(order_by = date, with_ties = TRUE)

  current_date <- ktc_snapshot$date[1]
  message("starting ", current_date)

  new_future_value <- if (current_date == today()) {
    # today's values were just simulated above with the same inputs, so reuse them
    today_value$future_value
  } else {
    future_value_at_date(
      future_value_names,
      ktc_snapshot |> select(-date),
      current_date,
      season_dates,
      models
    )$future_value
  }

  future_value_time <- bind_rows(new_future_value, future_value_time)

  message("finished ", current_date)

  last_date_fvt <- max(future_value_time$date)
}

future_value_time <- future_value_time |>
  arrange(desc(date), desc(future_value))

write_csv(future_value_time, here("Shiny/Saved Files/future_value_time.csv"))

# compute current player_total_value tibble (joined on player_id)
season_value_wide <- season_value_added |>
  select(player_id, season, total_value_added) |>
  pivot_wider(
    names_from = season,
    values_from = total_value_added,
    names_prefix = "sva_"
  )

player_total_value <- future_value_time |>
  filter(date == last_date_fvt) |>
  select(player_id, fv_name = name, future_value) |>
  full_join(season_value_wide, by = join_by(player_id)) |>
  left_join(
    season_value_added |>
      distinct(player_id, .keep_all = TRUE) |>
      select(player_id, sva_name = name),
    by = join_by(player_id)
  ) |>
  mutate(name = coalesce(fv_name, sva_name)) |>
  select(name, player_id, matches("^sva_\\d{4}$"), future_value) |>
  left_join(player_info |> select(-name), by = join_by(player_id)) |>
  left_join(
    keep_trade_cut |> select(player_id, ktc_value),
    by = join_by(player_id)
  ) |>
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

write_csv(player_total_value, here("Shiny/Saved Files/player_total_value.csv"))

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
  toy_tva_quantiles <- generate_samples(tva_fit, prep_toy_tva_data) |>
    compute_quantiles(tva_resid_fit, prep_toy_tva_data)
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
  ) |>
    # only a preseason KTC of 5,000 is plotted, so only predict those rows
    filter(historical_value == 5000)

  prep_toy_ktc_data <- prep_data_ktc(toy_ktc_data, ktc_scales)

  tic()
  toy_ktc_quantiles <- generate_samples(ktc_fit, prep_toy_ktc_data) |>
    compute_quantiles(ktc_resid_fit, prep_toy_ktc_data)
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
    )

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

  write_csv(toy_tva_plot_data, here("Shiny/Saved Files/toy_tva_plot_data.csv"))
  write_csv(toy_ktc_plot_data, here("Shiny/Saved Files/toy_ktc_plot_data.csv"))
}
