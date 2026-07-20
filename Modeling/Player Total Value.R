# Future Value and Total Value

suppressPackageStartupMessages({
  library("here")
  library("bundle")
  library("face")
})

message("begin computing Player Total Value...")
demonstrate_fit <- FALSE # set to true to rerun the model fit images
train_models <- TRUE # set to true to retrain models

source(here("Data Manipulation/Scrape Support.R")) # grab functions
source(here("Modeling/Player Total Value Functions.R")) # grab functions
season_value_added <- read_csv(here("Data/sva.csv"), show_col_types = FALSE) # shortcut
player_info <- read_csv(here("Data/player_info.csv"), show_col_types = FALSE) # shortcut
season_dates <- read_csv(here("Data/season_dates.csv"), show_col_types = FALSE)

ktc_list <- list.files(
  path = here("Data/ktc values"),
  full.names = T
) %>%
  set_names(basename(.)) %>%
  map(~ read_csv(.x, show_col_types = FALSE))

# these are the names of the dudes that I'll compute the future value of repetitively
future_value_names <- map_dfr(ktc_list, name_correction) %>%
  distinct(name) %>%
  left_join(player_info, by = join_by(name)) %>%
  select(-player_id) %>%
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
  drop_na()

# write_csv(future_value_names, here("Data/future_value_names.csv"))

# organize data sets
ktc_begin_end_dates <- list(
  year1 = list(
    pre_ktc_date = ymd("2024-08-23"),
    post_ktc_date = ymd("2025-08-24")
  ),
  year2 = list(
    pre_ktc_date = ymd("2025-08-24"),
    post_ktc_date = ymd("2026-01-06")
  )
) # push back as far as possible

# training ---------------------------------------------------------------
if (train_models) {
  hktc_data <- map_dfr(
    ktc_begin_end_dates,
    ~ compile_training_data(
      ktc_list,
      player_info,
      pre_ktc_date = .x$pre_ktc_date,
      post_ktc_date = .x$post_ktc_date
    )
  )

  all_seasons <- sort(unique(c(season_value_added$season, hktc_data$season)))

  real_obs <- bind_rows(
    season_value_added |>
      filter(!(position %in% c("K", "DST"))) |>
      transmute(name, season, position, total_value_added),
    hktc_data |>
      filter(!(position %in% c("K", "DST"))) |>
      transmute(name, season, position, total_value_added = tva_adj)
  ) |>
    distinct(name, season, position, .keep_all = TRUE) |>
    left_join(player_info, by = join_by(name, position))

  # Catch player_info join failures (name mismatches)
  name_mismatches <- real_obs |>
    filter(is.na(birth_date)) |>
    distinct(name, position)
  if (nrow(name_mismatches) > 0) {
    warning(sprintf(
      "%d player(s) failed to match player_info (NA birth_date) -- see `name_mismatches`. These will be dropped.",
      nrow(name_mismatches)
    ))
    print(name_mismatches)
  }

  fpca_data <- real_obs |>
    filter(!is.na(birth_date)) |>
    group_by(name, position) |>
    complete(season = seq(min(season), max(all_seasons))) |>
    fill(birth_date, .direction = "downup") |>
    mutate(
      total_value_added = replace_na(total_value_added, 0),
      age = as.numeric(ktc_begin_end_dates$year1$pre_ktc_date - birth_date) /
        365.25 +
        season -
        2024
    ) |>
    ungroup() |>
    transmute(
      subj = name,
      argvals = age,
      y = total_value_added,
      position,
      season
    ) |>
    arrange(position, subj, argvals)

  fpca_models <- compute_future_value(
    fpca_data,
    hktc_data,
    positions
  )

  save(fpca_models, file = here("Modeling/fpca_models.RData"))
}


# fpca_data |>
#   ggplot(aes(argvals, y, group = subj)) +
#   geom_line(alpha = 0.15) +
#   geom_point(alpha = 0.3, size = 0.8) +
#   facet_wrap(~position) +
#   labs(x = "Age", y = "VA (tva_adj)", title = "Raw career points by position")

# taper - c(5, 2), years ahead - 7
#    name               position estimate    se
#    <chr>              <chr>       <dbl> <dbl>
#  1 Bijan Robinson     RB           968. 103.
#  2 Jahmyr Gibbs       RB           954. 103.
#  3 De'Von Achane      RB           777. 101.
#  4 Drake Maye         QB           730. 140.
#  5 Josh Allen         QB           705. 135.
#  6 Caleb Williams     QB           693. 131.
#  7 Jayden Daniels     QB           622. 128.
#  8 Trey McBride       TE           617.  86.2
#  9 Lamar Jackson      QB           584. 118.
# 10 Bo Nix             QB           557. 114.
# 11 Jonathan Taylor    RB           553. 103.
# 12 Brock Purdy        QB           550. 113.
# 13 James Cook         RB           541. 102.
# 14 Jalen Hurts        QB           538. 112.
# 15 Jaxon Smith-Njigba WR           535.  74.8
# 16 Trevor Lawrence    QB           527. 116.
# 17 Ja'Marr Chase      WR           492. 107.
# 18 Puka Nacua         WR           482.  81.5
# 19 Chase Brown        RB           473.  95.1
# 20 Ashton Jeanty      RB           451. 129.

# taper - c(7, 2), years ahead - 7
# name               position estimate    se
#    <chr>              <chr>       <dbl> <dbl>
#  1 Bijan Robinson     RB           968. 103.
#  2 Jahmyr Gibbs       RB           954. 103.
#  3 De'Von Achane      RB           777. 101.
#  4 Drake Maye         QB           730. 140.
#  5 Josh Allen         QB           705. 135.
#  6 Caleb Williams     QB           693. 131.
#  7 Jayden Daniels     QB           622. 128.
#  8 Trey McBride       TE           617.  86.2
#  9 Lamar Jackson      QB           584. 118.
# 10 Bo Nix             QB           557. 114.
# 11 Jonathan Taylor    RB           553. 103.
# 12 Brock Purdy        QB           550. 113.
# 13 James Cook         RB           541. 102.
# 14 Jalen Hurts        QB           538. 112.
# 15 Jaxon Smith-Njigba WR           535.  74.8
# 16 Trevor Lawrence    QB           527. 116.
# 17 Ja'Marr Chase      WR           492. 107.
# 18 Puka Nacua         WR           482.  81.5
# 19 Chase Brown        RB           473.  95.1
# 20 Ashton Jeanty      RB           451. 129.

# taper - c(7, 2), years ahead - 10
# name               position estimate    se
#    <chr>              <chr>       <dbl> <dbl>
#  1 Bijan Robinson     RB          1089.  131.
#  2 Jahmyr Gibbs       RB          1081.  130.
#  3 Drake Maye         QB           916.  173.
#  4 Josh Allen         QB           914.  172.
#  5 De'Von Achane      RB           860.  128.
#  6 Caleb Williams     QB           841.  159.
#  7 Lamar Jackson      QB           749.  145.
#  8 Jayden Daniels     QB           733.  155.
#  9 Trey McBride       TE           661.  137.
# 10 Jalen Hurts        QB           653.  134.
# 11 Bo Nix             QB           653.  136.
# 12 Brock Purdy        QB           646.  135.
# 13 Trevor Lawrence    QB           616.  139.
# 14 Jaxon Smith-Njigba WR           586.  104.
# 15 Joe Burrow         QB           558.  145.
# 16 Jonathan Taylor    RB           556.  131.
# 17 Ashton Jeanty      RB           556.  166.
# 18 Jaxson Dart        QB           555.  159.
# 19 James Cook         RB           550.  130.
# 20 Justin Herbert     QB           543.  141.

# taper - c(5, 2), years ahead - 10
#    name               position estimate    se
#    <chr>              <chr>       <dbl> <dbl>
#  1 Bijan Robinson     RB          1089.  131.
#  2 Jahmyr Gibbs       RB          1081.  130.
#  3 Drake Maye         QB           916.  173.
#  4 Josh Allen         QB           914.  172.
#  5 De'Von Achane      RB           860.  128.
#  6 Caleb Williams     QB           841.  159.
#  7 Lamar Jackson      QB           749.  145.
#  8 Jayden Daniels     QB           733.  155.
#  9 Trey McBride       TE           661.  137.
# 10 Jalen Hurts        QB           653.  134.
# 11 Bo Nix             QB           653.  136.
# 12 Brock Purdy        QB           646.  135.
# 13 Trevor Lawrence    QB           616.  139.
# 14 Jaxon Smith-Njigba WR           586.  104.
# 15 Joe Burrow         QB           558.  145.
# 16 Jonathan Taylor    RB           556.  131.
# 17 Ashton Jeanty      RB           556.  166.
# 18 Jaxson Dart        QB           555.  159.
# 19 James Cook         RB           550.  130.
# 20 Justin Herbert     QB           543.  141.

# Future Value over Time --------------------------------------------------

# load models
load(file = here("Modeling/fpca_models.RData"))

# compute future value over time
last_date_fvt <- read_csv(
  here("Data/last_date_fvt.csv"),
  show_col_types = FALSE
) %>%
  pull(value)

last_date_fvt <- ymd("20240101")

keep_trade_cut <- select_ktc_list(ktc_list, last_date_fvt)$keep_trade_cut[[1]]
current_date <- select_ktc_list(ktc_list, last_date_fvt)$date[[1]]

# origin data set, set at beginning of last year
players_df <- compile_data_set(
  keep_trade_cut,
  future_value_names,
  season_value_added,
  current_date, #today()
  min(season_dates$season_start), # max(season_dates$season_start),
  min(season_dates$season_end) # max(season_dates$season_end)
)

message("begin mapping future value over time...")

projection <- project_careers(
  players_df,
  run_by_positions,
  n_years_ahead = 10,
  discount_rate = 0.95,
  taper_window = c(5, 2)
)

future_value_time <- projection$future_value |>
  rename(
    future_value = estimate
  ) |>
  mutate(date = current_date)
select(name, future_value, date) |>
  bind_rows(future_value_time)

write_csv(future_value_time, here("Shiny/Saved Files/future_value_time.csv"))
# make list of the dates already computed, so I don't have to compute them again
max(future_value_time$date) |>
  as_tibble() |>
  write_csv(here("Data/last_date_fvt.csv"))

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
  left_join(player_info, by = join_by(name)) |>
  left_join(keep_trade_cut, by = join_by(name)) |>
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

player_simulations <- map(
  split(projection$quantiles, projection$quantiles$season),
  ~ .x |>
    select(-season, -age, -position) |>
    mutate(quantile_prob = 100 * quantile_prob) |>
    pivot_wider(
      names_from = quantile_prob,
      values_from = quantile_value,
      names_prefix = "proj_tva_"
    )
)

save(player_simulations, file = here("Modeling/player_simulations.RData"))
load(here("Modeling/player_simulations.RData"))

# Demonstrate Model Fit ----------------------------------------------------
if (demonstrate_fit) {
  # # tva
  # toy_tva_data <- tibble(
  #   position = c(
  #     rep("QB", 1200),
  #     rep("RB", 1200),
  #     rep("WR", 1200),
  #     rep("TE", 1200)
  #   ),
  #   age = rep(
  #     c(
  #       rep(22, 100),
  #       rep(23, 100),
  #       rep(24, 100),
  #       rep(25, 100),
  #       rep(26, 100),
  #       rep(27, 100),
  #       rep(28, 100),
  #       rep(29, 100),
  #       rep(30, 100),
  #       rep(31, 100),
  #       rep(32, 100),
  #       rep(33, 100)
  #     ),
  #     4
  #   ),
  #   historical_value = rep(seq(from = 100, to = 10000, length.out = 100), 48),
  #   tva_adj = NA
  # )
  # prep_toy_tva_data <- toy_tva_data |> prep_data_tva(tva_scales)
  # tic()
  # toy_tva_quantiles <- generate_samples(tva_fit, prep_toy_tva_data$full_data) |>
  #   compute_quantiles(tva_resid_fit, prep_toy_tva_data$full_data)
  # toc()
  # toy_tva_plot_data <- toy_tva_data |>
  #   bind_cols(as_tibble(t(toy_tva_quantiles))) |>
  #   mutate(age = factor(age)) |>
  #   rename(
  #     median_tva = "V20",
  #     q05 = "V2",
  #     q10 = "V4",
  #     q90 = "V36",
  #     q95 = "V38"
  #   ) |>
  #   select(historical_value, age, position, median_tva, q05, q10, q90, q95)
  # toy_tva_plot_data |>
  #   ggplot() +
  #   geom_line(aes(x = historical_value, y = median_tva, color = position)) +
  #   geom_ribbon(
  #     aes(x = historical_value, ymin = q10, ymax = q90, fill = position),
  #     alpha = 0.2
  #   ) +
  #   facet_wrap(~age, nrow = 3) +
  #   labs(x = "KeepTradeCut", y = "Season Value Added") +
  #   theme(
  #     axis.text.x = element_text(angle = 30, vjust = 1.25, hjust = 1),
  #     legend.title = element_blank()
  #   )
  # # ktc
  # toy_ktc_data <- tibble(
  #   position = c(
  #     rep("QB", 24000),
  #     rep("RB", 24000),
  #     rep("WR", 24000),
  #     rep("TE", 24000)
  #   ),
  #   age = rep(
  #     c(
  #       rep(23, 2000),
  #       rep(24, 2000),
  #       rep(25, 2000),
  #       rep(26, 2000),
  #       rep(27, 2000),
  #       rep(28, 2000),
  #       rep(29, 2000),
  #       rep(30, 2000),
  #       rep(31, 2000),
  #       rep(32, 2000),
  #       rep(33, 2000),
  #       rep(34, 2000)
  #     ),
  #     4
  #   ),
  #   historical_value = rep(seq(from = 100, to = 10000, length.out = 100), 960),
  #   tva_adj = rep(
  #     c(
  #       rep(-28, 100),
  #       rep(-16, 100),
  #       rep(-4, 100),
  #       rep(8, 100),
  #       rep(20, 100),
  #       rep(32, 100),
  #       rep(44, 100),
  #       rep(56, 100),
  #       rep(68, 100),
  #       rep(80, 100),
  #       rep(92, 100),
  #       rep(104, 100),
  #       rep(116, 100),
  #       rep(128, 100),
  #       rep(140, 100),
  #       rep(152, 100),
  #       rep(164, 100),
  #       rep(176, 100),
  #       rep(188, 100),
  #       rep(200, 100)
  #     ),
  #     48
  #   ),
  #   ktc_value = 0
  # )
  # prep_toy_ktc_data <- prep_data_ktc(toy_ktc_data, ktc_scales)
  # tic()
  # toy_ktc_quantiles <- generate_samples(ktc_fit, prep_toy_ktc_data$full_data) |>
  #   compute_quantiles(ktc_resid_fit, prep_toy_ktc_data$full_data)
  # toc()
  # toy_ktc_plot_data <- toy_ktc_data |>
  #   bind_cols(as_tibble(t(toy_ktc_quantiles))) |>
  #   mutate(age = factor(age)) |>
  #   rename(
  #     median_ktc = "V20",
  #     q05 = "V2",
  #     q10 = "V4",
  #     q90 = "V36",
  #     q95 = "V38"
  #   ) |>
  #   select(
  #     historical_value,
  #     age,
  #     position,
  #     tva_adj,
  #     median_ktc,
  #     q05,
  #     q10,
  #     q90,
  #     q95
  #   ) |>
  #   filter(historical_value == 5000)
  # toy_ktc_plot_data |>
  #   ggplot() +
  #   geom_line(aes(x = tva_adj, y = median_ktc, color = position)) +
  #   geom_ribbon(
  #     aes(x = tva_adj, ymin = q10, ymax = q90, fill = position),
  #     alpha = 0.2
  #   ) +
  #   facet_wrap(~age, nrow = 3) +
  #   labs(x = "Total Value Added", y = "Post-season KeepTradeCut") +
  #   coord_cartesian(ylim = c(0, 10000)) +
  #   theme(legend.title = element_blank())
  # write_csv(toy_tva_plot_data, here("Data/toy_tva_plot_data.csv"))
  # write_csv(toy_ktc_plot_data, here("Data/toy_ktc_plot_data.csv"))
}
