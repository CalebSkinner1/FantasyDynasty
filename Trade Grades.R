# Trade Grades

# alright this is what this was all for amiright
library("here")
library("gt")
library("gtExtras")
library("tidymodels")
library("tidyverse"); theme_set(theme_minimal())
data_path <- "FantasyDynasty/"

load(here(data_path, "Data/transactions.RData"))

temp <- transactions %>%
  filter(type == "trade") %>%
  split(seq_len(nrow(.)))

a <- temp[[1]]$adds %>%
  pivot_longer(cols = everything(),
               names_to = "player_id",
               values_to = "roster_id") %>%
  drop_na()