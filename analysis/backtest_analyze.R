library(tidyverse)

files <- list.files("results/backtest", pattern = "*.rds", full.names = T, recursive = T)

read_backtest_stats <- function(file) {
  message(sprintf("Reading %s ...", file))
  stats <- read_rds(file)$stats
  stats$file <- basename(file)
  stats
}

results <- map_df(files, read_backtest_stats) %>%
  select(-Portfolio)

results
