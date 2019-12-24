library(dplyr)
library(tidyr)
library(seer)
library(glue)
library(readr)

run_backtest <- function(dataset, strategy, file_prefix, save_dir) {
  results_file <- file.path(save_dir, paste0(file_prefix, ".rds"))
  if (file.exists(results_file)) {
    return(read_rds(results_file))
  }
  message(glue("Running {file_prefix}..."))

  a <- Sys.time()

  capture.output(results <- backtest.opt(
    symbols = dataset,
    strategy = strategy,
  ), type = "output")

  b <- Sys.time()

  dt <- difftime(b, a, units = "secs")
  message(paste("Finished in ", dt, "secs"))
  write_rds(results, path = results_file)
  results
}

# Config ----
out_dir <- file.path("./results/backtest")
dir.create(file.path(out_dir, "macd"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "triplecross"), showWarnings = FALSE, recursive = TRUE)

dataset_dir <- "~/datasets/datasets_paper"

datasets <- c("ES35_D1", "GERMAN30_D1", "US30_D1")

macd_grid <- expand_grid(
  nFast = 1:10,
  nSlow = 5:20
) %>% filter(nFast < nSlow)

triplecross_grid <- expand_grid(
  nFast = 1:10,
  nMedium = 5:15,
  nSlow = 10:25
) %>% filter(nFast < nMedium && nMedium < nSlow)

for (dataset in datasets) {
  message(dataset)
  x <- read_dataset(dataset, datadir = dataset_dir)
  assign(dataset, x, envir = .GlobalEnv)

  macd_grid %>%
    mutate(file_prefix = glue("macd_f{nFast}_s{nSlow}_d{dataset}")) %>%
    rowwise() %>%
    do(test = run_backtest(
      dataset,
      macdhist(fastMA = .$nFast, slowMA = .$nSlow),
      .$file_prefix,
      save_dir = file.path(out_dir, "macd")
    ))

  triplecross_grid %>%
    mutate(file_prefix = glue("triplecross_f{nFast}_m{nMedium}_s{nSlow}_d{dataset}")) %>%
    rowwise() %>%
    do(test = run_backtest(
      dataset,
      triple_crossover(nFast = .$nFast, nMedium = .$nMedium, nSlow = .$nSlow),
      .$file_prefix,
      save_dir = file.path(out_dir, "triplecross")
    ))
  rm(list = dataset)
}
