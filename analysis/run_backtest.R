suppressMessages(library(seer))
suppressMessages(library(tidyverse))

run_backtest_grid <- function(dataset, filename, strategy_name, strategy, grid, cost, qty = 1, sell_at_end = T, debug = F) {
  result_file <- file.path("results", sprintf("%s-%s.rds", strategy_name, dataset))
  run_name <- paste0(strategy_name, " dataset: ", dataset, ", cost: ", cost, ", grid size: ", nrow(grid))
  if (file.exists(result_file)) {
    message(paste("Found previous", run_name, result_file))
    return(result_file)
  }
  message(paste0("Starting ", run_name))

  strategies <- grid %>%
    mutate(strategy = pmap(., strategy)) %>%
    rename_with(~ paste0("param.", .x), .cols = !starts_with("strategy")) %>%
    mutate(name = map_chr(strategy, "name"))

  data <- read_ohlcv(filename)

  results <- strategies %>%
    mutate(result = map(strategy, ~ run_backtest(
      symbol = dataset, data = data, strat = .x,
      cost = cost, qty = qty, sell_at_end = sell_at_end,
      debug = debug
    )))

  message(paste0("Finished ", run_name, ". Experiments: ", nrow(grid)))
  write_rds(results, result_file)
  result_file
}

extract_stats <- function(backtest_results) {
  backtest_results %>%
    mutate(map_df(result, "stats")) %>%
    select(-c(result, strategy)) %>%
    select(-starts_with("param"))
}

data_dir <- "~/datasets/jcr2020/datasets"

datasets <- tribble(
  ~dataset, ~cost,
  "d1_dax_2000-2018", 2,
  "d1_dj30_2000-2018", 2.4,
  "d1_ibex35_2000-2018", 5,
) %>%
  mutate(filename = file.path(data_dir, paste0(dataset, ".csv")))

backtests <- tribble(
  ~strategy_name, ~strategy, ~grid,
  
  "TEMA", triple_ema, expand_grid(
    nFast = 1:25,
    nMedium = 5:50,
    nSlow = 10:75
  ) %>% filter(nFast < nMedium & nMedium < nSlow),
  
  "MACD", macd, expand_grid(
    nFast = 1:25,
    nSlow = 5:75,
    nSig = 5:25
  ) %>% filter(nFast < nSlow)
)

results <- pmap_df(list(backtests$strategy_name, backtests$strategy, backtests$grid), function(strategy_name, strategy, grid) {
  message(sprintf("%s, n: %d", strategy_name, nrow(grid)))
  
  pmap_df(list(datasets$dataset, datasets$filename, datasets$cost), function(dataset, filename, cost) {
    data.frame(results_file=run_backtest_grid(dataset, filename, strategy_name = strategy_name, strategy = strategy, cost, grid = grid))
  })
})


# Summarize stats separately to prevent filling the memory
stats <- results %>%
  mutate(strategy_stats = map(results_file, function(results_file) {
    res <- read_rds(results_file)
    extract_stats(res)
  })) %>%
  unnest(strategy_stats)

write_rds(stats, file.path("results", "backtest_stats.rds"))