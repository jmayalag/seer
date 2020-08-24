library(seer)
library(tidyverse)

suppressMessages(require(tidyverse))

run_backtest_grid <- function(dataset, filename, strategy_name, strategy, grid, cost, qty = 1, sell_at_end = T, debug = F) {
  result_file <- file.path("results", sprintf("%s-%s.rds", strategy_name, dataset))
  if (file.exists(result_file)) {
    return(read_rds(result_file))
  }
  run_name <- paste0(strategy_name, " grid: ", dataset, ", cost: ", cost)
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
  results
}

extract_stats <- function(backtest_results) {
  backtest_results %>%
    pivot_longer(ends_with("results"), names_to = "strategy_result") %>%
    unnest(value) %>%
    rename(strategy_list = strategy) %>%
    mutate(map_df(result, "stats")) %>%
    select(-c(strategy_result, result)) %>%
    select(-starts_with("param"))
}

dataset_name <- function(dataset) {
  str_replace_all(dataset, "[\\-_\\d]", "") %>% str_replace("^d", "") %>% str_to_upper()
}

top_backtest <- function(stats, n = 1, column = "net_profit") {
  best_profit <- stats %>%
    select(-c(order_size)) %>%
    mutate(strategy = str_replace_all(strategy, "[_\\d]", "")) %>%
    group_by(dataset, strategy) %>%
    slice_max(net_profit, n = 1, with_ties = T)
  
  best_profit %>% slice_max(max_drawdown, n=1, with_ties = F)
}

data_dir <- "~/datasets/jcr2020/datasets"

datasets <- tribble(
  ~dataset, ~cost,
  "d1_dax_2000-2018", 2,
  "d1_dj30_2000-2018", 2.4,
  "d1_ibex35_2000-2018", 5,
) %>%
  mutate(filename = file.path(data_dir, paste0(dataset, ".csv")))

macd_grid <- expand_grid(
  nFast = 1:25,
  nSlow = 5:75
) %>% filter(nFast < nSlow)

tema_grid <- expand_grid(
  nFast = 1:25,
  nMedium = 15:50,
  nSlow = 25:75
) %>% filter(nFast < nMedium & nMedium < nSlow)

results <- datasets %>%
  mutate(tema_results = pmap(list(dataset, filename, cost), ~ run_backtest_grid(..1, ..2, strategy_name = "TEMA", strategy = triple_ema, ..3, grid = tema_grid))) %>%
  mutate(macd_results = pmap(list(dataset, filename, cost), ~ run_backtest_grid(..1, ..2, strategy_name = "MACD", strategy = macd, ..3, grid = macd_grid)))

stats <- extract_stats(results)
write_rds(stats, file.path("results", "backtest_stats_full.rds"))
stats <- stats %>% select(-c(strategy_list, symbol, filename))
write_rds(stats, file.path("results", "backtest_stats.rds"))

# Best params per strategy and dataset
best <- top_backtest(stats, n = 1)

best %>%
  mutate(dataset = dataset_name(dataset)) %>%
  t()
