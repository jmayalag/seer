if (file.exists("custbacktest.R")) {
  source("custbacktest.R")
} else {
  source("analysis/custbacktest.R")
}

suppressMessages(require(tidyverse))

run_backtest_grid <- function(dataset, filename, strategy_name, strategy, grid, cost, qty = 1, sell_at_end = T, debug = F) {
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
  write_rds(results, file.path("results", sprintf("%s-%s.rds", strategy_name, dataset)))
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

top_backtest <- function(backtest_results, n = 1) {
  backtest_results %>%
    extract_stats() %>%
    select(-c(filename, symbol, order_size)) %>%
    mutate(strategy = str_replace_all(strategy, "[_\\d]", "")) %>%
    group_by(dataset, strategy) %>%
    slice_max(net_profit, n = n)
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
  nFast = 1:10,
  nSlow = 5:20
) %>% filter(nFast < nSlow)

tema_grid <- expand_grid(
  nFast = 1:10,
  nMedium = 5:15,
  nSlow = 10:25
) %>% filter(nFast < nMedium & nMedium < nSlow)

results <- datasets %>%
  mutate(tema_results = pmap(list(dataset, filename, cost), ~ run_backtest_grid(..1, ..2, strategy_name = "TEMA", strategy = triple_ema, ..3, grid = tema_grid))) %>%
  mutate(macd_results = pmap(list(dataset, filename, cost), ~ run_backtest_grid(..1, ..2, strategy_name = "MACD", strategy = macd, ..3, grid = macd_grid)))

stats <- extract_stats(results)

# Best params per strategy and dataset
best <- top_backtest(results, n = 1)

best %>%
  mutate(dataset = dataset_name(dataset)) %>%
  t()