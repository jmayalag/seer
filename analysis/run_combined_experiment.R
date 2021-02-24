library(seer)
library(tidyverse)

ml_strategies <- function(base_strategy, model, h, w, model_name = NULL) {
  approaches <- tibble(approach = c("risky", "semirisky", "conservative"))
  approaches %>%
    mutate(strategy = map(approach, ~ technical_ml(base_strategy, model, h=h, w=w, approach = .x, model_name))) %>%
    select(-approach) %>%
    mutate(name = map_chr(strategy, 'name'))
}

model_dir <- "~/datasets/jcr2020/exp3/lmodels"
data_dir <- "~/datasets/jcr2020/exp3/datasets"
image_dir <- "results/backtest/orders"

datasets <- tribble(
  ~dataset, ~cost, ~strategy, ~model_name,
  "d1_ibex35_2019", 5, list(macd(9, 19, 6), triple_ema(2, 6, 10)), "ibex35_w72_lm",
  "d1_dax_2019", 2, list(macd(3, 11, 6), triple_ema(14, 30, 74)), "dax_w12_lm",
  "d1_dj30_2019", 2.4, list(macd(3, 28, 9), triple_ema(18, 34, 54)), "dj30_w96_lm",
) %>%
  mutate(w = str_match(model_name, "_w(\\d+)_")[,2] %>% as.numeric())

h <- 24

tb <- datasets %>%
  mutate(filename = file.path(data_dir, paste0(dataset, ".csv"))) %>%
  mutate(model_path = file.path(model_dir, paste0(model_name, ".rds"))) %>%
  mutate(model = map(model_path, read_rds)) %>%
  unnest(strategy)

strategies <- tb %>%
  rename(base_strategy = strategy) %>%
  mutate(ml_strategy = pmap(list(base_strategy, model, w, model_name), ~ ml_strategies(..1, ..2, h, ..3, ..4))) %>%
  unnest(ml_strategy) %>%
  rename(ml_strategy = strategy) %>%
  select(-name) %>%
  pivot_longer(c(base_strategy, ml_strategy), names_to = "type", values_to = "strategy") %>%
  mutate(name = map_chr(strategy, 'name')) %>%
  distinct(dataset, cost, name, .keep_all = T)

# Se requiere el flag debug = T para que se guarden todos los datos del backest (Close, indicadores y señales)
ml_results <- strategies %>%
  mutate(data = map(filename, read_ohlcv)) %>%
  mutate(results = pmap(list(dataset, data, strategy, cost), ~ run_backtest(symbol=..1, data=..2, strat=..3, cost=..4, qty=1, sell_at_end = T, debug = T))) %>%
  select(-data)

ml_stats <- ml_results %>%
  rename(strategy_list = strategy) %>%
  mutate(stats = map(results, 'stats')) %>%
  select(-cost) %>%
  unnest(stats) %>%
  # mutate(profit_factor = if_else(is.infinite(profit_factor), gross_profits, profit_factor)) %>%
  mutate(win_prob = wins / num_trades * 100) %>%
  mutate(across(where(is.numeric), ~ if_else(is.nan(.x), as.double(0), as.double(.x))))

ml_stats %>% select(name, dataset, num_trades, profit_factor, net_profit, avg_profit_per_trade, max_drawdown, win_prob) %>%
  filter(!str_detect(name, "(semirisky|conservative)")) %>%
  print(n=100)

write_rds(ml_results, file.path("results", "hybrid_backtest.rds"))
write_rds(ml_stats, file.path("results", "hybrid_stats.rds"))

if(file.exists("example_plot_backtest.R")) {
  source("example_plot_backtest.R")
} else {
  source("analysis/example_plot_backtest.R")
}
dir.create(image_dir, recursive = T, showWarnings = F)

plots <- ml_results %>%
  mutate(plot = map(results, ~ plot_trades(.x$data, .x$trades))) %>%
  select(dataset, name, plot) %>%
  mutate(plot_file = file.path(image_dir, paste0(dataset, "-", name, ".png")))

plots %>%
  filter(!str_detect(name, "(semirisky|conservative)")) %>%
  rowwise() %>%
  do(x = ggsave(.$plot_file, .$plot, width = 8, height = 5)) 

ml_stats %>% 
  select(name, dataset, num_trades, profit_factor, net_profit, avg_profit_per_trade, max_drawdown, wins) %>% 
  mutate(win_prob = wins / num_trades * 100) %>%
  pivot_longer(cols = -c(name, dataset), names_to = "metric") %>%
  mutate(base_strategy = str_split_fixed(name, pattern = "_", n = 2)[, 1]) %>%
  mutate(hybrid_strategy = str_split_fixed(name, pattern = "ml_", n = 2)[, 2]) %>%
  mutate(params = str_replace(name, "-.*", "") %>% str_replace("[a-zA-Z]+_", "") %>% str_replace_all("_", ", ")) %>%
  mutate(params = paste0("(", params, ")")) %>%
  write_csv("results/ml_hybrid_stats.csv") 
