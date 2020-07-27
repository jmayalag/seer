if(file.exists("custbacktest_plot.R")) {
  source("custbacktest.R")
} else {
  source("analysis/custbacktest.R")
}

ml_strategies <- function(base_strategy, model, h, w, model_name = NULL) {
  approaches <- tibble(approach = c("risky", "semirisky", "conservative"))
  approaches %>%
    mutate(strategy = map(approach, ~ technical_ml(base_strategy, model, h=h, w=w, approach = .x, model_name))) %>%
    select(-approach) %>%
    mutate(name = map_chr(strategy, 'name'))
}

model_path <- "~/datasets/jcr2020/exp3/h24_w12_dDAX_D1_lm.rds"

h <- 24
w <- 12

data_dir <- "~/datasets/jcr2020/datasets"

datasets <- tribble(
  ~dataset, ~cost, ~strategy,
  "d1_dax_2019", 2, list(macd(5, 12), triple_ema(1, 18, 72)),
  "d1_dj30_2019", 2.4, list(macd(6, 12), triple_ema(2, 18, 72)),
  "d1_ibex35_2019", 5, list(macd(7, 12), triple_ema(2, 18, 70))
)

tb <- datasets %>%
  mutate(filename = file.path(data_dir, paste0(dataset, ".csv"))) %>%
  unnest(strategy)

model_name <- basename(model_path) %>% stringr::str_remove(".rds")
model <- readr::read_rds(model_path)

strategies <- tb %>%
  rename(base_strategy = strategy) %>%
  mutate(ml_strategy = map(base_strategy, ~ ml_strategies(.x, model, h, w, model_name))) %>%
  unnest(ml_strategy) %>%
  rename(ml_strategy = strategy) %>%
  select(-name) %>%
  pivot_longer(c(base_strategy, ml_strategy), names_to = "type", values_to = "strategy") %>%
  mutate(name = map_chr(strategy, 'name')) %>%
  distinct(dataset, cost, name, .keep_all = T)

# Se requiere el flag debug = T para que se guarden todos los datos del backest (Close, indicadores y señales)
ml_results <- strategies %>%
  mutate(data = map(filename, read_ohlcv)) %>%
  mutate(results = pmap(list(dataset, data, strategy, cost), ~ run_backtest(symbol=..1, data=..2, strat=..3, cost=..4, qty=qty, sell_at_end = T, debug = F))) %>%
  select(-data)

ml_stats <- ml_results %>%
  rename(strategy_list = strategy) %>%
  mutate(stats = map(results, 'stats')) %>%
  select(-cost) %>%
  unnest(stats) %>%
  select(-strategy_list)

ml_stats %>% select(dataset, name, net_profit, num_trades, profit_factor) %>%
  arrange(dataset, name) %>% print(n=100)

write_rds(ml_results, file.path("results", paste0(model_name, "backtest", ".rds")))