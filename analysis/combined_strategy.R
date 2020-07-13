library(seer)
library(randomForest)
library(dplyr)
library(tidyr)
library(purrr)

combined_analysis <- function(dataset, dataset_name, 
                              model, model_name, 
                              h, w,
                              base_strat,
                              fees = 0, order_size = 1, verbose = F) {
  message(sprintf("\nh=%d, w=%d", h, w))
  model_name <- sprintf("%s_h%d_w%d", model_name, h, w)
  
  mkt <- ml_apply_indicator_signals(dataset, base_strat, model, h=h, w=w)
  
  dataset_name <- stringr::str_replace(dataset_name, "[^a-zA-z0-9_.]", "_")
  assign(dataset_name, mkt, envir = .GlobalEnv)
  
  strat <- ml_strat(base_strat$name, approach = "conservative", fees = fees, order_size = order_size)
  message(paste0(strat$name, "..."))
  result_conservative <- backtest.opt(strat, dataset_name, silent = !verbose)
  result_conservative$stats$model <- model_name
  
  strat <- ml_strat(base_strat$name, approach = "risky", fees = fees, order_size = order_size)
  message(paste0(strat$name, "..."))
  result_risky <- backtest.opt(strat, dataset_name, silent = !verbose)
  result_risky$stats$model <- model_name
  
  # debug <- seer::apply_indicator_signals(strat, get(dataset_name, envir = .GlobalEnv))
  
  strat <- base_strat
  message(paste0(strat$name, "..."))
  result_base <- backtest.opt(strat, dataset_name, silent = !verbose)
  
  results <- list(result_base, result_conservative, result_risky)
  results
}

plot_ml <- function(dataset, dataset_name, model, model_name, h, w, multiple = F) {
  if(multiple) {
    x <- predict_xts_multiple(model, dataset, h=h, w=w)
  } else {
    x <- predict_xts(model, dataset, h=h, w=w)
  }
  
  tb <- x %>% xts_to_df() %>% as_tibble()

  require(ggplot2)
  tb %>% select(-c(Open, High, Low, Volume)) %>%
    pivot_longer(-c(index)) %>%
    ggplot(aes(x = index, y = value)) +
    geom_line(aes(color = name)) +
    labs(title = model_name, subtitle = dataset_name)
}

plotly_ml <- function(...) {
  require(plotly)
  plot_ml(...) %>% ggplotly()
}

order_size <- 1
fees <- -5

dataset_path <- "~/datasets/jcr2020/exp3/d1_dax_2019.csv"
dataset_name <- basename(dataset_path) %>% stringr::str_remove(".csv")
dataset <- read_ohlcv(dataset_path)

model_path <- "~/datasets/jcr2020/exp3/h24_w12_dDAX_D1_lm.rds"
model_name <- basename(model_path) %>% stringr::str_remove(".rds")
model <- readr::read_rds(model_path)

base_strategy <- macd(fees = fees, order_size = order_size)

# combined_results <- combined_analysis(
#   dataset,
#   dataset_name,
#   model,
#   model_name,
#   h = 1,
#   w = 12,
#   base_strategy,
#   fees,
#   order_size,
#   verbose = F
# )

exp_results <- tibble(h = 1:24, w = 12) %>%
  mutate(results = map2(h, w, ~combined_analysis(
    dataset,
    dataset_name,
    model,
    model_name,
    h = .x,
    w = .y,
    base_strategy,
    fees,
    order_size,
    verbose = F
  )))

exp_stats <- exp_results %>%
  mutate(stats = map(results, ~ .x %>% map_df('stats'))) %>% # extract stats from each result
  unnest(stats) %>%
  select(h, w, Symbol, model, strategy, Num.Txns, Num.Trades, Net.Trading.PL, Gross.Profits)

exp_stats %>%
  group_by(h) %>% 
  slice_max(Net.Trading.PL, 1) %>% print(n=50)

# plotly_ml(dataset, dataset_name, model, model_name, h = 24, w = 12)