library(seer)
library(randomForest)
library(dplyr)
library(tidyr)
library(purrr)

ml_apply_indicator_signals <- function(dataset, base_strategy, model, h, w) {
  require(xts)
  mkt <- predict_xts(model, dataset, h = h, w = w)
  
  mkt <- seer::apply_indicator_signals(base_strategy, mkt)
  mkt$enterLong <- replace_na(mkt$enterLong, 0)
  mkt$exitLong <- replace_na(mkt$exitLong, 0)
  
  mkt$pred_bull_ind <- mkt$pred > mkt$Close
  mkt$pred_bear_ind <- mkt$pred < mkt$Close
  
  # Crossover signal, 1 only for the first observation in a row
  mkt$pred_bull <- mkt$pred_bull_ind - replace_na(lag.xts(mkt$pred_bull_ind), 0)
  mkt$pred_bull <- if_else(mkt$pred_bull == 1, 1, 0)
  mkt$pred_bear <- mkt$pred_bear_ind - replace_na(lag.xts(mkt$pred_bear_ind), 0)
  mkt$pred_bear <- if_else(mkt$pred_bear == 1, 1, 0)
  
  mkt
}

combined_analysis <- function(dataset, dataset_name, model, model_name, base_strat, fees = 0, order_size = 1, verbose = F) {
  params <- parse_params(model_name)
  mkt <- ml_apply_indicator_signals(dataset, base_strat, model, h=params$h, w=params$w)
  
  dataset_name <- stringr::str_replace(dataset_name, "[^a-zA-z0-9_.]", "_")
  assign(dataset_name, mkt, envir = .GlobalEnv)
  
  strat <- ml_strat(base_strat$name, approach = "conservative", fees = fees, order_size = order_size)
  message(paste0(strat$name, "..."))
  result_conservative <- backtest.opt(strat, dataset_name, silent = T)
  result_conservative$stats$model <- model_name
  
  strat <- ml_strat(base_strat$name, approach = "risky", fees = fees, order_size = order_size)
  message(paste0(strat$name, "..."))
  result_risky <- backtest.opt(strat, dataset_name, silent = T)
  result_risky$stats$model <- model_name
  
  # debug <- seer::apply_indicator_signals(strat, get(dataset_name, envir = .GlobalEnv))
  
  strat <- base_strat
  message(paste0(strat$name, "..."))
  result_base <- backtest.opt(strat, dataset_name, silent = T)
  
  results <- list(result_base, result_conservative, result_risky)
  results
}

order_size <- 1
fees <- -5

dataset_path <- "~/datasets/datasets_paper/d1_ibex35_2000-2018.csv"
dataset_name <- basename(dataset_path) %>% stringr::str_remove(".csv")
dataset <- read_ohlcv(dataset_path)

model_path <- "results/models/h1_w3_dES35_D1_rf.rds"
model_name <- basename(model_path) %>% stringr::str_remove(".rds")
fit <- readr::read_rds(model_path)
model <- fit$finalModel

base_strategy <- macd(fees = fees, order_size = order_size)

combined_results <- combined_analysis(
  dataset,
  dataset_name,
  model,
  model_name,
  base_strategy,
  fees,
  order_size
)

combined_results %>%
  map_df(~ .x$stats)
