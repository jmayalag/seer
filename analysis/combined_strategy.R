library(quantmod)
library(OHLCMerge)
library(dplyr)
library(seer)
library(quantstrat)

conservative_signals <- function(strat) {
  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "enterLong & pred_bull",
      cross = TRUE
    ),
    label = "enterLongPred"
  )

  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "exitLong | pred_bear",
      cross = TRUE
    ),
    label = "exitLongPred"
  )

  strat
}

risky_signals <- function(strat) {
  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "enterLong | pred_bull",
      cross = TRUE
    ),
    label = "enterLongPred"
  )

  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "exitLong | pred_bear",
      cross = TRUE
    ),
    label = "exitLongPred"
  )

  strat
}


#' ML strategy
#'
#' el dataset requiere las columnas
#' - enterLong, exitLong: Indicadores tecnicos
#' - pred_bull, pred_bear: Indicadores predictivos
#'
#' Enter: prediction & signal
#' @param base_strategy nombre de la estrategia de analisis tecnico
#' @param approach debe existir la funcion [approach]_signals
#' @param fees costo numerico o funcion de costo
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
ml_strat <- function(base_strategy, approach, fees = 0) {
  # Order params
  order_size <- 100

  strat <- strategy(paste(approach, "ml", base_strategy, sep = "_"))
  approach_fun <- get(paste0(approach, "_signals"))

  strat <- approach_fun(strat)

  strat <- add.rule(
    strategy = strat,
    name = "ruleSignal",
    arguments = default_rule_arguments(
      order_size = order_size,
      fees = fees,
      sigcol = "enterLongPred",
      sigval = TRUE,
      orderside = "long",
      replace = FALSE
    ),
    type = "enter",
    label = "rule_enterLongPred"
  )

  strat <- add_exit_rule(strategy = strat, fees = fees, sigcol = "exitLongPred")

  if (getOption("sell_at_end", TRUE)) {
    .sell_at_end(strat, fees = fees)
  } else {
    strat
  }
}

library(randomForest)
library(seer)
library(ggplot2)
library(dplyr)
library(tidyr)

mkt <- read_ohlcv("~/datasets/datasets_paper/ES35_D1.csv")
model_path <- "results/models/h1_w3_dES35_D1_rf.rds"
params <- parse_params(model_path)


fit <- readr::read_rds(model_path)

model <- fit$finalModel

x <- mkt
x_pred <- predict_xts(model, x, h = params$h, w = params$w)

order_size <- 100
fees <- 0

mkt <- x_pred
set.seed(42)
base_strategy <- macd()
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

ES35_D1 <- mkt

strat <- ml_strat(base_strategy$name, approach = "conservative")
result_conservative <- backtest.opt(strat, "ES35_D1")

strat <- ml_strat(base_strategy$name, approach = "risky")
result_risky <- backtest.opt(strat, "ES35_D1")

debg <- seer::apply_indicator_signals(strat, ES35_D1)

strat <- base_strategy
result_macd <- backtest.opt(strat, "ES35_D1")

stats <- list(result_semiconservative, result_conservative, result_risky, result_macd) %>%
  purrr::map_df(~ .x$stats) %>%
  as_tibble()

stats %>% select(strategy, Symbol, Num.Txns, Num.Trades, Net.Trading.PL, Avg.Trade.PL, Gross.Profits, Gross.Losses, Max.Drawdown)

tb <- xts_to_df(x_pred) %>%
  as_tibble() %>%
  mutate(pred_bull_ind = as.numeric(pred > Close), pred_bear_ind = as.numeric(pred < Close)) %>%
  mutate(pred_bull = pred_bull_ind - replace_na(lag(pred_bull_ind), 0)) %>%
  mutate(pred_bear = pred_bear_ind - replace_na(lag(pred_bear_ind), 0)) %>%
  mutate(pred_bull = if_else(pred_bull == 1, 1, 0), pred_bear = if_else(pred_bear == 1, 1, 0))

tb
