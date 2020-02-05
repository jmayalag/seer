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
      formula = "exitLong & pred_bear",
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

  strat
}

order_size <- 100
fees <- 0

set.seed(42)
mkt <- read_ohlcv("~/datasets/datasets_paper/ES35_D1.csv")
base_strategy <- macd()
mkt <- seer::apply_indicator_signals(base_strategy, mkt)
mkt$pred_bear <- runif(nrow(mkt)) < 0.5
mkt$pred_bull <- runif(nrow(mkt)) > 0.5
ES35_D1 <- mkt

strat <- ml_strat("macd", approach = "conservative")
result <- backtest.opt(strat, "ES35_D1")

strat <- ml_strat("macd", approach = "risky")
result <- backtest.opt(strat, "ES35_D1")
