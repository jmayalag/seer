library(quantmod)
library(OHLCMerge)
library(dplyr)
library(seer)
library(quantstrat)

#' Conservative strategy
#'
#' el dataset requiere las columnas
#' - ind_enter, ind_exit: Indicadores tecnicos
#' - pred_bull, pred_bear: Indicadores predictivos
#'
#' Enter: prediction & signal
#' @param fees costo numerico o funcion de costo
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
conservative_strat <- function(fees = 0) {
  # Order params
  order_size <- 100
  
  strat <- strategy(paste("conservative_ml", sep = "_"))
  
  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "ind_enter & pred_bull",
      cross = TRUE
    ),
    label = "sig_long_enter"
  )
  
  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "ind_exit | pred_bear",
      cross = TRUE
    ),
    label = "sig_long_exit"
  )
  
  strat <- add.rule(
    strategy = strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "sig_long_enter",
      sigval = TRUE,
      orderqty = order_size,
      ordertype = "market",
      orderside = "long",
      prefer = "Open",
      TxnFees = fees,
      replace = FALSE
    ),
    type = "enter",
    label = "rule_enter_long"
  )
  
  strat <- add.rule(
    strategy = strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "sig_long_exit",
      sigval = TRUE,
      orderside = "long",
      ordertype = "market",
      orderqty = "all",
      TxnFees = fees,
      replace = TRUE
    ),
    type = "exit",
    label = "rule_exit_long"
  )
  
  strat
}

#' Risky strategy
#'
#' el dataset requiere las columnas
#' - ind_enter, ind_exit: Indicadores tecnicos
#' - pred_bull, pred_bear: Indicadores predictivos
#'
#' Enter: prediction & signal
#' @param fees costo numerico o funcion de costo
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
risky_strat <- function(base_strategy, fees = 0) {
  # Order params
  order_size <- 100
  
  strat <- strategy(paste("risky_ml", sep = "_"))
  
  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "ind_enter & pred_bull",
      cross = TRUE
    ),
    label = "sig_long_enter"
  )
  
  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "ind_exit | pred_bear",
      cross = TRUE
    ),
    label = "sig_long_exit"
  )
  
  strat <- add.rule(
    strategy = strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "sig_long_enter",
      sigval = TRUE,
      orderqty = order_size,
      ordertype = "market",
      orderside = "long",
      prefer = "Open",
      TxnFees = fees,
      replace = FALSE
    ),
    type = "enter",
    label = "rule_enter_long"
  )
  
  strat <- add.rule(
    strategy = strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "sig_long_exit",
      sigval = TRUE,
      orderside = "long",
      ordertype = "market",
      orderqty = "all",
      TxnFees = fees,
      replace = TRUE
    ),
    type = "exit",
    label = "rule_exit_long"
  )
}

order_size <- 100
fees <- 0

set.seed(42)
mkt <- read_ohlcv("~/datasets/datasets_paper/ES35_D1.csv") %>% head(15)
mkt$pred_bear <- runif(nrow(mkt)) < 0.5
mkt$pred_bull <- runif(nrow(mkt)) > 0.5
mkt$ind_enter <- runif(nrow(mkt)) > 0.5
mkt$ind_exit <- runif(nrow(mkt)) > 0.8
ES35_D1 <- mkt


strat <- conservative_strat()
  
indi <- applyIndicators(strat, mktdata = mkt)
sign <- applySignals(strat, mkt, indicators = indi)
result <- backtest.opt(strat, "ES35_D1")