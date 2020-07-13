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
#' @param approach debe existir la funcion [approach]_signals. Por defecto puede ser una de `c("risky", "conservative")`
#' @param fees costo numerico o funcion de costo
#' @param order_size volumen de la orden
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
ml_strat <- function(base_strategy, approach, fees = 0, order_size = 1) {
  
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


#' Apply ML indicator and signals
#'
#' @param dataset the xts
#' @param base_strategy base technical analysis strategy
#' @param model the ML model
#' @param h prediction horizon
#' @param w historic window
#'
#' @return
#' @export
#' @importFrom dplyr if_else
#' @importFrom tidyr replace_na
ml_apply_indicator_signals <- function(dataset, base_strategy, model, h, w) {
  mkt <- predict_xts(model, dataset, h = h, w = w)
  pred_col <- sprintf("h%d", h)
  
  mkt <- apply_indicator_signals(base_strategy, mkt)
  mkt$enterLong <- replace_na(mkt$enterLong, 0)
  mkt$exitLong <- replace_na(mkt$exitLong, 0)
  
  mkt$pred_bull_ind <- mkt[, pred_col] > mkt$Close
  mkt$pred_bear_ind <- mkt[, pred_col] < mkt$Close
  
  # Crossover signal, 1 only for the first observation in a row
  mkt$pred_bull <- mkt$pred_bull_ind - replace_na(lag.xts(mkt$pred_bull_ind), 0)
  mkt$pred_bull <- if_else(mkt$pred_bull == 1, 1, 0)
  mkt$pred_bear <- mkt$pred_bear_ind - replace_na(lag.xts(mkt$pred_bear_ind), 0)
  mkt$pred_bear <- if_else(mkt$pred_bear == 1, 1, 0)
  
  mkt
}