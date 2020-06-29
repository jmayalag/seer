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