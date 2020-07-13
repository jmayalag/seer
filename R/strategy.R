#' Retorna una lista con argumentos por defecto
#'
#' @param order_size numero o "all"
#' @param fees
#' @param ... argumentos adicionales de ruleSignal. Reemplaza a los defaults
#'
#' @return
#' @export
#'
#' @examples
#' default_rule_arguments(100, 0, orderside = "long")
default_rule_arguments <- function(order_size, fees, ...) {
  defaults <- list(
    orderqty = order_size,
    ordertype = "market",
    prefer = "Open", # Utiliza el precio de apertura para la compra
    TxnFees = fees
  )
  args <- list(...)
  modifyList(defaults, args)
}

#' Agrega una regla de salida del mercado
#'
#' @param strategy el objeto strategy
#' @param fees
#' @param sigCol nombre del signal de salida
#' @param label nombre a utilizar para la regla
#' @param orderside tipo de operacion (long | short)
#'
#' @return la estrategia con esta regla agregada
#'
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal ruleSignal
add_exit_rule <- function(strategy, fees, sigcol = "exitLong", label = paste0("rule_", sigcol), orderside = "long") {
  add.rule(
    strategy = strategy,
    name = "ruleSignal",
    arguments = default_rule_arguments(
      order_size = "all",
      fees = fees,
      sigcol = sigcol,
      sigval = TRUE,
      orderside = orderside,
      ordertype = "market",
      replace = TRUE
    ),
    type = "exit",
    label = label
  )
}


#' Triple Crossover Strategy
#' Consiste en tener tres MAs de diferentes velocidades
#' - Fast MA
#' - Medium MA
#' - Slow MA
#'
#' @param nFast EMA Rapido
#' @param nMedium EMA Medio
#' @param nSlow EMA Lento
#' @param fees costo numerico o funcion de costo
#' @param order_size cantidad a comprar
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
#'
#' @examples
#' strat <- triple_crossover()
triple_crossover <- function(nFast = 10, nMedium = 25, nSlow = 50, fees = 0, order_size = 1) {
  if (nFast >= nMedium) {
    stop("nFast debe ser menor que nMedium")
  }

  if (nMedium >= nSlow) {
    stop("nMedium debe ser menor que nSlow")
  }

  # Order params

  strat <- strategy(paste("triple_crossover", nFast, nMedium, nSlow, sep = "_"))

  # Agrega indicadores
  strat <- add.indicator(
    strategy = strat,
    name = "EMA",
    arguments = list(x = quote(Cl(mktdata)), n = nFast),
    label = "nFast"
  )

  strat <- add.indicator(
    strategy = strat,
    name = "EMA",
    arguments = list(x = quote(Cl(mktdata)), n = nMedium),
    label = "nMedium"
  )

  strat <- add.indicator(
    strategy = strat,
    name = "EMA",
    arguments = list(x = quote(Cl(mktdata)), n = nSlow),
    label = "nSlow"
  )

  # Agrega señales
  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "(EMA.nFast > EMA.nMedium) & (EMA.nFast > EMA.nSlow)",
      cross = TRUE
    ),
    label = "enterLong"
  )

  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "(EMA.nFast < EMA.nMedium) & (EMA.nFast < EMA.nSlow)",
      cross = TRUE
    ),
    label = "enterShort"
  )

  strat <- add.signal(
    strategy = strat,
    name = "sigCrossover",
    arguments = list(
      columns = c("nFast", "nMedium"),
      relationship = "lte"
    ),
    label = "exitLong"
  )

  strat <- add.signal(
    strategy = strat,
    name = "sigCrossover",
    arguments = list(
      columns = c("nFast", "nMedium"),
      relationship = "gte"
    ),
    label = "exitShort"
  )

  ### Agrega reglas ###
  # Determina la position a tomar de acuerdo a las señales, que tipo de orden realizar y cuantos
  # shares comprar
  strat <- add.rule(
    strategy = strat,
    name = "ruleSignal",
    arguments = default_rule_arguments(
      order_size = order_size,
      fees = fees,
      sigcol = "enterLong",
      sigval = TRUE,
      orderside = "long",
      prefer = "Open",
      replace = FALSE
    ),
    type = "enter",
    label = "rule_enterLong"
  )

  strat <- add_exit_rule(
    strategy = strat,
    fees = fees,
    sigcol = "exitLong",
    label = "rule_exitLong"
  )

  if (getOption("sell_at_end", TRUE)) {
    .sell_at_end(strat, fees = fees)
  } else {
    strat
  }
}

#' MACD Strategy
#'
#' @param fastMA periodos del fast MA
#' @param slowMA periodos del slow MA
#' @param fees costo numerico o funcion de costo
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
#'
#' @examples
#' strat <- macd()
macd <- function(fastMA = 12, slowMA = 26, fees = 0, order_size = 1) {
  strat <- strategy(paste("macd", fastMA, slowMA, sep = "_"))

  # Agrega indicadores
  strat <- add.indicator(strat,
    name = "MACD",
    arguments = list(
      x = quote(Cl(mktdata)),
      nFast = fastMA,
      nSlow = slowMA
    ),
    label = "_"
  )

  ### Signals
  # Signal > 0
  strat <- add.signal(strat,
    name = "sigThreshold",
    arguments = list(
      column = "signal._",
      relationship = "gt",
      threshold = 0,
      cross = TRUE
    ),
    label = "enterLong"
  )

  strat <- add.signal(strat,
    name = "sigThreshold",
    arguments = list(
      column = "signal._",
      relationship = "lt",
      threshold = 0,
      cross = TRUE
    ),
    label = "exitLong"
  )


  ### Agrega reglas ###
  # Determina la position a tomar de acuerdo a las señales, que tipo de orden realizar y cuantos
  # shares comprar
  strat <- add.rule(strat,
    name = "ruleSignal",
    arguments = default_rule_arguments(
      order_size = order_size,
      fees = fees,
      sigcol = "enterLong",
      sigval = TRUE,
      ordertype = "market",
      orderside = "long"
    ),
    type = "enter",
    label = "rule_enterLong"
  )

  strat <- add_exit_rule(strategy = strat, fees = fees, sigcol = "exitLong", label = "rule_exitLong")

  if (getOption("sell_at_end", TRUE)) {
    .sell_at_end(strat, fees = fees)
  } else {
    strat
  }
}

#' MACD hist Strategy
#'
#' @param fastMA periodos del fast MA
#' @param slowMA periodos del slow MA
#' @param fees costo numerico o funcion de costo
#' @param order_size cantidad a comprar
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
#'
#' @examples
#' strat <- macdhist()
macdhist <- function(fastMA = 12, slowMA = 26, fees = 0, order_size = 1) {
  strat <- strategy(paste("macdhist", fastMA, slowMA, sep = "_"))

  # Agrega indicadores
  strat <- add.indicator(strat,
    name = "MACD",
    arguments = list(
      x = quote(Cl(mktdata)),
      nFast = fastMA,
      nSlow = slowMA
    ),
    label = "_"
  )

  ### Signals
  # hist.gt.zero
  strat <- add.signal(strat,
    name = "sigCrossover",
    arguments = list(
      columns = c("macd._", "signal._"),
      relationship = "gt"
    ),
    label = "enterLong"
  )

  # hist.lte.zero
  strat <- add.signal(strat,
    name = "sigCrossover",
    arguments = list(
      columns = c("macd._", "signal._"),
      relationship = "lte"
    ),
    label = "exitLong"
  )


  ### Agrega reglas ###
  # Determina la position a tomar de acuerdo a las señales, que tipo de orden realizar y cuantos
  # shares comprar
  strat <- add.rule(strat,
    name = "ruleSignal",
    arguments = default_rule_arguments(
      order_size = order_size,
      fees = fees,
      sigcol = "enterLong",
      sigval = TRUE,
      ordertype = "market",
      orderside = "long"
    ),
    type = "enter",
    label = "rule_enterLong"
  )

  strat <- add_exit_rule(strategy = strat, fees = fees, sigcol = "exitLong", label = "rule_exitLong")

  if (getOption("sell_at_end", TRUE)) {
    .sell_at_end(strat, fees = fees)
  } else {
    strat
  }
}

#' Start of series indicator
#'
#' @param x xts
#'
#' @return returns 1 on the first observation
#' @export
#'
#' @examples
#' .e
.start_indicator <- function(x) {
  n <- nrow(x)
  values <- data.frame(start = c(1, rep(0, n - 1)))

  xts(values, index(x))
}

#' End of series indicator.
#'
#' @param x data
#'
#' @return returns 1 on the second to last observation
#' @export
#'
#' @examples
#' .end_indicator(AAPL)
.end_indicator <- function(x) {
  n <- nrow(x)
  # Second to last, so the sell order executes on the last period
  values <- data.frame(end = c(rep(0, n - 3), 1, 0, 0))

  xts(values, index(x))
}

#' Buy and Hold strategy
#' @param fees
#' @param order_size
#' 
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
#'
#' @examples
#' strat <- buy_and_hold()
buy_and_hold <- function(fees = 0, order_size = 100) {
  strat <- strategy("buy_and_hold")

  # Agrega indicadores
  strat <- add.indicator(strat,
    name = ".start_indicator",
    arguments = list(
      x = quote(Cl(mktdata))
    ),
    label = "start"
  )

  ### Signals
  strat <- add.signal(strat,
    name = "sigThreshold",
    arguments = list(
      column = "start",
      relationship = "gt",
      threshold = 0
    ),
    label = "sig_buy"
  )

  ### Agrega reglas ###
  # Determina la position a tomar de acuerdo a las señales, que tipo de orden realizar y cuantos
  # shares comprar
  strat <- add.rule(strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "sig_buy",
      sigval = 1,
      orderqty = 100,
      ordertype = "market",
      orderside = "long",
      threshold = NULL,
      TxnFees = fees
    ),
    type = "enter",
    label = "rule_enterLong",
    storefun = FALSE
  )

  .sell_at_end(strat, fees = fees)
}


#' Cierra las ordenes pendientes al final del backtest
#'
#' @param strat objeto strategy
#' @param fees 
#'
#' @return
#' @export
#' @importFrom quantstrat add.indicator add.signal add.rule ruleSignal sigThreshold
.sell_at_end <- function(strat, fees) {
  strat <- add.indicator(strat,
    name = ".end_indicator",
    arguments = list(
      x = quote(Cl(mktdata))
    ),
    label = "end"
  )

  strat <- add.signal(strat,
    name = "sigThreshold",
    arguments = list(
      column = "end",
      relationship = "gt",
      threshold = 0
    ),
    label = "sig_sell_end"
  )

  strat <- add.rule(strat,
    name = "ruleSignal",
    arguments = default_rule_arguments(
      order_size = "all",
      fees = fees,
      sigcol = "sig_sell_end",
      sigval = 1,
      orderside = "long",
      ordertype = "market",
      replace = TRUE
    ),
    type = "exit",
    label = "rule_exitEnd"
  )

  strat
}


#' Aplica los indicadores y señales al mktdata
#'
#' @param strategy objecto strategy
#' @param mktdata xts con datos OHLC
#'
#' @return xts con las columnas adicionales de indicadores y señales
#' @export
#'
#' @examples
apply_indicator_signals <- function(strategy, mktdata) {
  indicators <- quantstrat::applyIndicators(strategy = strategy, mktdata = mktdata)
  signals <- quantstrat::applySignals(strategy = strategy, mktdata = indicators)
  signals
}