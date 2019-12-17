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
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
#'
#' @examples
#' strat <- triple_crossover()
triple_crossover <- function(nFast = 10, nMedium = 25, nSlow = 50, fees = 0) {
  if (nFast >= nMedium) {
    stop("nFast debe ser menor que nMedium")
  }

  if (nMedium >= nSlow) {
    stop("nMedium debe ser menor que nSlow")
  }

  # Order params
  order_size <- 100

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
    label = "long"
  )

  strat <- add.signal(
    strategy = strat,
    name = "sigFormula",
    arguments = list(
      formula = "(EMA.nFast < EMA.nMedium) & (EMA.nFast < EMA.nSlow)",
      cross = TRUE
    ),
    label = "short"
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
    arguments = list(
      sigcol = "long",
      sigval = TRUE,
      orderqty = order_size,
      ordertype = "market",
      orderside = "long",
      prefer = "Open",
      TxnFees = fees,
      replace = FALSE
    ),
    type = "enter",
    label = "EnterLONG"
  )

  strat <- add.rule(
    strategy = strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "exitLong",
      sigval = TRUE,
      orderside = "long",
      ordertype = "market",
      orderqty = "all",
      TxnFees = fees,
      replace = TRUE
    ),
    type = "exit",
    label = "Exit2"
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
macd <- function(fastMA = 12, slowMA = 26, fees = 0) {
  # Order params
  order_size <- 100
  trx_fee <- 10

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
  strat <- add.signal(strat,
    name = "sigThreshold",
    arguments = list(
      column = "signal._",
      relationship = "gt",
      threshold = 0,
      cross = TRUE
    ),
    label = "signal.gt.zero"
  )

  strat <- add.signal(strat,
    name = "sigThreshold",
    arguments = list(
      column = "signal._",
      relationship = "lt",
      threshold = 0,
      cross = TRUE
    ),
    label = "signal.lt.zero"
  )


  ### Agrega reglas ###
  # Determina la position a tomar de acuerdo a las señales, que tipo de orden realizar y cuantos
  # shares comprar
  strat <- add.rule(strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "signal.gt.zero",
      sigval = TRUE,
      orderqty = 100,
      ordertype = "market",
      orderside = "long",
      threshold = NULL,
      TxnFees = fees
    ),
    type = "enter",
    label = "enter",
    storefun = FALSE
  )

  strat <- add.rule(strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "signal.lt.zero",
      sigval = TRUE,
      orderqty = "all",
      ordertype = "market",
      orderside = "long",
      threshold = NULL,
      orderset = "exit2",
      TxnFees = fees
    ),
    type = "exit",
    label = "exit"
  )

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
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
#'
#' @examples
#' strat <- macdhist()
macdhist <- function(fastMA = 12, slowMA = 26, fees = 0) {
  # Order params
  order_size <- 100
  trx_fee <- 10

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
  strat <- add.signal(strat,
    name = "sigCrossover",
    arguments = list(
      columns = c("macd._", "signal._"),
      relationship = "gt"
    ),
    label = "hist.gt.zero"
  )

  strat <- add.signal(strat,
    name = "sigCrossover",
    arguments = list(
      columns = c("macd._", "signal._"),
      relationship = "lte"
    ),
    label = "hist.lte.zero"
  )


  ### Agrega reglas ###
  # Determina la position a tomar de acuerdo a las señales, que tipo de orden realizar y cuantos
  # shares comprar
  strat <- add.rule(strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "hist.gt.zero",
      sigval = TRUE,
      orderqty = 100,
      ordertype = "market",
      orderside = "long",
      threshold = NULL,
      TxnFees = fees
    ),
    type = "enter",
    label = "enter"
  )

  strat <- add.rule(strat,
    name = "ruleSignal",
    arguments = list(
      sigcol = "hist.lte.zero",
      sigval = TRUE,
      orderqty = "all",
      ordertype = "market",
      orderside = "long",
      threshold = NULL,
      orderset = "exit2",
      TxnFees = fees
    ),
    type = "exit",
    label = "exit"
  )

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
#'
#' @return un objeto Strategy
#' @export
#' @importFrom quantstrat strategy add.indicator add.rule add.signal
#'
#' @examples
#' strat <- buy_and_hold()
buy_and_hold <- function(fees = 0) {
  # Order params
  order_size <- 100
  trx_fee <- 10

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
    label = "enter",
    storefun = FALSE
  )

  .sell_at_end(strat, fees = fees)
}


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
    arguments = list(
      sigcol = "sig_sell_end",
      sigval = 1,
      orderside = "long",
      ordertype = "market",
      orderqty = "all",
      TxnFees = fees,
      replace = TRUE
    ),
    type = "exit",
    label = "exit_end"
  )

  strat
}
