#' Indica posiciones que componen un patron de vela complejo.
#'
#' @param flag vector logico que indica que posiciones son parte del patron de vela.
#' @param sessions numero de velas que componen el patron.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' multiple(flag = c(TRUE, TRUE, FALSE), sessions = 2)
multiple <- function(flag, sessions = 2) {
  # k negativo para aplicar un lead
  seq(0, sessions - 1) %>% lapply(function(k) lag.xts(flag, -k)) %>% Reduce(`|`, .data)
}


#' Calcula el momento de una tendencia.
#'
#' @param df datos OHLC.
#' @param value el valor utilizado para el calculo.
#' @param days dias en consideracion.
#' @return valor numérico.
#' @export
momentum <- function(df, value = Cl, days = 5) {
  EMA(value(df), n = days)
}


#' Indica si es una tendencia alcista.
#'
#' @param df datos OHLC.
#' @param value el valor utilizado para el calculo.
#' @param days dias en consideracion.
#' @return `TRUE` si es alcista, `FALSE` en otro caso.
#' @export
is.uptrend <- function(df, value = Cl, days = 5) {
  Cl(df) > momentum(df)
}


#' Indica si es una tendencia bajista
#'
#' @param df datos OHLC.
#' @param value el valor utilizado para el calculo.
#' @param days dias en consideracion.
#' @return `TRUE` si es bajista, `FALSE` en otro caso.
#' @export
is.downtrend <- function(df, value = Cl, days = 5) {
  Cl(df) < momentum(df)
}


#' lower_shadow
#'
#' @param df datos OHLC.
#' @export
lower_shadow <- function(df) {
  pmin(Op(df), Cl(df)) - Lo(df)
}


#' upper_shadow
#'
#' @param df datos OHLC.
#' @export
upper_shadow <- function(df) {
  Hi(df) - pmax(Op(df), Cl(df))
}


#' real_body
#'
#' @param df datos OHLC.
#' @export
real_body <- function(df) {
  abs(Op(df) - Cl(df))
}


#' Comprueba si una vela es bajista.
#' Cierre < Apertura
#'
#' @param df datos OHLC.
#' @export
is.bearish <- function(df) {
  Op(df) > Cl(df)
}


#' Comprueba si una vela es alcista.
#' Cierre > Apertura
#'
#' @param df datos OHLC.
#' @export
is.bullish <- function(df) {
  Cl(df) > Op(df)
}


#' is.small.lower_shadow
#'
#' @param df datos OHLC.
#' @export
is.small.lower_shadow <- function(df) {
  lower_shadow(df) < real_body(df) / 4
}


#' is.small.upper_shadow
#'
#' @param df datos OHLC.
#' @export
is.small.upper_shadow <- function(df) {
  upper_shadow(df) < real_body(df) / 4
}


#' is.doji
#'
#' @param df datos OHLC.
#' @export
is.doji <- function(df) {
  doji_max_body <- getOption("doji_max_body", default = 0)
  real_body(df) <= doji_max_body
}


#' is.dragonfly
#'
#' @param df datos OHLC.
#' @export
is.dragonfly <- function(df) {
  is.doji(df) & is.small.upper_shadow(df)
}


#' is.gravestone
#'
#' @param df datos OHLC.
#' @export
is.gravestone <- function(df) {
  is.doji(df) & is.small.lower_shadow(df)
}


#' is.spinning_top
#'
#' @param df datos OHLC.
#' @export
is.spinning_top <- function(df) {
  rbody <- real_body(df)
  lower_shadow(df) >= rbody & upper_shadow(df) >= rbody
}


#' is.marubozu
#'
#' @param df datos OHLC.
#' @export
is.marubozu <- function(df) {
  lower_shadow(df) == 0 & upper_shadow(df) == 0
}


#' is.hanging_man
#'
#' @param df datos OHLC.
#' @export
is.hanging_man <- function(df) {
  is.uptrend(df) & lower_shadow(df) > 2 * real_body(df) & is.small.upper_shadow(df)
}


#' is.hammer
#'
#' @param df datos OHLC.
#' @export
is.hammer <- function(df) {
  is.downtrend(df) & lower_shadow(df) > 2 * real_body(df) & is.small.upper_shadow(df)
}


#' is.belt_hold.bullish
#'
#' @param df datos OHLC.
#' @export
is.belt_hold.bullish <- function(df) {
  is.downtrend(df) & is.bullish(df) & lower_shadow(df) == 0 & is.small.upper_shadow(df)
}


#' is.belt_hold.bearish
#'
#' @param df datos OHLC.
#' @export
is.belt_hold.bearish <- function(df) {
  is.uptrend(df) & is.bearish(df) & upper_shadow(df) == 0 & is.small.lower_shadow(df)
}


#' is.shooting_star
#'
#' @param df datos OHLC.
#' @export
is.shooting_star <- function(df) {
  is.uptrend(df) & upper_shadow(df) > 2 * real_body(df) & is.small.lower_shadow(df)
}


#' is.engulfing.bullish
#'
#' @param df datos OHLC.
#' @export
is.engulfing.bullish <- function(df) {
  prev <- lag.xts(df)
  f <- is.downtrend(df) & is.bullish(df) & is.bearish(prev) & Cl(df) > Op(prev) & Op(df) < Cl(prev)
  multiple(f, 2)
}


#' is.engulfing.bearish
#'
#' @param df datos OHLC.
#' @export
is.engulfing.bearish <- function(df) {
  prev <- lag.xts(df)
  f <- is.uptrend(df) & is.bullish(df) & is.bearish(prev) & Cl(df) > Op(prev) & Op(df) < Cl(prev)
  multiple(f, 2)
}


#' is.harami.bullish
#'
#' @param df datos OHLC.
#' @export
is.harami.bullish <- function(df) {
  prev <- lag.xts(df)
  f <- is.downtrend(df) & is.bearish(df) & is.bullish(prev) & Cl(df) < Op(prev) & Op(df) > Cl(prev)
  multiple(f, 2)
}


#' is.harami.bearish
#'
#' @param df datos OHLC.
#' @export
is.harami.bearish <- function(df) {
  prev <- lag.xts(df)
  is.uptrend(df) & is.bearish(df) & is.bullish(prev) & Cl(df) > Op(prev) & Op(df) < Cl(prev)
}


#' is.separating_line.bullish
#'
#' @param df datos OHLC.
#' @export
is.separating_line.bullish <- function(df) {
  prev <- lag.xts(df)
  f <- is.uptrend(df) & is.bullish(df) & is.bearish(df) & Op(df) == Op(prev)
  multiple(f, 2)
}


#' is.separating_line.bearish
#'
#' @param df datos OHLC.
#' @export
is.separating_line.bearish <- function(df) {
  prev <- lag.xts(df)
  f <- is.downtrend(df) & is.bearish(df) & is.bullish(prev) & Op(df) == Op(prev)
  multiple(f, 2)
}


#' is.counter_attack_line.bullish
#'
#' @param df datos OHLC.
#' @export
is.counter_attack_line.bullish <- function(df) {
  prev <- lag.xts(df)
  f <- is.downtrend(df) & is.bullish(df) & is.bearish(prev) & Cl(df) == Cl(prev)
  multiple(f, 2)
}


#' is.counter_attack_line.bearish
#'
#' @param df datos OHLC.
#' @export
is.counter_attack_line.bearish <- function(df) {
  prev <- lag.xts(df)
  f <- is.uptrend(df) & is.bearish(df) & is.bullish(prev) & Cl(df) == Cl(prev)
  multiple(f, 2)
}


#' is.three_white_soldiers
#'
#' @param df datos OHLC.
#' @export
is.three_white_soldiers <- function(df) {
  prev <- lag.xts(df)
  pprev <- lag.xts(df, 2)
  f <- is.downtrend(df) & is.bullish(df) & is.bullish(prev) & is.bullish(pprev) & is.small.upper_shadow(df) &
    is.small.upper_shadow(prev) & is.small.upper_shadow(pprev) & Op(df) < Cl(prev) & Op(df) > Op(prev) &
    Op(prev) < Cl(pprev) & Op(prev) > Op(pprev)
  multiple(f, 3)
}


#' is.three_black_crows
#'
#' @param df datos OHLC.
#' @export
is.three_black_crows <- function(df) {
  prev <- lag.xts(df)
  pprev <- lag.xts(df, 2)
  f <- is.uptrend(df) &
    is.bearish(df) & is.bearish(prev) & is.bearish(pprev) &
    is.small.lower_shadow(df) & is.small.lower_shadow(prev) & is.small.lower_shadow(pprev) &
    Op(df) > Cl(prev) & Op(df) < Op(prev) &
    Op(prev) > Cl(pprev) & Op(prev) < Op(pprev)
  multiple(f, 3)
}


#' is.morning_star
#'
#' @param df datos OHLC.
#' @export
is.morning_star <- function(df) {
  prev <- lag.xts(df)
  pprev <- lag.xts(df, 2)
  f <- is.downtrend(df) & is.bullish(df) & is.bearish(pprev) & Cl(prev) < Cl(pprev) & Op(prev) < Cl(pprev) &
    Cl(df) >= (Op(pprev) + Cl(pprev)) / 2
  multiple(f, 3)
}


#' is.evening_star
#'
#' @param df datos OHLC.
#' @export
is.evening_star <- function(df) {
  prev <- lag.xts(df)
  pprev <- lag.xts(df, 2)
  f <- is.uptrend(df) & is.bearish(df) & is.bullish(pprev) & Cl(prev) > Cl(pprev) & Op(prev) > Cl(pprev) &
    Cl(df) <= (Op(pprev) + Cl(pprev)) / 2
  multiple(f, 3)
}


#' is.three_methods.bullish
#'
#' @param df datos OHLC.
#' @export
is.three_methods.bullish <- function(df) {
  prev1 <- lag.xts(df, 1)
  prev2 <- lag.xts(df, 2)
  prev3 <- lag.xts(df, 3)
  prev4 <- lag.xts(df, 4)
  f <- is.uptrend(df) & is.bullish(df) & is.bullish(prev4) & Cl(df) > Cl(prev4) & Cl(prev1) > Op(prev4) &
    Cl(prev2) > Op(prev4) & Cl(prev3) > Op(prev4) & Op(prev1) < Cl(prev4) & Op(prev2) < Cl(prev4) &
    Op(prev3) < Cl(prev4) & Op(df) < Cl(prev1) & Op(df) < Cl(prev2) & Op(df) < Cl(prev3)
  multiple(f, 5)
}


#' is.three_methods.bearish
#'
#' @param df datos OHLC.
#' @export
is.three_methods.bearish <- function(df) {
  prev1 <- lag.xts(df, 1)
  prev2 <- lag.xts(df, 2)
  prev3 <- lag.xts(df, 3)
  prev4 <- lag.xts(df, 4)
  f <- is.downtrend(df) & is.bearish(df) & is.bearish(prev4) & Cl(df) < Cl(prev4) & Cl(prev1) < Op(prev4) &
    Cl(prev2) < Op(prev4) & Cl(prev3) < Op(prev4) & Op(prev1) > Cl(prev4) & Op(prev2) < Cl(prev4) &
    Op(prev3) > Cl(prev4) & Op(df) > Cl(prev1) & Op(df) > Cl(prev2) & Op(df) > Cl(prev3)
  multiple(f, 5)
}
