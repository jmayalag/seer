#' Remove repetitions in a vector
#'
#' @param x vector of numbers
#'
#' @return
#' @export
#'
#' @examples
#' crossover(c(1, 0, 0, 1, 1, 1, 1, -1, -1))
crossover <- function(x) {
  x <- as.numeric(x)
  prev <- lag(x)
  prev[is.na(prev)] <- 0
  
  cross <- x - prev
  cross[is.na(cross)] <- 0
  cross[cross != 0] <- x[cross != 0]
  
  cross
}


#' Triple EMA strategy
#'
#' @param nFast n for fast EMA
#' @param nMedium n for medium EMA 
#' @param nSlow n for slow EMA
#'
#' @return a list object to pass to the backtest function
#' @export
#' @importFrom TTR EMA
#'
#' @examples
#' triple_ema(1, 5, 10)
triple_ema <- function(nFast, nMedium, nSlow) {
  strategy_fun <- function(x) {
    x$fast <- EMA(x$Close, n = nFast)
    x$medium <- EMA(x$Close, n = nMedium)
    x$slow <- EMA(x$Close, n = nSlow)
    
    x$enterLong <- crossover((x$fast > x$medium) & (x$fast > x$slow))
    x$exitLong <- crossover((x$fast <= x$medium) & (x$fast <= x$slow))
    
    x$buy <- tidyr::replace_na(x$enterLong == 1, 0)
    x$sell <- tidyr::replace_na(x$exitLong == 1, 0)
    x
  }
  
  name <- sprintf("tema_%d_%d_%d", nFast, nMedium, nSlow)
  
  strategy <- list(
    name = name,
    params = c(nFast, nMedium, nSlow),
    strategy = strategy_fun
  )
}

#' MACD strategy
#'
#' @param nFast n for fast EMA
#' @param nSlow n for slow EMA
#'
#' @return a list object to pass to the backtest function
#' @export
#'
#' @examples
#' macd(5, 12)
macd <- function(nFast, nSlow) {
  strategy_fun <- function(x) {
    x_macd <- TTR::MACD(x$Close, nFast = nFast, nSlow = nSlow)
    x$macd <- x_macd$macd
    x$signal <- x_macd$signal
    
    x$enterLong <- crossover(x$macd > x$signal)
    x$exitLong <- crossover(x$macd <= x$signal)
    
    x$buy <- tidyr::replace_na(x$enterLong == 1, 0)
    x$sell <- tidyr::replace_na(x$exitLong == 1, 0)
    x
  }
  
  name <- sprintf("macd_%d_%d", nFast, nSlow)
  
  strategy <- list(
    name = name,
    params = c(nFast, nSlow),
    strategy = strategy_fun
  )
}


#' Technical with ML strategy
#'
#' @param base_strategy the base technical strategy
#' @param model the ML model
#' @param h prediction horizon
#' @param w historical window
#' @param approach 
#' @param model_name name of the model. If not specified, the class name will be used.
#'
#' @return
#' @export
technical_ml <- function(base_strategy, model, h, w, approach = "risky", model_name = NULL) {
  strategy_fun <- function(x) {
    mkt <- predict_xts(model, x, h = h, w = w)
    pred_col <- sprintf("h%d", h)
    
    mkt <- base_strategy$strategy(mkt)
    
    mkt$pred_bull_ind <- crossover(mkt[, pred_col] > mkt$Close)
    mkt$pred_bear_ind <- crossover(mkt[, pred_col] <= mkt$Close)
    
    if (approach == "risky") {
      mkt$buy <- tidyr::replace_na(mkt$enterLong | mkt$pred_bull_ind == 1, 0)
      mkt$sell <- tidyr::replace_na(mkt$exitLong & mkt$pred_bear_ind == 1, 0)
    } else if (approach == "semirisky") {
      mkt$buy <- tidyr::replace_na(mkt$enterLong | mkt$pred_bull_ind == 1, 0)
      mkt$sell <- tidyr::replace_na(mkt$exitLong | mkt$pred_bear_ind == 1, 0)
    } else if (approach == "conservative") {
      mkt$buy <- tidyr::replace_na(mkt$enterLong & mkt$pred_bull_ind == 1, 0)
      mkt$sell <- tidyr::replace_na(mkt$exitLong | mkt$pred_bull_ind == 1, 0)
    } else {
      stop(paste0("Invalid approach ", approach))
    }
    mkt
  }
  model_name = if_else(is.null(model_name), sprintf("%s_%d_%d", class(model)[1], h, w), model_name)
  
  name <- sprintf("%s-ml_%s_%s", base_strategy$name, approach, model_name)
  
  strategy <- list(
    name = name,
    params = c(h, w, approach),
    strategy = strategy_fun
  )
}


#' Run the backtest with the specified parametrs
#'
#' @param data xts data
#' @param strat strategy list with the strat_fun
#' @param cost trade cost
#' @param qty order size
#' @param sell_at_end sell at the end of the backtest.
#' @param price_fun price function for buying and selling. By default, the opening price
#' @param debug return additional data for debugging
#'
#' @return
#' @export
backtest <- function(data, strat, cost, qty, sell_at_end = T, price_fun = quantmod::Op, debug = F) {
  x <- strat$strategy(data)
  backtest <- x[, c("buy", "sell")]
  backtest$order_price <- price_fun(x) # sell and buy price
  
  # Order is executed the next day
  backtest$order_buy <- xts::lag.xts(backtest$buy)
  backtest$order_sell <- xts::lag.xts(backtest$sell)
  if (sell_at_end) {
    backtest$order_sell[nrow(backtest)] <- 1
  }
  
  backtest$position <- backtest$order_buy - backtest$order_sell
  backtest <- backtest[backtest$position != 0]
  
  backtest$position_cross <- crossover(backtest$position) # to avoid buying/selling twice in a row
  backtest <- backtest[backtest$position_cross != 0] # remove trades without orders
  # Remove sell order at the start
  remove_start_sell <- all(backtest$position_cross[1] < 0)
  if (remove_start_sell) {
    backtest <- backtest[-1, ]
  }
  
  if (nrow(backtest) == 0) {
    z <- data.frame(buy=c(0, 0), sell=c(0,0), order_price=c(0,0), order_buy=c(0,0), order_sell=c(0,0), position=c(0,0), position_cross=c(1,0))
    backtest <- xts(z, index(x)[1:2])
  }
  
  backtest$position_cum <- cumsum(backtest$position_cross)
  # remove sell orders when there is no previous buy order (TODO: Update for short-selling)
  backtest <- backtest[backtest$position_cum >= 0]
  
  backtest$order_qty <- backtest$order_buy * qty + backtest$order_sell * -qty
  backtest$order_cost <- dplyr::if_else(backtest$order_qty < 0, cost, 0) # only add cost when selling
  backtest$order_value <- backtest$order_qty * backtest$order_price
  
  trades <- backtest[
    backtest$position_cross != 0,
    c("position_cross", "position_cum", "order_value", "order_qty", "order_cost")
  ]
  
  trades$delta <- -(trades$order_value + lag.xts(trades$order_value))
  trades$delta[trades$order_qty >= 0] <- 0
  trades$pips <- trades$delta - trades$order_cost
  trades$equity <- cumsum(trades$pips)
  
  if (debug) {
    list(data = x, backtest = backtest, trades = trades)
  } else {
    list(trades = trades)
  }
}

#' Calculate backtest stats
#'
#' @param result results of the backtest
#'
#' @return
#' @export
backtest_stats <- function(result) {
  result$drawdown <- cummax(result$equity) - result$equity
  
  net_profit <- as.numeric(xts::last(result)$equity)
  avg_profit_per_trade <- mean(result$pips[result$pips != 0])
  avg_winning_trade <- mean(result$pips[result$pips > 0])
  avg_losing_trade <- mean(result$pips[result$pips < 0])
  
  num_txns <- sum(result$position_cross != 0)
  num_trades <- sum(result$position_cross == -1)
  gross_profits <- sum(result$pips[result$pips > 0])
  gross_losses <- sum(result$pips[result$pips < 0])
  profit_factor <- abs(gross_profits / gross_losses)
  wins <- sum(result$pips > 0)
  losses <- sum(result$pips < 0)
  max_drawdown <- -max(result$drawdown)
  
  data.frame(
    net_profit,
    avg_profit_per_trade,
    avg_winning_trade,
    avg_losing_trade,
    num_txns,
    num_trades,
    gross_profits,
    gross_losses,
    profit_factor,
    wins,
    losses,
    max_drawdown
  )
}

#' Helper function to run the backtest
#'
#' @param symbol name of the dataset or symbol
#' @param data OHLC data
#' @param strat strategy list object
#' @param cost cost for each trade
#' @param qty qty for the order
#' @param ... other arguments for `backtest`
#'
#' @return
#' @export
run_backtest <- function(symbol, data, strat, cost=0, qty=1, ...) {
  message(paste0("Backtest ", symbol, "-", strat$name,"\n"))
  result <- backtest(data=data, strat=strat, cost=cost, qty = qty, ...)
  result$stats <- backtest_stats(result$trades)
  
  result$stats$symbol <- symbol
  result$stats$strategy <- strat$name
  result$stats$cost <- cost
  result$stats$order_size <- qty
  
  result
}

