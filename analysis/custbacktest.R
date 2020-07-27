library(seer)
library(tidyverse)

data <- read_ohlcv("~/datasets/jcr2020/datasets/d1_dax_2019.csv")

crossover <- function(x) {
  x <- as.numeric(x)
  prev <- lag(x)
  prev[is.na(prev)] <- 0

  cross <- x - prev
  cross[is.na(cross)] <- 0
  cross[cross != 0] <- x[cross != 0]

  cross
}

triple_ema <- function(nFast, nMedium, nSlow) {
  require(TTR)
  
  strategy_fun <- function(x) {
    x$fast <- EMA(x$Close, n = nFast)
    x$medium <- EMA(x$Close, n = nMedium)
    x$slow <- EMA(x$Close, n = nSlow)
    
    x$enterLong <- crossover((x$fast > x$medium) & (x$medium > x$slow))
    x$exitLong <- crossover((x$fast <= x$medium) & (x$fast <= x$slow))
    
    x$buy <- replace_na(x$enterLong == 1, 0)
    x$sell <- replace_na(x$exitLong == 1, 0)
    x
  }

  name <- sprintf("tema_%d_%d_%d", nFast, nMedium, nSlow)

  strategy <- list(
    name = name,
    params = c(nFast, nMedium, nSlow),
    strategy = strategy_fun
  )
}

backtest <- function(data, strat, cost, qty, sell_at_end = F, price_fun = quantmod::Op, debug = F) {
  require(xts)
  message(paste0("Backtest ", strat$name, "\n"))
  x <- strat$strategy(data)
  backtest <- x[, c("buy", "sell")]
  backtest$order_price <- price_fun(x) # sell and buy price

  # Order is executed the next day
  backtest$order_buy <- xts::lag.xts(backtest$buy)
  backtest$order_sell <- lag.xts(backtest$sell)
  if (sell_at_end) {
    backtest$order_sell[nrow(backtest)] <- 1
  }

  backtest <- backtest[backtest$order_buy | backtest$order_sell]

  backtest$position <- backtest$order_buy - backtest$order_sell

  backtest$position_eff <- crossover(backtest$position) # to avoid buying/selling twice in a row
  backtest$position_cum <- cumsum(backtest$position_eff)
  # remove sell orders when there is no previous buy order (TODO: Fix for short-selling)
  backtest <- backtest[backtest$position_cum >= 0]

  backtest$order_qty <- backtest$order_buy * qty + backtest$order_sell * -qty
  backtest$order_cost <- dplyr::if_else(backtest$order_qty < 0, cost, 0) # only add cost when selling
  backtest$order_value <- backtest$order_qty * backtest$order_price

  trades <- backtest[
    backtest$position_eff != 0,
    c("position_eff", "position_cum", "order_value", "order_qty", "order_cost")
  ]

  trades$delta <- -(trades$order_value + lag.xts(trades$order_value))
  trades$delta[trades$order_qty >= 0] <- 0
  trades$pips <- trades$delta - trades$order_cost
  trades$equity <- cumsum(trades$pips)
  trades$drawdown <- cummax(trades$equity) - trades$equity
  
  if (debug) {
    list(data = x, backtest = backtest, trades = trades)
  } else {
    list(trades = trades)
  }
}

run_backtest <- function(symbol, data, strat, cost=0, qty=1, ...) {
  result <- backtest(data=data, strat=strat, cost=cost, qty = qty, ...)
  result$stats <- backtest_stats(result$trades)
  
  result$stats$symbol <- symbol
  result$stats$strategy <- strat$name
  result$stats$cost <- cost
  result$stats$order_size <- qty
  
  result
}

backtest_stats <- function(result) {
  net_profit <- as.numeric(last(result)$equity)
  avg_profit_per_trade <- mean(result$pips[result$pips != 0])
  avg_winning_trade <- mean(result$pips[result$pips > 0])
  avg_losing_trade <- mean(result$pips[result$pips < 0])

  num_txns <- sum(result$position_eff != 0)
  num_trades <- sum(result$position_eff == -1)
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

tema_grid <- expand_grid(
  nFast = 1:10,
  nMedium = 5:15,
  nSlow = 10:25
) %>% filter(nFast < nMedium & nMedium < nSlow)

qty <- 1
cost <- 2
sell_at_end <- T

strategies <- tema_grid %>%
  mutate(strategy = pmap(list(nFast, nMedium, nSlow), ~ triple_ema(..1, ..2, ..3))) %>%
  mutate(name = map_chr(strategy, 'name'))

# results <- strategies %>%
#   mutate(result = map(strategy, ~ backtest(data = data, strat = .x, cost = cost, qty = qty, sell_at_end = T)))
# 
# stats <- results %>%
#   mutate(map_df(result, backtest_stats))
# 
# stats %>%
#   select(-c(strategy, result))
  

# View(stats)

# results <- strategies %>%
#   do(backtest = list(run_backtest(strat = .$strategy[[1]], cost = cost, qty = qty, sell_at_end = T)))

tema <- triple_ema(1, 18, 72)

result <- run_backtest("DAX", data, tema, cost=cost, qty=qty, sell_at_end = T, debug = T)

