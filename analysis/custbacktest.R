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

triple_ema <- function(x, nFast, nMedium, nSlow) {
  require(TTR)
  x$fast <- EMA(x$Close, n = nFast)
  x$medium <- EMA(x$Close, n = nMedium)
  x$slow <- EMA(x$Close, n = nSlow)

  x$enterLong <- crossover((x$fast > x$medium) & (x$medium > x$slow))
  x$exitLong <- crossover((x$fast <= x$medium) & (x$fast <= x$slow))

  x$buy <- replace_na(x$enterLong == 1, 0)
  x$sell <- replace_na(x$exitLong == 1, 0)

  name <- sprintf("tema_%d_%d_%d", nFast, nMedium, nSlow)

  strategy <- list(
    name = name,
    strategy = x,
    params = c(nFast, nMedium, nSlow)
  )
}

backtest <- function(strat, cost, qty, price_fun = quantmod::Op, sell_at_end = F) {
  require(xts)
  message(paste0("Backtest ", strat$name, "\n"))
  backtest <- strat$strategy[, c("buy", "sell")]
  backtest$order_price <- price_fun(strat$strategy) # sell and buy price

  # Order is executed the next day
  backtest$order_buy <- xts::lag.xts(backtest$buy)
  backtest$order_sell <- lag.xts(backtest$sell)
  if (sell_at_end) {
    backtest$order_sell[nrow(backtest)] <- 1
  }

  backtest <- backtest[backtest$order_buy | backtest$order_sell | index(backtest) == end(backtest)]

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

  trades$pips <- -(trades$order_value + lag.xts(trades$order_value))
  trades$pips[trades$order_qty >= 0] <- 0
  trades$pips_eff <- trades$pips - trades$order_cost
  trades$equity <- cumsum(trades$pips_eff)
  trades$drawdown <- cummax(trades$equity) - trades$equity

  trades
}

backtest_stats <- function(result) {
  net_profit <- as.numeric(last(result)$equity)
  avg_profit_per_trade <- mean(result$pips_eff[result$pips_eff != 0])
  avg_winning_trade <- mean(result$pips_eff[result$pips_eff > 0])
  avg_losing_trade <- mean(result$pips_eff[result$pips_eff < 0])

  num_txns <- sum(result$position_eff != 0)
  num_trades <- sum(result$position_eff == -1)
  gross_profits <- sum(result$pips_eff[result$pips_eff > 0])
  gross_losses <- sum(result$pips_eff[result$pips_eff < 0])
  profit_factor <- abs(gross_profits / gross_losses)
  wins <- sum(result$pips_eff > 0)
  losses <- sum(result$pips_eff < 0)
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

# strategies <- tema_grid %>%
#   mutate(strategy = pmap(list(nFast, nMedium, nSlow), ~ triple_ema(data, ..1, ..2, ..3)))
#
# results <- strategies %>%
#   mutate(result = map(strategy, ~ run_backtest(strat = .x, cost = cost, qty = qty, sell_at_end = T)))

# results <- strategies %>%
#   do(backtest = list(run_backtest(strat = .$strategy[[1]], cost = cost, qty = qty, sell_at_end = T)))

strat <- triple_ema(data, 1, 18, 72)

result <- run_backtest(strat, cost, qty, sell_at_end = T)
# run_backtest(strat, cost, qty, sell_at_end = F)

backtest_stats(result)
