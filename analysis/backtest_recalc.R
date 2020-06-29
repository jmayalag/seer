library(dplyr)
library(tidyr)
library(seer)
library(glue)
library(readr)
library(purrr)

path <- "~/datasets/jcr2020/exp2/macd"
files <- list.files(path, pattern = "*.rds", full.names = T, recursive = T)

read_backtest <- function(file) {
  message(sprintf("Loading %s...", basename(file)))
  read_rds(file)
}

recalc_cost <- function(result, cost = 0, buy = 0, sell = 0) {
  txns <- result$txns
  stats <- result$stats
}

#' Recalcula transacciones
#'
#' @param trades datos de las transacciones
#' @param fees cost de transaccion
#'
#' @return transacciones recalculadas
#' @export
recalc_trades <- function(trades, fees = 0) {
  require(xts)
  require(blotter)
  is.enter <- trades$Txn.Qty > 0
  is.exit <- trades$Txn.Qty < 0

  trades[is.exit, ]$Txn.Fees <- -abs(fees)

  trades$Txn.Value <- trades$Txn.Price * trades$Txn.Qty
  trades$Txn.Avg.Cost <- ifelse(trades$Txn.Qty == 0, 0, trades$Txn.Value / trades$Txn.Qty)

  prev <- lag.xts(trades)

  trades$Net.Txn.Realized.PL <- -(trades$Txn.Value + prev$Txn.Value + trades$Txn.Fees)
  trades[!is.exit, ]$Net.Txn.Realized.PL <- 0

  trades
}

files <- dir("~/datasets/jcr2020/exp2/macd/", pattern = "*.rds", full.names = T) %>% head(10)

z <- read_backtest(files[1])

recalc_stats <- function(trades) {
  sd.err <- function(x) {
    sd(x) / sqrt(length(x))
  }

  # Daily stats
  DailyPL <- xts::apply.daily(trades$Net.Txn.Realized.PL[trades$Net.Txn.Realized.PL != 0], sum)
  AvgDailyPL <- mean(DailyPL)
  MedDailyPL <- median(DailyPL)
  StdDailyPL <- sd(as.numeric(as.vector(DailyPL)))
  StdErrDailyPL <- sd.err(as.numeric(as.vector(DailyPL)))

  df <- xts_to_df(trades) %>%
    as_tibble() %>%
    mutate(is.enter = Txn.Qty > 0, is.exit = Txn.Qty < 0) %>%
    mutate(is.win = Net.Txn.Realized.PL > 0, is.loss = Net.Txn.Realized.PL < 0) %>%
    mutate(
      equity = cumsum(Net.Txn.Realized.PL),
      drawdown = cummax(equity) - equity,
    )

  stats <- df %>%
    summarise(
      Num.Txns = sum(is.exit) + sum(is.enter),
      Num.Trades = sum(is.exit),
      Net.Trading.PL = last(equity),
      Avg.Trade.PL = mean(Net.Txn.Realized.PL[is.win | is.loss]),
      Med.Trade.PL = median(Net.Txn.Realized.PL[is.win | is.loss]),
      Largest.Winner = max(Net.Txn.Realized.PL[is.win]),
      Largest.Loser = min(Net.Txn.Realized.PL[is.loss]),
      Gross.Profits = sum(Net.Txn.Realized.PL[is.win]),
      Gross.Losses = sum(Net.Txn.Realized.PL[is.loss]),
      Std.Dev.Trade.PL = sd(Net.Txn.Realized.PL[is.win | is.loss]),
      Std.Err.Trade.PL = sd.err(Net.Txn.Realized.PL[is.win | is.loss]),
      Wins = sum(is.win),
      Losses = sum(is.loss),
      Percent.Positive = Wins / Num.Trades * 100,
      Percent.Negative = Losses / Num.Trades * 100,
      Profit.Factor = abs(Gross.Profits / Gross.Losses),
      Avg.Win.Trade = mean(Net.Txn.Realized.PL[Net.Txn.Realized.PL > 0]),
      Med.Win.Trade = median(Net.Txn.Realized.PL[Net.Txn.Realized.PL > 0]),
      Avg.Losing.Trade = mean(Net.Txn.Realized.PL[Net.Txn.Realized.PL < 0]),
      Med.Losing.Trade = median(Net.Txn.Realized.PL[Net.Txn.Realized.PL < 0]),
      Avg.Daily.PL = AvgDailyPL,
      Std.Daily.PL = StdDailyPL,
      Std.Err.Daily.PL = StdErrDailyPL,
      Ann.Sharpe = AvgDailyPL / StdDailyPL * sqrt(252),
      Max.Drawdown = -max(drawdown),
      Profit.To.Max.Draw = -Net.Trading.PL / Max.Drawdown,
      Avg.WinLoss.Ratio = abs(Avg.Win.Trade / Avg.Losing.Trade),
      Med.WinLoss.Ratio = abs(Med.Win.Trade / Med.Losing.Trade),
      Max.Equity = max(cumsum(Net.Txn.Realized.PL)),
      Min.Equity = min(cumsum(Net.Txn.Realized.PL)),
      End.Equity = last(equity),
    ) %>%
    select(-c(Wins, Losses))

  stats %>% 
    mutate(across(where(is.numeric), ~ na_if(na_if(na_if(.x, NaN), Inf), -Inf))) %>% # replace any inf values with NA
    as.data.frame()
}

z$stats %>%
  pivot_longer(-c(Portfolio, Symbol, strategy)) %>%
  select(name, value) %>%
  mutate(value = formattable::digits(value, 2)) %>%
  as.data.frame()

recalc_stats(z$txns)