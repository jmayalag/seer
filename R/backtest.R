#' Verifica que no haya inconsistencias entre el portfolio y el account
#'
#' @param port.st nombre del portfolio
#' @param account.st nombre de la cuenta
#' @param verbose muestra mensajes de inconsistencias
#'
#' @return FALSE si hay inconsistencias, TRUE caso contrario
#' @export
checkBlotterUpdate <- function(port.st = portofolio.name,
                               account.st = account.name,
                               verbose = TRUE) {
  ok <- TRUE
  p <- blotter::getPortfolio(port.st)
  a <- blotter::getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(
    sapply(
      syms,
      FUN = function(x) eval(
        parse(
          text = paste("sum(p$symbols",
                       x,
                       "posPL.USD$Net.Trading.PL)",
                       sep = "$"
          )
        )
      )
    )
  )
  
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  
  if (!isTRUE(all.equal(port.tot, port.sum.tot))) {
    ok <- FALSE
    if (verbose) print("portfolio P&L doesn't match sum of symbols P&L")
  }
  
  initEq <- as.numeric(xts::first(a$summary$End.Eq))
  endEq <- as.numeric(xts::last(a$summary$End.Eq))
  
  if (!isTRUE(all.equal(port.tot, endEq - initEq))) {
    ok <- FALSE
    if (verbose) print("portfolio P&L doesn't match account P&L")
  }
  
  if (sum(duplicated(zoo::index(p$summary)))) {
    ok <- FALSE
    if (verbose) print("duplicate timestamps in portfolio summary")
  }
  
  if (sum(duplicated(zoo::index(a$summary)))) {
    ok <- FALSE
    if (verbose) print("duplicate timestamps in account summary")
  }
  return(ok)
}


#' Realiza el backtest sin hacer graficos.
#'
#' @param strategy el strategy object
#' @param symbols vector de nombres de datasets a utilizar en el backtest. Ver detalles.
#' @param geometric utilizar media geometrica para calcular returns.
#' @param initial_equity fondos de la cuenta al empezar el backtest
#' @param silent no imprime las transacciones realizadas
#'
#' @return lista con resultados del backtest
#'
#' @export
#'
#' @details
#' Los datasets especificados en `symbols` deben estar cargados en el entorno global
#' como workaround a las limitaciones de blotter/quantstrat.
#'
#' @examples
#' data("ES35_D1")
#' backtest.opt(macd(), "ES35_D1")
backtest.opt <- function(strategy, symbols, geometric = FALSE, initial_equity = 10000, silent = FALSE) {
  FinancialInstrument::currency("USD")
  Sys.setenv(TZ = "UTC")
  rm(list=ls(envir = .blotter), envir = .blotter)
  rm(list=ls(envir = .strategy), envir = .strategy)
  portfolio.st <- "Portfolio"
  account.st <- "Account"
  quantstrat::rm.strat(portfolio.st)
  quantstrat::rm.strat(account.st)
  
  # Equidad inicial de la cuenta
  init_equity <- initial_equity
  # Ajustar precios (dividends, stock splits, etc)
  adjustment <- TRUE
  
  FinancialInstrument::stock(symbols, currency = "USD", multiplier = 1)
  
  blotter::initPortf(
    name = portfolio.st,
    symbols = symbols
  )
  
  blotter::initAcct(
    name = account.st,
    portfolios = portfolio.st,
    initEq = init_equity
  )
  
  quantstrat::initOrders(
    portfolio = portfolio.st,
    symbols = symbols
  )
  
  if (silent) {
    invisible(capture.output(quantstrat::applyStrategy(strategy = strategy, portfolios = portfolio.st)))
  } else {
    quantstrat::applyStrategy(strategy = strategy, portfolios = portfolio.st)
  }
  
  blotter::updatePortf(portfolio.st)
  blotter::updateAcct(account.st)
  blotter::updateEndEq(account.st)
  
  if (!checkBlotterUpdate(portfolio.st, account.st)) {
    print("Existen diferencias entre cuenta y portfolio.")
  }
  
  results <- list()
  
  results$txns <- blotter::getTxns(portfolio.st, Symbol = symbols[1])
  results$stats <- blotter::tradeStats(portfolio.st)
  results$stats$strategy <- strategy$name
  results$stats$investment <- init_equity
  
  p <- blotter::getPortfolio(portfolio.st)
  results$returns <- blotter::PortfReturns(account.st)
  results$drawdowns <- PerformanceAnalytics::Drawdowns(results$returns)
  if (geometric) {
    results$cumreturns <- cumprod(1 + results$returns) - 1
  } else {
    results$cumreturns <- cumsum(results$returns)
  }
  
  results
}


#' Recalcula estadisticas en base a las transacciones
#'
#' @param trades xts con las txns del backtest
#'
#' @return data.frame con las estadisticas
#' @export
trade_stats <- function(trades) {
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
      Med.Daily.PL = MedDailyPL,
      Std.Dev.Daily.PL = StdDailyPL,
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


#' Recalcula estadisticas con los nuevos parametros de compra/venta
#'
#' @param backtest_result resultados de una ejecucion previa de backtest
#' @param fees costo de transaccion
#' @param quantity cantidad de compra/venta
#'
#' @return
#' @export
#'
#' @examples
recalc_stats <- function(backtest_result, fees, quantity) {
  trades <- recalc_trades(backtest_result$txn, fees, quantity)
  new_stats <- trade_stats(trades)
  stats <- backtest_result$stats
  stats[, colnames(new_stats)] <- new_stats[, colnames(new_stats)] # update with new stats
  stats
}

#' Recalcula transacciones
#'
#' @param trades datos de las transacciones
#' @param fees costo de transaccion
#' @param qty volumen de transaccion
#'
#' @return transacciones recalculadas
#' @export
recalc_trades <- function(trades, fees = 0, qty = 1) {
  require(xts)
  require(blotter)
  is.enter <- trades$Txn.Qty > 0
  is.exit <- trades$Txn.Qty < 0
  prev.qty <- dplyr::coalesce(abs(as.numeric(trades$Txn.Qty)[2]), 100) # Baseline qty
  trades$Txn.Qty <- (trades$Txn.Qty / prev.qty) * qty
  
  trades[is.exit, ]$Txn.Fees <- -abs(fees)
  
  trades$Txn.Value <- trades$Txn.Price * trades$Txn.Qty
  trades$Txn.Avg.Cost <- ifelse(trades$Txn.Qty == 0, 0, trades$Txn.Value / trades$Txn.Qty)
  
  prev <- lag.xts(trades)
  
  trades$Net.Txn.Realized.PL <- -(trades$Txn.Value + prev$Txn.Value - trades$Txn.Fees)
  trades[!is.exit, ]$Net.Txn.Realized.PL <- 0
  
  trades
}