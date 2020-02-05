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
