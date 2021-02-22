backtest_stats <- tibble::tribble(
  ~level, ~label,
  "Num Txns", "Number of transactions",
  "Num Trades", "Number of flat to flat trades performed",
  "Net Trading PL", "Net Profit and Loses",
  "Avg Trade PL", "Mean trading P&L per trade",
  "Med Trade PL", "Median trading P&L per trade",
  "Std Err Trade PL", "Standard error of the trading P&L per trade",
  "Largest Winner", "Largest winning trade",
  "Largest Loser", "Largest losing trade",
  "Gross Profits", "Gross (pre-fee) trade profits",
  "Gross Losses", "Gross trade losses",
  "Std Dev Trade PL", "Standard deviation of trade P&L",
  "Percent Positive", "Percent of trades that end positive",
  "Percent Negative", "Percent of trades that end negative",
  "Profit Factor", "Absolute value ratio of gross profits over gross losses",
  "Avg Win Trade", "Mean P&L of profitable trades",
  "Med Win Trade", "Median P&L of profitable trades",
  "Avg Losing Trade", "Mean P&L of losing trades",
  "Med Losing Trade", "Median P&L of losing trades",
  "Avg Daily PL", "Mean daily realized P&L on days there were transactions",
  "Med Daily PL", "Median daily P&L",
  "Std Dev Daily PL", "Standard deviation of daily P&L",
  "Std Err Daily PL", "Standard error of daily P&L",
  "Max Drawdown", "Maximum observed loss from a peak to a trough, before a new peak is attained",
  "Profit To Max Draw", "Ratio of Net Trading PL over the Max Drawdown",
  "Avg WinLoss Ratio", "Ratio of mean winning over mean losing trade",
  "Med WinLoss Ratio", "Ratio of median winning trade over median losing trade",
  "Max Equity", "Maximum account equity",
  "Min Equity", "Minimum account equity",
)


less_better <- c(
  "Ann Sharpe",
  "Avg Losing Trade",
  "Gross Losses",
  "Largest Loser",
  "Max Drawdown",
  "Med Losing Trade",
  "Num Trades",
  "Num Txns",
  "Percent Negative",
  "Profit To Max Draw",
  "Std Dev Daily PL",
  "Std Dev Trade PL",
  "Std Err Daily PL",
  "Std Err Trade PL"
)

symbols <- tibble::tribble(
  ~level, ~label,
  "d1_ibex35_2000_2018", "IBEX",
  "d1_dax_2000_2018", "DAX",
  "d1_dj30_2000_2018", "DJI",
  "IBEX35", "IBEX",
  "DAX", "DAX",
  "DJ30", "DJI"
)

strategies <- tibble::tribble(
  ~level, ~label,
  "tema", "TEMA",
  "macd", "MACD",
)

algorithms <- tibble::tribble(
  ~level, ~label,
  "LM", "LM",
  "NN", "NN",
  "SVR", "SVR",
  "RF", "RF",
  "EVTREE", "EVtree"
)

measures <- tibble::tribble(
  ~level, ~label,
  "MRE", "MRE",
  "R2", "R$^2$",
  "MAE", "MAE",
  "RMSE", "RMSE",
  "SMAPE", "sMAPE",
  "MAPE", "MAPE"
)

to_ordered <- function(values, levels) {
  factor(toupper(values), levels = toupper(levels$level), labels = levels$label, ordered = T)
}