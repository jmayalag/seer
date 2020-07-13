#' Returns a plot of the backtest
#'
#' @param strategy strategy object
#' @param dataset xts dataset
#' @param backtest_results backtest results from backtest.opt
#'
#' @return a ggplot
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select across `%>%` if_else case_when filter
#' @import ggplot2
plot_backtest <- function(strategy, dataset, backtest_results, range = "") {
  strat_name <- backtest_results$stats$strategy
  symbol <- backtest_results$stats$Symbol
  
  mkt <- apply_indicator_signals(strategy, dataset)[range]
  
  trades <- xts_to_df(backtest_results$txns[-1][range]) # ignore initial order
  
  rules <- trades %>%
    select(index, Txn.Value) %>%
    pivot_longer(-c(index)) %>%
    mutate(name = if_else(value >= 0, "Buy", "Sell")) %>%
    mutate(group = "3. Order")
  
  df <- xts_to_df(mkt) %>% as_tibble()
  
  if (startsWith(strat_name, "triple")) {
    df_tidy <- df %>%
      select(index, Close, EMA.nFast, EMA.nMedium, EMA.nSlow, enterLong, exitLong) %>%
      mutate(across(c(enterLong, exitLong), ~ if_else(.x == 1, EMA.nFast, as.numeric(NA)))) %>%
      pivot_longer(-c(index)) %>%
      mutate(indicator = name %in% c("EMA.nFast", "EMA.nMedium", "EMA.nSlow")) %>%
      mutate(signal = name %in% c("enterLong", "exitLong"))
  } else if (startsWith(strat_name, "macd")) {
    df_tidy <- df %>%
      select(index, Close, macd._, signal._, enterLong, exitLong) %>%
      mutate(across(c(enterLong, exitLong), ~ if_else(.x == 1, macd._, as.numeric(NA)))) %>%
      pivot_longer(-c(index)) %>%
      mutate(indicator = name %in% c("macd._", "signal._")) %>%
      mutate(signal = name %in% c("enterLong", "exitLong"))
  }
  
  df_tidy <- df_tidy %>% mutate(group = case_when(
    signal | indicator ~ "2.Signals",
    TRUE ~ "1.Series"
  ))
  
  series <- df_tidy %>% filter(!signal & !indicator)
  indicators <- df_tidy %>% filter(indicator)
  signals <- df_tidy %>% filter(signal)
  
  plot <- ggplot() +
    facet_grid(group ~ ., scales = "free_y") +
    geom_col(data = rules, aes(x = index, y = value, fill = name), alpha = 0.5) +
    geom_line(data = series, aes(x = index, y = value, color = name, linetype = name)) +
    geom_line(data = indicators, aes(x = index, y = value, color = name, linetype = name)) +
    geom_point(data = signals, aes(x = index, y = value, color = name, linetype = name)) +
    scale_linetype_manual(values = c("solid", "dotted", "dashed", "longdash", "twodash", "dotdash")) +
    scale_x_datetime(labels = scales::date_format("%b-%y")) +
    ylab("Points") +
    xlab(NULL) +
    labs(title = sprintf("%s - %s", symbol, strat_name), color = "Values", linetype = "Values", fill = "Order")
  
  plot
}

#' Recreate a strategy
#'
#' @param backtest_results backtest results list
#' @param fees cost per trade
#' @param order_size order volume
#'
#' @return strategy object
#' @export
recreate_strategy <- function(backtest_results, fees = 0, order_size = 1) {
  strat_name <- backtest_results$stats$strategy
  params <- as.numeric(stringr::str_extract_all(strat_name, "\\d+", simplify = T))
  
  if (startsWith(strat_name, "triple")) {
    triple_crossover(nFast = params[1], nMedium = params[2], nSlow = params[3], fees = fees, order_size = order_size)
  } else if (startsWith(strat_name, "macd")) {
    macd(fastMA = params[1], slowMA = params[2], fees = fees, order_size = order_size)
  } else {
    stop(paste0("Invalid strategy ", strat_name))
  }
}

#' Creates an interactive plot
#'
#' @param ... arguments for plot_backtest
#'
#' @return plotly plot object
#' @export
plot_backtest_interactive <- function(...) {
  plot <- seer::plot_backtest(...)
  plotly::ggplotly(plot)
}