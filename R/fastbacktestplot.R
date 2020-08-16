#' Plot backtest results
#'
#' @param result backtest results
#' @param range xts range (e.g. "2019", "2018/2019")
#'
#' @return
#' @export
#' @import plotly
plot_backtest <- function(result, range = "", additional_indicators) {
  df_all <- result$data[range] %>% xts_to_df() %>% as_tibble()
  trades <- result$trades[range] %>% xts_to_df() %>% as_tibble()
  start <- stats::start(result$data[range])
  end <- stats::end(result$data[range])
  indicators <- c("Close")
  
  if(startsWith(result$stats$strategy, "macd")) {
    indicators <- c(indicators, "macd", "signal")
    df <- df_all %>% select(c(index, Close, macd, signal, enterLong, exitLong)) %>%
      mutate(across(c(enterLong, exitLong), ~ if_else(.x == 1, macd, as.numeric(NA))))
  } else if (startsWith(result$stats$strategy, "tema")) {
    indicators <- c(indicators, "fast", "medium", "slow")
    df <- df_all %>% select(c(index, Close, fast, medium, slow, enterLong, exitLong)) %>%
      mutate(across(c(enterLong, exitLong), ~ if_else(.x == 1, fast, as.numeric(NA))))
  } else {
    stop("Invalid strategy. Should be macd or tema.")
  }
  
  if(str_detect(result$stats$strategy, "ml_")) {
    df <- df %>% add_column(df_all %>% select(h24, pred_bull_ind, pred_bear_ind)) %>%
      mutate(across(c(pred_bull_ind, pred_bear_ind), ~ if_else(.x == 1, h24, as.numeric(NA))))
    indicators <- c(indicators, "h24")
  }
  
  df_tidy <- df %>%
    pivot_longer(-c(index)) %>%
    mutate(indicator = name %in% indicators) %>%
    mutate(signal = name %in% c("enterLong", "exitLong", "pred_bull_ind", "pred_bear_ind"))
  
  df_tidy <- df_tidy %>% mutate(group = case_when(
    signal | indicator ~ "1.Signals",
    TRUE ~ "1. Series"
  ))
  
  series <- df_tidy %>% filter(name == "Close")
  indicators <- df_tidy %>% filter(indicator)
  signals <- df_tidy %>% filter(signal)
  
  orders <- trades %>%
    select(index, order_qty) %>%
    pivot_longer(-c(index)) %>%
    mutate(name = if_else(value >= 0, "Buy", "Sell")) %>%
    mutate(group = "2. Order")
  
  date_range <- sprintf("%s - %s", start, end)
  
  plot <- ggplot() +
    facet_wrap(~group, scales = "free_y", ncol = 1) +
    geom_line(data = indicators, aes(x = index, y = value, color = name, linetype = name)) +
    geom_line(data = indicators, aes(x = index, y = value, color = name, linetype = name)) +
    geom_point(data = signals, aes(x = index, y = value, color = name, linetype = name)) +
    geom_col(data = orders, aes(x = index, y = value, fill = name), position_dodge2(width = 0.9, preserve = "single")) +
    # scale_linetype_manual(values = c("solid", "dotted", "dashed", "longdash", "twodash", "dotdash")) +
    scale_x_datetime(labels = scales::date_format("%b-%y")) +
    ylab(NULL) +
    xlab(NULL) +
    labs(title = sprintf("%s - %s", result$stats$strategy, result$stats$symbol), subtitle = date_range,
         color = "Values", linetype = "Values", fill = "Order")
  
  plot
}

#' Plot with interaction using plotly
#'
#' @param ... arguments for `plot_backtest`
#'
#' @return
#' @export
#'
#' @examples
plotly_backtest <- function(...) {
  plotly::ggplotly(plot_backtest(...))
}