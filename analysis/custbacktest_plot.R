plot_backtest <- function(result, range = "") {
  require(ggplot2)
  df <- result$data[range] %>% xts_to_df() %>% as_tibble()
  trades <- result$trades[range] %>% xts_to_df() %>% as_tibble()
  start <- stats::start(result$data[range])
  end <- stats::end(result$data[range])
  indicators <- c("")
  
  if(startsWith(result$stats$strategy, "macd")) {
    indicators <- c("macd", "signal")
    df <- df %>% select(c(index, Close, fast, slow, macd, signal, enterLong, exitLong)) %>%
      mutate(across(c(enterLong, exitLong), ~ if_else(.x == 1, macd, as.numeric(NA))))
  } else if (startsWith(result$stats$strategy, "tema")) {
    indicators <- c("fast", "medium", "slow")
    df <- df %>% select(c(index, Close, fast, medium, slow, enterLong, exitLong)) %>%
      mutate(across(c(enterLong, exitLong), ~ if_else(.x == 1, fast, as.numeric(NA))))
  } else {
    stop("Invalid strategy. Should be macd or tema.")
  }
  
  df_tidy <- df %>%
    pivot_longer(-c(index)) %>%
    mutate(indicator = name %in% indicators) %>%
    mutate(signal = name %in% c("enterLong", "exitLong"))
  
  df_tidy <- df_tidy %>% mutate(group = case_when(
    signal | indicator ~ "2.Signals",
    TRUE ~ "1.Series"
  ))
  
  series <- df_tidy %>% filter(name == "Close")
  indicators <- df_tidy %>% filter(indicator)
  signals <- df_tidy %>% filter(signal)
  
  orders <- trades %>%
    select(index, order_qty) %>%
    pivot_longer(-c(index)) %>%
    mutate(name = if_else(value >= 0, "Buy", "Sell")) %>%
    mutate(group = "3. Order")
  
  date_range <- sprintf("%s - %s", start, end)
  
  plot <- ggplot() +
    facet_wrap(~group, scales = "free_y", ncol = 1) +
    geom_line(data = series, aes(x = index, y = value, color = name, linetype = name)) +
    geom_line(data = indicators, aes(x = index, y = value, color = name, linetype = name)) +
    geom_point(data = signals, aes(x = index, y = value, color = name, linetype = name)) +
    geom_col(data = orders, aes(x = index, y = value, fill = name)) +
    # scale_linetype_manual(values = c("solid", "dotted", "dashed", "longdash", "twodash", "dotdash")) +
    scale_x_datetime(labels = scales::date_format("%b-%y")) +
    ylab(NULL) +
    xlab(NULL) +
    labs(title = sprintf("%s - %s", result$stats$strategy, result$stats$symbol), subtitle = date_range,
         color = "Values", linetype = "Values", fill = "Order")
  
  plot
}

plotly_backtest <- function(...) {
  plotly::ggplotly(plot_backtest(...))
}