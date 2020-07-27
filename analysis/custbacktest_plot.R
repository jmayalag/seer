plot_backtest <- function(result) {
  df <- result$data %>% xts_to_df() %>% as_tibble()
  df_tidy <- df %>% select(-c(Open, High, Low, Volume)) %>%
    select(c(index, Close, fast, medium, slow, enterLong, exitLong)) %>%
    mutate(across(c(enterLong, exitLong), ~ if_else(.x == 1, fast, as.numeric(NA)))) %>%
    pivot_longer(-c(index)) %>%
    mutate(indicator = name %in% c("fast", "medium", "slow")) %>%
    mutate(signal = name %in% c("enterLong", "exitLong"))
  
  df_tidy <- df_tidy %>% mutate(group = case_when(
    signal | indicator ~ "2.Signals",
    TRUE ~ "1.Series"
  ))
  
  series <- df_tidy %>% filter(name == "Close")
  indicators <- df_tidy %>% filter(indicator)
  signals <- df_tidy %>% filter(signal)
  trades <- result$trades %>% xts_to_df() %>% as_tibble()
  
  orders <- trades %>%
    select(index, order_qty) %>%
    pivot_longer(-c(index)) %>%
    mutate(name = if_else(value >= 0, "Buy", "Sell")) %>%
    mutate(group = "3. Order")
  
  plot <- ggplot() +
    facet_wrap(~group, scales = "free_y", ncol = 1) +
    geom_line(data = series, aes(x = index, y = value, color = name, linetype = name)) +
    geom_line(data = indicators, aes(x = index, y = value, color = name, linetype = name)) +
    geom_point(data = signals, aes(x = index, y = value, color = name, linetype = name)) +
    geom_col(data = orders, aes(x = index, y = value, fill = name)) +
    scale_linetype_manual(values = c("solid", "dotted", "dashed", "longdash", "twodash", "dotdash")) +
    scale_x_datetime(labels = scales::date_format("%b-%y")) +
    ylab(NULL) +
    xlab(NULL) +
    labs(title = sprintf("%s - %s", "asd", "asd"), color = "Values", linetype = "Values", fill = "Order") +
  
  plot
}

plotly_backtest <- function(...) {
  plotly::ggplotly(plot_backtest(...))
}