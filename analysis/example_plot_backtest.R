library(seer)
library(tidyverse)
library(ggthemes)

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

plot_trades <- function(data, trades, range = "") {
  df <- cbind(data, trades)[range] %>%
    xts_to_df() %>%
    as_tibble()
  
  series <- df %>%
    mutate(order_type = case_when(
      order_qty > 0 ~ "Buy",
      order_qty < 0 ~ "Sell"
    ))
  
  trades <- series %>%
    select(index, order_type, Close) %>%
    drop_na(order_type)
  
  pl <- ggplot() +
    geom_line(data = series, aes(x = index, y = Close), color = "black") +
    geom_point(data = trades, aes(x = index, y = Close, fill = order_type, color = order_type, shape = order_type, group = order_type)) +
    scale_color_manual(values=c('red','green'), name = "Order Type") +
    scale_fill_manual(values=c('red','green'), name = "Order Type") +
    scale_shape_manual(values=c(24, 25), name = "Order Type") + 
    ylab("Closing Price") + xlab(NULL) +
    theme_Publication()
  
  pl
}

data_dir <- "~/datasets/jcr2020/datasets"

st_tema <- triple_ema(14, 30, 74)
result <- run_backtest("DAX", read_dataset("d1_dax_2019", data_dir), st_tema, cost = 2, debug = T, sell_at_end = T)

plot <- plot_trades(result$data, result$trades) +
  ggtitle(result$stats$strategy, subtitle = result$stats$symbol)
print(plot)