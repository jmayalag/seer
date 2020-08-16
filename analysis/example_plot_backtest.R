library(seer)
library(tidyverse)

source("analysis/custbacktest_plot.R")

ml_strategies <- function(base_strategy, model, h, w, model_name = NULL) {
  approaches <- tibble(approach = c("risky", "semirisky", "conservative"))
  approaches %>%
    mutate(strategy = map(approach, ~ technical_ml(base_strategy, model, h=h, w=w, approach = .x, model_name))) %>%
    select(-approach) %>%
    mutate(name = map_chr(strategy, 'name'))
}

model_path <- "~/datasets/jcr2020/exp3/h24_w12_dDAX_D1_lm.rds"

h <- 24
w <- 12

data_dir <- "~/datasets/jcr2020/datasets"

datasets <- tribble(
  ~dataset, ~cost, ~strategy,
  "d1_dax_2019", 2, list(macd(5, 12), triple_ema(14, 30, 74)),
  "d1_dj30_2019", 2.4, list(macd(6, 12), triple_ema(2, 18, 72)),
  "d1_ibex35_2019", 5, list(macd(7, 12), triple_ema(2, 18, 70))
)

st_tema <- triple_ema(14,30,74)
ml_tema <- technical_ml(st_tema, model, h, w)
result <- run_backtest("DAX", read_dataset("d1_dax_2019", data_dir), st_tema, cost=2, debug=T)
result.2 <- run_backtest("DAX", read_dataset("d1_dax_2019", data_dir), macd(5,12), cost=2, debug=T)
result.3 <- run_backtest("DAX", read_dataset("d1_dax_2019", data_dir), ml_tema, cost=2, debug=T)
plot_backtest(result)
plot_backtest(result.3)
# plot_backtest(result.2)


df <- result$data %>% xts_to_df() %>% as_tibble()
# series <- c("Close")
# indicators <- c("fast", "medium", "slow")
# signals <- c("enterLong", "exitLong")
# df %>% select({{indicators}})
