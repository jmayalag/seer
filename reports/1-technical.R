#' ---
#' title: "Backtest Tables"
#' output:
#'   pdf_document:
#'     extra_dependencies: ["diagbox", "amsmath", "siunitx"]
#'     keep_tex: true
#' author: ""
#' date: ""
#' ---

#+ setup, include=FALSE, 
suppressMessages(library(tidyverse))
suppressMessages(require(knitr))
suppressMessages(require(kableExtra))
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

if (basename(getwd()) != "seer") {
  setwd("..")
}

source("reports/constants.R")

top_backtest <- function(stats, n = 1, column = "net_profit") {
  best <- stats %>%
    select(-c(order_size)) %>%
    group_by(dataset, strategy) %>%
    slice_max(.data[[column]], n = n, with_ties = T)
  
  best %>% slice_max(max_drawdown, n=n, with_ties = F)
}

stats <- read_rds("results/backtest_stats.rds")

stats_fm <- stats %>%
  mutate(dataset = str_match(dataset, "d1_(\\w+)_")[,2] %>% str_to_upper()) %>%
  mutate(dataset = to_ordered(dataset, symbols)) %>%
  mutate(strategy = to_ordered(strategy_name, strategies)) %>%
  mutate(params = str_replace(name, "[a-zA-Z]+_", "") %>% str_replace_all("_", ", ") %>% paste0("(", ., ")")) %>%
  select(-name)

spec <- stats_fm %>%
  build_wider_spec(names_from = dataset, values_from = c(params, profit_factor)) %>%
  arrange(dataset) %>%
  mutate(.name = sprintf("%s %s", dataset, .value))

table <- top_backtest(stats_fm, n = 6, column = "profit_factor") %>%
  select(strategy, params, profit_factor, dataset) %>%
  arrange(strategy, dataset, desc(profit_factor)) %>%
  mutate(row = row_number()) %>%
  pivot_wider_spec(spec) %>%
  select(-row)

datasets <- stats_fm %>% distinct(dataset)
names(table) <- c("Strategy", rep(c("Parameters", "PF"), nrow(datasets)))

group_columns <- c(1, rep(2, nrow(datasets)))
names(group_columns) <- c(" ", levels(datasets$dataset))

#+ best params
table %>%
  kbl(booktabs = T, digits = 3) %>%
  kable_styling() %>%
  collapse_rows(columns = 1:2, latex_hline = "major") %>%
  add_header_above(group_columns, bold = T) %>%
  add_header_above(c(" " = 1, "Index" = length(table) - 1), bold = T)

