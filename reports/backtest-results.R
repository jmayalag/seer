#' ---
#' title: "Backtest Strategy Tables"
#' output:
#'   pdf_document:
#'     extra_dependencies: ["diagbox", "amsmath", "siunitx"]
#'     keep_tex: true
#' author: ""
#' date: ""
#' ---

#+ setup, include=FALSE, 
suppressMessages(require(tidyverse))
suppressMessages(require(knitr))
suppressMessages(require(kableExtra))
library(formattable)
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

root <- getwd()
if (basename(root) != "seer") {
  root <- file.path(root, "..")
}

source(file.path(root, "reports/constants.R"))


stats <- read_rds(file.path(root, "results/backtest_stats.rds"))

top_backtest <- function(stats, n = 1, column = "net_profit") {
  best <- stats %>%
    select(-c(order_size)) %>%
    group_by(dataset, strategy) %>%
    slice_max(.data[[column]], n = n, with_ties = T)
  
  best %>% slice_max(max_drawdown, n=n, with_ties = F)
}

stats_fm <- stats %>%
  rename(dataset = symbol) %>%
  mutate(dataset = str_match(dataset, "d1_(\\w+)_")[,2] %>% str_to_upper()) %>%
  mutate(dataset = to_ordered(dataset, symbols)) %>%
  mutate(strategy = str_split_fixed(name, "_", 2)[, 1]) %>%
  mutate(strategy = to_ordered(toupper(strategy), strategies)) %>%
  mutate(params = str_replace(name, "[a-zA-Z]+_", "") %>% str_replace_all("_", ", ") %>% paste0("(", ., ")")) %>%
  select(-c(name, results_file))

spec <- stats_fm %>%
  build_wider_spec(names_from = dataset, values_from = c(params, profit_factor)) %>%
  arrange(dataset) %>%
  mutate(.name = sprintf("%s %s", dataset, .value))

table <- top_backtest(stats_fm, n = 5, column = "profit_factor") %>%
  select(strategy, params, profit_factor, dataset) %>%
  arrange(strategy, dataset, desc(profit_factor)) %>%
  mutate(row = row_number()) %>%
  pivot_wider_spec(spec) %>%
  select(-row)

datasets <- stats_fm %>% distinct(dataset)
names(table) <- c("Strategy", rep(c("Parameters", "PF"), nrow(datasets)))

group_columns <- c(1, rep(2, nrow(datasets)))
names(group_columns) <- c(" ", levels(datasets$dataset))

#' # 1. Best parameters 
#' 
#+ best params

table %>%
  kbl(booktabs = T, digits = 3, caption="Best parameters") %>%
  kable_styling(latex_options = c("HOLD_position")) %>%
  collapse_rows(columns = 1:2, latex_hline = "major") %>%
  add_header_above(group_columns, bold = T) %>%
  add_header_above(c(" " = 1, "Index" = length(table) - 1), bold = T)


stats <- read_rds(file.path(root, "results/hybrid_stats.rds"))

stats_fm <- stats %>%
  mutate(dataset = str_match(dataset, "d1_(\\w+)_")[,2] %>% str_to_upper()) %>%
  mutate(dataset = to_ordered(dataset, symbols)) %>%
  mutate(base_strategy = str_split_fixed(name, pattern = "_", n = 2)[, 1]) %>%
  mutate(hybrid_strategy = str_split_fixed(name, pattern = "ml_", n = 2)[, 2]) %>%
  mutate(strategy = to_ordered(toupper(base_strategy), strategies)) %>%
  mutate(params = str_split_fixed(name, pattern = "-", n = 2)[, 1]) %>%
  mutate(params = str_replace(params, "[a-zA-Z]+_", "") %>% str_replace_all("_", ", ") %>% paste0("(", ., ")")) %>%
  select(dataset, strategy, hybrid_strategy, params, num_trades, profit_factor, net_profit, avg_profit_per_trade, max_drawdown, win_prob) %>%
  filter(hybrid_strategy == "" | str_starts(hybrid_strategy, "risky")) %>%
  mutate(hybrid_strategy = if_else(hybrid_strategy == "", "", "h")) %>%
  arrange(dataset, strategy)

table <- stats_fm %>%
  mutate(params = paste0(hybrid_strategy, params)) %>%
  select(-hybrid_strategy) %>%
  mutate(across(6:9, ~formattable::digits(.x, digits = 1))) %>%
  mutate(across(5, ~formattable::digits(.x, digits = 3)))

names(table) <- c("Index", "Strategy", "Parameters", "$\\#T$", "$PF$", "$NT$", "$\\overline{T}$", "$D_{max}$", "$PP$")


#' # 3. Hybrid strategies results

#+ results
table %>%
  kbl(booktabs = T, digits = 3, escape = F) %>%
  kable_styling(latex_options = c("HOLD_position")) %>%
  collapse_rows(columns = 1:2, latex_hline = "major")

#+ results2
table %>%
  kbl(booktabs = T, escape = F) %>%
  kable_styling(latex_options = c("HOLD_position")) %>%
  collapse_rows(columns = 1:2, latex_hline = "custom", custom_latex_hline = 1:2)