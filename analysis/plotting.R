library(OHLCMerge)
library(quantmod)
library(seer)
library(tidyquant)
library(caret)
library(dplyr)
library(tidyr)

set.seed(998)
x <- read_ohlcv("~/Downloads/datasets_paper/ES35_D1.csv")

# Preprocessing ----
tsp <- time_series_prediction_format(x, max_horizon = 1, max_window = 2)
tsp <- tsp %>% rename(.outcome = Close.pred.1)

# Splitting ----
split <- 0.70
split_idx <- floor(split * nrow(tsp))
in_training <- 1:nrow(tsp) <= split_idx

matrix <- subset(tsp, select = -c(index))

training <- matrix[in_training, ]
testing <- matrix[!in_training, ]

# Training ----

# 10-fold CV, repeated 10 times
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5
)

grid_nnet <- expand.grid(
  size = c(1, 5, 10, 30, 50, 70, 100),
  decay = c(0, 0.1, 0.01, 0.001, 0.0001)
)

fit_nnet <- train_nnet(training, fitControl, grid = grid_nnet)

tsp$predicted <- as.numeric(predict(fit_nnet, tsp))
tsp$dataset <- "1.training"
tsp[!in_training, ]$dataset <- "2.testing"
tsp <- tsp %>% rename(observed = .outcome)

tb <- tsp %>%
  pivot_longer(c(observed, predicted), names_to = "Series", values_to = "Close")

tb %>%
  ggplot(aes(x = index, y = Close)) +
  geom_line(aes(color = Series)) +
  facet_wrap(~dataset, scales = "free_x")

training <- tsp %>% filter(dataset == "1.training")
testing <- tsp %>% filter(dataset == "2.testing")

RMSE(training$predicted, training$observed)
RMSE(testing$predicted, testing$observed)
