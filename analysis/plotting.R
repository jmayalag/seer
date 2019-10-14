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
  number = 10,
  repeats = 10
)

train_nnet <- train(.outcome ~ .,
  data = training,
  method = "nnet",
  trControl = fitControl,
  verbose = FALSE,
  # nnet params
  linout = TRUE,
  skip = TRUE,
  MaxNWts = 10000,
  maxit = 1000
)

fit_nnet <- train_nnet$finalModel

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

RMSE(training$predicted, training$.outcome)
RMSE(testing$predicted, testing$.outcome)
