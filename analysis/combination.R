library(seer)
library(caret)
library(tidyverse)
library(xts)
library(quantmod)
library(nnet)

Sys.setenv(TZ = "UTC")

x <- read_ohlcv("~/datasets/datasets_paper/ES35_D1.csv")
w <- 10
h <- 1
tsp <- time_series_prediction_format(x, max_horizon = h, max_window = w)
# Lags, prediction, index
pred_column <- names(tsp)[w + h]
columns <- names(tsp)[c(1:w, w + h, w + h + 1)]
tsp <- tsp %>%
  select(!!columns) %>%
  rename(.outcome = !!pred_column)

# Split
split <- 0.70
split_idx <- floor(split * nrow(tsp))
in_training <- 1:nrow(tsp) <= split_idx

matrix <- subset(tsp, select = -c(index))
training <- matrix[in_training, ]

# training
fit_control <- trainControl(
  method = "none"
)

grid <- expand.grid(
  size = c(20),
  decay = c(0.001)
)

fit_nnet <- nnet(.outcome ~ .,
  linout = TRUE,
  skip = TRUE,
  MaxNWts = 10000,
  maxit = 1000,
  size = 10,
  decay = 0.001,
  data = matrix
)
tsp$nnet <- as.numeric(predict(fit_nnet, tsp))

fit_lm <- lm(.outcome ~ ., data = training)
tsp$lm <- predict(fit_lm, tsp)

tsp$set <- if_else(in_training, "train", "test")

tb <- tsp %>%
  as_tibble() %>%
  rename(observed = .outcome) %>%
  select(-(1:w))

tidy <- tb %>%
  mutate(actual = observed) %>%
  pivot_longer(c("nnet", "lm", "actual"), names_to = "method", values_to = "predicted")

tidy %>% 
  mutate(set = if_else(set == "train", "1. train", "2. test")) %>%
  ggplot(aes(x = index, y = predicted, color = method, linetype = method)) +
  geom_line() +
  facet_wrap(~set, scales = "free_x")

summary <- tidy %>%
  group_by(set, method) %>%
  summarise(
    RMSE = sqrt(sum(predicted - observed)^2 / n()),
    MAE = mean(abs(predicted - observed)),
    h = h,
    w = w
  )

summary

xts_pred <- tb %>% 
  mutate(prediction = nnet, in_training = if_else(set == "train", 1, 0)) %>% 
  select(index, prediction, in_training) %>%
  df_to_xts(order_by = index)


x <- cbind(x, xts_pred)

x
