library(OHLCMerge)
library(quantmod)
library(seer)
library(tidyquant)
library(caret)
library(dplyr)
library(tidyr)

x <- read_ohlcv("~/Downloads/datasets_paper/ES35_D1.csv")
chartSeries(x)

tsp <- time_series_prediction_format(x, max_horizon = 1, max_window = 2)
set.seed(998)


split <- 0.70
split_idx <- floor(split * nrow(tsp))
in_training <- 1:nrow(tsp) <= split_idx

training <- tsp[in_training, ]
testing <- tsp[!in_training, ]

df <- training

dates <- df$index
matrix <- subset(df, select = -c(index))

# 10-fold CV, repeated 10 times
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10
)

nnetfit <- train(Close.pred.1 ~ .,
  data = matrix,
  linout = TRUE,
  method = "nnet",
  trControl = fitControl,
  verbose = FALSE
)

fit <- nnet::nnet(Close.pred.1 ~ ., data = matrix, size = 10, linout = TRUE, skip = TRUE, MaxNWts = 10000, trace = FALSE, maxit = 1000)

df$pred <- as.numeric(predict(fit, matrix))

df_train <- df

df <- testing
matrix <- subset(df, select = -c(index))
df$pred <- as.numeric(predict(fit, matrix))

df_test <- df

df_train$dataset <- "1.train"
df_test$dataset <- "2.test"
df <- rbind(df_train, df_test) %>% as_tibble()

tb <- df %>% pivot_longer(c(Close.pred.1, pred), names_to = "Series", values_to = "Close")

tb %>%
  ggplot(aes(x = index, y = Close)) +
  geom_line(aes(color = Series)) +
  facet_wrap(~dataset, scales = "free_x")
