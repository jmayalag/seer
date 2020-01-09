library(OHLCMerge)
library(quantmod)
library(seer)
library(caret)
library(dplyr)
library(tidyr)
library(glue)
library(randomForest)

source("./analysis/learning.R")

run_test <- function(x, h, w, dataset, save_dir = "results") {
  models_dir <- file.path(save_dir, "models")
  preds_dir <- file.path(save_dir, "predictions")
  stats_dir <- file.path(save_dir, "stats")
  file_prefix <- paste(c("h", "w", "d"), c(h, w, dataset), collapse = "_", sep = "")

  dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(preds_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(stats_dir, showWarnings = FALSE, recursive = TRUE)

  message(glue("Running {file_prefix}..."))

  # Preprocessing ----
  tsp <- time_series_prediction_format(x, max_horizon = h, max_window = w)
  # Lags, prediction, index
  pred_column <- names(tsp)[w + h]
  columns <- names(tsp)[c(1:w, w + h, w + h + 1)]
  tsp <- tsp %>%
    select(!!columns) %>%
    rename(.outcome = !!pred_column)


  # Splitting ----
  split <- 0.70
  split_idx <- floor(split * nrow(tsp))
  in_training <- 1:nrow(tsp) <= split_idx

  matrix <- subset(tsp, select = -c(index))

  training <- matrix[in_training, ]

  # Training ----
  fit_nnet <- ml_nnet(training, glue("{models_dir}/{file_prefix}_nnet.rds"))
  fit_rf <- ml_randomforest(training, glue("{models_dir}/{file_prefix}_rf.rds"))
  fit_evtree <- ml_evtree(training, glue("{models_dir}/{file_prefix}_evtree.rds"))
  fit_lm <- glm(.outcome ~ ., data = training)

  # Predict ----
  tsp$nnet <- as.numeric(predict(fit_nnet$finalModel, tsp))
  tsp$rf <- predict(fit_rf$finalModel, tsp)
  tsp$evtree <- predict(fit_evtree$finalModel, tsp)
  tsp$lm <- predict(fit_lm, tsp)

  # set column to identify train/test observations
  tsp$set <- if_else(in_training, "train", "test")

  # Rename back the .outcome variable
  tb <- tsp %>%
    as_tibble() %>%
    rename(observed = .outcome) %>%
    select(-(1:w))

  stats <- tb %>%
    pivot_longer(c("nnet", "lm", "rf", "evtree"), names_to = "model", values_to = "predicted") %>%
    group_by(set, model) %>%
    summarise(
      RMSE = sqrt(sum(predicted - observed)^2 / n()) ,
      MAE = mean(abs(predicted - observed)),
      h = h,
      w = w,
      dataset = dataset
    )

  write_rds(tb, glue("{preds_dir}/{file_prefix}_pred.rds"))
  write_rds(stats, glue("{stats_dir}/{file_prefix}_stat.rds"))
  message(glue("Saved {file_prefix}"))
  file_prefix
}

# Config ----
dataset_dir <- "~/datasets/datasets_paper"

datasets <- c("ES35_D1", "GERMAN30_D1", "US30_D1")

run_grid <- expand_grid(
  h = c(1, 2, 3, 4, 5),
  w = c(3, 5, 7, 10, 15, 20)
)

for (dataset in datasets) {
  message(dataset)
  x <- read_dataset(dataset, datadir = dataset_dir)

  run_grid %>%
    rowwise() %>%
    do(test = run_test(x=x, dataset=dataset, h=.$h, w=.$w))
}
