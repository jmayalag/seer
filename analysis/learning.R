ml_nnet <- function(training, file) {
  require(readr)
  require(seer)
  require(caret)

  if (file.exists(file)) {
    return(read_rds(file))
  }
  
  fit_control <- trainControl(
    method = "none",
    number = 1,
  )

  grid <- expand.grid(
    size = c(5, 10, 30, 50, 70, 100),
    decay = c(0, 0.1, 0.01, 0.001, 0.0001)
  )

  fit <- train_nnet(training, fit_control, grid = grid)

  write_rds(x = fit, path = file)
  fit
}

ml_randomforest <- function(training, file) {
  require(readr)
  require(seer)
  require(caret)

  if (file.exists(file)) {
    return(read_rds(file))
  }

  # 10-fold CV, repeated 10 times
  fit_control <- trainControl(
    method = "none",
    number = 1,
  )

  fit <-train_rf(training, fit_control)

  write_rds(x = fit, path = file)
}

ml_evtree <- function(training, file) {
  require(readr)
  require(seer)
  require(caret)

  if (file.exists(file)) {
    return(read_rds(file))
  }

  # 10-fold CV, repeated 10 times
  fit_control <- trainControl(
    method = "none",
    number = 1,
    repeats = 1
  )

  grid <- expand.grid(
    alpha = c(0.1, 0.25, 0.5, 0.8, 1, 3, 5)
  )

  fit <- train_evtree(training, fit_control, grid = grid)

  write_rds(x = fit, path = file)
}
