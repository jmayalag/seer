#' Entrena nnet
#' El data.frame solo debe tener las columnas de input, y el output con el nombre '.outcome'.
#'
#' @param training data.frame de entrenamiento.
#' @param fitControl parametros para el entrenamiento.
#' @param grid espacio de busqueda de parametros.
#'
#' @return Resultados del entrenamiento.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- time_series_prediction_format(AAPL, max_horizon = 1, max_window = 2)
#' train_nnet(x, grid = c(size = 10, decay = 0.1))
#' }
train_nnet <- function(training, fitControl, grid) {
  caret::train(.outcome ~ .,
    data = training,
    method = "nnet",
    trControl = fitControl,
    verbose = FALSE,
    tuneGrid = grid,
    # nnet params
    linout = TRUE,
    skip = TRUE,
    MaxNWts = 10000,
    maxit = 1000,
  )
}

#' Train Random Forest
#'
#' @param training training dataset
#' @param fitControl parametros para el entrenamiento.
#' @param grid tuning grid
#'
#' @return the training information
#' @export
train_rf <- function(training, fitControl, grid = NULL) {
  caret::train(.outcome ~ .,
    data = training,
    method = "rf",
    trControl = fitControl,
    verbose = FALSE,
    tuneGrid = grid,
    # rf params
    ntree = 100,
    maxnodes = 100,
    proximity = TRUE
  )
}


#' Train evtree
#'
#' @param training training dataset
#' @param fitControl parametros para el entrenamiento.
#' @param grid tuning grid
#'
#' @return the training information
#' @export
train_evtree <- function(training, fitControl, grid = NULL) {
  caret::train(.outcome ~ .,
    data = training,
    method = "evtree",
    trControl = fitControl,
    verbose = FALSE,
    tuneGrid = grid,
    # evtree params
    ntrees = 300,
    niterations = 1000
  )
}
