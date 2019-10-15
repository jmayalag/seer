#' Entrena nnet
#' El data.frame solo debe tener las columnas de input, y el output con el nombre '.outcome'.
#'
#' @param training data.frame de entrenamiento
#' @param fitControl parametros para el entrenamiento
#' @param grid espacio de busqueda de parametros
#'
#' @return resultados del entrenamiento
#' @export
#'
#' @examples
#'
#' \dontrun{
#' x <- time_series_prediction_format(AAPL, max_horizon=1, max_window=2)
#' train_nnet(x, grid=c(size=10, decay=0.1))
#' }
train_nnet <- function(training, fitControl, grid) {
  train_nnet <- train(.outcome ~ .,
                      data = training,
                      method = "nnet",
                      trControl = fitControl,
                      verbose = FALSE,
                      tuneGrid = grid_nnet,
                      # nnet params
                      linout = TRUE,
                      skip = TRUE,
                      MaxNWts = 10000,
                      maxit = 1000,
  )

  train_nnet
}
