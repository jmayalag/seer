#' Convierte datos OHLC a formato requerido para prediccion de series de tiempo.
#'
#' Se puede especificar un valor alto de max_window y max_prediction_horizon si se va a reutilizar el data.frame
#'
#' @param source un OHLC de tipo xts
#' @param pred_col columna a utilizar en la prediccion
#' @param max_horizon maximo horizonte de prediccion
#' @param max_window maximo ventana a usar para el historico (w = 0 solo utiliza el dia actual )
#' @param discard descarta n primeros valores para que los restantes sean multiplos de w + h
#'
#' @return un data.frame formateado para la prediccion
#'
#' @export
#'
#' @examples
#' time_series_prediction_format(AAPL)
time_series_prediction_format <- function(source,
                                          pred_col = "Close",
                                          max_horizon = 1,
                                          max_window = 2,
                                          discard = TRUE,
                                          overlap = FALSE) {
  if (!inherits(index(source), "POSIXct")) {
    index(source) <- as.POSIXct(index(source)) # POSIXlt to POSIXct
  }

  data <- data.table::as.data.table(source)[, c("index", pred_col), with = FALSE]

  lags <- 0:(max_window - 1)
  lags <- rev(lags)
  leads <- 1:max_horizon

  cols <- pred_col
  lag_cols <- paste(cols, "lag", lags, sep = ".")
  pred_cols <- paste(cols, "pred", leads, sep = ".")

  # Calcula n periodos anteriores
  data[, (lag_cols) := data.table::shift(.SD, lags), .SDcols = pred_col]
  # Horizonte de prediccion
  data[, (pred_cols) := data.table::shift(.SD, leads, type = "lead"), .SDcols = pred_col]
  data[, (pred_col) := NULL]

  # Reordena las columnas
  data.table::setcolorder(data, c(lag_cols, pred_cols, "index"))

  data <- as.data.frame(data)
  class(data) <- c("ts.prediction", class(data))

  if (discard) {
    data <- data[complete.cases(data), ]
  }

  if (!overlap) {
    keep <- seq(nrow(data), 1, by = -(max_window + max_horizon))
    data <- data[rev(keep), ]
  }

  data
}
