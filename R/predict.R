#' Realiza prediccion utilizando un modelo de ML
#'
#' @param model modelo predictivo
#' @param x objeto xts
#' @param h
#' @param w
#' @param overlap TRUE para permitir predecir en cada observacion
#'
#' @return objeto xts con las columnas de prediccion
#' @export
predict_xts <- function(model, x, h, w, overlap = TRUE) {
  # Allow NA in the prediction column
  pred_format <- time_series_prediction_format(x, max_horizon = h, max_window = w, overlap = overlap, discard = FALSE)
  pred_format <- pred_format[(w + 1):nrow(pred_format), ]
  pred_col <- sprintf("h%d", h)
  
  values <- predict(model, pred_format[,  1:w])
  prediction <- as.numeric(values[, pred_col])
  pred <- xts(prediction, pred_format$index)
  colnames(pred) <- pred_col

  x_pred <- cbind(x, pred)
}

#' Realiza prediccion utilizando un modelo de ML con multiples salidas
#'
#' @param model modelo predictivo
#' @param x objeto xts
#' @param w
#' @param overlap TRUE para permitir predecir en cada observacion
#'
#' @return objeto xts con las columnas de prediccion
#' @export
predict_xts_multiple <- function(model, x, h, w, overlap = TRUE) {
  # Allow NA in the prediction column
  pred_format <- time_series_prediction_format(x, max_horizon = h, max_window = w, overlap = overlap, discard = FALSE)
  pred_format <- pred_format[(w + 1):nrow(pred_format), ]
  
  values <- predict(model, pred_format[,  1:w])
  pred <- xts(values, order.by = pred_format$index)
  
  x_pred <- cbind(x, pred)
}

#' Identifica parametros del experimento
#'
#' @param model_path path del archivo. Debe tener el formato "h[H]_w[W]_d[DATASET]_[METHOD].rds"
#'
#' @return
#' @export
#'
#' @examples
parse_params <- function(model_path) {
  model_name <- basename(model_path)
  params <- stringr::str_match(model_name, "h([:digit:]+)_w([:digit:]+)_d([[:alnum:]_]+)_(.*)")
  h <- as.numeric(params[[2]])
  w <- as.numeric(params[[3]])
  dataset <- params[[4]]
  method <- params[[5]]
  tibble::tibble(h, w, dataset, method)
}
