#' @importFrom OHLCMerge read_ohlcv
#' @export
OHLCMerge::read_ohlcv

#' @importFrom OHLCMerge read_dataset
#' @export
OHLCMerge::read_dataset

#' @importFrom OHLCMerge load_dataset
#' @export
OHLCMerge::load_dataset

#' Convierte un data.frame a un xts
#'
#' @param x datos a utilizar. Todas las columnas deben ser numericas
#' @param order_by columna a utilizar para el indice temporal.
#'
#' @return un xts
#' @export
df_to_xts <- function(x, order_by = index) {
  order_by_enquo <- dplyr::enquo(order_by)
  core_data <- dplyr::select(x, -!!order_by_enquo)
  order_by_data <- dplyr::pull(x, !!order_by_enquo)

  xts(core_data, order.by = order_by_data)
}

#' Convierte un xts a un data.frame
#'
#' @param x el objeto xts
#'
#' @return un data.frame
#' @export
xts_to_df <- function(x) {
  data.frame(index = zoo::index(x), zoo::coredata(x))
}
