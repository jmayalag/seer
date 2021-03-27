context("Test ensemble preprocessing")

create_test_xts <- function(n) {
  dates <- seq(from = as.Date("2012-01-01"), by = "days", length.out = n)
  xts(x = data.frame(Close = 1:n), order.by = dates)
}

test_that("El formato debe ser correcto para la prediccion", {
  max_window <- 5
  max_horizon <- 5
  df <- create_test_xts(20)

  x <- time_series_prediction_format(df,
    max_window = max_window,
    max_horizon = max_horizon
  )

  window_names <- names(x)[1:max_window]
  pred_names <- names(x)[(max_window + 1):(max_window + max_horizon)]
  expected_window_names <- paste0("w", (max_window - 1):0)
  expected_pred_names <- paste0("h", 1:max_horizon)

  expect_equal(window_names, expected_window_names)
  expect_equal(pred_names, expected_pred_names)

  values <- x[c(window_names, pred_names)]
  expected <- rbind(1:10, 11:20)
  expect_equivalent(as.matrix(values), expected)
})

test_that("El formato debe ser correcto para la prediccion 2", {
  max_window <- 5
  max_horizon <- 1
  df <- create_test_xts(20)

  x <- time_series_prediction_format(df,
                                     max_window = max_window,
                                     max_horizon = max_horizon
  )

  window_names <- names(x)[1:max_window]
  pred_names <- names(x)[(max_window + 1):(max_window + max_horizon)]
  expected_window_names <- paste0("w", (max_window - 1):0)
  expected_pred_names <- paste0("h", 1:max_horizon)

  expect_equal(window_names, expected_window_names)
  expect_equal(pred_names, expected_pred_names)

  values <- x[c(window_names, pred_names)]
  expected <- rbind(3:8, 9:14, 15:20)
  expect_equivalent(as.matrix(values), expected)
})
