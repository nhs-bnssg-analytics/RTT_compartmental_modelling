test_that("calculate_t1_value errors", {
  df <- dplyr::tibble(
    period_id = 1:5,
    val = runif(5)
  )

  expect_error(
    calculate_t1_value(df),
    "monthly_rtt is missing either 'period_id' or 'value' field"
  )

})

test_that("calculate_t1_value works", {
  df <- dplyr::tibble(
    period_id = 1:5,
    value = 1:5
  )

  expect_equal(
    suppressWarnings(calculate_t1_value(df)),
    5,
    info = "for perfect linear relationship, the latest value is used as the result"
  )

  set.seed(312)
  df <- dplyr::tibble(
    period_id = 1:5,
    value = runif(5)
  )

  expect_equal(
    calculate_t1_value(df),
    mean(df$value),
    info = "for random relationship, the result is the mean of the historical values"
  )

})


test_that("forecast_function errors", {
  df <- dplyr::tibble(
    period_id = 1:5,
    val = runif(5)
  )

  expect_snapshot(
    forecast_function(
      rtt_table = df,
      number_timesteps = 15,
      method = "linear",
      percent_change = 100
    ),
    error = TRUE
  )

})

test_that("forecast_function works", {
  df <- dplyr::tibble(
    period_id = 1:5,
    value = 1:5
  )

  outputs_linear <- suppressWarnings(
    forecast_function(
      rtt_table = df,
      number_timesteps = 13,
      method = "Linear",
      percent_change = 100
    )
  )

  expect_equal(
    outputs_linear[13],
    outputs_linear[1] * 2,
    info = "100% increase in value over one year"
  )

  expect_equal(
    length(unique(round(diff(outputs_linear), 8))),
    1,
    info = "linear increase in values"
  )

  outputs_uniform <- suppressWarnings(
    forecast_function(
      rtt_table = df,
      number_timesteps = 20,
      method = "Uniform",
      percent_change = 100
    )
  )

  expect_equal(
    length(unique(outputs_uniform)),
    1,
    info = "uniform change provides a single forecasted value"
  )
})
