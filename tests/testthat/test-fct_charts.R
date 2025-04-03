test_that("performance_text works", {
  df <- dplyr::tibble(
    Target_date = as.Date("2022-03-01") %m+% months(c(0, 12, 24)),
    Target_percentage = c(60, 65, 70)
  )


  expect_equal(
    performance_text(df),
    "60% (Mar 2022), 65% (Mar 2023) and 70% (Mar 2024)"
  )

  expect_equal(
    df |>
      head(2) |>
      performance_text(),
    "60% (Mar 2022) and 65% (Mar 2023)"
  )

  expect_equal(
    df |>
      head(1) |>
      performance_text(),
    "60% (Mar 2022)"
  )
})


library(testthat)
library(dplyr)
library(lubridate)

# Load the function to be tested (assuming it's in the same file or sourced)
# source("your_file_name.R")  # Uncomment and replace with your file name if needed

# Test suite for extend_period_type_data
test_that("Test extend_period_type_data", {

  # Helper function to create a sample dataset
  create_sample_data <- function() {
    data.frame(
      period = as.Date(c("2023-01-01", "2023-02-01","2023-03-01", "2023-04-01")),
      period_type = c("Type A", "Type A", "Type B", "Type B"),
      value = c(10, 20, 15, 25)
    )
  }

  # Test case 1: Basic test - check if the last period is extended correctly for each type

  sample_data <- create_sample_data()
  result <- extend_period_type_data(sample_data)

  # Check that the number of rows is correct (2 new rows added)
  expect_equal(
    nrow(result),
    nrow(sample_data) + 2,
    info = "Number of new records are correct"
  )

  # Check that the new periods are correct
  expected_new_periods <- as.Date(c("2023-03-01", "2023-05-01"))
  actual_new_periods <- result |>
    anti_join(
      sample_data,
      by = join_by(
        period, period_type
      )
    )

  expect_equal(
    actual_new_periods$period,
    expected_new_periods,
    info = "Extends the last period by one month for each period_type"
  )

  # Check that the values from the last month are copied correctly
  expected_values_A <- 20
  expected_values_B <- 25

  actual_values_A <- actual_new_periods |>
    filter(period_type == "Type A") |>
    pull(value)
  actual_values_B <- actual_new_periods |>
    filter(period_type == "Type B") |>
    pull(value)
  expect_equal(
    actual_values_A,
    expected_values_A,
    info = "New values from the previous month are copied correctly"
  )
  expect_equal(
    actual_values_B,
    expected_values_B,
    info = "New values from the previous month are copied correctly"
  )



  # Test case 2: Test with only one period

  one_period_data <- data.frame(
    period = as.Date(c("2023-01-01", "2023-01-01")),
    period_type = c("Type A", "Type B"),
    value = c(10, 20)
  )
  result <- extend_period_type_data(one_period_data)
  expect_equal(
    nrow(result),
    nrow(one_period_data) + 2,
    info = "Function works with one period only"
  )
  expected_new_periods <- as.Date(c("2023-02-01", "2023-02-01"))

  actual_new_periods <- result |>
    filter(period %in% expected_new_periods) |>
    pull(period)
  expect_equal(
    actual_new_periods,
    expected_new_periods,
    info = "New periods are calculated correctly when one period is passed to the function"
  )


})
