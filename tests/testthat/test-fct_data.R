test_that("get_rtt_data_with_progress works", {

  # Define a mock response
  mock_data <- tibble(
    trust_parent_org_code = character(),
    commissioner_parent_org_code = character(),
    commissioner_org_code = character(),
    trust = character(),
    specialty = character(),
    period = as.Date(x = integer(0), origin = "1970-01-01"),
    months_waited = numeric(),
    type = character(),
    value = numeric()
  )

  # Mock the function to return the mock data
  mocked_get_rtt_data <- function() {
    return(mock_data)
  }

  # Use local_mocked_bindings to mock the function
  local_mocked_bindings(
    get_rtt_data = function(...) mocked_get_rtt_data()
  )
# debugonce(get_rtt_data_with_progress)
  result <- get_rtt_data(
    date_start = as.Date("2024-10-01"),
    trust_codes = "R07",
    specialty_codes = "C_100",
    date_end = as.Date("2024-10-01"),
    progress = list()
  )

  # Assertions
  expect_s3_class(
    result,
    "tbl_df"
  )
  expect_equal(ncol(result), 9)
  expect_equal(nrow(result), 0)

  expect_equal(
    names(result),
    c("trust_parent_org_code",
      "commissioner_parent_org_code",
      "commissioner_org_code",
      "trust",
      "specialty",
      "period",
      "months_waited",
      "type",
      "value"
    ),
    info = "data has correct names"
  )

  expect_equal(result, mock_data)

})
