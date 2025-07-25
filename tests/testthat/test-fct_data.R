test_that("get_rtt_data_with_progress works", {

  # Mock the function to return the mock data
  mocked_get_rtt_data <- function(
    date_start = as.Date("2024-01-01"),
    date_end = as.Date("2024-12-01"),
    trust_parent_codes = NA,
    commissioner_parent_codes = NA,
    commissioner_org_codes = NA,
    trust_codes = NA,
    specialty_codes = NA,
    show_progress = FALSE,
    progress) {

    period_lkp <- dplyr::tibble(
      period = seq(
        from = lubridate::floor_date(
          date_start %m-% months(1), unit = "months"
        ),
        to = lubridate::floor_date(
          date_end, unit = "months"
        ),
        by = "months"
      )
    ) |>
      mutate(
        period_id = dplyr::row_number() - 1
      )

    df <- tidyr::expand_grid(
      typ = c("referral", "incomplete", "complete"),
      tpc = trust_parent_codes,
      cpc = commissioner_parent_codes,
      coc = commissioner_org_codes,
      tc = trust_codes,
      sc = specialty_codes
    )

    max_months <- 12

    out <- purrr::pmap(
        .l = df,
        .f = \(typ, tpc, cpc, coc, tc, sc) {
          NHSRtt::create_dummy_data(
              type = typ,
              max_months_waited = max_months,
              number_periods = max(period_lkp$period_id),
              seed  = 444
            ) |>
            mutate(
              trust_parent_org_code = tpc,
              commissioner_parent_org_code = cpc,
              commissioner_org_code = coc,
              trust = tc,
              specialty = sc
            )
        }
      ) |>
      purrr::list_rbind() |>
      dplyr::mutate(
        months_waited = case_when(
          !is.na(referrals) ~ "<1",
          months_waited_id == max_months ~ paste0(max_months, "+"),
          .default = paste0(months_waited_id, "-", months_waited_id + 1)
        ),
        value = case_when(
          !is.na(referrals) ~ referrals,
          !is.na(incompletes) ~ incompletes,
          !is.na(treatments) ~ treatments,
          .default = NA_real_
        ),
        type = case_when(
          !is.na(referrals) ~ "Referrals",
          !is.na(incompletes) ~ "Incomplete",
          !is.na(treatments) ~ "Complete",
          .default = NA_character_
        )
      ) |>
      left_join(
        period_lkp,
        by = join_by(period_id)
      ) |>
      dplyr::relocate(
        period, .before = dplyr::everything()
      ) |>
      dplyr::relocate(
        value, .after = dplyr::everything()
      ) |>
      select(
        !c(
          "referrals",
          "incompletes",
          "treatments",
          "period_id",
          "months_waited_id"
        )
      )

    return(out)
  }

  # Use local_mocked_bindings to mock the function
  local_mocked_bindings(
    get_rtt_data_with_progress = function(...) mocked_get_rtt_data(...)
  )

  specialty_codes <- c("C_100", "C_999")
  trust_codes <- c("RA7", "R0D")

  dates <- seq(
    from = as.Date("2024-10-01"),
    to = as.Date("2024-11-01"),
    by = "months"
  )

  result <- get_rtt_data_with_progress(
    date_start = min(dates),
    date_end = max(dates),
    trust_codes = trust_codes,
    specialty_codes = specialty_codes,
    progress = list()
  )

  # Assertions
  expect_s3_class(
    result,
    "tbl_df"
  )
  expect_equal(
    ncol(result),
    9,
    info = "correct number of columns from get_rtt_data"
  )

  expect_equal(
    nrow(result),
    length(specialty_codes) *
      length(trust_codes) *
      (3 * (1 + 13 + 13)), # eg, 3 periods, where each period has 1 referral record, and 13 complete/incomplete records (as it pools at 12+ months)
    info = "correct number of rows from get_rtt_data"
    )

  expect_equal(
    names(result),
    c(
      "period",
      "trust_parent_org_code",
      "commissioner_parent_org_code",
      "commissioner_org_code",
      "trust",
      "specialty",
      "months_waited",
      "type",
      "value"
    ),
    info = "data has correct names"
  )

  expect_equal(
    unique(result$specialty),
    specialty_codes,
    info = "all specialty codes are present"
  )

  expect_equal(
    unique(result$trust),
    trust_codes,
    info = "all trust codes are present"
  )
})

# Test suite for check_imported_data
test_that("Test check_imported_data", {

  # Test case 1: Valid data passes without errors
  valid_data <- sample_data # sample data is an internal dataset
  result <- check_imported_data(valid_data)
  expect_equal(
    result$msg,
    "Data successfully loaded!",
    info = "successful data import message"
  )
  expect_equal(
    result$imported_data_checked,
    valid_data,
    info = "unchanged data when successfully imported"
  )

  # Test case 2: Missing required columns
  invalid_data <- sample_data |>
    select(!c("period"))
  result <- check_imported_data(invalid_data)
  expect_match(
    result$msg,
    "Error: Missing required columns: period",
    info = "Missing columns returns an error message and NULL data message"
  )
  expect_null(
    result$imported_data_checked,
    info = "NULL data return when missing columns"
  )

  invalid_data <- sample_data |>
    select(!c("type", "value"))
  result <- check_imported_data(invalid_data)
  expect_match(
    result$msg,
    "Error: Missing required columns: type, value",
    info = "Multiple missing columns"
  )
  expect_null(
    result$imported_data_checked,
    info = "NULL return when multiple missing columns"
  )

  # Test case 3: Invalid values in 'type' column
  invalid_data <- sample_data
  invalid_data$type[1] <- "InvalidType"
  result <- check_imported_data(invalid_data)

  expect_match(
    result$msg,
    "Error: Invalid values in 'type' column: InvalidType",
    info = "Invalid 'type' values return an error message"
  )
  expect_null(
    result$imported_data_checked,
    info = "Invalid 'type' values return NULL data"
  )

  invalid_data <- sample_data |>
    mutate(
      type = case_when(
        type == "Referrals" ~ "InvalidType",
        type == "Complete" ~ "AnotherInvalid",
        .default = type
      )
    )

  result <- check_imported_data(invalid_data)
  expect_match(
    result$msg,
    "Error: Invalid values in 'type' column: InvalidType, AnotherInvalid",
    info = "Multiple invalid values in type column provides error message"
  )
  expect_null(
    result$imported_data_checked,
    info = "Multiple invalid values in type column provide NULL data return"
  )

  # Test case 4: Referral data with months_waited_id != 0
  update_record <- dplyr::tibble(
    period = as.Date("2024-02-01"),
    type = "Referrals",
    months_waited_id = 1
  )

  invalid_data <- sample_data |>
    dplyr::rows_update(
      update_record,
      by = c("period", "type")
    )

  result <- check_imported_data(invalid_data)
  expect_match(
    result$msg,
    "Referral records must have only months_waited_id equal to 0.",
    info = "Referral data with invalid months_waited_id returns an error"
  )
  expect_null(
    result$imported_data_checked,
    info = "Referral data with invalid months_waited_id returns NULL data"
  )

  # Test case 5: Incomplete data with missing periods compared to complete
  remove_record <- dplyr::tibble(
    period = as.Date("2024-02-01"),
    type = "Incomplete",
    months_waited_id = 1
  )
  invalid_data <- sample_data |>
    dplyr::rows_delete(
      remove_record,
      by = c("period", "type", "months_waited_id")
    )
  result <- check_imported_data(invalid_data)
  expect_match(
    result$msg,
    "Incomplete data must have same combinations of periods and months_waited_ids as complete data.",
    info = "Incomplete data with missing periods returns an error"
  )
  expect_null(
    result$imported_data_checked,
    info = "Incomplete data with missing periods returns NULL data"
  )


  # Test case 6: Empty Dataframe
  empty_data <- data.frame()
  result <- check_imported_data(empty_data)
  expect_match(
    result$msg,
    "Error: Missing required columns: period, type, value, months_waited_id",
    info = "Empty dataframe returns error for missing columns"
  )
  expect_null(
    result$imported_data_checked,
    info = "Empty dataframe returns NULL dataset"
  )

  # Test case 7: NA in period field
  invalid_data <- sample_data
  invalid_data$period[10] <- NA
  result <- check_imported_data(invalid_data)

  expect_match(
    result$msg,
    "Data not loaded. An ambiguous date format was used in the provided file. Accepted date formats are 'dd/mm/yyyy' and 'yyyy-mm-dd'.",
    info = "NA in period field of imported data"
  )
  expect_null(
    result$imported_data_checked,
    info = "NA in period field of imported data return NULL data"
  )
})


test_that("convert_to_date works", {


  dts1 <- c("2022-04-01", "2021-04-05", "2012-12-12", "2000-01-01")

  expect_false(
    any(is.na(convert_to_date(dts1))),
    info = "all dates with format yyyy-mm-dd are converted successfully"
  )


  dts2 <- c("12/12/2022", "24/05/2021", "25/12/2022", "01/01/2019")
  expect_false(
    any(is.na(convert_to_date(dts2))),
    info = "all dates with format dd/mm/yyyy are converted successfully"
  )

  dts3 <- c("12/12/2022", "24/05/2021", "25/12/2022", "01-01-2019")
  expect_true(
    any(is.na(convert_to_date(dts3))),
    info = "string vetor with multiple date formats produce NAs"
  )

})
