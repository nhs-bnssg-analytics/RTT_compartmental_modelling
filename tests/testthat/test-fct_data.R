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
