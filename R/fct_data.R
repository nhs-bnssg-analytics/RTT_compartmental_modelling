#' data with progress bar
#'
#' @description Adds a progress bar to the NHSRtt get_rtt_data function
#'
#' @return A tibble of all the data required for modelling
#' @importFrom NHSRtt get_rtt_data
#' @importFrom purrr imap list_rbind
#' @importFrom stats setNames
#' @importFrom lubridate floor_date
#' @noRd
get_rtt_data_with_progress <- function(
    date_start,
    date_end,
    trust_parent_codes = NULL,
    commissioner_parent_codes = NULL,
    commissioner_org_codes = NULL,
    trust_codes = NULL,
    specialty_codes = NULL,
    progress) {


  all_dates <- seq(
    from = lubridate::floor_date(
      date_start, unit = "months"
    ),
    to = lubridate::floor_date(
      date_end, unit = "months"
    ),
    by = "months"
  ) |>
    (\(x) setNames(
      x,
      nm = seq_len(
        length(x)
      )
    )
  )()

  monthly_rtt <- all_dates |>
    purrr::imap(
      \(x, idx) {
        progress$set(value = as.numeric(idx))
        NHSRtt::get_rtt_data(
          date_start = x,
          date_end = x,
          trust_parent_codes = trust_parent_codes,
          trust_codes = trust_codes,
          commissioner_parent_codes = commissioner_parent_codes,
          commissioner_org_codes = commissioner_org_codes,
          specialty_codes = specialty_codes
        )
      }
    ) |>
    purrr::list_rbind()

  return(monthly_rtt)
}


#' check the data imported into the app
#' @return list with two items; a message describing the outputs of the check,
#'   and the resulting data tibble (which will be NULL if the checks have
#'   failed)
check_imported_data <- function(imported_data) {
  # Check if required columns exist
  required_cols <- c("period", "type", "value", "months_waited_id")
  missing_cols <- setdiff(required_cols, names(imported_data))

  if (length(missing_cols) > 0) {
    msg <- paste("Error: Missing required columns:", paste(missing_cols, collapse = ", "))
    data_checked <- NULL

    return(
      list(
        msg = msg,
        imported_data_checked = data_checked
      )
    )
  }

  # Check if 'type' column has valid values
  valid_types <- c("Referrals", "Incomplete", "Complete")
  invalid_types <- setdiff(unique(imported_data$type), valid_types)

  if (length(invalid_types) > 0) {
    msg <- paste(
      "Error: Invalid values in 'type' column:",
      paste(invalid_types, collapse = ", "),
      ". Only 'Referral', 'Incomplete', and 'Complete' are allowed.")
    data_checked <- NULL

    return(
      list(
        msg = msg,
        imported_data_checked = data_checked
      )
    )
  }

  # check all Referrals data have months_waited_id == 0
  referral_months_waited <- imported_data |>
    filter(.data$type == "Referral") |>
    dplyr::pull(.data$months_waited_id) |>
    unique()

  check_referral_months <- setdiff(
    referral_months_waited, 0
  )

  if (length(check_referral_months) > 0) {
    msg <- "Referral records must have only months_waited_id equal to 0."
    data_checked <- NULL
  }

  # check incompletes have same number of periods than completes
  incompletes_periods <- imported_data |>
    filter(.data$type == "Incomplete") |>
    dplyr::pull(.data$period) |>
    unique()

  completes_periods <- imported_data |>
    filter(.data$type == "Complete") |>
    dplyr::pull(.data$period) |>
    unique()

  missing_periods <- setdiff(
    incompletes_periods,
    completes_periods
  )

  if (length(missing_periods) != 0) {
    msg <- "Incomplete data must have same periods as complete data."
    data_checked <- NULL
  }

  # If we got here, the data is valid
  data_checked <- imported_data
  check_outputs <- list(
    msg = "Data successfully loaded!",
    imported_data_checked = data_checked
  )

  return(check_outputs)
}
