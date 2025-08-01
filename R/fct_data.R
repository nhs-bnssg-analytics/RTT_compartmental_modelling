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


#' convert string to date format, but check on format of string before
#' conversion. If format is unrecognised then the function returns "ambiguous
#' date format".
#'
#' @details accepts date formats "dd/mm/yyyy" and "yyyy-mm-dd"
#' @noRd
convert_to_date <- function(char_vector) {
  # Attempt conversion from "dd/mm/yyyy" format
  dates_format1 <- as.Date(char_vector, format = "%d/%m/%Y")

  # Attempt conversion from "yyyy-mm-dd" format
  dates_format2 <- as.Date(char_vector, format = "%Y-%m-%d")

  # Check if NAs are produced by first conversion
  if (all(!is.na(dates_format1))) {
    return(dates_format1)
  } else if (all(!is.na(dates_format2))) {
    return(dates_format2)
  } else {
    NAs_format1 <- sum(is.na(dates_format1))
    NAs_format2 <- sum(is.na(dates_format2))

    if (NAs_format1 > NAs_format2) {
      final_vector <- dates_format2
    } else if (NAs_format2 > NAs_format1) {
      final_vector <- dates_format1
    } else {
      final_vector <- rep("Ambiguous date format", length(char_vector))
    }
  }

  return(final_vector)
}

#' check the data imported into the app
#' @param imported_data a tibble with columns of period, type, value and
#'   months_waited_id
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

  # check the dates in the period column
  if (any(is.na(imported_data$period))) {
    msg <- "Data not loaded. An ambiguous date format was used in the provided file. Accepted date formats are 'dd/mm/yyyy' and 'yyyy-mm-dd'."
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
    filter(.data$type == "Referrals") |>
    dplyr::pull(.data$months_waited_id) |>
    unique()

  check_referral_months <- setdiff(
    referral_months_waited, 0
  )

  if (length(check_referral_months) > 0) {
    msg <- "Referral records must have only months_waited_id equal to 0."
    data_checked <- NULL

    return(
      list(
        msg = msg,
        imported_data_checked = data_checked
      )
    )
  }

  # check incompletes have same number of periods than completes
  incompletes_periods <- imported_data |>
    filter(.data$type == "Incomplete") |>
    dplyr::distinct(.data$period, .data$months_waited_id) |>
    dplyr::arrange(.data$period, .data$months_waited_id)

  completes_periods <- imported_data |>
    filter(.data$type == "Complete") |>
    dplyr::distinct(.data$period, .data$months_waited_id) |>
    dplyr::arrange(.data$period, .data$months_waited_id)

  if (!identical(incompletes_periods, completes_periods)) {
    msg <- "Incomplete data must have same combinations of periods and months_waited_ids as complete data."
    data_checked <- NULL

    return(
      list(
        msg = msg,
        imported_data_checked = data_checked
      )
    )
  }

  # If we got here, the data is valid
  data_checked <- imported_data
  check_outputs <- list(
    msg = "Data successfully loaded!",
    imported_data_checked = data_checked
  )

  return(check_outputs)
}
