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
  progress
) {
  all_dates <- seq(
    from = lubridate::floor_date(
      date_start,
      unit = "months"
    ),
    to = lubridate::floor_date(
      date_end,
      unit = "months"
    ),
    by = "months"
  ) |>
    (\(x) {
      setNames(
        x,
        nm = seq_len(
          length(x)
        )
      )
    })()

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


#' Post-download data processing function
#'
#' @param data tibble as returned by the get_rtt_data_with_progress() function
#' @param specialty_aggregate can take the value "Aggregate", which will
#'   aggregate the specialties into one called "Aggregate". Otherwise, all of
#'   the specialties in the dataset will remain in the resulting table
#' @param trust_aggregatecan take the value "Aggregate", which will aggregate
#'   the trusts into one called "Aggregate". Otherwise, all of the trusts in the
#'   dataset will remain in the resulting table
#' @param selected_specialties character vector of specialties that are expected
#'   in the final table
#' @param min_date the minimum date for the resulting table
#' @param max_date the maximum date for the resulting table
#'
#' @importFrom dplyr summarise mutate arrange row_number
#' @importFrom NHSRtt convert_months_waited_to_id
#' @importFrom tidyr complete
#'
#' @returns table with fields for trust, specialty, type, period, period_id,
#'   months_waited_id and value
#' @noRd
aggregate_and_format_raw_data <- function(
  data,
  specialty_aggregate = NULL,
  trust_aggregate = NULL,
  selected_specialties = NULL,
  min_date,
  max_date
) {
  data <- data |>
    summarise(
      value = sum(.data$value),
      .by = c(
        "trust",
        "specialty",
        "period",
        "months_waited",
        "type"
      )
    ) |>
    mutate(
      months_waited_id = NHSRtt::convert_months_waited_to_id(
        .data$months_waited,
        12 # this pools the data at 12+ months (this can be a user input in the future)
      )
    )

  if (any(is.null(trust_aggregate), trust_aggregate != "Aggregated")) {
    data <- data |>
      mutate(
        trust = replace_fun(
          .data$trust,
          trust_lkp
        )
      )
  } else {
    data <- data |>
      mutate(
        trust = "Aggregated"
      )
  }

  if (any(is.null(specialty_aggregate), specialty_aggregate != "Aggregated")) {
    data <- data |>
      mutate(
        specialty = replace_fun(
          .data$specialty,
          treatment_function_codes
        )
      )
  } else {
    data <- data |>
      mutate(
        specialty = "Aggregated"
      )
  }

  data <- data |>
    summarise(
      value = sum(.data$value),
      .by = c(
        "trust",
        "specialty",
        "period",
        "type",
        "months_waited_id"
      )
    ) |>
    arrange(
      .data$trust,
      .data$specialty,
      .data$type,
      .data$months_waited_id,
      .data$period
    ) |>
    tidyr::complete(
      specialty = selected_specialties,
      type = c("Complete", "Incomplete"),
      .data$months_waited_id,
      period = seq(
        from = min_date,
        to = lubridate::floor_date(max_date, unit = "months"),
        by = "months"
      ),
      .data$trust,
      fill = list(value = 0)
    ) |>
    tidyr::complete(
      specialty = selected_specialties,
      type = "Referrals",
      months_waited_id = 0,
      period = seq(
        from = min_date,
        to = lubridate::floor_date(max_date, unit = "months"),
        by = "months"
      ),
      .data$trust,
      fill = list(value = 0)
    ) |>
    mutate(
      period_id = dplyr::row_number(), # we need period_id for later steps
      .by = c(
        "trust",
        "specialty",
        "type",
        "months_waited_id"
      )
    )

  return(data)
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
    msg <- paste(
      "Error: Missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
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
      ". Only 'Referral', 'Incomplete', and 'Complete' are allowed."
    )
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
    referral_months_waited,
    0
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

#' update the sample data to finish at the final month of the available online data
#' @param final_month the final month of the available online data
#' @return a tibble with the updated sample data
#' @noRd
update_sample_data <- function(final_month) {
  #sample_data is an internal data object
  sample_data_mnths <- unique(sample_data[["period"]]) |>
    sort()
  months_in_sample_data <- length(sample_data_mnths)
  calculated_first_month <- final_month %m-%
    months(months_in_sample_data - 1)

  mnth_lkp <- dplyr::tibble(
    period = sample_data_mnths,
    period_new = seq(
      from = calculated_first_month,
      to = final_month,
      by = "months"
    )
  )

  final_sample_data <- sample_data |>
    left_join(
      mnth_lkp,
      by = join_by(
        period
      )
    ) |>
    dplyr::select(
      period = "period_new",
      "months_waited_id",
      "type",
      "value"
    )

  return(final_sample_data)
}

#' function that splits the calibration data into two halves and returns a dataset that models the second half from the first half
#' @param data the processed calibration data
#' @param referrals_uplift logical; whether the referrals should be uplifted if they appear under-reported
#' @return a tibble with the processed data
#' @noRd
split_and_model_calibration_data <- function(data, referrals_uplift) {
  # order the data by period and months_waited_id
  data <- data |>
    arrange(
      .data$period,
      .data$months_waited_id
    )

  # determine the months in the first half of the data
  all_periods <- sort(unique(data$period))
  # note, all_periods includes t0 to enable the incompletes for the beginning of model calibration.
  # This isn't intuitive for the user based on their selection, so it should still be included in the
  # calibration process, but omitted from the display of results and treated as if that first
  # period isn't included
  if (length(all_periods) %% 2 == 0) {
    first_half_periods <- all_periods[1:((length(all_periods) / 2) + 1)]
  } else {
    first_half_periods <- all_periods[1:((length(all_periods) + 1) / 2)]
  }

  # calibrate model on first half of data
  # first, create the calibration dataset
  first_half_data <- data |>
    filter(
      .data$period %in% first_half_periods
    )

  if (isTRUE(referrals_uplift)) {
    # calculate the referrals uplift value by calibrating the parameters
    # with redistribute_m0_reneges set to FALSE
    referrals_uplift_value <- calibrate_parameters(
      first_half_data,
      max_months_waited = 12,
      redistribute_m0_reneges = FALSE,
      referrals_uplift = NULL
    ) |>
      tidyr::unnest("params") |>
      dplyr::filter(
        .data$months_waited_id == 0
      ) |>
      dplyr::pull(.data$renege_param)

    # check whether the referrals uplift value is negative
    if (referrals_uplift_value < 0) {
      referrals_uplift_value <- abs(
        referrals_uplift_value
      )
    } else {
      referrals_uplift_value <- 0
    }
  } else {
    referrals_uplift_value <- 0
  }

  referrals_uplift <- dplyr::tibble(
    trust = unique(first_half_data$trust),
    specialty = unique(first_half_data$specialty),
    referrals_uplift = referrals_uplift_value
  )

  # calculate the modelling parameters using the uplifted referrals
  params <- calibrate_parameters(
    first_half_data,
    max_months_waited = 12,
    redistribute_m0_reneges = FALSE,
    referrals_uplift = referrals_uplift
  )

  # apply parameters to projections
  # first, create the second half of the data
  second_half_data <- data |>
    filter(
      !.data$period %in% first_half_periods
    )

  # second, extract the referrals for the projected period
  adjusted_projection_referrals <- second_half_data |>
    filter(
      .data$type == "Referrals"
    ) |>
    pull(.data$value) |>
    (\(x) x + (x * referrals_uplift_value))()

  # third, extract the treatment capacity for the projected period
  projection_capacity <- second_half_data |>
    filter(
      .data$type == "Complete"
    ) |>
    summarise(
      value = sum(.data$value),
      .by = c("specialty", "trust", "type", "period", "period_id")
    ) |>
    pull(.data$value)

  # fourth, extract the incompletes for the start of the projected period
  t0_incompletes <- first_half_data |>
    filter(
      .data$type == "Incomplete",
      .data$period == max(.data$period)
    ) |>
    select(
      "months_waited_id",
      incompletes = "value"
    )

  # fifth, apply the parameters to the projection data
  projection_calcs <- NHSRtt::apply_params_to_projections(
    capacity_projections = projection_capacity,
    referrals_projections = adjusted_projection_referrals,
    incomplete_pathways = t0_incompletes,
    renege_capacity_params = params$params[[1]],
    max_months_waited = 12
  ) |>
    select(
      "period_id",
      "months_waited_id",
      modelled_incompletes = "incompletes"
    ) |>
    mutate(
      period_id = .data$period_id + max(first_half_data$period_id)
    )

  # filter original data for incompletes
  original_incompletes <- data |>
    filter(
      .data$type == "Incomplete"
    ) |>
    select(
      "period_id",
      "months_waited_id",
      original = "value"
    )

  # calculate the mean average percentage error
  modelled_incompletes <- projection_calcs |>
    left_join(
      original_incompletes,
      by = join_by(
        period_id,
        months_waited_id
      )
    )

  return(modelled_incompletes)
}


#' function to determine the mean absolute percentage error for the calibration data period
#' @param data the output of the split_and_model_calibration_data function
#' @importFrom dplyr summarise pull
#' @importFrom rlang .data
#' @return a string indicating the mean absolute percentage error (above 0%) or the mean
#'   absolute error (if any of the original values are 0 in the waiting list)
#' @noRd
error_calc <- function(data) {
  if (any(data$original == 0)) {
    error <- data |>
      summarise(
        mae = mean(
          abs(.data$original - .data$modelled_incompletes),
          na.rm = TRUE
        )
      ) |>
      pull(.data$mae) |>
      # round to two decimal places and turn into a percentage
      (\(x) as.character(round(x, 2)))()
  } else {
    error <- data |>
      summarise(
        mape = mean(
          abs(.data$original - .data$modelled_incompletes) / .data$original,
          na.rm = TRUE
        ) *
          100
      ) |>
      pull(.data$mape) |>
      # round to two decimal places and turn into a percentage
      (\(x) paste0(round(x, 2), "%"))()
  }
  return(error)
}
