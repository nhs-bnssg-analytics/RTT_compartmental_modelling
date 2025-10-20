#' Forecast n period based on a selected method
#'
#' @param rtt_table tibble containing a record for each period, and a value
#'   column
#' @param number_timesteps integer; number of time steps to forecast
#' @param method string; "Uniform" or "Linear"
#' @param percent_change numeric vector; if method is "Linear" the percent_change
#'   is the annual percentage change required relative to the extrapolated first
#'   time step (where 1 is a 1% annual uplift by time step 13)
#' @importFrom dplyr tibble mutate case_when summarise
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer unnest pivot_wider
#' @importFrom stats lm predict setNames
#' @importFrom rlang .data
#' @noRd
forecast_function <- function(
  rtt_table,
  number_timesteps = 13,
  method,
  percent_change
) {
  method <- match.arg(
    method,
    c("Linear", "Uniform")
  )

  first_val <- calculate_t1_value(rtt_table)

  if (method == "Uniform") {
    fcast <- rep(
      first_val * (1 + (percent_change / 100)),
      number_timesteps
    )
  } else if (method == "Linear") {
    # first, calculate the value for the first time step as either a linear
    # extrapolation of the data provided (if significant) or a mean (if linear
    # model is not significant)

    final_val <- first_val * (1 + (percent_change / 100))

    fcast <- dplyr::tibble(
      final = final_val,
      first = first_val
    ) |>
      pivot_longer(
        cols = c(.data$first, .data$final),
        names_to = "period_id",
        values_to = "value"
      ) |>
      mutate(
        period_id = case_when(
          .data$period_id == "first" ~ 1,
          .data$period_id == "final" ~ 13, # this needs to align with NHSRtt
          .default = NA_real_
        )
      ) %>%
      summarise(
        lm_fit = list(
          lm(value ~ period_id, data = .)
        )
      ) |>
      mutate(
        project = purrr::map(
          .data$lm_fit,
          ~ predict(
            object = .x,
            newdata = data.frame(period_id = 1:number_timesteps)
          )
        )
      ) |>
      select(c("project")) |>
      mutate(
        project = purrr::map(
          .data$project,
          ~ local_enframe(.x, name = "period_id", value_name = "value")
        )
      ) |>
      unnest(.data$project) |>
      pull(.data$value)
  }

  return(fcast)
}

#' pass in the rtt table and calculate the t1 value by either projecting a
#' linear model through the data (if it is significant) or taking a mean
#' @importFrom dplyr select arrange summarise `%>%` filter pull mutate case_when
#' @param monthly_rtt tibble; required a "period_id" and "value" field arranged
#'   by period
#' @param p_val_threshold numeric length 1; the threshold below which the
#'   linear model is accepted as the method to use for the projection of the
#'   first value
#' @noRd
calculate_t1_value <- function(monthly_rtt, p_val_threshold = 0.01) {
  # check names
  if (length(setdiff(c("period_id", "value"), names(monthly_rtt))) > 0) {
    stop("monthly_rtt is missing either 'period_id' or 'value' field")
  }

  first_period_id <- max(monthly_rtt[["period_id"]]) # + 1

  first_val <- monthly_rtt |>
    select("period_id", "value") |>
    arrange(.data$period_id) %>%
    summarise(
      mean_val = mean(.data$value),
      lm_fit = list(
        lm(value ~ period_id, data = .)
      ),
      pval = extract_pval(
        lm_object = .data$lm_fit[[1]],
        input_term = "period_id"
      ),
      lm_val = list(
        predict(
          object = .data$lm_fit[[1]],
          newdata = data.frame(period_id = first_period_id)
        )
      )
    ) |>
    mutate(
      t_1_val = case_when(
        pval <= p_val_threshold ~ as.numeric(.data$lm_val),
        .default = .data$mean_val
      ),
      # treatment capacity can't be less than zero, so it is fixed to zero if so
      t_1_val = case_when(
        .data$t_1_val < 0 ~ 0,
        .default = .data$t_1_val
      )
    ) |>
    pull(.data$t_1_val)

  return(first_val)
}

#' @param original_wl_data tibble; same structure as the r$waiting_list object ("period", "period_id", "")
#' @param new_referrals_capacity tibble; fields for "period", "calculated_treatments", and "adjusted_referrals" for the projected period
#' @param original_params list; the parameters used to calculate the original
#'   waiting list
#' @noRd
calculate_customised_projections <- function(
  original_wl_data,
  new_referrals_capacity,
  original_params
) {
  projections_capacity <- new_referrals_capacity |>
    dplyr::pull(.data$calculated_treatments)

  # obtain the customised referrals for the projected period
  projections_referrals <- new_referrals_capacity |>
    dplyr::pull(.data$adjusted_referrals)

  # calibration dataset
  calibration_data <- original_wl_data |>
    dplyr::filter(
      .data$period_type == "Observed"
    )

  # obtain capacity_skew
  original_capacity_skew <- calibration_data |>
    dplyr::pull(.data$capacity_skew) |>
    unique()

  # create period_lkp
  temp_period_lkp <- original_wl_data |>
    dplyr::distinct(
      .data$period,
      .data$period_id
    )

  # obtain the waiting list at the end of the observed period
  t0_incompletes <- calibration_data |>
    dplyr::filter(
      .data$period == max(.data$period)
    ) |>
    dplyr::select(
      "months_waited_id",
      "incompletes"
    ) |>
    dplyr::mutate(
      months_waited_id = extract_first_number(
        .data$months_waited_id
      )
    )

  # recalculate projections
  new_wl_data <- NHSRtt::apply_params_to_projections(
    capacity_projections = projections_capacity,
    referrals_projections = projections_referrals,
    incomplete_pathways = t0_incompletes,
    renege_capacity_params = original_params,
    max_months_waited = 12
  ) |>
    # add referrals onto data
    dplyr::left_join(
      dplyr::tibble(
        unadjusted_referrals = original_wl_data |>
          dplyr::filter(
            .data$period_type == "Projected",
            !is.na(.data$unadjusted_referrals)
          ) |>
          dplyr::pull(.data$unadjusted_referrals),
        adjusted_referrals = projections_referrals,
        months_waited_id = 0
      ) |>
        dplyr::mutate(
          period_id = dplyr::row_number()
        ),
      by = join_by(
        period_id,
        months_waited_id
      )
    ) |>
    mutate(
      period_id = .data$period_id + max(calibration_data$period_id),
      capacity_skew = original_capacity_skew,
      period_type = "Projected",
      months_waited_id = convert_month_to_factor(.data$months_waited_id)
    ) |>
    left_join(
      temp_period_lkp,
      by = join_by(period_id)
    ) |>
    dplyr::bind_rows(
      calibration_data
    ) |>
    dplyr::arrange(
      .data$period_id,
      .data$months_waited_id
    )
  return(new_wl_data)
}
