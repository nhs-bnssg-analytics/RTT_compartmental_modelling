#' append_current_status
#'
#' @noRd
append_current_status <- function(data, max_months_waited) {
  # calculate referrals uplift
  referrals_uplift <- calibrate_parameters(
    data,
    max_months_waited = max_months_waited,
    redistribute_m0_reneges = FALSE,
    referrals_uplift = NULL,
    allow_negative_params = TRUE
  ) |>
    tidyr::unnest("params") |>
    dplyr::filter(
      .data$months_waited_id == 0
    ) |>
    dplyr::mutate(
      referrals_uplift = case_when(
        .data$renege_param < 0 ~ abs(.data$renege_param),
        .default = 0
      )
    ) |>
    select("trust", "specialty", "referrals_uplift")

  # calculate referrals for start of projection period
  projection_referrals <- data |>
    filter(
      type == "Referrals",
      .data$period != min(.data$period)
    ) |>
    select(!c("type", "months_waited_id")) |>
    # uplift referrals based on under-reporting of referrals in
    # published data
    left_join(
      referrals_uplift,
      by = join_by(
        trust,
        specialty
      )
    ) |>
    mutate(
      value = .data$value + (.data$value * .data$referrals_uplift)
    ) |>
    tidyr::nest(
      cal_period = c("period", "period_id", "value")
    ) |>
    mutate(
      referrals_t1 = purrr::map_dbl(
        .data$cal_period,
        calculate_t1_value
      )
    ) |>
    select(!c("cal_period", "referrals_uplift"))

  # calculate the capacity for the first projected timestep
  projection_capacity <- data |>
    filter(
      type == "Complete",
      .data$period != min(.data$period)
    ) |>
    summarise(
      value = sum(.data$value),
      .by = c(
        "trust",
        "specialty",
        "period_id",
        "period"
      )
    ) |>
    tidyr::nest(
      cal_period = c("period", "period_id", "value")
    ) |>
    mutate(
      capacity_t1 = purrr::map_dbl(
        .data$cal_period,
        calculate_t1_value
      )
    ) |>
    select(!c("cal_period"))

  reneges_at_t0 <- calibrate_parameters(
    data,
    max_months_waited = max_months_waited,
    redistribute_m0_reneges = FALSE,
    referrals_uplift = referrals_uplift,
    allow_negative_params = FALSE,
    full_breakdown = TRUE
  ) |>
    mutate(
      reneges_t0 = purrr::map_dbl(
        .data$params,
        \(x) {
          x |>
            filter(.data$period_id == max(.data$period_id)) |>
            summarise(
              val = sum(.data$reneges, na.rm = TRUE)
            ) |>
            pull(.data$val) |>
            (\(x) ifelse(x < 0, 0, x))()
        }
      )
    ) |>
    select("specialty", "trust", "reneges_t0")

  # INCOMPLETES at t = 0

  # Here we use the latest observed waiting list as the starting point
  # for the projections
  incompletes_at_t0 <- data |>
    filter(
      .data$type == "Incomplete",
      .data$period_id == max(.data$period_id)
    ) |>
    summarise(
      incompletes_t0 = sum(.data$value),
      .by = c(
        "trust",
        "specialty"
      )
    )

  # calibration parameters
  params <- calibrate_parameters(
    data,
    max_months_waited = max_months_waited,
    redistribute_m0_reneges = FALSE,
    referrals_uplift = referrals_uplift,
    allow_negative_params = FALSE,
    full_breakdown = FALSE
  )

  current_status <- projection_referrals |>
    left_join(
      projection_capacity,
      by = join_by(
        trust,
        specialty
      )
    ) |>
    left_join(
      reneges_at_t0,
      by = join_by(
        trust,
        specialty
      )
    ) |>
    mutate(
      load = round(
        .data$referrals_t1 / (.data$capacity_t1 + .data$reneges_t0),
        2
      )
    ) |>
    left_join(
      incompletes_at_t0,
      by = join_by(
        trust,
        specialty
      )
    ) |>
    left_join(
      params,
      by = join_by(
        trust,
        specialty
      )
    )

  return(current_status)
}
