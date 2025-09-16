#' append_current_status
#'
#' @param percentile numeric; target percentile value
#' @param percentile_month numeric; month value that the percentile needs to be achieved
#' @importFrom dplyr distinct arrange mutate select left_join join_by filter
#'  summarise pull
#' @importFrom tidyr complete nesting unnest nest
#' @importFrom purrr map_dbl
#' @importFrom rlang .data
#' @importFrom NHSRtt hist_percentile_calc
#' @noRd
append_current_status <- function(
  data,
  max_months_waited,
  percentile,
  percentile_month
) {
  period_lkp <- data |>
    distinct(.data$period) |>
    arrange(.data$period) |>
    mutate(period_id = dplyr::row_number())

  data <- data |>
    select(!c("period_id")) |>
    tidyr::complete(
      # tidyr::nesting(.data$trust, .data$specialty),
      # tidyr::nesting(.data$type, .data$months_waited_id),
      tidyr::nesting(trust, specialty),
      tidyr::nesting(type, months_waited_id),
      period = period_lkp$period,
      fill = list(value = 0)
    ) |>
    left_join(
      period_lkp,
      by = join_by(period)
    )

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

  # calculate pressure metric
  pressure <- data |>
    filter(
      .data$type == "Incomplete",
      .data$period_id == max(.data$period_id)
    ) |>
    nest(data = c("months_waited_id", "value")) |>
    mutate(
      percentile_mnth = purrr::map_dbl(
        data,
        ~ NHSRtt::hist_percentile_calc(
          wl_structure = .x,
          percentile = percentile,
          wlsize_col = "value",
          time_col = "months_waited_id"
        )
      ),
      pressure = .data$percentile_mnth / percentile_month
    ) |>
    select("trust", "specialty", "pressure")

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
    ) |>
    left_join(
      pressure,
      by = join_by(
        trust,
        specialty
      )
    )

  return(current_status)
}

#' @importFrom NHSRtt optimise_steady_state
append_steady_state <- function(
  referrals,
  target,
  renege_params,
  percentile,
  target_time,
  method
) {
  # convert weeks input to months
  target_time <- convert_weeks_to_months(target_time)

  results <- NHSRtt::optimise_steady_state(
    referrals = referrals,
    target = target,
    renege_params = renege_params,
    percentile = percentile,
    target_time = target_time,
    method = method
  )

  output <- dplyr::tibble(
    capacity_ss = results$mu,
    reneges_ss = referrals - results$mu,
    incompletes_ss = results$wlsize,
    wl_ss = list(results$waiting_list)
  )

  return(output)
}


append_counterfactual <- function(
  capacity,
  referrals_start,
  referrals_end,
  incompletes_t0,
  renege_capacity_params,
  forecast_months,
  target_week
) {
  cap_proj <- rep(capacity, forecast_months)

  ref_proj <- seq(
    from = referrals_start,
    to = referrals_end,
    length.out = forecast_months
  )

  out <- NHSRtt::apply_params_to_projections(
    capacity_projections = cap_proj,
    referrals_projections = ref_proj,
    incomplete_pathways = incompletes_t0,
    renege_capacity_params = renege_capacity_params,
    max_months_waited = 12
  ) |>
    filter(.data$period_id == max(.data$period_id))

  perf <- calc_percentile_at_week(
    out,
    week = target_week,
    wlsize_col = "incompletes",
    time_col = "months_waited_id"
  )

  out <- out |>
    summarise(
      across(
        c("reneges", "incompletes"),
        sum,
        .names = "{.col}_counterf"
      )
    ) |>
    mutate(perf_counterf = perf)
  return(out)
}
