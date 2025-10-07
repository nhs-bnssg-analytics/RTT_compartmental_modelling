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
  data <- data |>
    tidyr::complete(
      tidyr::nesting(trust, specialty),
      tidyr::nesting(type, months_waited_id),
      period = period,
      fill = list(value = 0)
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

calculate_s_given <- function(
  data,
  max_months_waited,
  method = c("mean", "median", "latest")
) {
  # check method
  available_methods <- c("mean", "median", "latest")
  if (identical(method, available_methods)) {
    method <- "mean"
  }

  method <- match.arg(method, available_methods)

  s_given <- data |>
    filter(.data$type == "Complete") |>
    mutate(
      s = .data$value / sum(.data$value),
      .by = c("trust", "specialty", "period")
    )

  if (method == "mean") {
    s_given <- s_given |>
      summarise(
        s_vals = mean(.data$s, na.rm = TRUE),
        .by = c("trust", "specialty", "months_waited_id")
      )
  } else if (method == "median") {
    s_given <- s_given |>
      summarise(
        s_vals = stats::median(.data$s, na.rm = TRUE),
        .by = c("trust", "specialty", "months_waited_id")
      )
  } else if (method == "latest") {
    s_given <- s_given |>
      filter(.data$period == max(.data$period)) |>
      dplyr::select("trust", "specialty", "months_waited_id", s_vals = "s")
  }

  s_given <- s_given |>
    dplyr::arrange(.data$trust, .data$specialty, .data$months_waited_id) |>
    tidyr::nest(s_given_tbl = c("months_waited_id", "s_vals")) |>
    mutate(
      s_given = purrr::map(
        .data$s_given_tbl,
        \(x) x$s_vals
      )
    ) |>
    dplyr::select(!c("s_given_tbl"))

  return(s_given)
}

#' @importFrom NHSRtt optimise_steady_state
append_steady_state <- function(
  referrals,
  target,
  renege_params,
  percentile,
  target_time,
  s_given,
  method,
  tolerance = 0.03
) {
  if (is.na(target)) {
    output <- dplyr::tibble(
      capacity_ss = 0,
      reneges_ss = 0,
      incompletes_ss = 0,
      wl_ss = list(
        data.frame(
          months_waited_id = 0:12,
          r = rep(0, 13),
          service = rep(0, 13),
          sigma = rep(0, 13),
          wlsize = rep(0, 13)
        )
      )
    )
  } else {
    # convert weeks input to months
    target_time <- convert_weeks_to_months(target_time)

    results <- NHSRtt::optimise_steady_state(
      referrals = referrals,
      target = target,
      renege_params = renege_params,
      percentile = percentile,
      target_time = target_time,
      s_given = s_given,
      method = method
    )
    # assign results to best_results in case the first iteration has the lower mae
    # (as best_results won't be updated then)
    best_results <- results

    # compare s_given with modelled s

    s_modelled <- results$waiting_list$sigma / results$mu
    acc <- mean(abs(s_given - s_modelled)) # mae
    # acc <- mean(abs(s_given - s_modelled) / s_given) # mape

    min_acc <- acc
    increment <- 0.01

    while (acc > tolerance & target > 0) {
      target <- target - increment
      results <- NHSRtt::optimise_steady_state(
        referrals = referrals,
        target = target,
        renege_params = renege_params,
        percentile = percentile,
        target_time = target_time,
        s_given = s_given,
        method = method
      )

      s_modelled <- results$waiting_list$sigma / results$mu
      acc <- mean(abs(s_given - s_modelled)) # mae
      # acc <- mean(abs(s_given - s_modelled) / s_given) # mape
      if (any(!is.nan(s_modelled))) {
        if (acc < min_acc) {
          min_acc <- acc
          best_results <- results
        }
      } else {
        break
      }
    }

    if (is.na(acc)) {
      results <- best_results
    } else if (acc >= tolerance) {
      results <- best_results
    }

    output <- dplyr::tibble(
      capacity_ss = results$mu,
      reneges_ss = referrals - results$mu,
      incompletes_ss = results$wlsize,
      wl_ss = list(results$waiting_list)
    )
  }

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
