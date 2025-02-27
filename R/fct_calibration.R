#' Create the calibration dataset from the main data table
#'
#' @param data table of referrals, competes and incompletes (as different
#'   types); data needs the following field names: trust, specialty, period_id,
#'   type, months_waited_id, value
#' @param max_months_waited integer; the stock to pool the stocks that have
#'   waited longer into
#'
#' @importFrom dplyr filter distinct rename left_join join_by
#' @importFrom tidyr complete nest
create_modelling_data <- function(data, max_months_waited = 12) {
  periods <- unique(
    sort(
      data$period_id
    )
  )

  months_waited <- unique(
    sort(
      data$months_waited_id
    )
  )

  specialties <- unique(
    sort(
      data$specialty
    )
  )

  referrals <- data |>
    filter(
      .data$type == "Referrals"
    ) |>
    distinct(
      .data$trust,
      .data$specialty,
      .data$period_id,
      .data$value
    ) |>
    rename(
      referrals = "value"
    ) |>
    tidyr::complete(
      period_id = .data$periods,
      specialty = .data$specialties,
      .data$trust,
      fill = list(referrals = 0)
    ) |>
    tidyr::nest(
      referrals_data = c(
        .data$period_id,
        .data$referrals
      )
    )

  completes <- data |>
    filter(
      .data$type == "Complete"
    ) |>
    distinct(
      .data$trust,
      .data$specialty,
      .data$period_id,
      .data$months_waited_id,
      .data$value
    ) |>
    rename(
      treatments = "value"
    ) |>
    tidyr::complete(
      specialty = .data$specialties,
      period_id = .data$periods,
      months_waited_id = .data$months_waited,
      .data$trust,
      fill = list(treatments = 0)
    ) |>
    tidyr::nest(
      completes_data = c(
        .data$period_id,
        .data$months_waited_id,
        .data$treatments
      )
    )


  incompletes <- data |>
    filter(
      .data$type == "Incomplete"
    ) |>
    distinct(
      .data$trust,
      .data$specialty,
      .data$period_id,
      .data$months_waited_id,
      .data$value
    ) |>
    rename(
      incompletes = "value"
    ) |>
    tidyr::complete(
      specialty = .data$specialties,
      period_id = .data$periods,
      months_waited_id = .data$months_waited,
      .data$trust,
      fill = list(incompletes = 0)
    ) |>
    tidyr::nest(
      incompletes_data = c(
        .data$period_id,
        .data$months_waited_id,
        .data$incompletes
      )
    )

  all_calibration_data <- completes |>
    left_join(
      referrals,
      by = join_by(
        .data$trust, .data$specialty
      )
    ) |>
    left_join(
      incompletes,
      by = join_by(
        .data$trust, .data$specialty
      )
    )

  return(all_calibration_data)

}


#' calibrate data based on the time series supplied
#'
#' @description Uses the NHSRtt package to create the calibration parameters for
#'   the downloaded data
#'
#' @importFrom purrr pmap
#' @importFrom NHSRtt calibrate_capacity_renege_params
#' @importFrom dplyr select mutate
#' @return a single row tibble with a nested tables containing  the calibrated
#'   parameters data
#'
#' @noRd
calibrate_parameters <- function(rtt_data, max_months_waited = 12, full_breakdown = FALSE) {

  params <- create_modelling_data(rtt_data) |>
    mutate(
      params = purrr::pmap(
        .l = list(
          .data$referrals_data,
          .data$completes_data,
          .data$incompletes_data
        ),
        .f = \(ref, comp, incomp) NHSRtt::calibrate_capacity_renege_params(
          referrals = ref,
          completes = comp,
          incompletes = incomp,
          max_months_waited = max_months_waited,
          redistribute_m0_reneges = FALSE,
          full_breakdown = full_breakdown
        )
      )
    )

  if (full_breakdown == FALSE) {
    params <- params |>
      select(
        "trust", "specialty", "params"
      )
  }

  return(params)
}
