#' Create the calibration dataset from the main data table
#'
#' @param data table of referrals, competes and incompletes (as different
#'   types); data needs the following field names: trust, specialty, period_id,
#'   type, months_waited_id, value
#' @param max_months_waited integer; the stock to pool the stocks that have
#'   waited longer into
#' @param referrals_uplift numeric; single value - parameter to apply to
#'   referral inputs (absolute value of the renege_params in the first stock
#'   when calibrating the models). These occur due to under-reporting of
#'   referrals data. This is applied to the observed referrals using the
#'   following formula:
#'
#'   \deqn{referrals_{adjusted} = referrals_{obs} + (referrals_{obs} *
#'   uplift\_parameter)}
#'
#'   See details for more information on this argument and how it is applied.
#'
#' @details This is the maths for stock = 0. We know:
#'
#'
#'   (Equation 1) \deqn{incomplete_{obs} = referrals_{obs} - complete_{obs} -
#'   reneges_{calc}}
#'
#'   BUT, when \eqn{reneges_{calc}} are negative, we want to adjust referrals by
#'   that amount
#'
#'   \deqn{incompletes_{obs} = referrals\_adj_{calc} - complete_{obs}}
#'
#'   WHERE
#'
#'   (Equation 2) \deqn{referrals\_adj_{calc} = referrals_{obs} +
#'   reneges_{calc}} (where \eqn{reneges_{calc}} is from equation 1)
#'
#'   ALSO
#'
#'   (Equation 3)
#'
#'   \deqn{renege\_param_{calc} = \frac{reneges_{calc}}{referrals_{obs}}}
#'
#'   THEREFORE, combining eq. 2 and eq. 3 (substituting \eqn{reneges_{calc}})
#'
#'   (Equation 4) \deqn{renege\_param_{calc} = \frac{referrals\_adj_{calc} -
#'   referrals_{obs}}{referrals_{obs}}}
#'
#'   REARRANGING eq. 4
#'
#'   \deqn{referrals\_adj_{calc} = (renege\_param_{calc} * referrals_{obs}) +
#'   referrals_{obs}}
#'
#'
#' @importFrom dplyr filter distinct rename left_join join_by
#' @importFrom tidyr complete nest
create_modelling_data <- function(
  data,
  max_months_waited = 12,
  referrals_uplift
) {
  required_fields <- c(
    "trust",
    "specialty",
    "period_id",
    "type",
    "months_waited_id",
    "value"
  )

  if (length(dplyr::setdiff(required_fields, names(data)) > 0)) {
    stop(
      "incorrect fields in data object - requires 'trust', 'specialty', 'period_id', 'type', 'months_waited_id', 'value'"
    )
  }

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
      period_id = periods,
      specialty = specialties,
      .data$trust,
      fill = list(referrals = 0)
    )

  if (!is.null(referrals_uplift)) {
    referrals <- referrals |>
      left_join(
        referrals_uplift,
        by = join_by(
          trust,
          specialty
        )
      ) |>
      dplyr::mutate(
        referrals = .data$referrals + (.data$referrals * .data$referrals_uplift)
      ) |>
      select(!c("referrals_uplift"))
  }

  referrals <- referrals |>
    tidyr::nest(
      referrals_data = c(
        "period_id",
        "referrals"
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
      specialty = specialties,
      period_id = periods,
      months_waited_id = months_waited,
      .data$trust,
      fill = list(treatments = 0)
    ) |>
    tidyr::nest(
      completes_data = c(
        "period_id",
        "months_waited_id",
        "treatments"
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
      specialty = specialties,
      period_id = periods,
      months_waited_id = months_waited,
      .data$trust,
      fill = list(incompletes = 0)
    ) |>
    tidyr::nest(
      incompletes_data = c(
        "period_id",
        "months_waited_id",
        "incompletes"
      )
    )

  all_calibration_data <- completes |>
    left_join(
      referrals,
      by = join_by(
        trust,
        specialty
      )
    ) |>
    left_join(
      incompletes,
      by = join_by(
        trust,
        specialty
      )
    )

  return(all_calibration_data)
}


#' calibrate data based on the time series supplied
#'
#' @description Uses the NHSRtt package to create the calibration parameters for
#'   the downloaded data
#' @param rtt_data tibble with columns trust, specialty, period_id, type, months_waited_id and value
#' @param max_months_waited integer; the stock to pool the stocks that have
#'   waited longer into
#' @param referrals_uplift tibble with three columns; trust, specialty and referrals_uplift.
#'   referrals_uplift is numeric and is the multiplier for referral inputs (calculated
#'   from negative renege_params for months_waited_id = 0 when calibrating the models).
#'   These occur due to under-reporting of referrals data
#'   to under-reporting of referrals data
#' @param redistribute_m0_reneges logical; whether to redistribute the negative reneges into
#'   all the other compartments evenly
#' @param full_breakdown logical; whether to return a table of results by period and months_waited_id
#'   (TRUE) or a single record with a nested table of parameters (FALSE)
#'
#' @importFrom purrr pmap
#' @importFrom NHSRtt calibrate_capacity_renege_params
#' @importFrom dplyr select mutate
#' @return a single row tibble with a nested tables containing  the calibrated
#'   parameters data
#'
#' @noRd
calibrate_parameters <- function(
  rtt_data,
  max_months_waited = 12,
  redistribute_m0_reneges,
  referrals_uplift,
  full_breakdown = FALSE
) {
  params <- create_modelling_data(
    data = rtt_data,
    referrals_uplift = referrals_uplift
  ) |>
    mutate(
      params = purrr::pmap(
        .l = list(
          .data$referrals_data,
          .data$completes_data,
          .data$incompletes_data
        ),
        .f = \(ref, comp, incomp) {
          NHSRtt::calibrate_capacity_renege_params(
            referrals = ref,
            completes = comp,
            incompletes = incomp,
            max_months_waited = max_months_waited,
            redistribute_m0_reneges = redistribute_m0_reneges,
            full_breakdown = full_breakdown
          )
        }
      )
    )

  if (full_breakdown == FALSE) {
    params <- params |>
      select(
        "trust",
        "specialty",
        "params"
      )
  }

  return(params)
}
