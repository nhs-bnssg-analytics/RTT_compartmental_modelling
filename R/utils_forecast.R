#' Forecast n period based on a selected method
#'
#' @param rtt_table tibble containing a record for each period, and a value
#'   column
#' @param number_timesteps integer; number of time steps to forecast
#' @param method string; "Uniform" or "Linear"
#' @param percent_change numeric vector; if method is "Linear" thepercent_change
#'   is the annual percentage change required relative to the extrapolated first
#'   time step (where 1 is a 1% annual uplift by time step 13)
#' @importFrom dplyr tibble mutate case_when summarise
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer unnest pivot_wider
#' @importFrom stats lm predict setNames
#' @importFrom rlang .data
#' @noRd
forecast_function <- function(rtt_table, number_timesteps = 13, method, percent_change) {

  method <- match.arg(
    method,
    c("Linear",
      "Uniform")
  )

  if (method == "Uniform") {
    first_val <- calculate_t1_value(rtt_table)

    fcast <- rep(
      first_val * (1 + (percent_change / 100)),
      number_timesteps
    )

  # } else if (method == "tbats") {
  #   fcast <- rtt_table |>
  #     pull(value) |>
  #     ts(frequency = 12) |>
  #     forecast::tbats() |>
  #     forecast::forecast(h = number_timesteps) |>
  #     tidyr::as_tibble() |>
  #     select(
  #       Expected_referrals = "Point Forecast",
  #       Low_referrals = "Lo 80",
  #       High_referrals = "Hi 80"
  #     ) |>
  #     mutate(
  #       period_id = dplyr::row_number()
  #     )
  } else if (method == "Linear") {
    # first, calculate the value for the first time step as either a linear
    # extrapolation of the data provided (if significant) or a mean (if linear
    # model is not significant)

    first_val <- calculate_t1_value(rtt_table)

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
#' @noRd
calculate_t1_value <- function(monthly_rtt) {

  # check names
  if (length(setdiff(c("period_id", "value"), names(monthly_rtt))) > 0)
    stop("monthly_rtt is missing either 'period_id' or 'value' field")

  first_period_id <- max(monthly_rtt[["period_id"]])# + 1

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
        pval <= 0.05 ~ as.numeric(.data$lm_val),
        .default = .data$mean_val
      ),
      # capacity can't be less than zero, so it is fixed to zero if so
      t_1_val = case_when(
        .data$t_1_val < 0 ~ 0,
        .default = .data$t_1_val
      )
    ) |>
    pull(.data$t_1_val)

  return(first_val)
}


