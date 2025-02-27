#' Calculates performance by period from a given data set and target stock
#'
#' @param incompletes_data tibble; requires column headers of "period",
#'   "months_waited_id" and "value", where value is the count of incomplete
#'   pathways by period and the number of months the patients have waited
#' @param target_bin integer; the number of months waited where patients that
#'   have waited for greater or equal to that number of months have breached
#'   performance
#' @return A two column tibble containing "period" and "prop" columns, where
#'   prop is the proportion of people that are on the waiting list in each
#'   period that have waited for the time less that specified by the target_bin
#'
#' @noRd
calc_performance <- function(incompletes_data, target_bin) {
  performance <- incompletes_data |>
    mutate(
      perf = case_when(
        months_waited_id < target_bin ~ "Below",
        .default = "Above"
      )
    ) |>
    summarise(
      value = sum(value),
      .by = c(
        period, perf
      )
    ) |>
    mutate(
      prop = value / sum(value),
      .by = period
    ) |>
    filter(
      perf == "Below"
    ) |>
    select(
      "period", "prop"
    )

  return(performance)
}
