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
#' @importFrom rlang .data
#' @noRd
calc_performance <- function(incompletes_data, target_bin) {

  # check one record per month waited per period
  check_counts <- incompletes_data |>
    dplyr::count(
      .data$period,
      .data$months_waited_id
    ) |>
    dplyr::filter(
      .data$n > 1
    )

  if (nrow(check_counts) > 0) stop("duplicate counts per period and month waited")

  # check target_bin within the range of bins available
  if (!(target_bin %in% incompletes_data[["months_waited_id"]]))
    stop("target_bin not a valid month waited in the incompletes_data")

  # check names
  if (!all(c("period", "months_waited_id", "value") %in% names(incompletes_data)))
    stop("'period', 'months_waited_id' and 'value' should be present in incompletes_data")

  performance <- incompletes_data |>
    mutate(
      perf = case_when(
        .data$months_waited_id < target_bin ~ "Below",
        .default = "Above"
      )
    ) |>
    summarise(
      value = sum(.data$value),
      .by = c(
        "period", "perf"
      )
    ) |>
    mutate(
      prop = .data$value / sum(.data$value),
      .by = "period"
    ) |>
    filter(
      .data$perf == "Below"
    ) |>
    select(
      "period", "prop"
    )

  return(performance)
}

#' @importFrom dplyr as_tibble filter pull
#' @param lm_object the output from a lm() function
#' @param term string; the term name for the p.value of interest. This is used
#'   for a filter() operation
#' @noRd
extract_pval <- function(lm_object, input_term) {

  if (!inherits(lm_object, "lm")) stop("lm_object not lm class")

  p_val <- summary(lm_object)$coefficients |>
    dplyr::as_tibble(rownames = "term") |>
    filter(.data$term == input_term) |>
    pull(
      .data$`Pr(>|t|)`
    )

  return(p_val)

}

#' Replaces values in a string vector with corresponding values from a named
#' vector
#' @param string_vector A character vector
#' @param replacement_vector A named vector where names correspond to values in
#'   string_vector, adn can be regular expressions, and values are the
#'   replacements
#' @return A character vector with replaced values
#' @noRd
replace_fun <- function(string_vector, replacement_vector) {
  # Check if replacement_vector has names
  if (is.null(names(replacement_vector))) {
    stop("replacement_vector must have names")
  }

  replaced_vector <- string_vector

  for (pattern in names(replacement_vector)) {
    replaced_vector <- ifelse(grepl(pattern, replaced_vector),
                              replacement_vector[pattern],
                              replaced_vector)
  }

  return(replaced_vector)
}
