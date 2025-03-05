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
    progress) {


  all_dates <- seq(
    from = lubridate::floor_date(
      date_start, unit = "months"
    ),
    to = lubridate::floor_date(
      date_end, unit = "months"
    ),
    by = "months"
  ) |>
    (\(x) setNames(
      x,
      nm = seq_len(
        length(x)
      )
    )
  )()

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
