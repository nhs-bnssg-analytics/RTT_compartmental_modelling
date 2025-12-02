#' ---
#' title: "CRON job for updating RTT pin"
#' ---

# Remotes: nhs-bnssg-analytics/RTTshiny

board <- pins::board_connect()

# Latest date checks
max_rtt_date <- NHSRtt::latest_rtt_date() |> # the latest month of data
  lubridate::ymd() |>
  lubridate::floor_date("month")

latest_pin <- pins::pin_read(board, "rhian.davies/rtt_12months")
pinned_date_latest <-
  latest_pin |>
  dplyr::pull(period) |>
  max() |>
  lubridate::ymd() |>
  lubridate::floor_date("month")

# If there is new data not captured in the pins
# Pull new data
# Push new data to Connect pins
if (max_rtt_date > pinned_date_latest) {
  # Trust info
  org_lkp <- NHSRtt::latest_orgs()

  trust_lkp <- org_lkp |>
    dplyr::distinct(
      .data$`Provider Org Code`,
      .data$`Provider Org Name`
    )

  trust_lkp <- setNames(
    trust_lkp[["Provider Org Name"]],
    nm = trust_lkp[["Provider Org Code"]]
  )
  # Distinct Trusts
  lt <- unique(org_lkp$`Provider Org Name`)

  max_rtt_date <- NHSRtt::latest_rtt_date()

  # Full Data Run
  max_download_date <- lubridate::floor_date(
    max_rtt_date,
    unit = "months"
  ) # the latest month of data

  # min date is the 13th month prior to the latest month of data
  min_download_date <- lubridate::floor_date(
    max_download_date,
    unit = "months"
  ) %m-%
    months(24) # Months to collect

  period_lkp <- dplyr::tibble(
    period = seq(
      from = min_download_date,
      to = max_download_date,
      by = "months"
    )
  ) |>
    dplyr::mutate(
      period_id = dplyr::row_number()
    )

  grab_data <- period_lkp$period |>
    purrr::map(
      ~ NHSRtt::get_rtt_data(
        date_start = .x,
        date_end = .x
      )
    ) |>
    purrr::list_rbind()

  grab_data <- grab_data |>
    RTTshiny:::aggregate_and_format_raw_data(
      min_date = min_download_date,
      max_date = max_download_date,
      selected_specialties = RTTshiny:::replace_fun(
        unique(grab_data$specialty),
        treatment_function_codes
      )
    ) |>
    dplyr::select(!c("period_id")) |>
    dplyr::left_join(period_lkp, by = "period")

  other_total <- grab_data |>
    dplyr::filter(grepl("^Other -", specialty)) |>
    dplyr::summarise(
      value = sum(value),
      .by = c(
        "type",
        "months_waited_id",
        "period",
        "trust",
        "period_id"
      )
    ) |>
    dplyr::mutate(
      specialty = "Other - Total"
    )

  grab_data <- grab_data |>
    dplyr::bind_rows(other_total)

  # Save grab data as rtt 24 months
  board |>
    pins::pin_write(grab_data, "rhian.davies/rtt_24months")

  # Save small data as rtt 12 months
  small_data <- grab_data |>
    dplyr::filter(
      period >= max_download_date %m-% months(12),
      trust %in% lt # Trusts included in input
    )
  board |>
    pins::pin_write(small_data, "rhian.davies/rtt_12months")
}
