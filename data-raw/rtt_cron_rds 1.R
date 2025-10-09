# RTT RDS Management - Cron

# update data checks object
data_checks <- system.file(
  "extdata",
  "run_date.rds",
  package = "RTTshiny"
)

if (data_checks == "") {
  data_checks <- c("Data checked" = Sys.Date())
} else {
  data_checks <- readRDS(data_checks)
  data_checks["Data checked"] <- Sys.Date()
}

# Latest date checks
max_rtt_date <- NHSRtt::latest_rtt_date() # the latest month of data

latest_rds <- system.file(
  "extdata",
  "rtt_12months.rds",
  package = "RTTshiny"
)

if (latest_rds != "") {
  df_date_latest <- readRDS(latest_rds) |>
    dplyr::pull(period) |>
    max() # the latest month from RDS
} else {
  df_date_latest <- as.Date("1980-03-01") # a dummy date from a long time ago
}


# Run Check (If new data avaialable then run )
if (format(max_rtt_date, "%Y%m") != format(df_date_latest, "%Y%m")) {
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

  # Get data
  a <- Sys.time() # Start Time

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
      selected_specialties = replace_fun(
        unique(grab_data$specialty),
        treatment_function_codes
      )
    ) |>
    dplyr::select(!c("period_id")) |>
    dplyr::left_join(period_lkp, by = "period")

  other_total <- grab_data |>
    filter(grepl("^Other -", specialty)) |>
    summarise(
      value = sum(value),
      .by = c(
        "type",
        "months_waited_id",
        "period",
        "trust",
        "period_id"
      )
    ) |>
    mutate(
      specialty = "Other - Total"
    )

  grab_data <- grab_data |>
    bind_rows(other_total)

  b <- Sys.time() # End Time
  c <- difftime(b, a) # Total run time
  c # View total run time

  # Save
  if (nrow(grab_data) > 1000) {
    saveRDS(
      grab_data,
      paste(
        system.file(package = "RTTshiny"),
        "extdata",
        "rtt_24months.rds",
        sep = "/"
      )
    )

    # update the data update date
    data_checks["Data updated (24 month)"] <- Sys.Date()
  } else {
    print("Check")
    # Send Email notification
  }

  # Steady State 12 Months
  small_data <- grab_data |>
    dplyr::filter(
      period >= max_download_date %m-% months(12),
      trust %in% lt # Trusts included in input
    )

  # Save
  if (nrow(small_data) > 1000) {
    saveRDS(
      small_data,
      paste(
        system.file(package = "RTTshiny"),
        "extdata",
        "rtt_12months.rds",
        sep = "/"
      )
    )
    # update the data update date
    data_checks["Data updated (12 month)"] <- Sys.Date()
  } else {
    print("Check")
    # Send Email notification
  }
  # Check
} else {
  print("No new data")
}

# -----------------------------------------------------------------

saveRDS(
  data_checks,
  paste(
    system.file(package = "RTTshiny"),
    "extdata",
    "run_date.rds",
    sep = "/"
  )
)
