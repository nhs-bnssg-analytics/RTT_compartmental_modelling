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

## specialty lookup
treatment_function_codes <- c(
  "(:?C_|[INA]P)?100" = "General Surgery",
  "(:?C_|[INA]P)?101" = "Urology",
  "(:?C_|[INA]P)?110" = "Trauma and Orthopaedic",
  "(:?C_|[INA]P)?120" = "Ear Nose and Throat",
  "(:?C_|[INA]P)?130" = "Ophthalmology",
  "(:?C_|[INA]P)?140" = "Oral Surgery",
  "(:?C_|[INA]P)?150" = "Neurosurgical",
  "(:?C_|[INA]P)?160" = "Plastic Surgery",
  "(:?C_|[INA]P)?170" = "Cardiothoracic Surgery",
  "C_300" = "General Internal Medicine",
  "(:?C_|[INA]P)?301" = "Gastroenterology",
  "(:?C_|[INA]P)?320" = "Cardiology",
  "(:?C_|[INA]P)?330" = "Dermatology",
  "(:?C_|[INA]P)?340" = "Respiratory Medicine",
  "(:?C_|[INA]P)?400" = "Neurology",
  "(:?C_|[INA]P)?410" = "Rheumatology",
  "(:?C_|[INA]P)?430" = "Elderly Medicine",
  "(:?C_|[INA]P)?502" = "Gynaecology",
  "X01" = "Other - Total",
  "X02" = "Other - Medical Services",
  "X03" = "Other - Mental Health Services",
  "X04" = "Other - Paediatric Services",
  "X05" = "Other - Surgical Services",
  "X06" = "Other - Other Services",
  "C_999" = "Total"
)

specialty_lkp <- dplyr::tribble(
  ~Treatment.Function.Code,
  ~Treatment.Function.Name,
  "C_100",
  "General Surgery",
  "C_101",
  "Urology",
  "C_110",
  "Trauma and Orthopaedic",
  "C_120",
  "Ear Nose and Throat",
  "C_130",
  "Ophthalmology",
  "C_140",
  "Oral Surgery",
  "C_150",
  "Neurosurgical",
  "C_160",
  "Plastic Surgery",
  "C_170",
  "Cardiothoracic Surgery",
  "C_300",
  "General Internal Medicine",
  "C_301",
  "Gastroenterology",
  "C_320",
  "Cardiology",
  "C_330",
  "Dermatology",
  "C_340",
  "Respiratory Medicine",
  "C_400",
  "Neurology",
  "C_410",
  "Rheumatology",
  "C_430",
  "Elderly Medicine",
  "C_502",
  "Gynaecology",
  "C_999",
  "Total",
  "X01",
  "Other - Total",
  "X02",
  "Other - Medical Services",
  "X03",
  "Other - Mental Health Services",
  "X04",
  "Other - Paediatric Services",
  "X05",
  "Other - Surgical Services",
  "X06",
  "Other - Other Services"
)


# sample input data -------------------------------------------------------

date_start = as.Date("2024-01-01")
date_end = as.Date("2024-12-01")
period_lkp <- dplyr::tibble(
  period = seq(
    from = lubridate::floor_date(
      date_start %m-% months(1),
      unit = "months"
    ),
    to = lubridate::floor_date(
      date_end,
      unit = "months"
    ),
    by = "months"
  )
) |>
  mutate(
    period_id = dplyr::row_number() - 1
  )

max_months <- 12

sample_data <- purrr::map(
  .x = c("referral", "incomplete", "complete"),
  .f = ~ NHSRtt::create_dummy_data(
    type = .x,
    max_months_waited = max_months,
    number_periods = max(period_lkp$period_id),
    seed = 444
  )
) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    months_waited_id = case_when(
      !is.na(referrals) ~ 0L,
      .default = months_waited_id
    ),
    value = case_when(
      !is.na(referrals) ~ referrals,
      !is.na(incompletes) ~ incompletes,
      !is.na(treatments) ~ treatments,
      .default = NA_real_
    ),
    type = case_when(
      !is.na(referrals) ~ "Referrals",
      !is.na(incompletes) ~ "Incomplete",
      !is.na(treatments) ~ "Complete",
      .default = NA_character_
    )
  ) |>
  left_join(
    period_lkp,
    by = join_by(period_id)
  ) |>
  dplyr::relocate(
    period,
    .before = dplyr::everything()
  ) |>
  dplyr::relocate(
    value,
    .after = dplyr::everything()
  ) |>
  select(
    !c(
      "referrals",
      "incompletes",
      "treatments",
      "period_id"
    )
  )

# results data
example_chart_data <- read.csv(
  "tests/testthat/test_data_results.csv"
) |>
  mutate(
    period = as.Date(period, format = "%d/%m/%Y")
  )

# calculate target renege rates
update_renege_rates <- FALSE
# update_renege_rates <- TRUE # uncomment this to update the target renege rates, though this takes a while to run

if (isTRUE(update_renege_rates)) {
  data_raw <- seq(
    from = as.Date("2016-05-01"),
    to = lubridate::floor_date(Sys.Date(), unit = "months") %m-% months(3),
    by = "months"
  ) |>
    (\(x) {
      setNames(
        x,
        nm = seq_len(
          length(x)
        )
      )
    })() |>
    purrr::imap(
      \(x, idx) {
        NHSRtt::get_rtt_data(
          date_start = x,
          date_end = x
        ) |>
          summarise(
            value = sum(value),
            .by = c("trust", "specialty", "period", "months_waited", "type")
          )
      },
      .progress = TRUE
    ) |>
    purrr::list_rbind()

  # generate counts by type for all England specialties
  aggregate_specialty <- data_raw |>
    summarise(
      value = sum(value),
      .by = c("specialty", "period", "type")
    ) |>
    mutate(
      specialty = replace_fun(
        .data$specialty,
        treatment_function_codes
      )
    ) |>
    summarise(
      value = sum(value),
      .by = c("specialty", "period", "type")
    )

  # calculate the 92% performance for each month and specialty
  monthly_performance <- data_raw |>
    filter(type == "Incomplete") |>
    summarise(
      value = sum(value),
      .by = c("specialty", "period", "months_waited")
    ) |>
    mutate(
      specialty = replace_fun(
        .data$specialty,
        treatment_function_codes
      ),
      months_waited_id = convert_months_waited_to_id(months_waited, 12)
    ) |>
    summarise(
      value = sum(value),
      .by = c("specialty", "period", "months_waited_id")
    ) |>
    group_by(specialty, period) |>
    calc_performance(4)

  england_reneges <- aggregate_specialty |>
    tidyr::pivot_wider(
      names_from = type,
      values_from = value
    ) |>
    arrange(
      specialty,
      period
    ) |>
    mutate(
      incomplete_lag = dplyr::lag(Incomplete),
      renege = Referrals - (Complete + (incomplete_lag - Incomplete)),
      proportion_renege = renege / (Complete + renege),
      .by = "specialty"
    ) |>
    left_join(
      monthly_performance,
      by = join_by(
        specialty,
        period
      )
    ) |>
    filter(period != min(period)) |>
    group_by(specialty) |>
    arrange(period) |>
    mutate(
      row_id = dplyr::row_number(), # Track original row positions
      proportion_renege_NAs = case_when(
        proportion_renege > 1 | proportion_renege < 0 ~ NA_real_,
        .default = proportion_renege
      )
    ) |>
    select(
      !c("Incomplete", "Complete", "Referrals", "incomplete_lag", "renege")
    ) |>
    nest(
      data = c(period, proportion_renege, proportion_renege_NAs, row_id, prop)
    ) |>
    mutate(
      data = map(data, function(df_group) {
        # Filter out rows with NA in value
        df_clean <- df_group |> filter(!is.na(proportion_renege_NAs))

        # Fit loess only on clean data
        fit <- loess(
          proportion_renege_NAs ~ as.numeric(period),
          data = df_clean
        )

        # Add fitted values back to clean data
        df_clean$smoothed_value <- fit$fitted

        # Join back to original group data
        left_join(
          df_group,
          df_clean |> select(row_id, smoothed_value),
          by = "row_id"
        )
      })
    ) |>
    unnest(data) |>
    select(-row_id) |>
    ungroup() |>
    mutate(
      recent_high_date = {
        # Filter rows where proportion_renege is between 0 and 1
        valid_rows <- smoothed_value > 0 &
          smoothed_value < 1 &
          !is.na(smoothed_value)

        # Check if any of those rows have value >= 0.92
        if (any(prop[valid_rows] >= 0.92)) {
          period == max(period[valid_rows & prop >= 0.92], na.rm = TRUE)
        } else {
          period == period[which.max(ifelse(valid_rows, prop, NA))]
        }
      },
      .by = "specialty"
    )

  target_renege_proportions <- england_reneges |>
    filter(recent_high_date == TRUE) |>
    select(specialty, period, smoothed_value) |>
    rename(renege_proportion = smoothed_value)
} else {
  target_renege_proportions <- RTTshiny:::target_renege_proportions
}


usethis::use_data(
  org_lkp,
  trust_lkp,
  treatment_function_codes,
  specialty_lkp,
  sample_data,
  example_chart_data,
  target_renege_proportions,
  internal = TRUE,
  overwrite = TRUE
)
