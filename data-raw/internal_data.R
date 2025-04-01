

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
  "(:?C_|[INA]P)?150" ="Neurosurgical",
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
  "X0[1-6]" = "Other",
  "C_999" = "Total"
)

specialty_lkp <- dplyr::tribble(
  ~Treatment.Function.Code,            ~Treatment.Function.Name,
  "C_100",           "General Surgery",
  "C_101",                   "Urology",
  "C_110",    "Trauma and Orthopaedic",
  "C_120",       "Ear Nose and Throat",
  "C_130",             "Ophthalmology",
  "C_140",              "Oral Surgery",
  "C_150",             "Neurosurgical",
  "C_160",           "Plastic Surgery",
  "C_170",    "Cardiothoracic Surgery",
  "C_300", "General Internal Medicine",
  "C_301",          "Gastroenterology",
  "C_320",                "Cardiology",
  "C_330",               "Dermatology",
  "C_340",      "Respiratory Medicine",
  "C_400",                 "Neurology",
  "C_410",              "Rheumatology",
  "C_430",          "Elderly Medicine",
  "C_502",               "Gynaecology",
  "C_999",                     "Total",
  "X02",                       "Other",
  "X03",                       "Other",
  "X04",                       "Other",
  "X05",                       "Other",
  "X06",                       "Other"
)


# sample input data -------------------------------------------------------

date_start = as.Date("2024-01-01")
date_end = as.Date("2024-12-01")
period_lkp <- dplyr::tibble(
  period = seq(
    from = lubridate::floor_date(
      date_start %m-% months(1), unit = "months"
    ),
    to = lubridate::floor_date(
      date_end, unit = "months"
    ),
    by = "months"
  )
) |>
  mutate(
    period_id = dplyr::row_number() - 1
  )

max_months <- 12
periods <- period_lkp$period
compartments <- c(0, seq_len(max_months))

expected_data <- dplyr::bind_rows(
  dplyr::tibble(
    type = rep("Incomplete", length(periods) * length(compartments)),
    period = rep(periods, length(compartments)),
    months_waited_id = rep(compartments, each = length(periods))
  ),
  dplyr::tibble(
    type = rep("Complete", (length(periods) - 1) * length(compartments)),
    period = rep(periods[periods != min(periods)], length(compartments)),
    months_waited_id = rep(compartments, each = (length(periods) - 1))
  ),
  dplyr::tibble(
    type = rep("Referrals", (length(periods) - 1) * 1),
    period = periods[periods != min(periods)],
    months_waited_id = 0
  )
)

sample_data <- purrr::map(
  .x = c("referral", "incomplete", "complete"),
  .f = ~ NHSRtt::create_dummy_data(
      type = .x,
      max_months_waited = max_months,
      number_periods = max(period_lkp$period_id),
      seed  = 444
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
    period, .before = dplyr::everything()
  ) |>
  dplyr::relocate(
    value, .after = dplyr::everything()
  ) |>
  select(
    !c(
      "referrals",
      "incompletes",
      "treatments",
      "period_id"
    )
  ) |>
  dplyr::inner_join(
    expected_data,
    by = join_by(
      period, months_waited_id, type
    )
  )


usethis::use_data(
  org_lkp,
  trust_lkp,
  treatment_function_codes,
  specialty_lkp,
  sample_data,
  internal = TRUE,
  overwrite = TRUE
)
