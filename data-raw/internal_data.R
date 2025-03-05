

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

usethis::use_data(
  org_lkp,
  trust_lkp,
  treatment_function_codes,
  specialty_lkp,
  internal = TRUE,
  overwrite = TRUE
)
