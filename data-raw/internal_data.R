org_lkp <- NHSRtt::latest_orgs()

# Steady State Inputs
org_lkp_ss_inputs <- org_lkp |>
  distinct(
    `NHS Region Name`,
    `Provider Parent Name`,
    `Provider Org Name`
  ) |>
  dplyr::select(
    Region = "NHS Region Name",
    ICBFull = "Provider Parent Name",
    Trust = "Provider Org Name"
  ) |>
  mutate(ICB = gsub("NHS | INTEGRATED CARE BOARD", "", ICBFull))

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
  ~Treatment.Function.Code         ,
  ~Treatment.Function.Name         ,
  "C_100"                          ,
  "General Surgery"                ,
  "C_101"                          ,
  "Urology"                        ,
  "C_110"                          ,
  "Trauma and Orthopaedic"         ,
  "C_120"                          ,
  "Ear Nose and Throat"            ,
  "C_130"                          ,
  "Ophthalmology"                  ,
  "C_140"                          ,
  "Oral Surgery"                   ,
  "C_150"                          ,
  "Neurosurgical"                  ,
  "C_160"                          ,
  "Plastic Surgery"                ,
  "C_170"                          ,
  "Cardiothoracic Surgery"         ,
  "C_300"                          ,
  "General Internal Medicine"      ,
  "C_301"                          ,
  "Gastroenterology"               ,
  "C_320"                          ,
  "Cardiology"                     ,
  "C_330"                          ,
  "Dermatology"                    ,
  "C_340"                          ,
  "Respiratory Medicine"           ,
  "C_400"                          ,
  "Neurology"                      ,
  "C_410"                          ,
  "Rheumatology"                   ,
  "C_430"                          ,
  "Elderly Medicine"               ,
  "C_502"                          ,
  "Gynaecology"                    ,
  "C_999"                          ,
  "Total"                          ,
  "X01"                            ,
  "Other - Total"                  ,
  "X02"                            ,
  "Other - Medical Services"       ,
  "X03"                            ,
  "Other - Mental Health Services" ,
  "X04"                            ,
  "Other - Paediatric Services"    ,
  "X05"                            ,
  "Other - Surgical Services"      ,
  "X06"                            ,
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
  dplyr::mutate(
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
    months_waited_id = dplyr::case_when(
      !is.na(referrals) ~ 0L,
      .default = months_waited_id
    ),
    value = dplyr::case_when(
      !is.na(referrals) ~ referrals,
      !is.na(incompletes) ~ incompletes,
      !is.na(treatments) ~ treatments,
      .default = NA_real_
    ),
    type = dplyr::case_when(
      !is.na(referrals) ~ "Referrals",
      !is.na(incompletes) ~ "Incomplete",
      !is.na(treatments) ~ "Complete",
      .default = NA_character_
    )
  ) |>
  dplyr::left_join(
    period_lkp,
    by = dplyr::join_by(period_id)
  ) |>
  dplyr::relocate(
    period,
    .before = dplyr::everything()
  ) |>
  dplyr::relocate(
    value,
    .after = dplyr::everything()
  ) |>
  dplyr::select(
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
  dplyr::mutate(
    period = as.Date(period, format = "%d/%m/%Y")
  )

# pins board location
board_12 <- "https://connect.strategyunitwm.nhs.uk/rtt-data/12-months/"
board_24 <- "https://connect.strategyunitwm.nhs.uk/rtt-data/24-months/"

# inpatient/outpatient/activity split
# provided by Suchi Collingwood (via Lucy Morgan) at NHSE
raw_activity_proportions <- tibble::tribble(
  ~treatment_function         , ~op_only_split , ~mixed_split , ~avg_op_first_activity_per_pathway_op_only , ~avg_op_flup_activity_per_pathway_op_only , ~avg_op_first_activity_per_pathway_mixed , ~avg_op_flup_activity_per_pathway_mixed , ~avg_ip_activity_per_pathway_mixed , ~daycase_rate ,
  "Respiratory Medicine"      , 0.847          , 0.153        , 1.41                                       , 2.15                                      , 0.73                                     , 2.57                                    , 1.65                               , 0.8250833     ,
  "Oral Surgery"              , 0.724          , 0.276        , 0.78                                       , 0.71                                      , 0.6                                      , 0.97                                    , 0.98                               , 0.94          ,
  "Gastroenterology"          , 0.36           , 0.64         , 1.49                                       , 1.27                                      , 0.41                                     , 0.87                                    , 2                                  , 0.98075       ,
  "Trauma and Orthopaedic"    , 0.785          , 0.215        , 1.35                                       , 1.56                                      , 0.9                                      , 3.38                                    , 1.58                               , 0.59025       ,
  "General Internal Medicine" , 0.703          , 0.297        , 1.74                                       , 2.48                                      , 0.44                                     , 0.8                                     , 3.37                               , 0.91775       ,
  "Cardiothoracic Surgery"    , 0.604          , 0.396        , 0.41                                       , 0.61                                      , 0.42                                     , 1.02                                    , 0.44                               , 0.046         ,
  "Plastic Surgery"           , 0.602          , 0.398        , 1.32                                       , 1.67                                      , 1.02                                     , 2.79                                    , 1.6                                , 0.8735833     ,
  "General Surgery"           , 0.519          , 0.481        , 0.6                                        , 0.56                                      , 0.44                                     , 0.66                                    , 0.84                               , 0.83425       ,
  "Dermatology"               , 0.879          , 0.121        , 1.1                                        , 1.69                                      , 0.78                                     , 1.9                                     , 1.06                               , 0.9881667     ,
  "Cardiology"                , 0.865          , 0.135        , 1.8                                        , 1.56                                      , 0.91                                     , 2.8                                     , 2.04                               , 0.8150833     ,
  "Urology"                   , 0.664          , 0.336        , 1.13                                       , 1.49                                      , 0.61                                     , 2.17                                    , 1.52                               , 0.7416667     ,
  "Rheumatology"              , 0.749          , 0.251        , 1.62                                       , 3.65                                      , 0.2                                      , 1.65                                    , 1.43                               , 0.9836667     ,
  "Neurosurgical"             , 0.744          , 0.256        , 1.22                                       , 1.17                                      , 0.73                                     , 2.49                                    , 1.45                               , 0.3155        ,
  "Ear Nose and Throat"       , 0.861          , 0.139        , 1.11                                       , 0.87                                      , 0.77                                     , 2.21                                    , 1.13                               , 0.735         ,
  "Other - Total"             , 0.814          , 0.186        , 1.67                                       , 3.95                                      , 0.7                                      , 4.99                                    , 3.24                               , 0.8640833     ,
  "Ophthalmology"             , 0.733          , 0.267        , 1.34                                       , 2.35                                      , 0.74                                     , 3.02                                    , 1.42                               , 0.98          ,
  "Neurology"                 , 0.852          , 0.148        , 1.48                                       , 1.53                                      , 0.26                                     , 1.15                                    , 1.55                               , 0.9111667     ,
  "Gynaecology"               , 0.842          , 0.158        , 1.22                                       , 1.03                                      , 0.88                                     , 1.82                                    , 1.53                               , 0.7155        ,
  "Elderly Medicine"          , 0.926          , 0.074        , 1.37                                       , 1.72                                      , 0.45                                     , 1.41                                    , 1.87                               , 0.6304167     ,
  "Total"                     , 0.766          , 0.234        , 1.47                                       , 1.89                                      , 0.76                                     , 2.76                                    , 1.60                               , 0.28
)

# Column definitions:
## PATHWAY DISTRIBUTION
# op_only_split -> OP only pathways
# mixed_split -> OP and IP mixed pathways
## ACTIVITY PER PATHWAYS ASSUMPTIONS - OP ONLY ACTIVITY PER PATHWAY
# avg_op_first_activity_per_pathway_op_only -> Average OP First Attendances
# avg_op_flup_activity_per_pathway_op_only -> Average OP Follow-up Attendances
## ACTIVITY PER PATHWAY ASSUMPTIONS - MIXED ACTIVITY PER PATHWAY
# avg_op_first_activity_per_pathway_mixed -> Average OP First Attendances
# avg_op_flup_activity_per_pathway_mixed -> Average OP Follow-up attendances
# avg_ip_activity_per_pathway_mixed -> Average IP activity (ordinary elective spells)
# daycase_rate -> daycase rate (% of IP apts which are daycase)

usethis::use_data(
  org_lkp,
  org_lkp_ss_inputs,
  trust_lkp,
  treatment_function_codes,
  specialty_lkp,
  sample_data,
  example_chart_data,
  board_12,
  board_24,
  raw_activity_proportions,
  internal = TRUE,
  overwrite = TRUE
)
