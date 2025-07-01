test_that("create_modelling_data works", {

  top_bin <- 12
  periods <- 6

  df <- c("referral", "incomplete", "complete") |>
    purrr::map(
      ~ NHSRtt::create_dummy_data(
        type = .x,
        max_months_waited = top_bin,
        number_periods = periods,
        seed  = 444
      )
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      months_waited_id = case_when(
        !is.na(referrals) ~ 0,
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
      ),
      trust = "RXX",
      specialty = "C_XXX"
    ) |>
    select(!c("referrals", "incompletes", "treatments"))

  ref_uplift <- dplyr::tibble(
    trust = unique(df$trust),
    specialty = unique(df$specialty),
    referrals_uplift = 0.1
  )

  modelling_df <- create_modelling_data(
    df,
    max_months_waited = top_bin,
    referrals_uplift = ref_uplift
  )


  expect_equal(
    nrow(modelling_df),
    1,
    info = "1 row returned from create_modelling_data function"
  )

  expect_equal(
    names(modelling_df),
    c("specialty", "trust", "completes_data", "referrals_data", "incompletes_data"),
    info = "names of create_modelling_data outputs are expected"
  )


  expect_equal(
    df |>
      filter(type == "Referrals") |>
      pull(value) |>
      (\(x) x + (x * 0.1))(),
    modelling_df$referrals_data[[1]]$referrals,
    info = "referrals uplift works as expected"
  )

  expect_equal(
    nrow(modelling_df$completes_data[[1]]),
    length(unique(df$period_id)) * length(unique(df$months_waited_id)),
    info = "correct number of records in completes_data"
  )

  expect_equal(
    nrow(modelling_df$incompletes_data[[1]]),
    length(unique(df$period_id)) * length(unique(df$months_waited_id)),
    info = "correct number of records in incompletes_data"
  )

  expect_equal(
    sapply(modelling_df, class),
    c(specialty = "character",
      trust = "character",
      completes_data = "list",
      referrals_data = "list",
      incompletes_data = "list"
    ),
    info = "class of modelling_df fields are correct"
  )
})


test_that("calibrate_parameters works", {

  top_bin <- 12
  periods <- 6

  df <- c("referral", "incomplete", "complete") |>
    purrr::map(
      ~ NHSRtt::create_dummy_data(
        type = .x,
        max_months_waited = top_bin,
        number_periods = periods,
        seed  = 444
      )
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      months_waited_id = case_when(
        !is.na(referrals) ~ 0,
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
      ),
      trust = "RXX",
      specialty = "C_XXX"
    ) |>
    select(!c("referrals", "incompletes", "treatments"))

  ref_uplift <- dplyr::tibble(
    trust = unique(df$trust),
    specialty = unique(df$specialty),
    referrals_uplift = 0
  )

  params <- calibrate_parameters(
    rtt_data = df,
    max_months_waited = top_bin,
    redistribute_m0_reneges = TRUE,
    referrals_uplift = ref_uplift,
    full_breakdown = FALSE
  )

  expect_equal(
    nrow(params$params[[1]]),
    top_bin + 1,
    info = "params generated for each bin"
  )

  expect_equal(
    names(params$params[[1]]),
    c("months_waited_id", "renege_param", "capacity_param"),
    info = "expected names in params output"
  )

  params_full <- calibrate_parameters(
    rtt_data = df,
    max_months_waited = top_bin,
    redistribute_m0_reneges = TRUE,
    referrals_uplift = ref_uplift,
    full_breakdown = TRUE
  )

  expect_equal(
    nrow(params_full$params[[1]]),
    (top_bin + 1) * periods,
    info = "params generated for each bin and time period in full breakdown params output"
  )

  expect_equal(
    names(params_full$params[[1]]),
    c("period_id", "months_waited_id", "node_inflow", "treatments",
      "waiting_same_node", "reneges", "renege_param", "capacity_param"),
    info = "expected names in full breakdown params output"
  )


})
