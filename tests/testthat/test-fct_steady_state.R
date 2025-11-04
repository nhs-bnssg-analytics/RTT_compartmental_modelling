df <- sample_data |>
  mutate(
    trust = "trust_a",
    specialty = "Cardiology"
  ) |>
  arrange(type, months_waited_id, period) |>
  mutate(period_id = dplyr::row_number(), .by = c("type", "months_waited_id"))

test_that("append_current_status works", {
  max_months_waited <- max(df$months_waited_id)
  percentile <- 0.92
  percentile_month <- 4

  result <- append_current_status(
    data = df,
    max_months_waited = max_months_waited,
    percentile = percentile,
    percentile_month = percentile_month
  )

  expected_names <- c(
    "trust",
    "specialty",
    "referrals_t1",
    "capacity_t1",
    "reneges_t0",
    "load",
    "incompletes_t0",
    "params",
    "pressure"
  )

  expect_equal(
    names(result),
    expected_names,
    info = "names in output match what is expected"
  )

  expect_equal(
    sapply(result, class),
    c(
      trust = "character",
      specialty = "character",
      referrals_t1 = "numeric",
      capacity_t1 = "numeric",
      reneges_t0 = "numeric",
      load = "numeric",
      incompletes_t0 = "numeric",
      params = "list",
      pressure = "numeric"
    ),
    info = "data types in the result are correct"
  )

  expect_equal(
    round(result$referrals_t1 / (result$capacity_t1 + result$reneges_t0), 2),
    1.33,
    info = "load calculated correctly"
  )

  expect_equal(
    result$pressure,
    3.10157233,
    info = "pressure calculated correctly"
  )
})

test_that("calculate_s_given works - mean", {
  mean_results <- calculate_s_given(
    data = df,
    max_months_waited = 12,
    method = "mean"
  )

  expected_results <- df |>
    filter(type == "Complete") |>
    mutate(value = value / sum(value), .by = c("period")) |>
    summarise(value = mean(value), .by = c("months_waited_id")) |>
    pull(value) |>
    (\(x) x / sum(x))()

  expect_equal(
    mean_results$s_given[[1]],
    expected_results
  )

  expect_equal(
    sapply(mean_results, class),
    c(trust = "character", specialty = "character", s_given = "list")
  )
})

test_that("calculate_s_given works - median", {
  median_results <- calculate_s_given(
    data = df,
    max_months_waited = 12,
    method = "median"
  )

  expected_results <- df |>
    filter(type == "Complete") |>
    mutate(value = value / sum(value), .by = c("period")) |>
    summarise(value = stats::median(value), .by = c("months_waited_id")) |>
    mutate(value = value / sum(value)) |> # to make sure it adds up to 1
    pull(value)

  expect_equal(
    median_results$s_given[[1]],
    expected_results
  )
})

test_that("calculate_s_given works - latest", {
  latest_results <- calculate_s_given(
    data = df,
    max_months_waited = 12,
    method = "latest"
  )

  expected_results <- df |>
    filter(type == "Complete") |>
    mutate(value = value / sum(value), .by = c("period")) |>
    filter(period == max(period)) |>
    pull(value) |>
    (\(x) x / sum(x))()

  expect_equal(
    latest_results$s_given[[1]],
    expected_results
  )
})

test_that("append_steady_state - results", {
  set.seed(1234)
  s_giv <- runif(13) |>
    (\(x) round(x / sum(x), 2))()
  s_giv[13] <- s_giv[13] + 0.01

  r <- runif(13, max = 0.25)

  perc_target <- 0.2
  refs <- 1000
  results <- append_steady_state(
    referrals = refs,
    target = 0.5,
    renege_params = r,
    percentile = perc_target,
    target_time = 4,
    s_given = s_giv,
    method = "lp",
    tolerance = 0.03
  )

  expect_equal(
    sapply(results, class),
    c(
      capacity_ss = "numeric",
      reneges_ss = "numeric",
      incompletes_ss = "numeric",
      wl_ss = "list"
    )
  )

  # target = 0.5, so expect capacity = renege
  expect_equal(
    results$capacity_ss,
    results$reneges_ss,
    info = "target = 0.5, so expect capacity = renege"
  )

  expect_equal(
    sum(results$wl_ss[[1]]$wlsize[1]) / sum(results$wl_ss[[1]]$wlsize),
    perc_target,
    tolerance = 0.2,
    info = "waiting list performance is correct (the tolerance allows for the month to week conversion)"
  )

  # arrivals = departures
  reneges <- sum(
    results$wl_ss[[1]]$r * c(refs, results$wl_ss[[1]]$wlsize[1:12])
  )
  treatments <- sum(results$wl_ss[[1]]$sigma)

  expect_equal(
    reneges + treatments,
    refs,
    info = "arrivals = departures"
  )
})

test_that("append_steady_state - closest results", {
  set.seed(1234)
  s_giv <- runif(13) |>
    (\(x) round(x / sum(x), 2))()
  s_giv[13] <- s_giv[13] + 0.01

  r <- runif(13, max = 0.25)

  perc_target <- 0.5
  refs <- 1000
  results <- append_steady_state(
    referrals = refs,
    target = 0.5,
    renege_params = r,
    percentile = perc_target,
    target_time = 4,
    s_given = s_giv,
    method = "lp",
    tolerance = 0.03
  )

  expect_equal(
    sapply(results, class),
    c(
      capacity_ss = "numeric",
      reneges_ss = "numeric",
      incompletes_ss = "numeric",
      wl_ss = "list"
    )
  )
  # target != 0.5 because closest results are found
  expect_gt(
    results$capacity_ss,
    results$reneges_ss
  )

  expect_equal(
    sum(results$wl_ss[[1]]$wlsize[1]) / sum(results$wl_ss[[1]]$wlsize),
    perc_target,
    tolerance = 0.2,
    info = "waiting list performance is correct (the tolerance allows for the month to week conversion)"
  )

  # arrivals = departures
  reneges <- sum(
    results$wl_ss[[1]]$r * c(refs, results$wl_ss[[1]]$wlsize[1:12])
  )
  treatments <- sum(results$wl_ss[[1]]$sigma)

  expect_equal(
    reneges + treatments,
    refs,
    info = "arrivals = departures"
  )
})

test_that("append_steady_state - failed results", {
  set.seed(1234)
  s_giv <- runif(13) |>
    (\(x) round(x / sum(x), 2))()
  s_giv[13] <- s_giv[13] + 0.01

  # r <- runif(13, max = 0.25) + 0.2

  perc_target <- 0.5
  refs <- 1000
  results <- append_steady_state(
    referrals = refs,
    target = 0.5,
    renege_params = rep(0, 13), # no reneges so will always be 100% treatments for departures
    percentile = perc_target,
    target_time = 4,
    s_given = s_giv,
    method = "lp",
    tolerance = 0.03
  )

  expect_equal(
    sapply(results, class),
    c(
      capacity_ss = "logical",
      reneges_ss = "logical",
      incompletes_ss = "logical",
      wl_ss = "list"
    )
  )

  expect_true(
    all(
      is.na(results$capacity_ss),
      is.na(results$reneges_ss),
      is.na(results$incompletes_ss)
    )
  )

  expect_equal(
    results$wl_ss[[1]],
    data.frame(
      months_waited_id = 0:12,
      r = rep(0, 13),
      service = NA,
      sigma = NA,
      wlsize = NA
    ),
    info = "WL information is all NAs except reneges"
  )
})

test_that("append_steady_state - bad results", {
  set.seed(1234)
  s_giv <- runif(13) |>
    (\(x) round(x / sum(x), 2))()
  s_giv[13] <- s_giv[13] + 0.01

  r <- runif(13, max = 0.25)

  perc_target <- 0.2
  refs <- 1000

  expect_equal(
    append_steady_state(
      referrals = refs,
      target = NA,
      renege_params = r,
      percentile = perc_target,
      target_time = 4,
      s_given = s_giv,
      method = "lp",
      tolerance = 0.03
    ),
    dplyr::tibble(
      capacity_ss = 0,
      reneges_ss = 0,
      incompletes_ss = 0,
      wl_ss = list(
        data.frame(
          months_waited_id = 0:12,
          r = rep(0, 13),
          service = rep(0, 13),
          sigma = rep(0, 13),
          wlsize = rep(0, 13)
        )
      )
    ),
    info = "target is NA"
  )

  expect_equal(
    append_steady_state(
      referrals = 0,
      target = 0.5,
      renege_params = r,
      percentile = perc_target,
      target_time = 4,
      s_given = s_giv,
      method = "lp",
      tolerance = 0.03
    ),
    dplyr::tibble(
      capacity_ss = 0,
      reneges_ss = 0,
      incompletes_ss = 0,
      wl_ss = list(
        data.frame(
          months_waited_id = 0:12,
          r = rep(0, 13),
          service = rep(0, 13),
          sigma = rep(0, 13),
          wlsize = rep(0, 13)
        )
      )
    ),
    info = "referrals are 0"
  )

  expect_equal(
    append_steady_state(
      referrals = refs,
      target = 0.5,
      renege_params = r,
      percentile = perc_target,
      target_time = 4,
      s_given = rep(0, 13),
      method = "lp",
      tolerance = 0.03
    ),
    dplyr::tibble(
      capacity_ss = 0,
      reneges_ss = 0,
      incompletes_ss = 0,
      wl_ss = list(
        data.frame(
          months_waited_id = 0:12,
          r = rep(0, 13),
          service = rep(0, 13),
          sigma = rep(0, 13),
          wlsize = rep(0, 13)
        )
      )
    ),
    info = "s_given is rep(0, 13)"
  )

  expect_equal(
    append_steady_state(
      referrals = refs,
      target = 1,
      renege_params = r,
      percentile = perc_target,
      target_time = 4,
      s_given = s_giv,
      method = "lp",
      tolerance = 0.03
    ),
    dplyr::tibble(
      capacity_ss = 0,
      reneges_ss = 0,
      incompletes_ss = 0,
      wl_ss = list(
        data.frame(
          months_waited_id = 0:12,
          r = rep(0, 13),
          service = rep(0, 13),
          sigma = rep(0, 13),
          wlsize = rep(0, 13)
        )
      )
    ),
    info = "target = 1"
  )

  expect_error(
    append_steady_state(
      referrals = refs,
      target = 1.2,
      renege_params = r,
      percentile = perc_target,
      target_time = 4,
      s_given = s_giv,
      method = "lp",
      tolerance = 0.03
    ),
    "target must be between 0 and 1"
  )

  expect_error(
    append_steady_state(
      referrals = refs,
      target = 0.5,
      renege_params = r,
      percentile = -0.1,
      target_time = 4,
      s_given = s_giv,
      method = "lp",
      tolerance = 0.03
    ),
    "percentile must be between 0 and 1"
  )

  expect_error(
    append_steady_state(
      referrals = refs,
      target = 0.5,
      renege_params = r,
      percentile = 0.2,
      target_time = 4,
      s_given = s_giv + 0.01,
      method = "lp",
      tolerance = 0.03
    ),
    "s_given must sum to 1"
  )

  expect_error(
    append_steady_state(
      referrals = refs,
      target = 0.5,
      renege_params = r,
      percentile = 0.2,
      target_time = 4,
      s_given = s_giv,
      method = "lpl",
      tolerance = 0.03
    ),
    "method must be 'lp' or 'bs'"
  )
})


test_that("append_counterfactual", {
  cap <- 500
  ref <- 1200
  df <- sample_data |>
    arrange(type, months_waited_id, period) |>
    mutate(
      trust = "trust_a",
      specialty = "Cardiology",
      period_id = dplyr::row_number(),
      .by = c("type", "months_waited_id")
    )

  inc <- sample_data |>
    filter(type == "Incomplete", period == max(period)) |>
    select("months_waited_id", incompletes = "value")

  params <- calibrate_parameters(
    rtt_data = df,
    max_months_waited = 12,
    redistribute_m0_reneges = TRUE,
    referrals_uplift = NULL,
    full_breakdown = FALSE,
    allow_negative_params = TRUE
  ) |>
    purrr::pluck("params", 1)

  results <- append_counterfactual(
    capacity = cap,
    referrals_start = ref,
    referrals_end = ref,
    incompletes_t0 = inc,
    renege_capacity_params = params,
    forecast_months = 40,
    target_week = 18
  )

  expect_equal(
    sapply(results, class),
    c(
      reneges_counterf = "numeric",
      incompletes_counterf = "numeric",
      perf_counterf = "numeric"
    )
  )

  expect_equal(
    results$perf_counterf,
    0.809009337
  )
})
