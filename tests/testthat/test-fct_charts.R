test_that("plot_output function", {

  target_data <- example_chart_data |>
    filter(period == as.Date("2026-04-01")) |>
    mutate(months_waited_id = extract_first_number(months_waited_id)) |>
    rename(value = incompletes) |>
    calc_performance(target_bin = 4) |>
    mutate(prop = 100 * round(prop, 3)) |>
    rename(
      Target_date = "period",
      Target_percentage = "prop"
    )

  # referrals chart
  vdiffr::expect_doppelganger(
    title = "referrals chart",
    plot_output(
      data = example_chart_data |>
        dplyr::filter(.data$months_waited_id == "0-1 months") |>
        dplyr::mutate(p_var  = sum(.data$adjusted_referrals),
                      .by = c("period", "period_type")) |>
        extend_period_type_data(),
      p_trust = "Example trust",
      p_speciality = "specialty selection",
      p_chart = "referrals",
      p_scenario = "Estimate performance (from treatment capacity inputs)",
      p_cap_change = 5,
      p_cap_skew = 1,
      p_cap_change_type = "linear",
      p_target_data = NULL,
      p_referrals_percent_change = 3,
      p_referrals_change_type = "linear",
      p_perc = FALSE,
      p_facet = FALSE,
      p_target_line = FALSE
    )
  )

  # treatment capacity charts

  vdiffr::expect_doppelganger(
    title = "capacity total chart",
    plot_output(
      data = example_chart_data |>
        dplyr::summarise(p_var = sum(.data$calculated_treatments, na.rm = T),
                         .by = c("period", "period_type")) |>
        extend_period_type_data(),
      p_trust = "Example trust",
      p_speciality = "specialty selection",
      p_chart = "total treatment capacity",
      p_scenario = "Estimate performance (from treatment capacity inputs)",
      p_cap_change = 5,
      p_cap_skew = 1,
      p_cap_change_type = "linear",
      p_target_data = NULL,
      p_referrals_percent_change = 3,
      p_referrals_change_type = "linear",
      p_perc = FALSE,
      p_facet = FALSE,
      p_target_line = FALSE
    )
  )

  vdiffr::expect_doppelganger(
    title = "capacity facet chart",
    plot_output(
      data = example_chart_data |>
        dplyr::summarise(p_var = sum(.data$calculated_treatments, na.rm = T),
                         .by = c("period", "period_type", "months_waited_id")) |>
        extend_period_type_data(),
      p_trust = "Example trust",
      p_speciality = "specialty selection",
      p_chart = "treatment capacity by months waiting",
      p_scenario = "Estimate performance (from treatment capacity inputs)",
      p_cap_change = 5,
      p_cap_skew = 1,
      p_cap_change_type = "linear",
      p_target_data = NULL,
      p_referrals_percent_change = 3,
      p_referrals_change_type = "linear",
      p_perc = FALSE,
      p_facet = TRUE,
      p_target_line = FALSE
    )
  )

  # reneges charts
  vdiffr::expect_doppelganger(
    title = "reneges total chart",
    plot_output(
      data = example_chart_data |>
        dplyr::summarise(p_var = sum(.data$reneges, na.rm = T),
                         .by = c("period", "period_type")) |>
        extend_period_type_data(),
      p_trust = "Example trust",
      p_speciality = "specialty selection",
      p_chart = "total net reneges",
      p_scenario = "Estimate performance (from treatment capacity inputs)",
      p_cap_change = 5,
      p_cap_skew = 1,
      p_cap_change_type = "uniform",
      p_target_data = NULL,
      p_referrals_percent_change = 3,
      p_referrals_change_type = "linear",
      p_perc = FALSE,
      p_facet = FALSE,
      p_target_line = FALSE
    )
  )

  vdiffr::expect_doppelganger(
    title = "reneges facet chart",
    plot_output(
      data = example_chart_data |>
        dplyr::summarise(p_var = sum(.data$reneges, na.rm = T),
                         .by = c("period", "period_type", "months_waited_id")) |>
        extend_period_type_data(),
      p_trust = "Example trust",
      p_speciality = "specialty selection",
      p_chart = "net reneges by months waiting",
      p_scenario = "Estimate treatment capacity (from performance inputs)",
      p_cap_change = 5,
      p_cap_skew = 1,
      p_cap_change_type = "linear",
      p_target_data = target_data,
      p_referrals_percent_change = 3,
      p_referrals_change_type = "uniform",
      p_perc = FALSE,
      p_facet = TRUE,
      p_target_line = FALSE
    )
  )

# waiting list charts
  vdiffr::expect_doppelganger(
    title = "waiting list total chart",
    plot_output(
      data = example_chart_data |>
        dplyr::summarise(p_var = sum(.data$incompletes, na.rm = T),
                         .by = c("period", "period_type")) |>
        extend_period_type_data(),
      p_trust = "Example trust",
      p_speciality = "specialty selection",
      p_chart = "waiting list size",
      p_scenario = "Estimate treatment capacity (from performance inputs)",
      p_cap_change = 5,
      p_cap_skew = 1,
      p_cap_change_type = "uniform",
      p_target_data = target_data,
      p_referrals_percent_change = 3,
      p_referrals_change_type = "linear",
      p_perc = FALSE,
      p_facet = FALSE,
      p_target_line = FALSE
    )
  )

  vdiffr::expect_doppelganger(
    title = "waiting list facet chart",
    plot_output(
      data = example_chart_data |>
        dplyr::mutate(p_var = .data$incompletes) |>
        extend_period_type_data(),
      p_trust = "Example trust",
      p_speciality = "specialty selection",
      p_chart = "numbers waiting by period",
      p_scenario = "Estimate performance (from treatment capacity inputs)",
      p_cap_change = 5,
      p_cap_skew = 1,
      p_cap_change_type = "linear",
      p_target_data = NULL,
      p_referrals_percent_change = 3,
      p_referrals_change_type = "uniform",
      p_perc = FALSE,
      p_facet = TRUE,
      p_target_line = FALSE
    )
  )

  # performance chart
  vdiffr::expect_doppelganger(
    title = "performance chart",
    plot_output(
      data = example_chart_data |>
        dplyr::rename(value = "incompletes") |>
        dplyr::group_by(.data$period_type) |>
        mutate(
          months_waited_id = extract_first_number(.data$months_waited_id)
        ) |>
        calc_performance(
          target_bin = 4
        ) |>
        ungroup() |>
        rename(p_var = "prop") |>
        extend_period_type_data(),
      p_trust = "Example trust",
      p_speciality = "specialty selection",
      p_chart = "18 weeks performance",
      p_scenario = "Estimate treatment capacity (from performance inputs)",
      p_cap_change = 5,
      p_cap_skew = 1,
      p_cap_change_type = "uniform",
      p_target_data = target_data,
      p_referrals_percent_change = 3,
      p_referrals_change_type = "uniform",
      p_perc = TRUE,
      p_facet = FALSE,
      p_target_line = TRUE
    )
  )


})


test_that("performance_text works", {
  df <- dplyr::tibble(
    Target_date = as.Date("2022-03-01") %m+% months(c(0, 12, 24)),
    Target_percentage = c(60, 65, 70)
  )


  expect_equal(
    performance_text(df),
    "60% (Mar 2022), 65% (Mar 2023) and 70% (Mar 2024)"
  )

  expect_equal(
    df |>
      head(2) |>
      performance_text(),
    "60% (Mar 2022) and 65% (Mar 2023)"
  )

  expect_equal(
    df |>
      head(1) |>
      performance_text(),
    "60% (Mar 2022)"
  )
})


# Test suite for extend_period_type_data
test_that("Test extend_period_type_data", {

  # Helper function to create a sample dataset
  create_sample_data <- function() {
    data.frame(
      period = as.Date(c("2023-01-01", "2023-02-01","2023-03-01", "2023-04-01")),
      period_type = c("Type A", "Type A", "Type B", "Type B"),
      value = c(10, 20, 15, 25)
    )
  }

  # Test case 1: Basic test - check if the last period is extended correctly for each type

  sample_data <- create_sample_data()
  result <- extend_period_type_data(sample_data)

  # Check that the number of rows is correct (2 new rows added)
  expect_equal(
    nrow(result),
    nrow(sample_data) + 2,
    info = "Number of new records are correct"
  )

  # Check that the new periods are correct
  expected_new_periods <- as.Date(c("2023-03-01", "2023-05-01"))
  actual_new_periods <- result |>
    dplyr::anti_join(
      sample_data,
      by = join_by(
        period, period_type
      )
    )

  expect_equal(
    actual_new_periods$period,
    expected_new_periods,
    info = "Extends the last period by one month for each period_type"
  )

  # Check that the values from the last month are copied correctly
  expected_values_A <- 20
  expected_values_B <- 25

  actual_values_A <- actual_new_periods |>
    filter(period_type == "Type A") |>
    pull(value)
  actual_values_B <- actual_new_periods |>
    filter(period_type == "Type B") |>
    pull(value)
  expect_equal(
    actual_values_A,
    expected_values_A,
    info = "New values from the previous month are copied correctly"
  )
  expect_equal(
    actual_values_B,
    expected_values_B,
    info = "New values from the previous month are copied correctly"
  )



  # Test case 2: Test with only one period

  one_period_data <- data.frame(
    period = as.Date(c("2023-01-01", "2023-01-01")),
    period_type = c("Type A", "Type B"),
    value = c(10, 20)
  )
  result <- extend_period_type_data(one_period_data)
  expect_equal(
    nrow(result),
    nrow(one_period_data) + 2,
    info = "Function works with one period only"
  )
  expected_new_periods <- as.Date(c("2023-02-01", "2023-02-01"))

  actual_new_periods <- result |>
    filter(period %in% expected_new_periods) |>
    pull(period)
  expect_equal(
    actual_new_periods,
    expected_new_periods,
    info = "New periods are calculated correctly when one period is passed to the function"
  )


})

test_that("holding_chart is consistent", {

  vdiffr::expect_doppelganger(
    title = "holding chart to signpost to modelling",
    fig = holding_chart(type = "model")
  )

  vdiffr::expect_doppelganger(
    title = "holding chart to signport to chart selection",
    fig = holding_chart(type = "select_chart")
  )

  expect_snapshot(
    holding_chart(type = "other"),
    error = TRUE
  )

})

test_that("plot_skew is consistent" ,{

  dummy_params <- dplyr::tibble(
    months_waited_id = 0:6,
    renege_param = c(0.3, 0.1, 0.05, 0.02, 0.02, 0.03, 0.15),
    capacity_param = c(0.4, 0.2, 0.03, 0.04, 0.03, 0.06, 0.25)
  )

  expect_snapshot(
    plot_skew(
      params = dummy_params,
      skew_values = 1.5,
      pivot_bin = 2,
      skew_method = "bad input"
    ),
    error = TRUE
  )

  vdiffr::expect_doppelganger(
    title = "NULL data",
    fig = plot_skew(params = NULL)
  )

  vdiffr::expect_doppelganger(
    title = "uniform skew",
    fig = plot_skew(
      params = dummy_params,
      skew_values = 1.5,
      pivot_bin = 3,
      skew_method = "uniform"
    )
  )

  vdiffr::expect_doppelganger(
    title = "rotate skew",
    fig = plot_skew(
      params = dummy_params,
      skew_values = 0.7,
      pivot_bin = 2,
      skew_method = "rotate"
    )
  )

  vdiffr::expect_doppelganger(
    title = "multiple skews",
    fig = plot_skew(
      params = dummy_params,
      skew_values = c(0.5, 2.5),
      pivot_bin = 2,
      skew_method = "rotate"
    )
  )

  expect_error(
    plot_skew(
      params = dummy_params,
      skew_values = c(0.5, 2.5, 5),
      pivot_bin = 2,
      skew_method = "rotate"
    ),
    regexp = "skew_values must have length 1 or 2",
    info = "Too many skew values supplied"
  )
})

test_that("calc_breaks functionality", {

  date_limits <- as.Date(c("2020-01-01", "2024-01-01"))
  unfacetted_breaks <- january_breaks(
    date_limits
  )

  facetted_breaks <- january_breaks_facetted(
    date_limits
  )

  expect_equal(
    unfacetted_breaks,
    as.Date(
      c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01", "2021-01-01",
        "2021-04-01", "2021-07-01", "2021-10-01", "2022-01-01", "2022-04-01",
        "2022-07-01", "2022-10-01", "2023-01-01", "2023-04-01", "2023-07-01",
        "2023-10-01", "2024-01-01"
      )
    ),
    info = "unfacetted breaks are calculated consistently"
  )

  expect_equal(
    facetted_breaks,
    as.Date(
      c(
        "2020-01-01", "2020-07-01", "2021-01-01", "2021-07-01", "2022-01-01",
        "2022-07-01", "2023-01-01", "2023-07-01", "2024-01-01"
      )
    ),
    info = "facetted breaks are calculated consistently"
  )

  expect_true(
    any(months(unfacetted_breaks) == "January"),
    info = "January in unfacetted breaks"
  )

  expect_true(
    any(months(facetted_breaks) == "January"),
    info = "January in facetted breaks"
  )

})


test_that("click_info works", {
  chart_data <- tibble(
    period = seq(
      from = as.Date("2020-01-01"),
      to = as.Date("2020-12-01"),
      by = "months"
    ),
    period_type = c(rep("Observed", 4), rep("Projected", 8)),
    value = 1:12
  )

  chart_data <- chart_data |>
    bind_rows(
      chart_data |>
        dplyr::slice(4) |>
        mutate(period_type = "Projected")
    )

  expect_equal(
    click_info(
      data = chart_data,
      click_x = as.Date("2020-06-12")
    ),
    tibble(
      period = as.Date("2020-06-01"),
      period_type = "Projected",
      value = 6,
      months_waited_id = NA_real_
    ),
    info = "click_info works"
  )

  expect_equal(
    click_info(
      data = chart_data,
      click_x = as.Date("2020-04-06")
    ),
    tibble(
      period = as.Date("2020-04-01"),
      period_type = "Projected",
      value = 4,
      months_waited_id = NA_real_
    ),
    info = "click_info works for first month of projected period selected"
  )


  chart_data <- chart_data |>
    cross_join(
      tibble(
        months_waited_id = 0:4
      )
    ) |>
    mutate(
      value = value * months_waited_id
    )

  expect_equal(
    click_info(
      data = chart_data,
      click_x = as.Date("2020-02-20"),
      facet = 3
    ),
    tibble(
      period = as.Date("2020-02-01"),
      period_type = "Observed",
      value = 6,
      months_waited_id = 3
    ),
    info = "click_info works for facetted chart"
  )

})



# tool tip tests ----------------------------------------------------------

test_that("tooltip testing", {

  vdiffr::expect_doppelganger(
    title = "Linear tooltip",
    linear_tooltip()
  )

  vdiffr::expect_doppelganger(
    title = "Uniform tooltip",
    uniform_tooltip()
  )

  expect_snapshot(
    linear_uniform_tooltip(
      uniform_id = "dummy_uniform",
      linear_id = "dummy_linear"
    )
  )

  expect_snapshot(
    skew_tooltip()
  )
})
