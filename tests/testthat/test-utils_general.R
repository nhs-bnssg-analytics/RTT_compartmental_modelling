test_that("calc_performance errors", {
  incomp_data <- dplyr::tibble(
    period = rep(1:5, each = 5),
    months_waited_id = rep(0:4, times = 5),
    value = rep(4, 25)
  )

  expect_error(
    calc_performance(
      dplyr::bind_rows(
        incomp_data,
        incomp_data
      ),
      target_bin = 3
    ),
    "duplicate counts per period and month waited"
  )

  expect_error(
    calc_performance(
      incomp_data,
      target_bin = 5
    ),
    "target_bin not a valid month waited in the incompletes_data"
  )

  expect_error(
    calc_performance(
      incomp_data |>
        rename(
          incompletes = "value"
        ),
      target_bin = 3
    ),
    "'period', 'months_waited_id' and 'value' should be present in incompletes_data"
  )
})


test_that("calc_performance works", {
  incomp_data <- dplyr::tibble(
    period = rep(1:5, each = 5),
    months_waited_id = rep(0:4, times = 5),
    value = rep(4, 25)
  )

  expect_equal(
    calc_performance(
      incomp_data,
      target_bin = 3
    ),
    dplyr::tibble(
      period = 1L:5L,
      prop = 3 / 5
    ),
    info = "calc_performance works properly"
  )
})


test_that("extract_pval errors", {
  set.seed(123)
  df <- dplyr::tibble(
    x = 1:5,
    y = sample(1:10, 5)
  )

  ft <- lm(y ~ x, data = df)

  expect_error(
    extract_pval(
      lm_object = ft$model,
      input_term = "x"
    ),
    "lm_object not lm class"
  )
})

test_that("extract_pval works", {
  set.seed(123)
  df <- dplyr::tibble(
    x = 1:5,
    y = sample(1:10, 5)
  )

  ft <- lm(y ~ x, data = df)

  expect_equal(
    extract_pval(
      lm_object = ft,
      input_term = "x"
    ),
    0.76082038,
    info = "extract_pval works"
  )
})

test_that("replace_fun errors", {
  vec <- letters

  replace_vec <- c("junk", "flow")

  expect_error(
    replace_fun(
      string = vec,
      replacement_vector = replace_vec
    ),
    "replacement_vector must have names"
  )
})

test_that("replace_fun works", {
  vec <- letters[1:8]

  replace_vec <- c("h" = "junk", "d" = "flow")

  expect_equal(
    replace_fun(
      string = vec,
      replacement_vector = replace_vec
    ),
    c("a", "b", "c", "flow", "e", "f", "g", "junk"),
    info = "replace_fun works"
  )

  expect_equal(
    replace_fun(
      string = "RFF",
      replacement_vector = trust_lkp
    ),
    "BARNSLEY HOSPITAL NHS FOUNDATION TRUST",
    info = "replace_fun works for Barnsley Trust"
  )

  expect_equal(
    replace_fun(
      "C_430",
      treatment_function_codes
    ),
    "Elderly Medicine",
    info = "replace_fun works for specialty"
  )
})

test_that("local_enframe errors", {
  vec <- letters[1:5]

  new_name <- "letters"
  new_values <- "values"

  expect_error(
    local_enframe(
      named_vector = vec,
      name = new_name,
      value_name = new_values
    ),
    "named_vector must have names"
  )
})

test_that("local_enframe works", {
  vec <- setNames(
    1:5,
    nm = letters[1:5]
  )

  new_name <- "letters"
  new_values <- "values"

  expect_equal(
    local_enframe(
      named_vector = vec,
      name = new_name,
      value_name = new_values
    ),
    dplyr::tibble(
      letters = letters[1:5],
      values = 1:5
    ),
    info = "local_enframe works as expected"
  )
})

test_data <- data.frame(
  key = c("a", "b", "c"),
  val = c(1, 2, 3),
  extra = c("x", "y", "z"),
  stringsAsFactors = FALSE
)

test_that("local_deframe returns named vector correctly", {
  result <- local_deframe(test_data, "key", "val")
  expect_named(result, c("a", "b", "c"))
  expect_equal(result, c(a = 1, b = 2, c = 3))
})

test_that("local_deframe works with character values", {
  result <- local_deframe(test_data, "key", "extra")
  expect_equal(result, c(a = "x", b = "y", c = "z"))
})

test_that("local_deframe errors if name_col is missing", {
  expect_error(
    local_deframe(test_data, "missing", "val"),
    "name_col must be column name in data"
  )
})

test_that("local_deframe errors if value_col is missing", {
  expect_error(
    local_deframe(test_data, "key", "missing"),
    "value_col must be column name in data"
  )
})

test_that("local_deframe handles empty data frame", {
  empty_data <- data.frame(key = character(), val = numeric())
  result <- local_deframe(empty_data, "key", "val")
  expect_equal(length(result), 0)
  expect_named(result, character())
})


test_that("returns 'waitlist_cleared' when min projected is zero", {
  incompletes <- c(Projected = 0, Projected = 2, Observed = 5)
  expect_equal(
    min_incompletes_in_projected_period(incompletes),
    "waitlist_cleared"
  )
})

test_that("returns 'waitlist_reduced' when projected < observed", {
  incompletes <- c(Projected = 2, Projected = 3, Observed = 50)
  expect_equal(
    min_incompletes_in_projected_period(incompletes),
    "waitlist_reduced"
  )
})

test_that("returns 'converged' when projected >= observed", {
  incompletes <- c(Projected = 5, Projected = 6, Observed = 5)
  expect_equal(min_incompletes_in_projected_period(incompletes), "converged")
})


test_that("org_name_lkp errors", {
  expect_snapshot(
    org_name_lkp(
      names = "BUCKSHAW HOSPITAL",
      type = "PROVIDER Org"
    ),
    error = TRUE
  )

  expect_warning(
    org_name_lkp(
      names = c("BUCKSHAW HOSPITAL", "Made up hospital"),
      type = "Provider Org"
    ),
    "some names were not translated to codes as they were missing from the lookup"
  )
})

test_that("org_name_lkp works", {
  expect_equal(
    org_name_lkp(
      names = c("London", "South West"),
      type = "NHS Region"
    ),
    c("Y58", "Y56"),
    info = "region lookup works as expected"
  )

  expect_equal(
    org_name_lkp(
      names = "NHS MID AND SOUTH ESSEX INTEGRATED CARE BOARD",
      type = "Provider Parent"
    ),
    "QH8",
    info = "provider parent lookup works as expected"
  )

  expect_equal(
    org_name_lkp(
      names = "NHS GREATER MANCHESTER INTEGRATED CARE BOARD",
      type = "Commissioner Parent"
    ),
    "QOP",
    info = "commissioner parent lookup works as expected"
  )

  expect_equal(
    org_name_lkp(
      names = "BUCKSHAW HOSPITAL",
      type = "Provider Org"
    ),
    "A4M8P",
    info = "provider lookup works as expected"
  )

  expect_equal(
    org_name_lkp(
      names = "NHS BLACKBURN WITH DARWEN (SUB ICB LOCATION)",
      type = "Commissioner Org"
    ),
    "00Q",
    info = "commissioner org lookup works as expected"
  )

  expect_equal(
    org_name_lkp(
      names = NULL,
      type = "Provider Org"
    ),
    NULL,
    info = "NULL input returns NULL output"
  )
})

test_that("filters_displays errors", {
  expect_snapshot(
    filters_displays(
      nhs_only = TRUE,
      trust_parents = "NHS LANCASHIRE AND SOUTH CUMBRIA INTEGRATED CARE BOARD",
      trusts = "FULWOOD HALL HOSPITAL",
      comm_parents = c(
        "NHS SOUTH YORKSHIRE INTEGRATED CARE BOARD",
        "NHS NORTH EAST LONDON INTEGRATED CARE BOARD"
      ),
      comms = NULL,
      spec = "Total"
    ),
    error = TRUE
  )
})

test_that("filters_displays works", {
  specs <- c("General Surgery", "Total")

  lbls <- filters_displays(
    nhs_only = "nhs_only",
    trust_parents = "NHS LANCASHIRE AND SOUTH CUMBRIA INTEGRATED CARE BOARD",
    trusts = "FULWOOD HALL HOSPITAL",
    comm_parents = c(
      "NHS SOUTH YORKSHIRE INTEGRATED CARE BOARD",
      "NHS NORTH EAST LONDON INTEGRATED CARE BOARD"
    ),
    comms = NULL,
    spec = specs
  )

  expect_equal(
    length(lbls),
    5,
    info = "function returns 5 items"
  )

  expect_equal(
    lapply(lbls, names) |>
      unlist() |>
      unique(),
    c("selected_name", "selected_code", "display"),
    info = "all names of subgroups are expected"
  )

  expect_true(
    all(
      lbls$commissioner_parents$display == "Aggregated",
      lbls$commissioners$display == "Aggregated",
      lbls$specialties$display == "Aggregated"
    )
  )

  expect_equal(
    lbls$specialties$selected_name,
    specs,
    info = "selected specialties remain unchanged"
  )

  sw_trusts <- filters_displays(
    nhs_only = "nhs_only",
    nhs_regions = "South West",
    trusts = NULL,
    trust_parents = NULL,
    comm_parents = NULL,
    comms = NULL,
    spec = "Total"
  ) |>
    purrr::pluck("trusts", "selected_code")

  expect_equal(
    sw_trusts,
    c(
      "RA9",
      "RH8",
      "RK9",
      "RD1",
      "RN3",
      "RNZ",
      "RTE",
      "RH5",
      "REF",
      "RJ8",
      "RA7",
      "RVJ",
      "R0D",
      "RBD"
    ),
    info = "Trusts in SW are identified when region and NHS only are provided"
  )

  commissioners_and_parents <- filters_displays(
    nhs_only = "nhs_only",
    nhs_regions = NULL,
    trusts = NULL,
    trust_parents = NULL,
    comm_parents = "NHS LANCASHIRE AND SOUTH CUMBRIA INTEGRATED CARE BOARD",
    comms = "NHS BLACKBURN WITH DARWEN (SUB ICB LOCATION)",
    spec = "Total"
  )

  expect_equal(
    c(
      commissioners_and_parents$commissioners$selected_code,
      commissioners_and_parents$commissioner_parents$selected_code
    ),
    c(
      "00Q",
      "QE1"
    ),
    info = "Commissioners and commissioner parents are identified"
  )
})

test_that("extract_first_number works", {
  expect_equal(
    extract_first_number("0-1 months"),
    0,
    info = "extract_first_number works"
  )
})

test_that("convert_month_to_factor works", {
  expect_equal(
    convert_month_to_factor(0:12),
    factor(
      paste(
        c(
          paste0(0:11, "-", 1:12),
          "12+"
        ),
        "months"
      ),
      levels = paste(
        c(
          paste0(0:11, "-", 1:12),
          "12+"
        ),
        "months"
      )
    ),
    info = "convert_month_to_factor works"
  )
})

test_that("extract_percent works", {
  expect_equal(
    extract_percent("This sentence finishes with 50%"),
    50,
    info = "extract_percent works"
  )

  expect_equal(
    extract_percent("This sentence finishes with 50"),
    NA,
    info = "extract_percent works"
  )
})

test_that("value_box_text works", {
  golem::expect_shinytag(
    value_box_text(
      x_val = as.Date("2023-01-01"),
      y_title = "This is a title",
      y_val = 0.15,
      y_val_type = "percent",
      facet = NA
    )
  )

  golem::expect_shinytag(
    value_box_text(
      x_val = as.Date("2023-01-01"),
      y_title = "This is a title",
      y_val = 1050,
      y_val_type = "number",
      facet = 6
    )
  )
})


test_that("latest_performance_text works", {
  expect_equal(
    latest_performance_text(
      data = sample_data
    ),
    "Dec 2024: 49.4%"
  )
})

test_that("latest_performance_text works", {
  expect_equal(
    nov24_performance_text(
      data = sample_data,
      data_source = "upload"
    ),
    "The performance at Nov 2024 was 50.8%"
  )
})

test_that("performance_text_planner works", {
  expect_equal(
    performance_text_planner(
      data = sample_data,
      data_source = "upload"
    ),
    HTML(
      "<b>The performance at Nov 2024 was 50.8%</b><br><small><p>Dec 2024: 49.4%</p></small>"
    )
  )
})


test_that("Valid interpolation returns expected color", {
  lowval <- setNames(0, "#0000FF") # Blue
  midval <- setNames(50, "#00FF00") # Green
  highval <- setNames(100, "#FF0000") # Red

  # Midpoint should return green
  expect_equal(cell_colour(50, lowval, midval, highval), "#00FF00")

  # Low end should return blue
  expect_equal(cell_colour(0, lowval, midval, highval), "#0000FF")

  # High end should return red
  expect_equal(cell_colour(100, lowval, midval, highval), "#FF0000")

  # Value between low and mid
  expect_equal(
    cell_colour(25, lowval, midval, highval),
    rgb(0, 128, 128, maxColorValue = 255)
  )

  # Value between mid and high
  expect_equal(
    cell_colour(75, lowval, midval, highval),
    rgb(128, 128, 0, maxColorValue = 255)
  )
})

test_that("Error if any input is not length 1", {
  expect_error(
    cell_colour(
      25,
      c("a" = 0, "b" = 1),
      midval = setNames(50, "#00FF00"),
      highval = setNames(100, "#FF0000")
    ),
    "All inputs must be length 1"
  )
})

test_that("Error if any input is unnamed", {
  expect_error(
    cell_colour(
      25,
      lowval = c(0),
      midval = setNames(50, "#00FF00"),
      highval = setNames(100, "#FF0000")
    ),
    "All inputs must have a name"
  )
})

test_that("Error if any name is not a valid hex color", {
  expect_error(
    cell_colour(
      25,
      lowval = setNames(0, "blue"),
      midval = setNames(50, "#00FF00"),
      highval = setNames(100, "#FF0000")
    ),
    "All names must be hex value"
  )
})

test_that("Interpolation clamps values correctly", {
  lowval <- setNames(0, "#000000")
  midval <- setNames(50, "#FFFFFF")
  highval <- setNames(100, "#000000")

  # Should not exceed RGB bounds
  expect_true(grepl(
    "^#[A-Fa-f0-9]{6}$",
    cell_colour(25, lowval, midval, highval)
  ))
})


test_that("Returns a span tag with correct class and title", {
  result <- name_with_tooltip(
    "Column Name",
    "This is a definition that should wrap nicely at around thirty characters."
  )

  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "span")
  expect_equal(result$attribs$class, "table-headers")
  expect_equal(result$children[[1]], "Column Name")
})

test_that("Tooltip text is wrapped at approximately 30 characters", {
  definition <- "This is a long definition that should be wrapped at the nearest whitespace to every 30 characters."
  result <- name_with_tooltip("Test", definition)
  wrapped <- result$attribs$title

  # Check that wrapped text contains line breaks
  expect_true(grepl("\n", wrapped))

  # Check that no line exceeds 30 characters
  lines <- unlist(strsplit(wrapped, "\n"))
  expect_true(all(nchar(lines) <= 30))
})

test_that("Short definitions are not wrapped", {
  definition <- "Short definition."
  result <- name_with_tooltip("Test", definition)
  wrapped <- result$attribs$title

  expect_equal(wrapped, definition)
})

test_that("Single long word is not broken arbitrarily", {
  definition <- "Supercalifragilisticexpialidocious"
  result <- name_with_tooltip("Test", definition)
  wrapped <- result$attribs$title

  # Should not insert line breaks in the middle of a word
  expect_false(grepl("\n", wrapped))
  expect_equal(wrapped, definition)
})


test_that("Basic conversions are correct", {
  expect_equal(
    round(convert_weeks_to_months(4), 2),
    round(4 * 7 / (365.25 / 12), 2)
  )
  expect_equal(
    round(convert_weeks_to_months(52), 2),
    round(52 * 7 / (365.25 / 12), 2)
  ) # ~12 months
})

test_that("Zero weeks returns zero months", {
  expect_equal(convert_weeks_to_months(0), 0)
})

test_that("Negative input returns negative months", {
  expect_equal(
    round(convert_weeks_to_months(-4), 2),
    round(-4 * 7 / (365.25 / 12), 2)
  )
})

test_that("Fractional weeks are handled correctly", {
  expect_equal(
    round(convert_weeks_to_months(2.5), 2),
    round(2.5 * 7 / (365.25 / 12), 2)
  )
})

test_that("Vectorized input returns correct vector of months", {
  input <- c(1, 2, 3)
  expected <- input * 7 / (365.25 / 12)
  expect_equal(round(convert_weeks_to_months(input), 2), round(expected, 2))
})


test_that("calculates correct percentile for whole month", {
  # Sample data frame for testing
  sample_df <- data.frame(
    months_waited_id = 0:5,
    wlsize = c(10, 20, 30, 40, 50, 60)
  )
  result <- calc_percentile_at_week(sample_df, week = 8)
  expect_equal(result, 0.127603402757407)
})

test_that("returns NA when total wlsize is zero", {
  zero_df <- data.frame(
    months_waited_id = 0:5,
    wlsize = rep(0, 6)
  )
  result <- calc_percentile_at_week(zero_df, week = 10)
  expect_true(is.na(result))
})

test_that("works with custom column names", {
  # Sample data frame for testing
  sample_df <- data.frame(
    time = 0:5,
    size = c(10, 20, 30, 40, 50, 60)
  )
  result <- calc_percentile_at_week(
    sample_df,
    week = 10,
    wlsize_col = "size",
    time_col = "time"
  )
  expect_type(result, "double")
})

test_that("returns 0 when week is 0", {
  # Sample data frame for testing
  sample_df <- data.frame(
    months_waited_id = 0:5,
    wlsize = c(10, 20, 30, 40, 50, 60)
  )
  result <- calc_percentile_at_week(sample_df, week = 0)
  expect_equal(result, 0)
})


test_that("calc_shortfall returns correct shortfall calculation", {
  test_data <- tibble(
    period = rep("2025-09", 4),
    months_waited_id = c(1, 2, 3, 4),
    value = c(10, 20, 30, 40),
    group = c("A", "A", "A", "A")
  ) |>
    group_by(group)

  result <- calc_shortfall(test_data, target_bin = 3, target_performance = 0.8)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("period", "group", "shortfall"))
  expect_equal(nrow(result), 1)

  # Manual calculation:
  # wl_total = 100
  # wl_above_target = 30 + 40 = 70
  # shortfall = (70 - ((1 - 0.8) * 100)) / 0.8 = (70 - 20) / 0.8 = 62.5
  expect_equal(result$shortfall, 62.5)
})

test_that("calc_shortfall throws error for invalid target_performance", {
  test_data <- tibble(
    period = "2025-09",
    months_waited_id = 1,
    value = 10
  )

  expect_error(
    calc_shortfall(test_data, target_bin = 1, target_performance = -0.1),
    "target_performance must be between 0 and 1"
  )

  expect_error(
    calc_shortfall(test_data, target_bin = 1, target_performance = 1.5),
    "target_performance must be between 0 and 1"
  )
})

test_that("calc_shortfall throws error for invalid target_bin", {
  test_data <- tibble(
    period = "2025-09",
    months_waited_id = c(1, 2),
    value = c(10, 20)
  )

  expect_error(
    calc_shortfall(test_data, target_bin = 5, target_performance = 0.5),
    "target_bin not a valid month waited in the incompletes_data"
  )
})

test_that("calc_shortfall respects grouping", {
  test_data <- tibble(
    period = rep("2025-09", 6),
    months_waited_id = c(1, 2, 3, 1, 2, 3),
    value = c(10, 20, 30, 5, 15, 25),
    group = c("A", "A", "A", "B", "B", "B")
  ) |>
    group_by(group)

  result <- calc_shortfall(test_data, target_bin = 2, target_performance = 0.5)

  expect_equal(nrow(result), 2)
  expect_true(all(result$group %in% c("A", "B")))
})


test_that("tooltip_label returns correct HTML with custom lookup", {
  result <- tooltip_label("Fruit", label_lkp = "Waiting list")
  expect_equal(
    as.character(result),
    "<span title=\"The number of people that have been referred to treatment ('clock start'), but are yet to begin consultant-led treatment ('clock stop').\" style=\"border-bottom: 1px dotted #999; cursor: help;\">Fruit</span>"
  )

  result <- tooltip_label("Waiting list")
  expect_equal(
    as.character(result),
    "<span title=\"The number of people that have been referred to treatment ('clock start'), but are yet to begin consultant-led treatment ('clock stop').\" style=\"border-bottom: 1px dotted #999; cursor: help;\">Waiting list</span>"
  )
})

test_that("tooltip_label handles missing definition", {
  expect_equal(
    tooltip_label("unknown"),
    "unknown"
  )
})


test_that("Dates before or in March return next year's March", {
  expect_equal(get_next_march(as.Date("2025-01-15")), as.Date("2026-03-01"))
  expect_equal(get_next_march(as.Date("2025-03-01")), as.Date("2026-03-01"))
})

test_that("Dates after March but within 10 months return March two years ahead", {
  expect_equal(get_next_march(as.Date("2025-07-01")), as.Date("2027-03-01"))
  expect_equal(get_next_march(as.Date("2025-10-09")), as.Date("2027-03-01"))
})

test_that("Dates after March but exactly 10 months away return next year's March", {
  expect_equal(get_next_march(as.Date("2025-05-01")), as.Date("2026-03-01"))
})

test_that("Function works with default date", {
  result <- get_next_march()
  expect_true(inherits(result, "Date"))
  expect_match(format(result, "%m-%d"), "03-01")
})


test_that("returns NULL when no matching treatment functions", {
  clock_stops <- list("Derm" = 10)
  result <- convert_clock_stops_to_activity(clock_stops)
  # Should still return list with missing treatment filled
  expect_type(result, "list")
  expect_true("Derm" %in% names(result))
  expect_true(all(is.na(unlist(result$Derm)[-1])))
})

test_that("produces activity for valid treatment functions", {
  clock_stops <- list("Cardiology" = 10)
  result <- convert_clock_stops_to_activity(clock_stops)
  expect_type(result, "list")
  expect_true("Cardiology" %in% names(result))
  cardiology_activity <- result$Cardiology
  expect_s3_class(cardiology_activity, "data.frame")
  expect_true(all(
    c(
      "avg_op_first_activity_per_pathway_op_only",
      "avg_op_flup_activity_per_pathway_op_only",
      "avg_op_first_activity_per_pathway_mixed",
      "avg_op_flup_activity_per_pathway_mixed",
      "ip_daycase_count",
      "ip_non_daycase_count"
    ) %in%
      names(cardiology_activity)
  ))
  expect_true(all(!is.na(unlist(result$Cardiology))))
})

test_that("handles multiple treatment functions", {
  clock_stops <- list("Cardiology" = 10, "Oncology" = 20)
  result <- convert_clock_stops_to_activity(clock_stops)
  expect_equal(length(result), 2)
  expect_true(all(c("Cardiology", "Oncology") %in% names(result)))
})

test_that("missing treatments are filled with NA activity", {
  clock_stops <- list("Cardiology" = 10, "UnknownTF" = 5)
  result <- convert_clock_stops_to_activity(clock_stops)
  expect_true("UnknownTF" %in% names(result))
  expect_true(all(is.na(unlist(result$UnknownTF)[-1])))
})
