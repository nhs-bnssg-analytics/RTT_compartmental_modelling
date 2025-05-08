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


test_that("filters_displays works", {

  specs <- c("General Surgery", "Total")

  lbls <- filters_displays(
    trust_parents = "NHS LANCASHIRE AND SOUTH CUMBRIA INTEGRATED CARE BOARD",
    trusts = "FULWOOD HALL HOSPITAL",
    comm_parents = c("NHS SOUTH YORKSHIRE INTEGRATED CARE BOARD",
                     "NHS NORTH EAST LONDON INTEGRATED CARE BOARD"),
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
    nhs_only = TRUE,
    nhs_regions = "South West",
    trusts = NULL,
    trust_parents = NULL,
    comm_parents = NULL,
    comms = NULL,
    spec = specs
  ) |>
    purrr::pluck("trusts", "selected_code")

  expect_equal(
    sw_trusts,
    c("RA9", "RH8", "RK9", "RD1", "RN3", "RNZ", "RTE", "RH5", "REF", "RJ8", "RA7", "RVJ", "R0D", "RBD"),
    info = "Trusts in SW are identified when region and NHS only are provieded"
  )
})
