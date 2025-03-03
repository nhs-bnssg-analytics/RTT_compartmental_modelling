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
