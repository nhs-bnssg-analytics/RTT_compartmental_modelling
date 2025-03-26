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


