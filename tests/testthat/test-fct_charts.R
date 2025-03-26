test_that("performance_text works", {
  df <- dplyr::tibble(
    Target_date = as.Date("2022-03-01") %m+% months(c(0, 12, 24)),
    Target_percentage = c(60, 65, 70)
  )


  performance_text(df)

})


