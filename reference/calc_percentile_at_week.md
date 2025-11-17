# Calculate the percentile waiting at a given week

Calculate the percentile waiting at a given week

## Usage

``` r
calc_percentile_at_week(
  wl_shape,
  week,
  wlsize_col = "wlsize",
  time_col = "months_waited_id"
)
```

## Arguments

- wl_shape:

  a tibble with columns for the number of months waited (where 0 is 0-1
  month) and volume waiting

- week:

  numeric; the week to calculate the percentile for

- wlsize_col:

  character length 1; the name of the column containing the volume
  waiting

- time_col:

  character length 1; the name of the column containing the number of
  months waiting
