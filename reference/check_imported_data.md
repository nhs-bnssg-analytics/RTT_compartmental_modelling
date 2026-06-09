# check the data imported into the app

check the data imported into the app

## Usage

``` r
check_imported_data(imported_data, steady_state = F)
```

## Arguments

- imported_data:

  a tibble with columns of period, type, value and months_waited_id

- steady_state:

  boolean T F as to whether include check for if it is an input for the
  Steady state calculation. Default FALSE.

## Value

list with two items; a message describing the outputs of the check, and
the resulting data tibble (which will be NULL if the checks have failed)
