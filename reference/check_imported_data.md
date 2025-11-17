# check the data imported into the app

check the data imported into the app

## Usage

``` r
check_imported_data(imported_data)
```

## Arguments

- imported_data:

  a tibble with columns of period, type, value and months_waited_id

## Value

list with two items; a message describing the outputs of the check, and
the resulting data tibble (which will be NULL if the checks have failed)
