# Raw data often only contains values where they exist. This function expands the raw data so there are 0 values for periods that no counts existed. It also makes sure period and period_id are consistent between each specialty/trust combination

Raw data often only contains values where they exist. This function
expands the raw data so there are 0 values for periods that no counts
existed. It also makes sure period and period_id are consistent between
each specialty/trust combination

## Usage

``` r
clean_raw_data(raw_data, max_months_waited = 12)
```

## Arguments

- raw_data:

  table of referrals, competes and incompletes (as different types);
  data needs the following field names: trust, specialty, period_id,
  type, months_waited_id, value

- max_months_waited:

  integer; the stock to pool the stocks that have waited longer into
