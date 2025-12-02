# Convert clock-stop counts to activity counts

Convert clock-stop counts to activity counts

## Usage

``` r
convert_clock_stops_to_activity(clock_stops)
```

## Arguments

- clock_stops:

  named list; items must be named with the specialty name, and values
  equal to clock-stop counts (both treatment and renege clock-stops)

## Examples

``` r
list(
  "Respiratory Medicine" = 504,
  "Oral Surgery" = 772
) |>
convert_clock_stops_to_activity()
#> $`Oral Surgery`
#> # A tibble: 1 × 7
#>   treatment_function avg_op_first_activity_per_pathway_…¹ avg_op_flup_activity…²
#>   <chr>                                             <dbl>                  <dbl>
#> 1 Oral Surgery                                       436.                   397.
#> # ℹ abbreviated names: ¹​avg_op_first_activity_per_pathway_op_only,
#> #   ²​avg_op_flup_activity_per_pathway_op_only
#> # ℹ 4 more variables: avg_op_first_activity_per_pathway_mixed <dbl>,
#> #   avg_op_flup_activity_per_pathway_mixed <dbl>, ip_daycase_count <dbl>,
#> #   ip_non_daycase_count <dbl>
#> 
#> $`Respiratory Medicine`
#> # A tibble: 1 × 7
#>   treatment_function   avg_op_first_activity_per_pathwa…¹ avg_op_flup_activity…²
#>   <chr>                                             <dbl>                  <dbl>
#> 1 Respiratory Medicine                               602.                   918.
#> # ℹ abbreviated names: ¹​avg_op_first_activity_per_pathway_op_only,
#> #   ²​avg_op_flup_activity_per_pathway_op_only
#> # ℹ 4 more variables: avg_op_first_activity_per_pathway_mixed <dbl>,
#> #   avg_op_flup_activity_per_pathway_mixed <dbl>, ip_daycase_count <dbl>,
#> #   ip_non_daycase_count <dbl>
#> 
```
