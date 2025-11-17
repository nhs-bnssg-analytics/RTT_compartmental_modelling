# Plot Waiting List Distributions with Target Indicators

Generates a faceted bar plot showing the distribution of waiting list
sizes across different categories, with vertical lines and labels
indicating a target number of weeks and a percentile threshold.

## Usage

``` r
plot_waiting_lists_chart(data, target_week, target_value)
```

## Arguments

- data:

  A data frame containing waiting list information. Must include
  columns: - \`months_waited_id\`: numeric identifier for months
  waited - \`wlsize\`: number of people on the waiting list - \`sigma\`:
  the number of treatments for each compartment - \`wl_description\`:
  description used for faceting

- target_week:

  Numeric value indicating the target number of weeks to wait. This will
  be converted to months and shown as a vertical dashed line with a
  label.

- target_value:

  Numeric value representing the percentile (e.g., 90 for 90th
  percentile). Used for labeling the percentile line.

## Value

A \`ggplot2\` object representing the waiting list distribution plot.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_waiting_lists_chart(data = my_data,
                   target_week = 18,
                   target_value = 90)
} # }
```
