# Visualisation functions for module 3

Generate a time series plot of observed and projected performance.

This function creates a ggplot2 time series plot showing observed and
projected performance data, with options to customise the plot based on
different scenario parameters. It handles various scenarios, including
estimating performance from capacity inputs and optimising capacity to
achieve a target performance.

## Usage

``` r
plot_output(
  data,
  p_trust,
  p_speciality,
  p_chart,
  p_scenario,
  p_cap_change = 0,
  p_cap_change_type,
  p_cap_skew,
  p_target_data,
  p_referrals_percent_change,
  p_referrals_change_type,
  p_perc,
  p_facet = F,
  p_target_line = F,
  date_input = Sys.Date(),
  p_facet_scales = "fixed",
  p_facet_grouping = "months_waited_id"
)
```

## Arguments

- data:

  A data frame containing the data to be plotted. It should include
  columns 'period', 'period_type' (either "Observed" or "Projected"),
  and 'p_var' (the variable to be plotted).

- p_trust:

  A character string specifying the trust name.

- p_speciality:

  A character string specifying the specialty.

- p_chart:

  A character string describing the chart type or variable being
  plotted.

- p_scenario:

  A character string specifying the scenario type, either "Estimate
  performance (from capacity inputs)" or another scenario (e.g.,
  "Optimise Capacity").

- p_cap_change:

  A numeric value representing the percentage change in capacity. This
  can also take a string to accommodate a customised input for capacity
  change.

- p_cap_change_type:

  A character string describing the type of capacity change (e.g.,
  "linear", "uniform").

- p_cap_skew:

  A numeric value representing the utilisation skew factor.

- p_target_data:

  A table with two columns: Target_date and Target_percentage,
  containing entries for multiple dates and performance targets.

- p_referrals_percent_change:

  A numeric value representing the percentage change in referrals. This
  can also take a string to accommodate a customised input for referrals
  change.

- p_referrals_change_type:

  A character string describing the type of referrals change (e.g.,
  "linear", "uniform").

- p_perc:

  A logical value indicating whether the y-axis should be formatted as
  percentages.

- p_facet:

  A logical value indicating whether to facet the plot by
  'months_waited_id'.

- p_target_line:

  A logical value indicating whether to include target line and change
  colour of "target" in subheading

- date_input:

  date for chart caption

- p_facet_scales:

  either "fixed" or "free_y"

- p_facet_grouping:

  either "period" or "months_waited_id"

## Value

A ggplot2 plot object of selected values
