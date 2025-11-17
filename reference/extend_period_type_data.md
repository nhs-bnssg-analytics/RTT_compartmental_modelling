# geom_step in the charts do not display the final observed or projected months well because the stepped line terminates at the start of the month. This function adds an artificial month onto the observed and projected period_types so they are displayed better on the visualisations

geom_step in the charts do not display the final observed or projected
months well because the stepped line terminates at the start of the
month. This function adds an artificial month onto the observed and
projected period_types so they are displayed better on the
visualisations

## Usage

``` r
extend_period_type_data(plot_data)
```

## Arguments

- plot_data:

  tibble containing the columns period and period_type (which contains
  values "Observed" and "Projected")
