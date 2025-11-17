# Package index

## All functions

- [`RTTshiny`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/RTTshiny.md)
  : NHSRtt: A golem shiny app for modelling waiting times using a stock
  and flow method
- [`calc_percentile_at_week()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/calc_percentile_at_week.md)
  : Calculate the percentile waiting at a given week
- [`calc_performance()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/calc_performance.md)
  : Calculates performance by period from a given data set and target
  stock
- [`calc_shortfall()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/calc_shortfall.md)
  : Calculates performance by period from a given data set and target
  stock
- [`check_imported_data()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/check_imported_data.md)
  : check the data imported into the app
- [`clean_raw_data()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/clean_raw_data.md)
  : Raw data often only contains values where they exist. This function
  expands the raw data so there are 0 values for periods that no counts
  existed. It also makes sure period and period_id are consistent
  between each specialty/trust combination
- [`click_info()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/click_info.md)
  : function to return the data behind where the user has clicked
- [`create_modelling_data()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/create_modelling_data.md)
  : Create the calibration dataset from the main data table
- [`extend_period_type_data()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/extend_period_type_data.md)
  : geom_step in the charts do not display the final observed or
  projected months well because the stepped line terminates at the start
  of the month. This function adds an artificial month onto the observed
  and projected period_types so they are displayed better on the
  visualisations
- [`plot_output()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/plot_output.md)
  : Visualisation functions for module 3
- [`plot_waiting_lists_chart()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/plot_waiting_lists_chart.md)
  : Plot Waiting List Distributions with Target Indicators
- [`run_app()`](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/reference/run_app.md)
  : Run the Shiny Application
