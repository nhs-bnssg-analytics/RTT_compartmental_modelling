# holding_chart is consistent

    Code
      holding_chart(type = "other")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "model", "select_chart"

# plot_skew error

    Code
      plot_skew(params = dummy_params, skew_values = 1.5, pivot_bin = 2, skew_method = "bad input")
    Condition
      Error in `map2()`:
      i In index: 1.
      i With name: Skewed.
      Caused by error in `match.arg()`:
      ! 'arg' should be one of "rotate", "uniform"

# skew_tooltip testing

    Code
      skew_tooltip()
    Output
      A skew of 1 causes the profile of treatment capacity rates across the number of months waiting to be unchanged from the calibration period.<br><br>A skew of greater than 1 will increase the treatment capacity rate for the longer waiters, and decrease the treatment capacity rate for the shorter waiters.<br><br>A skew of less than 1 will decrease the treatment capacity rate for the longer waiters, and increase the treatment capacity rate for the shorter waiters.<br><br>All skew values leave the treatment capacity rates for individuals waiting less than 1 month unchanged from the treatment capacity rate calculated from the calibration period.

