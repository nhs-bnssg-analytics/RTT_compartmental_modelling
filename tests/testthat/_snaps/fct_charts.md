# holding_chart is consistent

    Code
      holding_chart(type = "other")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "model", "select_chart"

# plot_skew is consistent

    Code
      plot_skew(params = dummy_params, skew_values = 1.5, pivot_bin = 2, skew_method = "bad input")
    Condition
      Error in `map2()`:
      i In index: 1.
      i With name: Skewed.
      Caused by error in `match.arg()`:
      ! 'arg' should be one of "rotate", "uniform"

# tooltip testing

    Code
      linear_uniform_tooltip(uniform_id = "dummy_uniform", linear_id = "dummy_linear")
    Output
      <div>
        <strong>Uniform:</strong> Treatment capacity/referral change occurs in first month and remains flat for the whole 'Forecast horizon' period.<br><br>
        <div class="shiny-plot-output html-fill-item" id="dummy_uniform" style="width:100%;height:75px;"></div>
        <strong>Linear:</strong> The first month of the 'Forecast horizon' period is estimated from the historic data, and then treatment capacity/referral is changed linearly until the end of the 'Forecast horizon'.
        <div class="shiny-plot-output html-fill-item" id="dummy_linear" style="width:100%;height:75px;"></div>
      </div>

---

    Code
      skew_tooltip()
    Output
      A skew of 1 causes the profile of treatment capacity rates across the number of months waiting to be unchanged from the calibration period.<br><br>A skew of greater than 1 will increase the treatment capacity rate for the longer waiters, and decrease the treatment capacity rate for the shorter waiters.<br><br>A skew of less than 1 will decrease the treatment capacity rate for the longer waiters, and increase the treatment capacity rate for the shorter waiters.<br><br>All skew values leave the treatment capacity rates for individuals waiting less than 1 month unchanged from the treatment capacity rate calculated from the calibration period.

