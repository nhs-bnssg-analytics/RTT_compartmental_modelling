# Create the calibration dataset from the main data table

Create the calibration dataset from the main data table

## Usage

``` r
create_modelling_data(data, max_months_waited = 12, referrals_uplift)
```

## Arguments

- data:

  table of referrals, competes and incompletes (as different types);
  data needs the following field names: trust, specialty, period_id,
  type, months_waited_id, value

- max_months_waited:

  integer; the stock to pool the stocks that have waited longer into

- referrals_uplift:

  numeric; single value - parameter to apply to referral inputs
  (absolute value of the renege_params in the first stock when
  calibrating the models). These occur due to under-reporting of
  referrals data. This is applied to the observed referrals using the
  following formula:

  \$\$referrals\_{adjusted} = referrals\_{obs} + (referrals\_{obs} \*
  uplift\\parameter)\$\$

  See details for more information on this argument and how it is
  applied.

## Details

This is the maths for stock = 0. We know:

(Equation 1) \$\$incomplete\_{obs} = referrals\_{obs} -
complete\_{obs} - reneges\_{calc}\$\$

BUT, when \\reneges\_{calc}\\ are negative, we want to adjust referrals
by that amount

\$\$incompletes\_{obs} = referrals\\adj\_{calc} - complete\_{obs}\$\$

WHERE

(Equation 2) \$\$referrals\\adj\_{calc} = referrals\_{obs} +
reneges\_{calc}\$\$ (where \\reneges\_{calc}\\ is from equation 1)

ALSO

(Equation 3)

\$\$renege\\param\_{calc} = \frac{reneges\_{calc}}{referrals\_{obs}}\$\$

THEREFORE, combining eq. 2 and eq. 3 (substituting \\reneges\_{calc}\\)

(Equation 4) \$\$renege\\param\_{calc} = \frac{referrals\\adj\_{calc} -
referrals\_{obs}}{referrals\_{obs}}\$\$

REARRANGING eq. 4

\$\$referrals\\adj\_{calc} = (renege\\param\_{calc} \*
referrals\_{obs}) + referrals\_{obs}\$\$
