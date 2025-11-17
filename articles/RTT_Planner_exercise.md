# Exercise

This exercise helps you understand how the RTT Planner tool works for
forecasting an elective waiting list. The RTT Planner tool is located at
this URL:
<https://connect.strategyunitwm.nhs.uk/rtt_compartmental_modelling/>

This exercise sheet helps you understand the methods that underpin the
tool.

## A. Data

1.  Load the tool and go to the **Scenario Planner** tab.
2.  On the left, **select the provider** “University Hospitals
    Birmingham NHS Foundation Trust”, and the **specialty** “Cardiology”
    (feel free to do your own combination of interest).
3.  Press **Download RTT data** to download 12 months of data.

> ### What’s going on - the calibration process?
>
> - PUBLIC RTT DATA ARE OBTAINED AND REFORMATTED INTO REFERRALS,
>   TREATMENTS AND INCOMPLETE PATHWAYS FOR EACH MONTH FOR THE TOTAL
>   PERIOD SELECTED. TREATMENTS AND INCOMPLETE PATHWAYS ARE AGGREGATED
>   TO COUNTS BY THE NUMBERS OF MONTHS WAITED (**“COMPARTMENTS”**) FOR
>   EACH PERIOD.
> - THE DATA ARE THEN FORMATTED SO FOR EACH COMPARTMENT AND PERIOD THERE
>   IS AN **“INFLOW”** (EITHER THE COUNT OF REFERRALS OR THE INCOMPLETE
>   PATHWAYS FROM THE PREVIOUS COMPARTMENT IN THE PREVIOUS PERIOD), A
>   TREATMENT VALUE AND THE COUNT OF INCOMPLETE PATHWAYS.
> - **“RENEGES”** ARE CALCULATED AS THE INFLOW – TREATMENTS – INCOMPLETE
>   PATHWAYS FOR EACH COMPARTMENT FOR EACH PERIOD.
> - **PARAMETERS FOR TREATMENT AND RENEGES FOR EACH COMPARTMENT** ARE
>   CALCULATED AS THE MEAN VALUE FOR THE WHOLE TIME SERIES FOR EACH OF
>   THE TWO METRICS **AS A PROPORTION OF THE INFLOW**.

4.  **Observe** the model uncertainty.

> ### What’s going on – projecting forward?
>
> - THE DOWNLOADED CALIBRATED DATA ARE DIVIDED INTO TWO EQUAL PERIODS,
>   AND THE CALIBRATION PROCESS DESCRIBED ABOVE IS PERFORMED ON THE
>   FIRST HALF OF THE DATA.
> - THE MODEL PARAMETERS ARE COMBINED WITH THE **OBSERVED** REFERRALS
>   AND TOTAL CAPACITY, ALONG WITH THE INCOMPLETE PATHWAY COUNTS FROM
>   THE FINAL PERIOD FROM THE FIRST HALF OF THE DATA IN THE FOLLOWING
>   WAY:
> - THE INFLOWS FOR THE FIRST PERIOD IN THE ‘PROJECTED PERIOD’ ARE THE
>   REFERRALS AND INCOMPLETE PATHWAYS. THESE INFLOW VALUES, BY
>   COMPARTMENT, ARE MULTIPLIED BY THE TREATMENT AND RENEGE PARAMETERS
>   TO CALCULATE COUNTS OF TREATMENTS AND RENEGES BY COMPARTMENT. THE
>   TREATMENTS ARE THEN SCALED SO THE OVERALL TREATMENTS MATCH THE
>   OBSERVED TREATMENTS.
> - THE WAITING LIST AT THE END OF THE PERIOD IS THEN CALCULATED AND
>   USED AS THE INFLOW TO THE NEXT PERIOD, WHERE THE PROCESS IS
>   REPEATED.
> - THE MEAN ABSOLUTE ERROR (MAE) AND MEAN ABSOLUTE PERCENTAGE ERROR
>   (MAPE) ARE THEN CALCULATED ON THE OVERALL WAITING LIST SIZE, THE
>   18-WEEK PERFORMANCE, AND THE WAITING LIST SIZE BY COMPARTMENT.

## B. Projecting a “do nothing” scenario The right-hand side of the page allows the user to configure a future theoretical scenario.

1.  Keep the forecast end date as March 2027.
2.  **Increase** the annual percentage change in referrals to 1%,
    keeping the type of referral change as “linear”.
3.  **Select** the scenario type **“Estimate performance from treatment
    capacity inputs”**.
4.  Keep a 0% change for treatment capacity, with a linear change type,
    and a treatment capacity utilisation skew of 1 (these settings are
    equivalent to a “do nothing” scenario).
5.  Click **“Calculate future performance”**.
6.  Click the **“Results”** tab at the top of the page. The list on the
    left-hand side are the charts that the tool provides.
7.  **Check** that the “inputs” make sense (referrals and total
    treatment capacity charts). Note, you can click on the charts to
    obtain specific values.
8.  **Question:** when comparing the arrivals (referrals) with
    departures (treatment capacity and reneges) should the waiting list
    be growing or shrinking? **Look at** the total waiting list size
    chart to validate your understanding. **Make a note** of the waiting
    list size in March 2027.
9.  **Observe** the 18-week performance chart for the projected period
    and **make a note** of the performance in March 2027.

> ### What’s going on – projecting forward?
>
> HERE, THE SAME METHOD IS APPLIED AS PREVIOUSLY DESCRIBED FOR
> PROJECTING FORWARD. THE DIFFERENCE IS THAT WE ARE **ESTIMATING**
> FUTURE DEMAND AND CAPACITY (COMPARED WITH WHAT WE DID PREVIOUSLY,
> WHERE WE PROVIDED OBSERVED DEMAND AND CAPACITY DATA).
>
> ONE COMPLEXITY IS THAT THE TOOL CREATES AN ESTIMATE OF THE FIRST VALUE
> IN THE PROJECTED PERIOD FOR REFERRALS AND TREATMENT CAPACITY. THE WAY
> IT DOES THIS IS BY PERFORMING A LINEAR REGRESSION ON THE OBSERVED
> DATA, AND IF THE MODEL IS SIGNIFICANT (P-VALUE ≤ 0.01), IT WILL USE IT
> TO ESTIMATE THE FINAL VALUE IN THE OBSERVED PERIOD AND USE THAT AS THE
> FIRST VALUE IN THE PROJECTED PERIOD. IF THE MODEL IS NOT SIGNIFICANT,
> IT WILL USE THE MEAN OF THE OBSERVED PERIOD AS THE FIRST VALUE IN THE
> PROJECTED PERIOD.

## C. Changing future performance – increase capacity

1.  **Exercise:** return to scenario planner tab and run a scenario
    where treatment capacity is increased annually by 2% linearly.
2.  **Observe** the resulting March 2027 waiting list size and
    performance with the “do nothing” scenario. **Question:** does this
    make sense?

## D. Changing future performance – altering the treatment capacity utilisation skew to focus treatment on shorter waiters

1.  **Return** to the scenario planner tab.
2.  **Reduce** the capacity change back to 0% annually.
3.  **Expand** the “Advanced skew settings”.
4.  **Change the treatment capacity utilisation skew** (referred to as
    **“skew”** from now on) to 0.8.
5.  **View** the chart in the expanded section.
6.  Click **“Calculate future performance”**.
7.  **Observe** the resulting March 2027 waiting list size and
    performance with the “do nothing” scenario. **Question:** does this
    make sense?

> ### What’s going on – skew?
>
> - THE VISIBLE BLUE STEPPED LINE ARE THE TREATMENT CAPACITY PARAMETERS
>   FROM THE MODEL CALIBRATION.
> - THE COMMON SHAPE IS A BATHTUB – WHERE THERE ARE HIGHER RATES BEING
>   TREATED AT SHORTER AND LONGER WAIT TIMES, AND THERE IS A DIP IN THE
>   MIDDLE.
> - A “SKEW” OF LESS THAN 1 ADJUSTS THE TREATMENT CAPACITY PARAMETERS SO
>   THAT THE RATE OF TREATMENT FOR THOSE WAITING LESS THAN A PARTICULAR
>   MONTH (4 MONTHS BY DEFAULT) IS INCREASED, AND THE RATE OF TREATMENT
>   FOR THOSE WAITING LONGER THAN A PARTICULAR MONTH IS DECREASED. THIS
>   IS DISPLAYED BY THE YELLOW LINE COMPARED WITH THE ORIGINAL
>   PARAMETERS IN BLUE.
> - THIS HAS THE EFFECT OF FOCUSSING THE TREATMENT ON SHORTER WAITERS
>   COMPARED WITH WHAT HAS BEEN DONE IN THE CALIBRATION PERIOD.
> - **NOTE, THE FIRST COMPARTMENT (THOSE WAITING UP TO 1 MONTH) REMAIN
>   UNCHANGED** FROM THE ORIGINAL CALIBRATION AS THESE ARE CONSIDERED
>   THE **URGENT** CASES.

## E. Changing future performance – altering the treatment capacity utilisation skew to focus treatment on longer waiters

1.  **Exercise:** repeat the previous exercise with a skew value of 1.5.
2.  **Optional:** see the impact of changing the skew method from
    “Rotate” to “Uniform”.

## F. Changing future performance – a short-term trial or intervention Here we run through the process of implementing a short-term intervention, like say, a demand management scheme.

1.  **Return** to the scenario planner and **enter the inputs for a “do
    nothing” scenario**.
2.  **“Calculate future performance”**.
3.  Go to the **Results** tab.
4.  **View** one of the referrals charts, and beneath the chart **click
    “edit input data”**.
5.  **Reduce** the number of monthly referrals for the first three
    months to 1,600 (by double clicking in the cells of the table).
6.  Click **Save changes**.
7.  **Observe** the impact on the referrals chart.
8.  **Observe** the impact on the 18-week performance chart.
    **Question:** does this make sense?

> ### What’s going on – performance?
>
> WE CAN SEE THAT SHORT TERM PERFORMANCE (THE FIRST THREE MONTHS) ISN’T
> AS GOOD AS IT WAS IN THE “DO NOTHING” SCENARIO, BUT THE MARCH 2027
> PERFORMANCE IS BETTER.
>
> THE SHORT-TERM PERFORMANCE DETERIORATES BECAUSE REFERRALS ARE LOWER,
> MEANING **THERE ARE FEWER PEOPLE WAITING LESS THAN 4 MONTHS**, CAUSING
> THE PERFORMANCE METRIC (PROPORTION OF WAITING LIST WAITING LESS THAN
> 18 WEEKS) TO APPEAR TO DETERIORATE.
>
> ONCE REFERRALS RETURN TO A “NORMAL” LEVEL AND THE THREE MONTHS REDUCED
> DEMAND HAS FILTERED THROUGH THE SYSTEM, THE EFFECT IS THAT **OVERALL
> PERFORMANCE IMPROVES**.

## G. Optimising future performance – what capacity do we need to meet a target? We might want to answer a question like “how much extra capacity do I need to meet a 18-week performance of x by March 2027?”

1.  **Return** to the scenario planner.
2.  **Select** the scenario type **“Estimate treatment capacity from
    performance targets”**.
3.  **Add** 5% onto your previously calculated March 2027 “do nothing”
    scenario performance and use this value as a target performance.
4.  **Observe** the range of treatment capacity skews.
5.  Click **“Optimise treatment capacity”**.
6.  View the **results** tab.
7.  **Observe** the inputs (now the inputs are referrals and 18-week
    performance).
8.  **Observe** whether the 18-week performance target is achieved at
    the required date.
9.  **Observe** the results treatment capacity, waiting list size and
    reneges. **Question:** do these make sense?

> ### What’s going on – treatment capacity optimisation?
>
> WHEN THE OPTIMISE TREATMENT CAPACITY BUTTON IS CLICKED, A PROGRESS BAR
> APPEARS. FOR EACH INCREMENT OF THE PROGRESS BAR:
>
> - A SKEW VALUE WITHIN THE RANGE PROVIDED IS APPLIED TO THE CALIBRATED
>   MODEL PARAMETERS (INCREMENTING BY 0.05).
>
> - USING THESE ADJUSTED MODEL PARAMETERS AND A PROJECTED TREATMENT
>   CAPACITY PROFILE, THE PERFORMANCE AT THE PROVIDED DATE THAT THE
>   TARGET SHOULD BE ACHIEVED BY, IS CALCULATED. - IF THE PERFORMANCE IS
>   NOT WHAT THE USER HAS DEFINED, THE PROJECTED TREATMENT CAPACITY
>   PROFILE IS ADJUSTED, AND THE PERFORMANCE IS RECALCULATED AT THAT
>   TARGET DATE.
>
> - THIS CONTINUES UNTIL THE TARGET PERFORMANCE IS ACHIEVED AT THE
>   TARGET DATE **OR** THE MODEL DETERMINES THAT THE TARGET PERFORMANCE
>   CANNOT BE MET WITHIN THE MODELLING CONSTRAINTS.

## H. Unrealistic optimisation What happens if the required increase in capacity is too great to meet a performance target?

1.  **Return** to the scenario planner.
2.  **Amend** the target performance to 92% (or something unachievable
    in a short space of time).
3.  Click **“optimise treatment capacity”**.
4.  Go to the **results** tab.
5.  **Observe** the 18-week performance chart. **Question:** do you
    understand what is occurring in the final month?
6.  **Observe** the total waiting list size chart. **Question:** does
    this help to explain what is happening with performance?
7.  **Question:** within the tool, what options are there to be more
    efficient in using capacity to improve performance?

## I. Longer term planning – a healthy waiting list

The scenario planner described above can support medium-term planning.
It will often be the case that the results of the optimiser will be out
of balance, e.g., the arrivals (referrals) will not be equal to the
departures (treatments plus reneges). As a result, an extension of this
scenario will likely result in a growing or shrinking waiting list, and
a corresponding change of performance.

The steady state tab helpfully models a scenario where the **arrivals
are in balance with the departures** AND **a set performance target is
achieved**. This situation can be met with multiple configurations of
how treatment is allocated along with different quantities of reneging.
The final constraints this page provides is to fix the overall renege
rates (as a proportion of departures) to either a recently observed
renege rate value or a user defined value. It also aims to minimise the
difference in how treatment is proportionally applied across the waiting
list compared with how is has recently been applied across the waiting
list.

1.  **Select** the steady state tab.
2.  **Make a choice** of trust and specialty (use Cardiology at
    University Hospitals Birmingham NHS Foundation Trust again if you
    like).
3.  **Create** a single referrals scenario of an annual percentage
    increase of 1%.
4.  **Keep the default settings** of 92% of people waiting less than 18
    weeks by March 2029.
5.  Click **“calculate steady state”**.
6.  **Observe** the results table (you can hover over the column headers
    to understand their definition):

&nbsp;

1.  The “Current” section is the latest observed data.
2.  The “Do nothing” section is the equivalent of the scenario planner
    “do nothing” previously described.
3.  The “Steady state” section shows the results of the modelling
    described above.

&nbsp;

7.  **Expand** the table and **observe** how the waiting list size has
    changed over the last year, and how it compares to the steady state
    waiting list.
8.  **Do the same** for the treatments.
9.  **Make a note** of the steady state waiting list size.
10. **Return** to the inputs on the left-hand side and **select** “User
    input” at the very bottom to fix the overall renege proportions of
    the steady state solution – and retain the value of 10%.
11. Click **“calculate steady state”**.
12. **Observe** the resulting steady state waiting list size and compare
    with the size previously noted (in step 9). **Question:** does this
    make sense?
