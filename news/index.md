# Changelog

## RTTshiny 2.0.0.9000

### Steady state

- Changes table header from “Additional monthly removals required” to
  “Required monthly change in waiting list size”

## RTTshiny 2.0.0

### Throughout tool

- Fixes bug for data import causing low counts for completes pathways.
- Definitions of words are displayed on hover.
- The list of collaborators has increased on the acknowledgements tab.
- New tutorial videos are included to explain new features.

### Scenario planner

- Allows for filtering providers by NHS and non-NHS providers.
- Includes information about model accuracy once data has been
  imported/downloaded.
- When data are uploaded, the user is now required to enter a data
  description, which is used as the chart title.
- Nov 2024 performance is displayed when estimating capacity to meet
  performance.
- How to allocate surplus treatment capacity is now a user option in the
  advanced skew section.
- Provides warning when “negative reneging” occurring in the calibration
  period.

### Results

- Following “calculating performance from capacity inputs”, the user can
  now edit the input data for treatment capacity and referrals for the
  projection period to understand the impact of short-term measures on
  performance.
- The distribution charts allow for “free” scales as well as being able
  to flip the view to see distributions by different time periods.
- A new “Performance shortfall” chart is displayed when projecting
  performance from capacity inputs. This displays the number of
  additional ‘long-waiter’ clock stops that would be required to achieve
  a particular performance target.

### Steady state

- This applies a new methodology to allow users to model multiple trusts
  and specialties together. The modelling provides a view of what a
  “healthy” waiting list looks like when it is in steady state (e.g.,
  clock starts equals clock stops) and achieving a specific performance
  and renege rate target.

## RTTshiny 1.0.2

- Fixes minor bug when some data are downloaded, which occurred when
  referrals exceeded treatments in the first compartment in the
  calibration period.
- Fixes bug where optimisation can’t occur because it is attempting to
  use negative treatment capacities.

## RTTshiny 1.0.0

- First release of the tool.

### Enhancements

- includes status of optimisation process.
- option to put multiple performance targets.
- report download feature included.
- makes clickable charts, returning information about where the user has
  clicked.
- includes history of the project and acknowledgements.
- landing page includes tool information, how to use the tool, and key
  future developments.

### Bug fixes

- makes the geography selection simpler and less clunky

## RTTshiny 0.0.2

- Minimum viable product created and shared for testing.
- Optimisation capability.
- Future scenario capability.
- progress bars for downloading and optimising.
- adds dropdowns to allow for more refined area selections.
- advanced options for skew parameter.

## RTTshiny 0.0.1

- Minimum interface created.
