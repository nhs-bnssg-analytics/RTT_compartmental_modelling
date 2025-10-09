
# RTTshiny 1.0.2.9000

* On the results page, following "calculating performance from capacity inputs", the user can now edit the input data for treatment capacity and referrals for the projection period to understand the impact of temporary measures on performance.
* On the results page, the distribution charts now can allow for "free" scales as well as being able to flip the view to see distributions by different time periods.
* Allows for filtering providers by NHS and non-NHS providers.
* Includes information about model error once data has been imported/downloaded.
* A new "Steady state" tab has been introduced. This applies a new methodology to allow users to model multiple trusts and specialties together. The modelling provides a view of what a healthy waiting list looks like when it is in steady state (e.g., clock starts equals clock stops) and achieving a specific target.
* A new "Performance shortfall" chart is displayed on the results page when using the Scenario Planner to predict performance from capacity inputs. This displays the number of additional 'long-waiter' clock stops that would be required to achieve a particular performance target.
* When data are uploaded to the Scenario Planner, the user is now required to enter a data description, which is used as the chart title.
* Fixes bug for data import causing low counts for completes pathways.
* Definitions of words are displayed on hover.
* The list of collaborators has increased on the acknoledgements tab.
* Nov 2024 performance is displayed on the scenario planner when estimating capacity to meet performance.

# RTTshiny 1.0.2

* Fixes minor bug when some data are downloaded, which occurred when referrals exceeded treatments in the first compartment in the calibration period.
* Fixes bug where optimisation can't occur because it is attempting to use negative treatment capacities.

# RTTshiny 1.0.0

* First release of the tool.

## Enhancements

* includes status of optimisation process.
* option to put multiple performance targets.
* report download feature included.
* makes clickable charts, returning information about where the user has clicked.
* includes history of the project and acknowledgements.
* landing page includes tool information, how to use the tool, and key future developments.


## Bug fixes

* makes the geography selection simpler and less clunky

# RTTshiny 0.0.2

* Minimum viable product created and shared for testing.
* Optimisation capability.
* Future scenario capability.
* progress bars for downloading and optimising.
* adds dropdowns to allow for more refined area selections.
* advanced options for skew parameter.

# RTTshiny 0.0.1

* Minimum interface created.
