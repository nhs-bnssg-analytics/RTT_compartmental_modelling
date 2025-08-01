
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RTTshiny

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nhs-bnssg-analytics/RTT_compartmental_modelling/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/nhs-bnssg-analytics/RTT_compartmental_modelling/graph/badge.svg?token=O2KBKLVUOD)](https://codecov.io/gh/nhs-bnssg-analytics/RTT_compartmental_modelling)
<!-- badges: end -->

The goal of RTTshiny is to provide an interface to a stock-and-flow
model on NHS Referral to Treatment data, to allow planners to understand
how changes in future treatment capacity can effect the waiting list
size and knock on performance.

## Run without installing

If you have R installed on your machine, the following command may work
to run the application without installing the package:

``` r
# install.packages("shiny")
shiny::runGitHub("RTT_compartmental_modelling", "nhs-bnssg-analytics")
```

## Installation

You can install the development version of RTTshiny from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github(
  "nhs-bnssg-analytics/RTT_compartmental_modelling",
  dependencies = "Suggests"
)
```

## Usage

To launch the Shiny app following installation:

``` r
RTTshiny::run_app()
```

## Web pages

See more information about the model on the [pkgdown
website](https://nhs-bnssg-analytics.github.io/RTT_compartmental_modelling/).

