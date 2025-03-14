
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RTTshiny

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of RTTshiny is to provide an interface to a stock-and-flow
model on NHS Referral to Treatment data, to allow planners to understand
how changes in future clock stops can effect the waiting list size and
knock on performance.

## Run without installing

If you have R installed on your machine, the following command may work
to run the application without installing the package:

``` r
# install.packages("shiny")
shiny::runGitHub("RTT_compartmental_modelling", "nhsengland")
```

## Installation

You can install the development version of fptool from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github(
  "nhsengland/RTT_compartmental_modelling",
  dependencies = "Suggests"
)
```

## Usage

To launch the Shiny app following installation:

``` r
RTTshiny::run_app()
```

**THE FOLLOWING WILL BE UPDATED IN THE NEAR FUTURE**

# Model Card: {PROJECT NAME}

## Model Details

The implementation of the {PROJECT NAME} within this repository was
created as part of an NHS England PhD internship project undertaken by
{PROJECT AUTHOR} {LINK TO LAST COMMIT WITH ABBREVIATED SHA}. This model
card describes the updated version of the model, released {DATE OF
RELEASE}.

## Model Use

### Intended Use

This model is intended for use in {BRIEF DESCRIPTION OF USE}

### Out-of-Scope Use Cases

This model is not suitable to provide privacy guarantees in a production
environment.

{DETAIL KNOWN CASES WHERE PRIVACY IS NOT GUARENTEED}

## Training Data

Experiments in this repository are run against:

{LIST AND LINK DATA SOURCES}

## Performance and Limitations

{DETAIL PERFORMANCE EXPECTATIONS and LIMITATIONS}

## Additional notes

{DETAIL SUPPLEMENTARY INFORMATION}
