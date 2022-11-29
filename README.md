
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynamicSDM

<!-- badges: start -->

[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Codecov test
coverage](https://codecov.io/gh/r-a-dobson/dynamicSDM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-a-dobson/dynamicSDM?branch=main)
[![R-CMD-check](https://github.com/r-a-dobson/dynamicSDM/workflows/R-CMD-check/badge.svg)](https://github.com/r-a-dobson/dynamicSDM/actions/workflows/rcmdcheck.yml)
<!-- badges: end -->

# Summary

Across ecological research fields, species distribution and abundance
modelling (SDM) is a major tool for understanding the drivers and
patterns of species occurrence. To advance our ability to model species
inhabiting our dynamic ecosystems, **dynamicSDM** facilitates the
incorporation of explanatory variables that are dynamic in both space
and time. Our functions are:

  - **user-friendly** - requiring only simple inputs and outputs
  - **highly flexible** - offering diverse and open arguments for
    targeting study specifics;
  - **computer friendly** - utilising Google Earth Engine and Google
    Drive to minimise the computing power and storage demands associated
    with high spatiotemporal resolution
modelling.

# Package structure

<a href='https://r-a-dobson.github.io/dynamicSDM'><img src="https://raw.githubusercontent.com/r-a-dobson/dynamicSDM/main/man/figures/Figure1.png" align="centre" height="250"/></a>

### 1\) Response data functions

Functions for preparing species distribution or abundance model input
data for modelling with spatiotemporally dynamic explanatory variables.

  - `spatiotemp_check()` Check species occurrence record formatting,
    completeness and validity.
  - `spatiotemp_extent()` Filter species occurrence records by a given
    spatial and temporal extent.
  - `spatiotemp_resolution()` Filter species occurrence records by given
    spatial and temporal resolution.
  - `spatiotemp_bias()` Test for spatial and temporal bias in species
    occurrence records.
  - `spatiotemp_thin()` Thin species occurrence records by spatial and
    temporal proximity.
  - `spatiotemp_pseudoabs()` Generate pseudo-absence record coordinates
    and dates.
  - `spatiotemp_weights()` Calculate sampling effort across spatial and
    temporal buffer from species occurrence records.

### 2\) Explanatory variable functions

Functions for extracting spatiotemporally dynamic explanatory variable
data for species occurrence record co-ordinates and dates using Google
Earth Engine.

  - `extract_dynamic_coords()` Extract temporally dynamic explanatory
    variable data for occurrence records.
  - `get_moving_window()` Generate a “moving window” matrix of optimal
    size for spatial buffering of explanatory variable data.
  - `extract_buffered_coords()` Extract spatially buffered and
    temporally dynamic explanatory variable data for occurrence records.
  - `extract_coords_combine()` Combine extracted explanatory variable
    data for occurrence records into single data frame for model
    fitting.

### 3\) Modelling functions

Functions for generating species distribution or abundance models that
account for spatial and temporal autocorrelation in dynamic explanatory
variables.

  - `spatiotemp_autocorr()` Test for spatial and temporal
    autocorrelation in species distribution model explanatory data.
  - `spatiotemp_block()` Split occurrence records into spatial and
    temporal blocks for model fitting.
  - `brt_fit()` Fit boosted regression tree models to species
    distribution or abundance data.

### 4\) Projection functions

Functions for generating explanatory variable projection data frames at
given spatiotemporal extent and resolution, and projecting species
dynamic distribution and abundance patterns onto these.

  - `dynamic_proj_dates()` Generate vector of dates for dynamic
    projections
  - `extract_dynamic_raster()` Extract temporally dynamic rasters of
    explanatory variable data.
  - `extract_buffered_raster()` Extract spatially buffered and
    temporally dynamic rasters of explanatory variable data.
  - `dynamic_proj_covariates()` Combine explanatory variable rasters
    into a covariate data frame for each projection date.
  - `dynamic_proj()` Project species distribution and abundance models
    onto dynamic environmental covariates.
  - `dynamic_proj_GIF()` Create GIF of dynamic species distribution and
    abundance projections

# Installation

``` r
# Install using Github 
install_github("r-a-dobson/dynamicSDM")
```
