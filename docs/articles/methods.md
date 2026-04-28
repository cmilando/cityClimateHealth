# Methods and Description

This page is a work in progress

## Overview

This package contains functions that streamline an analytical pipepline
for estimating relative risks and attributable numbers.

Although situated in the contenxt of climate change and city-level
health impacts, there are not any physical limitations on applying this
code to other signals and responses.

This package is drawn heavily from the work of Dr. Antonio Gasparrini.

Chad Milando, PhD (cmilando\[at\]bu.edu); Alexis Arlak
(aarlak\[at\]bu.edu); Gregory Wellenius, ScD (wellenius\[at\]bu.edu).
The Center for Climate and Health, Boston University School of Public
Health.

## Exposures

We applied a distributed lag non-linear modeling (DLNM) framework to
capture both the non-linear and lagged effects of temperature exposure.
This approach represents exposure as a crossbasis matrix with separate
components for exposure magnitude and lag.

The reason we use DLNM is because we believe that exposure magnitude
have a non-linear relationship

The interpretation of the crossbasis matrix is that it allows the model
to account for nonlinear associations in exposure (temperatures of 100
°F do not have double the impact of temperatures of 100 °F) and in time
(exposures 2 days prior impact populations differently than those 1 day
prior or 3 days prior).

Defining the crossbasis has several components: \* maxlag \* arglag \*
argvar

For exposure magnitude, we used a natural spline with knots at the 50th
and 90th percentiles of the exposure distribution. For lag, we used a
natural spline with two evenly spaced log-knots between 0 and a maximum
lag of 8 days.

## Outcome

Any outcome will work.

## Timing

You can use a variety of timings: \* daily \* weekly \* monthly

## Model types

\[…\]

### Conditional Poisson

We used a time-stratified case-crossover study design, specifically a
single-stage conditional quasi-Poisson model with strata defined by ZIP
Code Tabulation Area (ZCTA), year, month, and day of week.

The conditional Poisson approach enables efficient estimation of model
coefficients without requiring estimation of strata-specific intercepts.

At the strata level, the model takes the form: log(yₛ,ᵢ) = αₛ + βwₛ,ᵢ +
holiday. Here, the daily count of emergency department visits (yₛ,ᵢ)
depends on a strata-specific intercept (αₛ), which is calculated in
post-processing due to the conditional Poisson formulation, crossbasis
weights (wₛ,ᵢ), and an indicator for federal holidays.

Strata with no outcomes are excluded

There are minimums that each strata must include: \* x

### Time-series

\[…\]

## Model structures

### 1-stage

A single-stage model pools statistical coefficients across all strata,
while the quasi-Poisson formulation adjusts variance to account for
over-dispersion in the observed outcome.

### 2-stage

\[…\]

### Spatial Bayesian methods

\[…\]

## Attributable numbers

After fitting the model, we calculated the number of emergency
department visits attributable to high ambient summertime temperatures
for each year. This requires defining a reference temperature, which
serves as the baseline for comparison. We selected 75°F as the
reference, corresponding to the average summertime daily maximum
temperature.

The attributable number represents the additional health impacts
occurring when temperatures exceed this baseline. In practical terms, it
reflects the potentially avoidable emergency department visits that
could be prevented if risks on very hot days were reduced to those
observed on an average summer day.

For example, using 75°F as the reference, if a day reaches 100°F and is
associated with 1,000 additional emergency department visits, this means
that compared to a 75°F day, there were 1,000 excess visits at 100°F.
Changing the reference temperature (for example, to 80°F or 70°F) would
change the estimated attributable number accordingly.
