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
[Gasparrini 2011](https://www.jstatsoft.org/article/view/v043i08)
describes these methods, and the R package `dlnm`.

This approach represents exposure as a crossbasis matrix with separate
components for exposure magnitude and lag. The interpretation of the
crossbasis matrix is that it allows the model to account for nonlinear
associations in exposure and in time. For example, temperatures of 100
°F do not have double the impact of temperatures of 50 °F, and exposures
2 days prior impact populations differently than those 1 day prior or 3
days prior.

Turning a single exposure time-series into a crossbasis matrix is done
via the `crossbasis()` function as part of `dlnm` and users must specify
several function arguments:

- `maxlag`, the maximum lag
- `arvagr`, the nature of the non-linear relationship with exposure
  magnitude
- `arglag`, the nature of the non-linear relationship with exposure
  timing

More details on how to specify `maxlag`, `arglag`, and `argvar` can be
found in the `dlnm` documentation.

For `cityClimateHealth` we chose some default values for these
arguments, specifically for the case study of looking at warm-season
temperature and mortality/morbidity:

- `maxlag = 5`
- `argvar`: natural spline with knots at the 50th and 90th percentiles
  of the exposure distribution.
- `arglag`: a natural spline with two evenly spaced log-knots between 0
  and a maximum lag of 8 days.

If other exposures / timings are desired, the user will need to adjust
these arguments accordingly.

## Outcome

Any time-series of outcomes will work.

## Timing

You can use a variety of timings: \* daily \* weekly \* monthly

## Model types

We can perform exposure-outcome analyses several ways:

- **space-time stratified case-crossover** – time is controlled by
  assigning a strata variable and comparing counts (or rates) of
  outcomes within strata. a common strata choice is \[spatial
  unit\]:\[year\]:\[month\]:\[day of week\]. The following model types
  can be used in this study design:

  - Conditional logistic
  - Poisson
  - Conditional Poisson

  [Gasparrini and
  Armstrong](http://chadmilando.com/cityClimateHealth/articles/) provide
  analysis that show that these provide the same results, although
  Conditional Poisson is more computationally efficient because the
  strata terms are conditioned out and not modeled.

- **time-series** – time is controlled by a natural spline with a
  specific number of knots for year, day of year, season, and decade.
  additional control is added by a categorical variable for day of week.
  The common choices for splines knots in this case are: XYZ, see
  [ref](http://chadmilando.com/cityClimateHealth/articles/)

### Conditional logistic

See Darren’s paper. we are working to implement this code.

Code examples includee: \*

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

Code examples includee: \*

### Time-series

we are working to implement this code.

Code examples includee: \*

## Model structures

### 1-stage

A single-stage model pools statistical coefficients across all strata,
while the quasi-Poisson formulation adjusts variance to account for
over-dispersion in the observed outcome.

Code examples includee: \*

### 2-stage

\[…\]

Code examples includee: \*

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

Code examples includee: \*
