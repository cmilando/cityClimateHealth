# Function to clean and prepare the exposure data matrix

Function to clean and prepare the exposure data matrix

## Usage

``` r
make_exposure_matrix(
  data,
  column_mapping,
  time_subset,
  dt_by = "day",
  maxgap = 5,
  maxlag = 5,
  grp_level = FALSE,
  keep_unit_exposures = FALSE,
  exposure_is_factor = FALSE
)
```

## Arguments

- data:

  a dataset of exposures

- column_mapping:

  a named list that indicates relevant columns in `data`. for the
  exposure data table, these need to be one of: c('date', "exposure",
  'geo_unit', 'geo_unit_grp')

- time_subset:

  the time period of interest for analysis, specified as years, months,
  or days must be specified by user - no default

- dt_by:

  is it daily data, or weekly or ...

- maxlag:

  the number of lags for the exposure variable (default is 5)

- grp_level:

  whether to summarize to the group level or not (default)

- keep_unit_exposures:

  if grp_level is true, whether to keep original unit-level exposures

- exposure_is_factor:

  exposure is a factor

- maxgaps:

  the maximum allowable missing exposure data gap, to be passed to
  zoo::na.approx (default is 5)

## Value

a data.table of class("exposure")

## Examples
