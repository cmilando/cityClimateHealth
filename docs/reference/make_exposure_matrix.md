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

  the time period of interest for analysis, specified as years or months
  must be specified by user - no default

- dt_by:

  is it daily data, or weekly or ...

- maxgap:

  the maximum allowable gap of missing exposure data, passed to
  [`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html)
  (default 5)

- maxlag:

  the number of lags for the exposure variable (default is 5)

- grp_level:

  whether to summarize to the group level or not (default)

- keep_unit_exposures:

  if grp_level is true, whether to keep original unit-level exposures

- exposure_is_factor:

  exposure is a factor

## Value

a data.table of class("exposure")

## Examples

``` r
exposure_columns <- list(
  "date" = "date",
  "exposure" = "tmax_C",
  "geo_unit" = "TOWN20",
  "geo_unit_grp" = "COUNTY20"
)
exposure_mat <- make_exposure_matrix(
  subset(ma_exposure, TOWN20 == "BOSTON"),
  exposure_columns,
  time_subset = list(month = 5:9, year = 2012:2015)
)
#> -- NA values automatically removed
#> strata dt_by = 'day', setting strata as geo_unit:yr:mn:dow
head(exposure_mat)
#>          date TOWN20 COUNTY20 tmax_C                   strata      match_strata
#>        <IDat> <char>   <char>  <num>                   <char>            <char>
#> 1: 2012-05-01 BOSTON  SUFFOLK   17.1 BOSTON:yr2012:mn05:dow03 BOSTON:2012-05-01
#> 2: 2012-05-02 BOSTON  SUFFOLK    8.6 BOSTON:yr2012:mn05:dow04 BOSTON:2012-05-02
#> 3: 2012-05-03 BOSTON  SUFFOLK   11.7 BOSTON:yr2012:mn05:dow05 BOSTON:2012-05-03
#> 4: 2012-05-04 BOSTON  SUFFOLK   12.4 BOSTON:yr2012:mn05:dow06 BOSTON:2012-05-04
#> 5: 2012-05-05 BOSTON  SUFFOLK   12.7 BOSTON:yr2012:mn05:dow07 BOSTON:2012-05-05
#> 6: 2012-05-06 BOSTON  SUFFOLK   17.8 BOSTON:yr2012:mn05:dow01 BOSTON:2012-05-06
#>    explag1 explag2 explag3 explag4 explag5
#>      <num>   <num>   <num>   <num>   <num>
#> 1:    14.3    14.3    12.7    18.3    15.8
#> 2:    17.1    14.3    14.3    12.7    18.3
#> 3:     8.6    17.1    14.3    14.3    12.7
#> 4:    11.7     8.6    17.1    14.3    14.3
#> 5:    12.4    11.7     8.6    17.1    14.3
#> 6:    12.7    12.4    11.7     8.6    17.1
```
