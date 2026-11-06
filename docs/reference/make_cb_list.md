# IN-PROGRESS functions that creates a list of cross-basis objects to try

IN-PROGRESS functions that creates a list of cross-basis objects to try

## Usage

``` r
make_cb_list(x, var_list, argvar_list, lag_list, arglag_list)
```

## Arguments

- x:

  the data.frame with the var_list data in it

- var_list:

  a character vector of variable names

- argvar_list:

  a list of argvar argument sets for `crossbasis`

- lag_list:

  a list of lag specifications

- arglag_list:

  a list of arglag argument sets for `crossbasis`

## Value

a nested list of cross-basis objects

## Examples

``` r
boston <- subset(ma_exposure, TOWN20 == "BOSTON" & date < as.Date("2010-04-01"))
cbs <- cityClimateHealth:::make_cb_list(
  x = boston, var_list = "tmax_C",
  argvar_list = list(list(fun = "ns", df = c(3, 4))),
  lag_list = 5,
  arglag_list = list(list(fun = "ns", df = 3))
)
length(cbs)
#> [1] 2
```
