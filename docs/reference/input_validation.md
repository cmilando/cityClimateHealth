# Input validation

Input validation

## Usage

``` r
input_validation(exposure_matrix, outcomes_tbl)
```

## Arguments

- exposure_matrix:

  an exposure matrix object of class "exposure"

- outcomes_tbl:

  an outcomes table object of class "outcome"

## Value

a list with validated `exposure_matrix` and `outcomes_tbl`

## Examples

``` r
if (FALSE) { # \dontrun{
# after creating exposure and outcome objects:
validated <- input_validation(exposure_matrix, outcomes_tbl)
exposure_matrix <- validated$exposure_matrix
outcomes_tbl <- validated$outcomes_tbl
} # }
```
