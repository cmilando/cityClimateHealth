# getRR method for condPois_1stage_list

getRR method for condPois_1stage_list

## Usage

``` r
# S3 method for class 'condPois_1stage_list'
getRR(x)
```

## Arguments

- x:

  an object of class condPois_1stage_list

## Value

a data.table of relative risk estimates across factor levels

## Examples

``` r
middlesex_deaths_tbl <- make_outcome_table(
middlesex_deaths,  outcome_columns, collapse_to = 'age_grp')
#> Error: object 'middlesex_deaths' not found

# run the model
m3 <- condPois_1stage(exposure_matrix = middlesex_exposure_mat,
                     outcomes_tbl = middlesex_deaths_tbl,
                     global_cen = 15,
                     multi_zone = TRUE,
                     verbose = 1)
#> Error: object 'middlesex_exposure_mat' not found
getRR(m3)
#> Error: object 'm3' not found
```
