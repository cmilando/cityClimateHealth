# Plot method for condPois_1stage_list

Plot method for condPois_1stage_list

## Usage

``` r
# S3 method for class 'condPois_1stage_list'
plot(x, xlab = NULL, ylab = NULL, title = NULL)
```

## Arguments

- x:

  an object of class condPois_1stage_list

- xlab:

  xlab override

- ylab:

  ylab override

- title:

  title override

## Value

a ggplot object

## Examples

``` r
middlesex_deaths_tbl <- make_outcome_table(
middlesex_deaths,  outcome_columns, collapse_to = 'age_grp')
#> Error in make_outcome_table(middlesex_deaths, outcome_columns, collapse_to = "age_grp"): unused argument (collapse_to = "age_grp")

# run the model
m3 <- condPois_1stage(exposure_matrix = middlesex_exposure_mat,
                     outcomes_tbl = middlesex_deaths_tbl,
                     global_cen = 15,
                     multi_zone = TRUE,
                     verbose = 1)
#> Error: object 'middlesex_exposure_mat' not found
plot(m3)
#> Error: object 'm3' not found
```
