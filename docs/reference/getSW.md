# Creates the list of spatial neighbors used in the `condPois_sb` Leroux model

Helper function to get neighbors

## Usage

``` r
getSW(shp, ni, include_self = T)
```

## Arguments

- include_self:

## Examples

``` r
if (FALSE) { # \dontrun{
# after loading an sf shapefile:
SW <- getSW(shp = my_shapefile, ni = 1, include_self = FALSE)
} # }
```
