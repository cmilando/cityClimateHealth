# Creates the list of spatial neighbors used in the `condPois_sb` Leroux model

Helper function to get neighbors

## Usage

``` r
getSW(shp, ni, include_self = T)
```

## Arguments

- shp:

  an sf object with one polygon per row

- ni:

  integer; neighbor order (0 = identity, 1 = first-order neighbors)

- include_self:

  logical; whether to include each unit as its own neighbor

## Value

a binary matrix of spatial weights

## Examples

``` r
if (FALSE) { # \dontrun{
# after loading an sf shapefile:
SW <- getSW(shp = my_shapefile, ni = 1, include_self = FALSE)
} # }
```
