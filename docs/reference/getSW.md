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
towns <- c("CHELSEA", "EVERETT", "REVERE", "MALDEN")
shp <- subset(ma_towns, TOWN20 %in% towns)
cityClimateHealth:::getSW(shp = shp, ni = 1, include_self = FALSE)
#> has to be one polygon per row in `shp`
#>      [,1] [,2] [,3] [,4]
#> [1,]    0    1    1    0
#> [2,]    1    0    1    1
#> [3,]    1    1    0    1
#> [4,]    0    1    1    0
```
