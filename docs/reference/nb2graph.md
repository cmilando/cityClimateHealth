# Convert a neighbor list to a graph edge list

https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R

## Usage

``` r
nb2graph(x)
```

## Arguments

- x:

  an nb object (neighbor list)

## Value

a list with N (nodes), N_edges, node1, and node2 integer vectors

## Examples

``` r
nb <- spdep::cell2nb(3, 3)
cityClimateHealth:::nb2graph(nb)
#> $N
#> [1] 9
#> 
#> $N_edges
#> [1] 12
#> 
#> $node1
#>  [1] 1 1 2 2 3 4 4 5 5 6 7 8
#> 
#> $node2
#>  [1] 2 4 3 5 6 5 7 6 8 9 8 9
#> 
```
