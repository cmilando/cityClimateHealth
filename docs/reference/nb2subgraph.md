# Convert a neighbor list subgraph to node pairs

https://github.com/stan-dev/example-models/blob/master/knitr/car-iar-poisson/nb_data_funs.R

## Usage

``` r
nb2subgraph(x, c_id, comp_ids, offsets)
```

## Arguments

- x:

  an nb object (neighbor list)

- c_id:

  subcomponent id to extract

- comp_ids:

  vector of subcomponent ids for each node

- offsets:

  vector of per-component node offsets

## Value

a list with node1 and node2 integer vectors defining graph edges

## Examples

``` r
nb <- spdep::cell2nb(3, 3)
comp_ids <- spdep::n.comp.nb(nb)$comp.id
offsets <- cityClimateHealth:::indexByComponent(comp_ids)
cityClimateHealth:::nb2subgraph(nb, c_id = 1, comp_ids = comp_ids, offsets = offsets)
#> $node1
#>  [1] 1 1 2 2 3 4 4 5 5 6 7 8
#> 
#> $node2
#>  [1] 2 4 3 5 6 5 7 6 8 9 8 9
#> 
```
