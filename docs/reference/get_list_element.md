# Return Particular Element from Each Element in a List

a function to take a list and get a particular part out of each element
in the list

## Usage

``` r
get_list_element(listolists, whichone = 1)
```

## Arguments

- listolists:

  a list

- whichone:

  which item to get out of each list (can be numeric or name)

## Value

list of all the elements 'whichone' from each list

## Examples

``` r
len <- 100 # number elements in list
lis <- lapply(1:len, function(l) list(x = (-l), y = l^2)) # create list
get_list_element(lis, "x")[1] # should be equal to -1
#> [[1]]
#> [1] -1
#> 
get_list_element(lis, 1)[1] # should be equal to -1
#> [[1]]
#> [1] -1
#> 
```
