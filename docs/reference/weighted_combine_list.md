# weighted_combine_list

A function that takes in either a list of vectors or matrices and
computes a weighted average of them, where the weights are applied to
every element in the list.

## Usage

``` r
weighted_combine_list(l, w, normalize_weights = TRUE)
```

## Arguments

- l:

  a list that contains either vectors or matrices of the same dimension
  that are to be combined

- w:

  a vector of weights, the weights should have the same number of
  elements as \`length(l)\`

- normalize_weights:

  whether or not to force the weights to sum to 1, default is true

## Value

matrix or vector corresponding to the weighted average of all of the
elements in \`l\`

## Examples

``` r
l <- list(c(1, 2, 3), c(4, 5, 6))
weighted_combine_list(l, w = c(0.5, 0.5)) ## returns c(2.5, 3.5, 4.5)
#> [1] 2.5 3.5 4.5
```
