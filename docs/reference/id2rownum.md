# Take particular id and convert to row number

id2rownum takes an id and converts it to the right row number in the
dataset; ids should be unique in the dataset that is, don't pass the
function panel data with multiple same ids

## Usage

``` r
id2rownum(id, data, idname)
```

## Arguments

- id:

  a particular id

- data:

  data frame

- idname:

  unique id
