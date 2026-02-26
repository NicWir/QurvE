# Combine two dataframes like a zip-fastener

Combine rows or columns of two dataframes in an alternating manner

## Usage

``` r
zipFastener(df1, df2, along = 2)
```

## Arguments

- df1:

  A first dataframe.

- df2:

  A second dataframe with the same dimensions as `df1`.

- along:

  `1` to alternate rows or `2` to alternate columns.

## Value

A dataframe with combined rows (or columns) of df1 and df2.

## Author

Mark Heckmann

## Examples

``` r
# data frames equal dimensions
df1 <- plyr::rdply(3, rep('o',4))[ ,-1]
df2 <- plyr::rdply(3, rep('X',4))[ ,-1]

zipFastener(df1, df2)
#>   V1 V1.1 V2 V2.1 V3 V3.1 V4 V4.1
#> 1  o    X  o    X  o    X  o    X
#> 2  o    X  o    X  o    X  o    X
#> 3  o    X  o    X  o    X  o    X
zipFastener(df1, df2, 2)
#>   V1 V1.1 V2 V2.1 V3 V3.1 V4 V4.1
#> 1  o    X  o    X  o    X  o    X
#> 2  o    X  o    X  o    X  o    X
#> 3  o    X  o    X  o    X  o    X
zipFastener(df1, df2, 1)
#>   V1 V2 V3 V4
#> 1  o  o  o  o
#> 4  X  X  X  X
#> 2  o  o  o  o
#> 5  X  X  X  X
#> 3  o  o  o  o
#> 6  X  X  X  X

# data frames unequal in no. of rows
df1 <- plyr::rdply(10, rep('o',4))[ ,-1]
zipFastener(df1, df2, 1)
#>    V1 V2 V3 V4
#> 1   o  o  o  o
#> 11  X  X  X  X
#> 2   o  o  o  o
#> 12  X  X  X  X
#> 3   o  o  o  o
#> 13  X  X  X  X
#> 4   o  o  o  o
#> 5   o  o  o  o
#> 6   o  o  o  o
#> 7   o  o  o  o
#> 8   o  o  o  o
#> 9   o  o  o  o
#> 10  o  o  o  o
zipFastener(df2, df1, 1)
#>    V1 V2 V3 V4
#> 1   X  X  X  X
#> 4   o  o  o  o
#> 2   X  X  X  X
#> 5   o  o  o  o
#> 3   X  X  X  X
#> 6   o  o  o  o
#> 7   o  o  o  o
#> 8   o  o  o  o
#> 9   o  o  o  o
#> 10  o  o  o  o
#> 11  o  o  o  o
#> 12  o  o  o  o
#> 13  o  o  o  o

# data frames unequal in no. of columns
df2 <- plyr::rdply(10, rep('X',3))[ ,-1]
zipFastener(df1, df2)
#>    V1 V1.1 V2 V2.1 V3 V3.1 V4
#> 1   o    X  o    X  o    X  o
#> 2   o    X  o    X  o    X  o
#> 3   o    X  o    X  o    X  o
#> 4   o    X  o    X  o    X  o
#> 5   o    X  o    X  o    X  o
#> 6   o    X  o    X  o    X  o
#> 7   o    X  o    X  o    X  o
#> 8   o    X  o    X  o    X  o
#> 9   o    X  o    X  o    X  o
#> 10  o    X  o    X  o    X  o
zipFastener(df2, df1, 2)
#>    V1 V1.1 V2 V2.1 V3 V3.1 V4
#> 1   X    o  X    o  X    o  o
#> 2   X    o  X    o  X    o  o
#> 3   X    o  X    o  X    o  o
#> 4   X    o  X    o  X    o  o
#> 5   X    o  X    o  X    o  o
#> 6   X    o  X    o  X    o  o
#> 7   X    o  X    o  X    o  o
#> 8   X    o  X    o  X    o  o
#> 9   X    o  X    o  X    o  o
#> 10  X    o  X    o  X    o  o
```
