# Export an R object as .RData file

Export an R object as .RData file

## Usage

``` r
export_RData(object, out.dir = tempdir(), out.nm = class(object))
```

## Arguments

- object:

  An R object.

- out.dir:

  The path to the output directory. Default: the working directory

- out.nm:

  The output filename (with or without '.RData' ending). Default: the
  class of `object` followed by '.RData'.

## Value

`NULL`

## Examples

``` r
if(interactive()){
df <- data.frame('A' = seq(1:10), 'B' = rev(seq(1:10)))

export_RData(df)
}
```
