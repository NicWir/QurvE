# Export a tabular object as tab-separated .txt file

Export a tabular object as tab-separated .txt file

## Usage

``` r
export_Table(table, out.dir = tempdir(), out.nm = deparse(substitute(table)))
```

## Arguments

- table:

  A tabular R object (dataframe, matrix, array)

- out.dir:

  The path to the output directory. Default: the working directory

- out.nm:

  The output filename (with or without '.txt' ending). Default: the name
  of `table` followed by '.txt'.

## Value

`NULL`

## Examples

``` r
if(interactive()){
df <- data.frame('A' = seq(1:10), 'B' = rev(seq(1:10)))

export_Table(df)
}
```
