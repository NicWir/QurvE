# Extract relevant data from a raw data export file generated with the "Gen5" or "Gen6" software.

Extract relevant data from a raw data export file generated with the
"Gen5" or "Gen6" software.

## Usage

``` r
parse_Gen5Gen6(input)
```

## Arguments

- input:

  A dataframe created by reading a table file with
  [`read_file`](https://nicwir.github.io/QurvE/reference/read_file.md)

## Value

a list of length two containing growth and/or fluorescence dataframes in
the first and second element, respectively. The first column in these
dataframes represents a time vector.

## Examples

``` r
if(interactive()){
input <- read_file(filename = system.file("fluorescence_test_Gen5.xlsx", package = "QurvE") )
parsed <- parse_Gen5Gen6(input)
}
```
