# Extract relevant data from a raw data export file generated from the software of Perkin Elmer's "Victor X3" plate readers.

Extract relevant data from a raw data export file generated from the
software of Perkin Elmer's "Victor X3" plate readers.

## Usage

``` r
parse_victorx3(input)
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
input <- read_file(filename = system.file("victorx3_output.txt", package = "QurvE") )
parsed <- parse_victorx3(input)
}
```
