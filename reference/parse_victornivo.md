# Extract relevant data from a raw data export file generated from the software of Perkin Elmer's "Victor Nivo" plate readers.

Extract relevant data from a raw data export file generated from the
software of Perkin Elmer's "Victor Nivo" plate readers.

## Usage

``` r
parse_victornivo(input)
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
input <- read_file(filename = system.file("nivo_output.csv", package = "QurvE"), csvsep = "," )
parsed <- parse_victornivo(input)
}
```
