# Call the appropriate function required to read a table file and return the table as a dataframe object.

`read_file` automatically detects the format of a file provided as
`filename` and calls the appropriate function to read the table file.

## Usage

``` r
read_file(filename, csvsep = ";", dec = ".", sheet = 1)
```

## Arguments

- filename:

  (Character) Name or path of the table file to read. Can be of type
  CSV, XLS, XLSX, TSV, or TXT.

- csvsep:

  (Character) separator used in CSV file (ignored for other file types).

- dec:

  (Character) decimal separator used in CSV, TSV and TXT files.

- sheet:

  (Numeric or Character) Number or name of a sheet in XLS or XLSX files
  (*optional*). Default: `";"`

## Value

A dataframe object with headers in the first row.

## Examples

``` r
input <- read_file(filename = system.file("2-FMA_toxicity.csv", package = "QurvE"), csvsep = ";" )
```
