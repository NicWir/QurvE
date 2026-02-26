# Create a PDF and HTML report with results from a fluorescence analysis workflow

`fl.report` requires a `flFitRes` object and creates a report in PDF and
HTML format that summarizes all results obtained.

## Usage

``` r
fl.report(
  flFitRes,
  out.dir = tempdir(),
  out.nm = NULL,
  ec50 = FALSE,
  format = c("pdf", "html"),
  export = FALSE,
  parallelize = TRUE,
  ...
)
```

## Arguments

- flFitRes:

  A `grofit` object created with
  [`fl.workflow`](https://nicwir.github.io/QurvE/reference/fl.workflow.md).

- out.dir:

  (Character) The path or name of the folder in which the report files
  are created. If `NULL`, the folder will be named with a combination of
  'Report.fluorescence\_' and the current date and time.

- out.nm:

  Character or `NULL` Define the name of the report files. If `NULL`,
  the files will be named with a combination of 'FluorescenceReport\_'
  and the current date and time.

- ec50:

  (Logical) Display results of dose-response analysis (`TRUE`) or not
  (`FALSE`).

- format:

  (Character) Define the file format for the report, PDF (`'pdf'`)
  and/or HTML (`'html'`). Default: (`c('pdf', 'html')`)

- export:

  (Logical) Shall all plots generated in the report be exported as
  individual PDF and PNG files `TRUE` or not `FALSE`?

- parallelize:

  (Logical) Create plots using all but one available processor cores
  (`TRUE`) or only a single core (`FALSE`).

- ...:

  Further arguments passed to create a report. Currently supported:

  - `mean.grp`: Define groups to combine into common plots in the report
    based on sample identifiers. Partial matches with sample/group names
    are accepted. Can be `'all'`, a vector of strings, or a list of
    string vectors. Note: The maximum number of sample groups (with
    unique condition/concentration indicators) is 50. If you have more
    than 50 groups, option `'all'` will produce the error
    `! Insufficient values in manual scale. [Number] needed but only 50 provided`.

  - `mean.conc`: Define concentrations to combine into common plots in
    the report. Can be a numeric vector, or a list of numeric vectors.

## Value

`NULL`

## Details

The template .Rmd file used within this function can be found within the
QurvE package installation directory.

## Examples

``` r
# load example dataset
if (FALSE) { # \dontrun{
input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
                   data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
                   csvsep = "\t",
                   csvsep.fl = "\t")

# Run workflow
res <- fl.workflow(grodata = input, ec50 = FALSE, fit.opt = 's',
                   x_type = 'time', norm_fl = TRUE,
                   dr.parameter = 'max_slope.spline',
                   suppress.messages = TRUE,
                   parallelize = FALSE)

fl.report(res, out.dir = tempdir(), parallelize = FALSE)
} # }
```
