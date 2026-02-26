# Create a PDF and HTML report with results from a growth curve analysis workflow

`growth.report` requires a `grofit` object and creates a report in PDF
and HTML format that summarizes all results.

## Usage

``` r
growth.report(
  grofit,
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

- grofit:

  A `grofit` object created with
  [`growth.workflow`](https://nicwir.github.io/QurvE/reference/growth.workflow.md).

- out.dir:

  (Character) The path or name of the folder in which the report files
  are created. If `NULL`, the folder will be named with a combination of
  'Report.growth\_' and the current date and time.

- out.nm:

  Character or `NULL` Define the name of the report files. If `NULL`,
  the files will be named with a combination of 'GrowthReport\_' and the
  current date and time.

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
    are accepted. Can be `'all'`, a string vector, or a list of string
    vectors. Note: The maximum number of sample groups (with unique
    condition/concentration indicators) is 50. If you have more than 50
    groups, option `'all'` will produce the error
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
if (FALSE) { # \dontrun{
# Create random growth data set
  rnd.data <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')


  # Run growth curve analysis workflow
  res <- growth.workflow(time = rnd.data$time,
                         data = rnd.data$data,
                         fit.opt = 's',
                         ec50 = FALSE,
                         export.res = FALSE,
                         suppress.messages = TRUE,
                         parallelize = FALSE)

  growth.report(res, out.dir = tempdir(), parallelize = FALSE)
} # }
```
