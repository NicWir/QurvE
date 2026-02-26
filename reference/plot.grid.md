# Plot a matrix of growth curve panels

`plot.grid` takes a `grofit` or `flFitRes` object and returns a facet
grid of individual growth and fluorescence plots

## Usage

``` r
# S3 method for class 'grid'
plot(
  x,
  data.type = c("spline", "raw", "norm.fl"),
  param = c("mu.linfit", "lambda.linfit", "dY.linfit", "A.linfit", "mu2.linfit",
    "lambda2.linfit", "mu.model", "lambda.model", "A.model", "A.orig.model", "dY.model",
    "dY.orig.model", "tD.linfit", "tD2.linfit", "tD.spline", "tD2.spline", "mu.spline",
    "lambda.spline", "A.spline", "dY.spline", "integral.spline", "mu2.spline",
    "lambda2.spline", "mu.bt", "lambda.bt", "A.bt", "integral.bt", "max_slope.linfit",
    "max_slope.spline"),
  pal = c("Green", "Orange", "Purple", "Magenta", "Grey", "Blue", "Grey", "Red", "Cyan",
    "Brown", "Mint"),
  invert.pal = FALSE,
  IDs = NULL,
  sort_by_ID = FALSE,
  names = NULL,
  conc = NULL,
  exclude.nm = NULL,
  exclude.conc = NULL,
  mean = TRUE,
  log.y = TRUE,
  n.ybreaks = 6,
  sort_by_conc = TRUE,
  nrow = NULL,
  basesize = 20,
  y.lim = NULL,
  x.lim = NULL,
  legend.lim = NULL,
  y.title = NULL,
  x.title = NULL,
  lwd = 1.1,
  plot = TRUE,
  export = FALSE,
  height = NULL,
  width = NULL,
  out.dir = NULL,
  out.nm = NULL,
  ...
)
```

## Arguments

- x:

  A `grofit` or `flFitRes` object created with
  [`growth.workflow`](https://nicwir.github.io/QurvE/reference/growth.workflow.md)
  or
  [`fl.workflow`](https://nicwir.github.io/QurvE/reference/fl.workflow.md)
  containing spline fits.

- data.type:

  (Character) Plot either raw data (`data.type = "raw"`) or the spline
  fit results

- param:

  (Character) The parameter used to compare different sample groups. Any
  name of a column containing numeric values in `gcTable` (which is
  stored within `grofit` or `gcFit` objects) can be used as input.
  Useful options are: 'mu.linfit', 'lambda.linfit', 'dY.linfit',
  'A.linfit', 'mu.model', 'lambda.model', 'A.model', 'mu.spline',
  'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu.bt',
  'lambda.bt', 'A.bt', 'integral.bt'

- pal:

  (Character string) Choose one of 'Green', 'Orange', 'Purple',
  'Magenta', 'Grey', 'Blue', 'Grey', 'Red', 'Cyan', 'Brown', or 'Mint'
  to visualize the value of the parameter chosen as `param` for each
  sample or condition.

- invert.pal:

  (Logical) Shall the colors in the chosen `pal` be inverted (`TRUE`) or
  not `FALSE`?

- IDs:

  (String or vector of strings) Define samples or groups (if
  `mean = TRUE`) to combine into a single plot based on exact matches
  with entries in the `label` or `condition` columns of
  `grofit$expdesign`. The order of strings within the vector defines the
  order of samples within the grid.

- sort_by_ID:

  (Logical) Shall samples/conditions be ordered as entered in `IDs`
  (`TRUE`) or alphabetically (`FALSE`)?

- names:

  (String or vector of strings) Define groups to combine into a single
  plot. Partial matches with sample/group names are accepted. If `NULL`,
  all samples are considered. Note: Ensure to use unique substrings to
  extract groups of interest. If the name of one condition is included
  in its entirety within the name of other conditions, it cannot be
  extracted individually.

- conc:

  (Numeric or numeric vector) Define concentrations to combine into a
  single plot. If `NULL`, all concentrations are considered. Note:
  Ensure to use unique concentration values to extract groups of
  interest. If the concentration value of one condition is included in
  its entirety within the name of other conditions (e.g., the dataset
  contains '1', '10', and '100', `code = 10` will select both '10 and
  '100'), it cannot be extracted individually.

- exclude.nm:

  (String or vector of strings) Define groups to exclude from the plot.
  Partial matches with sample/group names are accepted.

- exclude.conc:

  (Numeric or numeric vector) Define concentrations to exclude from the
  plot.

- mean:

  (Logical) Display the mean and standard deviation of groups with
  replicates (`TRUE`) or plot each sample individually (`FALSE`)?

- log.y:

  (Logical) Log-transform the y-axis of the plot (`TRUE`) or not
  (`FALSE`)?#'

- n.ybreaks:

  (Numeric) Number of breaks on the y-axis. The breaks are generated
  using
  [`scales::pretty_breaks`](https://scales.r-lib.org/reference/pretty_breaks.html).
  Thus, the final number of breaks can deviate from the user input.

- sort_by_conc:

  (Logical) Shall the samples/conditions be sorted with concentrations
  in rows and groups in columns?

- nrow:

  (Numeric) Defines the number of rows in the grid if `sort_by_conc` is
  `FALSE`.

- basesize:

  (Numeric) Base font size.

- y.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds of the y-axis of the growth curve plot as a
  vector in the form `c(l, u)`. If only the lower or upper bound should
  be fixed, provide `c(l, NA)` or `c(NA, u)`, respectively.

- x.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds of the x-axis of both growth curve and
  derivative plots as a vector in the form `c(l, u)`. If only the lower
  or upper bound should be fixed, provide `c(l, NA)` or `c(NA, u)`,
  respectively.

- legend.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds of the color scale applied to `param` as a
  vector in the form `c(l, u)`. If only the lower or upper bound should
  be fixed, provide `c(l, NA)` or `c(NA, u)`, respectively.

- y.title:

  (Character) Optional: Provide a title for the y-axis of the growth
  curve plot.

- x.title:

  (Character) Optional: Provide a title for the x-axis of both growth
  curve and derivative plots.

- lwd:

  (Numeric) Line width of the individual plots.

- plot:

  (Logical) Show the generated plot in the `Plots` pane (`TRUE`) or not
  (`FALSE`). If `FALSE`, a ggplot object is returned.

- export:

  (Logical) Export the generated plot as PDF and PNG files (`TRUE`) or
  not (`FALSE`).

- height:

  (Numeric) Height of the exported image in inches.

- width:

  (Numeric) Width of the exported image in inches.

- out.dir:

  (Character) Name or path to a folder in which the exported files are
  stored. If `NULL`, a "Plots" folder is created in the current working
  directory to store the files in.

- out.nm:

  (Character) The name of the PDF and PNG files if `export = TRUE`. If
  `NULL`, a name will be automatically generated including the chosen
  parameter.

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A plot matrix with all growth curves (raw measurements or nonparametric
fits) in a dataset, with replicates combined by the group averages (if
`mean = TRUE`) or not (`mean = FALSE`).

## Examples

``` r
# Create random growth data set
rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = "Test2")

rnd.data <- list()
rnd.data[["time"]] <- rbind(rnd.data1$time, rnd.data2$time)
rnd.data[["data"]] <- rbind(rnd.data1$data, rnd.data2$data)

# Run growth curve analysis workflow
res <- growth.workflow(time = rnd.data$time,
                       data = rnd.data$data,
                       fit.opt = "s",
                       ec50 = FALSE,
                       export.res = FALSE,
                       suppress.messages = TRUE,
                       parallelize = FALSE)


plot.grid(res, param = "mu.spline")

```
