# Compare growth parameters between samples or conditions

`plot.parameter` gathers physiological parameters from the results of a
growth fit analysis and compares a chosen parameter between each sample
or condition in a column plot. Error bars represent the 95% confidence
interval (only shown for \> 2 replicates).

## Usage

``` r
# S3 method for class 'parameter'
plot(
  x,
  param = c("mu.linfit", "lambda.linfit", "dY.linfit", "A.linfit", "mu2.linfit",
    "lambda2.linfit", "mu.model", "lambda.model", "A.model", "A.orig.model", "dY.model",
    "dY.orig.model", "tD.linfit", "tD2.linfit", "tD.spline", "tD2.spline", "mu.spline",
    "lambda.spline", "A.spline", "dY.spline", "integral.spline", "mu2.spline",
    "lambda2.spline", "mu.bt", "lambda.bt", "A.bt", "integral.bt", "max_slope.linfit",
    "max_slope.spline"),
  IDs = NULL,
  names = NULL,
  conc = NULL,
  exclude.nm = NULL,
  exclude.conc = NULL,
  reference.nm = NULL,
  reference.conc = NULL,
  order_by_conc = FALSE,
  colors = NULL,
  basesize = 12,
  label.size = NULL,
  shape.size = 2.5,
  legend.position = "right",
  legend.ncol = 1,
  plot = TRUE,
  export = FALSE,
  height = 7,
  width = NULL,
  out.dir = NULL,
  out.nm = NULL,
  ...
)
```

## Arguments

- x:

  A `grofit`, `gcFit`, or `gcTable` object obtained with
  [`growth.workflow`](https://nicwir.github.io/QurvE/reference/growth.workflow.md)
  or
  [`growth.gcFit`](https://nicwir.github.io/QurvE/reference/growth.gcFit.md).

- param:

  (Character) The parameter used to compare different sample groups. Any
  name of a column containing numeric values in `gcTable` (which is
  stored within `grofit` or `gcFit` objects) can be used as input.
  Useful options are: 'mu.linfit', 'lambda.linfit', 'dY.linfit',
  'A.linfit', 'mu.model', 'lambda.model', 'A.model', 'mu.spline',
  'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu.bt',
  'lambda.bt', 'A.bt', 'integral.bt'

- IDs:

  (String or vector of strings) Define samples or groups (if
  `mean = TRUE`) to combine into a single plot based on exact matches
  with entries in the `label` or `condition` columns of
  `grofit$expdesign`.

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

- reference.nm:

  (Character) Name of the reference condition, to which parameter values
  are normalized. Partially matching strings are tolerated as long as
  they can uniquely identify the condition.

- reference.conc:

  (Numeric) Concentration of the reference condition, to which parameter
  values are normalized.

- order_by_conc:

  (Logical) Shall the columns be sorted in order of ascending
  concentrations (`TRUE`) or by sample groups `FALSE`?

- colors:

  (vector of strings) Define a color palette used to draw the columns.
  If `NULL`, default palettes are chosen. Note: The number of provided
  colors should at least match the number of groups.

- basesize:

  (Numeric) Base font size.

- label.size:

  (Numeric) Font size for sample labels below x-axis.

- shape.size:

  (Numeric) The size of the symbols indicating replicate values.
  Default: 2.5

- legend.position:

  (Character) Position of the legend. One of "bottom", "top", "left",
  "right".

- legend.ncol:

  (Numeric) Number of columns in the legend.

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

A column plot comparing a selected growth parameter between tested
conditions.

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
                       parallelize = FALSE,
                       suppress.messages = TRUE)


plot.parameter(res,
               param = "mu.spline",
               legend.ncol = 4,
               legend.position = "bottom",
               basesize = 15,
               label.size = 11)

```
