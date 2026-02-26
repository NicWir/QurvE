# Compare fluorescence and growth over time

`plot.dual` creates a two-panel plot in which fluorescence or growth
values are shown over time, allowing for the identification of, e.g.,
expression patterns in different growth stages.

## Usage

``` r
# S3 method for class 'dual'
plot(
  x,
  fluorescence = c("fl", "norm.fl"),
  IDs = NULL,
  names = NULL,
  conc = NULL,
  mean = TRUE,
  exclude.nm = NULL,
  exclude.conc = NULL,
  log.y.growth = FALSE,
  log.y.fl = FALSE,
  n.ybreaks = 6,
  colors = NULL,
  color_groups = TRUE,
  group_pals = c("Green", "Orange", "Purple", "Magenta", "Grey", "Blue", "Grey", "Red",
    "Cyan", "Brown", "Mint"),
  basesize = 20,
  y.lim.growth = NULL,
  y.lim.fl = NULL,
  x.lim = NULL,
  x.title = NULL,
  y.title.growth = NULL,
  y.title.fl = NULL,
  lwd = 1.1,
  legend.position = "bottom",
  legend.ncol = 2,
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

  A `flFit`, `flFitRes`, or `grodata` object created with
  [`flFit`](https://nicwir.github.io/QurvE/reference/flFit.md),
  [`fl.workflow`](https://nicwir.github.io/QurvE/reference/fl.workflow.md)
  or
  [`read_data`](https://nicwir.github.io/QurvE/reference/read_data.md)

- fluorescence:

  (Character) Indicate, which type of fluorescence data should be
  displayed.

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

- mean:

  (Logical) Display the mean and standard deviation of groups with
  replicates (`TRUE`) or plot each sample individually (`FALSE`)?

- exclude.nm:

  (String or vector of strings) Define groups to exclude from the plot.
  Partial matches with sample/group names are accepted.

- exclude.conc:

  (Numeric or numeric vector) Define concentrations to exclude from the
  plot.

- log.y.growth:

  (Logical) Log-transform the y-axis of the growth plot (`TRUE`) or not
  (`FALSE`)?

- log.y.fl:

  (Logical) Log-transform the y-axis of the fluorescence plot (`TRUE`)
  or not (`FALSE`)?

- n.ybreaks:

  (Numeric) Number of breaks on the y-axis. The breaks are generated
  using
  [`scales::pretty_breaks`](https://scales.r-lib.org/reference/pretty_breaks.html).
  Thus, the final number of breaks can deviate from the user input.

- colors:

  (vector of strings) Define a color palette used to draw the plots. If
  `NULL`, default palettes are chosen based on the number of
  groups/samples within the plot. Note: The number of provided colors
  should at least match the number of groups/samples.

- color_groups:

  (Logical) Shall samples within the same group but with different
  concentrations be shown in different shades of the same color?

- group_pals:

  (String vector) Define the colors used to display sample groups with
  identical concentrations. The number of selected color palettes must
  be at least the number of displayed groups. The order of the chosen
  palettes corresponds to the oder of conditions in the legend.
  Available options: "Green", "Oranges", "Purple", "Cyan", "Grey",
  "Red", "Blue", and "Magenta".

- basesize:

  (Numeric) Base font size.

- y.lim.growth:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds of the y-axis of the growth plot as a vector in
  the form `c(l, u)`. If only the lower or upper bound should be fixed,
  provide `c(l, NA)` or `c(NA, u)`, respectively.

- y.lim.fl:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds of the y-axis of the fluorescence plot as a
  vector in the form `c(l, u)`.

- x.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds of the x-axis of both fluorescence and growth
  plots as a vector in the form `c(l, u)`. If only the lower or upper
  bound should be fixed, provide `c(l, NA)` or `c(NA, u)`, respectively.

- x.title:

  (Character) Optional: Provide a title for the x-axis of both growth
  curve and derivative plots.

- y.title.growth:

  (Character) Optional: Provide a title for the y-axis of the growth
  plot.

- y.title.fl:

  (Character) Optional: Provide a title for the y-axis of the
  fluorescence plot.

- lwd:

  (Numeric) Line width of the individual plots.

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

A two-panel plot, showing raw fluorescence (`fluorescence = "fl"`) or
normalized fluorescence (`fluorescence = "norm.fl"`) over time in the
top panel, and growth over time in the bottom panel.

## Examples

``` r
# load example dataset
input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
                   data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
                   csvsep = "\t",
                   csvsep.fl = "\t")
#> Sample data are stored in columns. If they are stored in row format, please run read_data() with data.format = 'row'.

# Run workflow
res <- fl.workflow(grodata = input, ec50 = FALSE, fit.opt = "s",
                   x_type = "time", norm_fl = TRUE,
                   dr.parameter = "max_slope.spline",
                   suppress.messages = TRUE,
                   parallelize = FALSE)

plot.dual(res, legend.ncol = 3, basesize = 15)


```
