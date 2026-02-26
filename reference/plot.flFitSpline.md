# Generic plot function for `flFitSpline` objects.

`plot.flFitSpline` generates the spline fit plot for a single sample.

## Usage

``` r
# S3 method for class 'flFitSpline'
plot(
  x,
  add = FALSE,
  raw = TRUE,
  slope = TRUE,
  deriv = TRUE,
  spline = TRUE,
  log.y = FALSE,
  basesize = 16,
  pch = 1,
  colData = 1,
  colSpline = "dodgerblue3",
  cex.point = 2,
  lwd = 0.7,
  y.lim = NULL,
  x.lim = NULL,
  y.lim.deriv = NULL,
  n.ybreaks = 6,
  y.title = NULL,
  x.title = NULL,
  y.title.deriv = NULL,
  plot = TRUE,
  export = FALSE,
  width = 8,
  height = ifelse(deriv == TRUE, 8, 6),
  out.dir = NULL,
  ...
)
```

## Arguments

- x:

  Object of class `flFitSpline`, created with
  [`flFitSpline`](https://nicwir.github.io/QurvE/reference/flFitSpline.md).

- add:

  (Logical) Shall the fitted spline be added to an existing plot? `TRUE`
  is used internally by
  [`plot.flBootSpline`](https://nicwir.github.io/QurvE/reference/plot.flBootSpline.md).

- raw:

  (Logical) Display raw growth as circles (`TRUE`) or not (`FALSE`).

- slope:

  (Logical) Show the slope at the maximum slope (`TRUE`) or not
  (`FALSE`).

- deriv:

  (Logical) Show the derivative (i.e., slope) over time in a secondary
  plot (`TRUE`) or not (`FALSE`).

- spline:

  (Logical) Only for `add = TRUE`: add the current spline to the
  existing plot (`FALSE`).

- log.y:

  (Logical) Log-transform the y-axis (`TRUE`) or not (`FALSE`).

- basesize:

  (Numeric) Base font size.

- pch:

  (Numeric) Symbol used to plot data points.

- colData:

  (Numeric or character) Contour color of the raw data circles.

- colSpline:

  (Numeric or character) Spline line colour.

- cex.point:

  (Numeric) Size of the raw data points.

- lwd:

  (Numeric) Spline line width.

- y.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on y-axis of the fluorescence curve plot as a
  vector in the form `c(l, u)`. If only the lower or upper bound should
  be fixed, provide `c(l, NA)` or `c(NA, u)`, respectively.

- x.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on the x-axis of both fluorescence curve and
  derivative plots as a vector in the form `c(l, u)`. If only the lower
  or upper bound should be fixed, provide `c(l, NA)` or `c(NA, u)`,
  respectively.

- y.lim.deriv:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on the y-axis of the derivative plot as a
  vector in the form `c(l, u)`. If only the lower or upper bound should
  be fixed, provide `c(l, NA)` or `c(NA, u)`, respectively.

- n.ybreaks:

  (Numeric) Number of breaks on the y-axis. The breaks are generated
  using [`axisTicks()`](https://rdrr.io/r/grDevices/axisTicks.html).
  Thus, the final number of breaks can deviate from the user input.

- y.title:

  (Character) Optional: Provide a title for the y-axis of the growth
  curve plot.

- x.title:

  (Character) Optional: Provide a title for the x-axis of both growth
  curve and derivative plots.

- y.title.deriv:

  (Character) Optional: Provide a title for the y-axis of the derivative
  plot.

- plot:

  (Logical) Show the generated plot in the `Plots` pane (`TRUE`) or not
  (`FALSE`). If `FALSE`, a ggplot object is returned.

- export:

  (Logical) Export the generated plot as PDF and PNG files (`TRUE`) or
  not (`FALSE`).

- width:

  (Numeric) Width of the exported image in inches.

- height:

  (Numeric) Height of the exported image in inches.

- out.dir:

  (Character) Name or path to a folder in which the exported files are
  stored. If `NULL`, a "Plots" folder is created in the current working
  directory to store the files in.

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A plot with the nonparametric fit.

## Examples

``` r
# load example dataset
input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
                   data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
                   csvsep = "\t",
                   csvsep.fl = "\t")
#> Sample data are stored in columns. If they are stored in row format, please run read_data() with data.format = 'row'.

# Extract time and normalized fluorescence data for single sample
time <- input$time[4,]
data <- input$norm.fluorescence[4,-(1:3)] # Remove identifier columns

# Perform linear fit
TestFit <- flFitSpline(time = time,
                       fl_data = data,
                       ID = "TestFit",
                       control = fl.control(fit.opt = "s", x_type = "time"))

plot(TestFit)
```
