# Generic plot function for `flBootSpline` objects.

Generic plot function for `flBootSpline` objects.

## Usage

``` r
# S3 method for class 'flBootSpline'
plot(
  x,
  pch = 1,
  colData = 1,
  deriv = TRUE,
  colSpline = "dodgerblue3",
  cex.point = 1,
  cex.lab = 1.5,
  cex.axis = 1.3,
  lwd = 2,
  y.lim = NULL,
  x.lim = NULL,
  y.lim.deriv = NULL,
  plot = TRUE,
  export = FALSE,
  height = 7,
  width = 9,
  out.dir = NULL,
  combine = FALSE,
  ...
)
```

## Arguments

- x:

  Object of class `flBootSpline`, created with
  [`flBootSpline`](https://nicwir.github.io/QurvE/reference/flBootSpline.md).

- pch:

  (Numeric) Size of the raw data circles.

- colData:

  (Numeric or Character) Color used to plot the raw data.

- deriv:

  (Logical) Show the derivatives (i.e., slope) over time in a secondary
  plot (`TRUE`) or not (`FALSE`).

- colSpline:

  (Numeric or Character) Color used to plot the splines.

- cex.point:

  (Numeric) Size of the raw data points.

- cex.lab:

  (Numeric) Font size of axis titles.

- cex.axis:

  (Numeric) Font size of axis annotations.

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

- plot:

  (Logical) Show the generated plot in the `Plots` pane (`TRUE`) or not
  (`FALSE`).

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

- combine:

  (Logical) Indicate whether both growth curves and parameter plots
  shall be shown within the same window.

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A single plot with the all spline fits from the bootstrapping operation
and statistical distribution of parameters if `combine = TRUE` or
separate plots for fits and parameter distributions (if
`combine = FALSE`).

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
TestFit <- flBootSpline(time = time,
                       fl_data = data,
                       ID = "TestFit",
                       control = fl.control(fit.opt = "s", x_type = "time",
                       nboot.fl = 50))

plot(TestFit, combine = TRUE, lwd = 0.5)
```
