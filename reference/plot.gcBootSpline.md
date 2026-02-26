# Generic plot function for `gcBootSpline` objects.

Generic plot function for `gcBootSpline` objects.

## Usage

``` r
# S3 method for class 'gcBootSpline'
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

  object of class `gcBootSpline`, created with
  [`growth.gcBootSpline`](https://nicwir.github.io/QurvE/reference/growth.gcBootSpline.md).

- pch:

  (Numeric) Symbol used to plot data points.

- colData:

  (Numeric or character) Contour color of the raw data circles.

- deriv:

  (Logical) Show the derivatives (i.e., slope) over time in a secondary
  plot (`TRUE`) or not (`FALSE`).

- colSpline:

  (Numeric or character) Spline line colour.

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
  and upper (`u`) bounds on y-axis of the growth curve plot as a vector
  in the form `c(l, u)`. If only the lower or upper bound should be
  fixed, provide `c(l, NA)` or `c(NA, u)`, respectively.

- x.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on the x-axis of both growth curve and
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

  Further arguments to refine the generated base R plot.

## Value

A single plot with the all spline growth fits from the bootstrapping
operation and statistical distribution of growth parameters if
`combine = TRUE` or separate plots for growth fits and parameter
distributions (if `combine = FALSE`).

## Examples

``` r
# Create random growth dataset
rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")

# Extract time and growth data for single sample
time <- rnd.dataset$time[1,]
data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns

# Introduce some noise into the measurements
data <- data + stats::runif(97, -0.01, 0.09)

# Perform bootstrapping spline fit
TestFit <- growth.gcBootSpline(time, data, gcID = "TestFit",
              control = growth.control(fit.opt = "s", nboot.gc = 50))

plot(TestFit, combine = TRUE, lwd = 0.5)
```
