# Generic plot function for `gcFittedLinear` objects. Plot the results of a linear regression on ln-transformed data

`plot.gcFitLinear` shows the results of a linear regression on
log-transformed data and visualizes raw data, data points included in
the fit, the tangent obtained by linear regression, and the lag time.

## Usage

``` r
# S3 method for class 'gcFitLinear'
plot(
  x,
  log = "y",
  which = c("fit", "diagnostics", "fit_diagnostics"),
  pch = 21,
  cex.point = 1,
  cex.lab = 1.5,
  cex.axis = 1.3,
  lwd = 2,
  color = "firebrick3",
  y.lim = NULL,
  x.lim = NULL,
  plot = TRUE,
  export = FALSE,
  height = ifelse(which == "fit", 7, 5),
  width = ifelse(which == "fit", 9, 9),
  out.dir = NULL,
  ...
)
```

## Arguments

- x:

  A `gcFittedLinear` object created with
  [`growth.gcFitLinear`](https://nicwir.github.io/QurvE/reference/growth.gcFitLinear.md)
  or stored within a `grofit` or `gcFit` object created with
  [`growth.workflow`](https://nicwir.github.io/QurvE/reference/growth.workflow.md)
  or
  [`growth.gcFit`](https://nicwir.github.io/QurvE/reference/growth.gcFit.md),
  respectively.

- log:

  ("x" or "y") Display the x- or y-axis on a logarithmic scale.

- which:

  ("fit" or "diagnostics") Display either the results of the linear fit
  on the raw data or statistical evaluation of the linear regression.

- pch:

  (Numeric) Shape of the raw data symbols.

- cex.point:

  (Numeric) Size of the raw data points.

- cex.lab:

  (Numeric) Font size of axis titles.

- cex.axis:

  (Numeric) Font size of axis annotations.

- lwd:

  (Numeric) Line width.

- color:

  (Character string) Enter color either by name (e.g., red, blue,
  coral3) or via their hexadecimal code (e.g., \#AE4371, \#CCFF00FF,
  \#0066FFFF). A full list of colors available by name can be found at
  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

- y.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on y-axis as a vector in the form `c(l, u)`.

- x.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on the x-axis as a vector in the form
  `c(l, u)`.

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

- ...:

  Further arguments to refine the generated base R plot.

## Value

A plot with the linear fit.

## Examples

``` r
# Create random growth dataset
rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")

# Extract time and growth data for single sample
time <- rnd.dataset$time[1,]
data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns

# Perform linear fit
TestFit <- growth.gcFitLinear(time, data, gcID = "TestFit",
                 control = growth.control(fit.opt = "l"))

plot(TestFit)

```
