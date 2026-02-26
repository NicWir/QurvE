# Generic plot function for `drFitSpline` objects.

`plot.drFitSpline` generates the spline fit plot for response-parameter
vs. concentration data

## Usage

``` r
# S3 method for class 'drFitSpline'
plot(
  x,
  add = FALSE,
  ec50line = TRUE,
  log = "",
  pch = 16,
  colSpline = 1,
  colData = 1,
  cex.point = 1,
  cex.lab = 1.5,
  cex.axis = 1.3,
  y.lim = NULL,
  x.lim = NULL,
  y.title = NULL,
  x.title = NULL,
  lwd = 2,
  plot = TRUE,
  export = FALSE,
  height = 7,
  width = 9,
  out.dir = NULL,
  ...
)
```

## Arguments

- x:

  object of class `drFitSpline`, created with
  [`growth.drFitSpline`](https://nicwir.github.io/QurvE/reference/growth.drFitSpline.md).

- add:

  (Logical) Shall the fitted spline be added to an existing plot? `TRUE`
  is used internally by
  [`plot.drBootSpline`](https://nicwir.github.io/QurvE/reference/plot.drBootSpline.md).

- ec50line:

  (Logical) Show pointed horizontal and vertical lines at the EC50 value
  (`TRUE`) or not (`FALSE`).

- log:

  ("x", "y", or "xy") Display the x- or y-axis on a logarithmic scale.

- pch:

  (Numeric) Shape of the raw data symbols.

- colSpline:

  (Numeric or character) Spline line colour.

- colData:

  (Numeric or character) Contour color of the raw data circles.

- cex.point:

  (Numeric) Size of the raw data symbols.

- cex.lab:

  (Numeric) Font size of axis titles.

- cex.axis:

  (Numeric) Font size of axis annotations.

- y.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on the y-axis as a vector in the form
  `c(l, u)`. If only the lower or upper bound should be fixed, provide
  `c(l, NA)` or `c(NA, u)`, respectively.

- x.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on the x-axis as a vector in the form
  `c(l, u)`. If only the lower or upper bound should be fixed, provide
  `c(l, NA)` or `c(NA, u)`, respectively.

- y.title:

  (Character) Optional: Provide a title for the y-axis.

- x.title:

  (Character) Optional: Provide a title for the x-axis.

- lwd:

  (Numeric) Line width of spline.

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

A plot with the nonparametric dose-response fit.

## Examples

``` r
conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
response <- c(1/(1+exp(-0.7*(4-conc[-20])))+stats::rnorm(19)/50, 0)

TestRun <- growth.drFitSpline(conc, response, drID = "test",
                     control = growth.control(log.x.dr = TRUE, smooth.dr = 0.8))
#> 
#> 
#> === Dose response curve estimation ================
#> --- EC 50 -----------------------------------------
#> --> test
#> xEC50 1.63700157762612 yEC50 0.469191716295588
#> --> Original scale 
#> xEC50 4.13973528735413 yEC50 0.469191716295588
#> 
#> 

print(summary(TestRun))
#>       EC50     yEC50 EC50.orig yEC50.orig test
#> 1 1.637002 0.4691917  4.139735  0.4691917   NA
plot(TestRun)
```
