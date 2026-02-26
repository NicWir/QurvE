# Generic plot function for `drFitModel` objects.

Generic plot function for `drFitModel` objects.

## Usage

``` r
# S3 method for class 'drFitModel'
plot(
  x,
  type = c("confidence", "all", "bars", "none", "obs", "average"),
  ec50line = TRUE,
  add = FALSE,
  broken = TRUE,
  bp,
  gridsize = 200,
  log = "x",
  n.xbreaks,
  n.ybreaks,
  x.lim,
  y.lim,
  pch = 1,
  cex.point,
  cex.axis = 1,
  cex.lab = 1.3,
  col = 1,
  lwd = 2,
  lty = 2,
  xlab,
  ylab,
  legend = TRUE,
  legendText,
  legendPos,
  cex.legend = NULL,
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

  object of class `drFitModel`, created with
  [`growth.drFitModel`](https://nicwir.github.io/QurvE/reference/growth.drFitModel.md).

- type:

  (Character) Specify how to plot the data. There are currently 5
  options: "average" (averages and fitted curve(s); default), "none"
  (only the fitted curve(s)), "obs" (only the data points), "all" (all
  data points and fitted curve(s)), "bars" (averages and fitted curve(s)
  with model-based standard errors (see Details)), and "confidence"
  (confidence bands for fitted curve(s)).

- ec50line:

  (Logical) Show pointed horizontal and vertical lines at the EC50
  values (`TRUE`) or not (`FALSE`).

- add:

  (Logical) If `TRUE` then add to already existing plot.

- broken:

  (Logical) If TRUE the x axis is broken provided this axis is
  logarithmic (using functionality in the CRAN package 'plotrix').

- bp:

  (Numeric) Specifying the break point below which the dose is zero (the
  amount of stretching on the dose axis above zero in order to create
  the visual illusion of a logarithmic scale including 0). The default
  is the base-10 value corresponding to the rounded value of the minimum
  of the log10 values of all positive dose values. This argument is only
  working for logarithmic dose axes.

- gridsize:

  (Numeric) Number of points in the grid used for plotting the fitted
  curves.

- log:

  (Character) String which contains '"x"' if the x axis is to be
  logarithmic, '"y"' if the y axis is to be logarithmic and '"xy"' or
  '"yx"' if both axes are to be logarithmic. The default is "x". The
  empty string "" yields the original axes.

- n.xbreaks:

  (Numeric) Number of breaks on the x-axis (if not log-transformed). The
  breaks are generated using `pretty`. Thus, the final number of breaks
  can deviate from the user input.

- n.ybreaks:

  (Numeric) Number of breaks on the y-axis (if not log-transformed). The
  breaks are generated using `pretty`. Thus, the final number of breaks
  can deviate from the user input.

- x.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on the x-axis of both growth curve and
  derivative plots as a vector in the form `c(l, u)`. If only the lower
  or upper bound should be fixed, provide `c(l, NA)` or `c(NA, u)`,
  respectively.

- y.lim:

  (Numeric vector with two elements) Optional: Provide the lower (`l`)
  and upper (`u`) bounds on y-axis of the growth curve plot as a vector
  in the form `c(l, u)`. If only the lower or upper bound should be
  fixed, provide `c(l, NA)` or `c(NA, u)`, respectively.

- pch:

  (Numeric) Symbol used to plot data points.

- cex.point:

  (Numeric) Size of the raw data points.

- cex.axis:

  (Numeric) Font size of axis annotations.

- cex.lab:

  (Numeric) Font size of axis titles.

- col:

  (Logical or a vector of colors) If `TRUE` default colours are used. If
  `FALSE` (default) no colors are used.

- lwd:

  (Numeric) Line width.

- lty:

  (Numeric) Specify the line type.

- xlab:

  (Character) An optional label for the x axis.

- ylab:

  (Character) An optional label for the y axis.

- legend:

  (Logical) If `TRUE` a legend is displayed.

- legendText:

  (Character) Specify the legend text (the position of the upper right
  corner of the legend box).

- legendPos:

  (Numeric) Vector of length 2 giving the position of the legend.

- cex.legend:

  numeric specifying the legend text size.

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

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A plot with the dose-response model fit.

## References

Christian Ritz, Florent Baty, Jens C. Streibig, Daniel Gerhard (2015).
*Dose-Response Analysis Using R*. PLoS ONE 10(12): e0146021. DOI:
10.1371/journal.pone.0146021

## Examples

``` r
conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
response <- c(1/(1+exp(-0.7*(4-conc[-20])))+stats::rnorm(19)/50, 0)

TestRun <- growth.drFitModel(conc, response, drID = "test")

print(summary(TestRun))
#>        EC50.Estimate EC50.Std..Error EC50.Lower EC50.Upper     yEC50      test
#> e:1:50      3.915821        0.086078   3.734212   4.097429 0.4721521 mu.linfit
#>        model
#> e:1:50  W1.3
plot(TestRun)
```
