# Generic plot function for `gcBootSpline` objects.

Generic plot function for `gcBootSpline` objects.

## Usage

``` r
# S3 method for class 'drBootSpline'
plot(
  x,
  pch = 19,
  colData = 1,
  colSpline = "black",
  cex.point = 1,
  cex.lab = 1.5,
  cex.axis = 1.3,
  lwd = 2,
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

  A `drBootSpline` object created with
  [`growth.drBootSpline`](https://nicwir.github.io/QurvE/reference/growth.drBootSpline.md)
  or stored within a `grofit` or `drFit` object created with
  [`growth.workflow`](https://nicwir.github.io/QurvE/reference/growth.workflow.md)
  or
  [`growth.drFit`](https://nicwir.github.io/QurvE/reference/growth.drFit.md),
  respectively.

- pch:

  (Numeric) Shape of the raw data symbols.

- colData:

  (Numeric or Character) Color used to plot the raw data.

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

  (Logical) Indicate whether both dose-response curves and parameter
  plots shall be shown within the same window.

- ...:

  Further arguments to refine the generated base R plot.

## Value

A plot with the all dose-response spline fits from the bootstrapping
operation.

## Examples

``` r
conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
response <- c(1/(1+exp(-0.7*(4-conc[-20])))+stats::rnorm(19)/50, 0)

TestRun <- growth.drBootSpline(conc, response, drID = "test",
        control = growth.control(log.x.dr = TRUE, smooth.dr = 0.8, nboot.dr = 50))
#> === Bootstrapping of dose response curve ==========
#> --- EC 50 -----------------------------------------
#> 
#> Mean  :  0.900225757061827 StDev :  0.108696709581542 
#> 90% CI:  0.896649635316594 90% CI:  0.903801878807059
#> 95% CI:  0.89596484604623 95% CI:  0.904486668077423
#> 
#> 
#> --- EC 50 in original scale -----------------------
#> 
#> Mean  :  1.46015844661168 
#> 90% CI:  1.45137633280738 90% CI:  1.46897202254679
#> 95% CI:  1.4496982312356 95% CI:  1.47066332712418
#> 

print(summary(TestRun))
#>   drboot.meanEC50 drboot.sdEC50 drboot.meanEC50y drboot.sdEC50y
#> 1       0.9002258     0.1086967        0.5336929      0.1068804
#>   drboot.ci90EC50.lo drboot.ci90EC50.up drboot.ci95EC50.lo drboot.ci95EC50.up
#> 1          0.7214197           1.079032          0.6871802           1.113271
#>   drboot.meanEC50.orig drboot.ci90EC50.orig.lo drboot.ci90EC50.orig.up
#> 1             1.460158                1.057352                 1.94183
#>   drboot.ci95EC50.orig.lo drboot.ci95EC50.orig.up
#> 1               0.9881016                2.044301
plot(TestRun, combine = TRUE)

```
