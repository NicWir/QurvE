# Generic summary function for drFitfl objects

Generic summary function for drFitfl objects

## Usage

``` r
# S3 method for class 'drFitfl'
summary(object, ...)
```

## Arguments

- object:

  object of class `drFitfl`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with parameters for all samples extracted from a
dose-response analysis.

## Examples

``` r
# load example dataset
input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
                   data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
                   csvsep = "\t",
                   csvsep.fl = "\t")
#> Sample data are stored in columns. If they are stored in row format, please run read_data() with data.format = 'row'.

# Define fit controls
control <- fl.control(fit.opt = 's',
             x_type = 'time', norm_fl = TRUE,
             dr.parameter = 'max_slope.spline',
             dr.method = 'model',
             suppress.messages = TRUE)

# Run curve fitting workflow
res <- flFit(fl_data = input$norm.fluorescence,
             time = input$time,
             parallelize = FALSE,
             control = control)

# Perform dose-response analysis with biosensor model
drFitfl <- fl.drFit(flTable = res$flTable, control = control)

summary(drFitfl)
#>             Test log.x log.y    yEC50    y.min    y.max        fc          K
#> 1   pSEVA634-GFP FALSE FALSE 446.4529 92.26689 800.6389  8.677423 0.03285860
#> 2  pSEVA634r-GFP FALSE FALSE 190.4523 30.77274 350.1318 11.377986 0.04563574
#> 3 pSEVA634rk-GFP FALSE FALSE 205.9583 64.17578 347.7407  5.418566 0.01018010
#>           n yEC50.orig     K.orig             test
#> 1 1.1757158   446.4529 0.03285860 max_slope.spline
#> 2 2.7157705   190.4523 0.04563574 max_slope.spline
#> 3 0.9764852   205.9583 0.01018010 max_slope.spline
```
