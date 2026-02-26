# Generic summary function for flBootSpline objects

Generic summary function for flBootSpline objects

## Usage

``` r
# S3 method for class 'flBootSpline'
summary(object, ...)
```

## Arguments

- object:

  object of class `flBootSpline`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with statistical parameters extracted from a dose-response
bootstrapping analysis.

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
                       ID = 'TestFit',
                       control = fl.control(fit.opt = 's', x_type = 'time',
                       nboot.fl = 50))

summary(TestFit)
#>       max_slope.bt        lambda.bt             A.bt            dY.bt
#> 1 243.222849803839 15.0844185221378 7533.33845404535 5997.09832349666
#>        integral.bt  stdmax_slope.bt      stdlambda.bt          stdA.bt
#> 1 147922.089459739 3.04256226424955 0.329855186242363 103.396396261667
#>           stddY.bt   stdintegral.bt reliable_fit.bt    ci90.mu.bt.lo
#> 1 128.434255088156 5101.19667599421            TRUE 238.217834879149
#>     ci90.mu.bt.up ci90.lambda.bt.lo ci90.lambda.bt.up    ci90.A.bt.lo
#> 1 248.22786472853  14.5418067407692  15.6270303035065 7363.2513821949
#>       ci90.A.bt.up ci90.integral.bt.lo ci90.integral.bt.up   ci95.mu.bt.lo
#> 1 7703.42552589579    139530.620927728    156313.557991749 237.25942776591
#>      ci95.mu.bt.up ci95.lambda.bt.lo ci95.lambda.bt.up     ci95.A.bt.lo
#> 1 249.186271841768  14.4379023571028  15.7309346871729 7330.68151737248
#>       ci95.A.bt.up ci95.integral.bt.lo ci95.integral.bt.up
#> 1 7735.99539071821     137923.74397479    157920.434944688
```
