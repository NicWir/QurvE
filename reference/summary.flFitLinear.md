# Generic summary function for flFitLinear objects

Generic summary function for flFitLinear objects

## Usage

``` r
# S3 method for class 'flFitLinear'
summary(object, ...)
```

## Arguments

- object:

  object of class `flFitLinear`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with parameters extracted from a linear fit.

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
TestFit <- flFitLinear(time = time,
                       fl_data = data,
                       ID = 'TestFit',
                       control = fl.control(fit.opt = 'l', x_type = 'time',
                       lin.R2 = 0.95, lin.RSD = 0.1,
                       lin.h = 20))

summary(TestFit)
#>   max_slope.linfit    lambda.linfit        dY.linfit         A.linfit
#> 1 233.480098034089 15.2207558251445 5820.56714471966 7503.25945241197
#>   x.mu.start.linfit x.mu.end.linfit       r2mu.linfit reliable_fit.linfit
#> 1              14.5            30.5 0.996370266149326                TRUE
#>   max_slope2.linfit x.mu2.start.linfit x.mu2.end.linfit r2mu2.linfit
#> 1              <NA>               <NA>             <NA>         <NA>
#>   reliable_fit2.linfit
#> 1                FALSE
```
