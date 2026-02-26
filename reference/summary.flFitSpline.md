# Generic summary function for flFitSpline objects

Generic summary function for flFitSpline objects

## Usage

``` r
# S3 method for class 'flFitSpline'
summary(object, ...)
```

## Arguments

- object:

  object of class `flFitSpline`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with parameters extracted from a nonparametric fit.

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
                       ID = 'TestFit',
                       control = fl.control(fit.opt = 's', x_type = 'time'))

summary(TestFit)
#>   max_slope.spline x.max.spline   lambda.spline max_slope2.spline x.max2.spline
#> 1 242.557704041626         23.5 14.982628746023              <NA>          <NA>
#>   lambda2.spline        y0.spline         A.spline        dY.spline
#> 1           <NA> 1519.80363776351 7592.80063751346 6072.99699974995
#>    integral.spline reliable_fit.spline reliable_fit2.spline smooth.spline
#> 1 151370.675832012                TRUE                FALSE          0.75
```
