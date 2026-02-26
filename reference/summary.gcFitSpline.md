# Generic summary function for gcFitSpline objects

Generic summary function for gcFitSpline objects

## Usage

``` r
# S3 method for class 'gcFitSpline'
summary(object, ...)
```

## Arguments

- object:

  object of class `gcFitSpline`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with parameters extracted from the nonparametric fit.

## Examples

``` r
# Create random growth dataset
rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')

# Extract time and growth data for single sample
time <- rnd.dataset$time[1,]
data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns

# Perform linear fit
TestFit <- growth.gcFitSpline(time, data, gcID = 'TestFit',
                 control = growth.control(fit.opt = 's'))

summary(TestFit)
#>           mu.spline        tD.spline tmax.spline   lambda.spline mu2.spline
#> 1 0.656948871637361 1.05510064859745        7.75 4.1651049339715       <NA>
#>   tD2.spline tmax2.spline lambda2.spline         y0.spline         A.spline
#> 1       <NA>         <NA>           <NA> 0.054831557454678 6.35761944876055
#>          dY.spline  integral.spline reliable_fit.spline reliable_fit2.spline
#> 1 6.30278789130588 76.8022171308121                TRUE                FALSE
#>   smooth.spline
#> 1          0.55
```
