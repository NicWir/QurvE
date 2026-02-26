# Generic summary function for gcFitLinear objects

Generic summary function for gcFitLinear objects

## Usage

``` r
# S3 method for class 'gcFitLinear'
summary(object, ...)
```

## Arguments

- object:

  object of class `gcFitLinear`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with parameters extracted from the linear fit.

## Examples

``` r
# Create random growth dataset
rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')

# Extract time and growth data for single sample
time <- rnd.dataset$time[1,]
data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns

# Perform linear fit
TestFit <- growth.gcFitLinear(time, data, gcID = 'TestFit',
                 control = growth.control(fit.opt = 'l'))

summary(TestFit)
#>           mu.linfit        tD.linfit    lambda.linfit        dY.linfit
#> 1 0.648250132070668 1.06925883431117 4.31024326888779 5.92637359616997
#>           A.linfit tmu.start.linfit tmu.end.linfit       r2mu.linfit
#> 1 5.97392828303011             6.25             10 0.999486824226406
#>   reliable_fit.linfit mu2.linfit tD2.linfit tmu2.start.linfit tmu2.end.linfit
#> 1                TRUE       <NA>       <NA>              <NA>            <NA>
#>   r2mu2.linfit reliable_fit2.linfit
#> 1         <NA>                FALSE
```
