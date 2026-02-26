# Generic summary function for gcBootSpline objects

Generic summary function for gcBootSpline objects

## Usage

``` r
# S3 method for class 'gcBootSpline'
summary(object, ...)
```

## Arguments

- object:

  object of class `gcBootSpline`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with statistical parameters extracted from the spline fit
bootstrapping computation.

## Examples

``` r
# Create random growth dataset
rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')

# Extract time and growth data for single sample
time <- rnd.dataset$time[1,]
data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns

# Introduce some noise into the measurements
data <- data + stats::runif(97, -0.01, 0.09)

# Perform bootstrapping spline fit
TestFit <- growth.gcBootSpline(time, data, gcID = 'TestFit',
              control = growth.control(fit.opt = 's', nboot.gc = 50))

summary(TestFit)
#>               mu.bt        lambda.bt             A.bt            dY.bt
#> 1 0.647715675812015 4.59115101633482 6.05618267059454 5.99321509988996
#>        integral.bt           stdmu.bt      stdlambda.bt            stdA.bt
#> 1 73.9419682739723 0.0154483784308979 0.150342273280296 0.0231571200280006
#>             stddY.bt   stdintegral.bt reliable_fit.bt     ci90.mu.bt.lo
#> 1 0.0239623292598788 1.16011667706715            TRUE 0.622303093293188
#>       ci90.mu.bt.up ci90.lambda.bt.lo ci90.lambda.bt.up     ci90.A.bt.lo
#> 1 0.673128258330842  4.34383797678873  4.83846405588091 6.01808920814848
#>      ci90.A.bt.up ci90.integral.bt.lo ci90.integral.bt.up     ci95.mu.bt.lo
#> 1 6.0942761330406    72.0335763401968    75.8503602077477 0.617436854087455
#>       ci95.mu.bt.up ci95.lambda.bt.lo ci95.lambda.bt.up     ci95.A.bt.lo
#> 1 0.677994497536575  4.29648016070544   4.8858218719642 6.01079471533966
#>       ci95.A.bt.up ci95.integral.bt.lo ci95.integral.bt.up
#> 1 6.10157062584942    71.6681395869206    76.2157969610239
```
