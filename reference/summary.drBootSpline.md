# Generic summary function for drBootSpline objects

Generic summary function for drBootSpline objects

## Usage

``` r
# S3 method for class 'drBootSpline'
summary(object, ...)
```

## Arguments

- object:

  object of class `drBootSpline`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with statistical parameters extracted from the dose-response
bootstrapping analysis.

## Examples

``` r
conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
response <- c(1/(1+exp(-0.7*(4-conc[-20])))+stats::rnorm(19)/50, 0)

TestRun <- growth.drBootSpline(conc, response, drID = 'test',
        control = growth.control(log.x.dr = TRUE, smooth.dr = 0.8, nboot.dr = 50))
#> === Bootstrapping of dose response curve ==========
#> --- EC 50 -----------------------------------------
#> 
#> Mean  :  0.912745214292531 StDev :  0.107495490567349 
#> 90% CI:  0.909208612652865 90% CI:  0.916281815932197
#> 95% CI:  0.908531391062291 95% CI:  0.916959037522771
#> 
#> 
#> --- EC 50 in original scale -----------------------
#> 
#> Mean  :  1.49115190086037 
#> 90% CI:  1.48235724971858 90% CI:  1.49997771024447
#> 95% CI:  1.48067671290569 95% CI:  1.50167132253648
#> 

print(summary(TestRun))
#>   drboot.meanEC50 drboot.sdEC50 drboot.meanEC50y drboot.sdEC50y
#> 1       0.9127452     0.1074955        0.5292214      0.1052089
#>   drboot.ci90EC50.lo drboot.ci90EC50.up drboot.ci95EC50.lo drboot.ci95EC50.up
#> 1          0.7359151           1.089575          0.7020541           1.123436
#>   drboot.meanEC50.orig drboot.ci90EC50.orig.lo drboot.ci90EC50.orig.up
#> 1             1.491152                1.087391                1.973011
#>   drboot.ci95EC50.orig.lo drboot.ci95EC50.orig.up
#> 1                1.017893                2.075404
```
