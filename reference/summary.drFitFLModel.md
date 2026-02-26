# Generic summary function for drFitFLModel objects

Generic summary function for drFitFLModel objects

## Usage

``` r
# S3 method for class 'drFitFLModel'
summary(object, ...)
```

## Arguments

- object:

  object of class `drFitModel`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with biosensor response parameters.

## Examples

``` r
# Create concentration values via a serial dilution
conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)

# Simulate response values via biosensor equation
response <- biosensor.eq(conc, y.min = 110, y.max = 6000, K = 0.5, n = 2) +
            0.01*6000*rnorm(10)

# Perform fit
TestRun <- fl.drFitModel(conc, response, drID = 'test', control = fl.control())
#> 
#> 
#> === Dose response curve estimation ================
#> --- EC 50 -----------------------------------------
#> --> test
#> sensitivity: 0.503 | yEC50: 3042 | fold change: 85.01 | leakiness: 70.7
#> 
#> 

print(summary(TestRun))
#>      yEC50    y.min    y.max       fc         K        n yEC50.orig    K.orig
#> 1 3042.338 70.74402 6013.931 85.00975 0.5033856 1.974188   3042.338 0.5033856
#>   test
#> 1   NA
```
