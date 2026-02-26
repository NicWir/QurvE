# Generic summary function for drFitModel objects

Generic summary function for drFitModel objects

## Usage

``` r
# S3 method for class 'drFitModel'
summary(object, ...)
```

## Arguments

- object:

  object of class `drFitModel`

- ...:

  Additional arguments. This has currently no effect and is only meant
  to fulfill the requirements of a generic function.

## Value

A dataframe with parameters extracted from the dose-response analysis of
a single sample.

## Examples

``` r
conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
response <- c(1/(1+exp(-0.7*(4-conc[-20])))+rnorm(19)/50, 0)

TestRun <- growth.drFitModel(conc, response, drID = 'test')

print(summary(TestRun))
#>        EC50.Estimate EC50.Std..Error EC50.Lower EC50.Upper     yEC50      test
#> e:1:50      4.117371       0.1043342   3.897245   4.337497 0.4717212 mu.linfit
#>        model
#> e:1:50  W1.3
```
