# Generic summary function for drFitSpline objects

Generic summary function for drFitSpline objects

## Usage

``` r
# S3 method for class 'drFitSpline'
summary(object, ...)
```

## Arguments

- object:

  object of class `drFitSpline`

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

TestRun <- growth.drFitSpline(conc, response, drID = 'test',
              control = growth.control(log.x.dr = TRUE, smooth.dr = 0.8))
#> 
#> 
#> === Dose response curve estimation ================
#> --- EC 50 -----------------------------------------
#> --> test
#> xEC50 1.65140335103632 yEC50 0.478328816619607
#> --> Original scale 
#> xEC50 4.21429217741346 yEC50 0.478328816619607
#> 
#> 

print(summary(TestRun))
#>       EC50     yEC50 EC50.orig yEC50.orig test
#> 1 1.651403 0.4783288  4.214292  0.4783288   NA
```
