# Fit various models to response vs. concentration data of a single sample to determine the EC50.

Fit various models to response vs. concentration data of a single sample
to determine the EC50.

## Usage

``` r
growth.drFitModel(conc, test, drID = "undefined", control = growth.control())
```

## Arguments

- conc:

  Vector of concentration values.

- test:

  Vector of response parameter values of the same length as `conc`.

- drID:

  (Character) The name of the analyzed condition

- control:

  A `grofit.control` object created with
  [`growth.control`](https://nicwir.github.io/QurvE/reference/growth.control.md),
  defining relevant fitting options.

## Value

A `drFitModel` object.

## References

Christian Ritz, Florent Baty, Jens C. Streibig, Daniel Gerhard (2015).
*Dose-Response Analysis Using R*. PLoS ONE 10(12): e0146021. DOI:
10.1371/journal.pone.0146021

## Examples

``` r
conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
response <- c(1/(1+exp(-0.7*(4-conc[-20])))+rnorm(19)/50, 0)

TestRun <- growth.drFitModel(conc, response, drID = 'test')

print(summary(TestRun))
#>        EC50.Estimate EC50.Std..Error EC50.Lower EC50.Upper     yEC50      test
#> e:1:50      4.064872      0.05576315   3.947222   4.182522 0.4702602 mu.linfit
#>        model
#> e:1:50  W1.3
plot(TestRun)
```
