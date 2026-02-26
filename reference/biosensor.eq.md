# Internal function used to fit a biosensor response model with [`nlsLM`](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html)

Calculates the values of biosensor response model for given time points
and response parameters.

## Usage

``` r
biosensor.eq(x, y.min, y.max, K, n)
```

## Arguments

- x:

  A vector of concentration values

- y.min:

  The minimum fluorescence value

- y.max:

  The maximum fluorescence value

- K:

  Sensitivity parameter

- n:

  Cooperativity parameter

## Value

A vector of fluorescence values

## References

Meyer, A.J., Segall-Shapiro, T.H., Glassey, E. et al. *Escherichia coli
“Marionette” strains with 12 highly optimized small-molecule sensors.*
Nat Chem Biol 15, 196–204 (2019). DOI: 10.1038/s41589-018-0168-3

## Examples

``` r
n <- seq(1:10)
conc <- rev(10*(1/2)^n)
fit <- biosensor.eq(conc, 300, 82000, 0.85, 2)
```
