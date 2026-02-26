# Helper functions for handling linear fits.

`lm_window` performs a linear regression with the Theil-Sen estimator on
a subset of data.

## Usage

``` r
lm_parms(m)

lm_window(x, y, i0, h = 5)
```

## Arguments

- m:

  linear model (`lm`) object

- x:

  vector of independent variable (e.g. time).

- y:

  vector of dependent variable (concentration of organisms).

- i0:

  index of first value used for a window.

- h:

  with of the window (number of data).

## Value

linear model object of class `lm` (lm_window) resp. vector with
parameters of the fit (lm_parms).

## References

Hall, B. G., H. Acar and M. Barlow 2013. Growth Rates Made Easy. Mol.
Biol. Evol. 31: 232-238
[doi:10.1093/molbev/mst197](https://doi.org/10.1093/molbev/mst197)

## Examples

``` r
# Create random growth dataset
rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")

# Extract time and growth data for single sample
time <- rnd.dataset$time[1,]
data <- as.numeric(rnd.dataset$data[1,-(1:3)]) # Remove identifier columns
data.log <- log(data/data[1])

# Perform linear fit on 8th window of size 8
linreg <- lm_window(time, data.log, 8, h=8)

summary(linreg)
#> 
#> Call:
#> theil_sen_regression(formula = y ~ x)
#> 
#> Residuals:
#>       Min        1Q    Median        3Q       Max 
#> -0.015586 -0.003322  0.000000  0.009126  0.032135 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -0.15449    0.02916  -5.299  0.00183 ** 
#> x            0.11348    0.01085  10.458 4.49e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.01758 on 6 degrees of freedom
#> Multiple R-squared:  0.948,  Adjusted R-squared:  0.9393 
#> F-statistic: 109.4 on 1 and 6 DF,  p-value: 4.485e-05
#> 

lm_parms(linreg)
#>           a           b        b.se          r2       b.rsd 
#> -0.15449242  0.11348346  0.01085171  0.94799008  0.09562371 
```
