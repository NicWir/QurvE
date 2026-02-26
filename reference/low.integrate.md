# Function to estimate the area under a curve given as x and y(x) values

Function to estimate the area under a curve given as x and y(x) values

## Usage

``` r
low.integrate(x, y)
```

## Arguments

- x:

  Numeric vector of x values.

- y:

  Numeric vector of y values with the same length as `x`.

## Value

Numeric value: Area under the smoothed spline.

## Details

The function uses the the R internal function
[`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html).

## See also

[`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html)

## Examples

``` r
# Create random growth dataset
rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')

# Extract time and growth data for single sample
time <- rnd.dataset$time[1,]
data <- as.numeric(rnd.dataset$data[1,-(1:3)]) # Remove identifier columns

plot(time, data)


print(low.integrate(time, data))
#> [1] 98.24702
```
