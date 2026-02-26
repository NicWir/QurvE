# Perform a bootstrap on growth vs. time data followed by spline fits for each resample

`growth.gcBootSpline` resamples the growth-time value pairs in a dataset
with replacement and performs a spline fit for each bootstrap sample.

## Usage

``` r
growth.gcBootSpline(time, data, gcID = "undefined", control = growth.control())
```

## Arguments

- time:

  Vector of the independent variable (usually: time).

- data:

  Vector of dependent variable (usually: growth values).

- gcID:

  (Character) The name of the analyzed sample.

- control:

  A `grofit.control` object created with
  [`growth.control`](https://nicwir.github.io/QurvE/reference/growth.control.md),
  defining relevant fitting options.

## Value

A `gcBootSpline` object containing a distribution of growth parameters
and a `gcFitSpline` object for each bootstrap sample. Use
[`plot.gcBootSpline`](https://nicwir.github.io/QurvE/reference/plot.gcBootSpline.md)
to visualize all bootstrapping splines as well as the distribution of
physiological parameters.

- raw.time:

  Raw time values provided to the function as `time`.

- raw.data:

  Raw growth data provided to the function as `data`.

- gcID:

  (Character) Identifies the tested sample.

- boot.time:

  Table of time values per column, resulting from each spline fit of the
  bootstrap.

- boot.data:

  Table of growth values per column, resulting from each spline fit of
  the bootstrap.

- boot.gcSpline:

  List of `gcFitSpline` object, created by
  [`growth.gcFitSpline`](https://nicwir.github.io/QurvE/reference/growth.gcFitSpline.md)
  for each resample of the bootstrap.

- lambda:

  Vector of estimated lambda (lag time) values from each bootstrap
  entry.

- mu:

  Vector of estimated mu (maximum growth rate) values from each
  bootstrap entry.

- A:

  Vector of estimated A (maximum growth) values from each bootstrap
  entry.

- integral:

  Vector of estimated integral values from each bootstrap entry.

- bootFlag:

  (Logical) Indicates the success of the bootstrapping operation.

- control:

  Object of class `grofit.control` containing list of options passed to
  the function as `control`.

## References

Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig,
Maik Kschischo (2010). *grofit: Fitting Biological Growth Curves with
R*. Journal of Statistical Software, 33(7), 1-21. DOI:
10.18637/jss.v033.i07

## See also

Other growth fitting functions:
[`growth.drFit()`](https://nicwir.github.io/QurvE/reference/growth.drFit.md),
[`growth.gcFit()`](https://nicwir.github.io/QurvE/reference/growth.gcFit.md),
[`growth.gcFitLinear()`](https://nicwir.github.io/QurvE/reference/growth.gcFitLinear.md),
[`growth.gcFitModel()`](https://nicwir.github.io/QurvE/reference/growth.gcFitModel.md),
[`growth.gcFitSpline()`](https://nicwir.github.io/QurvE/reference/growth.gcFitSpline.md),
[`growth.workflow()`](https://nicwir.github.io/QurvE/reference/growth.workflow.md)

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

plot(TestFit, combine = TRUE, lwd = 0.5)

```
