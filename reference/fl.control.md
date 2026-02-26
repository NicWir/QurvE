# Create a `fl.control` object.

A `fl.control` object is required to perform various computations on
fluorescence data stored within `grodata` objects (created with
[`read_data`](https://nicwir.github.io/QurvE/reference/read_data.md) or
[`parse_data`](https://nicwir.github.io/QurvE/reference/parse_data.md)).
A `fl.control` object is created automatically as part of
[`fl.workflow`](https://nicwir.github.io/QurvE/reference/fl.workflow.md).

## Usage

``` r
fl.control(
  fit.opt = c("l", "s"),
  x_type = c("growth", "time"),
  norm_fl = TRUE,
  t0 = 0,
  tmax = NA,
  min.growth = NA,
  max.growth = NA,
  log.x.lin = FALSE,
  log.x.spline = FALSE,
  log.y.lin = FALSE,
  log.y.spline = FALSE,
  lin.h = NULL,
  lin.R2 = 0.97,
  lin.RSD = 0.05,
  lin.dY = 0.05,
  dr.parameter = "max_slope.spline",
  dr.method = c("model", "spline"),
  dr.have.atleast = 5,
  smooth.dr = NULL,
  log.x.dr = FALSE,
  log.y.dr = FALSE,
  nboot.dr = 0,
  biphasic = FALSE,
  interactive = FALSE,
  nboot.fl = 0,
  smooth.fl = 0.75,
  growth.thresh = 1.5,
  suppress.messages = FALSE,
  neg.nan.act = FALSE,
  clean.bootstrap = TRUE
)
```

## Arguments

- fit.opt:

  (Character or vector of strings) Indicates whether the program should
  perform a linear regression (`'l'`) and/or spline fit (`'s'`).
  Default: `fit.opt = c('l', 's')`.

- x_type:

  (Character) Which data type shall be used as independent variable?
  Options are `'growth'` and `'time'`.

- norm_fl:

  (Logical) use normalized (to growth) fluorescence data in fits. Has an
  effect only when `x_type = 'time'`

- t0:

  (Numeric) Minimum time value considered for linear and spline fits (if
  `x_type = 'time'`).

- tmax:

  (Numeric) Maximum time value considered for linear and spline fits (if
  `x_type = 'time'`)..

- min.growth:

  (Numeric) Indicate whether only values above a certain threshold
  should be considered for linear regressions or spline fits (if
  `x_type = 'growth'`).

- max.growth:

  (Numeric) Indicate whether only growth values below a certain
  threshold should be considered for linear regressions or spline fits
  (if `x_type = 'growth'`).

- log.x.lin:

  (Logical) Indicates whether *ln(x+1)* should be applied to the
  independent variable for *linear* fits. Default: `FALSE`.

- log.x.spline:

  (Logical) Indicates whether *ln(x+1)* should be applied to the
  independent variable for *spline* fits. Default: `FALSE`.

- log.y.lin:

  (Logical) Indicates whether *ln(y/y0)* should be applied to the
  fluorescence data for *linear* fits. Default: `FALSE`

- log.y.spline:

  (Logical) Indicates whether *ln(y/y0)* should be applied to the
  fluorescence data for *spline* fits. Default: `FALSE`

- lin.h:

  (Numeric) Manually define the size of the sliding window used in
  [`flFitLinear`](https://nicwir.github.io/QurvE/reference/flFitLinear.md).
  If `NULL`, h is calculated for each samples based on the number of
  measurements in the fluorescence increase phase of the plot.

- lin.R2:

  (Numeric) R² threshold for
  [`flFitLinear`](https://nicwir.github.io/QurvE/reference/flFitLinear.md).

- lin.RSD:

  (Numeric) Relative standard deviation (RSD) threshold for the
  calculated slope in
  [`flFitLinear`](https://nicwir.github.io/QurvE/reference/flFitLinear.md).

- lin.dY:

  (Numeric) Threshold for the minimum fraction of growth increase a
  linear regression window should cover. Default: 0.05 (5%).

- dr.parameter:

  (Character or numeric) The response parameter in the output table to
  be used for creating a dose response curve. See
  [`fl.drFit`](https://nicwir.github.io/QurvE/reference/fl.drFit.md) for
  further details. Default: `'max_slope.spline'`, which represents the
  maximum slope of the spline fit Typical options include:
  `'max_slope.linfit'`, `'dY.linfit'`, `'max_slope.spline'`, and
  `'dY.spline'`.

- dr.method:

  (Character) Perform either a smooth spline fit on response parameter
  vs. concentration data (`'spline'`) or fit a biosensor response model
  with `'model'` (proposed by Meyer et al., 2019).

- dr.have.atleast:

  (Numeric) Minimum number of different values for the response
  parameter one should have for estimating a dose response curve. Note:
  All fit procedures require at least six unique values. Default: `6`.

- smooth.dr:

  (Numeric) Smoothing parameter used in the spline fit by smooth.spline
  during dose response curve estimation. Usually (not necessesary) in
  (0; 1\]. See
  [`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html) for
  further details. Default: `NULL`.

- log.x.dr:

  (Logical) Indicates whether `ln(x+1)` should be applied to the
  concentration data of the dose response curves. Default: `FALSE`.

- log.y.dr:

  (Logical) Indicates whether `ln(y+1)` should be applied to the
  response data of the dose response curves. Default: `FALSE`.

- nboot.dr:

  (Numeric) Defines the number of bootstrap samples for EC50 estimation.
  Use `nboot.dr = 0` to disable bootstrapping. Default: `0`.

- biphasic:

  (Logical) Shall
  [`flFitLinear`](https://nicwir.github.io/QurvE/reference/flFitLinear.md)
  and
  [`flFitSpline`](https://nicwir.github.io/QurvE/reference/flFitSpline.md)
  try to extract fluorescence parameters for two different phases (as
  observed with, e.g., regulator-promoter systems with varying response
  in different growth stages) (`TRUE`) or not (`FALSE`)?

- interactive:

  (Logical) Controls whether the fit for each sample and method is
  controlled manually by the user. If `TRUE`, each fit is visualized in
  the *Plots* pane and the user can adjust fitting parameters and
  confirm the reliability of each fit per sample. Default: `TRUE`.

- nboot.fl:

  (Numeric) Number of bootstrap samples used for nonparametric curve
  fitting with
  [`flBootSpline`](https://nicwir.github.io/QurvE/reference/flBootSpline.md).
  Use `nboot.fl = 0` to disable the bootstrap. Default: `0`

- smooth.fl:

  (Numeric) Parameter describing the smoothness of the spline fit;
  usually (not necessary) within (0;1\]. `smooth.gc=NULL` causes the
  program to query an optimal value via cross validation techniques.
  Especially for datasets with few data points the option `NULL` might
  cause a too small smoothing parameter. This can result a too tight fit
  that is susceptible to measurement errors (thus overestimating slopes)
  or produce an error in
  [`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html) or lead
  to overfitting. The usage of a fixed value is recommended for
  reproducible results across samples. See
  [`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html) for
  further details. Default: `0.55`

- growth.thresh:

  (Numeric) Define a threshold for growth. Only if any growth value in a
  sample is greater than `growth.thresh` (default: 1.5) times the start
  growth, further computations are performed. Else, a message is
  returned.

- suppress.messages:

  (Logical) Indicates whether messages (information about current
  fluorescence curve, EC50 values etc.) should be displayed (`FALSE`) or
  not (`TRUE`). This option is meant to speed up the high-throughput
  processing data. Note: warnings are still displayed. Default: `FALSE`.

- neg.nan.act:

  (Logical) Indicates whether the program should stop when negative
  fluorescence values or NA values appear (`TRUE`). Otherwise, the
  program removes these values silently (`FALSE`). Improper values may
  be caused by incorrect data or input errors. Default: `FALSE`.

- clean.bootstrap:

  (Logical) Determines if negative values which occur during bootstrap
  should be removed (`TRUE`) or kept (`FALSE`). Note: Infinite values
  are always removed. Default: `TRUE`.

## Value

Generates a list with all arguments described above as entries.

## References

Meyer, A.J., Segall-Shapiro, T.H., Glassey, E. et al. *Escherichia coli
“Marionette” strains with 12 highly optimized small-molecule sensors.*
Nat Chem Biol 15, 196–204 (2019). DOI: 10.1038/s41589-018-0168-3

## Examples

``` r
# default option
control_default <- fl.control()
# user defined
control_manual <- fl.control(fit.opt = c('s'),
                             smooth.fl = 0.6,
                             x_type = 'time',
                             t0 = 2)
```
