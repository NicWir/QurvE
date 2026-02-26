# Create a `grofit.control` object.

A `grofit.control` object is required to perform various computations on
growth data stored within `grodata` objects (created with
[`read_data`](https://nicwir.github.io/QurvE/reference/read_data.md) or
[`parse_data`](https://nicwir.github.io/QurvE/reference/parse_data.md)).
A `grofit.control` object is created automatically as part of
[`growth.workflow`](https://nicwir.github.io/QurvE/reference/growth.workflow.md).

## Usage

``` r
growth.control(
  neg.nan.act = FALSE,
  clean.bootstrap = TRUE,
  suppress.messages = FALSE,
  fit.opt = c("a"),
  t0 = 0,
  tmax = NA,
  min.growth = NA,
  max.growth = NA,
  log.x.gc = FALSE,
  log.y.lin = TRUE,
  log.y.spline = TRUE,
  log.y.model = TRUE,
  lin.h = NULL,
  lin.R2 = 0.97,
  lin.RSD = 0.1,
  lin.dY = 0.05,
  biphasic = FALSE,
  interactive = FALSE,
  nboot.gc = 0,
  smooth.gc = 0.55,
  model.type = c("logistic", "richards", "gompertz", "gompertz.exp", "huang", "baranyi"),
  dr.method = c("model", "spline"),
  dr.model = c("gammadr", "multi2", "LL.2", "LL.3", "LL.4", "LL.5", "W1.2", "W1.3",
    "W1.4", "W2.2", "W2.3", "W2.4", "LL.3u", "LL2.2", "LL2.3", "LL2.3u", "LL2.4",
    "LL2.5", "AR.2", "AR.3", "MM.2"),
  dr.have.atleast = 6,
  dr.parameter = c("mu.linfit", "lambda.linfit", "dY.linfit", "A.linfit", "mu.spline",
    "lambda.spline", "dY.spline", "A.spline", "mu.model", "lambda.model",
    "dY.orig.model", "A.orig.model"),
  smooth.dr = NULL,
  log.x.dr = FALSE,
  log.y.dr = FALSE,
  nboot.dr = 0,
  growth.thresh = 1.5
)
```

## Arguments

- neg.nan.act:

  (Logical) Indicates whether the program should stop when negative
  growth values or NA values appear (`TRUE`). Otherwise, the program
  removes these values silently (`FALSE`). Improper values may be caused
  by incorrect data or input errors. Default: `FALSE`.

- clean.bootstrap:

  (Logical) Determines if negative values which occur during bootstrap
  should be removed (TRUE) or kept (FALSE). Note: Infinite values are
  always removed. Default: TRUE.

- suppress.messages:

  (Logical) Indicates whether messages (information about current growth
  curve, EC50 values etc.) should be displayed (`FALSE`) or not
  (`TRUE`). This option is meant to speed up the processing of high
  throughput data. Note: warnings are still displayed. Default: `FALSE`.

- fit.opt:

  (Character or character vector) Indicates whether the program should
  perform a linear regression (`'l'`), model fit (`'m'`), spline fit
  (`'s'`), or all (`'a'`). Combinations can be freely chosen by
  providing a character vector, e.g. `fit.opt = c('l', 's')` Default:
  `fit.opt = c('l', 's')`.

- t0:

  (Numeric) Minimum time value considered for linear and spline fits.

- tmax:

  (Numeric) Maximum time value considered for linear and spline fits.

- min.growth:

  (Numeric) Indicate whether only growth values above a certain
  threshold should be considered for linear regressions or spline fits.

- max.growth:

  (Numeric) Indicate whether only growth values below a certain
  threshold should be considered for linear regressions or spline fits.

- log.x.gc:

  (Logical) Indicates whether *ln(x+1)* should be applied to the time
  data for *linear* and *spline* fits. Default: `FALSE`.

- log.y.lin:

  (Logical) Indicates whether *ln(y/y0)* should be applied to the growth
  data for *linear* fits. Default: `TRUE`

- log.y.spline:

  (Logical) Indicates whether *ln(y/y0)* should be applied to the growth
  data for *spline* fits. Default: `TRUE`

- log.y.model:

  (Logical) Indicates whether *ln(y/y0)* should be applied to the growth
  data for *model* fits. Default: `TRUE`

- lin.h:

  (Numeric) Manually define the size of the sliding window used in
  [`growth.gcFitLinear`](https://nicwir.github.io/QurvE/reference/growth.gcFitLinear.md)
  If `NULL`, h is calculated for each samples based on the number of
  measurements in the growth phase of the plot.

- lin.R2:

  (Numeric) R² threshold for
  [`growth.gcFitLinear`](https://nicwir.github.io/QurvE/reference/growth.gcFitLinear.md)

- lin.RSD:

  (Numeric) Relative standard deviation (RSD) threshold for the
  calculated slope in
  [`growth.gcFitLinear`](https://nicwir.github.io/QurvE/reference/growth.gcFitLinear.md)

- lin.dY:

  (Numeric) Threshold for the minimum fraction of growth increase a
  linear regression window should cover. Default: 0.05 (5%).

- biphasic:

  (Logical) Shall
  [`growth.gcFitLinear`](https://nicwir.github.io/QurvE/reference/growth.gcFitLinear.md)
  and
  [`growth.gcFitSpline`](https://nicwir.github.io/QurvE/reference/growth.gcFitSpline.md)
  try to extract growth parameters for two different growth phases (as
  observed with, e.g., diauxic shifts) (`TRUE`) or not (`FALSE`)?

- interactive:

  (Logical) Controls whether the fit of each growth curve and method is
  controlled manually by the user. If `TRUE`, each fit is visualized in
  the *Plots* pane and the user can adjust fitting parameters and
  confirm the reliability of each fit per sample. Default: `TRUE`.

- nboot.gc:

  (Numeric) Number of bootstrap samples used for nonparametric growth
  curve fitting with
  [`growth.gcBootSpline`](https://nicwir.github.io/QurvE/reference/growth.gcBootSpline.md).
  Use `nboot.gc = 0` to disable the bootstrap. Default: `0`

- smooth.gc:

  (Numeric) Parameter describing the smoothness of the spline fit;
  usually (not necessary) within (0;1\]. `smooth.gc=NULL` causes the
  program to query an optimal value via cross validation techniques.
  Especially for datasets with few data points the option `NULL` might
  cause a too small smoothing parameter. This can result a too tight fit
  that is susceptible to measurement errors (thus overestimating growth
  rates) or produce an error in
  [`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html) or lead
  to overfitting. The usage of a fixed value is recommended for
  reproducible results across samples. See
  [`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html) for
  further details. Default: `0.55`

- model.type:

  (Character) Vector providing the names of the parametric models which
  should be fitted to the data. Default:
  `c('logistic', 'richards', 'gompertz', 'gompertz.exp', 'huang', 'baranyi')`.

- dr.method:

  (Character) Define the method used to perform a dose-responde
  analysis: smooth spline fit (`'spline'`) or model fitting (`'model'`).

- dr.model:

  (Character) Provide a list of models from the R package 'drc' to
  include in the dose-response analysis (if `dr.method = 'model'`). If
  more than one model is provided, the best-fitting model will be chosen
  based on the Akaike Information Criterion.

- dr.have.atleast:

  (Numeric) Minimum number of different values for the response
  parameter one should have for estimating a dose response curve. Note:
  All fit procedures require at least six unique values. Default: `6`.

- dr.parameter:

  (Character or numeric) The response parameter in the output table to
  be used for creating a dose response curve. See
  [`growth.drFit`](https://nicwir.github.io/QurvE/reference/growth.drFit.md)
  for further details. Default: `'mu.linfit'`, which represents the
  maximum slope of the linear regression. Typical options include:
  `'mu.linfit'`, `'lambda.linfit'`, `'dY.linfit'`, `'mu.spline'`,
  `'dY.spline'`, `'mu.model'`, and `'A.model'`.

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

- growth.thresh:

  (Numeric) Define a threshold for growth. Only if any growth value in a
  sample is greater than `growth.thresh` (default: 1.5) times the start
  growth, further computations are performed. Else, a message is
  returned.

## Value

Generates a list with all arguments described above as entries.

## References

Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig,
Maik Kschischo (2010). *grofit: Fitting Biological Growth Curves with
R*. Journal of Statistical Software, 33(7), 1-21. DOI:
10.18637/jss.v033.i07

## Examples

``` r
# default option
control_default <- growth.control()
# user defined
control_manual <- growth.control(fit.opt = c('s', 'm'),
                                 smooth.gc = 0.5,
                                 model.type = c('huang', 'baranyi'))
```
