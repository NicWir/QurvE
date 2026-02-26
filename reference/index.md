# Package index

## All functions

- [`big_palette`](https://nicwir.github.io/QurvE/reference/big_palette.md)
  : A palette with 40 colors

- [`biosensor.eq()`](https://nicwir.github.io/QurvE/reference/biosensor.eq.md)
  :

  Internal function used to fit a biosensor response model with
  [`nlsLM`](https://rdrr.io/pkg/minpack.lm/man/nlsLM.html)

- [`export_RData()`](https://nicwir.github.io/QurvE/reference/export_RData.md)
  : Export an R object as .RData file

- [`export_Table()`](https://nicwir.github.io/QurvE/reference/export_Table.md)
  : Export a tabular object as tab-separated .txt file

- [`fl.control()`](https://nicwir.github.io/QurvE/reference/fl.control.md)
  :

  Create a `fl.control` object.

- [`fl.drFit()`](https://nicwir.github.io/QurvE/reference/fl.drFit.md) :
  Fit a biosensor model (Meyer et al., 2019) to response vs.
  concentration data

- [`fl.drFitModel()`](https://nicwir.github.io/QurvE/reference/fl.drFitModel.md)
  : Perform a biosensor model fit on response vs. concentration data of
  a single sample.

- [`fl.report()`](https://nicwir.github.io/QurvE/reference/fl.report.md)
  : Create a PDF and HTML report with results from a fluorescence
  analysis workflow

- [`fl.workflow()`](https://nicwir.github.io/QurvE/reference/fl.workflow.md)
  : Run a complete fluorescence curve analysis and dose-reponse analysis
  workflow.

- [`flBootSpline()`](https://nicwir.github.io/QurvE/reference/flBootSpline.md)
  : flBootSpline: Function to generate a bootstrap

- [`flFit()`](https://nicwir.github.io/QurvE/reference/flFit.md) :
  Perform a fluorescence curve analysis on all samples in the provided
  dataset.

- [`flFitLinear()`](https://nicwir.github.io/QurvE/reference/flFitLinear.md)
  : Data fit via a heuristic linear method

- [`flFitSpline()`](https://nicwir.github.io/QurvE/reference/flFitSpline.md)
  : Perform a smooth spline fit on fluorescence data

- [`growth.control()`](https://nicwir.github.io/QurvE/reference/growth.control.md)
  :

  Create a `grofit.control` object.

- [`growth.drBootSpline()`](https://nicwir.github.io/QurvE/reference/growth.drBootSpline.md)
  : Perform a smooth spline fit on response vs. concentration data of a
  single sample

- [`growth.drFit()`](https://nicwir.github.io/QurvE/reference/growth.drFit.md)
  : Perform a dose-response analysis on response vs. concentration data

- [`growth.drFitModel()`](https://nicwir.github.io/QurvE/reference/growth.drFitModel.md)
  : Fit various models to response vs. concentration data of a single
  sample to determine the EC50.

- [`growth.drFitSpline()`](https://nicwir.github.io/QurvE/reference/growth.drFitSpline.md)
  : Perform a smooth spline fit on response vs. concentration data of a
  single sample to determine the EC50.

- [`growth.gcBootSpline()`](https://nicwir.github.io/QurvE/reference/growth.gcBootSpline.md)
  : Perform a bootstrap on growth vs. time data followed by spline fits
  for each resample

- [`growth.gcFit()`](https://nicwir.github.io/QurvE/reference/growth.gcFit.md)
  : Perform a growth curve analysis on all samples in the provided
  dataset.

- [`growth.gcFitLinear()`](https://nicwir.github.io/QurvE/reference/growth.gcFitLinear.md)
  : Fit an exponential growth model with a heuristic linear method

- [`growth.gcFitModel()`](https://nicwir.github.io/QurvE/reference/growth.gcFitModel.md)
  : Fit nonlinear growth models to growth data

- [`growth.gcFitSpline()`](https://nicwir.github.io/QurvE/reference/growth.gcFitSpline.md)
  : Perform a smooth spline fit on growth data

- [`growth.report()`](https://nicwir.github.io/QurvE/reference/growth.report.md)
  : Create a PDF and HTML report with results from a growth curve
  analysis workflow

- [`growth.workflow()`](https://nicwir.github.io/QurvE/reference/growth.workflow.md)
  : Run a complete growth curve analysis and dose-reponse analysis
  workflow.

- [`inflect()`](https://nicwir.github.io/QurvE/reference/inflect.md) :
  Find indices of maxima an minima in a data series

- [`lm_parms()`](https://nicwir.github.io/QurvE/reference/lm_window.md)
  [`lm_window()`](https://nicwir.github.io/QurvE/reference/lm_window.md)
  : Helper functions for handling linear fits.

- [`low.integrate()`](https://nicwir.github.io/QurvE/reference/low.integrate.md)
  : Function to estimate the area under a curve given as x and y(x)
  values

- [`parse_Gen5Gen6()`](https://nicwir.github.io/QurvE/reference/parse_Gen5Gen6.md)
  : Extract relevant data from a raw data export file generated with the
  "Gen5" or "Gen6" software.

- [`parse_data()`](https://nicwir.github.io/QurvE/reference/parse_data.md)
  : Parse raw plate reader data and convert it to a format compatible
  with QurvE

- [`parse_victornivo()`](https://nicwir.github.io/QurvE/reference/parse_victornivo.md)
  : Extract relevant data from a raw data export file generated from the
  software of Perkin Elmer's "Victor Nivo" plate readers.

- [`parse_victorx3()`](https://nicwir.github.io/QurvE/reference/parse_victorx3.md)
  : Extract relevant data from a raw data export file generated from the
  software of Perkin Elmer's "Victor X3" plate readers.

- [`plot(`*`<drBootSpline>`*`)`](https://nicwir.github.io/QurvE/reference/plot.drBootSpline.md)
  :

  Generic plot function for `gcBootSpline` objects.

- [`plot(`*`<drFit>`*`)`](https://nicwir.github.io/QurvE/reference/plot.drFit.md)
  :

  Generic plot function for `drFit` objects.

- [`plot(`*`<drFitFLModel>`*`)`](https://nicwir.github.io/QurvE/reference/plot.drFitFLModel.md)
  :

  Generic plot function for `drFitFLModel` objects.

- [`plot(`*`<drFitModel>`*`)`](https://nicwir.github.io/QurvE/reference/plot.drFitModel.md)
  :

  Generic plot function for `drFitModel` objects.

- [`plot(`*`<drFitSpline>`*`)`](https://nicwir.github.io/QurvE/reference/plot.drFitSpline.md)
  :

  Generic plot function for `drFitSpline` objects.

- [`plot(`*`<drFitfl>`*`)`](https://nicwir.github.io/QurvE/reference/plot.drFitfl.md)
  :

  Generic plot function for `drFitFL` objects.

- [`plot(`*`<dr_parameter>`*`)`](https://nicwir.github.io/QurvE/reference/plot.dr_parameter.md)
  : Compare calculated dose-response parameters between conditions.

- [`plot(`*`<dual>`*`)`](https://nicwir.github.io/QurvE/reference/plot.dual.md)
  : Compare fluorescence and growth over time

- [`plot(`*`<flBootSpline>`*`)`](https://nicwir.github.io/QurvE/reference/plot.flBootSpline.md)
  :

  Generic plot function for `flBootSpline` objects.

- [`plot(`*`<flFitLinear>`*`)`](https://nicwir.github.io/QurvE/reference/plot.flFitLinear.md)
  :

  Generic plot function for `flcFittedLinear` objects. Plot the results
  of a linear regression on ln-transformed data

- [`plot(`*`<flFitRes>`*`)`](https://nicwir.github.io/QurvE/reference/plot.flFitRes.md)
  [`plot(`*`<flFit>`*`)`](https://nicwir.github.io/QurvE/reference/plot.flFitRes.md)
  : Combine different groups of samples into a single plot

- [`plot(`*`<flFitSpline>`*`)`](https://nicwir.github.io/QurvE/reference/plot.flFitSpline.md)
  :

  Generic plot function for `flFitSpline` objects.

- [`plot(`*`<gcBootSpline>`*`)`](https://nicwir.github.io/QurvE/reference/plot.gcBootSpline.md)
  :

  Generic plot function for `gcBootSpline` objects.

- [`plot(`*`<gcFitLinear>`*`)`](https://nicwir.github.io/QurvE/reference/plot.gcFitLinear.md)
  :

  Generic plot function for `gcFittedLinear` objects. Plot the results
  of a linear regression on ln-transformed data

- [`plot(`*`<gcFitModel>`*`)`](https://nicwir.github.io/QurvE/reference/plot.gcFitModel.md)
  :

  Generic plot function for `gcFitModel` objects.

- [`plot(`*`<gcFitSpline>`*`)`](https://nicwir.github.io/QurvE/reference/plot.gcFitSpline.md)
  :

  Generic plot function for `gcFitSpline` objects.

- [`plot(`*`<grid>`*`)`](https://nicwir.github.io/QurvE/reference/plot.grid.md)
  : Plot a matrix of growth curve panels

- [`plot(`*`<grodata>`*`)`](https://nicwir.github.io/QurvE/reference/plot.grodata.md)
  :

  Generic plot function for `grodata` objects. Plots raw growth,
  fluorescence, or normalized fluorescence data of multiple samples or
  conditions.

- [`plot(`*`<grofit>`*`)`](https://nicwir.github.io/QurvE/reference/plot.grofit.md)
  :

  Generic plot function for `grofit` objects. Combine different groups
  of samples into a single plot

- [`plot(`*`<parameter>`*`)`](https://nicwir.github.io/QurvE/reference/plot.parameter.md)
  : Compare growth parameters between samples or conditions

- [`rdm.data()`](https://nicwir.github.io/QurvE/reference/rdm.data.md) :

  The function calls the `baranyi` function to generate curves between
  time zero and `t` and adds some random noise to the x- and y-axes. The
  three growth parameters given as input values will be slightly changed
  to produce different growth curves. The resulting datasets can be used
  to test the `growth.workflow` function.

- [`read_data()`](https://nicwir.github.io/QurvE/reference/read_data.md)
  : Read growth and fluorescence data in table format

- [`read_file()`](https://nicwir.github.io/QurvE/reference/read_file.md)
  : Call the appropriate function required to read a table file and
  return the table as a dataframe object.

- [`run_app()`](https://nicwir.github.io/QurvE/reference/run_app.md) :
  Run Shiny QurvE App

- [`single_hue_palettes`](https://nicwir.github.io/QurvE/reference/single_hue_palettes.md)
  : Single hue palettes for ggplot2

- [`summary(`*`<drBootSpline>`*`)`](https://nicwir.github.io/QurvE/reference/summary.drBootSpline.md)
  : Generic summary function for drBootSpline objects

- [`summary(`*`<drFit>`*`)`](https://nicwir.github.io/QurvE/reference/summary.drFit.md)
  : Generic summary function for drFit objects

- [`summary(`*`<drFitFLModel>`*`)`](https://nicwir.github.io/QurvE/reference/summary.drFitFLModel.md)
  : Generic summary function for drFitFLModel objects

- [`summary(`*`<drFitModel>`*`)`](https://nicwir.github.io/QurvE/reference/summary.drFitModel.md)
  : Generic summary function for drFitModel objects

- [`summary(`*`<drFitSpline>`*`)`](https://nicwir.github.io/QurvE/reference/summary.drFitSpline.md)
  : Generic summary function for drFitSpline objects

- [`summary(`*`<drFitfl>`*`)`](https://nicwir.github.io/QurvE/reference/summary.drFitfl.md)
  : Generic summary function for drFitfl objects

- [`summary(`*`<flBootSpline>`*`)`](https://nicwir.github.io/QurvE/reference/summary.flBootSpline.md)
  : Generic summary function for flBootSpline objects

- [`summary(`*`<flFit>`*`)`](https://nicwir.github.io/QurvE/reference/summary.flFit.md)
  : Generic summary function for flFit objects

- [`summary(`*`<flFitLinear>`*`)`](https://nicwir.github.io/QurvE/reference/summary.flFitLinear.md)
  : Generic summary function for flFitLinear objects

- [`summary(`*`<flFitSpline>`*`)`](https://nicwir.github.io/QurvE/reference/summary.flFitSpline.md)
  : Generic summary function for flFitSpline objects

- [`summary(`*`<gcBootSpline>`*`)`](https://nicwir.github.io/QurvE/reference/summary.gcBootSpline.md)
  : Generic summary function for gcBootSpline objects

- [`summary(`*`<gcFit>`*`)`](https://nicwir.github.io/QurvE/reference/summary.gcFit.md)
  : Generic summary function for gcFit objects

- [`summary(`*`<gcFitLinear>`*`)`](https://nicwir.github.io/QurvE/reference/summary.gcFitLinear.md)
  : Generic summary function for gcFitLinear objects

- [`summary(`*`<gcFitModel>`*`)`](https://nicwir.github.io/QurvE/reference/summary.gcFitModel.md)
  : Generic summary function for gcFitModel objects

- [`summary(`*`<gcFitSpline>`*`)`](https://nicwir.github.io/QurvE/reference/summary.gcFitSpline.md)
  : Generic summary function for gcFitSpline objects

- [`table_group_fluorescence_linear()`](https://nicwir.github.io/QurvE/reference/table_group_fluorescence_linear.md)
  : Generate a grouped results table for linear fits with average and
  standard deviations

- [`table_group_fluorescence_spline()`](https://nicwir.github.io/QurvE/reference/table_group_fluorescence_spline.md)
  : Generate a grouped results table for spline fits with average and
  standard deviations

- [`table_group_growth_linear()`](https://nicwir.github.io/QurvE/reference/table_group_growth_linear.md)
  : Generate a grouped results table for linear fits with average and
  standard deviations

- [`table_group_growth_model()`](https://nicwir.github.io/QurvE/reference/table_group_growth_model.md)
  : Generate a grouped results table for parametric fits with average
  and standard deviations

- [`table_group_growth_spline()`](https://nicwir.github.io/QurvE/reference/table_group_growth_spline.md)
  : Generate a grouped results table for spline fits with average and
  standard deviations

- [`zipFastener()`](https://nicwir.github.io/QurvE/reference/zipFastener.md)
  : Combine two dataframes like a zip-fastener
