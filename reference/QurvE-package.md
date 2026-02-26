# QurvE: Robust and User-Friendly Analysis of Growth and Fluorescence Curves

High-throughput analysis of growth curves and fluorescence data using
three methods: linear regression, growth model fitting, and smooth
spline fit. Analysis of dose-response relationships via smoothing
splines or dose-response models. Complete data analysis workflows can be
executed in a single step via user-friendly wrapper functions. The
results of these workflows are summarized in detailed reports as well as
intuitively navigable 'R' data containers. A 'shiny' application
provides access to all features without requiring any programming
knowledge. The package is described in further detail in Wirth et al.
(2023)
[doi:10.1038/s41596-023-00850-7](https://doi.org/10.1038/s41596-023-00850-7)
.

## See also

Useful links:

- <https://github.com/NicWir/QurvE>

- <https://nicwir.github.io/QurvE/>

- Report bugs at <https://github.com/NicWir/QurvE/issues>

## Author

**Maintainer**: Nicolas T. Wirth <mail.nicowirth@gmail.com>
([ORCID](https://orcid.org/0000-0003-0799-1321)) \[copyright holder\]

Authors:

- Jonathan Funk <funk.jonathan21@gmail.com> (Co-developer of shiny app.)

Other contributors:

- Matthias Kahm (Author of 'grofit' package, whose general data
  structure was adopted for QurvE.) \[contributor\]

- Maik Kschischo (Author of 'grofit' package, whose general data
  structure was adopted for QurvE.) \[contributor\]

- Thomas Petzoldt <thomas.petzoldt@tu-dresden.de>
  ([ORCID](https://orcid.org/0000-0002-4951-6468)) (Creator of the
  package 'growthrates', whose function for calculating linear
  regressions served as a template in QurvE.) \[contributor\]

- Andrew Stein <andy.stein@gmail.com> (Creator of 'xgxr' package from
  which QurvE adopted code to plot axis ticks on log10 scale.)
  \[contributor\]

- Michael W. Kearney <kearneymw@missouri.edu> (Creator of 'tfse' package
  from which QurvE adopted the match_arg function.) \[contributor\]

- Santiago I. Hurtado <santih@carina.fcaglp.unlp.edu.ar> (Creator of
  'RobustLinearReg' package from which QurvE adopted the Theil Sehn
  Regression method.) \[contributor\]

- Mark Heckmann (Creator of the 'zipFastener' function; source:
  https://ryouready.wordpress.com/2009/03/27/r-zip-fastener-for-two-data-frames-combining-rows-or-columns-of-two-dataframes-in-an-alternating-manner/)
  \[contributor\]

- Nicholas Hamilton (Creator of the 'colFmt' function.) \[contributor\]

- Evan Friedland (Creator of the 'inflect' function.) \[contributor\]

- Heather Turner (Creator of the 'base_breaks' function.)
  \[contributor\]

- Georgi N. Boshnakov <georgi.boshnakov@manchester.ac.uk> (Creator of
  'gbRd' package from which functions are used to display function help
  pages within the shiny app.) \[contributor\]
