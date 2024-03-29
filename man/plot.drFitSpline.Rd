% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growth_plots.R
\name{plot.drFitSpline}
\alias{plot.drFitSpline}
\title{Generic plot function for \code{drFitSpline} objects.}
\usage{
\method{plot}{drFitSpline}(
  x,
  add = FALSE,
  ec50line = TRUE,
  log = "",
  pch = 16,
  colSpline = 1,
  colData = 1,
  cex.point = 1,
  cex.lab = 1.5,
  cex.axis = 1.3,
  y.lim = NULL,
  x.lim = NULL,
  y.title = NULL,
  x.title = NULL,
  lwd = 2,
  plot = TRUE,
  export = FALSE,
  height = 7,
  width = 9,
  out.dir = NULL,
  ...
)
}
\arguments{
\item{x}{object of class \code{drFitSpline}, created with \code{\link{growth.drFitSpline}}.}

\item{add}{(Logical) Shall the fitted spline be added to an existing plot? \code{TRUE} is used internally by \code{\link{plot.drBootSpline}}.}

\item{ec50line}{(Logical) Show pointed horizontal and vertical lines at the EC50 value (\code{TRUE}) or not (\code{FALSE}).}

\item{log}{("x", "y", or "xy") Display the x- or y-axis on a logarithmic scale.}

\item{pch}{(Numeric) Shape of the raw data symbols.}

\item{colSpline}{(Numeric or character) Spline line colour.}

\item{colData}{(Numeric or character) Contour color of the raw data circles.}

\item{cex.point}{(Numeric) Size of the raw data symbols.}

\item{cex.lab}{(Numeric) Font size of axis titles.}

\item{cex.axis}{(Numeric) Font size of axis annotations.}

\item{y.lim}{(Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.}

\item{x.lim}{(Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.}

\item{y.title}{(Character) Optional: Provide a title for the y-axis.}

\item{x.title}{(Character) Optional: Provide a title for the x-axis.}

\item{lwd}{(Numeric) Line width of spline.}

\item{plot}{(Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).}

\item{export}{(Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).}

\item{height}{(Numeric) Height of the exported image in inches.}

\item{width}{(Numeric) Width of the exported image in inches.}

\item{out.dir}{(Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.}

\item{...}{Further arguments to refine the generated base R plot.}
}
\value{
A plot with the nonparametric dose-response fit.
}
\description{
\code{plot.drFitSpline} generates the spline fit plot for response-parameter vs. concentration data
}
\examples{
conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
response <- c(1/(1+exp(-0.7*(4-conc[-20])))+stats::rnorm(19)/50, 0)

TestRun <- growth.drFitSpline(conc, response, drID = "test",
                     control = growth.control(log.x.dr = TRUE, smooth.dr = 0.8))

print(summary(TestRun))
plot(TestRun)
}
