#' Create a \code{fl.control} object.
#'
#' A \code{fl.control} object is required to perform various computations on fluorescence data stored within \code{grodata} objects (created with \code{\link{read_data}} or \code{\link{parse_data}}). A \code{fl.control} object is created automatically as part of \code{\link{fl.workflow}}.
#'
#' @param x_type (Character) Which data type shall be used as independent variable? Options are \code{'density'} and \code{'time'}.
#' @param fit.opt (Character or vector of strings) Indicates whether the program should perform a linear regression (\code{"l"}) and/or spline fit (\code{"s"}). Default:  \code{fit.opt = c("l", "s")}.
#' @param norm_fl (Logical) use normalized (to density) fluorescence data in fits. Has an effect only when \code{x_type = 'time'}
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param min.density (Numeric) Indicate whether only values above a certain threshold should be considered for linear regressions or spline fits.
#' @param log.x.lin (Logical) Indicates whether _ln(x+1)_ should be applied to the independent variable for _linear_ fits. Default: \code{FALSE}.
#' @param log.x.spline (Logical) Indicates whether _ln(x+1)_ should be applied to the independent variable for _spline_ fits. Default: \code{FALSE}.
#' @param log.y.lin (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ fits. Default: \code{FALSE}
#' @param log.y.spline (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _spline_ fits. Default: \code{FALSE}
#' @param lin.h (Numeric) Manually define the size of the sliding window used in \code{\link{fl.gcFitLinear}}. If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{\link{fl.gcFitLinear}}.
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for the calculated slope in \code{\link{fl.gcFitLinear}}.
#' @param lin.dY (Numeric) Threshold for the minimum fraction of density increase a linear regression window should cover. Default: 0.05 (5%).
#' @param dr.parameter (Character or numeric) The response parameter in the output table to be used for creating a dose response curve. See \code{\link{fl.drFit}} for further details. Default: \code{"max_slope.spline"}, which represents the maximum slope of the spline fit Typical options include: \code{"max_slope.linfit"}, \code{"dY.linfit"}, \code{"max_slope.spline"}, and \code{"dY.spline"}.
#' @param dr.method (Character) Perform either a smooth spline fit on response parameter vs. concentration data (\code{"spline"}) or fit a biosensor response model (proposed by Meyer et al., 2019).
#' @param dr.have.atleast (Numeric) Minimum number of different values for the response parameter one should have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: \code{6}.
#' @param smooth.dr (Numeric) Smoothing parameter used in the spline fit by smooth.spline during dose response curve estimation. Usually (not necessesary) in (0; 1]. See \code{\link{smooth.spline}} for further details. Default: \code{NULL}.
#' @param log.x.dr (Logical) Indicates whether \code{ln(x+1)} should be applied to the concentration data of the dose response curves. Default: \code{FALSE}.
#' @param log.y.dr (Logical) Indicates whether \code{ln(y+1)} should be applied to the response data of the dose response curves. Default: \code{FALSE}.
#' @param nboot.dr (Numeric) Defines the number of bootstrap samples for EC50 estimation. Use \code{nboot.dr = 0} to disable bootstrapping. Default: \code{0}.
#' @param biphasic (Logical) Shall \code{\link{flFitLinear}} and \code{\link{flFitSpline}} try to extract fluorescence parameters for two different phases (as observed with, e.g., regulator-promoter systems with varying response in different growth stages) (\code{TRUE}) or not (\code{FALSE})?
#' @param interactive (Logical) Controls whether the fit for each sample and method is controlled manually by the user. If \code{TRUE}, each fit is visualized in the _Plots_ pane and the user can adjust fitting parameters and confirm the reliability of each fit per sample. Default: \code{TRUE}.
#' @param nboot.fl (Numeric) Number of bootstrap samples used for nonparametric curve fitting with \code{\link{flBootSpline}}. Use \code{nboot.fl = 0} to disable the bootstrap. Default: \code{0}
#' @param smooth.fl (Numeric) Parameter describing the smoothness of the spline fit; usually (not necessary) within (0;1]. \code{smooth.gc=NULL} causes the program to query an optimal value via cross validation techniques. Especially for datasets with few data points the option \code{NULL} might cause a too small smoothing parameter. This can result a too tight fit that is susceptible to measurement errors (thus overestimating slopes) or produce an error in \code{\link{smooth.spline}} or lead to overfitting. The usage of a fixed value is recommended for reproducible results across samples. See \code{\link{smooth.spline}} for further details. Default: \code{0.55}
#' @param growth.thresh (Numeric) Define a threshold for growth. Only if any density value in a sample is greater than \code{growth.thresh} (default: 1.5) times the start density, further computations are performed. Else, a message is returned.
#' @param suppress.messages (Logical) Indicates whether messages (information about current growth curve, EC50 values etc.) should be displayed (\code{FALSE}) or not (\code{TRUE}). This option is meant to speed up the high-throughput processing data. Note: warnings are still displayed. Default: \code{FALSE}.
#' @param neg.nan.act (Logical) Indicates whether the program should stop when negative growth values or NA values appear (\code{TRUE}). Otherwise, the program removes these values silently (\code{FALSE}). Improper values may be caused by incorrect data or input errors. Default: \code{FALSE}.
#' @param clean.bootstrap (Logical) Determines if negative values which occur during bootstrap should be removed (\code{TRUE}) or kept (\code{FALSE}). Note: Infinite values are always removed. Default: \code{TRUE}.
#'
#' @return Generates a list with all arguments described above as entries.
#'
#' @references Meyer, A.J., Segall-Shapiro, T.H., Glassey, E. et al. _Escherichia coli “Marionette” strains with 12 highly optimized small-molecule sensors._ Nat Chem Biol 15, 196–204 (2019). DOI: 10.1038/s41589-018-0168-3
#'
#' @export
#'
fl.control <- function(fit.opt = c("l", "s"),
                       x_type = c("density", "time"),
                       norm_fl = TRUE,
                       t0 = 0,
                       min.density = NA,
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
                       clean.bootstrap = TRUE)
{
  x_type <- match.arg(x_type)
  dr.method <- match.arg(dr.method)
  if ((is.character(fit.opt) == FALSE | !any(fit.opt %in% c("l", "s"))))
    stop("value of fit.opt must be character and contain one of or both 'l' and 's'.")
  if ((is.character(x_type) == FALSE | !any(x_type %in% c("density", "time"))))
    stop("value of x_type must be character and contain one of 'density' or 'time'.")
  if ((is.character(dr.method) == FALSE | !any(dr.method %in% c("model", "spline"))))
    stop("value of dr.method must be character and contain one of 'model' or 'spline'.")
  if ((is.logical(suppress.messages) == FALSE) | (length(suppress.messages) != 1))
    stop("value of suppress.messages must be logical and of one element")
  if ((is.logical(log.x.lin) == FALSE) | (length(log.x.lin) != 1))
    stop("value of log.x.lin must be logical and of one element")
  if ((is.logical(log.x.spline) == FALSE) | (length(log.x.spline) != 1))
    stop("value of log.x.spline must be logical and of one element")
  if ((is.logical(log.y.lin) == FALSE) | (length(log.y.lin) != 1))
    stop("value of log.y.spline must be logical and of one element")
  if ((is.logical(log.y.spline) == FALSE) | (length(log.y.spline) != 1))
    stop("value of log.y.spline must be logical and of one element")
  if ((is.logical(interactive) == FALSE) | (length(interactive) != 1))
    stop("value of interactive must be logical and of one element")
  if ((is.numeric(nboot.fl) == FALSE) | (length(nboot.fl) !=1) | (nboot.fl < 0))
    stop("value of nboot.fl must be numeric (>=0) and of one element")
  if ((is.numeric(lin.dY) == FALSE) | (length(lin.dY) != 1) | (lin.dY < 0))
    stop("value of lin.dY must be numeric (>=0) and of one element")
  if (((is.numeric(smooth.fl) == FALSE)))
    stop("value of smooth.fl must be numeric")
  if ((is.numeric(dr.have.atleast) == FALSE) | (length(dr.have.atleast) != 1) | (dr.have.atleast < 5))
    stop("value of dr.have.atleast must be numeric (>=5) and of one element")
  if (((is.numeric(lin.R2) == FALSE) |  (length(lin.R2) != 1) | !(0 < lin.R2 && lin.R2 < 1) ))
    stop("value of lin.R2 must be numeric (0 < lin.R2 < 1) and of one element")
  if (((is.numeric(lin.RSD) == FALSE) |  (length(lin.RSD) != 1) | !(0 < lin.RSD) ))
    stop("value of lin.RSD must be numeric (0 < lin.RSD) and of one element")
  if (((is.numeric(lin.h) == FALSE) && (is.null(lin.h) == FALSE)))
    stop("value of lin.h must be numeric (> 0) and of one element")
  if (((is.numeric(growth.thresh) == FALSE) && (is.na(growth.thresh) == FALSE)))
    stop("value of growth.thresh must be numeric (one element) or NA")
  if ((is.logical(biphasic) == FALSE) | (length(biphasic) != 1))
    stop("value of biphasic must be logical and of one element")
  if ((is.numeric(t0) == FALSE) | (length(t0) != 1) | (t0 < 0))
    stop("value of t0 must be numeric (>=0) and of one element")

  fl.control <- list(fit.opt = fit.opt,
                     x_type = x_type,
                     norm_fl = norm_fl,
                     t0 = t0,
                     min.density = min.density,
                     log.x.lin = log.x.lin,
                     log.x.spline = log.x.spline,
                     log.y.lin = log.y.lin,
                     log.y.spline = log.y.spline,
                     lin.h = lin.h,
                     lin.R2 = lin.R2,
                     lin.RSD = lin.RSD,
                     lin.dY = lin.dY,
                     biphasic = biphasic,
                     dr.parameter = dr.parameter,
                     dr.method = dr.method,
                     dr.have.atleast = dr.have.atleast,
                     smooth.dr = smooth.dr,
                     log.x.dr = log.x.dr,
                     log.y.dr = log.y.dr,
                     nboot.dr = nboot.dr,
                     interactive = interactive,
                     nboot.fl = nboot.fl,
                     smooth.fl = smooth.fl,
                     growth.thresh = growth.thresh,
                     suppress.messages = suppress.messages,
                     neg.nan.act = neg.nan.act,
                     clean.bootstrap = clean.bootstrap)
  class(fl.control) <- "fl.control"
  fl.control
}

#' Perform a bootstrap on a dataset containing fluorescence data followed by spline fits for each resample
#'
#' \code{flBootSpline} resamples the fluorescence-'x' value pairs in a dataset with replacement and performs a spline fit for each bootstrap sample.
#'
#' @param time Vector of the independent variable: time (if \code{x_type = 'time'} in \code{fl.control} object.
#' @param density Vector of the independent variable: density (if \code{x_type = 'density'} in \code{fl.control} object.
#' @param fl_data Vector of dependent variable: fluorescence.
#' @param ID (Character) The name of the analyzed sample.
#' @param control A \code{fl.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#'
#' @family fluorescence fitting functions
#'
#' @return A \code{gcBootSpline} object containing a distribution of growth parameters and
#'   a \code{gcFitSpline} object for each bootstrap sample. Use \code{\link{plot.gcBootSpline}}
#'   to visualize all bootstrapping splines as well as the distribution of
#'   physiological parameters.
#'
#' \item{x.in}{Raw x values provided to the function as \code{time} or \code{density}.}
#' \item{fl.in}{Raw fluorescence data provided to the function as \code{fl_data}.}
#' \item{raw.x}{Filtered x values used for the spline fit.}
#' \item{raw.fl}{Filtered fluorescence values used for the spline fit.}
#' \item{ID}{(Character) Identifies the tested sample.}
#' \item{fit.x}{Fitted x values.}
#' \item{fit.fl}{Fitted fluorescence values.}
#' \item{parameters}{List of determined parameters.}
#' \itemize{
#' \item \code{A}: {Maximum fluorescence.}
#' \item \code{dY}: {Difference in maximum fluorescence and minimum fluorescence.}
#' \item \code{max_slope}: {Maximum slope of fluorescence-vs.-x data (i.e., maximum in first derivative of the spline).}
#' \item \code{x.max}: {Time at the maximum slope.}
#' \item \code{lambda}: {Lag time.}
#' \item \code{b.tangent}: {Intersection of the tangent at the maximum slope with the abscissa.}
#' \item \code{max_slope2}: {For biphasic course of fluorescence: Maximum slope of fluorescence-vs.-x data of the second phase.}
#' \item \code{lambda2}: {For biphasic course of fluorescence: Lag time determined for the second phase.}
#' \item \code{x.max2}: {For biphasic course of fluorescence: Time at the maximum slope of the second phase.}
#' \item \code{b.tangent2}: {For biphasic course of fluorescence: Intersection of the tangent at the maximum slope of the second phase with the abscissa.}
#' \item \code{integral}: {Area under the curve of the spline fit.}
#' }
#' \item{spline}{\code{smooth.spline} object generated by the \code{\link{smooth.spline}} function.}
#' \item{spline.deriv1}{list of time ('x') and density ('y') values describing the first derivative of the spline fit.}
#' \item{reliable}{(Logical) Indicates whether the performed fit is reliable (to be set manually).}
#' \item{fitFlag}{(Logical) Indicates whether a spline fit was successfully performed on the data.}
#' \item{fitFlag2}{(Logical) Indicates whether a second phase was identified.}
#' \item{control}{Object of class \code{fl.control} containing list of options passed to the function as \code{control}.}
#'
#' @export
#'
flFitSpline <- function(time = NULL, density = NULL, fl_data, ID = "undefined",
                        control = fl.control(x_type = c("density", "time"), log.x.spline = FALSE, log.y.spline = FALSE, smooth.fl = 0.75, t0 = 0, min.density = NA))
{
  x_type <- control$x_type
  if(!is.null(control$t0) && !is.na(control$t0) && control$t0 != ""){
    t0 <- as.numeric(control$t0)
  } else {
    t0 <- 0
  }

  if (is(control) != "fl.control")
    stop("control must be of class fl.control!")
  if (!any(control$fit.opt %in% "s"))
    stop("Fit option is not set for a fluorescence spline fit. See fl.control()")

  if(!is.null(time))   time.in <- time <- as.vector(as.numeric(as.matrix(time)))[!is.na(as.vector(as.numeric(as.matrix(time))))]
  if(!is.null(density)) density.in <- density <- as.vector(as.numeric(as.matrix(density)))[!is.na(as.vector(as.numeric(as.matrix(density))))]
  fl_data.in <- fl_data <- as.vector(as.numeric(as.matrix(fl_data)))[!is.na(as.vector(as.numeric(as.matrix(fl_data))))]
  bad.values <- (fl_data < 0)
  if (TRUE %in% bad.values) {
    fl_data <- fl_data.in <- fl_data[!bad.values]
    if(x_type == "density"){
      density <- density.in <- density[!bad.values]
    } else {
      time <- time.in <- time[!bad.values]
    }
  }
  if(x_type == "density" && is.null(density))
    stop("To perform a spline fit of fluorescence vs. density data, please provide a 'density' vector of the same length as 'fl_data'.")
  if(x_type == "time" && is.null(time))
    stop("To perform a spline fit of fluorescence vs. time data, please provide a 'time' vector of the same length as 'fl_data'.")
  if(x_type == "density" && length(density) != length(fl_data))
    stop("flFitSpline: length of input vectors (density and fl_data) differ!")
  if(x_type == "time" && length(time) != length(fl_data))
    stop("flFitSpline: length of input vectors (time and fl_data) differ!")
  if (length(fl_data) < 5) {
    cat("flFitSpline: There is not enough valid data. Must have at least 5!")
    flFitSpline <- list(time.in = time.in, density.in = density.in , fl_data.in = fl_data.in, raw.time = time, raw.density = density, raw.fl = fl_data,
                        fit.x = rep(NA, length(get(ifelse(x_type == "density", "density.in", "time.in")))), fit.fl = rep(NA, length(fl_data.in)),
                        parameters = list(A = NA, dY = NA, max_slope = NA, x.max = NA, lambda = NA, b.tangent = NA, max_slope2 = NA, x.max2 = NA,
                                          lambda2 = NA, b.tangent2 = NA, integral = NA),
                        spline = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                        control = control)
    class(flFitSpline) <- "flFitSpline"
    return(flFitSpline)
  }
  # Consider only data points up to max density or time, respectively
  if(x_type == "time"){
    ndx.max <- which.max(time)
    time <- time[1:ndx.max]
    fl_data <- fl_data[1:ndx.max]
  }
  if(x_type == "density"){
    ndx.max <- which.max(density)
    density <- density[1:ndx.max]
    fl_data <- fl_data[1:ndx.max]
    bad.values <- (fl_data < 0)
  }
  fl_data.log <- log(fl_data/fl_data[1])
  if(x_type == "density"){
    bad.values <- (is.na(density)) | (is.na(fl_data)) | fl_data <0 |
      (!is.numeric(density)) | (!is.numeric(fl_data) )
    if (TRUE %in% bad.values) {
      density <- density[!bad.values]
      fl_data <- fl_data[!bad.values]
      fl_data.log <- fl_data.log[!bad.values]
    }
    if (control$log.x.spline == TRUE) {
      bad.values <- (density < 0)
      if (TRUE %in% bad.values) {
        density <- density[!bad.values]
        fl_data <- fl_data[!bad.values]
        fl_data.log <- fl_data.log[!bad.values]
      }
      density.log <- log(density/density[1])
    }

    if(max(density) < control$growth.thresh * density[1]){
      if(control$suppress.messages==F) message(paste0("flFitSpline: No significant growth detected (with all values below ", control$growth.thresh, " * start_value)."))
      flFitSpline <- list(x.in = density.in, fl.in = fl_data.in, raw.x = density, raw.fl = fl_data,
                          fit.x = rep(NA, length(get(ifelse(x_type == "density", "density.in", "time.in")))), fit.fl = rep(NA, length(fl_data.in)),
                          parameters = list(A = NA, dY = NA, max_slope = NA, x.max = NA, lambda = NA, b.tangent = NA, max_slope2 = NA, x.max2 = NA,
                                            lambda2 = NA, b.tangent2 = NA, integral = NA),
                          spline = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                          control = control)
      class(flFitSpline) <- "flFitSpline"
      return(flFitSpline)
    }
    # Implement min.density into dataset
    if(!is.null(control$min.density)) {
      if (!is.na(control$min.density) && control$min.density != 0) {
        min.density <- control$min.density
        if (control$log.y.spline == TRUE) {
          # perfom log transformation on min.density (Ln(y/y0))
          min.density <- log(min.density / density[1])
          fl_data.log <- fl_data.log[density.log >= min.density]
          density.log <- density.log[density.log >= min.density]
        } else {
          fl_data <- fl_data[density >= min.density]
          density <- density[density >= min.density]
        }
      }
    }
    if (control$log.x.spline == FALSE) {
      x <- density
    } else {
      x <- density.log
    }
    if(length(x)<4){
      message("flFitSpline: Not enough data points above the chosen min.density.")
      flFitSpline <- list(x.in = density.in, fl.in = fl_data.in, raw.x = density, raw.fl = fl_data,
                          fit.x = rep(NA, length(get(ifelse(x_type == "density", "density.in", "time.in")))), fit.fl = rep(NA, length(fl_data.in)),
                          parameters = list(A = NA, dY = NA, max_slope = NA, x.max = NA, lambda = NA, b.tangent = NA, max_slope2 = NA, x.max2 = NA,
                                            lambda2 = NA, b.tangent2 = NA, integral = NA),
                          spline = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                          control = control)
      class(flFitSpline) <- "flFitSpline"
      return(flFitSpline)
    }
  } # if(x_type == "density")
  if(x_type == "time"){
    bad.values <- (is.na(time)) | (is.na(fl_data)) | fl_data <0 |
      (!is.numeric(time)) | (!is.numeric(fl_data) )
    if (TRUE %in% bad.values) {
      time <- time[!bad.values]
      fl_data <- fl_data[!bad.values]
      fl_data.log <- fl_data.log[!bad.values]
    }

    if (control$log.x.spline == TRUE) {
      bad.values <- (time <= 0)
      if (TRUE %in% bad.values) {
        time <- time[!bad.values]
        fl_data <- fl_data[!bad.values]
        fl_data.log <- fl_data.log[!bad.values]
      }
      time.log <- log(time)
    }
    if(max(time) < control$t0){
      if(control$suppress.messages==F) message(paste0("flFitSpline: All time values are below the chosen 't0'."))
      flFitSpline <- list(x.in = time.in, fl.in = fl_data.in, raw.x = time, raw.fl = fl_data,
                          fit.x = rep(NA, length(get(ifelse(x_type == "density", "density.in", "time.in")))), fit.fl = rep(NA, length(fl_data.in)),
                          parameters = list(A = NA, dY = NA, max_slope = NA, x.max = NA, lambda = NA, b.tangent = NA, max_slope2 = NA, x.max2 = NA,
                                            lambda2 = NA, b.tangent2 = NA, integral = NA),
                          spline = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                          control = control)
      class(flFitSpline) <- "flFitSpline"
      return(flFitSpline)
    }
    # Implement t0 into dataset
    if(is.numeric(t0) && t0 > 0){
      if (control$log.y.spline == TRUE) {
        fl_data.log <- fl_data.log[which.min(abs(time-t0)):length(fl_data.log)]
      } else {
        fl_data <- fl_data[which.min(abs(time-t0)):length(fl_data)]
      }
      if (control$log.x.spline == FALSE) {
        time <- time[which.min(abs(time-t0)):length(time)]
      } else {
        t0 <- log(t0)
        time.log <- time.log[which.min(abs(time.log-t0)):length(time.log)]
      }
    }
    if (control$log.x.spline == TRUE) {
      x <- time.log
    } else {
      x <- time
    }
    if(length(x)<4){
      message("flFitSpline: Not enough data points above the chosen t0.")
      flFitSpline <- list(x.in = density.in, fl.in = fl_data.in, raw.x = density, raw.fl = fl_data,
                          fit.x = rep(NA, length(get(ifelse(x_type == "density", "density.in", "time.in")))), fit.fl = rep(NA, length(fl_data.in)),
                          parameters = list(A = NA, dY = NA, max_slope = NA, x.max = NA, lambda = NA, b.tangent = NA, max_slope2 = NA, x.max2 = NA,
                                            lambda2 = NA, b.tangent2 = NA, integral = NA),
                          spline = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                          control = control)
      class(flFitSpline) <- "flFitSpline"
      return(flFitSpline)
    }
  } # if(x_type == "time")
  x.in = get(ifelse(x_type == "density", "density.in", "time.in"))
  try(spline <- smooth.spline(x = x,
                              y = if(control$log.y.spline == TRUE){
                                fl_data.log
                              } else {
                                fl_data
                              }, spar = control$smooth.fl, cv = NA, keep.data = FALSE))
  if (!exists("spline") || is.null(spline)) {
    warning("flFitSpline: Spline could not be fitted to data!")
    flFitSpline <- list(x.in = get(ifelse(x_type == "density", "density.in", "time.in")), fl.in = fl_data.in,
                        raw.x = get(ifelse(x_type == "density", "density", "time")), raw.fl = fl_data,
                        fit.x = rep(NA, length(get(ifelse(x_type == "density", "density.in", "time.in")))), fit.fl = rep(NA, length(fl_data.in)),
                        parameters = list(A = NA, dY = NA, max_slope = NA, x.max = NA, lambda = NA, b.tangent = NA, max_slope2 = NA, x.max2 = NA,
                                          lambda2 = NA, b.tangent2 = NA, integral = NA),
                        spline = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                        control = control)
    class(flFitSpline) <- "flFitSpline"
    return(flFitSpline)
  } # if(!exists("spline") || is.null(spline) == TRUE)
  else {
    # Perform spline fit and extract parameters
    deriv1 <- predict(spline, x, deriv = 1)
    # find maximum in deriv1, exclude maxima at beginning of fit, if x_type is "time"
    deriv1.test <- deriv1
    spline.test <- spline
    # if(x_type == "time"){
    # Take only max slope values that are not at the start of the curve
    #   success <- FALSE
    #   while (!success){
    #     if(length(deriv1.test$y) > 2){
    #       max_slope.index <- which.max(deriv1.test$y)
    #       if(!(max_slope.index %in% 1:3)){
    #         max_slope.index <- max_slope.index
    #         success <- TRUE
    #       } else {
    #         deriv1.test <- lapply(1:length(deriv1.test), function(x) deriv1.test[[x]][-max_slope.index])
    #         names(deriv1.test) <- c("x", "y")
    #         spline.test$x <- spline$x[-max_slope.index]
    #         spline.test$y <- spline$y[-max_slope.index]
    #       }
    #     } else{
    #       max_slope.index <- which.max(deriv1$y)
    #       spline.test <- spline
    #       success <- TRUE
    #     }
    #   }
    # } else {
    #   max_slope.index <- which.max(deriv1$y)
    # }
    max_slope.index <- which.max(deriv1$y)
    spline <- spline.test
    max_slope.index.spl <- which(spline$x == deriv1$x[max_slope.index]) # index of data point with maximum slope in spline fit
    x.max <- deriv1$x[max_slope.index] # x of maximum slope
    max_slope <- max(deriv1$y) # maximum value of first derivative of spline fit (i.e., greatest slope in growth curve spline fit)
    y.max <- spline$y[max_slope.index.spl] # cell density at x of max slope
    b.spl <- y.max - max_slope * x.max # the y-intercept of the tangent at µmax
    lambda.spl <- -b.spl/max_slope  # lag x
    integral <- low.integrate(spline$x, spline$y)

    if(control$biphasic) {
      # determine number of data points in period until maximum density
      n.spl <- length(x[which.min(abs(x)):which.max(fl_data)])
      # Find local minima that frame max_slope and remove the 'peak' from deriv1
      n <- round((log(n.spl+4, base=2.1))/0.75)/2
      minima <- inflect(deriv1$y, threshold = n)$minima
      if(length(minima)>0){
        if(length(minima)>1){
          for(i in 1:(length(minima)-1)){
            if(any(minima[i]:minima[i+1] %in% max_slope.index)){
              min.ndx <- c(minima[i], minima[i+1])
            }
            else if(any(minima[i]:length(spline$x) %in% max_slope.index)){
              min.ndx <- c(minima[i], length(spline$x))
            }
            else if(any(1:minima[i] %in% max_slope.index)){
              min.ndx <- c(1, minima[i])
            }
          }
        }
        else if(any(minima:length(spline$x) %in% max_slope.index)){
          min.ndx <- c(minima, length(spline$x))
        }
        else if(any(1:minima %in% max_slope.index)){
          min.ndx <- c(1, minima)
        }
      }
      if(exists("min.ndx")){
        deriv1.2 <- deriv1
        deriv1.2$y[min.ndx[1]:min.ndx[2]] <- 0
        # find second max_slope
        max_slope2.index <- which.max(deriv1.2$y) # index of data point with maximum slope in first derivative fit
        max_slope2.index.spl <- which(spline$x == deriv1.2$x[max_slope2.index]) # index of data point with maximum slope in spline fit
        x.max2 <- deriv1.2$x[max_slope2.index] # x of maximum slope
        max_slope2 <- max(deriv1.2$y) # maximum value of first derivative of spline fit (i.e., greatest slope in growth curve spline fit)
        y.max2 <- spline$y[max_slope2.index.spl] # cell density at x of max slope
        b.spl2 <- y.max2 - max_slope2 * x.max2 # the y-intercept of the tangent at µmax
        lambda.spl2 <- -b.spl2/max_slope2  # lag x
        fitFlag2 <- TRUE
      } else {
        max_slope2.index <- NA
        max_slope2.index.spl <- NA
        x.max2 <- NA
        max_slope2 <- NA
        y.max2 <- NA
        b.spl2 <- NA
        lambda.spl2 <- NA
        fitFlag2 <- FALSE
      }
    } # if(control$biphasic)
    else {
      max_slope2.index <- NA
      max_slope2.index.spl <- NA
      x.max2 <- NA
      max_slope2 <- NA
      y.max2 <- NA
      b.spl2 <- NA
      lambda.spl2 <- NA
      fitFlag2 <- FALSE
    }
  } # else of if (!exists("spline") || is.null(spline) == TRUE)

  flFitSpline <-
    list(
      x.in = get(ifelse(x_type == "density", "density.in", "time.in")),
      fl.in = fl_data.in,
      raw.x = get(ifelse(x_type == "density", "density", "time")),
      raw.fl = fl_data,
      ID = ID,
      fit.x = spline$x,
      fit.fl = spline$y,
      parameters = list(
        A = if (control$log.y.spline == TRUE) {
          # Correct ln(N/N0) transformation for max density value
          fl_data[1] * exp(max(spline$y))
        } else {
          max(spline$y)
        },
        dY = if (control$log.y.spline == TRUE) {
          fl_data[1] * exp(max(spline$y)) -  fl_data[1] * exp(spline$y[1])
        } else {
          max(spline$y) - spline$y[1]
        },
        max_slope = max_slope,
        x.max = x.max,
        lambda = lambda.spl,
        b.tangent = b.spl,
        max_slope2 = max_slope2,
        x.max2 = x.max2,
        lambda2 = lambda.spl2,
        b.tangent2 = b.spl2,
        integral = integral),
      spline = spline,
      spline.deriv1 = deriv1,
      reliable = NULL,
      fitFlag = TRUE,
      fitFlag2 = fitFlag2,
      control = control
    )
  class(flFitSpline) <- "flFitSpline"
  flFitSpline
}

#' flBootSpline: Function to generate a bootstrap
#'
#' \code{fl.gcBootSpline} resamples the fluorescence-'x' value pairs in a dataset with replacement and performs a spline fit for each bootstrap sample.
#'
#' @param time Vector of the independent variable: time (if \code{x_type = 'time'} in \code{fl.control} object.
#' @param density Vector of the independent variable: density (if \code{x_type = 'density'} in \code{fl.control} object.
#' @param fl_data Vector of dependent variable: fluorescence.
#' @param ID (Character) The name of the analyzed sample.
#' @param control A \code{fl.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#'
#' @return A \code{gcBootSpline} object containing a distribution of growth parameters and
#'   a \code{gcFitSpline} object for each bootstrap sample. Use \code{\link{plot.gcBootSpline}}
#'   to visualize all bootstrapping splines as well as the distribution of
#'   physiological parameters.
#' \item{raw.x}{Raw time values provided to the function as \code{time}.}
#' \item{raw.fl}{Raw density data provided to the function as \code{data}.}
#' \item{ID}{(Character) Identifies the tested sample.}
#' \item{boot.x}{Table of time values per column, resulting from each spline fit of the bootstrap.}
#' \item{boot.fl}{Table of density values per column, resulting from each spline fit of the bootstrap.}
#' \item{boot.flSpline}{List of \code{flFitSpline} object, created by \code{\link{flFitSpline}} for each resample of the bootstrap.}
#' \item{lambda}{Vector of estimated lambda (lag time) values from each bootstrap entry.}
#' \item{max_slope}{Vector of estimated max_slope (maximum slope) values from each bootstrap entry.}
#' \item{A}{Vector of estimated A (maximum fluorescence) values from each bootstrap entry.}
#' \item{integral}{Vector of estimated integral values from each bootstrap entry.}
#' \item{bootFlag}{(Logical) Indicates the success of the bootstrapping operation.}
#' \item{control}{Object of class \code{fl.control} containing list of options passed to the function as \code{control}.}
#'
#' @family fluorescence fitting functions
#'
#' @export
#'
flBootSpline <- function(time = NULL, density = NULL, fl_data, ID = "undefined",
                          control = fl.control())
{
  x_type <- control$x_type
  if (is(control) != "fl.control")
    stop("control must be of class fl.control!")
  if (control$nboot.fl == 0)
    stop("Number of bootstrap samples is zero! See ?fl.control")
  if(!is.null(time))   time.in <- time <- as.vector(as.numeric(as.matrix(time)))[!is.na(as.vector(as.numeric(as.matrix(time))))]
  if(!is.null(density)) density.in <- density <- as.vector(as.numeric(as.matrix(density)))[!is.na(as.vector(as.numeric(as.matrix(density))))]
  fl_data.in <- fl_data <- as.vector(as.numeric(as.matrix(fl_data)))[!is.na(as.vector(as.numeric(as.matrix(fl_data))))]
  if (control$log.y.spline == TRUE) {
    fl_data.log <- log(fl_data/fl_data[1])
  }

  if(x_type == "density" && length(density) != length(fl_data))
    stop("flBootSpline: length of input vectors (density and fl_data) differ!")
  if(x_type == "time" && length(time) != length(fl_data))
    stop("flBootSpline: length of input vectors (time and fl_data) differ!")
  # Consider only data points up to max density or time, respectively
  if(x_type == "density"){
    ndx.max <- which.max(density)
    density <- density[1:ndx.max]
    fl_data <- fl_data[1:ndx.max]
    bad.values <- (is.na(density)) | (is.na(fl_data)) |
      (!is.numeric(density)) | (!is.numeric(fl_data) )
    if (TRUE %in% bad.values) {
      density <- density[!bad.values]
      fl_data <- fl_data[!bad.values]
    } else {
      ndx.max <- which.max(time)
      time <- time[1:ndx.max]
      fl_data <- fl_data[1:ndx.max]
      bad.values <- (is.na(time)) | (is.na(fl_data)) |
        (!is.numeric(time)) | (!is.numeric(fl_data) )
      if (TRUE %in% bad.values) {
        time <- time[!bad.values]
        if (control$log.y.spline == TRUE) {
          fl_data.log <- fl_data.log[!bad.values]
        } else {
          fl_data <- fl_data[!bad.values]
        }
      }
    }
    if (control$log.x.spline == TRUE) {
      bad.values <- (density < 0)
      if (TRUE %in% bad.values) {
        density <- density[!bad.values]
        fl_data <- fl_data[!bad.values]
      }
      density.log <- log(density/density[1])
    }
  }

  if (length(fl_data) < 6) {
    warning("flBootSpline: There is not enough valid data. Must have at least 6 unique values!")
    flBootSpline <- list(raw.x = get(ifelse(x_type == "density", "density", "time")), raw.fl = fl_data,
                         ID = ID, boot.x = NA, boot.fl = NA, boot.flSpline = NA,
                         lambda = NA, max_slope = NA, A = NA, integral = NA, bootFlag = FALSE,
                         control = control)
    class(flBootSpline) <- "flBootSpline"
    return(flBootSpline)
  }
  x <- get(ifelse(x_type == "density", "density", "time"))
  A <- NA
  max_slope <- NA
  lambda <- NA
  integral <- NA
  boot.y <- array(NA, c(control$nboot.fl, length(x)))
  boot.x <- array(NA, c(control$nboot.fl, length(x)))
  nonpara <- list()
  control.change <- control
  control.change$fit.opt <- "s"
  if (control$nboot.fl > 0) {
    for (j in 1:control$nboot.fl) {
      choose <- sort(sample(1:length(x), length(x), replace = TRUE))
      while (length(unique(choose)) < 5) {
        choose <- sort(sample(1:length(x), length(x),
                              replace = TRUE))
      }
      x.cur <- x[choose]
      fl.cur <- fl_data[choose]
      if(IQR(x.cur) > 0){
        if(x_type == "density"){
          nonpara[[j]] <- flFitSpline(density = x.cur, fl_data = fl.cur, ID = ID, control = control.change)
        } else {
          nonpara[[j]] <- flFitSpline(time = x.cur, fl_data = fl.cur, ID = ID, control = control.change)
        }

        if(nonpara[[j]]$fitFlag==FALSE | is.na(nonpara[[j]]$fit.fl[1])){
          boot.y[j, 1:length(nonpara[[j]]$fit.x)] <- rep(NA, length(nonpara[[j]]$fit.fl))
          boot.x[j, 1:length(nonpara[[j]]$fit.fl)] <- rep(NA, length(nonpara[[j]]$fit.x))
        }
        else{
          boot.y[j, 1:length(nonpara[[j]]$fit.fl)] <- nonpara[[j]]$fit.fl
          boot.x[j, 1:length(nonpara[[j]]$fit.x)] <- nonpara[[j]]$fit.x
        }
        lambda[j] <- nonpara[[j]]$parameters$lambda
        max_slope[j] <- nonpara[[j]]$parameters$max_slope
        A[j] <- nonpara[[j]]$parameters$A
        integral[j] <- nonpara[[j]]$parameters$integral
      }
    }
    lambda[which(!is.finite(lambda))] <- NA
    max_slope[which(!is.finite(lambda))] <- NA
    A[which(!is.finite(lambda))] <- NA
    integral[which(!is.finite(lambda))] <- NA
    # remove negative values which occured during bootstrap
    lambda[which(lambda < 0)] <- NA
    max_slope[which(max_slope < 0)] <- NA
    A[which(A < 0)] <- NA
    integral[which(integral < 0)] <- NA
  }
  if (control$log.x.spline == TRUE) {
    bad.values <- (x < 0)
    if (TRUE %in% bad.values) {
      time <- x[!bad.values]
      fl_data <- fl_data[!bad.values]
    }
    x.log <- log(1 + x)
  }
  flBootSpline <- list(
    raw.x = if (control$log.x.spline == TRUE) {
      x.log
    } else {
      x
    },
    raw.fl = if (control$log.y.spline == TRUE) {
      fl_data.log
    } else {
      fl_data
    },
    ID = ID,
    boot.x = boot.x,
    boot.fl = boot.y,
    boot.flSpline = nonpara,
    lambda = lambda,
    max_slope = max_slope,
    A = A,
    integral = integral,
    bootFlag = TRUE,
    control = control
  )
  class(flBootSpline) <- "flBootSpline"
  flBootSpline
}

#' Perform a fluorescence curve analysis on all samples in the provided dataset.
#'
#' \code{flFit} performs all computational fluorescence fitting operations based on the user input.
#'
#' @param fl_data Either... \enumerate{ \item a \code{grodata} object created with \code{\link{read_data}} or \code{\link{parse_data}},
#'   \item a list containing a \code{'time'} matrix (for x_type == "time") or \code{'density'} dataframe (for x_type == "density") and, \code{'fluorescence1'} and \code{'fluorescence2'} (if applicable) dataframes,
#'   or \item a dataframe containing (normalized) fluorescence values (if a \code{time} matrix or \code{density} dataframe is provided as separate argument).}
#' @param time (optional) A matrix containing time values for each sample.
#' @param density (optional) A dataframe containing density values for each sample and sample identifiers in the first three columns.
#' @param control A \code{fl.control} object created with \code{\link{fl.control}},
#'   defining relevant fitting options.
#' @param ... Further arguments passed to the shiny app.
#'
#' @return An \code{flFit} object that contains all fluorescence fitting results, compatible with
#'   various plotting functions of the QurvE package.
#' \item{raw.x}{Raw x matrix passed to the function as \code{time} (for x_type = 'time') or \code{density} (for x_type = 'density').}
#' \item{raw.fl}{Raw density dataframe passed to the function as \code{data}.}
#' \item{flTable}{Table with growth parameters and related statistics for each growth curve evaluation performed by the function. This table, which is also returned by the generic \code{\link{summary.flFit}} method applied to a \code{flFit} object, is used as an input for \code{\link{fl.drFit}}.}
#' \item{flFittedLinear}{List of all \code{flFitLinear} objects, generated by the call of \code{\link{flFitLinear}}. Note: access to each object in the list via double brace: flFittedLinear[[#n]].}
#' \item{flFittedSplines}{List of all \code{flFitSpline} objects, generated by the call of \code{\link{flFitSpline}}. Note: access to each object via double brace: flFittedSplines[[#n]].}
#' \item{flBootSplines}{List of all \code{flBootSpline} objects, generated by the call of \code{\link{flBootSpline}}. Note: access to each object via double brace: flFittedSplines[[#n]].}
#' \item{control}{Object of class \code{fl.control} containing list of options passed to the function as \code{control}.}
#'
#' @family workflows
#' @family fluorescence fitting functions
#' @family dose-response analysis functions
#'
#' @return
#' @export
#'
#' @importFrom foreach %dopar%
flFit <- function(fl_data, time = NULL, density = NULL, control= fl.control(), ...)
{
  # Define objects based on additional function calls
  call <- match.call()

  ## remove strictly defined arguments
  call$time <- call$density <- call$fl_data <- call$control <- NULL


  arglist <- sapply(call, function(x) x)
  arglist <- unlist(arglist)[-1]
  ## Assign additional arguments (...) as R objects
  if(length(arglist) > 0){
    for(i in 1:length(arglist)){
      assign(names(arglist)[i], arglist[[i]])
    }
  }

  x_type <- control$x_type

  if(!(class(fl_data) %in% c("list", "grodata"))){
    if (x_type == "time" && is.numeric(as.matrix(time)) == FALSE)
      stop("Need a numeric matrix for 'time' (for x_type = 'time') or a grodata object created with read_data() or parse_data() in the 'fl_data' argument.")
    if (x_type == "density" && is.numeric(as.matrix(density[-1:-3])) == FALSE)
      stop("Need a dataframe for 'density' (for x_type = 'density') or a grodata object created with read_data() or parse_data() in the 'fl_data' argument.")
    if (is.numeric(as.matrix(fl_data[-1:-3])) == FALSE)
      stop("Need a dataframe for 'fl_data' or a grodata object created with read_data() or parse_data() in the 'fl_data' argument.")
  } else if(!is.null(fl_data)){
    time <- fl_data$time
    density <- fl_data$density
    fl_data <- fl_data$fluorescence1
  }
  # /// check if start density values are above min.density in all samples
  if(!is.null(density) && !is.na(density)){
    max.density <- unlist(lapply(1:nrow(density), function (x) max(as.numeric(as.matrix(density[x,-1:-3]))[!is.na(as.numeric(as.matrix(density[x,-1:-3])))])))
    if(is.numeric(control$min.density) && control$min.density != 0){
      if(!is.na(control$min.density) && all(as.numeric(max.density) < control$min.density)){
        stop(paste0("The chosen global start density value (min.density) is larger than every value in your dataset.\nThe maximum value in your dataset is: ",
                    max(as.numeric(max.density))))
      }
    }
  }

  # /// check input parameters
  if (is(control)!="fl.control") stop("control must be of class fl.control!")

  # Check presence of data for chosen fits
  if(x_type == "density" && is.null(density))
    stop("To perform a fits on fluorescence vs. density data, please provide a 'density' data matrix of the same dimensions as 'fl_data'.")
  if(x_type == "time" && is.null(time))
    stop("To perform a fits on fluorescence vs. time data, please provide a 'time' data matrix of the same dimensions as 'fl_data'.")
  # /// check number of datasets
  if(control$x_type == "density"){
    if ( (dim(density)[1])!=(dim(fl_data)[1]) ) stop("flFit: Different number of datasets in fl_data and density")
    x <- density[,-(1:3)]
  }
  if(control$x_type == "time"){
    if ( (dim(time)[1])!=(dim(fl_data)[1]) ) stop("flFit: Different number of datasets in fl_data and time")
    x <- time
  }


  # /// check fitting options
  if (!all(control$fit.opt %in% c("s", "l"))){
    options(warn=1)
    warning("fit.opt must contain 's', and/or 'l'. Changed to c('s', 'l') (both fit methods)!")
    fit.opt=c('s', 'l')
    options(warn=0)
  }

  # /// Initialize some parameters
  out.table       <- NULL
  fitnonpara.all  <- list()
  fitlinear.all <- list()
  boot.all        <- list()
  fitted.param    <- NULL
  fitted.nonparam <- NULL
  bootstrap.param <- NULL
  reliability_tag_linear <- NA
  reliability_tag_nonpara <- NA

  if(control$interactive == FALSE &&
     dim(fl_data)[1] > 30 &&
     (
       ("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ||
       ("s" %in% control$fit.opt && control$nboot.fl > 0)
     )
  ){
    x.ls    <- lapply(1:nrow(x), function(j) x[j, ][!is.na(x[j, ])][!is.na(fl_data[j, -1:-3])])
    wells.ls <- lapply(1:nrow(fl_data), function(j) as.numeric(fl_data[j, -1:-3][!is.na(x[j, ])][!is.na(fl_data[j, -1:-3])]))
    IDs.ls    <- lapply(1:nrow(fl_data), function(j) as.matrix(fl_data[j, 1:3]))
    wellnames.ls <- lapply(1:nrow(fl_data), function(j) paste(as.character(fl_data[j,1]), as.character(fl_data[j,2]),as.character(fl_data[j,3]), sep=" | "))

    # Set up computing clusters (all available processor cores - 1)
    cl <- parallel::makeCluster(parallel::detectCores(all.tests = FALSE, logical = TRUE)-1)
    doParallel::registerDoParallel(cl)

    # Perform linear fits in parallel
    if (("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
      fitlinear.all <- foreach::foreach(i = 1:dim(fl_data)[1]
      ) %dopar% {
        if(control$x_type == "density"){
          QurvE::flFitLinear(density = x.ls[[i]], fl_data = wells.ls[[i]], ID = IDs.ls[[i]], control = control)
        } else {
          QurvE::flFitLinear(time = x.ls[[i]], fl_data = wells.ls[[i]], ID = IDs.ls[[i]], control = control)
        }
      }
    } else {
      # /// generate list with empty objects
      fitlinear.all <- lapply(1:nrow(fl_data), function(j) list(raw.x = x.ls[[j]],
                                                             raw.fl_data = wells.ls[[j]],
                                                             filt.x = NA,
                                                             filt.fl_data = NA,
                                                             log.fl_data = NA,
                                                             ID = IDs.ls[[j]],
                                                             FUN = NA,
                                                             fit = NA,
                                                             par = c(y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                                                                     t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                                                                     tmax2_end = NA),
                                                             ndx = NA, ndx2 = NA,
                                                             quota = NA,
                                                             rsquared = NA, rsquared2 = NA,
                                                             control = control,
                                                             fitFlag = FALSE, fitFlag2 = FALSE)
      )
    }

    # Perform spline bootstrappings in parallel
    if ((("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ) &&
        (control$nboot.fl > 10) ){
      boot.all <- foreach::foreach(i = 1:dim(fl_data)[1]
      ) %dopar% {
        QurvE::flBootSpline(x.ls[[i]], wells.ls[[i]], IDs.ls[[i]], control)
      }
    }
    else{
      # /// create empty gcBootSpline  object
      boot.all            <- lapply(1:nrow(fl_data), function(j) list(raw.x=x.ls[[j]],
                                                                   raw.fl_data=wells.ls[[j]],
                                                                   ID =IDs.ls[[j]],
                                                                   boot.x=NA,
                                                                   boot.y=NA,
                                                                   boot.gcSpline=NA,
                                                                   lambda=NA, mu=NA, A=NA, integral=NA,
                                                                   bootFlag=FALSE, control=control
      )
      )
    }
    parallel::stopCluster(cl = cl)

    # Assign classes to list elements
    for(i in 1:length(fitlinear.all)){
      class(fitlinear.all[[i]]) <- "flFitLinear"
    }
    # for(i in 1:length(fitpara.all)){
    #   class(fitpara.all[[i]]) <- "gcFitModel"
    # }
    # for(i in 1:length(fitnonpara.all)){
    #   class(fitnonpara.all[[i]]) <- "gcFitSpline"
    # }
    for(i in 1:length(boot.all)){
      class(boot.all[[i]]) <- "flBootSpline"
    }
  }

  reliability_tag <- c()

  # /// loop over all wells

  for (i in 1:dim(fl_data)[1]){
    # Progress indicator for shiny app
    if(exists("shiny") && shiny == TRUE){
      shiny::incProgress(
        amount = 1/(dim(fl_data)[1]),
        message = "Computations completed")
    }

    # /// conversion, to handle even data.frame inputs
    actx    <-
      as.numeric(as.matrix(x[i, ]))[!is.na(as.numeric(as.matrix(x[i, ])))][!is.na(as.numeric(as.matrix((fl_data[i, -1:-3]))))]
    actwell <-
      as.numeric(as.matrix((fl_data[i, -1:-3])))[!is.na(as.numeric(as.matrix(x[i, ])))][!is.na(as.numeric(as.matrix((fl_data[i, -1:-3]))))]

    ID    <- as.matrix(fl_data[i,1:3])
    wellname <- paste(as.character(fl_data[i,1]), as.character(fl_data[i,2]),as.character(fl_data[i,3]), sep=" | ")
    if ((control$suppress.messages==FALSE)){
      cat("\n\n")
      cat(paste("=== ", as.character(i), ". [", wellname, "] growth curve =================================\n", sep=""))
      cat("----------------------------------------------------\n")
    }
    if(control$interactive == TRUE ||
       dim(fl_data)[1] <= 30 ||
       !("l" %in% control$fit.opt || "a" %in% control$fit.opt || ("s" %in% control$fit.opt && control$nboot.fl > 10))
    ){
      # /// Linear regression fl_data
      if ("l" %in% control$fit.opt){
        if(control$x_type == "density"){
          fitlinear          <- flFitLinear(density = actx, fl_data = actwell, ID = ID, control = control)
        } else {
          fitlinear          <- flFitLinear(time = actx, fl_data = actwell, ID = ID, control = control)
        }
        fitlinear.all[[i]] <- fitlinear
      }
      else{
        # /// generate empty object
        fitlinear <- list(x.in = actx, fl.in = actwell,
                          raw.x = actx, raw.fl = actwell,
                          filt.x = actx, filt.fl = actwell,
                          ID = ID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                            x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                            x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = 0.95, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
        class(fitlinear)   <- "flFitLinear"
        fitlinear.all[[i]] <- fitlinear
      }
      # /// plot linear fit
      if ((control$interactive == TRUE)) {
        if (("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt)) {
          answer_satisfied <- "n"
          reliability_tag_linear <- NA
          while ("n" %in% answer_satisfied) {
            try(plot(fitlinear, log = ""))
            mtext(side = 3, line = 0, adj = 0,
                  outer = F,
                  cex = 1,
                  wellname)
            answer_satisfied <- readline("Are you satisfied with the linear fit (y/n)?\n\n")
            if ("n" %in% answer_satisfied) {
              test_answer <- readline("Enter: t0, h, quota, min.density, R2, RSD                         >>>>\n\n [Skip (enter 'n'), or adjust fit parameters (see ?flFitLinear).\n Leave {blank} at a given position if standard parameters are desired.]\n\n")
              if ("n" %in% test_answer) {
                cat("\n Tagged the linear fit of this sample as unreliable !\n\n")
                reliability_tag_linear              <- FALSE
                fitlinear$reliable <- FALSE
                fitlinear.all[[i]]$reliable    <- FALSE
                answer_satisfied <- "y"
              } # end if ("n" %in% test_answer)
              else {
                new_params <- unlist(strsplit(test_answer, split = ","))
                t0_new <- ifelse(!is.na(as.numeric(new_params[1])), as.numeric(new_params[1]), control$t0)
                h_new <- if(!is.na(as.numeric(new_params[2]))){
                  as.numeric(new_params[2])
                } else {
                  control$lin.h
                }
                quota_new <- ifelse(!is.na(as.numeric(new_params[3])), as.numeric(new_params[3]), 0.95)
                min.density_new <- ifelse(!is.na(as.numeric(new_params[4])), as.numeric(new_params[4]), control$min.density)
                R2_new <- ifelse(!is.na(as.numeric(new_params[5])), as.numeric(new_params[5]), control$lin.R2)
                RSD_new <- ifelse(!is.na(as.numeric(new_params[6])), as.numeric(new_params[6]), control$lin.RSD)
                control_new <- control
                control_new$t0 <- t0_new
                control_new$lin.h <- h_new
                control_new$lin.R2 <- R2_new
                control_new$lin.RSD <- RSD_new
                if(is.numeric(min.density_new)){
                  if(!is.na(min.density_new) && all(as.vector(actwell) < min.density_new)){
                    message(paste0("Start density values need to be greater than 'min.density'.\nThe minimum start value in your dataset is: ",
                                   min(as.vector(actwell)),". 'min.density' was not adjusted."), call. = FALSE)
                  } else if(!is.na(min.density_new)){
                    control_new$min.density <- min.density_new
                  }
                }
                if ("l" %in% control$fit.opt){
                  if(control$x_type == "density"){
                    fitlinear          <- flFitLinear(density = actx, fl_data = actwell, ID = ID, control = control_new, quota = quota_new)
                  } else {
                    fitlinear          <- flFitLinear(time = actx, fl_data = actwell, ID = ID, control = control_new, quota = quota_new)
                  }
                  fitlinear.all[[i]] <- fitlinear
                }
                fitlinear.all[[i]] <- fitlinear
              } #end else
            } # end if ("n" %in% test_answer)
            else{
              reliability_tag_linear <- TRUE
              fitlinear$reliable <- TRUE
              fitlinear.all[[i]]$reliable <- TRUE
              cat("Sample was (more or less) o.k.\n")
            } # end else
          } # end while ("n" %in% answer_satisfied)
        } # end if (("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt))
      } # end if ((control$interactive == TRUE))
      else {
        reliability_tag_linear <- TRUE
        fitlinear$reliable <- TRUE
        fitlinear.all[[i]]$reliable <- TRUE
      }
    } # # control$interactive == TRUE || dim(fl_data)[1] <= 30


    # /// Non parametric fit
    if ("s" %in% control$fit.opt){
      if(control$x_type == "density"){
        nonpara             <- flFitSpline(density = actx, fl_data = actwell, ID = ID, control = control)
      } else {
        nonpara             <- flFitSpline(time = actx, fl_data = actwell, ID = ID, control = control)
      }
      fitnonpara.all[[i]] <- nonpara
    }
    else{
      # /// generate empty object
      nonpara             <- list(raw.x = actx, raw.fl = actwell,
                                  fit.x = rep(NA, length(actx)), fit.fl = rep(NA, length(actwell)),
                                  parameters = list(A = NA, dY = NA, max_slope = NA, x.max = NA, lambda = NA, b.tangent = NA, max_slope2 = NA, x.max2 = NA,
                                                    lambda2 = NA, b.tangent2 = NA, integral = NA),
                                  spline = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                                  control = control)
      class(nonpara)      <- "flFitSpline"
      fitnonpara.all[[i]] <- nonpara
    }
    # /// plotting parametric fit
    if ((control$interactive == TRUE)) {
      # /// plotting nonparametric fit
      if (("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt)) {
        if (nonpara$fitFlag == TRUE) {
          answer_satisfied <- "n"
          reliability_tag_nonpara <- NA
          while ("n" %in% answer_satisfied) {
            plot(nonpara, add=FALSE, raw=TRUE,slope = T, colData=1, cex=1, plot=T, export=F)
            answer_satisfied <- readline("Are you satisfied with the spline fit (y/n)?\n\n")
            if ("n" %in% answer_satisfied) {
              test_answer <- readline("Enter: smooth.fl, t0, min.density                                        >>>> \n\n [Skip (enter 'n'), or smooth.fl, t0, and min.density (see ?fl.control).\n Leave {blank} at a given position if standard parameters are desired.]\n\n ")
              if ("n" %in% test_answer) {
                cat("\n Tagged the linear fit of this sample as unreliable !\n\n")
                reliability_tag_nonpara              <- FALSE
                nonpara$reliable <- FALSE
                fitnonpara.all[[i]]$reliable    <- FALSE
                fitnonpara.all[[i]]$FitFlag    <- FALSE
                answer_satisfied <- "y"
              } # end if ("n" %in% test_answer)
              else{
                new_params <- unlist(strsplit(test_answer, split = ","))
                if(!is.na(as.numeric(new_params[2])) && as.numeric(new_params[2]) != ""){
                  t0_new <- as.numeric(new_params[2])
                } else {
                  t0_new <- control$t0
                }
                smooth.fl_new <- as.numeric(new_params[1])
                control_new <- control
                if(!is.na(smooth.fl_new) && smooth.fl_new != ""){
                  control_new$smooth.fl <- smooth.fl_new
                }
                control_new$t0 <- t0_new
                min.density_new <- as.numeric(new_params[3])
                if(!is.na(min.density_new)){
                  if(is.numeric(min.density_new) && min.density_new != 0 && all(as.vector(actwell) < min.density_new)){
                    message(paste0("Start density values need to be below 'min.density'.\nThe minimum start value in your dataset is: ",
                                   min(as.vector(data[,4])),". 'min.density' was not adjusted."), call. = FALSE)
                  } else if(!is.na(min.density_new)){
                    control_new$min.density <- min.density_new
                  }
                }
                if(control$x_type == "density"){
                  nonpara             <- flFitSpline(density = actx, fl_data = actwell, ID = ID, control = control_new)
                } else {
                  nonpara             <- flFitSpline(time = actx, fl_data = actwell, ID = ID, control = control_new)
                }
                fitnonpara.all[[i]] <- nonpara
              } #end else
            } # end if ("n" %in% answer_satisfied)
            else{
              reliability_tag_nonpara <- TRUE
              nonpara$reliable <- TRUE
              fitnonpara.all[[i]]$reliable <- TRUE
              fitnonpara.all[[i]]$FitFlag <- TRUE
              cat("Sample was (more or less) o.k.\n")
            } # end else
          } # end while ("n" %in% answer_satisfied)
        } # end if (nonpara$fitFlag == TRUE)
      } # end if (("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt) )
    } # end of if((control$interactive == TRUE))
    else{
      reliability_tag_nonpara <- TRUE
    }

    if(control$interactive == TRUE ||
       dim(fl_data)[1] <= 30 ||
       !("l" %in% control$fit.opt || "a" %in% control$fit.opt || ("s" %in% control$fit.opt && control$nboot.fl > 10))
    ){
      # /// Beginn Bootstrap
      if ((("s" %in% control$fit.opt) ) &&
          (control$nboot.fl > 0) && (reliability_tag_nonpara ==TRUE) && nonpara$fitFlag == TRUE){
        if(control$x_type == "density")   bt <- flBootSpline(density = actx, fl_data = actwell, ID = ID, control = control)
        if(control$x_type == "time")      bt <- flBootSpline(time = actx, fl_data = actwell, ID = ID, control = control)
        boot.all[[i]] <- bt
      } # /// end of if (control$nboot.fl ...)
      else{
        # /// create empty flBootSpline  object
        bt            <- list(raw.x=actx, raw.fl=actwell, ID =ID, boot.x=NA, boot.y=NA, boot.flSpline=NA,
                              lambda=NA, mu=NA, A=NA, integral=NA, bootFlag=FALSE, control=control)
        class(bt)     <- "flBootSpline"
        boot.all[[i]] <- bt
      }
    } # if(interactive == TRUE || dim(fl_data)[1] <= 30 ||
    reliability_tag <- c(reliability_tag, any(reliability_tag_linear, reliability_tag_nonpara))
    # create output table
    # description     <- data.frame(TestId=fl_data[i,1], AddId=fl_data[i,2],concentration=fl_data[i,3],
    #                               reliability_tag=reliability_tag,
    #                               log.x.spline=control$log.x.spline, log.y.spline=control$log.y.spline,
    #                               log.x.lin=control$log.x.lin, log.y.spline=control$log.y.lin, nboot.fl=control$nboot.fl)
    #
    # fitted          <- cbind(description, summary.flFitLinear(fitlinear), summary.flFitSpline(nonpara), summary.flBootSpline(bt))
    #
    # out.table       <- rbind(out.table, fitted)
    # class(out.table) <- c("data.frame", "flTable")

  } # /// end of for (i in 1:dim(fl_data)[1])

  # Assign names to list elements
  names(fitlinear.all) <- names(fitnonpara.all) <- names(boot.all) <- paste0(as.character(fl_data[,1]), " | ", as.character(fl_data[,2]), " | ", as.character(fl_data[,3]))

  # create output table
  description     <- lapply(1:nrow(fl_data), function(x) data.frame(TestId = fl_data[x,1], AddId = fl_data[x,2],concentration = fl_data[x,3],
                                                                 reliability_tag = reliability_tag[x],
                                                                 log.x.spline = control$log.x.spline,
                                                                 log.y.spline = control$log.y.spline,
                                                                 log.x.lin = control$log.x.lin,
                                                                 log.y.lin  =control$log.y.lin,
                                                                 nboot.fl = control$nboot.fl
  )
  )

  fitted          <- lapply(1:length(fitlinear.all), function(x) cbind(description[[x]],
                                                                       summary.flFitLinear(fitlinear.all[[x]]),
                                                                       summary.flFitSpline(fitnonpara.all[[x]]),
                                                                       summary.flBootSpline(boot.all[[x]])
                                                                       )
  )

  out.table       <- do.call(rbind, fitted)
  class(out.table) <- c("data.frame", "flTable")
  flFit           <- list(raw.x = x, raw.fl = fl_data, flTable = out.table, flFittedLinear = fitlinear.all, flFittedSplines = fitnonpara.all, flBootSplines = boot.all, control=control)

  class(flFit)    <- "flFit"
  flFit
}

#' Data fit via a heuristic linear method
#'
#' Determine maximum slopes from using a heuristic approach similar to the ``growth rates made easy''-method of
#' Hall et al. (2013).
#'
#' @param time Vector of the independent time variable (if x_type = "time" in control object).
#' @param density Vector of the independent time density (if x_type = "density" in control object).
#' @param fl_data Vector of the dependent fluorescence variable.
#' @param ID (Character) The name of the analyzed sample.
#' @param quota (Numeric, between 0 an 1) Define what fraction of \code{max_slope} the slope of regression windows adjacent to the window with highest slope should have to be included in the overall linear fit.
#' @param control A \code{fl.control} object created with \code{\link{fl.control}}, defining relevant fitting options.
#'
#' @return A \code{gcFitLinear} object with parameters of the fit. The lag time is
#'   estimated as the intersection between the fit and the horizontal line with
#'   \eqn{y=y_0}, where \code{y0} is the first value of the dependent variable.
#'   Use \code{\link{plot.gcFitSpline}} to visualize the linear fit.
#' \item{raw.x}{Filtered x values used for the spline fit.}
#' \item{raw.fl}{Filtered fluorescence values used for the spline fit.}
#' \item{filt.x}{Filtered x values.}
#' \item{filt.fl}{Filtered fluorescence values.}
#' \item{ID}{(Character) Identifies the tested sample.}
#' \item{FUN}{Linear _function_ used for plotting the tangent at mumax.}
#' \item{fit}{\code{lm} object; result of the final call of \code{\link{lm}} to perform the linear regression.}
#' \item{par}{List of determined growth parameters.}
#' \itemize{
#' \item \code{y0}: {Minimum fluorescence value considered for the heuristic linear method.}
#' \item \code{dY}: {Difference in maximum fluorescence and minimum fluorescence}
#' \item \code{A}: {Maximum fluorescence}
#' \item \code{y0_lm}: {Intersection of the linear fit with the abscissa.}
#' \item \code{max_slope}: {Maximum slope of the linear fit.}
#' \item \code{tD}: {Doubling time.}
#' \item \code{slope.se}: {Standard error of the maximum slope.}
#' \item \code{lag}: {Lag X.}
#' \item \code{x.max_start}: {X value of the first data point within the window used for the linear regression.}
#' \item \code{x.max_end}: {X value of the last data point within the window used for the linear regression.}
#' \item \code{x.turn}: {For biphasic: X at the inflection point that separates two phases.}
#' \item \code{max.slope2}: {For biphasic: Slope of the second phase.}
#' \item \code{tD2}: {Doubling time of the second phase.}
#' \item \code{y0_lm2}: {For biphasic: Intersection of the linear fit of the second phase with the abscissa.}
#' \item \code{lag2}: {For biphasic: Lag time determined for the second phase..}
#' \item \code{x.max2_start}: {For biphasic: X value of the first data point within the window used for the linear regression of the second phase.}
#' \item \code{x.max2_end}: {For biphasic: X value of the last data point within the window used for the linear regression of the second phase.}
#' }
#' \item{ndx}{Index of data points used for the linear regression.}
#' \item{ndx2}{Index of data points used for the linear regression for the second phase.}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#' \item{rsquared}{\ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} of the linear regression.}
#' \item{rsquared2}{\ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} of the linear regression for the second phase.}
#' \item{fitFlag}{(Logical) Indicates whether linear regression was successfully performed on the data.}
#' \item{fitFlag2}{(Logical) Indicates whether a second phase was identified.}
#' \item{reliable}{(Logical) Indicates whether the performed fit is reliable (to be set manually).}
#'
#' @references Hall, BG., Acar, H, Nandipati, A and Barlow, M (2014) Growth Rates Made Easy. Mol. Biol. Evol. 31: 232-38, DOI: 10.1093/molbev/mst187
#' @references Petzoldt T (2022). _growthrates: Estimate Growth Rates from Experimental Data_. R package version 0.8.3, <https://CRAN.R-project.org/package=growthrates>.
#'
#' @export
#'
flFitLinear <- function(time = NULL, density = NULL, fl_data, ID = "undefined",  quota = 0.95,
                        control = fl.control(x_type = c("density", "time"), t0 = 0, min.density = NA, lin.h = NULL, lin.R2 = 0.98, lin.RSD = 0.05, lin.dY = 0.05, biphasic = FALSE))
{
  x_type <- control$x_type
  R2 <- control$lin.R2
  RSD <- control$lin.RSD
  h <- control$lin.h
  t0 <- control$t0
  min.density <- control$min.density

  if (is(control) != "fl.control")
    stop("control must be of class fl.control!")
  if (!any(control$fit.opt %in% "l"))
    stop("Fit option is not set for a fluorescence linear fit. See fl.control()")

  if(!is.null(time))   time.in <- time <- as.vector(as.numeric(as.matrix(time)))[!is.na(as.vector(as.numeric(as.matrix(time))))]
  if(!is.null(density)) density.in <- density <- as.vector(as.numeric(as.matrix(density)))[!is.na(as.vector(as.numeric(as.matrix(density))))]
  fl_data.in <- fl_data <- as.vector(as.numeric(as.matrix(fl_data)))[!is.na(as.vector(as.numeric(as.matrix(fl_data))))]

  if(!is.null(t0) && !is.na(t0) && t0 != ""){
    t0 <- as.numeric(t0)
  } else {
    t0 <- 0
  }
  if(x_type == "density" && is.null(density))
    stop("flFitLinear: To perform a linear fit of fluorescence vs. density data, please provide a 'density' vector of the same length as 'fl_data'.")
  if(x_type == "time" && is.null(time))
    stop("flFitLinear: To perform a linear fit of fluorescence vs. time data, please provide a 'time' vector of the same length as 'fl_data'.")
  if(x_type == "density" && length(density) != length(fl_data))
    stop("flFitLinear: length of input vectors (density and fl_data) differ!")
  if(x_type == "time" && length(time) != length(fl_data))
    stop("flFitLinear: length of input vectors (time and fl_data) differ!")
  if (length(fl_data) < 5) {
    cat("flFitLinear: There is not enough valid data. Must have at least 5!")
  }

  # Consider only data points up to max density or time, respectively
  if(x_type == "time"){
    ndx.max <- which.max(time)
    time <- time[1:ndx.max]
    fl_data <- fl_data[1:ndx.max]
    bad.values <- (fl_data < 0)
    if (TRUE %in% bad.values) {
      fl_data <- fl_data[!bad.values]
      time <- time[!bad.values]
    }
  }
  if(x_type == "density"){
    ndx.max <- which.max(density)
    density <- density[1:ndx.max]
    fl_data <- fl_data[1:ndx.max]
    bad.values <- (fl_data < 0)
    if (TRUE %in% bad.values) {
      fl_data <- fl_data[!bad.values]
      density <- density[!bad.values]
    }
  }

  fl_data.log <- log(fl_data/fl_data[1])

  if(x_type == "density"){
    bad.values <- (is.na(density)) | (is.na(fl_data)) |
      (!is.numeric(density)) | (!is.numeric(fl_data) )
    if (TRUE %in% bad.values) {
      density <- density[!bad.values]
      fl_data <- fl_data[!bad.values]
    }
    if (control$log.x.lin == TRUE) {
      bad.values <- (density < 0)
      if (TRUE %in% bad.values) {
        density <- density[!bad.values]
        fl_data <- fl_data[!bad.values]
      }
      density.log <- log(density/density[1])
    }

    if(max(density) < control$growth.thresh * density[1]){
      if(control$suppress.messages==F) message(paste0("flFitLinear: No significant growth detected (with all values below ", control$growth.thresh, " * start_value)."))
      flFitLinear <- list(x.in = get(ifelse(x_type == "density", "density.in", "time.in")), fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "density", "density.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "density", "density", "time")), filt.fl = fl_data,
                          ID = ID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                            x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                            x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
      class(flFitLinear) <- "flFitLinear"
      return(flFitLinear)
    }
    # Implement min.density into dataset
    # if(!is.null(control$min.density)) {
    #   if (!is.na(control$min.density) && control$min.density != 0) {
    #     min.density <- control$min.density
    #     if (control$log.y.lin == TRUE) {
    #       # perfom log transformation on min.density (Ln(y/y0))
    #       min.density <- log(min.density / density[1])
    #       fl_data.log <- fl_data.log[density.log >= min.density]
    #       density.log <- density.log[density.log >= min.density]
    #     } else {
    #       fl_data <- fl_data[density >= min.density]
    #       density <- density[density >= min.density]
    #     }
    #   }
    # }

    # Remove data points where y values stack on top of each other
    fl_data <- fl_data[density >= cummax(density)]
    fl_data.log <- fl_data.log[density >= cummax(density)]
    density <- density[density >= cummax(density)]
    if (control$log.x.lin == FALSE) {
      x <- density
    } else {
      x <- density.log
    }
    if(length(x)<4){
      message("flFitLinear: Not enough data points above the chosen min.density.")
      flFitLinear <- list(x.in = get(ifelse(x_type == "density", "density.in", "time.in")), fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "density", "density.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "density", "density", "time")), filt.fl = fl_data,
                          ID = ID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                            x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                            x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
      class(flFitLinear) <- "flFitLinear"
      return(flFitLinear)
    }
  } # if(x_type == "density")
  if(x_type == "time"){
    bad.values <- (is.na(time)) | (is.na(fl_data)) |
      (!is.numeric(time)) | (!is.numeric(fl_data) )
    if (TRUE %in% bad.values) {
      time <- time[!bad.values]
      if (control$log.y.lin == TRUE) {
        fl_data.log <- fl_data.log[!bad.values]
        fl_data.in <- fl_data[!bad.values]
      } else {
        fl_data <- fl_data[!bad.values]
      }
    }

    if (control$log.x.lin == TRUE) {
      bad.values <- (time <= 0)
      if (TRUE %in% bad.values) {
        time <- time[!bad.values]
        if (control$log.y.lin == TRUE) {
          fl_data.log <- fl_data.log[!bad.values]
          fl_data.in <- fl_data[!bad.values]
        } else {
          fl_data <- fl_data[!bad.values]
        }
      }
      time.log <- log(time)
    }
    if(max(time) < control$t0){
      if(control$suppress.messages==F) message(paste0("flFitLinear: All time values are below the chosen 't0'."))
      flFitLinear <- list(x.in = get(ifelse(x_type == "density", "density.in", "time.in")), fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "density", "density.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "density", "density", "time")), filt.fl = fl_data,
                          ID = ID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                            x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                            x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
      class(flFitLinear) <- "flFitLinear"
      return(flFitLinear)
    }
    # Implement t0 into dataset
    # if(is.numeric(t0) && t0 > 0){
    #   if (control$log.y.lin == TRUE) {
    #     fl_data.log <- fl_data.log[which.min(abs(x-t0)):length(fl_data.log)]
    #   } else {
    #     fl_data <- fl_data[which.min(abs(time-t0)):length(fl_data)]
    #   }
    #   if (control$log.x.lin == FALSE) {
    #     time <- time[which.min(abs(time-t0)):length(time)]
    #   } else {
    #     t0 <- log(t0)
    #     time.log <- time.log[which.min(abs(time.log-t0)):length(time.log)]
    #   }
    # }
    if (control$log.x.lin == TRUE) {
      x <- time.log
    } else {
      x <- time
    }
    if(length(x)<4){
      message("flFitLinear: Not enough data points above the chosen t0.")
      flFitLinear <- list(x.in = get(ifelse(x_type == "density", "density.in", "time.in")), fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "density", "density.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "density", "density", "time")), filt.fl = fl_data,
                          ID = ID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                            x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                            x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
      class(flFitLinear) <- "flFitLinear"
      return(flFitLinear)
    }
  } # if(x_type == "time")
  # extract period of growth (from defined t0)
  x.in = get(ifelse(x_type == "density", "density.in", "time.in"))
  end <- FALSE
  step <- 0
  while(end==FALSE){
    step <- step+1
    fldat <- fl_data[step:length(fl_data)]
    fl.max.ndx <- which.max(fldat)
    if(fl.max.ndx == 1) next
    else fl.max.ndx <- fl.max.ndx + (step); end = TRUE
  }
  t.growth <- x[which.min(abs(x)):fl.max.ndx]
  if(!is.null(h) && !is.na(h) && h != ""){
    h <- as.numeric(h)
  } else {
    # determine number of fl_data points in period until maximum density
    n.spl <- length(t.growth)
    # Calculate h via log-transformation of the number of fl_data points
    # if(n.spl <= 100){
    h <- round((log(n.spl+4, base=2.1))/0.75)
    #test h calculation
    # s <- c(1:500)
    # plot(s, round((log(s+4, base=2.1))/0.75))
    # } else {
    #   h <- round((log(n.spl/30, base=1.1))*0.75)
    #   #test h calculation
    #    s <- c(100:500)
    #    plot(s, round((log(s/20, base=1.15))*0.75))
    # }
  }

  if(!is.null(quota) && !is.na(quota) && quota != ""){
    if(quota > 1){
      quota <- as.numeric(quota)/100
    } else {
      quota <- as.numeric(quota)
    }
  } else {
    quota <- 0.95
  }
  if(!is.null(R2) && !is.na(R2) && R2 != ""){
    R2 <- as.numeric(R2)
  } else {
    R2 <- 0.95
  }
  if(!is.null(RSD) && !is.na(RSD) && RSD != ""){
    RSD <- as.numeric(RSD)
  } else {
    RSD <- 0.05
  }
  fl_data.log <- log(fl_data/fl_data[1])

  bad.values <- ((is.na(fl_data.log))|(is.infinite(fl_data.log))|(is.na(x))|(is.na(fl_data.log)))

  # /// remove bad values or stop program
  if (TRUE%in%bad.values){
      x    <- x[!bad.values]
      fl_data.log    <- fl_data.log[!bad.values]
      fl_data <- fl_data[!bad.values]
  }

  # store filtered and transformed fl_data
  obs <- data.frame(x, fl_data)
  obs$ylog <- fl_data.log



  ## number of values
  N <- nrow(obs)

  if(N > h && N>3){
    # Perform linear regression for all N windows and save results in 'ret'
    ret <- matrix(0, nrow = N - h, ncol = 6)
    if (control$log.y.lin == TRUE) {
      for(i in 1:(N - h)) {
        ret[i, ] <- c(i, with(obs, (lm_parms(lm_window(x, ylog, i0 = i, h = h)))))
      }
    } else {
      for(i in 1:(N - h)) {
        ret[i, ] <- c(i, with(obs, (lm_parms(lm_window(x, fl_data, i0 = i, h = h)))))
      }
    }

    colnames(ret) <- c("index", "y-intersect", "slope", "X4", "R2", "RSD")
    # add x and density values as columns in ret
    if (control$log.y.lin == TRUE) {
      ret <- data.frame(ret, x = x[ret[,1]], fl_data = obs$ylog[ret[,1]])
    } else {
      ret <- data.frame(ret, x = x[ret[,1]], fl_data = obs$fl_data[ret[,1]])
    }

    bad <- is.na(ret[,5]) | is.na(ret[,6])
    ret <- ret[!bad,]


    if(x_type == "density"){
      ret <- ret[which.min(abs(ret$fl_data-min.density)) : nrow(ret),] # consider only slopes from defined t0 and min.density
    } else{
      ret <- ret[which.min(abs(x-t0)):nrow(ret),] # consider only slopes from defined t0
    }
    ret <- ret[!is.na(ret[,1]), ]

    if(nrow(ret)<2){
      flFitLinear <- list(x.in = x.in, fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "density", "density.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "density", "density", "time")), filt.fl = fl_data,
                          ID = ID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                            x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                            x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
      class(flFitLinear) <- "flFitLinear"
      if(control$suppress.messages==F) message("No fl_data range in accordance with the chosen parameters identified with appropriate linearity.")
      return(flFitLinear)
    }
    else{
      # duplicate ret for further tuning of fit
      ret.check <- ret

      # Consider only positive slopes
      ret.check <- ret.check[ret.check[,"slope"]>0, ]

      if(nrow(ret.check)<2){
        flFitLinear <- list(x.in = x.in, fl.in = fl_data.in,
                            raw.x = get(ifelse(x_type == "density", "density.in", "time.in")), raw.fl = fl_data.in,
                            filt.x = get(ifelse(x_type == "density", "density", "time")), filt.fl = fl_data,
                            ID = ID, FUN = grow_exponential, fit = NA, par = c(
                              y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                              x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                              x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
        class(flFitLinear) <- "flFitLinear"
        if(control$suppress.messages==F) message("No fl_data range in accordance with the chosen parameters identified with appropriate linearity.")
        return(flFitLinear)
      } else {
        ## Determine index of window with maximum slope, iterate until regression is found that meets R2 and RSD criterion
        success <- FALSE
        if(any(ret.check[,5] >= R2 & abs(ret.check[,6]) <= RSD)){
          while (!success){
            index.max <- which.max(ret.check[, 3])
            if(ret.check[index.max,5] >= R2 && abs(ret.check[index.max,6]) <= RSD && !is.na(ret.check[index.max,6]) ){ # prerequisites for suitable µmax candidate: R2 and RSD
              slope.max <- ret.check[index.max,3]
              success <- TRUE
            } else {
              ret.check <- ret.check[-index.max,]
            }
          }
          index.max.ret <- ret.check[which(ret.check[,3]==slope.max),1] # index of maximum slope in fit table
          slope.quota <- quota * slope.max
          if(x_type == "density"){
            if(exists("min.density")){
              candidates <- which(ret[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                    ret[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                    abs(ret[, 6]) <= 1.02 * RSD & # RSD criterion for candidates
                                    ret[, 7] >= min.density  # consider only slopes at densities higher than "min.density"
                                  )
            } else{
              candidates <- which(ret[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                    ret[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                    abs(ret[, 6]) <= 1.02 * RSD # RSD criterion for candidates
                                  )
            }
          } else {
            if(exists("t0")){
              candidates <- which(ret[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                    ret[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                    abs(ret[, 6]) <= 1.02 * RSD & # RSD criterion for candidates
                                    ret[, 7] >= t0 # consider only slopes after defined t0
                                  )
            } else {
              candidates <- which(ret[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                    ret[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                    abs(ret[, 6]) <= 1.02 * RSD  # RSD criterion for candidates
                                  )
            }
          }
          candidates <- ret[candidates,1]

          #consider only candidate windows next to index.max.ret
          candidate_intervals <- split(candidates, cumsum(c(1, diff(candidates) != 1)))
          if(any(index.max.ret %in% unlist(candidate_intervals))){
            candidates <-
              candidate_intervals[as.numeric(which(
                sapply(
                  candidate_intervals,
                  FUN = function(X)
                    index.max.ret %in% X
                )
              ))][[1]]
          }

          if(length(candidates) > 0) {
            # perform linear regression with candidate fl_data points
            tp <- seq(min(candidates), max(candidates) + h-1)
            if (control$log.y.lin == TRUE) {
              m <- lm_window(obs$x, obs$ylog, min(tp), length(tp)) # linear model
            } else {
              m <- lm_window(obs$x, obs$fl_data, min(tp), length(tp)) # linear model
            }
            p  <- c(lm_parms(m), n=length(tp)) # # slope equation parameters (linear model)
            ## get x window of linear fit
            x.max_start <- obs$x[tp[1]]
            x.max_end <- obs$x[tp[length(tp)]]

            y0_lm    <- unname(coef(m)[1]) # y-intercept of tangent

            if(x_type == "time"){
              if(control$log.y.lin == TRUE){
                y0_data  <- obs$ylog[obs$x>=t0][1] # y0 in dataset
              } else {
                y0_data  <- obs$fl_data[obs$x>=t0][1] # y0 in dataset
              }
            } else {
              if(control$log.y.lin == TRUE){
                y0_data  <- obs$ylog[obs$x>=min.density][1] # y0 in dataset
              } else {
                y0_data  <- obs$fl_data[obs$x>=min.density][1] # y0 in dataset
              }
            }

            max_slope <- unname(coef(m)[2])

            ## estimate lag phase
            lambda <- (y0_data - y0_lm) / max_slope

            # correct y0 values for Ln(y(t)/y0)
            if(control$log.y.lin == TRUE){
              y0_lm <- obs$fl_data[1] * exp(y0_lm)
            }

            # get indices of x points used in linear fit
              # ndx <- seq(min(match(ret[candidates, "x"], x.in)),
              #            max(match(ret[candidates, "x"], x.in)) + h-1)
            ndx <- tp
            ndx.in <- match(fl_data[tp], fl_data.in)

            slope.se <- as.numeric(p[3]) # standard error of slope
            fitFlag <- TRUE

          }
          else { # of if(length(candidates) > 0)
            flFitLinear <- list(x.in = x.in, fl.in = fl_data.in,
                                raw.x = get(ifelse(x_type == "density", "density.in", "time.in")), raw.fl = fl_data.in,
                                filt.x = get(ifelse(x_type == "density", "density", "time")), filt.fl = fl_data,
                                ID = ID, FUN = grow_exponential, fit = NA, par = c(
                                  y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                                  x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                                  x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
            class(flFitLinear) <- "flFitLinear"
            if(!control$suppress.messages) message(paste0("No linear fit in accordance with the chosen parameters identified with: R2 >= ", R2, ", RSD <= ", RSD, ", t0 = ", t0, ", and min.density = ", control$min.density, "."))
            return(flFitLinear)
          }
          if(control$biphasic) {
            if(control$log.y.lin == TRUE){
              spline <- stats::smooth.spline(obs$x, obs$ylog, spar = 0.5, cv = NA, keep.data = FALSE)
            } else {
              spline <- stats::smooth.spline(obs$x, obs$fl_data, spar = 0.55, cv = NA, keep.data = FALSE)
            }
            deriv2 <- stats::predict(spline, deriv = 2)
            deriv2_spline <- stats::smooth.spline(deriv2$x, deriv2$y, spar = 0.4, cv = NA, keep.data = FALSE)
            # extract local minima and maxima of deriv2
            n = h/2
            minima <- inflect(deriv2_spline$y, threshold = n)$minima
            maxima <- inflect(deriv2_spline$y, threshold = n)$maxima
            # Regard only (negative) minima and (positive) maxima with a deriv2 value of >= 10% max_slope
            minima <- minima[deriv2_spline$y[minima] <= 0.1 * (-max_slope)]
            maxima <- maxima[deriv2_spline$y[maxima] >= 0.1 * max_slope]

            # expand max_slope window with more relaxed quota
            slope.quota.ext <- 0.8 * quota * slope.max
            if(x_type == "density"){
              if(exists("min.density")){
                candidates.ext <- which(ret[, 3] >= slope.quota.ext & # indices of slopes greater than slope.quota
                                      ret[, 5] >= 0.95*R2 & # R2 criterion for candidates
                                      abs(ret[, 6]) <= 1.1 * RSD & # RSD criterion for candidates
                                      ret[, 7] >= min.density  # consider only slopes at densities higher than "min.density"
                )
              } else{
                candidates.ext <- which(ret[, 3] >= slope.quota.ext & # indices of slopes greater than slope.quota
                                      ret[, 5] >= 0.95*R2 & # R2 criterion for candidates
                                      abs(ret[, 6]) <= 1.1 * RSD # RSD criterion for candidates
                )
              }
            } else {
              if(exists("t0")){
                candidates.ext <- which(ret[, 3] >= slope.quota.ext & # indices of slopes greater than slope.quota
                                      ret[, 5] >= 0.95*R2 & # R2 criterion for candidates
                                      abs(ret[, 6]) <= 1.1 * RSD & # RSD criterion for candidates
                                      ret[, 7] >= t0 # consider only slopes after defined t0
                )
              } else {
                candidates.ext <- which(ret[, 3] >= slope.quota.ext & # indices of slopes greater than slope.quota
                                      ret[, 5] >= 0.95*R2 & # R2 criterion for candidates
                                      abs(ret[, 6]) <= 1.1 * RSD  # RSD criterion for candidates
                )
              }
            }
            #consider only candidate windows next to index.max.ret
            candidate_intervals.ext <- split(candidates.ext, cumsum(c(1, diff(candidates.ext) != 1)))
            if(index.max.ret %in% unlist(candidate_intervals.ext)){
              candidates.ext <-
                candidate_intervals.ext[as.numeric(which(
                  sapply(
                    candidate_intervals.ext,
                    FUN = function(X)
                      index.max.ret %in% X
                  )
                ))][[1]]
            }

            #Regard only local minima after candidates.ext
            minima <- minima[minima > candidates.ext[length(candidates.ext)]]

            #Regard only local maxima before candidates.ext
            maxima <- maxima[maxima < candidates.ext[1]]

            tp.ext <- seq(min(candidates.ext), max(candidates.ext) + h-1)
            if(length(minima)>0){
              # get local deriv2-minimum after max_slope
              postmin.ndx <- minima[which.min(abs(minima - tp.ext[length(tp.ext)]))]
              # extract linear regression results after post-max_slope turning point
              ret.postmin <- ret[ret$x >= obs$x[postmin.ndx],]
              # remove indices included in extended max_slope regression
              ret.postmin <- ret.postmin[!(ret.postmin[,1] %in% tp.ext),]
              # Consider only positive slopes that are at least 10% of max_slope
              ret.postmin <- ret.postmin[ret.postmin[, "slope"] > 0 & ret.postmin[, "slope"] >= 0.1*max_slope, ]
              ## Determine index of window with maximum slope, iterate until regression is found that meets R2 and RSD criterion
              success <- FALSE
              # apply min.density to list of linear regressions
              if(x_type == "density"){
                ret.postmin.check <- ret.postmin[ret.postmin[, 7] >= min.density, ]
              } else {
                ret.postmin.check <- ret.postmin
              }

              if (any(ret.postmin.check[, 5] >= R2 &
                      abs(ret.postmin.check[, 6]) <= RSD)) {
                while (!success) {
                  index.max <- which.max(ret.postmin.check[, 3])
                  if (ret.postmin.check[index.max, 5] >= R2 &&
                      abs(ret.postmin.check[index.max, 6]) <= RSD &&
                      !is.na(ret.postmin.check[index.max, 6])) {
                    # prerequisites for suitable µmax candidate: R2 and RSD
                    slope.max.postmin <- ret.postmin.check[index.max, 3]
                    success <- TRUE
                  } else {
                    ret.postmin.check <- ret.postmin.check[-index.max,]
                  }
                }
                index.max.ret.postmin <- ret.postmin[which(ret.postmin[, 3] == slope.max.postmin), 1] # index of maximum slope in fit table
                slope.quota <- quota * slope.max.postmin

                if(x_type == "density"){
                  if(exists("min.density")){
                    candidates.postmin <- ret.postmin[which(
                      ret.postmin[, 3] >= slope.quota &
                        ret.postmin[, 5] >= 0.98 * R2 &
                        abs(ret.postmin[, 6]) <= 1.02 * RSD &
                        ret.postmin[, 7] >= min.density), 1]
                  } else{
                    candidates.postmin <- ret.postmin[which(
                      ret.postmin[, 3] >= slope.quota &
                        ret.postmin[, 5] >= 0.98 * R2 &
                        abs(ret.postmin[, 6]) <= 1.02 * RSD), 1]
                  }
                } else {
                  if(exists("t0")){
                    candidates.postmin <- ret.postmin[which(
                      ret.postmin[, 3] >= slope.quota &
                        ret.postmin[, 5] >= 0.98 * R2 &
                        abs(ret.postmin[, 6]) <= 1.02 * RSD &
                        ret.postmin[, 7] >= t0), 1]
                  } else {
                    candidates.postmin <- ret.postmin[which(
                      ret.postmin[, 3] >= slope.quota &
                        ret.postmin[, 5] >= 0.98 * R2 &
                        abs(ret.postmin[, 6]) <= 1.02 * RSD), 1]
                  }
                }
                #consider only candidate windows next to index.max.ret.postmin
                candidate_intervals.postmin <- split(candidates.postmin, cumsum(c(1, diff(candidates.postmin) != 1)))
                if (index.max.ret.postmin %in% unlist(candidate_intervals.postmin)) {
                  candidates.postmin <- candidate_intervals.postmin[
                    as.numeric(which(sapply(candidate_intervals.postmin,FUN = function(X) index.max.ret.postmin %in% X)))][[1]]
                }

                if (length(candidates.postmin) > 0) {
                  #perform linear regression with candidate fl_data points
                  tp.postmin <- seq(min(candidates.postmin), max(candidates.postmin) + h - 1)
                  if (control$log.y.lin == TRUE) {
                    m.postmin <- lm_window(obs$x, obs$ylog, min(tp.postmin), length(tp.postmin)) # linear model
                  } else {
                    m.postmin <- lm_window(obs$x, obs$fl_data, min(tp.postmin), length(tp.postmin)) # linear model
                  }
                  p.postmin  <- c(lm_parms(m.postmin), n = length(tp.postmin)) # # slope equation parameters (linear model)
                } else {
                  p.postmin <- c(
                    a = 0,
                    b = 0,
                    se = 0,
                    r2 = 0,
                    cv = 0,
                    n = 0
                  )
                  m = NULL
                }

                if (length(candidates.postmin) > 0) {
                  ## get x window of exponential fit
                  x.max_start.postmin <- obs$x[tp.postmin[1]]
                  x.max_end.postmin <- obs$x[tp.postmin[length(tp.postmin)]]
                  if (control$log.y.lin == TRUE) {
                    y_turn.postmin <- obs$ylog[postmin.ndx] # y at turning point
                  } else {
                    y_turn.postmin <- obs$fl_data[postmin.ndx] # y at turning point
                  }
                  x.turn.postmin <- obs$x[postmin.ndx]
                  y0_lm.postmin    <- unname(coef(m.postmin)[1]) # y-intercept of tangent
                  max_slope.postmin <- unname(coef(m.postmin)[2])

                  ## estimate lag phase between first and second growth phase
                  if (control$log.y.lin == TRUE) {
                    lambda.postmin <- (obs$ylog[1] - y0_lm.postmin) / max_slope.postmin
                  } else {
                    lambda.postmin <- (obs$fl_data[1] - y0_lm.postmin) / max_slope.postmin
                  }

                  # correct y0 values for Ln(y(t)/y0)
                  if(control$log.y.lin == TRUE){
                    y0_lm.postmin <- obs$fl_data[1] * exp(y0_lm.postmin)
                  }

                  # get indices of x points used in linear fit
                  ndx.postmin <- seq(min(match(ret[candidates.postmin, "x"], x.in)),
                                     max(match(ret[candidates.postmin, "x"], x.in)) + h -
                                       1)
                  slope.se.postmin <- as.numeric(p.postmin[3]) # standard error of slope
                  rsquared.postmin <- p.postmin["r2"]
                  fitFlag.postmin <- TRUE
                }
                else {
                  # of if(length(candidates.postmin) > 0)
                  y0_lm.postmin = NA
                  max_slope.postmin = NA
                  slope.se.postmin = NA
                  lag.postmin = NA
                  x.max_start.postmin = NA
                  x.max_end.postmin = NA
                  ndx.postmin = NA
                  rsquared.postmin = NA
                  fitFlag.postmin = FALSE
                }
              } # if (any(ret.postmin.check[, 5] >= R2 & abs(ret.postmin.check[, 6]) <= RSD))
              else {
                y0_lm.postmin = NA
                max_slope.postmin = NA
                slope.se.postmin = NA
                lag.postmin = NA
                x.max_start.postmin = NA
                x.max_end.postmin = NA
                ndx.postmin = NA
                rsquared.postmin = NA
                fitFlag.postmin = FALSE
              }
            }
            else {
              y0_lm.postmin = NA
              max_slope.postmin = NA
              slope.se.postmin = NA
              lag.postmin = NA
              x.max_start.postmin = NA
              x.max_end.postmin = NA
              ndx.postmin = NA
              rsquared.postmin = NA
              fitFlag.postmin = FALSE
            }

            # get local deriv2-maximum before max_slope
            # premin.ndx <- maxima[(maxima - tp.ext[1]) <= 0][which.max(maxima[(maxima - tp.ext[1]) <= 0])]
            if(length(maxima)>0){
              premin.ndx <- maxima[which.min(abs(maxima - tp.ext[1]))]
              if(premin.ndx > h){
                # extract linear regression results before pre-max_slope turning point
                ret.premin <- ret[ret$x <= obs$x[premin.ndx-h],]
                # Consider only positive slopes and slopes that are at least 10% of max_slope
                ret.premin <- ret.premin[ret.premin[, "slope"] > 0 & ret.postmin[, "slope"] >= 0.1*max_slope, ]
                #remove regressions included in the extended candidate list
                ret.premin <- ret.premin[!(ret.premin[, 1] %in% tp.ext), ]
                ## Determine index of window with maximum slope, iterate until regression is found that meets R2 and RSD criterion
                success <- FALSE
                # apply min.density to list of linear regressions
                if(x_type == "density"){
                  ret.premin.check <- ret.premin[ret.premin[, 8] >= min.density, ]
                } else {
                  ret.premin.check <- ret.premin
                }
                ret.premin.check <- ret.premin.check[!is.na(ret.premin.check[,1]), ]
                if (any(ret.premin.check[, 5] >= R2 &
                        abs(ret.premin.check[, 6]) <= RSD)) {
                  while (!success) {
                    index.max <- which.max(ret.premin.check[, 3])
                    if (ret.premin.check[index.max, 5] >= R2 &&
                        abs(ret.premin.check[index.max, 6]) <= RSD &&
                        !is.na(ret.premin.check[index.max, 6])) {
                      # prerequisites for suitable µmax candidate: R2 and RSD
                      slope.max.premin <- ret.premin.check[index.max, 3]
                      success <- TRUE
                    } else {
                      ret.premin.check <- ret.premin.check[-index.max,]
                    }
                  }
                  index.max.ret.premin <- ret.premin[which(ret.premin[, 3] == slope.max.premin), 1] # index of maximum slope in fit table
                  slope.quota <- quota * slope.max.premin
                  candidates.premin <- ret.premin[which(
                    ret.premin[, 3] >= slope.quota &
                      ret.premin[, 5] >= 0.98 * R2 &
                      abs(ret.premin[, 6]) <= 1.02 * RSD &
                      ret.premin[, 7] >= t0), 1]

                  #consider only candidate windows next to index.max.ret.premin
                  candidate_intervals.premin <-split(candidates.premin, cumsum(c(1, diff(candidates.premin) != 1)))
                  if (index.max.ret.premin %in% unlist(candidate_intervals.premin)) {
                    candidates.premin <- candidate_intervals.premin[
                      as.numeric(which(sapply(candidate_intervals.premin,FUN = function(X) index.max.ret.premin %in% X)))][[1]]
                  }


                  if (length(candidates.premin) > 0) {
                    #perform linear regression with candidate fl_data points
                    tp.premin <- seq(min(candidates.premin), max(candidates.premin) + h - 1)
                    if (control$log.y.lin == TRUE) {
                      m.premin <- lm_window(obs$x, obs$ylog, min(tp.premin), length(tp.premin)) # linear model
                    } else {
                      m.premin <- lm_window(obs$x, obs$fl_data, min(tp.premin), length(tp.premin)) # linear model
                    }
                    p.premin  <- c(lm_parms(m.premin), n = length(tp.premin)) # # slope equation parameters (linear model)
                  } else {
                    p.premin <- c(
                      a = 0,
                      b = 0,
                      se = 0,
                      r2 = 0,
                      cv = 0,
                      n = 0
                    )
                    m = NULL
                  }

                  if (length(candidates.premin) > 0) {
                    ## get x window of exponential fit
                    x.max_start.premin <- obs$x[tp.premin[1]]
                    x.max_end.premin <- obs$x[tp.premin[length(tp.premin)]]
                    if (control$log.y.lin == TRUE) {
                      y_turn.premin <- obs$ylog[tp.premin[length(tp.premin)]] # y at turning point
                    } else {
                      y_turn.premin <- obs$fl_data[tp.premin[length(tp.premin)]] # y at turning point
                    }
                    x.turn.premin <- obs$x[premin.ndx]
                    y0_lm.premin    <- unname(coef(m.premin)[1]) # y-intercept of tangent
                    max_slope.premin <- unname(coef(m.premin)[2])

                    ## estimate lag phase between first and second growth phase
                    if (control$log.y.lin == TRUE) {
                      lambda.premin <- (obs$ylog[1] - y0_lm.premin) / max_slope.premin
                    } else {
                      lambda.premin <- (obs$fl_data[1] - y0_lm.premin) / max_slope.premin
                    }

                    # correct y0 values for Ln(y(t)/y0)
                    if(control$log.y.lin == TRUE){
                      y0_lm.premin <-  obs$fl_data[1] * exp(y0_lm.premin)
                    }

                    # get indices of x points used in linear fit
                    ndx.premin <- seq(min(match(ret[candidates.premin, "x"], x.in)),
                                      max(match(ret[candidates.premin, "x"], x.in)) + h -
                                        1)
                    slope.se.premin <- as.numeric(p.premin[3]) # standard error of slope
                    rsquared.premin <- p.premin["r2"]
                    fitFlag.premin <- TRUE
                  }
                  else {
                    # of if(length(candidates.premin) > 0)
                    y0_lm.premin = NA
                    max_slope.premin = NA
                    slope.se.premin = NA
                    lag.premin = NA
                    x.max_start.premin = NA
                    x.max_end.premin = NA
                    ndx.premin = NA
                    rsquared.premin = NA
                    fitFlag.premin = FALSE
                  }
                } # if (any(ret.premin.check[, 5] >= R2 & abs(ret.premin.check[, 6]) <= RSD))
                else{
                  y0_lm.premin = NA
                  max_slope.premin = NA
                  slope.se.premin = NA
                  lag.premin = NA
                  x.max_start.premin = NA
                  x.max_end.premin = NA
                  ndx.premin = NA
                  rsquared.premin = NA
                  fitFlag.premin = FALSE
                }
              } # if(premin.ndx > h)
              else {
                # of if(length(candidates.premin) > 0)
                y0_lm.premin = NA
                max_slope.premin = NA
                slope.se.premin = NA
                lag.premin = NA
                x.max_start.premin = NA
                x.max_end.premin = NA
                ndx.premin = NA
                rsquared.premin = NA
                fitFlag.premin = FALSE
              }
            }
            else {
              # of if(length(candidates.premin) > 0)
              y0_lm.premin = NA
              max_slope.premin = NA
              slope.se.premin = NA
              lag.premin = NA
              x.max_start.premin = NA
              x.max_end.premin = NA
              ndx.premin = NA
              rsquared.premin = NA
              fitFlag.premin = FALSE
            }

            # Choose regression before or after max_slope as second growth phase based on second slope
            if(!is.na(max_slope.premin) && !is.na(max_slope.postmin)){
              max_slope2 <- ifelse(max_slope.premin > max_slope.postmin, max_slope.premin, max_slope.postmin)
              y0_2 <- ifelse(max_slope.premin > max_slope.postmin, y0_lm.premin, y0_lm.postmin)
              lag2 <- ifelse(max_slope.premin > max_slope.postmin, lambda.premin, lambda.postmin)
              x.max2_start <- ifelse(max_slope.premin > max_slope.postmin, x.max_start.premin, x.max_start.postmin)
              x.max2_end <- ifelse(max_slope.premin > max_slope.postmin, x.max_end.premin, x.max_end.postmin)
              rsquared2 <- ifelse(max_slope.premin > max_slope.postmin, rsquared.premin, rsquared.postmin)
              ndx2 <- if(max_slope.premin > max_slope.postmin){ndx.premin} else{ndx.postmin}
              ndx2.in <- match(fl_data[ndx2], fl_data.in)
              x.turn <- ifelse(max_slope.premin > max_slope.postmin, x.turn.premin, x.turn.postmin)
              fitFlag2 <- TRUE
            } else if (!is.na(max_slope.premin)){
              max_slope2 <- max_slope.premin
              y0_2 <- y0_lm.premin
              lag2 <- lambda.premin
              x.max2_start <- x.max_start.premin
              x.max2_end <- x.max_end.premin
              x.turn <- x.turn.premin
              rsquared2 <- rsquared.premin
              ndx2 <- ndx.premin
              ndx2.in <- match(fl_data[ndx2], fl_data.in)
              fitFlag2 <- TRUE
            } else if (!is.na(max_slope.postmin)){
              max_slope2 <- max_slope.postmin
              y0_2 <- y0_lm.postmin
              lag2 <- lambda.postmin
              x.max2_start <- x.max_start.postmin
              x.max2_end <- x.max_end.postmin
              x.turn <- x.turn.postmin
              rsquared2 <- rsquared.postmin
              ndx2 <- ndx.postmin
              ndx2.in <- match(fl_data[ndx2], fl_data.in)
              fitFlag2 <- TRUE
            } else {
              max_slope2 <- NA
              y0_2 <- NA
              lag2 <- NA
              x.max2_start <- NA
              x.max2_end <- NA
              x.turn <- NA
              rsquared2 <- NA
              ndx2 <- NA
              ndx2.in <- NA
              fitFlag2 <- FALSE
            }
          } # if(control$biphasic)
          else{
            max_slope2 <- NA
            y0_2 <- NA
            lag2 <- NA
            x.max2_start <- NA
            x.max2_end <- NA
            x.turn <- NA
            rsquared2 <- NA
            ndx2 <- NA
            ndx2.in <- NA
            fitFlag2 <- FALSE
          }
        } # if(any(ret.check[,5] >= R2 & ret.check[,6] <= RSD))
        else{
          flFitLinear <- list(x.in = get(ifelse(x_type == "density", "density.in", "time.in")), fl.in = fl_data.in,
                              raw.x = get(ifelse(x_type == "density", "density.in", "time.in")), raw.fl = fl_data.in,
                              filt.x = get(ifelse(x_type == "density", "density", "time")), filt.fl = fl_data,
                              ID = ID, FUN = grow_exponential, fit = NA, par = c(
                                y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                                x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                                x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
          class(flFitLinear) <- "flFitLinear"
          if (!control$suppress.messages)
            message(
              paste0(
                "No linear fit in accordance with the chosen parameters identified with an R2 value of >= ",
                R2,
                " and an RSD of <= ",
                RSD,
                "."
              )
            )
          return(flFitLinear)
        }
      } # else of if(nrow(ret.check)<2)
    } # else of if(nrow(ret)<2)
  } # if(N >= 3)
  else {
    message("Not enough observations in the dataset to perform linear fit. flFitLinear requires at least 3 fl_data points within the given t0 and min.density thresholds.")
    flFitLinear <- list(x.in = get(ifelse(x_type == "density", "density.in", "time.in")), fl.in = fl_data.in,
                        raw.x = get(ifelse(x_type == "density", "density.in", "time.in")), raw.fl = fl_data.in,
                        filt.x = get(ifelse(x_type == "density", "density", "time")), filt.fl = fl_data,
                        ID = ID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                          x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                          x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
    class(flFitLinear) <- "flFitLinear"
    return(flFitLinear)
  }



  flFitLinear <- list(
    x.in = get(ifelse(x_type == "density", "density.in", "time.in")),
    fl.in = fl_data.in,
    raw.x = x.in,
    raw.fl = fl_data.in,
    filt.x = obs$x,
    filt.fl = obs$fl_data,
    log.fl = obs$ylog,
    ID = ID,
    FUN = grow_exponential,
    fit = m,
    par = c(
      y0 = y0_data,
      dY = max(obs$fl_data)-obs$fl_data[1],
      A = max(obs$fl_data),
      y0_lm = y0_lm,
      max_slope = max_slope,
      tD = log(2)/max_slope,
      slope.se = slope.se,
      lag = lambda,
      x.max_start = x.max_start,
      x.max_end = x.max_end,
      x.turn = x.turn,
      max_slope2 = max_slope2,
      tD2 = log(2)/max_slope2,
      y0_lm2 = y0_2,
      lag2 = lag2,
      x.max2_start = x.max2_start,
      x.max2_end = x.max2_end
    ),
    ndx = ndx,
    ndx.in = ndx.in,
    ndx2 = ndx2,
    ndx2.in = ndx2.in,
    quota = quota,
    rsquared = p["r2"],
    rsquared2 = rsquared2,
    control = control,
    fitFlag = fitFlag,
    fitFlag2 = fitFlag2
  )

  class(flFitLinear) <- "flFitLinear"

  invisible(flFitLinear)
}

#' Run a complete fluorescence curve analysis and dose-reponse analysis workflow.
#'
#' \code{fl.workflow} runs \code{\link{fl.control}} to create a \code{fl.control} object and then performs all computational fitting operations based on the user input. Finally, if desired, a final report is created in PDF or HTML format that summarizes all results obtained.
#'
#' @param grodata A \code{grodata} object created with \code{\link{read_data}} or \code{\link{parse_data}}, containing fluorescence data and data for the independent variable (i.e., time or density).
#' @param time (optional) A matrix containing time values for each sample (if a \code{fl_data} dataframe is provided as separate argument).
#' @param fl_data (optional) A dataframe containing growth data (if a \code{fl_data} matrix is provided as separate argument).
#' @param fl_data (optional) A dataframe containing fluorescence data (if a \code{time} matrix or \code{density} dataframe is provided as separate argument).
#' @param ec50 (Logical) Perform dose-response analysis (\code{TRUE}) or not (\code{FALSE}).
#' @param mean.grp (\code{"all"}, a string vector, or a list of string vectors) Define groups to combine into common plots in the final report based on sample identifiers (if \code{report == TRUE}). Partial matches with sample/group names are accepted. Note: The maximum number of sample groups (with unique condition/concentration indicators) is 50. If you have more than 50 groups, option \code{"all"} will produce the error \code{! Insufficient values in manual scale. [Number] needed but only 50 provided}.
#' @param mean.conc (A numeric vector, or a list of numeric vectors) Define concentrations to combine into common plots in the final report (if \code{report == TRUE}).
#' @param fit.opt (Character or character vector) Indicates whether the program should perform a linear regression (\code{"l"}), model fit (\code{"m"}), spline fit (\code{"s"}), or all (\code{"a"}). Combinations can be freely chosen by providing a character vector, e.g. \code{fit.opt = c("l", "s")} Default:  \code{fit.opt = c("l", "s")}.
#' @param x_type (Character) Which data type shall be used as independent variable? Options are \code{'density'} and \code{'time'}.
#' @param norm_fl (Logical) use normalized (to density) fluorescence data in fits. Has an effect only when \code{x_type = 'time'}
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param min.density (Numeric) Indicate whether only values above a certain threshold should be considered for linear regressions or spline fits.
#' @param log.x.lin (Logical) Indicates whether _ln(x+1)_ should be applied to the independent variable for _linear_ fits. Default: \code{FALSE}.
#' @param log.x.spline (Logical) Indicates whether _ln(x+1)_ should be applied to the independent variable for _spline_ fits. Default: \code{FALSE}.
#' @param log.y.lin (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ fits. Default: \code{FALSE}
#' @param log.y.spline (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _spline_ fits. Default: \code{FALSE}
#' @param lin.h (Numeric) Manually define the size of the sliding window used in \code{\link{fl.gcFitLinear}}. If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{\link{fl.gcFitLinear}}.
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for the calculated slope in \code{\link{fl.gcFitLinear}}.
#' @param lin.dY (Numeric) Threshold for the minimum fraction of density increase a linear regression window should cover. Default: 0.05 (5%).
#' @param biphasic (Logical) Shall \code{\link{flFitLinear}} and \code{\link{flFitSpline}} try to extract fluorescence parameters for two different phases (as observed with, e.g., regulator-promoter systems with varying response in different growth stages) (\code{TRUE}) or not (\code{FALSE})?
#' @param interactive (Logical) Controls whether the fit for each sample and method is controlled manually by the user. If \code{TRUE}, each fit is visualized in the _Plots_ pane and the user can adjust fitting parameters and confirm the reliability of each fit per sample. Default: \code{TRUE}.
#' @param dr.parameter (Character or numeric) The response parameter in the output table to be used for creating a dose response curve. See \code{\link{fl.drFit}} for further details. Default: \code{"max_slope.spline"}, which represents the maximum slope of the spline fit Typical options include: \code{"max_slope.linfit"}, \code{"dY.linfit"}, \code{"max_slope.spline"}, and \code{"dY.spline"}.
#' @param dr.method (Character) Perform either a smooth spline fit on response parameter vs. concentration data (\code{"spline"}) or fit a biosensor response model (proposed by Meyer et al., 2019).
#' @param dr.have.atleast (Numeric) Minimum number of different values for the response parameter one should have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: \code{6}.
#' @param smooth.dr (Numeric) Smoothing parameter used in the spline fit by smooth.spline during dose response curve estimation. Usually (not necessesary) in (0; 1]. See \code{\link{smooth.spline}} for further details. Default: \code{NULL}.
#' @param log.x.dr (Logical) Indicates whether \code{ln(x+1)} should be applied to the concentration data of the dose response curves. Default: \code{FALSE}.
#' @param log.y.dr (Logical) Indicates whether \code{ln(y+1)} should be applied to the response data of the dose response curves. Default: \code{FALSE}.
#' @param nboot.dr (Numeric) Defines the number of bootstrap samples for EC50 estimation. Use \code{nboot.dr = 0} to disable bootstrapping. Default: \code{0}.
#' @param nboot.fl (Numeric) Number of bootstrap samples used for nonparametric curve fitting with \code{\link{flBootSpline}}. Use \code{nboot.fl = 0} to disable the bootstrap. Default: \code{0}
#' @param smooth.fl (Numeric) Parameter describing the smoothness of the spline fit; usually (not necessary) within (0;1]. \code{smooth.gc=NULL} causes the program to query an optimal value via cross validation techniques. Especially for datasets with few data points the option \code{NULL} might cause a too small smoothing parameter. This can result a too tight fit that is susceptible to measurement errors (thus overestimating slopes) or produce an error in \code{\link{smooth.spline}} or lead to overfitting. The usage of a fixed value is recommended for reproducible results across samples. See \code{\link{smooth.spline}} for further details. Default: \code{0.55}
#' @param growth.thresh (Numeric) Define a threshold for growth. Only if any density value in a sample is greater than \code{growth.thresh} (default: 1.5) times the start density, further computations are performed. Else, a message is returned.
#' @param suppress.messages (Logical) Indicates whether messages (information about current growth curve, EC50 values etc.) should be displayed (\code{FALSE}) or not (\code{TRUE}). This option is meant to speed up the high-throughput processing data. Note: warnings are still displayed. Default: \code{FALSE}.
#' @param neg.nan.act (Logical) Indicates whether the program should stop when negative growth values or NA values appear (\code{TRUE}). Otherwise, the program removes these values silently (\code{FALSE}). Improper values may be caused by incorrect data or input errors. Default: \code{FALSE}.
#' @param clean.bootstrap (Logical) Determines if negative values which occur during bootstrap should be removed (\code{TRUE}) or kept (\code{FALSE}). Note: Infinite values are always removed. Default: \code{TRUE}.
#' @param report (Character or NULL) Create a PDF (\code{'pdf'}) and/or HTML (\code{'html'}) report after running all computations. Define \code{NULL} if no report should be created. Default: (\code{c('pdf', 'html')})
#' @param out.dir {Character or \code{NULL}} Define the name of a folder in which all result files are stored. If \code{NULL}, the folder will be named with a combination of "Report.growth_" and the current date and time.
#' @param out.nm {Character or \code{NULL}} Define the name of the report files. If \code{NULL}, the files will be named with a combination of "GrowthReport_" and the current date and time.
#' @param export.fig (Logical) Export all figures created in the report as separate PNG and PDF files (\code{TRUE}) or not (\code{FALSE}).
#' @param ... Further arguments passed to the shiny app.
#'
#' @return A \code{grofit} object that contains all computation results, compatible with various plotting functions of the QurvE package and with \code{\link{growth.report}}.
#' \item{time}{Raw time matrix passed to the function as \code{time} (if no \code{grofit} object is provided. Else, extracted from \code{grofit}).}
#' \item{data}{Raw data dataframe passed to the function as \code{grodata}.}
#' \item{flFit1}{\code{flFit} object created with the call of \code{\link{flFit}} on fluorescence1 data.}
#' \item{flFit2}{\code{flFit} object created with the call of \code{\link{flFit}} on fluorescence2 data.}
#' \item{drFit1}{\code{drFit} or \code{drFitfl} object created with the call of \code{\link{growth.drFit}} or \code{\link{fl.drFit}} for fluorescence1 data (based on the \code{dr.method} argument in \code{control}; see \code{\link{fl.control}}).}
#' \item{drFit2}{\code{drFit} or \code{drFitfl} object created with the call of \code{\link{growth.drFit}} or \code{\link{fl.drFit}} for fluorescence2 data (based on the \code{dr.method} argument in \code{control}; see \code{\link{fl.control}}).}
#' \item{expdesign}{Experimental design table inherited from \code{grodata} or created from the identifier columns (columns 1-3) in \code{data}.}
#' \item{control}{Object of class \code{fl.control} created with the call of \code{\link{fl.control}}.}
#' @export
#'
fl.workflow <- function(grodata = NULL,
                        time = NULL,
                        density = NULL,
                        fl_data = NULL,
                        ec50 = FALSE,
                        mean.grp = NA,
                        mean.conc = NA,
                        fit.opt = c("l", "s"),
                        x_type = c("density", "time"),
                        norm_fl = TRUE,
                        t0 = 0,
                        min.density = 0,
                        log.x.lin = FALSE,
                        log.x.spline = FALSE,
                        log.y.lin = FALSE,
                        log.y.spline = FALSE,
                        lin.h = NULL,
                        lin.R2 = 0.97,
                        lin.RSD = 0.05,
                        lin.dY = 0.05,
                        biphasic = FALSE,
                        interactive = FALSE,
                        dr.parameter = "max_slope.spline",
                        dr.method = c("model", "spline"),
                        dr.have.atleast = 5,
                        smooth.dr = NULL,
                        log.x.dr = FALSE,
                        log.y.dr = FALSE,
                        nboot.dr = 0,
                        nboot.fl = 0,
                        smooth.fl = 0.75,
                        growth.thresh = 1.5,
                        suppress.messages = FALSE,
                        neg.nan.act = FALSE,
                        clean.bootstrap = TRUE,
                        report = c('pdf', 'html'),
                        out.dir = NULL,
                        out.nm = NULL,
                        export.fig = FALSE,
                        ...)
{
  # Define objects based on additional function calls
  call <- match.call()

  ## remove strictly defined arguments
  call$grodata <- call$time <- call$density <- call$fl_data <- call$ec50 <- call$mean.grp <- call$mean.conc <- call$neg.nan.act <- call$clean.bootstrap <- call$suppress.messages <-
    call$fit.opt <- call$t0 <- call$min.density <- call$log.x.lin <- call$log.x.spline <- call$log.y.spline <- call$log.y.lin <- call$biphasic <- call$norm_fl <- call$x_type <-
    call$lin.h <- call$lin.R2 <- call$lin.RSD <- call$lin.dY <- call$interactive <- call$nboot.fl <- call$smooth.fl <- call$dr.method <- call$growth.thresh <-
    call$dr.have.atleast <- call$dr.parameter  <- call$smooth.dr  <- call$log.x.dr  <- call$log.y.dr <- call$nboot.dr <- call$report <- call$out.dir <- call$out.nm <- call$export.fig <- NULL


  arglist <- sapply(call, function(x) x)
  arglist <- unlist(arglist)[-1]
  ## Assign additional arguments (...) as R objects
  if(length(arglist) > 0){
    for(i in 1:length(arglist)){
      assign(names(arglist)[i], arglist[[i]])
    }
  }


  if(!is.null(grodata) && !(class(grodata)=="list") && !(class(grodata)=="grodata")){
    if (is.numeric(as.matrix(time)) == FALSE)
      stop("Need a numeric matrix for 'time' or a grodata object created with read_data() or parse_data().")
    if (is.numeric(as.matrix(data[-1:-3])) == FALSE)
      stop("Need a numeric matrix for 'data' or a grodata object created with read_data() or parse_data().")
    if (is.logical(ec50) == FALSE)
      stop("Need a logical value for 'ec50'")
  } else {
    if(!is.null(grodata$time)) time <- grodata$time
    if(!is.null(grodata$density)) density <- grodata$density
    if(!is.null(grodata$expdesign)) expdesign <- grodata$expdesign
    if(!is.null(grodata$fluorescence1)) fluorescence1 <- grodata$fluorescence1
    if(!is.null(grodata$fluorescence2)) fluorescence2 <- grodata$fluorescence2
    if(!is.null(grodata$norm.fluorescence1)) norm.fluorescence1 <- grodata$norm.fluorescence1
    if(!is.null(grodata$norm.fluorescence2)) norm.fluorescence2 <- grodata$norm.fluorescence2

    if(!is.null(time)) time <- time
    if(!is.null(density)) density <- density
    if(!is.null(fl_data)) fluorescence1 <- fl_data
  }
  control <- fl.control(fit.opt = fit.opt, norm_fl = norm_fl, x_type = x_type, t0 = t0, min.density = min.density, log.x.lin = log.x.lin,
                        log.x.spline = log.x.spline, log.y.lin = log.y.lin, log.y.spline = log.y.spline,
                        lin.h = lin.h, lin.R2 = lin.R2, lin.RSD = lin.RSD, lin.dY = lin.dY, dr.have.atleast = dr.have.atleast,
                        smooth.dr = smooth.dr, log.x.dr = log.x.dr, log.y.dr = log.y.dr, nboot.dr = nboot.dr,
                        biphasic = biphasic, interactive = interactive, nboot.fl = nboot.fl, dr.parameter = dr.parameter, dr.method = dr.method, clean.bootstrap = clean.bootstrap,
                        smooth.fl = smooth.fl, growth.thresh = growth.thresh, suppress.messages = suppress.messages, neg.nan.act = neg.nan.act)
  nboot.fl <- control$nboot.fl
  nboot.dr <- control$nboot.dr
  out.flFit <- NA
  out.drFit <- NA

  # /// fit of fluorescence curves -----------------------------------
  if(norm_fl == TRUE && x_type == "time" && (!is.null(norm.fluorescence1) && length(norm.fluorescence1) > 1 && !all(is.na(norm.fluorescence1)))){
    if ((control$suppress.messages==FALSE)){
      cat("\n\n")
      cat(paste("=== Performing Fits for Fluorescence 1 =================================\n"))
      cat("----------------------------------------------------\n")
    }
    if(exists("shiny") && shiny == TRUE){
      out.flFit1 <- flFit(time = time, density = density, fl_data = norm.fluorescence1, control = control, shiny = TRUE)
    } else {
      out.flFit1 <- flFit(time = time, density = density, fl_data = norm.fluorescence1, control = control, shiny = FALSE)
    }

  } else if (!is.null(fluorescence1) && length(fluorescence1) > 1 && !all(is.na(fluorescence1))){
    if(exists("shiny") && shiny == TRUE){
      out.flFit1 <- flFit(time = time, density = density, fl_data = fluorescence1, control = control, shiny = TRUE)
    } else {
      out.flFit1 <- flFit(time = time, density = density, fl_data = fluorescence1, control = control, shiny = FALSE)
    }
  }
  if(norm_fl == TRUE && x_type == "time" && (!is.null(norm.fluorescence2) && length(norm.fluorescence2) > 1 && !all(is.na(norm.fluorescence2)))){
    if ((control$suppress.messages==FALSE)){
      cat("\n\n")
      cat(paste("=== Performing Fits for Fluorescence 2 =================================\n"))
      cat("----------------------------------------------------\n")
    }
    if(exists("shiny") && shiny == TRUE){
      out.flFit2 <- flFit(time = time, density = density, fl_data = norm.fluorescence2, control = control, shiny = TRUE)
    } else {
      out.flFit2 <- flFit(time = time, density = density, fl_data = norm.fluorescence2, control = control, shiny = FALSE)
    }
  } else if (!is.null(fluorescence2) && length(fluorescence2) > 1 && !all(is.na(fluorescence2))){
    if ((control$suppress.messages==FALSE)){
      cat("\n\n")
      cat(paste("=== Performing Fits for Fluorescence 2 =================================\n"))
      cat("----------------------------------------------------\n")
    }
    if(exists("shiny") && shiny == TRUE){
      out.flFit2 <- flFit(time = time, density = density, fl_data = fluorescence2, control = control, shiny = TRUE)
    } else {
      out.flFit2 <- flFit(time = time, density = density, fl_data = fluorescence2, control = control, shiny = FALSE)
    }
  }

  # /// Estimate EC50 values
  if (ec50 == TRUE) {
    if (!is.null(fluorescence1) && length(fluorescence1) > 1 && !all(is.na(fluorescence1))){
      if ((control$suppress.messages==FALSE)){
        cat("\n\n")
        cat(paste("=== Performing Dose-Response Analysis for  Fluorescence 1 =================================\n"))
        cat("----------------------------------------------------\n")
      }
      if(control$dr.method == "spline"){
        out.drFit1 <- growth.drFit(summary.flFit(out.flFit1), control)
        boot.ec1 <- out.drFit1$boot.ec
      } else {
        out.drFit1 <- fl.drFit(summary.flFit(out.flFit1), control)
        boot.ec1 <- NA
      }
      EC50.table1 <- out.drFit1$drTable
    }
    if (!is.null(fluorescence2) && length(fluorescence2) > 1 && !all(is.na(fluorescence2))){
      if ((control$suppress.messages==FALSE)){
        cat("\n\n")
        cat(paste("=== Performing Dose-Response Analysis for  Fluorescence 2 =================================\n"))
        cat("----------------------------------------------------\n")
      }
      if(control$dr.method == "spline"){
        out.drFit2 <- growth.drFit(summary.flFit(out.flFit2), control)
        boot.ec2 <- out.drFit2$boot.ec
      } else {
        out.drFit2 <- fl.drFit(summary.flFit(out.flFit2), control)
        boot.ec2 <- NA
      }
      EC50.table2 <- out.drFit2$drTable
    }
  }
  # ///
  na.obj <- NA
  flFitRes <- list(time = time, data = grodata, flFit1 = get(ifelse(exists("out.flFit1"), "out.flFit1", "na.obj")),
                   flFit2 = get(ifelse(exists("out.flFit2"), "out.flFit2", "na.obj")),
                 drFit1 = get(ifelse(exists("out.drFit1"), "out.drFit1", "na.obj")),
                 drFit2 = get(ifelse(exists("out.drFit2"), "out.drFit2", "na.obj")),
                 expdesign = expdesign, control = control)
  class(flFitRes) <- "flFitRes"

  if(!exists("shiny") || shiny != TRUE){
    if(!is.null(out.dir)){
      wd <- paste0(out.dir)
    } else {
      wd <- paste(getwd(), "/FluorescenceResults_", format(Sys.time(),
                                                     "%Y%m%d_%H%M%S"), sep = "")
    }
    dir.create(wd, showWarnings = F)

    if (!is.null(fluorescence1) && length(fluorescence1) > 1 && !all(is.na(fluorescence1))){
      flTable1 <- data.frame(apply(flFitRes[["flFit1"]][["flTable"]],2,as.character))
      res.table.fl1 <- Filter(function(x) !all(is.na(x)),flTable1)
      export_Table(table = res.table.fl1, out.dir = wd, out.nm = "results.fl1")
      cat(paste0("\nResults of fluorescence 1 analysis saved as tab-delimited text file in:\n",
                 wd, "/results.fl1.txt\n"))
    }
    if (!is.null(fluorescence2) && length(fluorescence2) > 1 && !all(is.na(fluorescence2))){
      flTable2 <- data.frame(apply(flFitRes[["flFit2"]][["flTable"]],2,as.character))
      res.table.fl2 <- Filter(function(x) !all(is.na(x)),flTable2)
      export_Table(table = res.table.fl2, out.dir = wd, out.nm = "results.fl2")
      cat(paste0("Results of fluorescence 2 analysis saved as tab-delimited text file in:\n",
                 wd, "/results.fl2.txt\n"))
    }

    if (ec50 == TRUE) {
      if (!is.null(fluorescence1) && length(fluorescence1) > 1 && !all(is.na(fluorescence1))){
        if(!is.null(EC50.table1) && !is.na(EC50.table1)) {
          res.table.dr_fl1 <- Filter(function(x) !all(is.na(x)),EC50.table1)
          export_Table(table = res.table.dr_fl1, out.dir = wd, out.nm = "results.fl_dr1")
          cat(paste0("\nResults of EC50 analysis for fluorescence 1 saved as tab-delimited in:\n",
                     wd, "/results.fl_dr1.txt\n"))
        }
      }
      if (!is.null(fluorescence2) && length(fluorescence2) > 1 && !all(is.na(fluorescence2))){
        if(!is.null(EC50.table2) && !is.na(EC50.table2)) {
          res.table.dr_fl2 <- Filter(function(x) !all(is.na(x)),EC50.table2)
          export_Table(table = res.table.dr_fl2, out.dir = wd, out.nm = "results.fl_dr2")

          cat(paste0("Results of EC50 analysis for fluorescence 2 saved as tab-delimited in:\n",
                     wd, "/results.fl_dr2.txt\n"))
        }
      }


    } else {
      res.table.dr_fl1 <- NULL
      res.table.dr_fl2 <- NULL
    }
    # Export RData object
    export_RData(flFitRes, out.dir = wd)

    if(any(report %in% c('pdf', 'html'))){
      try(fl.report(flFitRes, out.dir = gsub(paste0(getwd(), "/"), "", wd), mean.grp = mean.grp, mean.conc = mean.conc, ec50 = ec50,
                    export = export.fig, format = report, out.nm = out.nm))
    }
  }

  flFitRes
}

#' Create a PDF and HTML report with results from a fluorescence analysis workflow
#'
#' \code{fl.report} requires a \code{flFitRes} object and creates a report in PDF and HTML format that summarizes all results obtained.
#'
#' @param flFitRes A \code{grofit} object created with \code{\link{fl.workflow}}.
#' @param out.dir (Character) The path or name of the folder in which the report files are created.  If \code{NULL}, the folder will be named with a combination of "Report.fluorescence_" and the current date and time.
#' @param out.nm {Character or \code{NULL}} Define the name of the report files. If \code{NULL}, the files will be named with a combination of "FluorescenceReport_" and the current date and time.
#' @param ec50 (Logical) Was a dose-response analysis performed in \code{\link{fl.workflow}} \code{TRUE} or not \code{FALSE}?
#' @param export (Logical) Shall all plots generated in the report be exported as individual PDF and PNG files \code{TRUE} or not \code{FALSE}?
#' @param ... Further arguments passed to create a report. Currently required:
#' \itemize{
#'    \item \code{mean.grp}: Define groups to combine into common plots in the report based on sample identifiers. Partial matches with sample/group names are accepted. Can be \code{"all"}, a vector of strings, or a list of string vectors. Note: The maximum number of sample groups (with unique condition/concentration indicators) is 50. If you have more than 50 groups, option \code{"all"} will produce the error \code{! Insufficient values in manual scale. [Number] needed but only 50 provided}.
#'    \item \code{mean.conc}: Define concentrations to combine into common plots in the  report. Can be a numeric vector, or a list of numeric vectors.
#' }
#' @param ec50 (Logical) Display results of dose-response analysis (\code{TRUE}) or not (\code{FALSE}).
#' @param format(Character) Define the file format for the report, PDF (\code{'pdf'}) and/or HTML (\code{'html'}). Default: (\code{c('pdf', 'html')})
#'
#' @export
#'
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
#' @importFrom foreach %dopar%
#' @import kableExtra
#' @import knitr
#' @import plyr
#' @include general_misc_utils.R
fl.report <- function(flFitRes, out.dir = NULL, out.nm = NULL, ec50 = FALSE, format = c('pdf', 'html'), export = FALSE, ...)
{
  # results an object of class grofit
  if(class(flFitRes) != "flFitRes") stop("flFitRes needs to be an object created with fl.workflow().")

  # Define objects based on additional function calls
  call <- match.call()
  ## remove strictly defined arguments
  call$flFitRes <- call$out.dir <- call$out.nm <- call$ec50 <- call$format <- NULL

  arglist <- sapply(call, function(x) x)
  arglist <- unlist(arglist)[-1]
  ## Assign additional arguments (...) as R objects
  if(length(arglist) > 0){
    for(i in 1:length(arglist)){
      assign(names(arglist)[i], arglist[[i]])
    }
  }

  if(!exists("mean.grp")) mean.grp <- NA
  if(!exists("mean.conc")) mean.conc <- NA
  flFit1 <- flFitRes$flFit1
  drFit1 <- flFitRes$drFit1
  flFit2 <- flFitRes$flFit2
  drFit2 <- flFitRes$drFit2

  control <- flFitRes$control
  time <- flFitRes$time
  data <- flFitRes$data
  if(!exists("res.table.fl1")){
    res.table.fl1 <- flFitRes$flFit1$flTable
  }
  if(!exists("res.table.dr1")){
    if(length(flFitRes$drFit1)>1 && !is.na(flFitRes$drFit1$drTable)) res.table.dr1 <- flFitRes$drFit1$drTable
  }
  if(!exists("res.table.gc2")){
    if(length(flFit2)>1){
      res.table.fl2 <- flFitRes$flFit2$flTable
    }
  }
  if(!exists("res.table.dr2")){
    if(length(flFitRes$drFit2)>1 && !is.na(flFitRes$drFit2$drTable)) res.table.dr2 <- flFitRes$drFit2$drTable
  }
  # find minimum and maximum mu values in whole dataset to equilibrate derivative plots for spline fits
  mu.min1 <- suppressWarnings(min(sapply(1:length(flFitRes$flFit1$gcFittedSplines), function(x) min(flFitRes$flFit1$gcFittedSplines[[x]]$spline.deriv1$y))))*1.05
  if(mu.min1 >0) mu.min1 <- 0
  mu.max1 <- suppressWarnings(max(sapply(1:length(flFitRes$flFit1$gcFittedSplines), function(x) max(flFitRes$flFit1$gcFittedSplines[[x]]$spline.deriv1$y))))*1.05
  if(length(flFit2)>1){
    mu.min2 <- suppressWarnings(min(sapply(1:length(flFitRes$flFit2$gcFittedSplines), function(x) min(flFitRes$flFit2$gcFittedSplines[[x]]$spline.deriv1$y))))*1.05
    if(mu.min2 >0) mu.min2 <- 0
    mu.max1 <- suppressWarnings(max(sapply(1:length(flFitRes$flFit2$gcFittedSplines), function(x) max(flFitRes$flFit2$gcFittedSplines[[x]]$spline.deriv1$y))))*1.05
  }

  if(!is.null(out.dir)){
    wd <- out.dir
  } else {
    wd <- paste(getwd(), "/Report.fluorescence_", format(Sys.time(),
                                                   "%Y%m%d_%H%M%S"), sep = "")
  }
  if(is.null(out.nm)){
    out.nm <- paste("/FluorescenceReport_", format(Sys.time(),
                                             "%Y%m%d_%H%M%S"), sep = "")
  }

  dir.create(wd, showWarnings = F)

  message("Render reports...")

  # for(i in 1:length(.libPaths())){
  #   QurvE.ndx <- grep("QurvE", list.files(.libPaths()[i]))
  #   if(length(QurvE.ndx)>0){
  #     Report.wd <- paste0(.libPaths()[i], "/QurvE")
  #   }
  # }
  Report.wd <- paste0("C:/Users/nicwir/Documents/DTU_Biosustain/Scripts_and_Modelling/curvE package/QurvE/inst/")
  file <- paste0(Report.wd, "/Report_Fluorescence.Rmd")
  if(all(c('pdf', 'html') %in% format)){
    format <- c('html_document', 'pdf_document')
  } else if ('pdf' %in% format){
    format <- 'pdf_document'
  } else if ('html' %in% format){
    format <- 'html_document'
  } else {
    stop("Please define a valid report format, either 'pdf', 'html', or c('pdf', 'html').")
  }
  rmarkdown::render(file, output_format = format, output_dir = wd, output_file = out.nm,
                    quiet = TRUE)
  message(paste0("Files saved in: '", wd, "'"))
  unlink(paste0(tempdir(), "/Plots"), recursive = TRUE)
}

#' Fit a biosensor model (Meyer et al., 2019) to response vs. concentration data
#'
#' @param FitData A dataframe containing the data for the dose-response model estimation. Such table of class \code{flTable} can be obtained by running \code{\link{flFit}} with \code{dr.method = "model"} as argument in the \code{fl.control} object.
#' @param control A \code{fl.control} object created with \code{\link{fl.control}}, defining relevant fitting options.
#'
#' @return An object of class \code{drFit}.
#' \item{raw.data}{Data that passed to the function as \code{flTable}.}
#' \item{drTable}{Dataframe containing condition identifiers, fit options, and results of the dose-response analysis.}
#' \item{drFittedModels}{List of all \code{drFitModel} objects generated by the call of \code{\link{fl.drFitModel}} for each distinct experiment.}
#' \item{control}{Object of class \code{fl.control} created with the call of \code{\link{fl.control}}.}
#'
#' @export
#'
#' @references Meyer, A.J., Segall-Shapiro, T.H., Glassey, E. et al. _Escherichia coli “Marionette” strains with 12 highly optimized small-molecule sensors._ Nat Chem Biol 15, 196–204 (2019). DOI: 10.1038/s41589-018-0168-3
#'
fl.drFit <- function(FitData, control = fl.control())
{
  if (is(control) != "fl.control")
    stop("control must be of class fl.control!")
  EC50.table <- NULL
  all.EC50 <- NA
  if(is.character(control$dr.parameter)){
    dr.parameter <- match(control$dr.parameter, colnames(FitData))
  }
  FitData <- FitData[!is.na(FitData[,1]), ]
  table.tests <- table((FitData[, 1])[which((FitData[,
                                                     4] == TRUE) & (is.na(FitData[, dr.parameter]) ==
                                                                      FALSE))])
  distinct <- names(table.tests)
  EC50 <- list()
  validdata <- cbind(as.character(distinct), table.tests)
  colnames(validdata) <- c("TestID", "Number")
  rownames(validdata) <- rep("     ", times = dim(validdata)[1])
  if (control$suppress.messages == FALSE) {
    cat("\n")
    cat("=== Dose-Response Estimation via Model Fit ==============================\n")
    cat("---------------------------------------------------\n")
    cat("--> Checking data ...\n")
    cat(paste("--> Number of distinct tests found:", as.character(length(distinct))),
        "\n")
    cat("--> Valid datasets per test: \n")
    print(validdata, quote = FALSE)
  }
  if (TRUE %in% (table.tests < control$dr.have.atleast)) {
    cat(paste("Warning: following tests have not enough ( <",
              as.character(control$dr.have.atleast - 1), ") datasets:\n"))
    cat(paste(distinct[(table.tests < control$dr.have.atleast)], sep = "\n"))
    cat("These tests will not be regarded\n")
    distinct <- distinct[table.tests >= control$dr.have.atleast]
  }
  if ((length(distinct)) == 0) {
    cat(paste("There are no tests having enough ( >", as.character(control$dr.have.atleast -
                                                                     1), ") datasets!\n"))
    drFitfl <- list(raw.data = FitData, drTable = NA,
                    drFittedModels = NA, control = control)
    class(drFitfl) <- "drFitfl"
    return(drFitfl)
  }
  else {
    skip <- c()
    for (i in 1:length(distinct)) {
      conc <- factor((FitData[, 3])[which(FitData[, 1] ==
                                            distinct[i])])
      if(length(levels(conc)) < 4){
        message(paste0(distinct[i], " does not have enough unique concentrations. A condition must have at least 4 different concentrations to be considered for dose-response analysis."))
        skip <- c(skip, i)
        next
      }
      test <- (as.numeric(FitData[, dr.parameter]))[FitData[, 1] == distinct[i]]
      names(test) <- rep(names(FitData)[dr.parameter], length(test))
      drID <- distinct[i]
      EC50[[i]] <- try(fl.drFitModel(conc, test, drID, control), silent = T)
      if(class(EC50[[i]]) != "try-error"){
        description <- data.frame(Test = distinct[i], log.x = control$log.x.dr,
                                  log.y = control$log.y.dr)
        out.row <- cbind(description, summary.drFitModel(EC50[[i]]))
      } else {
        out.row <- rep(NA, 12)
      }
      EC50.table <- rbind(EC50.table, out.row)

    }
  }
  class(EC50.table) <- c("drTable", "list")
  if(exists("skip") && !is.null(skip)){
    distinct <- distinct[-skip]
    EC50 <- EC50[-skip]
  }
  names(EC50) <- distinct
  drFitfl <- list(raw.data = FitData, drTable = EC50.table,
                drFittedModels = EC50, control = control)
  class(drFitfl) <- "drFitfl"
  drFitfl
}

#' Perform a biosensor model fit on response vs. concentration data of a single sample.
#'
#' \code{fl.drFitModel} fits the biosensor model proposed by Meyer et al. (2019) to the provided response (e.g., \code{max_slope.spline} vs. concentration data to determine the leakiness, sensitivity, induction fold-change, and cooperativity.
#'
#' @param conc Vector of concentration values.
#' @param test Vector of response parameter values of the same length as \code{conc}.
#' @param drID (Character) The name of the analyzed condition
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#'
#' @return A \code{drFitSpline} object.
#' \item{raw.conc}{Raw data provided to the function as \code{conc}.}
#' \item{raw.test}{Raw data for the response parameter provided to the function as \code{test}.}
#' \item{drID}{(Character) Identifies the tested condition}
#' \item{fit.conc}{Fitted concentration values.}
#' \item{fit.test}{Fitted response values.}
#' \item{model}{\code{nls} object generated by the \code{\link{minpack.lm::nlsLM}} function.}
#' \item{parameters}{List of parameters estimated from dose response curve fit.}
#' \itemize{
#' \item \code{yEC50}: {Response value related to EC50.}
#' \item \code{y.min}: {Minimum fluorescence ('leakiness', if lowest concentration is 0).}
#' \item \code{y.max}: {Maximum fluorescence.}
#' \item \code{fc}: {Fold change (\code{y.max} divided by \code{y.min}).}
#' \item \code{K}: {Concentration at half-maximal response ('sensitivity').}
#' \item \code{n}: {Cooperativity.}
#' \item \code{yEC50.orig}: {Response value for EC50 in original scale, if a transformation was applied.}
#' \item \code{K.orig}: {K in original scale, if a transformation was applied.}
#' \item \code{test.nm}: {Test identifier extracted from \code{test}.}
#' }
#' \item{fitFlag}{(Logical) Indicates whether a spline could fitted successfully to data.}
#' \item{reliable}{(Logical) Indicates whether the performed fit is reliable (to be set manually).}
#' \item{control}{Object of class \code{fl.control} created with the call of \code{\link{fl.control}}.}
#' Use \code{\link{plot.drFitModel}} to visualize the model fit.
#'
#' @export
#'
#' @references Meyer, A.J., Segall-Shapiro, T.H., Glassey, E. et al. _Escherichia coli “Marionette” strains with 12 highly optimized small-molecule sensors._ Nat Chem Biol 15, 196–204 (2019). DOI: 10.1038/s41589-018-0168-3
#'
fl.drFitModel <- function(conc, test, drID = "undefined", control = fl.control())
{
  if (is(control) != "fl.control")
    stop("control must be of class fl.control!")
  test.nm <- names(test)[1]
  test <- as.vector(as.numeric(as.matrix(test)))
  conc <- as.vector(as.numeric(as.matrix(conc)))
  if (is.vector(conc) == FALSE || is.vector(test) == FALSE)
    stop("fl.drFitModel: dose or response data must be a vector !")
  if (control$neg.nan.act == FALSE) {
    missings <- is.na(conc) | is.na(test) | !is.numeric(conc) |
      !is.numeric(test)
    conc <- conc[!missings]
    test <- test[!missings]
    negs <- (conc < 0) | (test < 0)
    conc <- conc[!negs]
    test <- test[!negs]
  }
  else {
    if (sum(is.na(conc) | is.na(test)))
      stop("fl.drFitModel: NA values encountered. Program terminated")
    if ((sum((conc < 0)) > 0) | (sum((test < 0)) > 0))
      stop("fl.drFitModel: Negative values encountered. Program terminated")
    if ((FALSE %in% is.numeric(conc)) || (FALSE %in% is.numeric(test)))
      stop("fl.drFitModel: Non numeric values encountered. Program terminated")
  }
  if (length(test) < 4) {
    warning("drFitModel: There is not enough valid data. Must have at least 4 unique values!")
    drFitModel <- list(raw.conc = conc, raw.test = test,
                        drID = drID, fit.conc = NA, fit.test = NA, spline = NA,
                        parameters = list(EC50 = NA, yEC50 = NA, EC50.orig = NA,
                                          yEC50.orig = NA), fitFlag = FALSE, reliable = NULL,
                        control = control)
    class(drFitModel) <- "drFitModel"
    return(drFitModel)
  }
  if (length(test) < control$dr.have.atleast) {
    warning("drFitModel: number of valid data points is below the number specified in 'dr.have.atleast'. See growth.control().")
    drFitModel <- list(raw.conc = conc, raw.test = test,
                        drID = drID, fit.conc = NA, fit.test = NA, spline = NA,
                        parameters = list(EC50 = NA, yEC50 = NA, EC50.orig = NA,
                                          yEC50.orig = NA), fitFlag = FALSE, reliable = NULL,
                        control = control)
    class(drFitModel) <- "drFitModel"
    return(drFitModel)
  }
  if (control$log.x.dr == TRUE)
    conc.log <- log(conc + 1)
  if (control$log.y.dr == TRUE)
    test.log <- log(test + 1)
  if (control$log.x.dr == TRUE) {
    conc.fit <- log(conc + 1)
  }
  else {
    conc.fit <- conc
  }
  if (control$log.y.dr == TRUE) {
    test.fit <- log(test + 1)
  }
  else {
    test.fit <- test
  }
  test.fit <- test.fit[order(conc.fit)]
  conc.fit <- conc.fit[order(conc.fit)]
  y.min <- mean(test.fit[which(conc.fit == conc.fit[1])])
  fitFlag <- TRUE
  n_candidates <- seq(0,1,0.01)
  i <- 1
  # plot(conc.fit, test.fit)
  # title(drID)
  df <- data.frame(x=conc.fit,test.fit=test.fit)
  y.model <- list()
  y.model[["convInfo"]] <- list()
  y.model$convInfo[["isConv"]] <- FALSE
  while( y.model$convInfo$isConv == FALSE && i < 100){
    i <- i+1
    try(
      suppressWarnings(y.model <- minpack.lm::nlsLM(test.fit ~ biosensor.eq(x=conc.fit, y.min, y.max, K, n), start = initbiosensor(x=conc.fit, y=test.fit, n = n_candidates[i]))),
      silent = T
    )
    # try(
    #   suppressWarnings(y.model <- nls(test.fit ~ biosensor.eq(x=conc.fit, y.min, y.max, K, n), start = initbiosensor(x=conc.fit, y=test.fit, n = n_candidates[i]))),
    #   silent = F
    # )
  }
  if (y.model$convInfo$isConv == FALSE) {
    if (control$suppress.messages == FALSE) {
      cat("Model could not be fitted in dose-response analysis!\n")
    }
    fitFlag <- FALSE
    stop("Error in fl.drFitModel")
  }
  # lines(conc.fit, biosensor.eq(x=conc.fit, y.min=coef(y.model)[1], y.max=coef(y.model)[2], K=coef(y.model)[3], n=coef(y.model)[4]), col = "red")
  m <- summary(y.model)
  par <- m$parameters
  # y.min <- par[1,1]
  y.max <- par[1,1]
  fc <- par[1,1]/y.min # fold-change
  K <- par[2,1] # sensitivity
  n <- par[3,1] # cooperativity

  x_fit <- seq(0,max(conc.fit), length.out = 1000)
  y_fit <- biosensor.eq(x_fit, y.min=y.min, y.max=par[1,1], K=par[2,1], n=par[3,1])
  # plot(conc.fit, test.fit)
  # lines(xin,  biosensor.eq(xin, y.min=par[1,1], y.max=par[2,1], K=par[3,1], n=par[4,1]))

  # /// estimating EC 50 values
  yEC.test <- y.min + (y.max - y.min) * ( K^n / (K^n + K^n) )
  EC.test <- K
  if (control$suppress.messages == FALSE) {
    cat("\n\n=== Dose response curve estimation ================\n")
    cat("--- EC 50 -----------------------------------------\n")
    cat(paste("-->", as.character(drID)))
    cat("\n")
    cat(paste(c("sensitivity:", "yEC50:", "fold change:", "leakiness:"), c(signif(EC.test, digits = 3), round(yEC.test), round(fc, 2), round(y.min, 1)), collapse =  " | "))
  }
  if ((control$log.x.dr == TRUE) && (control$log.y.dr == FALSE)) {
    if (control$suppress.messages == FALSE) {
      cat("\n--> Original scale \n")
      cat(paste(c("xEC50", "yEC50"), c(exp(EC.test) - 1,
                                       yEC.test)))
    }
    EC.orig <- c(exp(EC.test) - 1, yEC.test)
  }
  else {
    if ((control$log.x.dr == FALSE) && (control$log.y.dr ==
                                        TRUE)) {
      if (control$suppress.messages == FALSE) {
        cat("\n--> Original scale \n")
        cat(paste(c("xEC50", "yEC50"), c(EC.test, exp(yEC.test) -
                                           1)))
      }
      EC.orig <- c(EC.test, exp(yEC.test) - 1)
    }
    else {
      if ((control$log.x.dr == TRUE) && (control$log.y.dr ==
                                         TRUE)) {
        if (control$suppress.messages == FALSE) {
          cat("\n--> Original scale \n")
          cat(paste(c("xEC50", "yEC50"), c(exp(EC.test) -
                                             1, exp(yEC.test) - 1)))
        }
        EC.orig <- c(exp(EC.test) - 1, exp(yEC.test) -
                       1)
      }
      else {
        if ((control$log.x.dr == FALSE) && (control$log.y.dr ==
                                            FALSE)) {
          EC.orig <- c(EC.test, yEC.test)
        }
      }
    }
  }
  if (control$suppress.messages == FALSE) {
    cat("\n\n\n")
  }
  drFitModel <- list(raw.conc = conc, raw.test = test, drID = drID,
                      fit.conc = x_fit, fit.test = y_fit, model = y.model,
                      parameters = list(yEC50 = yEC.test, y.min = y.min, y.max = y.max, fc = fc, K = K, n = n,
                                        yEC50.orig = EC.orig[2], K.orig = EC.orig[1], test = test.nm),
                      fitFlag = fitFlag, reliable = NULL, control = control)
  class(drFitModel) <- "drFitModel"
  drFitModel
}


