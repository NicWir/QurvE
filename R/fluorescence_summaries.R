#' Generic summary function for flFitSpline objects
#'
#' @param object object of class \code{flFitSpline}
#' @param ...
#'
#' @return A dataframe with parameters extracted from the nonparametric fit.
#' @export
#'
summary.flFitSpline <- function(object,...)
{

  # object of class flFitSpline

  contents.fitted.spline  <- c("max_slope.spline", "x.max.spline", "lambda.spline",
                               "max_slope2.spline", "x.max2.spline", "lambda2.spline",
                               "y0.spline", "A.spline", "dY.spline", "integral.spline", "reliable_fit.spline", "reliable_fit2.spline",
                               "smooth.spline")

  if ((is.na(object$fitFlag)==TRUE)|(object$fitFlag==FALSE)){
    table <- c(0, rep(NA,length(contents.fitted.spline)-4), as.character(object$fitFlag), as.character(object$fitFlag2), ifelse(is.null(object$control$smooth.gc), "NULL", as.numeric(object$control$smooth.gc)))
  }
  else{
    table <- c(object$parameters$max_slope, object$parameters$x.max, object$parameters$lambda,
               object$parameters$max_slope2, object$parameters$x.max2, object$parameters$lambda2,
               object$parameters$A-object$parameters$dY, object$parameters$A, object$parameters$dY, object$parameters$integral, as.character(object$fitFlag), as.character(object$fitFlag2),
               ifelse(is.null(object$control$smooth.fl), "NULL", as.numeric(object$control$smooth.fl)))
  }

  table               <- data.frame(t(table))
  colnames(table)     <- contents.fitted.spline
  summary.flFitSpline <- table
  summary.flFitSpline
}

#' Generic summary function for flFitLinear objects
#'
#' @param object object of class \code{flFitLinear}
#' @param ...
#'
#' @return A dataframe with parameters extracted from the linear fit.
#' @export
#'
summary.flFitLinear <- function(object,...)
{
  # object of class flFitLinear

  contents.fitted.param     = c("max_slope.linfit",
                                "lambda.linfit",
                                "dY.linfit",
                                "A.linfit", "x.mu.start.linfit",
                                "x.mu.end.linfit",
                                "r2mu.linfit", "reliable_fit.linfit",
                                "max_slope2.linfit",
                                "x.mu2.start.linfit", "x.mu2.end.linfit",
                                "r2mu2.linfit", "reliable_fit2.linfit")


  if ((is.na(object$fitFlag)==TRUE)|(object$fitFlag==FALSE)){
    table<-c(0, rep(NA,length(contents.fitted.param)-1))
  }
  else{
    table <- c(object$par["max_slope"],
               ifelse(!is.na(object$par["lag2"]), ifelse(object$par["lag2"]<object$par["lag"], object$par["lag2"], object$par["lag"]), object$par["lag"]),
               object$par["dY"],
               object$par["A"], object$par["x.max_start"],
               object$par["x.max_end"],
               object$rsquared, as.character(object$fitFlag),
               object$par["max_slope2"],
               object$par["x.max2_start"], object$par["x.max2_end"],
               object$rsquared2, as.character(object$fitFlag2))

  }
  table <- data.frame(t(table))
  colnames(table) <- contents.fitted.param
  summary.flFitLinear <- table
  summary.flFitLinear
}

#' Generic summary function for flBootSpline objects
#'
#' @param object object of class \code{flBootSpline}
#' @param ...
#'
#' @return A dataframe with statistical parameters extracted from the dose-response bootstrapping analysis.
#' @export
#'
summary.flBootSpline <- function(object, ...)
{
  # object of class flBootSpline
  contents.bootstrap        <- c("max_slope.bt", "lambda.bt", "A.bt", "dY.bt", "integral.bt",
                                 "stdmax_slope.bt", "stdlambda.bt", "stdA.bt", "stddY.bt", "stdintegral.bt",
                                 "reliable_fit.bt",
                                 "ci90.mu.bt.lo", "ci90.mu.bt.up", "ci90.lambda.bt.lo", "ci90.lambda.bt.up",
                                 "ci90.A.bt.lo", "ci90.A.bt.up", "ci90.integral.bt.lo", "ci90.integral.bt.up",
                                 "ci95.mu.bt.lo", "ci95.mu.bt.up", "ci95.lambda.bt.lo", "ci95.lambda.bt.up",
                                 "ci95.A.bt.lo", "ci95.A.bt.up", "ci95.integral.bt.lo", "ci95.integral.bt.up")


  if (object$bootFlag==FALSE){
    table<-rep(NA,length(contents.bootstrap))
  }
  else{
    mu          <- mean(object$max_slope, na.rm=TRUE)
    lambda      <- mean(object$lambda, na.rm=TRUE)
    A           <- mean(object$A, na.rm=TRUE)
    dY          <- mean(object$dY, na.rm=TRUE)
    integral    <- mean(object$integral, na.rm=TRUE)

    mu.sd       <- sd(object$max_slope, na.rm=TRUE)
    lambda.sd   <- sd(object$lambda, na.rm=TRUE)
    A.sd        <- sd(object$A, na.rm=TRUE)
    dY.sd       <- sd(object$dY, na.rm=TRUE)
    integral.sd <- sd(object$integral, na.rm=TRUE)

    table <- c(mu, lambda, A, dY, integral,
               mu.sd, lambda.sd, A.sd, dY.sd, integral.sd,
               as.character(object$bootFlag),
               mu-1.645*mu.sd, mu+1.645*mu.sd, lambda-1.645*lambda.sd,     lambda+1.645*lambda.sd,
               A-1.645*A.sd,   A+1.645*A.sd,   integral-1.645*integral.sd, integral+1.645*integral.sd,
               mu-1.96*mu.sd,  mu+1.96*mu.sd,  lambda-1.96*lambda.sd,      lambda+1.96*lambda.sd,
               A-1.96*A.sd,    A+1.96*A.sd,    integral-1.96*integral.sd,  integral+1.96*integral.sd)
  }

  table               <- data.frame(t(table))
  colnames(table)     <- contents.bootstrap
  summary.flBootSpline <- table
  summary.flBootSpline
}

#' Generic summary function for flFit objects
#'
#' @param object object of class \code{flFit}
#' @param ...
#'
#' @return A dataframe with parameters extracted from all fits of a workflow.
#' @export
#'
summary.flFit <- function(object,...)
{
  # object of class flFit
  summary.flFit <- data.frame(object$flTable)
  summary.flFit
}

#' Generic summary function for drFitModel objects
#'
#' @param object object of class \code{drFitModel}
#' @param ...
#'
#' @return A dataframe with biosensor response parameters.
#' @export
#'
summary.drFitModel <- function(object, ...)
{
  # object of class drFitModel
  data.frame(object$parameters)
}
