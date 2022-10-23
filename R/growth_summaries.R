#' Generic summary function for drFitSpline objects
#'
#' @param object object of class \code{drFitSpline}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters extracted from the dose-response analysis of a single sample.
#' @export
#'
summary.drFitSpline <- function (object,...)
  {
    # object of class drFitSpline
    data.frame(object$parameters)
}

#' Generic summary function for drFitModel objects
#'
#' @param object object of class \code{drFitModel}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters extracted from the dose-response analysis of a single sample.
#' @export
#'
summary.drFitModel <- function (object,...)
{
  # object of class drFitSpline
  data.frame(object$parameters)
}

#' Generic summary function for gcFitSpline objects
#'
#' @param object object of class \code{gcFitSpline}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters extracted from the nonparametric fit.
#' @export
#'
summary.gcFitSpline <- function(object,...)
  {

    # object of class gcFitSpline

    contents.fitted.spline  <- c("mu.spline", "tD.spline", "tmax.spline", "lambda.spline",
                                 "mu2.spline", "tD2.spline", "tmax2.spline", "lambda2.spline",
                                 "y0.spline", "A.spline", "dY.spline", "integral.spline", "reliable_fit.spline", "reliable_fit2.spline",
                                 "smooth.spline")

    if ((is.na(object$fitFlag)==TRUE)|(object$fitFlag==FALSE)){
      table <- c(0, rep(NA,length(contents.fitted.spline)-6), 0, 0, as.character(object$fitFlag), as.character(object$fitFlag2), ifelse(is.null(object$control$smooth.gc), "NULL", as.numeric(object$control$smooth.gc)))
    }
    else{
      table <- c(object$parameters$mu, log(2)/object$parameters$mu, object$parameters$t.max, object$parameters$lambda,
                 object$parameters$mu2, log(2)/object$parameters$mu2, object$parameters$t.max2, object$parameters$lambda2,
                 object$parameters$A-object$parameters$dY, object$parameters$A, object$parameters$dY, object$parameters$integral, as.character(object$fitFlag), as.character(object$fitFlag2),
                 ifelse(is.null(object$control$smooth.gc), "NULL", as.numeric(object$control$smooth.gc)))
    }

    table               <- data.frame(t(table))
    colnames(table)     <- contents.fitted.spline
    summary.gcFitSpline <- table
    summary.gcFitSpline
}

#' Generic summary function for gcFitModel objects
#'
#' @param object object of class \code{gcFitModel}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters extracted from the growth model fit.
#' @export
#'
summary.gcFitModel <- function(object, ...)
  {
    # object of class gcFitModel

    contents.fitted.param     = c("mu.model", "tD.model", "lambda.model", "A.model", "dY.model",
                                  "A.orig.model", "dY.orig.model",
                                  "integral.model",
                                  "parameter_nu.model",
                                  "parameter_alpha.model",
                                  "parameter_t_shift.model",
                                  "parameter_y0.model",
                                  "stdmu.model", "stdlambda.model", "stdA.model", "reliable_fit.model",
                                  "ci90.mu.model.lo", "ci90.mu.model.up",
                                  "ci90.lambda.model.lo", "ci90.lambda.model.up",
                                  "ci90.A.model.lo", "ci90.A.model.up",
                                  "ci95.mu.model.lo", "ci95.mu.model.up",
                                  "ci95.lambda.model.lo", "ci95.lambda.model.up",
                                  "ci95.A.model.lo", "ci95.A.model.up")


    if ((is.na(object$fitFlag)==TRUE)|(object$fitFlag==FALSE)){
      if(is.na(object$parameters$mu[1])){
        table <- c(rep(NA,15), "FALSE",  rep(NA,12))
      } else {
        table <- c(0, rep(NA,14), as.character(object$fitFlag),  rep(NA,12))
      }
    }
    else{
      table <- c(object$parameters$mu[1], object$parameters$tD[1], object$parameters$lambda[1],  object$parameters$A[1], object$parameters$dY[1],
                 object$parameters$A.orig[1], object$parameters$dY.orig[1],
                 object$parameters$integral,
                 ifelse(!is.null(object$parameters$fitpar$nu[1]),object$parameters$fitpar$nu[1], NA),
                 ifelse(!is.null(object$parameters$fitpar$alpha[1]), object$parameters$fitpar$alpha[1], NA),
                 ifelse(!is.null(object$parameters$fitpar$t_shift[1]), object$parameters$fitpar$t_shift[1], NA),
                 ifelse(!is.null(object$parameters$fitpar$y0[1,1]), object$parameters$fitpar$y0[1,1], NA),
                 object$parameters$mu[2], object$parameters$lambda[2],  object$parameters$A[2], as.character(object$fitFlag),
                 object$parameters$mu[1]-1.645*object$parameters$mu[2], object$parameters$mu[1]+1.645*object$parameters$mu[2],
                 object$parameters$lambda[1]-1.645*object$parameters$lambda[2], object$parameters$lambda[1]+1.645*object$parameters$lambda[2],
                 object$parameters$A[1]-1.645*object$parameters$A[2], object$parameters$A[1]+1.645*object$parameters$A[2],
                 object$parameters$mu[1]-1.96*object$parameters$mu[2], object$parameters$mu[1]+1.96*object$parameters$mu[2],
                 object$parameters$lambda[1]-1.96*object$parameters$lambda[2], object$parameters$lambda[1]+1.96*object$parameters$lambda[2],
                 object$parameters$A[1]-1.96*object$parameters$A[2], object$parameters$A[1]+1.96*object$parameters$A[2])

    }
    table <- data.frame(t(table))
    colnames(table) <- contents.fitted.param
    summary.gcFitModel <- table
    summary.gcFitModel
}

#' Generic summary function for drFit objects
#'
#' @param object object of class \code{drFit}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters for all samples extracted from the dose-response analysis.
#' @export
#'
summary.drFit <- function(object, ...)
  {
    # object of class drFit
  summary.drFit <- data.frame(object$drTable)
  summary.drFit
}

#' Generic summary function for gcBootSpline objects
#'
#' @param object object of class \code{gcBootSpline}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with statistical parameters extracted from the spline fit bootstrapping computation.
#' @export
#'
summary.gcBootSpline <- function(object, ...)
  {
    # object of class gcBootSpline
    contents.bootstrap        <- c("mu.bt", "lambda.bt", "A.bt", "dY.bt", "integral.bt",
                                   "stdmu.bt", "stdlambda.bt", "stdA.bt", "stddY.bt", "stdintegral.bt",
                                   "reliable_fit.bt",
                                   "ci90.mu.bt.lo", "ci90.mu.bt.up", "ci90.lambda.bt.lo", "ci90.lambda.bt.up",
                                   "ci90.A.bt.lo", "ci90.A.bt.up", "ci90.integral.bt.lo", "ci90.integral.bt.up",
                                   "ci95.mu.bt.lo", "ci95.mu.bt.up", "ci95.lambda.bt.lo", "ci95.lambda.bt.up",
                                   "ci95.A.bt.lo", "ci95.A.bt.up", "ci95.integral.bt.lo", "ci95.integral.bt.up")


    if (object$bootFlag==FALSE){
      table<-rep(NA,length(contents.bootstrap))
    }
    else{
      mu          <- mean(object$mu, na.rm=TRUE)
      lambda      <- mean(object$lambda, na.rm=TRUE)
      A           <- mean(object$A, na.rm=TRUE)
      dY          <- mean(object$dY, na.rm=TRUE)
      integral    <- mean(object$integral, na.rm=TRUE)

      mu.sd       <- sd(object$mu, na.rm=TRUE)
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
    summary.gcBootSpline <- table
    summary.gcBootSpline
}

#' Generic summary function for drBootSpline objects
#'
#' @param object object of class \code{drBootSpline}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with statistical parameters extracted from the dose-response bootstrapping analysis.
#' @export
#'
summary.drBootSpline <- function(object, ...)
  {
    # object of class drBootSpline
    contents.bootstrap <- c("drboot.meanEC50", "drboot.sdEC50", "drboot.meanEC50y", "drboot.sdEC50y", "drboot.ci90EC50.lo", "drboot.ci90EC50.up", "drboot.ci95EC50.lo", "drboot.ci95EC50.up",
                            "drboot.meanEC50.orig", "drboot.ci90EC50.orig.lo", "drboot.ci90EC50.orig.up", "drboot.ci95EC50.orig.lo", "drboot.ci95EC50.orig.up")
    if (object$bootFlag==FALSE){
      table                <- rep(NA,length(contents.bootstrap))
    }
    else{
      m.test <- mean(object$ec50.boot, na.rm=TRUE)
      s.test <- sd(object$ec50.boot, na.rm=TRUE)
      m.ECy <- mean(object$ec50y.boot, na.rm = TRUE)
      s.ECy <- sd(object$ec50y.boot, na.rm = TRUE)
      EC50   <- c(m.test, s.test, m.ECy, s.ECy, m.test-1.645*s.test, m.test+1.645*s.test, m.test-1.96*s.test, m.test+1.96*s.test)
      if (object$control$log.x.dr==TRUE){
        EC50.orig <- c(exp(m.test)-1, exp(m.test-1.645*s.test)-1, exp(m.test+1.645*s.test)-1, exp(m.test-1.96*s.test)-1, exp(m.test+1.96*s.test)-1)
      }
      else
      {
        EC50.orig <- c(m.test, m.test-1.645*s.test, m.test+1.645*s.test, m.test-1.96*s.test, m.test+1.96*s.test)
      }

      table <- c(EC50, EC50.orig)
    }

    table                <- data.frame(t(table))
    colnames(table)      <- contents.bootstrap
    summary.drBootSpline <- table
    summary.drBootSpline
}

#' Generic summary function for gcFit objects
#'
#' @param object object of class \code{gcFit}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters extracted from all fits of a workflow.
#' @export
#'
summary.gcFit <- function(object,...)
  {
    # object of class gcFit
  summary.gcFit <- data.frame(object$gcTable)
  summary.gcFit
}

#' Generic summary function for gcFitLinear objects
#'
#' @param object object of class \code{gcFitLinear}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters extracted from the linear fit.
#' @export
#'
summary.gcFitLinear <- function(object,...)
  {
  # object of class gcFitLinear

  contents.fitted.param     = c("mu.linfit", "tD.linfit",
                                "lambda.linfit",
                                "dY.linfit",
                                "A.linfit", "tmu.start.linfit",
                                "tmu.end.linfit",
                                "r2mu.linfit", "reliable_fit.linfit",
                                "mu2.linfit", "tD2.linfit",
                                "tmu2.start.linfit", "tmu2.end.linfit",
                                "r2mu2.linfit", "reliable_fit2.linfit")


  if ((is.na(object$fitFlag)==TRUE)|(object$fitFlag==FALSE)){
    table<-c(0, NA, NA, 0, NA, NA, NA, NA, "FALSE", rep(NA,5), "FALSE")
  }
  else{
    table <- c(object$par[5], log(2)/object$par[5],
               ifelse(!is.na(object$par["lag2"]), ifelse(object$par["lag2"]<object$par["lag"]&&object$par["lag2"]>0, object$par["lag2"], object$par["lag"]), object$par["lag"]),
               object$par[2],
               object$par[3], object$par[9],
               object$par[10],
               object$rsquared, as.character(object$fitFlag),
               object$par[12], object$par[13],
               object$par[16], object$par[17],
               object$rsquared2, as.character(object$fitFlag2))

  }
  table <- data.frame(t(table))
  colnames(table) <- contents.fitted.param
  summary.gcFitLinear <- table
  summary.gcFitLinear
}
