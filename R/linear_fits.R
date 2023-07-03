#' Fit an exponential growth model with a heuristic linear method
#'
#' Determine maximum growth rates from the log-linear part of a growth curve using
#' a heuristic approach similar to the ``growth rates made easy''-method of
#' Hall et al. (2013).
#'
#' The algorithm works as follows:
#' \enumerate{
#'   \item Fit linear regressions (Theil-Sen estimator) to all subsets of \code{h} consecutive, log-transformed data
#'     points (sliding window of size \code{h}). If for example \eqn{h=5}, fit a linear regression to points
#'     1 \dots 5, 2 \dots 6, 3 \dots 7 and so on.
#'   \item Find the subset with the highest slope \eqn{mu_{max}}. Do the \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} and relative standard deviation (RSD) values of the regression meet the in \code{lin.R2} and \code{lin.RSD} defined thresholds and do the data points within the regression window account for a fraction of at least \code{lin.dY} of the total growth increase? If not, evaluate the subset with the second highest slope, and so on.
#'   \item Include also the data points of adjacent subsets that have a slope of at least \eqn{quota \cdot mu{max}}, e.g., all regression windows that have at least 95% of the maximum slope.
#'   \item Fit a new linear model to the extended data window identified in step 3.
#' }
#' If \code{biphasic = TRUE}, the following steps are performed to define a second growth phase:
#' \enumerate{
#'   \item Perform a smooth spline fit on the data with a smoothing factor of 0.5.
#'   \item Calculate the second derivative of the spline fit and perform a smooth spline fit of the derivative with a smoothing factor of 0.4.
#'   \item Determine local maxima and minima in the second derivative.
#'   \item Find the local minimum following \eqn{mu_{max}} and repeat the heuristic linear method for later time values.
#'   \item Find the local maximum before \eqn{mu_{max}} and repeat the heuristic linear method for earlier time values.
#'   \item Choose the greater of the two independently determined slopes as \eqn{mu_{max}2}.
#' }
#'
#' @param time Vector of the independent variable (usually: time).
#' @param data Vector of dependent variable (usually: growth values).
#' @param quota (Numeric, between 0 an 1) Define what fraction of \eqn{mu_{max}} the slope of regression windows adjacent to the window with highest slope should have to be included in the overall linear fit.
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#' @param gcID (Character) The name of the analyzed sample.
#' @param log.x.gc (Logical) Indicates whether _ln(x+1)_ should be applied to the time data for _linear_ and _spline_ fits. Default: \code{FALSE}.
#' @param log.y.lin (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ fits. Default: \code{TRUE}
#' @param min.growth (Numeric) Indicate whether only growth values above a certain threshold should be considered for linear regressions.
#' @param max.growth (Numeric) Indicate whether only growth values below a certain threshold should be considered for linear regressions.
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param tmax (Numeric) Minimum time value considered for linear and spline fits.
#' @param lin.h (Numeric) Manually define the size of the sliding window . If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{\link{growth.gcFitLinear}}
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for calculated slope in \code{\link{growth.gcFitLinear}}
#' @param lin.dY (Numeric) Enter the minimum percentage of growth increase that a linear regression should cover.
#' @param biphasic (Logical) Shall \code{\link{growth.gcFitLinear}} try to extract growth parameters for two different growth phases (as observed with, e.g., diauxic shifts) (\code{TRUE}) or not (\code{FALSE})?
#'
#' @return A \code{gcFitLinear} object with parameters of the fit. The lag time is
#'   estimated as the intersection between the fit and the horizontal line with
#'   \eqn{y=y_0}, where \code{y0} is the first value of the dependent variable.
#'   Use \code{\link{plot.gcFitSpline}} to visualize the linear fit.
#' \item{raw.time}{Raw time values provided to the function as \code{time}.}
#' \item{raw.data}{Raw growth data provided to the function as \code{data}.}
#' \item{filt.time}{Filtered time values used for the heuristic linear method.}
#' \item{filt.data}{Filtered growth values.}
#' \item{log.data}{Log-transformed, filtered growth values used for the heuristic linear method.}
#' \item{gcID}{(Character) Identifies the tested sample.}
#' \item{FUN}{Linear _function_ used for plotting the tangent at mumax.}
#' \item{fit}{\code{lm} object; result of the final call of \code{\link{lm}} to perform the linear regression.}
#' \item{par}{List of determined growth parameters.}
#' \itemize{
#' \item \code{y0}: {Minimum growth value considered for the heuristic linear method.}
#' \item \code{dY}: {Difference in maximum growth and minimum growth.}
#' \item \code{A}: {Maximum growth.}
#' \item \code{y0_lm}: {Intersection of the linear fit with the abscissa.}
#' \item \code{mumax}: {Maximum growth rate (i.e., slope of the linear fit).}
#' \item \code{tD}: {Doubling time.}
#' \item \code{mu.se}: {Standard error of the maximum growth rate.}
#' \item \code{lag}: {Lag time.}
#' \item \code{tmax_start}: {Time value of the first data point within the window used for the linear regression.}
#' \item \code{tmax_end}: {Time value of the last data point within the window used for the linear regression.}
#' \item \code{t_turn}: {For biphasic growth: Time of the inflection point that separates two growth phases.}
#' \item \code{mumax2}: {For biphasic growth: Growth rate of the second growth phase.}
#' \item \code{tD2}: {Doubling time of the second growth phase.}
#' \item \code{y0_lm2}: {For biphasic growth: Intersection of the linear fit of the second growth phase with the abscissa.}
#' \item \code{lag2}: {For biphasic growth: Lag time determined for the second growth phase..}
#' \item \code{tmax2_start}: {For biphasic growth: Time value of the first data point within the window used for the linear regression of the second growth phase.}
#' \item \code{tmax2_end}: {For biphasic growth: Time value of the last data point within the window used for the linear regression of the second growth phase.}
#' }
#' \item{ndx}{Index of data points used for the linear regression.}
#' \item{ndx2}{Index of data points used for the linear regression for the second growth phase.}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#' \item{rsquared}{\ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} of the linear regression.}
#' \item{rsquared2}{\ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} of the linear regression for the second growth phase.}
#' \item{fitFlag}{(Logical) Indicates whether linear regression was successfully performed on the data.}
#' \item{fitFlag2}{(Logical) Indicates whether a second growth phase was identified.}
#' \item{reliable}{(Logical) Indicates whether the performed fit is reliable (to be set manually).}
#'
#' @references Hall, BG., Acar, H, Nandipati, A and Barlow, M (2014) Growth Rates Made Easy. _Mol. Biol. Evol._ 31: 232-38, DOI: 10.1093/molbev/mst187
#' @references Petzoldt T (2022). growthrates: Estimate Growth Rates from Experimental Data. R package version 0.8.3, <https://CRAN.R-project.org/package=growthrates>.
#' @references Theil, H.(1992). A rank-invariant method of linear and polynomial regression analysis. In: Henri Theil’s contributions to economics and econometrics. Springer, pp. 345–381. DOI: 10.1007/978-94-011-2546-8_20
#'
#' @family growth fitting functions
#'
#' @export
#' @examples
#' # Create random growth dataset
#' rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#'
#' # Extract time and growth data for single sample
#' time <- rnd.dataset$time[1,]
#' data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns
#'
#' # Perform linear fit
#' TestFit <- growth.gcFitLinear(time, data, gcID = "TestFit",
#'                  control = growth.control(fit.opt = "l"))
#'
#' plot(TestFit)
#'
growth.gcFitLinear <- function(time, data, gcID = "undefined", quota = 0.95,
                               control = growth.control(t0 = 0, tmax = NA, log.x.gc = FALSE, log.y.lin = TRUE, min.growth = NA, max.growth = NA, lin.h = NULL, lin.R2 = 0.97, lin.RSD = 0.1, lin.dY = 0.05, biphasic = FALSE))
{
  R2 <- control$lin.R2
  RSD <- control$lin.RSD
  h <- control$lin.h
  fit.dY <- control$lin.dY
  t0 <- control$t0
  tmax <- control$tmax
  min.growth <- control$min.growth
  max.growth <- control$max.growth

  if(length(data[data<0]) > 0){
    data <- data + abs(min(data[data<0]))+0.01 # add the absolute value of the minimum negative growth (+ 0.01) to the data
  }

  bad.values <- ((is.na(time))|(is.na(data)) | time < 0 | data <=0)
  data.in <- data <- data[!bad.values]
  time.in <- time <- time[!bad.values]
  if(!is.null(t0) && !is.na(t0) && t0 != ""){
    t0 <- as.numeric(t0)
  } else {
    t0 <- 0
  }
  if(!is.null(tmax) && !is.na(tmax) && tmax != ""){
    tmax <- as.numeric(tmax)
  } else {
    tmax <- NA
  }

  if(length(data) < 4){
    if(control$suppress.messages==F) message(paste0("Linear fit: Not enough valid values in sample to perform fit."))
    gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = NA, filt.data = NA,
                        log.data = NA, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                          t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                          tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
    )
    class(gcFitLinear) <- "gcFitLinear"
    return(gcFitLinear)
  }

  # extract period of growth (from defined t0 to tmax)
  t.growth <- time[which.min(abs(time-t0)):which.max(data)]
  if(!is.na(tmax)){
    t.growth <-  t.growth[t.growth <= tmax]
  }
  if(!is.null(h) && !is.na(h) && h != ""){
    h <- as.numeric(h)
  } else {
    # determine number of data points in period until maximum growth
    n.spl <- length(t.growth)
    # Calculate h via log-transformation of the number of data points
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
  control$lin.h <- h

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
  data.log <- log(data.in/data.in[1])
  if(!is.null(control$min.growth) && !is.na(control$min.growth)){
    if(control$min.growth != 0 && control$log.y.lin == TRUE){
      min.growth <- log(control$min.growth / data[1])
    } else {
      min.growth <- 0
    }
  } else {
    min.growth <- 0
  }
  if(!is.null(control$max.growth) && !is.na(control$max.growth)){
    if(control$log.y.lin == TRUE){
      max.growth <- log(max.growth / data[1])
    }
  } else {
    max.growth <- NA
  }
  bad.values <- ((is.na(data.log))|(is.infinite(data.log))|(is.na(time))|(is.na(data.log)))

  # /// remove bad values or stop program
  if (TRUE%in%bad.values){
    if (control$neg.nan.act==FALSE){
      time    <- time[!bad.values]
      data.log    <- data.log[!bad.values]
      data <- data[!bad.values]
    }
    else{
      stop("Bad values in gcFitModel")
    }
  }
  if (any(duplicated(time))) stop("time variable must not contain duplicated values")

  # store filtered and transformed data
  obs <- data.frame(time, data)
  obs$ylog <- data.log
  obs.max.growth <- max(obs$data)
  dY.total <- obs.max.growth - obs$data[1]

  if(max(data.in) < control$growth.thresh * data.in[1]){
    if(control$suppress.messages==F) message(paste0("Linear fit: No significant growth detected (with all values below ", control$growth.thresh, " * start_value)."))
    gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                        log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                          t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                          tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
    )
    class(gcFitLinear) <- "gcFitLinear"
    if(control$suppress.messages==F) message("Linear fit: No data range in accordance with the chosen parameters identified with appropriate linearity.")
    return(gcFitLinear)
  }
  ## number of values
  N <- nrow(obs)

  if(N > h && N>3){
    # Perform linear regression for all N windows and save results in 'ret'
    ret <- matrix(0, nrow = N - h, ncol = 6)
    if (control$log.y.lin == TRUE) {
      for(i in 1:(N - h)) {
        ret[i, ] <- c(i, with(obs, (lm_parms(lm_window(time, ylog, i0 = i, h = h)))))
      }
    } else {
      for(i in 1:(N - h)) {
        ret[i, ] <- c(i, with(obs, (lm_parms(lm_window(time, data, i0 = i, h = h)))))
      }
    }
    colnames(ret) <- c("index", "y-intersect", "slope", "X4", "R2", "RSD")
    # add time and growth values as columns in ret
    if (control$log.y.lin == TRUE) {
      ret <- data.frame(ret, time = time[ret[,1]], data = obs$ylog[ret[,1]])
    } else {
      ret <- data.frame(ret, time = time[ret[,1]], data = obs$data[ret[,1]])
    }

    # add dY, i.e., the percentage of growth that a regression window covers, to ret
    ret <- data.frame(ret, dY = ((obs$data[match(ret[, "time"], obs$time)+(h-1)] - obs$data[match(ret[, "time"], obs$time)]) / dY.total))

    bad <- is.na(ret[,5]) | is.na(ret[,6])
    ret <- ret[!bad,]
    # Consider only regressions within the growth phase (from start to maximum growth)
    ret <- ret[ret$time <= t.growth[length(t.growth)], ]
    if(nrow(ret)<2){
      gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                          log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                            t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                            tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
      )
      class(gcFitLinear) <- "gcFitLinear"
      if(control$suppress.messages==F) message("No data range in accordance with the chosen parameters identified with appropriate linearity.")
      return(gcFitLinear)
    }
    else{
      # duplicate ret for further tuning of fit
      if(exists("min.growth")){
        ret.check <- ret[max(which.min(abs(time-t0)), which.min(abs(ret$data-min.growth))) : nrow(ret),] # consider only slopes from defined t0 and min.growth
      } else {
        ret.check <- ret[which.min(abs(time-t0)):nrow(ret),] # consider only slopes from defined t0
      }
      if(!is.na(tmax)){
        ret.check <- ret.check[(ret.check[,"time"]+(h*time[2]-time[1])) <= tmax, ] # consider only slopes up to defined tmax
      }
      if(!is.na(max.growth)){
        if (control$log.y.lin) {
          ret.check <- ret.check[unlist(lapply(1:nrow(ret.check), function(x) obs$ylog[ret.check[x, 1]+(h-1)])) <= max.growth, ] # consider only slopes up to defined max.growth
        } else {
          ret.check <- ret.check[unlist(lapply(1:nrow(ret.check), function(x) obs$data[ret.check[x, 1]+(h-1)])) <= max.growth, ] # consider only slopes up to defined max.growth
        }
      }

      #Consider only slopes that span at least fit.dY
      ret.check <- ret.check[ret.check[,"dY"]>=fit.dY, ]

      # Consider only positive slopes
      ret.check <- ret.check[ret.check[,"slope"]>0, ]

      if(nrow(ret.check)<2){
        gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                            log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                              y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                              t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                              tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
        )
        class(gcFitLinear) <- "gcFitLinear"
        if(control$suppress.messages==F) message("No data range in accordance with the chosen parameters identified with appropriate linearity.")
        return(gcFitLinear)
      } else {
        ## Determine index of window with maximum growth rate, iterate until regression is found that meets R2 and RSD criterion
        success <- FALSE
        # apply min.growth to list of linear regressions
        ret.check <- ret.check[ret.check[, 8] >= min.growth, ]
        ret.check2 <- ret.check
        if(any(ret.check2[,5] >= R2 & abs(ret.check2[,6]) <= RSD)){
          while (!success){
            index.max <- which.max(ret.check2[, 3])
            if(ret.check2[index.max,5] >= R2 && abs(ret.check2[index.max,6]) <= RSD && !is.na(ret.check2[index.max,6]) ){ # prerequisites for suitable µmax candidate: R2 and RSD
              slope.max <- ret.check2[index.max,3]
              success <- TRUE
            } else {
              ret.check2 <- ret.check2[-index.max,]
            }
          }
          index.max.ret <- ret.check[which(ret.check[,3]==slope.max),1] # index of maximum slope in fit table
          slope.quota <- quota * slope.max
          if(exists("min.growth")){
            candidates <- ret.check[which(ret.check[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                            ret.check[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                            abs(ret.check[, 6]) <= 1.02 * RSD & # RSD criterion for candidates
                                            ret.check[, 7] >= t0 & # consider only slopes after defined t0
                                            ret.check[, 8] >= min.growth # consider only slopes at densities higher than "min.growth"
            ), 1]
          } else{
            candidates <- ret.check[which(ret.check[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                            ret.check[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                            abs(ret.check[, 6]) <= 1.02 * RSD & # RSD criterion for candidates
                                            ret.check[, 7] >= t0), 1] # consider only slopes after defined t0
          }
          #consider only candidate windows next to index.max.ret
          candidate_intervals <- split(candidates, cumsum(c(1, diff(candidates) != 1))) # split candidates into consecutive intervals
          if(any(index.max.ret %in% unlist(candidate_intervals))){
            # which interval contains maximum slope?
            ndx <-  as.numeric(which(sapply(
              candidate_intervals,
              FUN = function(X)
                any(X %in% index.max.ret)
            )))

            candidates <-
              unlist(candidate_intervals[ndx])
          }


          if(length(candidates) > 0) {
            #perform linear regression with candidate data points
            tp <- seq(min(candidates), max(candidates) + h-1)
            if (control$log.y.lin == TRUE) {
              m <- lm_window(obs$time, obs$ylog, min(tp), length(tp)) # linear model
            } else {
              m <- lm_window(obs$time, obs$data, min(tp), length(tp)) # linear model
            }
            p  <- c(lm_parms(m), n=length(tp)) # # slope equation parameters (linear model)
          } else {
            p <- c(a=0, b=0, se=0, r2=0, cv=0, n=0)
            m = NULL
          }

          if(length(candidates) > 0) {
            ## get time window of exponential fit
            tmax_start <- obs$time[tp[1]]
            tmax_end <- obs$time[tp[length(tp)]]

            y0_lm    <- unname(coef(m)[1]) # y-intercept of tangent
            if (control$log.y.lin == TRUE) {
              y0_data  <- obs$ylog[1] # y0 in dataset
            } else {
              y0_data  <- obs$data[1] # y0 in dataset
            }
            mumax <- unname(coef(m)[2])

            ## estimate lag phase
            lambda <- (y0_data - y0_lm) / mumax

            # correct y0 values for Ln(y(t)/y0)
            if (control$log.y.lin == TRUE) {
              y0_lm <- obs$data[1] * exp(y0_lm)
              y0_data <- obs$data[1]
            }


            # get indices of time points used in linear fit
            ndx <- seq(min(match(ret.check[match(candidates, ret.check[,1]), "time"], time.in)),
                       max(match(ret.check[match(candidates, ret.check[,1]), "time"], time.in)) + h-1)

            mu.se <- as.numeric(p[3]) # standard error of slope
            fitFlag <- TRUE

          }
          else { # of if(length(candidates) > 0)
            gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                                log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                                  y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                                  t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                                  tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
            )
            class(gcFitLinear) <- "gcFitLinear"
            if(!control$suppress.messages) message(paste0("No linear fit in accordance with the chosen parameters identified with: R2 >= ", R2, ", RSD <= ", RSD, ", t0 = ", t0, ", and min.growth = ", control$min.growth, "."))
            return(gcFitLinear)
          }
          if(control$biphasic) {
            if (control$log.y.lin == TRUE) {
              spline <- stats::smooth.spline(obs$time, obs$ylog, spar = 0.5, cv = NA, keep.data = FALSE)
            } else {
              spline <- stats::smooth.spline(obs$time, obs$data, spar = 0.5, cv = NA, keep.data = FALSE)
            }
            deriv2 <- stats::predict(spline, deriv = 2)
            deriv2_spline <- stats::smooth.spline(deriv2$x, deriv2$y, spar = 0.4, cv = NA, keep.data = FALSE)
            # extract local minima and maxima of deriv2
            n = ceiling(h/2)
            minima <- inflect(deriv2_spline$y, threshold = n)$minima
            maxima <- inflect(deriv2_spline$y, threshold = n)$maxima
            # Regard only (negative) minima and (positive) maxima with a deriv2 value of >= 10% mumax
            minima <- minima[deriv2_spline$y[minima] <= 0.1 * (-mumax)]
            maxima <- maxima[deriv2_spline$y[maxima] >= 0.1 * mumax]

            # expand mumax window with more relaxed quota
            slope.quota.ext <- 0.8 * slope.max
            if(exists("min.growth")){
              candidates.ext <- ret[which(ret[, 3] >= slope.quota.ext & # indices of slopes greater than slope.quota
                                            ret[, 5] >= 0.95*R2 & # R2 criterion for candidates
                                            abs(ret[, 6]) <= 1.1 * RSD & # RSD criterion for candidates
                                            ret[, 7] >= t0 & # consider only slopes after defined t0
                                            ret[, 8] >= min.growth # consider only slopes at densities higher than "min.growth"
              ), 1]
            } else{
              candidates.ext <- ret[which(ret[, 3] >= slope.quota.ext & # indices of slopes greater than slope.quota
                                            ret[, 5] >= 0.95*R2 & # R2 criterion for candidates
                                            abs(ret[, 6]) <= 1.1 * RSD & # RSD criterion for candidates
                                            ret[, 7] >= t0), 1] # consider only slopes after defined t0
            }



            #consider only candidate windows next to index.max.ret
            candidate_intervals.ext <- split(candidates.ext, cumsum(c(1, diff(candidates.ext) != 1)))
            if(any(index.max.ret %in% unlist(unname(candidate_intervals.ext)))){
              ndx.ext <-  as.numeric(which(sapply(
                candidate_intervals.ext,
                FUN = function(X)
                  any(X %in% index.max.ret)
              )))

              candidates.ext <-
                candidate_intervals.ext[[ndx.ext]]
            }
            tp.ext <- seq(min(candidates.ext), max(candidates.ext) + h-1)



            # # # Color functions
            #   cf.1 <- grDevices::colorRampPalette(c("red", "red"))
            #   cf.2 <- grDevices::colorRampPalette(c("blue", "blue"))
            #   plot(deriv2$x, deriv2$y, type = 'l', main = "Minima \nVariable Thresholds")
            #   points(
            #       deriv2$x[minima],
            #       deriv2$y[minima],
            #       pch = 16,
            #       col = cf.2(1)[1],
            #       cex = 1.5
            #     )
            #   points(deriv2$x[maxima], deriv2$y[maxima], pch = 16, col = cf.1(1)[1], cex = 1.5)
            #
            #   plot(spline$x, spline$y, type = "l")
            #   points(spline$x[minima], spline$y[minima], pch = 16, col = cf.2(1)[1], cex = 1.5)
            #   points(spline$x[maxima], spline$y[maxima], pch = 16, col = cf.1(1)[1], cex = 1.5)
            #
            #   plot(deriv2_spline$x, deriv2_spline$y, type = 'l', main = "Minima \nVariable Thresholds")
            #   points(
            #     deriv2_spline$x[minima],
            #     deriv2_spline$y[minima],
            #     pch = 16,
            #     col = cf.2(1)[1],
            #     cex = 1.5
            #   )
            #   points(deriv2_spline$x[maxima], deriv2_spline$y[maxima], pch = 16, col = cf.1(1)[1], cex = 1.5)


            # get local deriv2-minimum after mumax
            # postmin.ndx <- minima[(minima - tp.ext[length(tp.ext)]) >= 0][which.min(minima[(minima - tp.ext[length(tp.ext)]) >= 0])]
            postmin.ndx <- minima[which.min(abs(minima - tp.ext[length(tp.ext)]))]
            # extract linear regression results after post-mumax turning point
            ret.postmin <- ret[ret$time >= obs$time[postmin.ndx],]
            # remove indices included in extended mumax regression
            ret.postmin <- ret.postmin[!(ret.postmin[,1] %in% tp.ext),]
            #Consider only slopes that span at least fit.dY
            ret.postmin <- ret.postmin[ret.postmin[, "dY"] >= fit.dY, ]
            # Consider only positive slopes
            ret.postmin <- ret.postmin[ret.postmin[, "slope"] > 0, ]
            ## Determine index of window with maximum growth rate, iterate until regression is found that meets R2 and RSD criterion
            success <- FALSE
            # apply min.growth to list of linear regressions
            ret.postmin.check <- ret.postmin[ret.postmin[, 8] >= min.growth, ]
            # apply max.growth to list of linear regressions
            if(!is.na(max.growth)){
              ret.postmin.check <- ret.postmin.check[ret.postmin.check[,"data"] <= max.growth, ] # consider only slopes up to defined max.growth
            }
            # apply max. time to list of linear regeressions
            if(!is.na(tmax)){
              ret.postmin.check <- ret.postmin.check[ret.postmin.check[,"time"] <= tmax, ] # consider only slopes up to defined t0
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
              candidates.postmin <- ret.postmin[which(
                ret.postmin[, 3] >= slope.quota &
                  ret.postmin[, 5] >= 0.98 * R2 &
                  abs(ret.postmin[, 6]) <= 1.02 * RSD &
                  ret.postmin[, 7] >= t0), 1]
              #consider only candidate windows next to index.max.ret.postmin
              candidate_intervals.postmin <- split(candidates.postmin, cumsum(c(1, diff(candidates.postmin) != 1)))
              if (any(index.max.ret.postmin %in% unlist(candidate_intervals.postmin))) {
                ndx.postmin <-  as.numeric(which(sapply(
                  candidate_intervals.postmin,
                  FUN = function(X)
                    any(X %in% index.max.ret.postmin)
                )))

                candidates.postmin <-
                  candidate_intervals.postmin[[ndx.postmin]]
              }

              if (length(candidates.postmin) > 0) {
                #perform linear regression with candidate data points
                tp.postmin <- seq(min(candidates.postmin), max(candidates.postmin) + h - 1)
                if (control$log.y.lin == TRUE) {
                  m.postmin <- lm_window(obs$time, obs$ylog, min(tp.postmin), length(tp.postmin)) # linear model
                } else {
                  m.postmin <- lm_window(obs$time, obs$data, min(tp.postmin), length(tp.postmin)) # linear model
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
                ## get time window of exponential fit
                tmax_start.postmin <- obs$time[tp.postmin[1]]
                tmax_end.postmin <- obs$time[tp.postmin[length(tp.postmin)]]
                t_turn.postmin <- obs$time[postmin.ndx]
                y0_lm.postmin <- unname(coef(m.postmin)[1]) # y-intercept of tangent
                mumax.postmin <- unname(coef(m.postmin)[2])

                ## estimate lag phase between first and second growth phase
                if (control$log.y.lin == TRUE) {
                  lambda.postmin <- (obs$ylog[1] - y0_lm.postmin) / mumax.postmin
                } else {
                  lambda.postmin <- (obs$data[1] - y0_lm.postmin) / mumax.postmin
                }

                # correct y0 values for Ln(y(t)/y0)
                if (control$log.y.lin == TRUE) {
                  y0_lm.postmin <- obs$data[1] * exp(y0_lm.postmin)
                }

                # get indices of time points used in linear fit
                ndx.postmin <- seq(min(match(ret[match(candidates.postmin, ret[,1]), "time"], time.in)),
                                   max(match(ret[match(candidates.postmin, ret[,1]), "time"], time.in)) + h -
                                     1)
                mu.se.postmin <- as.numeric(p.postmin[3]) # standard error of slope
                rsquared.postmin <- p.postmin["r2"]
                fitFlag.postmin <- TRUE
              }
              else {
                # of if(length(candidates.postmin) > 0)
                y0_lm.postmin = NA
                mumax.postmin = NA
                mu.se.postmin = NA
                lag.postmin = NA
                tmax_start.postmin = NA
                tmax_end.postmin = NA
                ndx.postmin = NA
                rsquared.postmin = NA
                fitFlag.postmin = FALSE
              }
            } # if (any(ret.postmin.check[, 5] >= R2 & abs(ret.postmin.check[, 6]) <= RSD))
            else {
              y0_lm.postmin = NA
              mumax.postmin = NA
              mu.se.postmin = NA
              lag.postmin = NA
              tmax_start.postmin = NA
              tmax_end.postmin = NA
              ndx.postmin = NA
              rsquared.postmin = NA
              fitFlag.postmin = FALSE
            }

            # get local deriv2-maximum before mumax
            # premin.ndx <- maxima[(maxima - tp.ext[1]) <= 0][which.max(maxima[(maxima - tp.ext[1]) <= 0])]
            premin.ndx <- maxima[which.min(abs(maxima - tp.ext[1]))]
            # extract linear regression results before pre-mumax turning point
            ret.premin <- ret[ret$time <= obs$time[premin.ndx-h],]
            #Consider only slopes that span at least fit.dY
            ret.premin <- ret.premin[ret.premin[, "dY"] >= fit.dY, ]
            # remove indices included in extended mumax regression
            ret.premin <- ret.premin[!(ret.premin[,1] %in% tp.ext),]
            # Consider only positive slopes
            ret.premin <- ret.premin[ret.premin[, "slope"] > 0, ]
            ## Determine index of window with maximum growth rate, iterate until regression is found that meets R2 and RSD criterion
            success <- FALSE
            # apply min.growth to list of linear regressions
            ret.premin.check <- ret.premin[ret.premin[, 8] >= min.growth, ]
            # apply max.growth to list of linear regressions
            if(!is.na(max.growth)){
              ret.premin.check <- ret.premin.check[ret.premin.check[,"data"] <= max.growth, ] # consider only slopes up to defined max.growth
            }
            # apply max. time to list of linear regeressions
            if(!is.na(tmax)){
              ret.premin.check <- ret.premin.check[ret.premin.check[,"time"] <= tmax, ] # consider only slopes up to defined tmax
            }
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
              candidate_intervals.premin <- split(candidates.premin, cumsum(c(1, diff(candidates.premin) != 1)))
              if (any(unlist(candidate_intervals.premin) %in% index.max.ret.premin)) {
                ndx.premin <-  as.numeric(which(sapply(
                  candidate_intervals.premin,
                  FUN = function(X)
                    any(X %in% index.max.ret.premin)
                )))

                candidates.premin <-
                  candidate_intervals.premin[[ndx.premin]]
              }


              if (length(candidates.premin) > 0) {
                #perform linear regression with candidate data points
                tp.premin <- seq(min(candidates.premin), max(candidates.premin) + h - 1)
                if (control$log.y.lin == TRUE) {
                  m.premin <- lm_window(obs$time, obs$ylog, min(tp.premin), length(tp.premin)) # linear model
                } else {
                  m.premin <- lm_window(obs$time, obs$data, min(tp.premin), length(tp.premin)) # linear model
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
                ## get time window of exponential fit
                tmax_start.premin <- obs$time[tp.premin[1]]
                tmax_end.premin <- obs$time[tp.premin[length(tp.premin)]]
                t_turn.premin <- obs$time[premin.ndx]
                y0_lm.premin    <- unname(coef(m.premin)[1]) # y-intercept of tangent
                mumax.premin <- unname(coef(m.premin)[2])

                ## estimate lag phase between first and second growth phase
                if (control$log.y.lin == TRUE) {
                  lambda.premin <- (obs$ylog[1] - y0_lm.premin) / mumax.premin
                } else {
                  lambda.premin <- (obs$data[1] - y0_lm.premin) / mumax.premin
                }

                # correct y0 values for Ln(y(t)/y0)
                if (control$log.y.lin == TRUE) {
                  y0_lm.premin <-  obs$data[1] * exp(y0_lm.premin)
                }

                # get indices of time points used in linear fit
                ndx.premin <- seq(min(match(ret[match(candidates.premin, ret[,1]), "time"], time.in)),
                                  max(match(ret[match(candidates.premin, ret[,1]), "time"], time.in)) + h -
                                    1)
                mu.se.premin <- as.numeric(p.premin[3]) # standard error of slope
                rsquared.premin <- p.premin["r2"]
                fitFlag.premin <- TRUE
              }
              else {
                # of if(length(candidates.premin) > 0)
                y0_lm.premin = NA
                mumax.premin = NA
                mu.se.premin = NA
                lag.premin = NA
                tmax_start.premin = NA
                tmax_end.premin = NA
                ndx.premin = NA
                rsquared.premin = NA
                fitFlag.premin = FALSE
              }
            } # if (any(ret.premin.check[, 5] >= R2 & abs(ret.premin.check[, 6]) <= RSD))
            else{
              y0_lm.premin = NA
              mumax.premin = NA
              mu.se.premin = NA
              lag.premin = NA
              tmax_start.premin = NA
              tmax_end.premin = NA
              ndx.premin = NA
              rsquared.premin = NA
              fitFlag.premin = FALSE
            }

            # Choose regression before or after mumax as second growth phase based on second growth rate
            if(!is.na(mumax.premin) && !is.na(mumax.postmin)){
              mumax2 <- ifelse(mumax.premin > mumax.postmin, mumax.premin, mumax.postmin)
              y0_2 <- ifelse(mumax.premin > mumax.postmin, y0_lm.premin, y0_lm.postmin)
              lag2 <- ifelse(mumax.premin > mumax.postmin, lambda.premin, lambda.postmin)
              tmax2_start <- ifelse(mumax.premin > mumax.postmin, tmax_start.premin, tmax_start.postmin)
              tmax2_end <- ifelse(mumax.premin > mumax.postmin, tmax_end.premin, tmax_end.postmin)
              rsquared2 <- ifelse(mumax.premin > mumax.postmin, rsquared.premin, rsquared.postmin)
              ndx2 <- if(mumax.premin > mumax.postmin){ndx.premin} else{ndx.postmin}
              t_turn <- ifelse(mumax.premin > mumax.postmin, t_turn.premin, t_turn.postmin)
              fitFlag2 <- TRUE
            } else if (!is.na(mumax.premin)){
              mumax2 <- mumax.premin
              y0_2 <- y0_lm.premin
              lag2 <- lambda.premin
              tmax2_start <- tmax_start.premin
              tmax2_end <- tmax_end.premin
              t_turn <- t_turn.premin
              rsquared2 <- rsquared.premin
              ndx2 <- ndx.premin
              fitFlag2 <- TRUE
            } else if (!is.na(mumax.postmin)){
              mumax2 <- mumax.postmin
              y0_2 <- y0_lm.postmin
              lag2 <- lambda.postmin
              tmax2_start <- tmax_start.postmin
              tmax2_end <- tmax_end.postmin
              t_turn <- t_turn.postmin
              rsquared2 <- rsquared.postmin
              ndx2 <- ndx.postmin
              fitFlag2 <- TRUE
            } else {
              mumax2 <- NA
              y0_2 <- NA
              lag2 <- NA
              tmax2_start <- NA
              tmax2_end <- NA
              t_turn <- NA
              rsquared2 <- NA
              ndx2 <- NA
              fitFlag2 <- FALSE
            }
          } # if(control$biphasic)
          else{
            mumax2 <- NA
            y0_2 <- NA
            lag2 <- NA
            tmax2_start <- NA
            tmax2_end <- NA
            t_turn <- NA
            rsquared2 <- NA
            ndx2 <- NA
            fitFlag2 <- FALSE
          }
        } # if(any(ret.check[,5] >= R2 & ret.check[,6] <= RSD))
        else{
          gcFitLinear <-
            list(
              raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
              log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
            )
          class(gcFitLinear) <- "gcFitLinear"
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
          return(gcFitLinear)
        }
      } # else of if(nrow(ret.check)<2)
    } # else of if(nrow(ret)<2)
  } # if(N >= 3)
  else {
    message("Not enough observations in the dataset to perform linear fit. growth.gcFitLinear requires at least 3 data points between t0 and the time of maximum growth.")
    gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                        log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                          t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                          tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
    )
    class(gcFitLinear) <- "gcFitLinear"
    return(gcFitLinear)
  }

  gcFitLinear <- list(
    raw.time = time.in,
    raw.data = data.in,
    filt.time = obs$time,
    filt.data = obs$data,
    log.data = obs$ylog,
    gcID = gcID,
    FUN = if (control$log.y.lin == TRUE) {
      grow_exponential
    } else {
      grow_linear
    },
    fit = m,
    par = c(
      y0 = y0_data,
      dY = max(obs$data)-obs$data[1],
      A = max(obs$data),
      y0_lm = y0_lm,
      mumax = mumax,
      tD = log(2)/mumax,
      mu.se = mu.se,
      lag = lambda,
      tmax_start = tmax_start,
      tmax_end = tmax_end,
      t_turn = t_turn,
      mumax2 = mumax2,
      tD2 = log(2)/mumax2,
      y0_lm2 = y0_2,
      lag2 = lag2,
      tmax2_start = tmax2_start,
      tmax2_end = tmax2_end
    ),
    ndx = ndx,
    ndx2 = ndx2,
    quota = quota,
    rsquared = p["r2"],
    rsquared2 = rsquared2,
    control = control,
    fitFlag = fitFlag,
    fitFlag2 = fitFlag2,
    reliable = NULL
  )

  class(gcFitLinear) <- "gcFitLinear"

  invisible(gcFitLinear)
}

#' Data fit via a heuristic linear method
#'
#' Determine maximum slopes from using a heuristic approach similar to the ``growth rates made easy''-method of
#' Hall et al. (2013).
#'
#' @param time Vector of the independent time variable (if x_type = "time" in control object).
#' @param growth Vector of the independent time growth (if x_type = "growth" in control object).
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
#' \item{par}{List of determined fluorescence parameters.}
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
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file("lac_promoters.xlsx", package = "QurvE"),
#'                    data.fl = system.file("lac_promoters.xlsx", package = "QurvE"),
#'                    sheet.growth = 1,
#'                    sheet.fl = 2 )
#'
#' # Extract time and normalized fluorescence data for single sample
#' time <- input$time[4,]
#' data <- input$norm.fluorescence[4,-(1:3)] # Remove identifier columns
#'
#' # Perform linear fit
#' TestFit <- flFitLinear(time = time,
#'                        fl_data = data,
#'                        ID = "TestFit",
#'                        control = fl.control(fit.opt = "l", x_type = "time",
#'                        lin.R2 = 0.95, lin.RSD = 0.1,
#'                        lin.h = 20))
#'
#' plot(TestFit)
#
flFitLinear <- function(time = NULL, growth = NULL, fl_data, ID = "undefined",  quota = 0.95,
                        control = fl.control(x_type = c("growth", "time"), log.x.lin = FALSE, log.y.lin = FALSE, t0 = 0, min.growth = NA, lin.h = NULL, lin.R2 = 0.98, lin.RSD = 0.05, lin.dY = 0.05, biphasic = FALSE))
{
  x_type <- control$x_type
  R2 <- control$lin.R2
  RSD <- control$lin.RSD
  h <- control$lin.h
  t0 <- control$t0
  tmax <- control$tmax
  min.growth <- control$min.growth
  max.growth <- control$max.growth

  if (is(control) != "fl.control")
    stop("control must be of class fl.control!")
  if (!any(control$fit.opt %in% "l"))
    stop("Fit option is not set for a fluorescence linear fit. See fl.control()")

  if(!is.null(time))   time.in <- time <- as.vector(as.numeric(as.matrix(time)))[!is.na(as.vector(as.numeric(as.matrix(time))))]
  if(!is.null(growth)) growth.in <- growth <- as.vector(as.numeric(as.matrix(growth)))[!is.na(as.vector(as.numeric(as.matrix(growth))))]
  fl_data.in <- fl_data <- as.vector(as.numeric(as.matrix(fl_data)))[!is.na(as.vector(as.numeric(as.matrix(fl_data))))]

  if(!is.null(t0) && !is.na(t0) && t0 != ""){
    t0 <- as.numeric(t0)
  } else {
    t0 <- 0
  }
  if(x_type == "growth" && is.null(growth))
    stop("flFitLinear: To perform a linear fit of fluorescence vs. growth data, please provide a 'growth' vector of the same length as 'fl_data'.")
  if(x_type == "time" && is.null(time))
    stop("flFitLinear: To perform a linear fit of fluorescence vs. time data, please provide a 'time' vector of the same length as 'fl_data'.")
  if(x_type == "growth" && length(growth) != length(fl_data))
    stop("flFitLinear: length of input vectors (growth and fl_data) differ!")
  if(x_type == "time" && length(time) != length(fl_data))
    stop("flFitLinear: length of input vectors (time and fl_data) differ!")
  if (length(fl_data) < 5) {
    warning("flFitLinear: There is not enough valid data. Must have at least 5!")
  }

  # Consider only data points up to max growth or time, respectively
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
  if(x_type == "growth"){
    ndx.max <- which.max(growth)
    growth <- growth[1:ndx.max]
    fl_data <- fl_data[1:ndx.max]
    bad.values <- (fl_data < 0)
    if (TRUE %in% bad.values) {
      fl_data <- fl_data[!bad.values]
      growth <- growth[!bad.values]
    }
  }

  if(length(fl_data) < 4){
    if(control$suppress.messages==F) message(paste0("Linear fit: Not enough valid values in sample to perform fit."))
    flFitLinear <- list(x.in = get(ifelse(x_type == "growth", "growth.in", "time.in")), fl.in = fl_data.in,
                        raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                        filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
                        ID = ID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                          x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                          x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
    class(flFitLinear) <- "flFitLinear"
    return(flFitLinear)
  }

  fl_data.log <- log(fl_data/fl_data[1])

  if(x_type == "growth"){
    bad.values <- (is.na(growth)) | (is.na(fl_data)) |
      (!is.numeric(growth)) | (!is.numeric(fl_data) )
    if (TRUE %in% bad.values) {
      growth <- growth[!bad.values]
      fl_data <- fl_data[!bad.values]
    }
    if (control$log.x.lin == TRUE) {
      bad.values <- (growth < 0)
      if (TRUE %in% bad.values) {
        growth <- growth[!bad.values]
        fl_data <- fl_data[!bad.values]
      }
      growth.log <- log(growth/growth[1])
    }

    if(max(growth) < control$growth.thresh * growth[1]){
      if(control$suppress.messages==F) message(paste0("flFitLinear: No significant growth detected (with all values below ", control$growth.thresh, " * start_value)."))
      flFitLinear <- list(x.in = get(ifelse(x_type == "growth", "growth.in", "time.in")), fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
                          ID = ID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                            x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                            x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
      class(flFitLinear) <- "flFitLinear"
      return(flFitLinear)
    }
    # Implement min.growth into dataset
    # if(!is.null(control$min.growth)) {
    #   if (!is.na(control$min.growth) && control$min.growth != 0) {
    #     min.growth <- control$min.growth
    #     if (control$log.y.lin == TRUE) {
    #       # perfom log transformation on min.growth (Ln(y/y0))
    #       min.growth <- log(min.growth / growth[1])
    #       fl_data.log <- fl_data.log[growth.log >= min.growth]
    #       growth.log <- growth.log[growth.log >= min.growth]
    #     } else {
    #       fl_data <- fl_data[growth >= min.growth]
    #       growth <- growth[growth >= min.growth]
    #     }
    #   }
    # }


    # Remove data points where y values stack on top of each other
    fl_data <- fl_data[growth >= cummax(growth)]
    fl_data.log <- fl_data.log[growth >= cummax(growth)]
    growth <- growth[growth >= cummax(growth)]
    if (control$log.x.lin == FALSE) {
      x <- growth
    } else {
      x <- growth.log
    }
    if(!is.null(control$max.growth) && !is.na(control$max.growth)){
      if(control$log.x.lin == TRUE){
        max.growth <- log(max.growth / growth[1])
      }
    } else {
      max.growth <- NA
    }
    if(length(x)<4){
      message("flFitLinear: Not enough data points above the chosen min.growth.")
      flFitLinear <- list(x.in = get(ifelse(x_type == "growth", "growth.in", "time.in")), fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
                          ID = ID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                            x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                            x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
      class(flFitLinear) <- "flFitLinear"
      return(flFitLinear)
    }
  } # if(x_type == "growth")
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
      flFitLinear <- list(x.in = get(ifelse(x_type == "growth", "growth.in", "time.in")), fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
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
      flFitLinear <- list(x.in = get(ifelse(x_type == "growth", "growth.in", "time.in")), fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
                          ID = ID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                            x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                            x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
      class(flFitLinear) <- "flFitLinear"
      return(flFitLinear)
    }
  } # if(x_type == "time")
  # extract period of fluorescence increase
  x.in = get(ifelse(x_type == "growth", "growth.in", "time.in"))
  end <- FALSE
  step <- 0
  while(end==FALSE){
    step <- step+1
    fldat <- fl_data[step:length(fl_data)]
    fl.max.ndx <- which.max(fldat)
    if(fl.max.ndx == 1) next
    else fl.max.ndx <- fl.max.ndx + (step); end = TRUE
  }
  x.inc <- x[which.min(abs(x)):fl.max.ndx]
  if(x_type == "time" && !is.na(tmax)){
    x.inc <-  x.inc[x.inc <= tmax]
  }
  if(!is.null(h) && !is.na(h) && h != ""){
    h <- as.numeric(h)
  } else {
    # determine number of fl_data points in period until maximum value
    n.spl <- length(x.inc)
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
  control$lin.h <- h

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
    # add x and fluorescence values as columns in ret
    if (control$log.y.lin == TRUE) {
      ret <- data.frame(ret, x = x[ret[,1]], fl_data = obs$ylog[ret[,1]])
    } else {
      ret <- data.frame(ret, x = x[ret[,1]], fl_data = obs$fl_data[ret[,1]])
    }

    bad <- is.na(ret[,5]) | is.na(ret[,6])
    ret <- ret[!bad,]


    if(x_type == "growth"){
      if(!is.na(min.growth)){
        ret <- ret[which.min(abs(ret$fl_data-min.growth)) : nrow(ret),] # consider only slopes from defined min.growth
      }
      if(!is.na(max.growth)){
        ret <- ret[(ret[,"x"]+(h*x[2]-x[1])) <= max.growth, ] # consider only slopes up to defined max.growth
      }
    } else{
      ret <- ret[which.min(abs(x-t0)):nrow(ret),] # consider only slopes from defined t0
      if(!is.na(tmax)){
        ret <- ret[(ret[,"x"]+(h*x[2]-x[1])) <= tmax, ] # consider only slopes up to defined tmax
      }
    }
    ret <- ret[!is.na(ret[,1]), ]

    if(nrow(ret)<2){
      flFitLinear <- list(x.in = x.in, fl.in = fl_data.in,
                          raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                          filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
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
                            raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                            filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
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
            if(ret.check[index.max,5] >= R2 && abs(ret.check[index.max,6]) <= RSD && !is.na(ret.check[index.max,6]) ){ # prerequisites for suitable mumax candidate: R2 and RSD
              slope.max <- ret.check[index.max,3]
              success <- TRUE
            } else {
              ret.check <- ret.check[-index.max,]
            }
          }
          index.max.ret <- ret.check[which(ret.check[,3]==slope.max),1] # index of maximum slope in fit table
          slope.quota <- quota * slope.max
          if(x_type == "growth"){
            if(exists("min.growth")){
              candidates <- which(ret[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                    ret[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                    abs(ret[, 6]) <= 1.02 * RSD & # RSD criterion for candidates
                                    ret[, 7] >= min.growth  # consider only slopes at densities higher than "min.growth"
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
                y0_data  <- obs$ylog[obs$x>=min.growth][1] # y0 in dataset
              } else {
                y0_data  <- obs$fl_data[obs$x>=min.growth][1] # y0 in dataset
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
                                raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                                filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
                                ID = ID, FUN = grow_exponential, fit = NA, par = c(
                                  y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                                  x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                                  x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
            class(flFitLinear) <- "flFitLinear"
            if(!control$suppress.messages) message(paste0("No linear fit in accordance with the chosen parameters identified with: R2 >= ", R2, ", RSD <= ", RSD, ", t0 = ", t0, ", and min.growth = ", control$min.growth, "."))
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
            if(x_type == "growth"){
              if(exists("min.growth")){
                candidates.ext <- which(ret[, 3] >= slope.quota.ext & # indices of slopes greater than slope.quota
                                          ret[, 5] >= 0.95*R2 & # R2 criterion for candidates
                                          abs(ret[, 6]) <= 1.1 * RSD & # RSD criterion for candidates
                                          ret[, 7] >= min.growth  # consider only slopes at densities higher than "min.growth"
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
            if(any(index.max.ret %in% unlist(candidate_intervals.ext))){
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
              # apply min.growth to list of linear regressions
              if(x_type == "growth"){
                ret.postmin.check <- ret.postmin[ret.postmin[, 7] >= min.growth, ]
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
                    # prerequisites for suitable mumax candidate: R2 and RSD
                    slope.max.postmin <- ret.postmin.check[index.max, 3]
                    success <- TRUE
                  } else {
                    ret.postmin.check <- ret.postmin.check[-index.max,]
                  }
                }
                index.max.ret.postmin <- ret.postmin[which(ret.postmin[, 3] == slope.max.postmin), 1] # index of maximum slope in fit table
                slope.quota <- quota * slope.max.postmin

                if(x_type == "growth"){
                  if(exists("min.growth")){
                    candidates.postmin <- ret.postmin[which(
                      ret.postmin[, 3] >= slope.quota &
                        ret.postmin[, 5] >= 0.98 * R2 &
                        abs(ret.postmin[, 6]) <= 1.02 * RSD &
                        ret.postmin[, 7] >= min.growth), 1]
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
                if (any(index.max.ret.postmin %in% unlist(candidate_intervals.postmin))) {
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

                  ## estimate lag phase between first and second phase
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
                ret.premin <- ret.premin[ret.premin[, "slope"] > 0 & ret.premin[, "slope"] >= 0.1*max_slope, ]
                #remove regressions included in the extended candidate list
                ret.premin <- ret.premin[!(ret.premin[, 1] %in% tp.ext), ]
                ## Determine index of window with maximum slope, iterate until regression is found that meets R2 and RSD criterion
                success <- FALSE
                # apply min.growth to list of linear regressions
                if(x_type == "growth"){
                  ret.premin.check <- ret.premin[ret.premin[, 8] >= min.growth, ]
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
                      # prerequisites for suitable mumax candidate: R2 and RSD
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
                  if (any(index.max.ret.premin %in% unlist(candidate_intervals.premin))) {
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

                    ## estimate lag phase between first and second phase
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

            # Choose regression before or after max_slope as second phase based on second slope
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
          flFitLinear <- list(x.in = get(ifelse(x_type == "growth", "growth.in", "time.in")), fl.in = fl_data.in,
                              raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                              filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
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
    message("Not enough observations in the dataset to perform linear fit. flFitLinear requires at least 3 fl_data points within the given t0 and min.growth thresholds.")
    flFitLinear <- list(x.in = get(ifelse(x_type == "growth", "growth.in", "time.in")), fl.in = fl_data.in,
                        raw.x = get(ifelse(x_type == "growth", "growth.in", "time.in")), raw.fl = fl_data.in,
                        filt.x = get(ifelse(x_type == "growth", "growth", "time")), filt.fl = fl_data,
                        ID = ID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, dY= NA, A = NA, y0_lm = NA, max_slope = 0, tD = NA, slope.se = NA, lag = NA, x.max_start = NA, x.max_end = NA,
                          x.turn = NA, max_slope2 = NA, tD2 = NA, y0_lm2 = NA, lag2 = NA, x.max2_start = NA,
                          x.max2_end = NA), ndx = NA, ndx.in = NA, ndx2 = NA, ndx2.in = NA, quota = quota, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
    class(flFitLinear) <- "flFitLinear"
    return(flFitLinear)
  }



  flFitLinear <- list(
    x.in = get(ifelse(x_type == "growth", "growth.in", "time.in")),
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


#' @param m linear model (\code{lm}) object
#'
#' @rdname lm_window
#' @export lm_parms
#'
lm_parms <- function (m)
{
  suppressWarnings(sm <- summary(m))
  if(dim(sm$coefficients)[1] >1 ){
    a <- sm$coefficients[1, 1]
    b <- sm$coefficients[2, 1]
    b.se <- sm$coefficients[2, 2]
    r2 <- sm$r.squared
    c(a = a, b = b, b.se = b.se, r2 = r2, b.rsd = b.se/b)
  }
  else {
    c(a = NA, b = NA, b.se = NA, r2 = NA, b.rsd = NA)
  }
}

#' Helper functions for handling linear fits.
#'
#' \code{lm_window} performs a linear regression with the Theil-Sen estimator on a subset of data.
#'
#' @param x vector of independent variable (e.g. time).
#' @param y vector of dependent variable (concentration of organisms).
#' @param i0 index of first value used for a window.
#' @param h with of the window (number of data).
#'
#' @return linear model object of class \code{lm} (lm_window)
#'         resp. vector with parameters of the fit (lm_parms).
#'
#' @references Hall, B. G., H. Acar and M. Barlow 2013. Growth Rates Made Easy.
#'   Mol. Biol. Evol. 31: 232-238 \doi{10.1093/molbev/mst197}
#'
#' @export lm_window
#'
#' @examples
#' # Create random growth dataset
#' rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#'
#' # Extract time and growth data for single sample
#' time <- rnd.dataset$time[1,]
#' data <- as.numeric(rnd.dataset$data[1,-(1:3)]) # Remove identifier columns
#' data.log <- log(data/data[1])
#'
#' # Perform linear fit on 8th window of size 8
#' linreg <- lm_window(time, data.log, 8, h=8)
#'
#' summary(linreg)
#'
#' lm_parms(linreg)
lm_window <- function (x, y, i0, h = 5)
{
  x <- x[i0 - 1 + (1:h)]
  y <- y[i0 - 1 + (1:h)]
  m <- theil_sen_regression(y ~ x)
  return(m)
}

#' Compute Theil Sen Regression
#'
#' From the 'RobustLinearReg' package
#'
#' @param formula A formula in the form \code{y ~ x}.
#' @param data optional: A dataframe in which numeric values for \code{x} and \code{y} are stored
#'
#' @return A model object of class \code{lm}.
#'
#' @references Henri Theil and Pranab K. Sen, 1950 and 1968 respectively
#'
#' @author Santiago I. Hurtado
#'
#' @keywords internal
#' @noRd
#'
theil_sen_regression <- function (formula, data = NULL)
{
  aux_data <- is.null(data)
  if (aux_data) {
    data <- environment(formula)
  }
  term <- as.character(attr(terms(formula), "variables")[-1])
  if (length(term) > 2) {
    stop("only linear models alow")
  }
  y <- as.vector(data[[term[1]]])
  x <- as.vector(data[[term[2]]])
  n <- length(y)
  if (n != length(x)) {
    stop("x and y must be of same length")
  }
  y1 <- matrix(y, n, n, byrow = TRUE)
  y2 <- t(y1)
  x1 <- matrix(x, n, n, byrow = TRUE)
  x2 <- t(x1)
  aux <- (y1 - y2)/(x1 - x2)
  a <- median(aux, na.rm = TRUE)
  aux <- y - a * x
  b <- median(aux, na.rm = TRUE)
  output = list()
  output$coefficients <- c(b, a)
  names(output$coefficients)[1:2] <- c("(Intercept)", term[[2]])
  output$residuals <- y - a * x - b
  output$fitted.values <- x * a + b
  output$df.residual <- n - 2
  output$rank <- 2
  output$terms <- terms(formula)
  output$call <- match.call()
  output$model <- data.frame(y, x)
  names(output$model) <- term
  output$assign <- c(0, 1)
  if (aux_data) {
    output$effects <- lm(formula)$effects
    output$qr <- lm(formula)$qr
  }
  else {
    output$effects <- lm(formula, data)$effects
    output$qr <- lm(formula, data)$qr
  }
  output$effects[2] <- sqrt(sum((output$fitted - mean(output$fitted))^2))
  output$xlevels <- list()
  names(output$model) <- term
  attr(output$model, "terms") <- terms(formula)
  class(output) <- c("lm")
  return(output)
}

#' # from the 'RobustLinearReg' package
#' #' Title
#' #'
#' #' @param formula
#' #' @param data
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' siegel_regression <- function (formula, data = NULL)
#' {
#'   aux_data <- is.null(data)
#'   if (aux_data) {
#'     data <- environment(formula)
#'   }
#'   term <- as.character(attr(terms(formula), "variables")[-1])
#'   if (length(term) > 2) {
#'     stop("only linear models alow")
#'   }
#'   y <- as.vector(data[[term[1]]])
#'   x <- as.vector(data[[term[2]]])
#'   n <- length(y)
#'   if (n != length(x)) {
#'     stop("x and y must be of same length")
#'   }
#'   y1 <- matrix(y, n, n, byrow = TRUE)
#'   y2 <- t(y1)
#'   x1 <- matrix(x, n, n, byrow = TRUE)
#'   x2 <- t(x1)
#'   aux <- (y1 - y2)/(x1 - x2)
#'   a <- median(apply(aux, 1, median, na.rm = TRUE))
#'   aux <- (x1 * y2 - x2 * y1)/(x1 - x2)
#'   b <- median(apply(aux, 1, median, na.rm = TRUE))
#'   output = list()
#'   output$coefficients <- c(b, a)
#'   names(output$coefficients)[1:2] <- c("(Intercept)", term[[2]])
#'   output$residuals <- y - a * x - b
#'   output$fitted.values <- x * a + b
#'   output$df.residual <- n - 2
#'   output$rank <- 2
#'   output$terms <- terms(formula)
#'   output$call <- match.call()
#'   output$model <- data.frame(y, x)
#'   names(output$model) <- term
#'   output$assign <- c(0, 1)
#'   if (aux_data) {
#'     output$effects <- lm(formula)$effects
#'     output$qr <- lm(formula)$qr
#'   }
#'   else {
#'     output$effects <- lm(formula, data)$effects
#'     output$qr <- lm(formula, data)$qr
#'   }
#'   output$effects[2] <- sqrt(sum((output$fitted - mean(output$fitted))^2))
#'   output$xlevels <- list()
#'   names(output$model) <- term
#'   attr(output$model, "terms") <- terms(formula)
#'   class(output) <- c("lm")
#'   return(output)
#' }
