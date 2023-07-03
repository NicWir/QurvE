#' Generic summary function for flFitSpline objects
#'
#' @param object object of class \code{flFitSpline}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters extracted from a nonparametric fit.
#'
#' @export
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file('lac_promoters.xlsx', package = 'QurvE'),
#'                    data.fl = system.file('lac_promoters.xlsx', package = 'QurvE'),
#'                    sheet.growth = 1,
#'                    sheet.fl = 2 )
#'
#' # Extract time and normalized fluorescence data for single sample
#' time <- input$time[4,]
#' data <- input$norm.fluorescence[4,-(1:3)] # Remove identifier columns
#'
#' # Perform linear fit
#' TestFit <- flFitSpline(time = time,
#'                        fl_data = data,
#'                        ID = 'TestFit',
#'                        control = fl.control(fit.opt = 's', x_type = 'time'))
#'
#' summary(TestFit)
#
summary.flFitSpline <- function(object, ...)
    {

    # object of class flFitSpline

    contents.fitted.spline <- c(
        "max_slope.spline", "x.max.spline", "lambda.spline",
        "max_slope2.spline", "x.max2.spline", "lambda2.spline",
        "y0.spline", "A.spline", "dY.spline", "integral.spline",
        "reliable_fit.spline", "reliable_fit2.spline",
        "smooth.spline"
    )

    if ((is.na(object$fitFlag) ==
        TRUE) | (object$fitFlag == FALSE))
        {
        table <- c(
            0, rep(
                NA, length(contents.fitted.spline) -
                  4
            ),
            as.character(object$fitFlag),
            as.character(object$fitFlag2),
            ifelse(
                is.null(object$control$smooth.gc),
                "NULL", as.numeric(object$control$smooth.gc)
            )
        )
    } else
    {
        table <- c(
            object$parameters$max_slope, object$parameters$x.max,
            ifelse(
                length(object$parameters$lambda) <
                  1, NA, object$parameters$lambda
            ),
            object$parameters$max_slope2, object$parameters$x.max2,
            object$parameters$lambda2, object$parameters$A -
                object$parameters$dY, object$parameters$A,
            object$parameters$dY, object$parameters$integral,
            as.character(object$fitFlag),
            as.character(object$fitFlag2),
            ifelse(
                is.null(object$control$smooth.fl),
                "NULL", as.numeric(object$control$smooth.fl)
            )
        )
    }

    table <- data.frame(t(table))
    colnames(table) <- contents.fitted.spline
    summary.flFitSpline <- table
    summary.flFitSpline
}

#' Generic summary function for flFitLinear objects
#'
#' @param object object of class \code{flFitLinear}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters extracted from a linear fit.
#'
#' @export
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file('lac_promoters.xlsx', package = 'QurvE'),
#'                    data.fl = system.file('lac_promoters.xlsx', package = 'QurvE'),
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
#'                        ID = 'TestFit',
#'                        control = fl.control(fit.opt = 'l', x_type = 'time',
#'                        lin.R2 = 0.95, lin.RSD = 0.1,
#'                        lin.h = 20))
#'
#' summary(TestFit)
#
summary.flFitLinear <- function(object, ...)
    {
    # object of class flFitLinear

    contents.fitted.param = c(
        "max_slope.linfit", "lambda.linfit", "dY.linfit",
        "A.linfit", "x.mu.start.linfit", "x.mu.end.linfit",
        "r2mu.linfit", "reliable_fit.linfit", "max_slope2.linfit",
        "x.mu2.start.linfit", "x.mu2.end.linfit", "r2mu2.linfit",
        "reliable_fit2.linfit"
    )


    if ((is.na(object$fitFlag) ==
        TRUE) | (object$fitFlag == FALSE))
        {
        table <- c(
            0, rep(NA, 6),
            "FALSE", rep(NA, 4),
            "FALSE"
        )
    } else
    {
        table <- c(
            object$par["max_slope"], ifelse(
                !is.na(object$par["lag2"]),
                ifelse(
                  object$par["lag2"] < object$par["lag"],
                  object$par["lag2"], object$par["lag"]
              ),
                object$par["lag"]
            ),
            object$par["dY"], object$par["A"], object$par["x.max_start"],
            object$par["x.max_end"], object$rsquared,
            as.character(object$fitFlag),
            object$par["max_slope2"], object$par["x.max2_start"],
            object$par["x.max2_end"], object$rsquared2,
            as.character(object$fitFlag2)
        )

    }
    table <- data.frame(t(table))
    colnames(table) <- contents.fitted.param
    summary.flFitLinear <- table
    summary.flFitLinear
}

#' Generic summary function for flBootSpline objects
#'
#' @param object object of class \code{flBootSpline}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with statistical parameters extracted from a dose-response bootstrapping analysis.
#'
#' @export
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file('lac_promoters.xlsx', package = 'QurvE'),
#'                    data.fl = system.file('lac_promoters.xlsx', package = 'QurvE'),
#'                    sheet.growth = 1,
#'                    sheet.fl = 2 )
#'
#' # Extract time and normalized fluorescence data for single sample
#' time <- input$time[4,]
#' data <- input$norm.fluorescence[4,-(1:3)] # Remove identifier columns
#'
#' # Perform linear fit
#' TestFit <- flBootSpline(time = time,
#'                        fl_data = data,
#'                        ID = 'TestFit',
#'                        control = fl.control(fit.opt = 's', x_type = 'time',
#'                        nboot.fl = 50))
#'
#' summary(TestFit)
#
summary.flBootSpline <- function(object, ...)
    {
    # object of class flBootSpline
    contents.bootstrap <- c(
        "max_slope.bt", "lambda.bt", "A.bt", "dY.bt",
        "integral.bt", "stdmax_slope.bt", "stdlambda.bt",
        "stdA.bt", "stddY.bt", "stdintegral.bt", "reliable_fit.bt",
        "ci90.mu.bt.lo", "ci90.mu.bt.up", "ci90.lambda.bt.lo",
        "ci90.lambda.bt.up", "ci90.A.bt.lo", "ci90.A.bt.up",
        "ci90.integral.bt.lo", "ci90.integral.bt.up",
        "ci95.mu.bt.lo", "ci95.mu.bt.up", "ci95.lambda.bt.lo",
        "ci95.lambda.bt.up", "ci95.A.bt.lo", "ci95.A.bt.up",
        "ci95.integral.bt.lo", "ci95.integral.bt.up"
    )


    if (object$bootFlag == FALSE)
    {
        table <- rep(NA, length(contents.bootstrap))
    } else
    {
        mu <- mean(object$max_slope, na.rm = TRUE)
        lambda <- mean(object$lambda, na.rm = TRUE)
        A <- mean(object$A, na.rm = TRUE)
        dY <- mean(object$dY, na.rm = TRUE)
        integral <- mean(object$integral, na.rm = TRUE)

        mu.sd <- sd(object$max_slope, na.rm = TRUE)
        lambda.sd <- sd(object$lambda, na.rm = TRUE)
        A.sd <- sd(object$A, na.rm = TRUE)
        dY.sd <- sd(object$dY, na.rm = TRUE)
        integral.sd <- sd(object$integral, na.rm = TRUE)

        table <- c(
            mu, lambda, A, dY, integral, mu.sd, lambda.sd,
            A.sd, dY.sd, integral.sd, as.character(object$bootFlag),
            mu - 1.645 * mu.sd, mu + 1.645 * mu.sd,
            lambda - 1.645 * lambda.sd, lambda + 1.645 *
                lambda.sd, A - 1.645 * A.sd, A + 1.645 *
                A.sd, integral - 1.645 * integral.sd,
            integral + 1.645 * integral.sd, mu - 1.96 *
                mu.sd, mu + 1.96 * mu.sd, lambda -
                1.96 * lambda.sd, lambda + 1.96 * lambda.sd,
            A - 1.96 * A.sd, A + 1.96 * A.sd, integral -
                1.96 * integral.sd, integral + 1.96 *
                integral.sd
        )
    }

    table <- data.frame(t(table))
    colnames(table) <- contents.bootstrap
    summary.flBootSpline <- table
    summary.flBootSpline
}

#' Generic summary function for flFit objects
#'
#' @param object object of class \code{flFit}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters extracted from all fits of a workflow.
#'
#' @export
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file('lac_promoters.xlsx', package = 'QurvE'),
#'                    data.fl = system.file('lac_promoters.xlsx', package = 'QurvE'),
#'                    sheet.growth = 1,
#'                    sheet.fl = 2 )
#'
#' # Run curve fitting workflow
#' res <- flFit(fl_data = input$norm.fluorescence,
#'              time = input$time,
#'              parallelize = FALSE,
#'              control = fl.control(fit.opt = 's', suppress.messages = TRUE,
#'              x_type = 'time', norm_fl = TRUE, nboot.fl = 20))
#'
#' summary(res)
#'
summary.flFit <- function(object, ...)
    {
    # object of class flFit
    summary.flFit <- data.frame(object$flTable)
    summary.flFit
}

#' Generic summary function for drFitFLModel objects
#'
#' @param object object of class \code{drFitModel}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with biosensor response parameters.
#'
#' @export
#'
#' @examples
#' # Create concentration values via a serial dilution
#' conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
#'
#' # Simulate response values via biosensor equation
#' response <- biosensor.eq(conc, y.min = 110, y.max = 6000, K = 0.5, n = 2) +
#'             0.01*6000*rnorm(10)
#'
#' # Perform fit
#' TestRun <- fl.drFitModel(conc, response, drID = 'test', control = fl.control())
#'
#' print(summary(TestRun))
#'
summary.drFitFLModel <- function(object, ...)
    {
    object$parameters[unlist(
        lapply(
            1:length(object$parameters),
            function(x) is.null(object$parameters[[x]])
        )
    )] <- NA
    # object of class drFitModel
    data.frame(object$parameters)
}

#' Generic summary function for drFitfl objects
#'
#' @param object object of class \code{drFitfl}
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A dataframe with parameters for all samples extracted from a dose-response analysis.
#'
#' @export
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file('lac_promoters.xlsx', package = 'QurvE'),
#'                    data.fl = system.file('lac_promoters.xlsx', package = 'QurvE'),
#'                    sheet.growth = 1,
#'                    sheet.fl = 2 )
#'
#' # Define fit controls
#' control <- fl.control(fit.opt = 's',
#'              x_type = 'time', norm_fl = TRUE,
#'              dr.parameter = 'max_slope.spline',
#'              dr.method = 'model',
#'              suppress.messages = TRUE)
#'
#' # Run curve fitting workflow
#' res <- flFit(fl_data = input$norm.fluorescence,
#'              time = input$time,
#'              parallelize = FALSE,
#'              control = control)
#'
#' # Perform dose-response analysis with biosensor model
#' drFitfl <- fl.drFit(flTable = res$flTable, control = control)
#'
#' summary(drFitfl)
#'
summary.drFitfl <- function(object, ...)
    {
    # object of class drFitModel
    data.frame(object$drTable)
}
