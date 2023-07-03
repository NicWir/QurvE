#' Create a \code{grofit.control} object.
#'
#' A \code{grofit.control} object is required to perform various computations on growth data stored within \code{grodata} objects (created with \code{\link{read_data}} or \code{\link{parse_data}}). A \code{grofit.control} object is created automatically as part of \code{\link{growth.workflow}}.
#'
#' @param neg.nan.act (Logical) Indicates whether the program should stop when negative growth values or NA values appear (\code{TRUE}). Otherwise, the program removes these values silently (\code{FALSE}). Improper values may be caused by incorrect data or input errors. Default: \code{FALSE}.
#' @param clean.bootstrap (Logical) Determines if negative values which occur during bootstrap should be removed (TRUE) or kept (FALSE). Note: Infinite values are always removed. Default: TRUE.
#' @param suppress.messages (Logical) Indicates whether messages (information about current growth curve, EC50 values etc.) should be displayed (\code{FALSE}) or not (\code{TRUE}). This option is meant to speed up the processing of high throughput data. Note: warnings are still displayed. Default: \code{FALSE}.
#' @param fit.opt (Character or character vector) Indicates whether the program should perform a linear regression (\code{'l'}), model fit (\code{'m'}), spline fit (\code{'s'}), or all (\code{'a'}). Combinations can be freely chosen by providing a character vector, e.g. \code{fit.opt = c('l', 's')} Default:  \code{fit.opt = c('l', 's')}.
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param tmax (Numeric) Maximum time value considered for linear and spline fits.
#' @param min.growth (Numeric) Indicate whether only growth values above a certain threshold should be considered for linear regressions or spline fits.
#' @param max.growth (Numeric) Indicate whether only growth values below a certain threshold should be considered for linear regressions or spline fits.
#' @param log.x.gc (Logical) Indicates whether _ln(x+1)_ should be applied to the time data for _linear_ and _spline_ fits. Default: \code{FALSE}.
#' @param log.y.lin (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ fits. Default: \code{TRUE}
#' @param log.y.spline (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _spline_ fits. Default: \code{TRUE}
#' @param log.y.model (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _model_ fits. Default: \code{TRUE}
#' @param biphasic (Logical) Shall \code{\link{growth.gcFitLinear}} and \code{\link{growth.gcFitSpline}} try to extract growth parameters for two different growth phases (as observed with, e.g., diauxic shifts) (\code{TRUE}) or not (\code{FALSE})?
#' @param lin.h (Numeric) Manually define the size of the sliding window used in \code{\link{growth.gcFitLinear}} If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{\link{growth.gcFitLinear}}
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for the calculated slope in \code{\link{growth.gcFitLinear}}
#' @param lin.dY (Numeric) Threshold for the minimum fraction of growth increase a linear regression window should cover. Default: 0.05 (5%).
#' @param interactive (Logical) Controls whether the fit of each growth curve and method is controlled manually by the user. If \code{TRUE}, each fit is visualized in the _Plots_ pane and the user can adjust fitting parameters and confirm the reliability of each fit per sample. Default: \code{TRUE}.
#' @param nboot.gc (Numeric) Number of bootstrap samples used for nonparametric growth curve fitting with \code{\link{growth.gcBootSpline}}. Use \code{nboot.gc = 0} to disable the bootstrap. Default: \code{0}
#' @param smooth.gc (Numeric) Parameter describing the smoothness of the spline fit; usually (not necessary) within (0;1]. \code{smooth.gc=NULL} causes the program to query an optimal value via cross validation techniques. Especially for datasets with few data points the option \code{NULL} might cause a too small smoothing parameter. This can result a too tight fit that is susceptible to measurement errors (thus overestimating growth rates) or produce an error in \code{\link{smooth.spline}} or lead to overfitting. The usage of a fixed value is recommended for reproducible results across samples. See \code{\link{smooth.spline}} for further details. Default: \code{0.55}
#' @param model.type (Character) Vector providing the names of the parametric models which should be fitted to the data. Default: \code{c('logistic', 'richards', 'gompertz', 'gompertz.exp', 'huang', 'baranyi')}.
#' @param dr.method (Character) Define the method used to perform a dose-responde analysis: smooth spline fit (\code{'spline'}) or model fitting (\code{'model'}).
#' @param dr.model (Character) Provide a list of models from the R package 'drc' to include in the dose-response analysis (if \code{dr.method = 'model'}). If more than one model is provided, the best-fitting model will be chosen based on the Akaike Information Criterion.
#' @param dr.have.atleast (Numeric) Minimum number of different values for the response parameter one should have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: \code{6}.
#' @param dr.parameter (Character or numeric) The response parameter in the output table to be used for creating a dose response curve. See \code{\link{growth.drFit}} for further details. Default: \code{'mu.linfit'}, which represents the maximum slope of the linear regression. Typical options include: \code{'mu.linfit'}, \code{'lambda.linfit'}, \code{'dY.linfit'}, \code{'mu.spline'}, \code{'dY.spline'}, \code{'mu.model'}, and \code{'A.model'}.
#' @param smooth.dr (Numeric) Smoothing parameter used in the spline fit by smooth.spline during dose response curve estimation. Usually (not necessesary) in (0; 1]. See \code{\link{smooth.spline}} for further details. Default: \code{NULL}.
#' @param log.x.dr (Logical) Indicates whether \code{ln(x+1)} should be applied to the concentration data of the dose response curves. Default: \code{FALSE}.
#' @param log.y.dr (Logical) Indicates whether \code{ln(y+1)} should be applied to the response data of the dose response curves. Default: \code{FALSE}.
#' @param nboot.dr (Numeric) Defines the number of bootstrap samples for EC50 estimation. Use \code{nboot.dr = 0} to disable bootstrapping. Default: \code{0}.
#' @param growth.thresh (Numeric) Define a threshold for growth. Only if any growth value in a sample is greater than \code{growth.thresh} (default: 1.5) times the start growth, further computations are performed. Else, a message is returned.
#'
#' @return Generates a list with all arguments described above as entries.
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @export
#'
#' @examples
#' # default option
#' control_default <- growth.control()
#' # user defined
#' control_manual <- growth.control(fit.opt = c('s', 'm'),
#'                                  smooth.gc = 0.5,
#'                                  model.type = c('huang', 'baranyi'))
growth.control <- function(
    neg.nan.act = FALSE, clean.bootstrap = TRUE, suppress.messages = FALSE,
    fit.opt = c("a"),
    t0 = 0, tmax = NA, min.growth = NA, max.growth = NA,
    log.x.gc = FALSE, log.y.lin = TRUE, log.y.spline = TRUE,
    log.y.model = TRUE, lin.h = NULL, lin.R2 = 0.97,
    lin.RSD = 0.1, lin.dY = 0.05, biphasic = FALSE,
    interactive = FALSE, nboot.gc = 0, smooth.gc = 0.55,
    model.type = c(
        "logistic", "richards", "gompertz", "gompertz.exp",
        "huang", "baranyi"
    ),
    dr.method = c("model", "spline"),
    dr.model = c(
        "gammadr", "multi2", "LL.2", "LL.3", "LL.4",
        "LL.5", "W1.2", "W1.3", "W1.4", "W2.2", "W2.3",
        "W2.4", "LL.3u", "LL2.2", "LL2.3", "LL2.3u",
        "LL2.4", "LL2.5", "AR.2", "AR.3", "MM.2"
    ),
    dr.have.atleast = 6, dr.parameter = c(
        "mu.linfit", "lambda.linfit", "dY.linfit",
        "A.linfit", "mu.spline", "lambda.spline", "dY.spline",
        "A.spline", "mu.model", "lambda.model", "dY.orig.model",
        "A.orig.model"
    ),
    smooth.dr = NULL, log.x.dr = FALSE, log.y.dr = FALSE,
    nboot.dr = 0, growth.thresh = 1.5
)
    {
    dr.parameter <- match.arg(dr.parameter)
    if (length(dr.method) !=
        1 || dr.method != "model.MM")
        dr.method <- match.arg(dr.method)
    if (!is.null(lin.h) &&
        (lin.h == "" || lin.h == "NULL" || lin.h ==
            0))
        lin.h <- NULL
    if (nboot.gc == "" || is.null(nboot.gc))
        nboot.gc <- 0
    if ((is.character(fit.opt) ==
        FALSE) | !any(fit.opt %in% c("l", "s", "m", "a")))
        stop(
            "value of fit.opt must be character and contain one or more of 'l', 's', or 'm', or be 'a' (for all)."
        )
    if (is.character(model.type) ==
        FALSE)
        stop("value of model.type must be character")
    if (is.character(dr.model) ==
        FALSE)
        stop("value of dr.model must be character")
    if ((is.logical(neg.nan.act) ==
        FALSE) | (length(neg.nan.act) !=
        1))
        stop(
            "value of neg.nan.act must be logical and of one element"
        )
    if ((is.logical(clean.bootstrap) ==
        FALSE) | (length(clean.bootstrap) !=
        1))
        stop(
            "value of clean.bootstrap must be logical and of one element"
        )
    if ((is.logical(suppress.messages) ==
        FALSE) | (length(suppress.messages) !=
        1))
        stop(
            "value of suppress.messages must be logical and of one element"
        )
    if ((is.logical(log.x.gc) ==
        FALSE) | (length(log.x.gc) !=
        1))
        stop(
            "value of log.x.gc must be logical and of one element"
        )
    if ((is.logical(log.y.lin) ==
        FALSE) | (length(log.y.lin) !=
        1))
        stop(
            "value of log.y.lin must be logical and of one element"
        )
    if ((is.logical(log.y.spline) ==
        FALSE) | (length(log.y.spline) !=
        1))
        stop(
            "value of log.y.spline must be logical and of one element"
        )
    if ((is.logical(interactive) ==
        FALSE) | (length(interactive) !=
        1))
        stop(
            "value of interactive must be logical and of one element"
        )
    if ((is.logical(log.x.dr) ==
        FALSE) | (length(log.x.dr) !=
        1))
        stop(
            "value of log.x.dr must be logical and of one element"
        )
    if ((is.logical(log.y.dr) ==
        FALSE) | (length(log.y.dr) !=
        1))
        stop(
            "value of log.y.dr must be logical and of one element"
        )
    if ((is.numeric(nboot.gc) ==
        FALSE) | (length(nboot.gc) !=
        1) | (nboot.gc < 0))
        stop(
            "value of nboot.gc must be numeric (>=0) and of one element"
        )
    if ((is.numeric(dr.have.atleast) ==
        FALSE) | (length(dr.have.atleast) !=
        1) | (dr.have.atleast < 6))
        stop(
            "value of dr.have.atleast must be numeric (>=6) and of one element"
        )
    if (((is.character(dr.parameter) ==
        FALSE) && (is.numeric(dr.parameter) ==
        FALSE)) | (length(dr.parameter) !=
        1))
        stop(
            "value of dr.parameter must be a string or numeric and of one element"
        )
    if ((is.character(dr.method) ==
        FALSE) | (length(dr.method) !=
        1))
        stop(
            "value of dr.method must be a string and of one element"
        )
    if ((is.numeric(nboot.dr) ==
        FALSE) | (length(nboot.dr) !=
        1) | (nboot.dr < 0))
        stop(
            "value of nboot.dr must be numeric (>=0) and of one element"
        )
    if (((is.numeric(smooth.gc) ==
        FALSE)))
        stop("value of smooth.gc must be numeric")
    if (((is.numeric(smooth.dr) ==
        FALSE) && (is.null(smooth.dr) ==
        FALSE)))
        stop("value of smooth.dr must be numeric or NULL")
    if ((is.logical(biphasic) ==
        FALSE) | (length(biphasic) !=
        1))
        stop(
            "value of biphasic must be logical and of one element"
        )
    if ((is.numeric(lin.dY) ==
        FALSE) | (length(lin.dY) !=
        1) | (lin.dY < 0))
        stop(
            "value of lin.dY must be numeric (>=0) and of one element"
        )
    if (((is.numeric(lin.R2) ==
        FALSE) | (length(lin.R2) !=
        1) | !(0 < lin.R2 && lin.R2 < 1)))
        stop(
            "value of lin.R2 must be numeric (0 < lin.R2 < 1) and of one element"
        )
    if (((is.numeric(lin.RSD) ==
        FALSE) | (length(lin.RSD) !=
        1) | !(0 < lin.RSD)))
        stop(
            "value of lin.RSD must be numeric (0 < lin.RSD) and of one element"
        )
    if (((is.numeric(lin.h) ==
        FALSE) && (is.null(lin.h) ==
        FALSE)))
        stop(
            "value of lin.h must be numeric (> 0) and of one element, or NULL"
        )
    if (((is.numeric(growth.thresh) ==
        FALSE) && (is.na(growth.thresh) ==
        FALSE)))
        stop(
            "value of growth.thresh must be numeric (one element) or NA"
        )
    if ((is.numeric(t0) ==
        FALSE) | (length(t0) !=
        1) | (t0 < 0))
        stop(
            "value of t0 must be numeric (>=0) and of one element"
        )

    dr.parameters.opt <- c(
        "TestId", "AddId", "concentration", "reliability_tag",
        "used.model", "log.x", "log.y", "nboot.gc",
        "mu.linfit", "lambda.linfit", "stdmu.linfit",
        "dY.linfit", "A.linfit", "tmu.start.linfit",
        "tmu.end.linfit", "r2mu.linfit", "reliable_fit.linfit",
        "mu.model", "lambda.model", "A.model", "integral.model",
        "parameter_nu.model", "parameter_alpha.model",
        "parameter_t_shift.model", "stdmu.model", "stdlambda.model",
        "stdA.model", "reliable_fit.model", "ci90.mu.model.lo",
        "ci90.mu.model.up", "ci90.lambda.model.lo",
        "ci90.lambda.model.up", "ci90.A.model.lo",
        "ci90.A.model.up", "ci95.mu.model.lo", "ci95.mu.model.up",
        "ci95.lambda.model.lo", "ci95.lambda.model.up",
        "ci95.A.model.lo", "ci95.A.model.up", "mu.spline",
        "lambda.spline", "y0.spline", "A.spline", "dY.spline",
        "integral.spline", "reliable_fit.spline", "smooth.spline",
        "mu.bt", "lambda.bt", "A.bt", "integral.bt",
        "stdmu.bt", "stdlambda.bt", "stdA.bt", "stdintegral.bt",
        "reliable_fit.bt", "ci90.mu.bt.lo", "ci90.mu.bt.up",
        "ci90.lambda.bt.lo", "ci90.lambda.bt.up", "ci90.A.bt.lo",
        "ci90.A.bt.up", "ci90.integral.bt.lo", "ci90.integral.bt.up",
        "ci95.mu.bt.lo", "ci95.mu.bt.up", "ci95.lambda.bt.lo",
        "ci95.lambda.bt.up", "ci95.A.bt.lo", "ci95.A.bt.up",
        "ci95.integral.bt.lo", "ci95.integral.bt.up"
    )
    parameter.in <- dr.parameter
    if (is.character(dr.parameter))
        {
        if (!any(dr.parameters.opt %in% parameter.in))
            {
            stop(
                paste0(
                  parameter.in, " is not a valid parameter for the dose-response analysis. See ?growth.drFit for possible options"
              )
            )
        }
    }
    grofit.control <- list(
        neg.nan.act = neg.nan.act, clean.bootstrap = clean.bootstrap,
        suppress.messages = suppress.messages, fit.opt = fit.opt,
        t0 = t0, tmax = tmax, min.growth = min.growth,
        max.growth = max.growth, log.x.gc = log.x.gc,
        log.y.lin = log.y.lin, log.y.spline = log.y.spline,
        log.y.model = log.y.model, biphasic = biphasic,
        lin.h = lin.h, lin.R2 = lin.R2, lin.RSD = lin.RSD,
        lin.dY = lin.dY, interactive = interactive,
        nboot.gc = round(nboot.gc),
        smooth.gc = smooth.gc, smooth.dr = smooth.dr,
        dr.method = dr.method, dr.model = dr.model,
        dr.have.atleast = round(dr.have.atleast),
        dr.parameter = dr.parameter, log.x.dr = log.x.dr,
        log.y.dr = log.y.dr, nboot.dr = round(nboot.dr),
        model.type = model.type, growth.thresh = growth.thresh
    )
    class(grofit.control) <- "grofit.control"
    invisible(grofit.control)
}

#' Create a \code{fl.control} object.
#'
#' A \code{fl.control} object is required to perform various computations on fluorescence data stored within \code{grodata} objects (created with \code{\link{read_data}} or \code{\link{parse_data}}). A \code{fl.control} object is created automatically as part of \code{\link{fl.workflow}}.
#'
#' @param x_type (Character) Which data type shall be used as independent variable? Options are \code{'growth'} and \code{'time'}.
#' @param fit.opt (Character or vector of strings) Indicates whether the program should perform a linear regression (\code{'l'}) and/or spline fit (\code{'s'}). Default:  \code{fit.opt = c('l', 's')}.
#' @param norm_fl (Logical) use normalized (to growth) fluorescence data in fits. Has an effect only when \code{x_type = 'time'}
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits (if \code{x_type = 'time'}).
#' @param tmax (Numeric) Maximum time value considered for linear and spline fits (if \code{x_type = 'time'})..
#' @param min.growth (Numeric) Indicate whether only values above a certain threshold should be considered for linear regressions or spline fits (if \code{x_type = 'growth'}).
#' @param max.growth (Numeric) Indicate whether only growth values below a certain threshold should be considered for linear regressions or spline fits (if \code{x_type = 'growth'}).
#' @param log.x.lin (Logical) Indicates whether _ln(x+1)_ should be applied to the independent variable for _linear_ fits. Default: \code{FALSE}.
#' @param log.x.spline (Logical) Indicates whether _ln(x+1)_ should be applied to the independent variable for _spline_ fits. Default: \code{FALSE}.
#' @param log.y.lin (Logical) Indicates whether _ln(y/y0)_ should be applied to the fluorescence data for _linear_ fits. Default: \code{FALSE}
#' @param log.y.spline (Logical) Indicates whether _ln(y/y0)_ should be applied to the fluorescence data for _spline_ fits. Default: \code{FALSE}
#' @param lin.h (Numeric) Manually define the size of the sliding window used in \code{\link{flFitLinear}}. If \code{NULL}, h is calculated for each samples based on the number of measurements in the fluorescence increase phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{\link{flFitLinear}}.
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for the calculated slope in \code{\link{flFitLinear}}.
#' @param lin.dY (Numeric) Threshold for the minimum fraction of growth increase a linear regression window should cover. Default: 0.05 (5%).
#' @param dr.parameter (Character or numeric) The response parameter in the output table to be used for creating a dose response curve. See \code{\link{fl.drFit}} for further details. Default: \code{'max_slope.spline'}, which represents the maximum slope of the spline fit Typical options include: \code{'max_slope.linfit'}, \code{'dY.linfit'}, \code{'max_slope.spline'}, and \code{'dY.spline'}.
#' @param dr.method (Character) Perform either a smooth spline fit on response parameter vs. concentration data (\code{'spline'}) or fit a biosensor response model with \code{'model'} (proposed by Meyer et al., 2019).
#' @param dr.have.atleast (Numeric) Minimum number of different values for the response parameter one should have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: \code{6}.
#' @param smooth.dr (Numeric) Smoothing parameter used in the spline fit by smooth.spline during dose response curve estimation. Usually (not necessesary) in (0; 1]. See \code{\link{smooth.spline}} for further details. Default: \code{NULL}.
#' @param log.x.dr (Logical) Indicates whether \code{ln(x+1)} should be applied to the concentration data of the dose response curves. Default: \code{FALSE}.
#' @param log.y.dr (Logical) Indicates whether \code{ln(y+1)} should be applied to the response data of the dose response curves. Default: \code{FALSE}.
#' @param nboot.dr (Numeric) Defines the number of bootstrap samples for EC50 estimation. Use \code{nboot.dr = 0} to disable bootstrapping. Default: \code{0}.
#' @param biphasic (Logical) Shall \code{\link{flFitLinear}} and \code{\link{flFitSpline}} try to extract fluorescence parameters for two different phases (as observed with, e.g., regulator-promoter systems with varying response in different growth stages) (\code{TRUE}) or not (\code{FALSE})?
#' @param interactive (Logical) Controls whether the fit for each sample and method is controlled manually by the user. If \code{TRUE}, each fit is visualized in the _Plots_ pane and the user can adjust fitting parameters and confirm the reliability of each fit per sample. Default: \code{TRUE}.
#' @param nboot.fl (Numeric) Number of bootstrap samples used for nonparametric curve fitting with \code{\link{flBootSpline}}. Use \code{nboot.fl = 0} to disable the bootstrap. Default: \code{0}
#' @param smooth.fl (Numeric) Parameter describing the smoothness of the spline fit; usually (not necessary) within (0;1]. \code{smooth.gc=NULL} causes the program to query an optimal value via cross validation techniques. Especially for datasets with few data points the option \code{NULL} might cause a too small smoothing parameter. This can result a too tight fit that is susceptible to measurement errors (thus overestimating slopes) or produce an error in \code{\link{smooth.spline}} or lead to overfitting. The usage of a fixed value is recommended for reproducible results across samples. See \code{\link{smooth.spline}} for further details. Default: \code{0.55}
#' @param growth.thresh (Numeric) Define a threshold for growth. Only if any growth value in a sample is greater than \code{growth.thresh} (default: 1.5) times the start growth, further computations are performed. Else, a message is returned.
#' @param suppress.messages (Logical) Indicates whether messages (information about current fluorescence curve, EC50 values etc.) should be displayed (\code{FALSE}) or not (\code{TRUE}). This option is meant to speed up the high-throughput processing data. Note: warnings are still displayed. Default: \code{FALSE}.
#' @param neg.nan.act (Logical) Indicates whether the program should stop when negative fluorescence values or NA values appear (\code{TRUE}). Otherwise, the program removes these values silently (\code{FALSE}). Improper values may be caused by incorrect data or input errors. Default: \code{FALSE}.
#' @param clean.bootstrap (Logical) Determines if negative values which occur during bootstrap should be removed (\code{TRUE}) or kept (\code{FALSE}). Note: Infinite values are always removed. Default: \code{TRUE}.
#'
#' @return Generates a list with all arguments described above as entries.
#'
#' @references Meyer, A.J., Segall-Shapiro, T.H., Glassey, E. et al. _Escherichia coli “Marionette” strains with 12 highly optimized small-molecule sensors._ Nat Chem Biol 15, 196–204 (2019). DOI: 10.1038/s41589-018-0168-3
#'
#' @export
#'
#' @examples
#' # default option
#' control_default <- fl.control()
#' # user defined
#' control_manual <- fl.control(fit.opt = c('s'),
#'                              smooth.fl = 0.6,
#'                              x_type = 'time',
#'                              t0 = 2)
fl.control <- function(
    fit.opt = c("l", "s"),
    x_type = c("growth", "time"),
    norm_fl = TRUE, t0 = 0, tmax = NA, min.growth = NA,
    max.growth = NA, log.x.lin = FALSE, log.x.spline = FALSE,
    log.y.lin = FALSE, log.y.spline = FALSE, lin.h = NULL,
    lin.R2 = 0.97, lin.RSD = 0.05, lin.dY = 0.05, dr.parameter = "max_slope.spline",
    dr.method = c("model", "spline"),
    dr.have.atleast = 5, smooth.dr = NULL, log.x.dr = FALSE,
    log.y.dr = FALSE, nboot.dr = 0, biphasic = FALSE,
    interactive = FALSE, nboot.fl = 0, smooth.fl = 0.75,
    growth.thresh = 1.5, suppress.messages = FALSE,
    neg.nan.act = FALSE, clean.bootstrap = TRUE
)
    {
    if (!is.null(lin.h) &&
        (lin.h == "" || lin.h == "NULL" || lin.h ==
            0))
        lin.h <- NULL
    x_type <- match.arg(x_type)
    dr.method <- match.arg(dr.method)
    if (is.null(nboot.fl) ||
        nboot.fl == "")
        nboot.fl <- 0
    if ((is.character(fit.opt) ==
        FALSE | !any(fit.opt %in% c("l", "s"))))
        stop(
            "value of fit.opt must be character and contain one of or both 'l' and 's'."
        )
    if ((is.character(x_type) ==
        FALSE | !any(x_type %in% c("growth", "time"))))
        stop(
            "value of x_type must be character and contain one of 'growth' or 'time'."
        )
    if ((is.character(dr.method) ==
        FALSE | !any(dr.method %in% c("model", "spline"))))
        stop(
            "value of dr.method must be character and contain one of 'model' or 'spline'."
        )
    if ((is.logical(suppress.messages) ==
        FALSE) | (length(suppress.messages) !=
        1))
        stop(
            "value of suppress.messages must be logical and of one element"
        )
    if ((is.logical(log.x.lin) ==
        FALSE) | (length(log.x.lin) !=
        1))
        stop(
            "value of log.x.lin must be logical and of one element"
        )
    if ((is.logical(log.x.spline) ==
        FALSE) | (length(log.x.spline) !=
        1))
        stop(
            "value of log.x.spline must be logical and of one element"
        )
    if ((is.logical(log.y.lin) ==
        FALSE) | (length(log.y.lin) !=
        1))
        stop(
            "value of log.y.spline must be logical and of one element"
        )
    if ((is.logical(log.y.spline) ==
        FALSE) | (length(log.y.spline) !=
        1))
        stop(
            "value of log.y.spline must be logical and of one element"
        )
    if ((is.logical(interactive) ==
        FALSE) | (length(interactive) !=
        1))
        stop(
            "value of interactive must be logical and of one element"
        )
    if ((is.numeric(nboot.fl) ==
        FALSE) | (length(nboot.fl) !=
        1) | (nboot.fl < 0))
        stop(
            "value of nboot.fl must be numeric (>=0) and of one element"
        )
    if ((is.numeric(lin.dY) ==
        FALSE) | (length(lin.dY) !=
        1) | (lin.dY < 0))
        stop(
            "value of lin.dY must be numeric (>=0) and of one element"
        )
    if (((is.numeric(smooth.fl) ==
        FALSE)))
        stop("value of smooth.fl must be numeric")
    if ((is.numeric(dr.have.atleast) ==
        FALSE) | (length(dr.have.atleast) !=
        1) | (dr.have.atleast < 5))
        stop(
            "value of dr.have.atleast must be numeric (>=5) and of one element"
        )
    if (((is.numeric(lin.R2) ==
        FALSE) | (length(lin.R2) !=
        1) | !(0 < lin.R2 && lin.R2 < 1)))
        stop(
            "value of lin.R2 must be numeric (0 < lin.R2 < 1) and of one element"
        )
    if (((is.numeric(lin.RSD) ==
        FALSE) | (length(lin.RSD) !=
        1) | !(0 < lin.RSD)))
        stop(
            "value of lin.RSD must be numeric (0 < lin.RSD) and of one element"
        )
    if (((is.numeric(lin.h) ==
        FALSE) && (is.null(lin.h) ==
        FALSE)))
        stop(
            "value of lin.h must be numeric (> 0) and of one element"
        )
    if (((is.numeric(growth.thresh) ==
        FALSE) && (is.na(growth.thresh) ==
        FALSE)))
        stop(
            "value of growth.thresh must be numeric (one element) or NA"
        )
    if ((is.logical(biphasic) ==
        FALSE) | (length(biphasic) !=
        1))
        stop(
            "value of biphasic must be logical and of one element"
        )
    if ((is.numeric(t0) ==
        FALSE) | (length(t0) !=
        1) | (t0 < 0))
        stop(
            "value of t0 must be numeric (>=0) and of one element"
        )

    dr.parameters.opt <- c(
        "TestId", "AddId", "concentration", "reliability_tag",
        "log.x", "log.y", "nboot.gc", "max_slope.linfit",
        "lambda.linfit", "stdmax_slope.linfit", "dY.linfit",
        "A.linfit", "tmax_slope.start.linfit", "tmax_slope.end.linfit",
        "r2max_slope.linfit", "reliable_fit.linfit",
        "lambda.model", "integral.model", "max_slope.spline",
        "lambda.spline", "y0.spline", "A.spline", "dY.spline",
        "integral.spline", "reliable_fit.spline", "smooth.spline",
        "max_slope.bt", "lambda.bt", "A.bt", "integral.bt",
        "stdmax_slope.bt", "stdlambda.bt", "stdA.bt",
        "stdintegral.bt", "reliable_fit.bt", "ci90.max_slope.bt.lo",
        "ci90.max_slope.bt.up", "ci90.lambda.bt.lo",
        "ci90.lambda.bt.up", "ci90.A.bt.lo", "ci90.A.bt.up",
        "ci90.integral.bt.lo", "ci90.integral.bt.up",
        "ci95.max_slope.bt.lo", "ci95.max_slope.bt.up",
        "ci95.lambda.bt.lo", "ci95.lambda.bt.up", "ci95.A.bt.lo",
        "ci95.A.bt.up", "ci95.integral.bt.lo", "ci95.integral.bt.up"
    )
    parameter.in <- dr.parameter
    if (is.character(dr.parameter))
        {
        if (!any(dr.parameters.opt %in% parameter.in))
            {
            stop(
                paste0(
                  parameter.in, " is not a valid parameter for the dose-response analysis. See ?fl.drFit for possible options"
              )
            )
        }
    }

    fl.control <- list(
        fit.opt = fit.opt, x_type = x_type, norm_fl = norm_fl,
        t0 = t0, tmax = tmax, min.growth = min.growth,
        max.growth = max.growth, min.growth = min.growth,
        log.x.lin = log.x.lin, log.x.spline = log.x.spline,
        log.y.lin = log.y.lin, log.y.spline = log.y.spline,
        lin.h = lin.h, lin.R2 = lin.R2, lin.RSD = lin.RSD,
        lin.dY = lin.dY, biphasic = biphasic, dr.parameter = dr.parameter,
        dr.method = dr.method, dr.have.atleast = dr.have.atleast,
        smooth.dr = smooth.dr, log.x.dr = log.x.dr,
        log.y.dr = log.y.dr, nboot.dr = nboot.dr, interactive = interactive,
        nboot.fl = nboot.fl, smooth.fl = smooth.fl,
        growth.thresh = growth.thresh, suppress.messages = suppress.messages,
        neg.nan.act = neg.nan.act, clean.bootstrap = clean.bootstrap
    )
    class(fl.control) <- "fl.control"
    invisible(fl.control)
}
