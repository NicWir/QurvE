#' Run a complete growth curve analysis and dose-reponse analysis workflow.
#'
#' \code{growth.workflow} runs \code{\link{growth.control}} to create a \code{grofit.control} object and then performs all computational fitting operations based on the user input. Finally, if desired, a final report is created in PDF or HTML format that summarizes all results obtained.
#'
#' @param grodata A \code{grodata} object created with \code{\link{read_data}} or \code{\link{parse_data}}, or a list containing a \code{'time'} matrix as well as a \code{'growth'} dataframe.
#' @param time (optional) A matrix containing time values for each sample.
#' @param data (optional) A dataframe containing growth data (if a \code{time} matrix is provided as separate argument).
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param tmax (Numeric) Maximum time value considered for linear and spline fits.
#' @param ec50 (Logical) Perform dose-response analysis (\code{TRUE}) or not (\code{FALSE}).
#' @param mean.grp (\code{'all'}, a string vector, or a list of string vectors) Define groups to combine into common plots in the final report based on sample identifiers (if \code{report == TRUE}). Partial matches with sample/group names are accepted. Note: The maximum number of sample groups (with unique condition/concentration indicators) is 50. If you have more than 50 groups, option \code{'all'} will produce the error \code{! Insufficient values in manual scale. [Number] needed but only 50 provided}.
#' @param mean.conc (A numeric vector, or a list of numeric vectors) Define concentrations to combine into common plots in the final report (if \code{report == TRUE}).
#' @param neg.nan.act (Logical) Indicates whether the program should stop when negative growth values or NA values appear (\code{TRUE}). Otherwise, the program removes these values silently (\code{FALSE}). Improper values may be caused by incorrect data or input errors. Default: \code{FALSE}.
#' @param clean.bootstrap (Logical) Determines if negative values which occur during bootstrap should be removed (TRUE) or kept (FALSE). Note: Infinite values are always removed. Default: TRUE.
#' @param suppress.messages (Logical) Indicates whether grofit messages (information about current growth curve, EC50 values etc.) should be displayed (\code{FALSE}) or not (\code{TRUE}). This option is meant to speed up the high-throughput processing data. Note: warnings are still displayed. Default: \code{FALSE}.
#' @param fit.opt (Character or character vector) Indicates whether the program should perform a linear regression (\code{'l'}), model fit (\code{'m'}), spline fit (\code{'s'}), or all (\code{'a'}). Combinations can be freely chosen by providing a character vector, e.g. \code{fit.opt = c('l', 's')} Default:  \code{fit.opt = c('l', 's')}.
#' @param min.growth (Numeric) Indicate whether only growth values above a certain threshold should be considered for linear regressions or spline fits.
#' @param max.growth (Numeric) Indicate whether only growth values below a certain threshold should be considered for linear regressions or spline fits.
#' @param log.x.gc (Logical) Indicates whether _ln(x+1)_ should be applied to the time data for _linear_ and _spline_ fits. Default: \code{FALSE}.
#' @param log.y.lin (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ fits. Default: \code{TRUE}
#' @param log.y.spline (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _spline_ fits. Default: \code{TRUE}
#' @param log.y.model (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _model_ fits. Default: \code{TRUE}
#' @param biphasic (Logical) Shall \code{\link{growth.gcFitLinear}} and \code{\link{growth.gcFitSpline}} try to extract growth parameters for two different growth phases (as observed with, e.g., diauxic shifts) (\code{TRUE}) or not (\code{FALSE})?
#' @param lin.h (Numeric) Manually define the size of the sliding window used in \code{\link{growth.gcFitLinear}} If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{\link{growth.gcFitLinear}}
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for calculated slope in \code{\link{growth.gcFitLinear}}
#' @param lin.dY (Numeric) Threshold for the minimum fraction of growth increase a linear regression window should cover. Default: 0.05 (5%).
#' @param interactive (Logical) Controls whether the fit of each growth curve and method is controlled manually by the user. If \code{TRUE}, each fit is visualized in the _Plots_ pane and the user can adjust fitting parameters and confirm the reliability of each fit per sample. Default: \code{TRUE}.
#' @param nboot.gc (Numeric) Number of bootstrap samples used for nonparametric growth curve fitting with \code{\link{growth.gcBootSpline}}. Use \code{nboot.gc = 0} to disable the bootstrap. Default: \code{0}
#' @param smooth.gc (Numeric) Parameter describing the smoothness of the spline fit; usually (not necessary) within (0;1]. \code{smooth.gc=NULL} causes the program to query an optimal value via cross validation techniques. Especially for datasets with few data points the option NULL might cause a too small smoothing parameter. This can result a too tight fit that is susceptible to measurement errors (thus overestimating growth rates) or produce an error in \code{smooth.spline} or lead to an overestimation. The usage of a fixed value is recommended for reproducible results across samples. See \code{?smooth.spline} for further details. Default: \code{0.55}
#' @param model.type (Character) Vector providing the names of the parametric models which should be fitted to the data. Default: \code{c('logistic', 'richards', 'gompertz', 'gompertz.exp', 'huang', 'baranyi')}.
#' @param dr.method (Character) Define the method used to perform a dose-responde analysis: smooth spline fit (\code{'spline'}) or model fitting (\code{'model'}).
#' @param dr.model (Character) Provide a list of models from the R package 'drc' to include in the dose-response analysis (if \code{dr.method = 'model'}). If more than one model is provided, the best-fitting model will be chosen based on the Akaike Information Criterion.
#' @param growth.thresh (Numeric) Define a threshold for growth. Only if any growth value in a sample is greater than \code{growth.thresh} (default: 1.5) times the start growth, further computations are performed. Else, a message is returned.
#' @param dr.have.atleast (Numeric) Minimum number of different values for the response parameter one should have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: \code{6}.
#' @param dr.parameter (Character or numeric) The response parameter in the output table to be used for creating a dose response curve. See \code{\link{growth.drFit}} for further details. Default: \code{'mu.linfit'}, which represents the maximum slope of the linear regression. Typical options include: \code{'mu.linfit'}, \code{'lambda.linfit'}, \code{'dY.linfit'}, \code{'mu.spline'}, \code{'dY.spline'}, \code{'mu.model'}, and \code{'A.model'}.
#' @param smooth.dr (Numeric) Smoothing parameter used in the spline fit by smooth.spline during dose response curve estimation. Usually (not necessesary) in (0; 1]. See documentation of smooth.spline for further details. Default: \code{NULL}.
#' @param log.x.dr (Logical) Indicates whether \code{ln(x+1)} should be applied to the concentration data of the dose response curves. Default: \code{FALSE}.
#' @param log.y.dr (Logical) Indicates whether \code{ln(y+1)} should be applied to the response data of the dose response curves. Default: \code{FALSE}.
#' @param nboot.dr (Numeric) Defines the number of bootstrap samples for EC50 estimation. Use \code{nboot.dr = 0} to disable bootstrapping. Default: \code{0}.
#' @param report (Character or NULL) Create a PDF (\code{'pdf'}) and/or HTML (\code{'html'}) report after running all computations. Define \code{NULL} if no report should be created. Default: (\code{c('pdf', 'html')})
#' @param out.dir {Character or \code{NULL}} Define the name of a folder in which all result files are stored. If \code{NULL}, the folder will be named with a combination of 'GrowthResults_' and the current date and time.
#' @param out.nm {Character or \code{NULL}} Define the name of the report files. If \code{NULL}, the files will be named with a combination of 'GrowthReport_' and the current date and time.
#' @param export.fig (Logical) Export all figures created in the report as separate PNG and PDF files (\code{TRUE}) or not (\code{FALSE}). Only effective if \code{report = TRUE}.
#' @param export.res (Logical) Create tab-separated TXT files containing calculated growth parameters and dose-response analysis results as well as an .RData file for the resulting `grofit` object.
#' @param parallelize Run linear fits and bootstrapping operations in parallel using all but one available processor cores
#' @param ... Further arguments passed to the shiny app.
#'
#' @family workflows
#' @family growth fitting functions
#' @family dose-response analysis functions
#'
#' @return A \code{grofit} object that contains all computation results, compatible with various plotting functions of the QurvE package and with \code{\link{growth.report}}.
#' \item{time}{Raw time matrix passed to the function as \code{time} (if no \code{grofit} object is provided).}
#' \item{data}{Raw growth dataframe passed to the function as \code{data} (if no \code{grofit} object is provided).}
#' \item{gcFit}{\code{gcFit} object created with the call of \code{\link{growth.gcFit}}.}
#' \item{drFit}{\code{drFit} object created with the call of \code{\link{growth.drFit}}.}
#' \item{expdesign}{Experimental design table inherited from \code{grodata} or created from the identifier columns (columns 1-3) in \code{data}.}
#' \item{control}{Object of class \code{grofit.control} created with the call of \code{\link{growth.control}}.}
#'
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
#'
#' @details
#' Common response parameters used in dose-response analysis:<br><br><b>Linear fit:</b><br>- mu.linfit: Growth rate<br>- lambda.linfit: Lag time<br>- dY.linfit: Density increase<br>- A.linfit: Maximum measurement<br><br><b>Spline fit:</b><br>- mu.spline: Growth rate<br>- lambda.spline: Lag time<br>- A.spline: Maximum measurement<br>- dY.spline: Density increase<br>- integral.spline: Integral<br><br><b>Parametric fit:</b><br>- mu.model: Growth rate<br>- lambda.model: Lag time<br>- A.model: Maximum measurement<br>- integral.model: Integral'
#'
#'
#' @examples
#' # Create random growth data set
#'   rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')
#'   rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = 'Test2')
#'
#'   rnd.data <- list()
#'   rnd.data[['time']] <- rbind(rnd.data1$time, rnd.data2$time)
#'   rnd.data[['data']] <- rbind(rnd.data1$data, rnd.data2$data)
#'
#'   # Run growth curve analysis workflow
#'   res <- growth.workflow(time = rnd.data$time,
#'                          data = rnd.data$data,
#'                          fit.opt = 's',
#'                          ec50 = FALSE,
#'                          export.res = FALSE,
#'                          suppress.messages = TRUE,
#'                          parallelize = FALSE)
#'
#' # Load custom dataset
#'   input <- read_data(data.growth = system.file('2-FMA_toxicity.csv', package = 'QurvE'))
#'
#'   res <- growth.workflow(grodata = input,
#'                          fit.opt = 's',
#'                          ec50 = TRUE,
#'                          export.res = FALSE,
#'                          suppress.messages = TRUE,
#'                          parallelize = FALSE)
#'
#'   plot(res)
#'
growth.workflow <- function(
    grodata = NULL, time = NULL, data = NULL, ec50 = TRUE,
    mean.grp = NA, mean.conc = NA, neg.nan.act = FALSE,
    clean.bootstrap = TRUE, suppress.messages = FALSE,
    fit.opt = c("a"),
    t0 = 0, tmax = NA, min.growth = NA, max.growth = NA,
    log.x.gc = FALSE, log.y.lin = TRUE, log.y.spline = TRUE,
    log.y.model = TRUE, biphasic = FALSE, lin.h = NULL,
    lin.R2 = 0.97, lin.RSD = 0.1, lin.dY = 0.05, interactive = FALSE,
    nboot.gc = 0, smooth.gc = 0.55, model.type = c(
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
    growth.thresh = 1.5, dr.have.atleast = 6, dr.parameter = c(
      "mu.linfit", "lambda.linfit", "dY.linfit",
      "A.linfit", "mu.spline", "lambda.spline", "dY.spline",
      "A.spline", "mu.model", "lambda.model", "dY.orig.model",
      "A.orig.model"
    ),
    smooth.dr = 0.1, log.x.dr = FALSE, log.y.dr = FALSE,
    nboot.dr = 0, report = NULL, out.dir = NULL, out.nm = NULL,
    export.fig = FALSE, export.res = FALSE, parallelize = TRUE,
    ...
)
{
  dr.parameter <- match.arg(dr.parameter)
  if (ec50 == TRUE)
  {
    dr.parameter.fit.method <- gsub(".+\\.", "", dr.parameter)
    if ((dr.parameter.fit.method == "spline" &&
         !any(fit.opt %in% c("a", "s"))) ||
        (dr.parameter.fit.method == "model" &&
         !any(fit.opt %in% c("a", "m"))) ||
        (dr.parameter.fit.method == "linfit" &&
         !any(fit.opt %in% c("a", "l"))))
      message(
        "The chosen 'dr.parameter' is not compatible with the selected fitting options ('fit.opt'). Dose-response analysis will not be performed."
      )
  }
  if (exists("lin.h") &&
      !is.null(lin.h) &&
      (is.na(lin.h) ||
       lin.h == ""))
    lin.h <- NULL
  # Define objects based on additional function
  # calls
  call <- match.call()

  ## remove strictly defined arguments
  call$grodata <- call$time <- call$data <- call$ec50 <- call$mean.grp <- call$mean.conc <- call$neg.nan.act <- call$clean.bootstrap <- call$suppress.messages <- call$export.res <- call$fit.opt <- call$t0 <- call$min.growth <- call$log.x.gc <- call$log.y.spline <- call$log.y.lin <- call$log.y.model <- call$biphasic <- call$tmax <- call$max.growth <- call$parallelize <- call$lin.h <- call$lin.R2 <- call$lin.RSD <- call$lin.dY <- call$interactive <- call$nboot.gc <- call$smooth.gc <- call$model.type <- call$growth.thresh <- call$dr.method <- call$dr.model <- call$dr.have.atleast <- call$dr.parameter <- call$smooth.dr <- call$log.x.dr <- call$log.y.dr <- call$nboot.dr <- call$report <- call$out.dir <- call$out.nm <- call$export.fig <- NULL


  arglist <- sapply(call, function(x) x)
  arglist <- unlist(arglist)[-1]
  ## Assign additional arguments (...) as R
  ## objects
  if (length(arglist) >
      0)
  {
    for (i in 1:length(arglist))
    {
      assign(
        names(arglist)[i],
        arglist[[i]]
      )
    }
  }

  # Test input
  if (is.null(grodata) ||
      !(is(grodata) ==
        "list") && !(is(grodata) ==
                     "grodata"))
  {
    if (is.numeric(as.matrix(time)) ==
        FALSE)
      stop(
        "Need a numeric matrix for 'time' or a grodata object created with read_data() or parse_data()."
      )
    if (is.numeric(as.matrix(data[-1:-3])) ==
        FALSE)
      stop(
        "Need a numeric matrix for 'data' or a grodata object created with read_data() or parse_data()."
      )
    if (is.logical(ec50) ==
        FALSE)
      stop("Need a logical value for 'ec50'")
  }
  if (!is.null(grodata))
  {
    time <- grodata$time
    if (!is.null(grodata$expdesign))
      expdesign <- grodata$expdesign
    data <- grodata$growth
  } else
  {
    dat.mat <- as.matrix(data)
    label <- unlist(
      lapply(
        1:nrow(dat.mat),
        function(x) paste(
          dat.mat[x, 1], dat.mat[x, 2], dat.mat[x,
                                                3], sep = " | "
        )
      )
    )
    condition <- dat.mat[, 1]
    replicate <- dat.mat[, 2]
    concentration <- dat.mat[, 3]
    expdesign <- data.frame(
      label, condition, replicate, concentration,
      check.names = FALSE
    )
  }
  if (ec50 == TRUE)
  {
    if (length(unique(expdesign$concentration)) <
        4)
      message(
        "No or not enough unique concentration information provided. Dose-Response analysis will be omitted."
      )
  }
  control <- growth.control(
    neg.nan.act = neg.nan.act, clean.bootstrap = clean.bootstrap,
    suppress.messages = suppress.messages, fit.opt = fit.opt,
    t0 = t0, min.growth = min.growth, tmax = tmax,
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
  nboot.gc <- control$nboot.gc
  nboot.dr <- control$nboot.dr
  out.gcFit <- NA
  out.drFit <- NA
  class(out.drFit) <- "drFit"

  # /// fit of growth curves
  # -----------------------------------
  if (exists("shiny") &&
      shiny == TRUE)
    out.gcFit <- growth.gcFit(time, data, control, shiny = TRUE, parallelize = parallelize)
  else
    out.gcFit <- growth.gcFit(time, data, control, parallelize = parallelize)

  # /// Estimate EC50 values
  if (ec50 == TRUE && !((dr.parameter.fit.method ==
                         "spline" && !any(fit.opt %in% c("a", "s"))) ||
                        (dr.parameter.fit.method == "model" && !any(fit.opt %in% c("a", "m"))) ||
                        (dr.parameter.fit.method == "linfit" && !any(fit.opt %in% c("a", "l")))) &&
      (length(unique(expdesign$concentration)) >=
       4))
  {
    out.drFit <- growth.drFit(
      summary.gcFit(out.gcFit),
      control
    )
    if (length(out.drFit) >
        1)
    {
      EC50.table <- out.drFit$drTable
      boot.ec <- out.drFit$boot.ec
    }
  }
  # ///
  grofit <- list(
    time = time, data = data, gcFit = out.gcFit,
    drFit = out.drFit, expdesign = expdesign, control = control
  )
  class(grofit) <- "grofit"
  if (!exists("shiny") ||
      shiny != TRUE)
  {
    if (!is.null(out.dir))
    {
      wd <- paste0(out.dir)
    } else
    {
      wd <- paste(
        getwd(), "/GrowthResults_", format(Sys.time(), "%Y%m%d_%H%M%S"),
        sep = ""
      )
    }
    if (export.res)
      dir.create(wd, showWarnings = FALSE)

    if(nrow(grofit[["gcFit"]][["gcTable"]]) == 1){
      gcTable <- as.data.frame((grofit[["gcFit"]][["gcTable"]]))
      gcTable <- data.frame(lapply(gcTable, as.character))
    } else {      
    gcTable <- data.frame(
      apply(
        grofit[["gcFit"]][["gcTable"]], 2,
        as.character
      )
    )
        }

      
    res.table.gc <- cbind(
      gcTable[, 1:3], Filter(
        function(x) !all(is.na(x)),
        gcTable[, -(1:3)]
      )
    )
    if (export.res)
    {
      export_Table(
        table = res.table.gc, out.dir = wd,
        out.nm = "results.gc"
      )
      # res.table.gc[, c(8:14, 20:27,
      # 29:44)] <- apply(res.table.gc[,
      # c(8:16, 20:27, 29:44)], 2,
      # as.numeric)
      message(
        paste0(
          "\nResults of growth fit analysis saved as tab-delimited text file in:\n''",
          "...", gsub(".+/", "", wd),
          "/results.gc.txt'"
        )
      )
    }

    # Export grouped results table
    if (("l" %in% control$fit.opt) || ("a" %in%
                                       control$fit.opt))
    {
      table_linear_group <- table_group_growth_linear(res.table.gc)
      names <- gsub(
        "<sub>", "_", gsub(
          "</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table_linear_group))
        )
      )
      table_linear_group <- as.data.frame(
        lapply(
          1:ncol(table_linear_group),
          function(x) gsub(
            "<strong>", "", gsub(
              "</strong>", "", table_linear_group[,
                                                  x]
            )
          )
        )
      )
      colnames(table_linear_group) <- names
      if (export.res)
        export_Table(
          table = table_linear_group, out.dir = wd,
          out.nm = "grouped_results_linear"
        )
    }

    if (("s" %in% control$fit.opt) || ("a" %in%
                                       control$fit.opt))
    {
      table_spline_group <- table_group_growth_spline(res.table.gc)
      names <- gsub(
        "<sub>", "_", gsub(
          "</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table_spline_group))
        )
      )
      table_spline_group <- as.data.frame(
        lapply(
          1:ncol(table_spline_group),
          function(x) gsub(
            "<strong>", "", gsub(
              "</strong>", "", table_spline_group[,
                                                  x]
            )
          )
        )
      )
      colnames(table_spline_group) <- names
      if (export.res)
        export_Table(
          table = table_spline_group, out.dir = wd,
          out.nm = "grouped_results_spline"
        )
    }

    if (("m" %in% control$fit.opt) || ("a" %in%
                                       control$fit.opt))
    {
      table_model_group <- table_group_growth_model(res.table.gc)
      names <- gsub(
        "<sub>", "_", gsub(
          "</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table_model_group))
        )
      )
      table_model_group <- as.data.frame(
        lapply(
          1:ncol(table_model_group),
          function(x) gsub(
            "<strong>", "", gsub(
              "</strong>", "", table_model_group[,
                                                 x]
            )
          )
        )
      )
      colnames(table_model_group) <- names
      if (export.res)
        export_Table(
          table = table_model_group, out.dir = wd,
          out.nm = "grouped_results_model"
        )
    }
    # # export table
    # utils::write.table(combined.df,
    # paste(wd, 'mean_results.gc.txt',
    # sep = '/'), row.names = FALSE, sep
    # = '\t') cat(paste0('Per-group
    # average results of growth fit
    # analysis saved as tab-delimited
    # text file in:\n', wd,
    # '/mean_results.gc.txt\n\n'))

    if (ec50 == TRUE && !((dr.parameter.fit.method ==
                           "spline" && !any(fit.opt %in% c("a", "s"))) ||
                          (dr.parameter.fit.method == "model" &&
                           !any(fit.opt %in% c("a", "m"))) ||
                          (dr.parameter.fit.method == "linfit" &&
                           !any(fit.opt %in% c("a", "l")))) &&
        (length(unique(expdesign$concentration)) >=
         4) && length(out.drFit) >
        1)
    {
      res.table.dr <- Filter(
        function(x) !all(is.na(x)),
        EC50.table
      )
      if (export.res)
      {
        export_Table(
          table = res.table.dr, out.dir = wd,
          out.nm = "results.dr"
        )
        message(
          paste0(
            "Results of EC50 analysis saved as tab-delimited text file in:\n'",
            "...", gsub(".+/", "", wd),
            "/results.dr.txt'"
          )
        )
      }
    } else
    {
      res.table.dr <- NULL
    }
    # Export RData object
    if (export.res)
      export_RData(grofit, out.dir = wd)

    if (any(report %in% c("pdf", "html")))
    {
      try(
        growth.report(
          grofit, out.dir = gsub(
            paste0(getwd(), "/"),
            "", wd
          ),
          ec50 = ifelse(
            length(out.drFit) >
              1, ec50, FALSE
          ),
          mean.grp = mean.grp, mean.conc = mean.conc,
          export = export.fig, format = report,
          out.nm = out.nm, parallelize = parallelize
        )
      )
    }
  }  # if(!exists('shiny') || shiny != TRUE)

  invisible(grofit)
}

#' Perform a growth curve analysis on all samples in the provided dataset.
#'
#' \code{growth.gcFit} performs all computational growth fitting operations based on the
#' user input.
#'
#' @param time (optional) A matrix containing time values for each sample.
#' @param data  Either... \enumerate{ \item a \code{grodata} object created with \code{\link{read_data}} or \code{\link{parse_data}},
#'   \item a list containing a \code{'time'} matrix as well as \code{'growth'} and, if appropriate, a \code{'fluorescence'} dataframes,
#'   or \item a dataframe containing growth values (if a \code{time} matrix is provided as separate argument).}
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#' @param parallelize Run linear fits and bootstrapping operations in parallel using all but one available processor cores
#' @param ... Further arguments passed to the shiny app.
#'
#' @return A \code{gcFit} object that contains all growth fitting results, compatible with
#'   various plotting functions of the QurvE package.
#' \item{raw.time}{Raw time matrix passed to the function as \code{time}.}
#' \item{raw.data}{Raw growth dataframe passed to the function as \code{data}.}
#' \item{gcTable}{Table with growth parameters and related statistics for each growth curve evaluation performed by the function. This table, which is also returned by the generic \code{summary.gcFit} method applied to a \code{gcFit} object, is used as an input for \code{\link{growth.drFit}}.}
#' \item{gcFittedLinear}{List of all \code{gcFitLinear} objects, generated by the call of \code{\link{growth.gcFitLinear}}. Note: access to each object in the list via double brace: gcFittedLinear\[\[#n\]\].}
#' \item{gcFittedModels}{List of all \code{gcFitModel} objects, generated by the call of \code{\link{growth.gcFitModel}}. Note: access to each object in the list via double brace: gcFittedModels\[\[#n\]\].}
#' \item{gcFittedSplines}{List of all \code{gcFitSpline} objects, generated by the call of \code{\link{growth.gcFitSpline}}. Note: access to each object via double brace: gcFittedSplines\[\[#n\]\].}
#' \item{gcBootSplines}{List of all \code{gcBootSpline} objects, generated by the call of \code{\link{growth.gcBootSpline}}. Note: access to each object via double brace: gcFittedSplines\[\[#n\]\].}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#'
#' @family workflows
#' @family growth fitting functions
#' @family dose-response analysis functions
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text
#'   geom_bar geom_errorbar geom_line geom_point geom_ribbon geom_segment ggplot
#'   ggplot_build ggplot ggtitle labs position_dodge scale_color_manual scale_fill_brewer
#'   scale_color_brewer scale_fill_manual scale_x_continuous scale_y_continuous
#'   scale_y_log10 theme theme_classic theme_minimal xlab ylab
#' @import foreach
#'
#' @examples
#' # Create random growth data set
#'   rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')
#'   rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = 'Test2')
#'
#'   rnd.data <- list()
#'   rnd.data[['time']] <- rbind(rnd.data1$time, rnd.data2$time)
#'   rnd.data[['data']] <- rbind(rnd.data1$data, rnd.data2$data)
#'
#' # Run growth curve analysis workflow
#'   res <- growth.gcFit(time = rnd.data$time,
#'                       data = rnd.data$data,
#'                       parallelize = FALSE,
#'                       control = growth.control(suppress.messages = TRUE,
#'                                                fit.opt = 's'))
#'
#'
#'
growth.gcFit <- function(
    time, data, control = growth.control(), parallelize = TRUE,
    ...
)
{
  # Define objects based on additional function
  # calls
  call <- match.call()

  ## remove strictly defined arguments
  call$time <- call$data <- call$control <- call$parallelize <- NULL


  arglist <- sapply(call, function(x) x)
  arglist <- unlist(arglist)[-1]
  ## Assign additional arguments (...) as R
  ## objects
  if (length(arglist) >
      0)
  {
    for (i in 1:length(arglist))
    {
      assign(
        names(arglist)[i],
        arglist[[i]]
      )
    }
  }

  if (!(any(
    is(data) ==
    "grodata"
  )))
  {
    if (is.numeric(as.matrix(time)) ==
        FALSE)
      stop(
        "Need a numeric matrix for 'time' or a grodata object created with read_data() or parse_data()."
      )
    if (is.numeric(as.matrix(data[-1:-3])) ==
        FALSE)
      stop(
        "Need a numeric matrix for 'data' or a grodata object created with read_data() or parse_data()."
      )
  } else if (!is.null(data))
  {
    time <- data$time
    data <- data$growth
  }
  # /// check if start growth values are above
  # min.growth in all samples
  max.growth <- unlist(
    lapply(
      1:nrow(data),
      function(x) max(
        as.numeric(as.matrix(data[x, -1:-3]))[!is.na(as.numeric(as.matrix(data[x, -1:-3])))]
      )
    )
  )
  if (is.numeric(control$min.growth) &&
      control$min.growth != 0)
  {
    if (!is.na(control$min.growth) &&
        all(
          as.numeric(max.growth) <
          control$min.growth
        ))
    {
      stop(
        paste0(
          "The chosen global start growth value (min.growth) is larger than every value in your dataset.\nThe maximum value in your dataset is: ",
          max(as.numeric(max.growth))
        )
      )
    }
  }
  # /// check input parameters
  if (methods::is(control) !=
      "grofit.control")
    stop("control must be of class grofit.control!")

  # /// check number of datasets
  if ((dim(time)[1]) !=
      (dim(data)[1]))
    stop(
      "gcFit: Different number of datasets in data and time"
    )

  # /// check fitting options
  old.options <- options()
  on.exit(options(old.options))
  if (!all(control$fit.opt %in% c("s", "m", "a", "l")))
  {
    options(warn = 1)
    warning(
      "fit.opt must contain 's', 'm', 'l', or 'a'. Changed to 'a' (all fit methods)!"
    )
    fit.opt = "a"
    options(warn = 0)
  }

  # /// Initialize some parameters
  out.table <- NULL
  used.model <- NULL
  fitpara.all <- list()
  fitnonpara.all <- list()
  fitlinear.all <- list()
  boot.all <- list()
  fitted.param <- NULL
  fitted.nonparam <- NULL
  bootstrap.param <- NULL
  reliability_tag_linear <- NA
  reliability_tag_param <- NA
  reliability_tag_nonpara <- NA

  if (control$interactive == FALSE && parallelize ==
      TRUE && dim(data)[1] >
      30 && (("l" %in% control$fit.opt) || ("a" %in%
                                            control$fit.opt) || ("s" %in% control$fit.opt &&
                                                                 control$nboot.gc > 0)))
  {
    times.ls <- lapply(
      1:nrow(time),
      function(x) time[x,
      ][!is.na(time[x, ])][!is.na(data[x, -1:-3])]
    )
    wells.ls <- lapply(
      1:nrow(data),
      function(x) as.numeric(
        data[x, -1:-3][!is.na(time[x, ])][!is.na(data[x, -1:-3])]
      )
    )
    gcIDs.ls <- lapply(
      1:nrow(data),
      function(x) as.matrix(data[x, 1:3])
    )
    wellnames.ls <- lapply(
      1:nrow(data),
      function(x) paste(
        as.character(data[x, 1]),
        as.character(data[x, 2]),
        as.character(data[x, 3]),
        sep = " | "
      )
    )

    # Set up computing clusters (all
    # available processor cores - 1)
    cl <- parallel::makeCluster(
      parallel::detectCores(all.tests = FALSE, logical = TRUE) -
        1
    )
    doParallel::registerDoParallel(cl)

    # Perform linear fits in parallel
    if (("l" %in% control$fit.opt) || ("a" %in%
                                       control$fit.opt))
    {
      fitlinear.all <- foreach::foreach(i = 1:dim(data)[1]) %dopar%
        {
          QurvE::growth.gcFitLinear(
            times.ls[[i]], wells.ls[[i]], gcID = gcIDs.ls[[i]],
            control = control
          )
        }
    } else
    {
      # /// generate list with empty
      # objects
      fitlinear.all <- lapply(
        1:nrow(data),
        function(x) list(
          raw.time = times.ls[[x]], raw.data = wells.ls[[x]],
          filt.time = NA, filt.data = NA, log.data = NA,
          gcID = gcIDs.ls[[x]], FUN = NA, fit = NA,
          par = c(
            y0 = NA, y0_lm = NA, mumax = 0,
            mu.se = NA, lag = NA, tmax_start = NA,
            tmax_end = NA, t_turn = NA, mumax2 = NA,
            y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
            tmax2_end = NA
          ),
          ndx = NA, ndx2 = NA, quota = NA,
          rsquared = NA, rsquared2 = NA, control = control,
          fitFlag = FALSE, fitFlag2 = FALSE
        )
      )
    }

    # # Perform model fits in parallel if
    # (('m' %in% control$fit.opt) || ('a'
    # %in% control$fit.opt)){ fitpara.all <-
    # foreach::foreach(i = 1:dim(data)[1] )
    # %dopar% {
    # QurvE::growth.gcFitModel(times.ls[[i]],
    # wells.ls[[i]], gcID = gcIDs.ls[[i]],
    # control = control) } } else { # ///
    # generate list with empty objects
    # fitpara.all <- lapply(1:nrow(data),
    # function(x) list(time.in =
    # times.ls[[x]], data.in = wells.ls[[x]],
    # raw.time = times.ls[[x]], raw.data =
    # wells.ls[[x]], gcID = gcIDs.ls[[x]],
    # fit.time = NA, fit.data = NA,
    # parameters = list(A=NA, mu=NA,
    # lambda=NA, integral=NA), model = NA,
    # nls = NA, reliable=NULL, fitFlag=FALSE,
    # control = control) ) } # Perform spline
    # fits in parallel if (('s' %in%
    # control$fit.opt) || ('a' %in%
    # control$fit.opt)){ fitnonpara.all <-
    # foreach::foreach(i = 1:dim(data)[1] )
    # %dopar% {
    # QurvE::growth.gcFitSpline(times.ls[[i]],
    # wells.ls[[i]], gcID = gcIDs.ls[[i]],
    # control = control) } } else { # ///
    # generate list with empty objects
    # fitnonpara.all <- lapply(1:nrow(data),
    # function(x) list(raw.time =
    # times.ls[[x]], raw.data =
    # wells.ls[[x]], gcID = gcIDs.ls[[x]],
    # fit.time = NA, fit.data = NA,
    # parameters = list(A = NA, dY = NA, mu =
    # NA, t.max = NA, lambda = NA, b.tangent
    # = NA, mu2 = NA, t.max2 = NA, lambda2 =
    # NA, b.tangent2 = NA, integral = NA),
    # spline = NA, parametersLowess = list(A
    # = NA, mu = NA, lambda = NA), reliable =
    # NULL, fitFlag = FALSE, fitFlag2 =
    # FALSE, control = control) ) }

    # Perform spline bootstrappings in
    # parallel
    if ((("s" %in% control$fit.opt) || ("a" %in%
                                        control$fit.opt)) && (control$nboot.gc >
                                                              10))
    {
      boot.all <- foreach::foreach(i = 1:dim(data)[1]) %dopar%
        {
          QurvE::growth.gcBootSpline(
            times.ls[[i]], wells.ls[[i]], gcIDs.ls[[i]],
            control
          )
        }
    } else
    {
      # /// create empty gcBootSpline
      # object
      boot.all <- lapply(
        1:nrow(data),
        function(x) list(
          raw.time = times.ls[[x]], raw.data = wells.ls[[x]],
          gcID = gcIDs.ls[[x]], boot.x = NA,
          boot.y = NA, boot.gcSpline = NA,
          lambda = NA, mu = NA, A = NA, integral = NA,
          bootFlag = FALSE, control = control
        )
      )
    }
    parallel::stopCluster(cl = cl)

    # Assign classes to list elements
    for (i in 1:length(fitlinear.all))
    {
      class(fitlinear.all[[i]]) <- "gcFitLinear"
    }
    # for(i in 1:length(fitpara.all)){
    # class(fitpara.all[[i]]) <- 'gcFitModel'
    # } for(i in 1:length(fitnonpara.all)){
    # class(fitnonpara.all[[i]]) <-
    # 'gcFitSpline' }
    for (i in 1:length(boot.all))
    {
      class(boot.all[[i]]) <- "gcBootSpline"
    }
  }

  reliability_tag <- c()
  # /// loop over all wells
  for (i in 1:dim(data)[1])
  {
    # Progress indicator for shiny app
    if (exists("shiny") &&
        shiny == TRUE)
    {
      shiny::incProgress(
        amount = 1/(dim(data)[1]),
        message = "Computations completed"
      )
    }
    # /// conversion, to handle even
    # data.frame inputs
    acttime <- as.numeric(as.matrix(time[i, ]))[!is.na(as.numeric(as.matrix(time[i, ])))][!is.na(as.numeric(as.matrix((data[i, -1:-3]))))]
    actwell <- as.numeric(as.matrix((data[i, -1:-3])))[!is.na(as.numeric(as.matrix(time[i, ])))][!is.na(as.numeric(as.matrix((data[i, -1:-3]))))]

    gcID <- as.matrix(data[i, 1:3])
    wellname <- paste(
      as.character(data[i, 1]),
      as.character(data[i, 2]),
      as.character(data[i, 3]),
      sep = " | "
    )
    if (control$suppress.messages == FALSE)
    {
      cat("\n\n")
      cat(
        paste(
          "=== ", as.character(i),
          ". [", wellname, "] growth curve =================================\n",
          sep = ""
        )
      )
      cat(
        "----------------------------------------------------\n"
      )
    }
    if (parallelize == FALSE || control$interactive ==
        TRUE || dim(data)[1] <=
        30 || !("l" %in% control$fit.opt || "a" %in%
                control$fit.opt || ("s" %in% control$fit.opt &&
                                    control$nboot.gc > 10)))
    {
      # /// Linear regression on
      # log-transformed data
      if (("l" %in% control$fit.opt) || ("a" %in%
                                         control$fit.opt))
      {
        fitlinear <- growth.gcFitLinear(
          acttime, actwell, gcID = gcID,
          control = control
        )
        fitlinear.all[[i]] <- fitlinear
      } else
      {
        # /// generate empty object
        fitlinear <- list(
          raw.time = acttime, raw.data = actwell,
          filt.time = NA, filt.data = NA,
          log.data = NA, gcID = gcID, FUN = NA,
          fit = NA, par = c(
            y0 = NA, y0_lm = NA, mumax = 0,
            mu.se = NA, lag = NA, tmax_start = NA,
            tmax_end = NA, t_turn = NA, mumax2 = NA,
            y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
            tmax2_end = NA
          ),
          ndx = NA, ndx2 = NA, quota = NA,
          rsquared = NA, rsquared2 = NA,
          control = control, fitFlag = FALSE,
          fitFlag2 = FALSE
        )
        class(fitlinear) <- "gcFitLinear"
        fitlinear.all[[i]] <- fitlinear
      }
      # /// plot linear fit
      if ((control$interactive == TRUE))
      {
        if (("l" %in% control$fit.opt) ||
            ("a" %in% control$fit.opt))
        {
          answer_satisfied <- "n"
          reliability_tag_linear <- NA
          while ("n" %in% answer_satisfied)
          {
            if (control$log.y.lin)
            {
              try(plot(fitlinear, log = "y"))
            } else
            {
              try(plot(fitlinear, log = ""))
            }
            graphics::mtext(
              side = 3, line = 3, adj = 0,
              outer = FALSE, cex = 1, wellname
            )
            answer_satisfied <- readline(
              "Are you satisfied with the linear fit (y/n)?\n\n"
            )
            if ("n" %in% answer_satisfied)
            {
              test_answer <- readline(
                "Enter: t0, h, quota, min.growth, R2, RSD, tmax, max.growth                         >>>>\n\n [Skip (enter 'n'), or adjust fit parameters (see ?growth.gcFitLinear).\n Leave {blank} at a given position if standard parameters are desired.]\n\n"
              )
              if ("n" %in% test_answer)
              {
                cat(
                  "\n Tagged the linear fit of this sample as unreliable !\n\n"
                )
                reliability_tag_linear <- FALSE
                fitlinear$reliable <- FALSE
                fitlinear.all[[i]]$reliable <- FALSE
                answer_satisfied <- "y"
              }  # end if ('n' %in% test_answer)
              else
              {
                new_params <- unlist(strsplit(test_answer, split = ","))
                t0_new <- ifelse(
                  !is.na(as.numeric(new_params[1])),
                  as.numeric(new_params[1]),
                  control$t0
                )
                h_new <- if_else(
                  !is.na(as.numeric(new_params[2])),
                  as.numeric(new_params[2]),
                  control$lin.h
                )
                quota_new <- ifelse(
                  !is.na(as.numeric(new_params[3])),
                  as.numeric(new_params[3]),
                  0.95
                )
                min.growth_new <- ifelse(
                  !is.na(as.numeric(new_params[4])),
                  as.numeric(new_params[4]),
                  control$min.growth
                )
                R2_new <- ifelse(
                  !is.na(as.numeric(new_params[5])),
                  as.numeric(new_params[5]),
                  control$lin.R2
                )
                RSD_new <- ifelse(
                  !is.na(as.numeric(new_params[6])),
                  as.numeric(new_params[6]),
                  control$lin.RSD
                )
                tmax_new <- ifelse(
                  !is.na(as.numeric(new_params[7])),
                  as.numeric(new_params[7]),
                  control$tmax
                )
                max.growth_new <- ifelse(
                  !is.na(as.numeric(new_params[8])),
                  as.numeric(new_params[8]),
                  control$max.growth
                )

                control_new <- control
                control_new$t0 <- t0_new
                if (!is.na(h_new))
                  control_new$lin.h <- h_new
                control_new$lin.R2 <- R2_new
                control_new$lin.RSD <- RSD_new
                control_new$tmax <- tmax_new
                control_new$max.growth <- max.growth_new

                if (is.numeric(min.growth_new))
                {
                  if (!is.na(min.growth_new) &&
                      all(
                        as.vector(actwell) <
                        min.growth_new
                      ))
                  {
                    message(
                      paste0(
                        "Start growth values need to be greater than 'min.growth'.\nThe minimum start value in your dataset is: ",
                        min(as.vector(actwell)),
                        ". 'min.growth' was not adjusted."
                      ),
                      call. = FALSE
                    )
                  } else if (!is.na(min.growth_new))
                  {
                    control_new$min.growth <- min.growth_new
                  }
                }
                fitlinear <- growth.gcFitLinear(
                  acttime, actwell,
                  gcID = gcID, control = control_new,
                  quota = quota_new
                )
                fitlinear.all[[i]] <- fitlinear
              }  #end else
            }  # end if ('n' %in% test_answer)
            else
            {
              reliability_tag_linear <- TRUE
              fitlinear$reliable <- TRUE
              fitlinear.all[[i]]$reliable <- TRUE
              if (control$suppress.messages == FALSE)
                cat("Sample was (more or less) o.k.\n")
            }  # end else
          }  # end while ('n' %in% answer_satisfied)
        }  # end if (('l' %in% control$fit.opt) || ('a'  %in% control$fit.opt))
      }  # end if ((control$interactive == TRUE))
      else
      {
        reliability_tag_linear <- TRUE
        fitlinear$reliable <- TRUE
        fitlinear.all[[i]]$reliable <- TRUE
      }
    }  # control$interactive == TRUE || dim(data)[1] <= 30
    # /// Parametric fit
    if (("m" %in% control$fit.opt) || ("a" %in%
                                       control$fit.opt))
    {
      fitpara <- growth.gcFitModel(acttime, actwell, gcID, control)
      fitpara.all[[i]] <- fitpara
    } else
    {
      # /// generate empty object
      fitpara <- list(
        time.in = acttime, data.in = actwell,
        raw.time = acttime, raw.data = actwell,
        gcID = gcID, fit.time = NA, fit.data = NA,
        parameters = list(
          A = NA, mu = NA, tD = NA, lambda = NA,
          integral = NA
        ),
        model = NA, nls = NA, reliable = NULL,
        fitFlag = FALSE, control = control
      )
      class(fitpara) <- "gcFitModel"
      fitpara.all[[i]] <- fitpara
    }

    # /// Non parametric fit
    if (("s" %in% control$fit.opt) || ("a" %in%
                                       control$fit.opt))
    {
      nonpara <- growth.gcFitSpline(acttime, actwell, gcID, control)
      fitnonpara.all[[i]] <- nonpara
    } else
    {
      # /// generate empty object
      nonpara <- list(
        raw.time = acttime, raw.data = actwell,
        gcID = gcID, fit.time = NA, fit.data = NA,
        parameters = list(
          A = NA, dY = NA, mu = NA, t.max = NA,
          lambda = NA, b.tangent = NA, mu2 = NA,
          t.max2 = NA, lambda2 = NA, b.tangent2 = NA,
          integral = NA
        ),
        parametersLowess = list(A = NA, mu = NA, lambda = NA),
        spline = NA, spline.deriv1 = NA, reliable = NULL,
        fitFlag = FALSE, fitFlag2 = FALSE,
        control = control
      )
      class(nonpara) <- "gcFitSpline"
      fitnonpara.all[[i]] <- nonpara
    }
    # /// plotting parametric fit
    if ((control$interactive == TRUE))
    {
      if ((("m" %in% control$fit.opt) ||
           ("a" %in% control$fit.opt)))
      {
        if (fitpara$fitFlag == TRUE)
        {
          plot.gcFitModel(
            fitpara, colData = 1, colModel = 2,
            colLag = 3, cex.point = 2,
            raw = T
          )
          # legend(x='bottomright',
          # legend=fitpara$model,
          # col='red', lty=1)
          # title('Parametric fit')
          # graphics::mtext(line =
          # 0.5, side=3, outer = FALSE,
          # cex=1, wellname)
        }
        # /// here a manual
        # reliability tag is set in
        # the interactive mode
        reliability_tag_param <- NA
        answer <- readline(
          "Are you satisfied with the model fit (y/n)?\n\n"
        )
        if ("n" %in% answer)
        {
          cat(
            "\n Tagged the parametric fit of this sample as unreliable !\n\n"
          )
          reliability_tag_param <- FALSE
          fitpara$reliable <- FALSE
          fitpara.all[[i]]$reliable <- FALSE
        } else
        {
          reliability_tag_param <- TRUE
          fitpara$reliable <- TRUE
          fitpara.all[[i]]$reliable <- TRUE
          if (control$suppress.messages == FALSE)
            cat("Sample was (more or less) o.k.\n")
        }
      }  # if ((('m' %in% control$fit.opt) || ('a'  %in% control$fit.opt) ) && fitpara$fitFlag == TRUE)
      else
      {
        reliability_tag_param <- FALSE
        fitpara$reliable <- FALSE
        fitpara.all[[i]]$reliable <- FALSE
      }
      # /// plotting nonparametric fit
      if (("s" %in% control$fit.opt) || ("a" %in%
                                         control$fit.opt))
      {
        if (nonpara$fitFlag == TRUE)
        {
          answer_satisfied <- "n"
          reliability_tag_nonpara <- NA
          while ("n" %in% answer_satisfied)
          {
            if (control$log.y.spline)
            {
              plot.gcFitSpline(
                nonpara, add = FALSE,
                raw = TRUE, slope = TRUE,
                colData = 1, cex.point = 2,
                plot = TRUE, export = F
              )
            } else
            {
              plot.gcFitSpline(
                nonpara, add = FALSE,
                raw = TRUE, slope = TRUE,
                log.y = FALSE, colData = 1,
                cex.point = 2, plot = TRUE,
                export = F
              )
            }
            answer_satisfied <- readline(
              "Are you satisfied with the spline fit (y/n)?\n\n"
            )
            if ("n" %in% answer_satisfied)
            {
              test_answer <- readline(
                "Enter: smooth.gc, t0, min.growth, tmax, max.growth                        >>>> \n\n [Skip (enter 'n'), or smooth.gc, t0, and min.growth (see ?growth.control).\n Leave {blank} at a given position if standard parameters are desired.]\n\n "
              )
              if ("n" %in% test_answer)
              {
                cat(
                  "\n Tagged the linear fit of this sample as unreliable !\n\n"
                )
                reliability_tag_nonpara <- FALSE
                nonpara$reliable <- FALSE
                fitnonpara.all[[i]]$reliable <- FALSE
                fitnonpara.all[[i]]$FitFlag <- FALSE
                answer_satisfied <- "y"
              }  # end if ('n' %in% test_answer)
              else
              {
                new_params <- unlist(strsplit(test_answer, split = ","))
                if (!is.na(as.numeric(new_params[2])) &&
                    as.numeric(new_params[2]) !=
                    "")
                {
                  t0_new <- as.numeric(new_params[2])
                } else
                {
                  t0_new <- control$t0
                }
                smooth.gc_new <- as.numeric(new_params[1])

                control_new <- control
                if (!is.na(smooth.gc_new) &&
                    smooth.gc_new !=
                    "")
                {
                  control_new$smooth.gc <- smooth.gc_new
                }
                control_new$t0 <- t0_new
                min.growth_new <- as.numeric(new_params[3])
                if (!is.na(min.growth_new))
                {
                  if (is.numeric(min.growth_new) &&
                      min.growth_new !=
                      0 && all(
                        as.vector(actwell) <
                        min.growth_new
                      ))
                  {
                    message(
                      paste0(
                        "Start growth values need to be below 'min.growth'.\nThe minimum start value in your dataset is: ",
                        min(
                          as.vector(
                            data[,
                                 4]
                          )
                        ),
                        ". 'min.growth' was not adjusted."
                      ),
                      call. = FALSE
                    )
                  } else if (!is.na(min.growth_new))
                  {
                    control_new$min.growth <- min.growth_new
                  }
                }

                tmax_new <- as.numeric(new_params[4])
                if (!is.na(tmax_new) &&
                    tmax_new != "")
                {
                  control_new$tmax <- tmax_new
                }
                max.growth_new <- as.numeric(new_params[5])
                if (!is.na(max.growth_new) &&
                    max.growth_new !=
                    "")
                {
                  control_new$max.growth <- max.growth_new
                }

                nonpara <- growth.gcFitSpline(
                  acttime, actwell,
                  gcID, control_new
                )
                fitnonpara.all[[i]] <- nonpara
              }  #end else
            }  # end if ('n' %in% answer_satisfied)
            else
            {
              reliability_tag_nonpara <- TRUE
              nonpara$reliable <- TRUE
              fitnonpara.all[[i]]$reliable <- TRUE
              fitnonpara.all[[i]]$FitFlag <- TRUE
              if (control$suppress.messages == FALSE)
                cat("Sample was (more or less) o.k.\n")
            }  # end else
          }  # end while ('n' %in% answer_satisfied)
        }  # end if (nonpara$fitFlag == TRUE)
      }  # end if (('s' %in% control$fit.opt) || ('a'  %in% control$fit.opt) )
    }  # end of if((control$interactive == TRUE))
    else
    {
      reliability_tag_param <- TRUE
      reliability_tag_nonpara <- TRUE
      nonpara$reliable <- TRUE
      fitpara$reliable <- TRUE
      fitnonpara.all[[i]]$reliable <- TRUE
      fitpara.all[[i]]$reliable <- TRUE
    }
    if (parallelize == FALSE || control$interactive ==
        TRUE || dim(data)[1] <=
        30 || !("l" %in% control$fit.opt || "a" %in%
                control$fit.opt || ("s" %in% control$fit.opt &&
                                    control$nboot.gc > 10)))
    {
      # /// Beginn Bootstrap
      if ((("s" %in% control$fit.opt) ||
           ("a" %in% control$fit.opt)) && (control$nboot.gc >
                                           0) && (reliability_tag_nonpara ==
                                                  TRUE) && nonpara$fitFlag == TRUE)
      {
        bt <- growth.gcBootSpline(acttime, actwell, gcID, control)
        boot.all[[i]] <- bt
      }  # /// end of if (control$nboot.gc ...)
      else
      {
        # /// create empty gcBootSpline
        # object
        bt <- list(
          raw.time = acttime, raw.data = actwell,
          gcID = gcID, boot.x = NA, boot.y = NA,
          boot.gcSpline = NA, lambda = NA,
          mu = NA, A = NA, integral = NA,
          bootFlag = FALSE, control = control
        )
        class(bt) <- "gcBootSpline"
        boot.all[[i]] <- bt
      }
    }  # if(interactive == TRUE || 1:dim(data)[1] <= 30 ||
    reliability_tag <- c(
      reliability_tag, any(
        reliability_tag_linear, reliability_tag_nonpara,
        reliability_tag_param
      )
    )
    # create output table description <-
    # data.frame(TestId=data[i,1],
    # AddId=data[i,2],concentration=data[i,3],
    # reliability_tag=reliability_tag,
    # used.model=fitpara$model,
    # log.x=control$log.x.gc,
    # log.y=control$log.y.spline,
    # nboot.gc=control$nboot.gc)

    # fitted <- cbind(description,
    # summary.gcFitLinear(fitlinear),
    # summary.gcFitModel(fitpara),
    # summary.gcFitSpline(nonpara),
    # summary.gcBootSpline(bt))

    # out.table <- rbind(out.table, fitted)
    # class(out.table) <- c('data.frame',
    # 'gcTable')

  }  # /// end of for (i in 1:dim(data)[1])
  # Assign names to list elements
  names(fitlinear.all) <- names(fitpara.all) <- names(fitnonpara.all) <- names(boot.all) <- paste0(
    as.character(data[, 1]),
    " | ", as.character(data[, 2]),
    " | ", as.character(data[, 3])
  )

  # create output table
  description <- lapply(
    1:nrow(data),
    function(x) data.frame(
      TestId = data[x, 1], AddId = data[x, 2],
      concentration = data[x, 3], reliability_tag = reliability_tag[x],
      used.model = ifelse(
        is.null(fitpara.all[[x]]$model),
        NA, fitpara.all[[x]]$model
      ),
      log.x = control$log.x.gc, log.y.lin = control$log.y.lin,
      log.y.spline = control$log.y.spline, log.y.model = control$log.y.model,
      nboot.gc = control$nboot.gc
    )
  )

  fitted <- lapply(
    1:length(fitlinear.all),
    function(x) cbind(
      description[[x]], summary.gcFitLinear(fitlinear.all[[x]]),
      summary.gcFitModel(fitpara.all[[x]]),
      summary.gcFitSpline(fitnonpara.all[[x]]),
      summary.gcBootSpline(boot.all[[x]])
    )
  )

  out.table <- do.call(rbind, fitted)
  class(out.table) <- c("data.frame", "gcTable")
  # Combine results into list 'gcFit'
  gcFit <- list(
    raw.time = time, raw.data = data, gcTable = out.table,
    gcFittedLinear = fitlinear.all, gcFittedModels = fitpara.all,
    gcFittedSplines = fitnonpara.all, gcBootSplines = boot.all,
    control = control
  )
  class(gcFit) <- "gcFit"
  invisible(gcFit)
}
