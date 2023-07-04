#' Create a PDF and HTML report with results from a growth curve analysis workflow
#'
#' \code{growth.report} requires a \code{grofit} object and creates a report in PDF and HTML format that summarizes all results.
#'
#' @param grofit A \code{grofit} object created with \code{\link{growth.workflow}}.
#' @param out.dir (Character) The path or name of the folder in which the report files are created.  If \code{NULL}, the folder will be named with a combination of 'Report.growth_' and the current date and time.
#' @param out.nm {Character or \code{NULL}} Define the name of the report files. If \code{NULL}, the files will be named with a combination of 'GrowthReport_' and the current date and time.
#' @param ec50 (Logical) Was a dose-response analysis performed in \code{\link{growth.workflow}} \code{TRUE} or not \code{FALSE}?
#' @param export (Logical) Shall all plots generated in the report be exported as individual PDF and PNG files \code{TRUE} or not \code{FALSE}?
#' @param ... Further arguments passed to create a report. Currently supported:
#' \itemize{
#'    \item \code{mean.grp}: Define groups to combine into common plots in the report based on sample identifiers. Partial matches with sample/group names are accepted. Can be \code{'all'}, a string vector, or a list of string vectors. Note: The maximum number of sample groups (with unique condition/concentration indicators) is 50. If you have more than 50 groups, option \code{'all'} will produce the error \code{! Insufficient values in manual scale. [Number] needed but only 50 provided}.
#'    \item \code{mean.conc}: Define concentrations to combine into common plots in the  report. Can be a numeric vector, or a list of numeric vectors.
#' }
#' @param ec50 (Logical) Display results of dose-response analysis (\code{TRUE}) or not (\code{FALSE}).
#' @param format (Character) Define the file format for the report, PDF (\code{'pdf'}) and/or HTML (\code{'html'}). Default: (\code{c('pdf', 'html')})
#' @param parallelize (Logical) Create plots using all but one available processor cores (\code{TRUE}) or only a single core (\code{FALSE}).
#'
#' @details
#' The template .Rmd file used within this function can be found within the QurvE package installation directory.
#'
#'
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
#' @importFrom foreach %dopar%
#' @importFrom DT datatable
#' @importFrom stats AIC as.formula coef fitted.values formula integrate lm loess lowess median nls predict sd setNames smooth.spline terms time
#' @importFrom magrittr %>%
#' @import knitr
#' @importFrom plyr rbind.fill
#' @importFrom kableExtra kable_styling column_spec linebreak
#' @include utils.R
#' @family reports
#' @return \code{NULL}
#' @examples
#' \dontrun{
#' # Create random growth data set
#'   rnd.data <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')
#'
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
#'   growth.report(res, out.dir = tempdir(), parallelize = FALSE)
#' }
growth.report <- function(
    grofit, out.dir = tempdir(), out.nm = NULL, ec50 = FALSE,
    format = c("pdf", "html"),
    export = FALSE, parallelize = TRUE, ...
)
    {
    if (any(format %in% "pdf"))
        {
        if (!requireNamespace("tinytex", quietly = TRUE))
            {
            stop(
                "Please install package 'tinytex' to render PDF reports."
            )
        } else if (!tinytex::is_tinytex())
        {
            stop(
                "TinyTex was not found on your system. To render PDF reports, please execute tinytex::install_tinytex()."
            )
        }
    }
    try(
        showModal(
            modalDialog(
              HTML("Rendering report...<br><br>(This can take up to several minutes)"),
                footer = NULL
            )
        ),
        silent = TRUE
    )
    # results an object of class grofit
    if (methods::is(grofit) !=
        "grofit")
        stop(
            "grofit needs to be an object created with growth.workflow()."
        )
    # Define objects based on additional function
    # calls
    call <- match.call()
    ## remove strictly defined arguments
    call$grofit <- call$out.dir <- call$out.nm <- call$ec50 <- call$format <- call$export <- call$parallelize <- NULL
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
    if (!exists("mean.grp"))
        mean.grp <- NA
    if (!exists("mean.conc"))
        mean.conc <- NA
    gcFit <- grofit$gcFit
    drFit <- grofit$drFit
    control <- grofit$control
    time <- grofit$gcFit$raw.time
    data <- grofit$gcFit$raw.data
    if (!exists("res.table.gc"))
        {
        res.table.gc <- grofit$gcFit$gcTable
    }
    if (!exists("res.table.dr"))
        {
        if (length(grofit$drFit) >
            1)
            res.table.dr <- grofit$drFit$drTable
    }
    if (any(
        c("a", "b", "s") %in%
            grofit$control$fit.opt
    ))
        {
        # find minimum and maximum mu values in
        # whole dataset to equilibrate derivative
        # plots for spline fits
        mu.min <- suppressWarnings(
            min(
                sapply(
                  1:length(grofit$gcFit$gcFittedSplines),
                  function(x) ifelse(
                    all(
                      is.na(grofit$gcFit$gcFittedSplines[[x]]$spline.deriv1)
                  ),
                    NA, min(
                      grofit$gcFit$gcFittedSplines[[x]]$spline.deriv1$y
                  )
                )
              ),
                na.rm = TRUE
            )
        ) *
            1.05
        if (mu.min > 0)
            mu.min <- 0
        mu.max <- suppressWarnings(
            max(
                sapply(
                  1:length(grofit$gcFit$gcFittedSplines),
                  function(x) ifelse(
                    all(
                      is.na(grofit$gcFit$gcFittedSplines[[x]]$spline.deriv1)
                  ),
                    NA, max(
                      grofit$gcFit$gcFittedSplines[[x]]$spline.deriv1$y
                  )
                )
              ),
                na.rm = TRUE
            )
        ) *
            1.05
    }
    if (!is.null(out.dir))
        {
        wd <- out.dir
    } else
    {
        wd <- paste(
            getwd(), "/Report.growth_", format(Sys.time(), "%Y%m%d_%H%M%S"),
            sep = ""
        )
    }
    if (is.null(out.nm))
        {
        out.nm <- paste(
            "/GrowthReport_", format(Sys.time(), "%Y%m%d_%H%M%S"),
            sep = ""
        )
    }
    dir.create(wd, showWarnings = FALSE)
    message("Render reports...")
    for (i in 1:length(.libPaths()))
        {
        QurvE.ndx <- grep("QurvE", list.files(.libPaths()[i]))
        if (length(QurvE.ndx) >
            0)
            {
            Report.wd <- paste0(.libPaths()[i], "/QurvE")
        }
    }
    file <- paste0(Report.wd, "/Report_Growth.Rmd")

    # Copy report files into temp directory
    report_path <- tempfile(fileext = ".Rmd")
    file.copy(file, report_path, overwrite = TRUE)

    if (all( c("pdf", "html") %in% format )) {
      format <- c("html_document", "pdf_document")
    } else if ("pdf" %in% format){
      format <- "pdf_document"
    } else if ("html" %in% format){
      format <- "html_document"
    } else {
      stop(
        "Please define a valid report format, either 'pdf', 'html', or c('pdf', 'html')."
      )
    }


      rmarkdown::render(
        input = report_path, output_format = format, output_dir = wd,
        output_file = out.nm, quiet = TRUE
      )

    message(paste0("Report files saved in: '/", wd, "'"))
    unlink(
        paste0(tempdir(), "/Plots"),
        recursive = TRUE
    )
    try(removeModal(), silent = TRUE)
    invisible(NULL)
}

#' Create a PDF and HTML report with results from a fluorescence analysis workflow
#'
#' \code{fl.report} requires a \code{flFitRes} object and creates a report in PDF and HTML format that summarizes all results obtained.
#'
#' @param flFitRes A \code{grofit} object created with \code{\link{fl.workflow}}.
#' @param out.dir (Character) The path or name of the folder in which the report files are created.  If \code{NULL}, the folder will be named with a combination of 'Report.fluorescence_' and the current date and time.
#' @param out.nm {Character or \code{NULL}} Define the name of the report files. If \code{NULL}, the files will be named with a combination of 'FluorescenceReport_' and the current date and time.
#' @param ec50 (Logical) Was a dose-response analysis performed in \code{\link{fl.workflow}} \code{TRUE} or not \code{FALSE}?
#' @param format (Character) Define the file format for the report, PDF (\code{'pdf'}) and/or HTML (\code{'html'}). Default: (\code{c('pdf', 'html')})
#' @param export (Logical) Shall all plots generated in the report be exported as individual PDF and PNG files \code{TRUE} or not \code{FALSE}?
#' @param ... Further arguments passed to create a report. Currently required:
#' \itemize{
#'    \item \code{mean.grp}: Define groups to combine into common plots in the report based on sample identifiers. Partial matches with sample/group names are accepted. Can be \code{'all'}, a vector of strings, or a list of string vectors. Note: The maximum number of sample groups (with unique condition/concentration indicators) is 50. If you have more than 50 groups, option \code{'all'} will produce the error \code{! Insufficient values in manual scale. [Number] needed but only 50 provided}.
#'    \item \code{mean.conc}: Define concentrations to combine into common plots in the  report. Can be a numeric vector, or a list of numeric vectors.
#' }
#' @param ec50 (Logical) Display results of dose-response analysis (\code{TRUE}) or not (\code{FALSE}).
#' @param format (Character) Define the file format for the report, PDF (\code{'pdf'}) and/or HTML (\code{'html'}). Default: (\code{c('pdf', 'html')})
#' @param parallelize (Logical) Create plots using all but one available processor cores (\code{TRUE}) or only a single core (\code{FALSE}).
#'
#' @export
#'
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
#' @importFrom foreach %dopar%
#' @importFrom DT datatable
#' @importFrom kableExtra kable_styling column_spec linebreak
#' @import knitr
#' @include utils.R
#' @return \code{NULL}
#' @details The template .Rmd file used within this function can be found within the QurvE package installation directory.
#' @examples
#' # load example dataset
#' \dontrun{
#' input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
#'                    data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
#'                    csvsep = "\t",
#'                    csvsep.fl = "\t")
#'
#' # Run workflow
#' res <- fl.workflow(grodata = input, ec50 = FALSE, fit.opt = 's',
#'                    x_type = 'time', norm_fl = TRUE,
#'                    dr.parameter = 'max_slope.spline',
#'                    suppress.messages = TRUE,
#'                    parallelize = FALSE)
#'
#' fl.report(res, out.dir = tempdir(), parallelize = FALSE)
#' }
fl.report <- function(
    flFitRes, out.dir = tempdir(), out.nm = NULL, ec50 = FALSE,
    format = c("pdf", "html"),
    export = FALSE, parallelize = TRUE, ...
)
    {
    if (any(format %in% "pdf"))
        {
        if (!requireNamespace("tinytex", quietly = TRUE))
            {
            stop(
                "Please install package 'tinytex' to render PDF reports."
            )
        } else if (!tinytex::is_tinytex())
        {
            stop(
                "TinyTex was not found on your system. To render PDF reports, please execute tinytex::install_tinytex()."
            )
        }
    }
    try(
        showModal(
            modalDialog(
                HTML("Rendering report...<br><br>(This can take up to several minutes)"),
                footer = NULL
            )
        ),
        silent = TRUE
    )
    # results an object of class grofit
    if (is(flFitRes) !=
        "flFitRes")
        stop(
            "flFitRes needs to be an object created with fl.workflow()."
        )
    # Define objects based on additional function
    # calls
    call <- match.call()
    ## remove strictly defined arguments
    call$flFitRes <- call$out.dir <- call$out.nm <- call$ec50 <- call$format <- call$export <- call$parallelize <- NULL
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
    if (!exists("mean.grp"))
        mean.grp <- NA
    if (!exists("mean.conc"))
        mean.conc <- NA
    flFit <- flFitRes$flFit
    drFit <- flFitRes$drFit
    # flFit2 <- flFitRes$flFit2 drFit2 <-
    # flFitRes$drFit2
    control <- flFitRes$control
    time <- flFitRes$time
    data <- flFitRes$data
    if (!exists("res.table.fl"))
        {
        res.table.fl <- flFitRes$flFit$flTable
    }
    if (!exists("res.table.dr"))
        {
        if (length(flFitRes$drFit) >
            1 && length(flFitRes$drFit$drTable) >
            2)
            res.table.dr <- flFitRes$drFit$drTable
    }
    # if(!exists('res.table.gc2')){
    # if(length(flFit2)>1){ res.table.fl2 <-
    # flFitRes$flFit2$flTable } }
    # if(!exists('res.table.dr2')){
    # if(length(flFitRes$drFit2)>1 &&
    # !is.na(flFitRes$drFit2$drTable))
    # res.table.dr2 <- flFitRes$drFit2$drTable }
    if (any(
        c("a", "s") %in%
            flFitRes$control$fit.opt
    ))
        {
        # find minimum and maximum mu values in
        # whole dataset to equilibrate derivative
        # plots for spline fits
        mu.min1 <- suppressWarnings(
            min(
                sapply(
                  1:length(flFitRes$flFit$flFittedSplines),
                  function(x) ifelse(
                    all(
                      is.na(
                        flFitRes$flFit$flFittedSplines[[x]]$spline.deriv1
                    )
                  ),
                    NA, min(
                      flFitRes$flFit$flFittedSplines[[x]]$spline.deriv1$y
                  )
                )
              ),
                na.rm = TRUE
            )
        ) *
            1.05
        if (mu.min1 > 0)
            mu.min1 <- 0
        mu.max1 <- suppressWarnings(
            max(
                sapply(
                  1:length(flFitRes$flFit$flFittedSplines),
                  function(x) ifelse(
                    all(
                      is.na(
                        flFitRes$flFit$flFittedSplines[[x]]$spline.deriv1
                    )
                  ),
                    NA, max(
                      flFitRes$flFit$flFittedSplines[[x]]$spline.deriv1$y
                  )
                )
              ),
                na.rm = TRUE
            )
        ) *
            1.05
        # if(length(flFit2)>1){ mu.min2 <-
        # suppressWarnings(min(sapply(1:length(flFitRes$flFit2$flFittedSplines),
        # function(x)
        # ifelse(all(is.na(flFitRes$flFit2$flFittedSplines[[x]]$spline.deriv1)),
        # NA,
        # min(flFitRes$flFit2$flFittedSplines[[x]]$spline.deriv1$y))),
        # na.rm = TRUE))*1.05 if(mu.min2 >0)
        # mu.min2 <- 0 mu.max1 <-
        # suppressWarnings(max(sapply(1:length(flFitRes$flFit2$flFittedSplines),
        # function(x)
        # ifelse(all(is.na(flFitRes$flFit2$flFittedSplines[[x]]$spline.deriv1)),
        # NA,
        # max(flFitRes$flFit2$flFittedSplines[[x]]$spline.deriv1$y))),
        # na.rm = TRUE))*1.05 }
    }
    if (!is.null(out.dir))
        {
        wd <- out.dir
    } else
    {
        wd <- paste(
            getwd(), "/Report.fluorescence_", format(Sys.time(), "%Y%m%d_%H%M%S"),
            sep = ""
        )
    }
    if (is.null(out.nm))
        {
        out.nm <- paste(
            "/FluorescenceReport_", format(Sys.time(), "%Y%m%d_%H%M%S"),
            sep = ""
        )
    }
    dir.create(wd, showWarnings = FALSE)
    message("Render reports...")
    # for(i in 1:length(.libPaths())){ QurvE.ndx
    # <- grep('QurvE',
    # list.files(.libPaths()[i]))
    # if(length(QurvE.ndx)>0){ Report.wd <-
    # paste0(.libPaths()[i], '/QurvE') } }
    Report.wd <- paste0(
        "C:/Users/nicwir/Documents/DTU_Biosustain/Scripts_and_Modelling/curvE package/QurvE/inst/"
    )
    file <- paste0(Report.wd, "/Report_Fluorescence.Rmd")

    # Copy report files into temp directory
    report_path <- tempfile(fileext = ".Rmd")
    file.copy(file, report_path, overwrite = TRUE)

    if (all(
        c("pdf", "html") %in%
            format
    ))
        {
        format <- c("html_document", "pdf_document")
    } else if ("pdf" %in% format)
    {
        format <- "pdf_document"
    } else if ("html" %in% format)
    {
        format <- "html_document"
    } else {
      stop(
        "Please define a valid report format, either 'pdf', 'html', or c('pdf', 'html')."
      )
    }

      rmarkdown::render(
        report_path, output_format = format, output_dir = wd,
        output_file = out.nm, quiet = TRUE
      )

    message(paste0("Files saved in: '", wd, "'"))
    unlink(
        paste0(tempdir(), "/Plots"),
        recursive = TRUE
    )
    try(removeModal(), silent = TRUE)
    invisible(NULL)
}
#' Format font color for Markdown reports
#'
#' \code{colFmt} formats the input depending on PDF or HTML output to give colored text in reports.
#'
#' @param x A character string. The text to be colored.
#' @param color (Character) A color.
#' @return A LaTeX- or HTML-formatted string to assign a color to text based on the output format.
#' @keywords internal
#' @noRd
#'
colFmt <- function(x, color)
    {
    outputFormat <- knitr::opts_knit$get("rmarkdown.pandoc.to")
    if (outputFormat == "latex")
    {
        ret <- paste(
            "\\textcolor{", color, "}{", gsub("%", "\\\\%", gsub("_", "\\\\_", x)),
            "}", sep = ""
        )
    } else if (outputFormat == "html")
    {
        ret <- paste(
            "<font color='", color, "'>", x, "</font>",
            sep = ""
        )
    } else
    {
        ret <- x
    }
    return(ret)
}
