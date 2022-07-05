#' Read growth and fluorescence data in table format
#'
#' [growth.read_data] reads table files or R dataframe objects containing growth and fluorescence data and extracts datasets, sample and group information, performs blank correction and combines technical replicates.
#'
#' @param data.density An R dataframe object or a table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing density data.
#' In column format, the first three table rows contain
#' \enumerate{
#'    \item sample description
#'    \item replicate number (_optional_: followed by a letter to indicate technical replicates)
#'    \item concentration value (_optional_)
#' }
#' @param data.fluoro1 (optional) An R dataframe object or a table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing fluorescence data. Table layout must mimic that of \code{data.density}.
#' @param data.fluoro2 (optional) An R dataframe object or a table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing fluorescence data. Table layout must mimic that of \code{data.density}.
#' @param data.format (Character) "col" for samples in columns, or "row" for samples in rows. Default: ["col"]
#' @param csvsep (Character) separator used in CSV file (ignored for other file types).
#' @param dec (Character) decimal separator used in CSV, TSV and TXT files.
#' @param sheet (Numeric or Character) Number or name of a sheet in XLS or XLSX files (_optional_). Default: \code{";"}
#' @param subtract.blank (Logical) Shall blank values be subtracted from values within the same experiment ([TRUE], the default) or not ([FALSE]).
#'
#' @details
#' \figure{Data_layout.png}
#' @return An R list object of class \code{grodata} containing a time matrix, a data matrix, and an experimental design table. The \code{grodata} object can be directly used to run \code{growth.workflow} or, together with a \code{grofit.control} object in \code{growth.gcFit}, \code{growth.gcFitLinear}, \code{growth.gcFitModel}, \code{growth.gcFitSpline}, or \code{growth.gcBootSpline}
#' @export
#' @md
growth.read_data <- function(data.density, data.fluoro1 = NA, data.fluoro2 = NA, data.format = "col", csvsep = ";", dec = ".", sheet = 1, subtract.blank  = T)
{
  # Load density data
  if (!is.character(data.density)) {
    dat <- data.density
  } else {
    # Read table file
    dat <- read_file(data.density, csvsep=csvsep, dec=dec, sheet=sheet)
  }
  if(data.format == "col"){
    dat <- t(dat)
  }
  if(data.format == "col"){
    message("Sample data are stored in columns. If they are stored in row format, please run growth.read_data() with data.format = 'row'.")
  } else {
    message("Sample data are stored in rows. If they are stored in column format, please run growth.read_data() with data.format = 'col'.")
  }
  if(!(any(grepl("time", unlist(dat[,1]), ignore.case = TRUE)))){
    if(data.format == "col"){
      stop("Could not find 'time' in column 1 of data.density")
    } else {
      stop("Could not find 'time' in row 1 of data.density")
    }
  }
  # Load fluorescence 1 data
  if(length(data.fluoro1)>1){
    if (!is.character(data.fluoro1)) {
      fluoro1 <- data.fluoro1
    } else {
      # Read table file
      fluoro1 <- read_file(data.fluoro1, csvsep=csvsep, dec=dec, sheet=sheet)
    }
    if(data.format == "col"){
      fluoro1 <- t(fluoro1)
    }
    if(!(any(grepl("time", unlist(fluoro1[,1]), ignore.case = TRUE)))){
      if(data.format == "col"){
        stop("Could not find 'time' in column 1 of data.fluoro1")
      } else {
        stop("Could not find 'time' in row 1 of data.fluoro1")
      }
    }
  } else {
    fluoro1 <- NA
  }
  # Load fluorescence 2 data
  if(length(data.fluoro2)>1){
    if (!is.character(data.fluoro2)) {
      fluoro2 <- data.fluoro2
    } else {
      # Read table file
      fluoro2 <- read_file(data.fluoro2, csvsep=csvsep, dec=dec, sheet=sheet)
    }
    if(data.format == "col"){
      fluoro2 <- t(fluoro2)
    }
    if(!(any(grepl("time", unlist(fluoro1[,1]), ignore.case = TRUE)))){
      if(data.format == "col"){
        stop("Could not find 'time' in column 1 of data.fluoro2")
      } else {
        stop("Could not find 'time' in row 1 of data.fluoro2")
      }
    }
  } else {
    fluoro2 <- NA
  }

  # subtract blank
  if(subtract.blank){
    subtract_blank <- function(df){
      #test if more than one time entity is present
      time.ndx <- grep("time", unlist(df[,1]), ignore.case = TRUE)
      if(length(time.ndx)==1){
        blank.ndx <- grep("blank", df[1:nrow(df),1], ignore.case = T)
        if(length(blank.ndx)>0){
          blank <- rowMeans(apply(df[blank.ndx, 4:ncol(df)], 1, as.numeric), na.rm = T)
          df[(2:nrow(df))[!((2:nrow(df)) %in% blank.ndx)], 4:ncol(df)] <- apply(df[(2:nrow(df))[!((2:nrow(df)) %in% blank.ndx)], 4:ncol(df)], 1, as.numeric)-blank
        }
      } else { # identify different datasets based on the occurence of multiple 'time' entities
        # identify additional time entities
        blank.ndx <- grep("blank", df[(time.ndx[1]) : (time.ndx[2]-1),1], ignore.case = T)
        if(length(blank.ndx)>0){
          blank <- rowMeans(apply(df[blank.ndx, 4:ncol(df)], 1, as.numeric))
          df[((time.ndx[1] + 1):(time.ndx[2] - 1))[!(((time.ndx[1] + 1):(time.ndx[2] - 1)) %in% blank.ndx)], 4:ncol(df)] <-
              t(apply(df[((time.ndx[1] + 1):(time.ndx[2] - 1))[!(((time.ndx[1] + 1):(time.ndx[2] - 1)) %in% blank.ndx)], 4:ncol(df)], 1, as.numeric) - blank)
          for (i in 2:(length(time.ndx))){
            blank.ndx <- grep("blank", df[if (is.na(time.ndx[i + 1])) {
                (time.ndx[i] + 1):nrow(df)
              } else {
                (time.ndx[i] + 1):(time.ndx[i + 1] - 1)
              }, 1], ignore.case = T) + time.ndx[i]
            if(length(blank.ndx)>0){
              blank <- rowMeans(apply(df[blank.ndx, 4:ncol(df)], 1, as.numeric))

              df[if (is.na(time.ndx[i + 1])) {
                ((time.ndx[i] + 1):nrow(df))[!((time.ndx[i] + 1):nrow(df) %in% blank.ndx)]
              } else {
                ((time.ndx[i] + 1):(time.ndx[i + 1] - 1))[!(((time.ndx[i] + 1):(time.ndx[i + 1] - 1)) %in% blank.ndx)]
              }, 4:ncol(df)] <-
                t(apply(df[if (is.na(time.ndx[i + 1])) {
                  ((time.ndx[i] + 1):nrow(df))[!((time.ndx[i] + 1):nrow(df) %in% blank.ndx)]
                } else {
                  ((time.ndx[i] + 1):(time.ndx[i + 1] - 1))[!(((time.ndx[i] + 1):(time.ndx[i + 1] - 1)) %in% blank.ndx)]
                }, 4:ncol(df)], 1, as.numeric) - blank)
            }
          } # end of for (i in 2:(length(time.ndx)))
        } # if(length(blank.ndx)>0){
      } # end of else {}
      return(df)
    }
    dat <- subtract_blank(dat)
    if(length(data.fluoro1)>1)     fluoro1 <- subtract_blank(df=fluoro1)
    if(length(data.fluoro2)>1)     fluoro2 <- subtract_blank(df=fluoro2)
  }

  ### Combine technical replicates
  combine_techrep <- function(dat){
    sample_names <- as.character(paste0(dat[2:nrow(dat),1], "...", dat[2:nrow(dat),2], "___", dat[2:nrow(dat),3]))
    conditions <-
      unique(gsub("\\.\\.\\..+___", "___", sample_names))
    # remove time from samples in case of several time entities
    time.ndx <- grep("time", unlist(dat[,1]), ignore.case = TRUE)
    if(length(time.ndx)>1){
      conditions <- conditions[-grep("time", gsub("___.+", "", conditions), ignore.case = T)]
    }
    # remove blanks from conditions
    blankcond.ndx <- grep("blank", gsub("___.+", "", conditions), ignore.case = TRUE)
    if(length(blankcond.ndx)>1){
      conditions <- conditions[-blankcond.ndx]
    }

    remove <- c()
    for(i in 1:length(conditions)){
      ndx.cond <-  which(gsub("\\.\\.\\..+___", "___", sample_names) %in% conditions[i])
      name <- dat[ndx.cond[1]+1,1]
      conc <- dat[ndx.cond[1]+1,3]
      tech.rep <- suppressWarnings(as.numeric(unique(gsub("[[:alpha:]]___.+", "", gsub(".+\\.\\.\\.", "", sample_names[ndx.cond])))))
      tech.rep <- tech.rep[!is.na(tech.rep)]
    if(length(tech.rep)>1){
        for(j in 1:length(tech.rep)){
          ndx.rep <- ndx.cond[which(gsub("[[:alpha:]]___.+", "", gsub(".+\\.\\.\\.", "", sample_names[ndx.cond])) %in% tech.rep[j])]
          if(length(ndx.rep)>1){
            values <- apply(dat[ndx.rep+1, 4:ncol(dat)], 1, as.numeric)
            means <- rowMeans(values)
            dat[ndx.rep[1]+1, 4:ncol(dat)] <- means
            dat[ndx.rep[1]+1, 2] <- as.numeric(tech.rep[j])
            remove <- c(remove, ndx.rep[-1]+1)
          } else {
            dat[ndx.rep[1]+1, 2] <- as.numeric(tech.rep[j])
          }
        }
      }
    }
    if(length(remove)>1){
      dat <- dat[-remove,]
    }
    return(dat)
  }
  dat <- combine_techrep(dat)
  if(length(data.fluoro1)>1)     fluoro1 <- combine_techrep(dat=fluoro1)
  if(length(data.fluoro2)>1)     fluoro2 <- combine_techrep(dat=fluoro2)

  # remove blank columns from dataset
  remove_blank <- function(dat){
    blank.ndx <- grep("blank", dat[1:nrow(dat),1], ignore.case = T)
    if(length(blank.ndx)>1){
      dat <- dat[-blank.ndx, ]
    }
    return(dat)
  }
  dat <- remove_blank(dat)
  if(length(data.fluoro1)>1)     fluoro1 <- remove_blank(dat=fluoro1)
  if(length(data.fluoro2)>1)     fluoro2 <- remove_blank(dat=fluoro2)

  # Remove columns with NA measurements in all samples
  dat <- dat[, which(unlist(lapply(4:ncol(dat), function(x)!all(is.na(dat[2:nrow(dat),x])))))]
  # Create time matrix
  time.ndx <- grep("time", unlist(dat[,1]), ignore.case = TRUE)
  if(length(time.ndx)==1){
    time <- as.numeric(unlist(dat[time.ndx[1],4:ncol(dat)]))
    t.mat <- data.matrix(data.frame(matrix(
      data = rep(time, nrow(dat)-1),
      nrow = nrow(dat)-1,
      byrow = T
    )))
  } else { # identify different datasets based on the occurence of multiple 'time' entities
    time <- list()
    time[[1]] <- as.numeric(unlist(dat[time.ndx[1],4:ncol(dat)]))
    t.mat <- data.matrix(data.frame(matrix(
      data = rep(time[[1]], time.ndx[2]-time.ndx[1]-1),
      nrow = time.ndx[2]-time.ndx[1]-1,
      byrow = T
    )))
    for (i in 2:(length(time.ndx))){
      time[[i]] <- as.numeric(unlist(dat[time.ndx[i],4:ncol(dat)]))
      t.mat <- rbind(t.mat,
                     data.matrix(data.frame(
                       matrix(
                         data = rep(time[[i]], times = if (is.na(time.ndx[i + 1])) {
                           nrow(dat) - time.ndx[i]
                         } else {
                           time.ndx[i + 1] - time.ndx[i] - 1
                         }),
                         nrow = if (is.na(time.ndx[i + 1])) {
                           nrow(dat) - time.ndx[i]
                         } else {
                           time.ndx[i + 1] - time.ndx[i] - 1
                         },
                         byrow = T)
                     )
                     )
      )
    } # end of for (i in 2:(length(time.ndx)))
  } # end of else {}

  # Create data matrix for density values
  create_datmat <- function(dat, time.ndx){
    if(length(time.ndx)==1){
      dat.mat <- data.frame(dat[(time.ndx[1]+1):nrow(dat),])
    } else { # identify different datasets based on the occurence of multiple 'time' entities
      dat.mat <- data.frame(dat[(time.ndx[1]+1) : (time.ndx[2]-1), ])
      for (i in 2:(length(time.ndx))){
        dat.mat <- rbind(dat.mat,
                         data.frame(dat[ if (is.na(time.ndx[i + 1])) {
                           (time.ndx[i]+1) : nrow(dat)
                         } else {
                           (time.ndx[i]+1) : (time.ndx[i+1] - 1)
                         } , ])
        )
      } # end of for (i in 2:(length(time.ndx)))
    } # end of else {}
    return(dat.mat)
  }
  dat.mat <- create_datmat(dat, time.ndx=time.ndx)
  if(length(data.fluoro1)>1){fluoro1.mat <- create_datmat(dat=fluoro1, time.ndx=time.ndx)}else{fluoro1.mat <- NA}
  if(length(data.fluoro2)>1){fluoro2.mat <- create_datmat(dat=fluoro2, time.ndx=time.ndx)}else{fluoro2.mat <- NA}

  colnames(dat.mat)[1:3] <- c("condition", "replicate", "concentration")
  if(length(data.fluoro1)>1)     colnames(fluoro1.mat)[1:3] <- c("condition", "replicate", "concentration")
  if(length(data.fluoro2)>1)    colnames(fluoro2.mat)[1:3] <- c("condition", "replicate", "concentration")

  label <- unlist(lapply(1:nrow(dat.mat), function(x) paste(dat.mat[x,1], dat.mat[x,2], dat.mat[x,3], sep = " | ")))
  condition <- dat.mat[, 1]
  replicate <- dat.mat[, 2]
  concentration <- dat.mat[, 3]

  expdesign <- data.frame(label, condition, replicate, concentration, check.names = FALSE)

  dat.mat <- as.data.frame(unclass(dat.mat), stringsAsFactors = TRUE)

  dataset <- list("time" = t.mat,
                  "density" = dat.mat,
                  "fluorescence1" = fluoro1.mat,
                  "fluorescence2" = fluoro2.mat,
                  "expdesign" = expdesign)

  class(dataset) <- "grodata"
  invisible(dataset)
  }

#' Create a \code{grofit.control} object.
#'
#' A \code{grofit.control} object is required to perform various computations on \code{grodata} objects created with \code{growth.read_data()}. Such object is created automatically as part of \code{growth.workflow()}.
#'
#' @param neg.nan.act (Logical) Indicates whether the program should stop when negative growth values or NA values appear (\code{TRUE}). Otherwise, the program removes these values silently (\code{FALSE}). Improper values may be caused by incorrect data or input errors. Default: \code{FALSE}.
#' @param clean.bootstrap (Logical) Determines if negative values which occur during bootstrap should be removed (TRUE) or kept (FALSE). Note: Infinite values are always removed. Default: TRUE.
#' @param suppress.messages (Logical) Indicates wether grofit messages (information about current growth curve, EC50 values etc.) should be displayed (\code{FALSE}) or not (\code{TRUE}). This option is meant to speed up the processing of high throuput data. Note: warnings are still displayed. Default: \code{FALSE}.
#' @param fit.opt (Character or character vector) Indicates whether the program should perform a linear regression (\code{"l"}), model fit (\code{"m"}), spline fit (\code{"s"}), or all (\code{"a"}). Combinations can be freely chosen by providing a character vector, e.g. \code{fit.opt = c("l", "s")} Default: \code{"a"}.
#' @param min.density (Numeric) Indicate whether only values above a certain threshold should be considered for linear regressions or spline fits.
#' @param log.x.gc (Logical) Indicates whether _ln(x+1)_ should be applied to the time data for _linear_ and _spline_ fits. Default: \code{FALSE}.
#' @param log.y.gc (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ and spline fits. Default: \code{TRUE}
#' @param log.y.model (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _model_ fits. Default: \code{TRUE}
#' @param interactive (Logical) Controls whether the fit of each growth curve and method is controlled manually by the user. If \code{TRUE}, each fit is visualized in the _Plots_ pane and the user can adjust fitting parameters and confirm the reliability of each fit per sample. Default: \code{TRUE}.
#' @param nboot.gc (Numeric) Number of bootstrap samples used for nonparametric growth curve fitting with \code{growth.gcBootSpline()}. Use \code{nboot.gc = 0} to disable the bootstrap. Default: \code{0}
#' @param smooth.gc (Numeric) Parameter describing the smoothness of the spline fit; usually (not necessary) within (0;1]. \code{smooth.gc=NULL} causes the program to query an optimal value via cross validation techniques. Especially for datasets with few data points the option NULL might cause a too small smoothing parameter. This can result a too tight fit that is susceptible to measurement errors (thus overestimating growth rates) or produce an error in \code{smooth.spline} or lead to an overestimation. The usage of a fixed value is recommended for reproducible results across samples. See \code{?smooth.spline} for further details. Default: \code{0.55}
#' @param model.type (Character) Vector providing the names of the parametric models which should be fitted to the data. Default: \code{c("gompertz", "logistic", "gompertz.exp", "richards")}.
#' @param have.atleast (Numeric) Minimum number of different values for the response parameter one should have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: \code{6}.
#' @param parameter (Character or numeric) The response parameter in the output table which should be used for creating a dose response curve. See \code{drFit} or \code{?summary.gcFit} for further details. Default: \code{"mu.linfit"}, which represents the maximum slope of the linear regression. Typical options include: \code{"mu.linfit"}, \code{"lambda.linfit"}, \code{"dY.linfit"}, \code{"mu.spline"}, and \code{"dY.spline"}.
#' @param smooth.dr (Numeric) Smoothing parameter used in the spline fit by smooth.spline during dose response curve estimation. Usually (not necessesary) in (0; 1]. See documentation of smooth.spline for further details. Default: \code{NULL}.
#' @param log.x.dr (Logical) Indicates whether \code{ln(x+1)} should be applied to the concentration data of the dose response curves. Default: \code{FALSE}.
#' @param log.y.dr (Logical) Indicates whether \code{ln(y+1)} should be applied to the response data of the dose response curves. Default: \code{FALSE}.
#' @param nboot.dr (Numeric) Defines the number of bootstrap samples for EC50 estimation. Use \code{nboot.dr = 0} to disable bootstrapping. Default: \code{0}.
#'
#' @export
#'
growth.control <-
  function (neg.nan.act = FALSE,
            clean.bootstrap = TRUE,
            suppress.messages = FALSE,
            fit.opt = "a",
            min.density = NULL,
            log.x.gc = FALSE,
            log.y.gc = TRUE,
            log.y.model = FALSE,
            interactive = TRUE,
            nboot.gc = 0,
            smooth.gc = 0.55,
            model.type = c("logistic",
                           "richards", "gompertz", "gompertz.exp"),
            have.atleast = 6, # Minimum number of different values for the response parameter one shoud have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: 6.
            parameter = "mu.linfit", # parameter used for creating dose response curve. # 34 is µ determined with spline fit
            smooth.dr = NULL,
            log.x.dr = FALSE,
            log.y.dr = FALSE,
            nboot.dr = 0)
{
  if ((is.character(fit.opt) == FALSE))
    stop("value of fit.opt must be character and of one element")
  if (is.character(model.type) == FALSE)
    stop("value of model.type must be character")
  if ((is.logical(neg.nan.act) == FALSE) | (length(neg.nan.act) !=
                                            1))
    stop("value of neg.nan.act must be logical and of one element")
  if ((is.logical(clean.bootstrap) == FALSE) | (length(clean.bootstrap) !=
                                                1))
    stop("value of clean.bootstrap must be logical and of one element")
  if ((is.logical(suppress.messages) == FALSE) | (length(suppress.messages) !=
                                                  1))
    stop("value of suppress.messages must be logical and of one element")
  if ((is.logical(log.x.gc) == FALSE) | (length(log.x.gc) !=
                                         1))
    stop("value of log.x.gc must be logical and of one element")
  if ((is.logical(log.y.gc) == FALSE) | (length(log.y.gc) !=
                                         1))
    stop("value of log.y.gc must be logical and of one element")
  if ((is.logical(interactive) == FALSE) | (length(interactive) !=
                                            1))
    stop("value of interactive must be logical and of one element")
  if ((is.logical(log.x.dr) == FALSE) | (length(log.x.dr) !=
                                         1))
    stop("value of log.x.dr must be logical and of one element")
  if ((is.logical(log.y.dr) == FALSE) | (length(log.y.dr) !=
                                         1))
    stop("value of log.y.dr must be logical and of one element")
  if ((is.numeric(nboot.gc) == FALSE) | (length(nboot.gc) !=
                                         1) | (nboot.gc < 0))
    stop("value of nboot.gc must be numeric (>=0) and of one element")
  if ((is.numeric(have.atleast) == FALSE) | (length(have.atleast) !=
                                             1) | (have.atleast < 6))
    stop("value of have.atleast must be numeric (>=6) and of one element")
  if (((is.character(parameter) == FALSE) && (is.numeric(parameter) == FALSE)) | (length(parameter) !=
                                          1))
    stop("value of parameter must be a string or numeric and of one element")
  if ((is.numeric(nboot.dr) == FALSE) | (length(nboot.dr) !=
                                         1) | (nboot.dr < 0))
    stop("value of nboot.dr must be numeric (>=0) and of one element")
  if (((is.numeric(smooth.gc) == FALSE) && (is.null(smooth.gc) ==
                                            FALSE)))
    stop("value of smooth.gc must be numeric or NULL")
  if (((is.numeric(smooth.dr) == FALSE) && (is.null(smooth.dr) ==
                                            FALSE)))
    stop("value of smooth.dr must be numeric or NULL")

  parameters.opt <- c('TestId', 'AddId', 'concentration', 'reliability_tag', 'used.model', 'log.x',
    'log.y', 'nboot.gc', 'mu.linfit', 'lambda.linfit', 'stdmu.linfit', 'dY.linfit',
    'A.linfit', 'tmu.start.linfit', 'tmu.end.linfit', 'r2mu.linfit',
    'reliable_fit.linfit', 'mu.model', 'lambda.model', 'A.model', 'integral.model',
    'parameter_nu.model', 'parameter_alpha.model', 'parameter_t_shift.model',
    'stdmu.model', 'stdlambda.model', 'stdA.model', 'reliable_fit.model',
    'ci90.mu.model.lo', 'ci90.mu.model.up', 'ci90.lambda.model.lo',
    'ci90.lambda.model.up', 'ci90.A.model.lo', 'ci90.A.model.up', 'ci95.mu.model.lo',
    'ci95.mu.model.up', 'ci95.lambda.model.lo', 'ci95.lambda.model.up',
    'ci95.A.model.lo', 'ci95.A.model.up', 'mu.spline', 'lambda.spline', 'y0.spline',
    'A.spline', 'dY.spline', 'integral.spline', 'reliable_fit.spline', 'smooth.spline',
    'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt', 'stdmu.bt', 'stdlambda.bt', 'stdA.bt',
    'stdintegral.bt', 'reliable_fit.bt', 'ci90.mu.bt.lo', 'ci90.mu.bt.up',
    'ci90.lambda.bt.lo', 'ci90.lambda.bt.up', 'ci90.A.bt.lo', 'ci90.A.bt.up',
    'ci90.integral.bt.lo', 'ci90.integral.bt.up', 'ci95.mu.bt.lo', 'ci95.mu.bt.up',
    'ci95.lambda.bt.lo', 'ci95.lambda.bt.up', 'ci95.A.bt.lo', 'ci95.A.bt.up',
    'ci95.integral.bt.lo', 'ci95.integral.bt.up')
  parameter.in <- parameter
  if(is.character(parameter)){
    parameter <- match(parameter, parameters.opt)
    if(is.na(parameter)){
      stop(paste0(parameter.in, " is not a valid parameter for the dose-response analysis. See ?growth.drFit for possible options"))
    }
  }
  grofit.control <- list(neg.nan.act = neg.nan.act, clean.bootstrap = clean.bootstrap,
                         suppress.messages = suppress.messages, fit.opt = fit.opt, min.density = min.density,
                         log.x.gc = log.x.gc, log.y.gc = log.y.gc, log.y.model=log.y.model, interactive = interactive,
                         nboot.gc = round(nboot.gc), smooth.gc = smooth.gc, smooth.dr = smooth.dr,
                         have.atleast = round(have.atleast), parameter = round(parameter),
                         log.x.dr = log.x.dr, log.y.dr = log.y.dr, nboot.dr = round(nboot.dr),
                         model.type = model.type)
  class(grofit.control) <- "grofit.control"
  grofit.control
}

#' Run a complete growth curve analysis and dose-reponse analysis workflow.
#'
#' \code{grofit.workflow()} runs \code{growth.control()} to create a \code{grofit.control} object and then performs all computational fitting operations based on the user input. Finally, if desired, a final report is created in PDF and HTML format that summarizes all results obtained.
#'
#' @param time (optional) A matrix containing time values for each sample.
#' @param data Either a \code{grodata} object created with \code{growth.read_data()}, a list containing a \code{'time'} matrix as well as \code{'density'} and, if appropriate, \code{'fluorescence1'} and \code{'fluorescence2'} dataframes, or a dataframe containing growth data (if a \code{time} matrix is provided as separate argument).
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param ec50 (Logical) Perform dose-response analysis (\code{TRUE}) or not (\code{FALSE}).
#' @param mean.grp (\code{"all"}, a string vector, or a list of string vectors) Define groups to combine into common plots in the final report based on sample identifiers (if \code{report == TRUE}). Partial matches with sample/group names are accepted.
#' @param mean.conc (A numeric vector, or a list of numeric vectors) Define concentrations to combine into common plots in the final report (if \code{report == TRUE}).
#' @param neg.nan.act (Logical) Indicates whether the program should stop when negative growth values or NA values appear (\code{TRUE}). Otherwise, the program removes these values silently (\code{FALSE}). Improper values may be caused by incorrect data or input errors. Default: \code{FALSE}.
#' @param clean.bootstrap (Logical) Determines if negative values which occur during bootstrap should be removed (TRUE) or kept (FALSE). Note: Infinite values are always removed. Default: TRUE.
#' @param suppress.messages (Logical) Indicates wether grofit messages (information about current growth curve, EC50 values etc.) should be displayed (\code{FALSE}) or not (\code{TRUE}). This option is meant to speed up the processing of high throuput data. Note: warnings are still displayed. Default: \code{FALSE}.
#' @param fit.opt (Character or character vector) Indicates whether the program should perform a linear regression (\code{"l"}), model fit (\code{"m"}), spline fit (\code{"s"}), or all (\code{"a"}). Combinations can be freely chosen by providing a character vector, e.g. \code{fit.opt = c("l", "s")} Default: \code{"a"}.
#' @param min.density (Numeric) Indicate whether only values above a certain threshold should be considered for linear regressions or spline fits.
#' @param log.x.gc (Logical) Indicates whether _ln(x+1)_ should be applied to the time data for _linear_ and _spline_ fits. Default: \code{FALSE}.
#' @param log.y.gc (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ and spline fits. Default: \code{TRUE}
#' @param log.y.model (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _model_ fits. Default: \code{TRUE}
#' @param lin.h (Numeric) Manually define the size of the sliding window used in \code{growth.gcFitLinear()}. If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{growth.gcFitLinear()}.
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for \code{growth.gcFitLinear()}.
#' @param lin.dY (Numeric) Threshold for the minimum fraction of density increase a linear regression window should cover. Default: 0.05 (5%).
#' @param interactive (Logical) Controls whether the fit of each growth curve and method is controlled manually by the user. If \code{TRUE}, each fit is visualized in the _Plots_ pane and the user can adjust fitting parameters and confirm the reliability of each fit per sample. Default: \code{TRUE}.
#' @param nboot.gc (Numeric) Number of bootstrap samples used for nonparametric growth curve fitting with \code{growth.gcBootSpline()}. Use \code{nboot.gc = 0} to disable the bootstrap. Default: \code{0}
#' @param smooth.gc (Numeric) Parameter describing the smoothness of the spline fit; usually (not necessary) within (0;1]. \code{smooth.gc=NULL} causes the program to query an optimal value via cross validation techniques. Especially for datasets with few data points the option NULL might cause a too small smoothing parameter. This can result a too tight fit that is susceptible to measurement errors (thus overestimating growth rates) or produce an error in \code{smooth.spline} or lead to an overestimation. The usage of a fixed value is recommended for reproducible results across samples. See \code{?smooth.spline} for further details. Default: \code{0.55}
#' @param model.type (Character) Vector providing the names of the parametric models which should be fitted to the data. Default: \code{c("gompertz", "logistic", "gompertz.exp", "richards")}.
#' @param have.atleast (Numeric) Minimum number of different values for the response parameter one should have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: \code{6}.
#' @param parameter (Character or numeric) The response parameter in the output table which should be used for creating a dose response curve. See \code{drFit} or \code{?summary.gcFit} for further details. Default: \code{"mu.linfit"}, which represents the maximum slope of the linear regression. Typical options include: \code{"mu.linfit"}, \code{"lambda.linfit"}, \code{"dY.linfit"}, \code{"mu.spline"}, and \code{"dY.spline"}.
#' @param smooth.dr (Numeric) Smoothing parameter used in the spline fit by smooth.spline during dose response curve estimation. Usually (not necessesary) in (0; 1]. See documentation of smooth.spline for further details. Default: \code{NULL}.
#' @param log.x.dr (Logical) Indicates whether \code{ln(x+1)} should be applied to the concentration data of the dose response curves. Default: \code{FALSE}.
#' @param log.y.dr (Logical) Indicates whether \code{ln(y+1)} should be applied to the response data of the dose response curves. Default: \code{FALSE}.
#' @param nboot.dr (Numeric) Defines the number of bootstrap samples for EC50 estimation. Use \code{nboot.dr = 0} to disable bootstrapping. Default: \code{0}.
#' @param report (Logical) Create a PDF and HTML report after running all computations (\code{TRUE}) or not (\code{FALSE}).
#' @param out.dir {Character or \code{NULL}} Define the name of a folder in which all result files are stored. If \code{NULL}, the folder will be named with a combination of "Report.growth_" and the current date and time.
#' @param export (Logical) Export all figures created in the report as separate PNG and PDF files (\code{TRUE}) or not (\code{FALSE}).
#'
#' @return A \code{grofit} object that contains all computation results, compatible with various plotting functions of the QurvE package and with \code{growth.report()}.
#' @export
#' @importFrom ggplot2 aes annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
growth.workflow <- function (time,
                             data,
                             t0 = 0,
                             ec50 = FALSE,
                             mean.grp = "all",
                             mean.conc = NA,
                             neg.nan.act = FALSE,
                             clean.bootstrap = TRUE,
                             suppress.messages = FALSE,
                             fit.opt = "a",
                             min.density = NA,
                             log.x.gc = FALSE,
                             log.y.gc = TRUE,
                             log.y.model = FALSE,
                             lin.h = NULL,
                             lin.R2 = 0.98,
                             lin.RSD = 0.02,
                             lin.dY = 0.05,
                             interactive = TRUE,
                             nboot.gc = 0,
                             smooth.gc = 0.55,
                             model.type = c("logistic",
                                            "richards", "gompertz", "gompertz.exp"),
                             have.atleast = 6,
                             parameter = 34,
                             smooth.dr = NULL,
                             log.x.dr = FALSE,
                             log.y.dr = FALSE,
                             nboot.dr = 0,
                             report = TRUE,
                             out.dir = NULL,
                             export = FALSE
)
{
  if(!(class(data)=="list") && !(class(data)=="grodata")){
    if (is.numeric(as.matrix(time)) == FALSE)
      stop("Need a numeric matrix for 'time'")
    if (is.numeric(as.matrix(data[-1:-3])) == FALSE)
      stop("Need a numeric matrix for 'data'")
    if (is.logical(ec50) == FALSE)
      stop("Need a logical value for 'ec50'")
  } else {
    time <- data$time
    if(!is.null(data$expdesign)) expdesign <- data$expdesign
    if(!is.null(data$fluorescence1)) fluorescence1 <- data$fluorescence1
    if(!is.null(data$fluorescence2)) fluorescence2 <- data$fluorescence2
    data <- data$density
  }
  control <- growth.control(neg.nan.act = neg.nan.act, clean.bootstrap = clean.bootstrap,
                            suppress.messages = suppress.messages, fit.opt = fit.opt, min.density = min.density,
                            log.x.gc = log.x.gc, log.y.gc = log.y.gc, log.y.model = log.y.model, interactive = interactive,
                            nboot.gc = round(nboot.gc), smooth.gc = smooth.gc, smooth.dr = smooth.dr,
                            have.atleast = round(have.atleast), parameter = parameter,
                            log.x.dr = log.x.dr, log.y.dr = log.y.dr, nboot.dr = round(nboot.dr),
                            model.type = model.type)
  nboot.gc <- control$nboot.gc
  nboot.dr <- control$nboot.dr
  out.gcFit <- NA
  out.drFit <- NA

  # /// fit of growth curves -----------------------------------
  out.gcFit <- growth.gcFit(time, data, control, t0, lin.h, lin.R2, lin.RSD, lin.dY)

  # /// Estimate EC50 values
  if (ec50 == TRUE) {
    out.drFit <- growth.drFit(summary.gcFit(out.gcFit), control)
    EC50.table <- out.drFit$drTable
    boot.ec <- out.drFit$boot.ec
  }
  # ///
  grofit <- list(time = time, data = data, gcFit = out.gcFit,
                 drFit = out.drFit, expdesign = expdesign, control = control)
  class(grofit) <- "grofit"

  if(!is.null(out.dir)){
    wd <- paste0(getwd(), "/", out.dir)
  } else {
    wd <- paste(getwd(), "/Report.growth_", format(Sys.time(),
                                            "%Y%m%d_%H%M%S"), sep = "")
  }
  dir.create(wd, showWarnings = F)

  gcTable <- data.frame(apply(grofit[["gcFit"]][["gcTable"]],2,as.character))
  res.table.gc <- Filter(function(x) !all(is.na(x)),gcTable)
  utils::write.table(res.table.gc, paste(wd, "results.gc.txt",
                                  sep = "/"), row.names = FALSE, sep = "\t")
  cat(paste0("Results of growth fit analysis saved as tab-delimited text file in:\n",
             wd, "/results.gc.txt\n"))
  if (ec50 == TRUE) {
    res.table.dr <- Filter(function(x) !all(is.na(x)),EC50.table)
    utils::write.table(res.table.dr, paste(wd, "results.dr.txt",
                                           sep = "/"), row.names = FALSE, sep = "\t")
    cat(paste0("Results of EC50 analysis saved as tab-delimited in:\n",
               wd, "/results.dr.txt\n"))
  } else {
    res.table.dr <- NULL
  }
  if(report == TRUE){
    growth.report(grofit, report.dir = gsub(paste0(getwd(), "/"), "", wd), res.table.gc=res.table.gc,
                  res.table.dr=res.table.dr, ec50=ec50, t0 = t0, mean.grp = mean.grp, mean.conc = mean.conc,
                  export = export)
  }

  grofit
}

#' Create a PDF and HTML report with results from a growth curve analysis workflow
#'
#' \code{growth.report()} requires a \code{grofit} object and creates a report in PDF and HTML format that summarizes all results obtained.
#'
#' @param grofit A \code{grofit} object created with \code{growth.workflow()}.
#' @param report.dir (Character) The path or name of the folder in which the report files are created.  If \code{NULL}, the folder will be named with a combination of "Report.growth_" and the current date and time.
#' @param ... Further arguments passed to create a report. Currently required:
#' \itemize{
#'    \item \code{res.table.gc}: The table exported as 'results.gc.txt' by \code{growth.workflow()} or \code{growth.gcFit()}.
#'    \item \code{res.table.dr] (if \code{ec50 = TRUE})  The table exported as 'results.dr.txt' by \code{growth.workflow()} or \code{growth.gcFit()}.
#'    \item \code{ec50}: \code{TRUE} or \code{FALSE}: Was a dose-response analysis performed in \code{growth.workflow()} or \code{growth.gcFit()}.
#'    \item \code{t0}: The minimum time value used to run \code{growth.workflow()} or \code{growth.gcFit()}.
#'    \item \code{mean.grp}: Define groups to combine into common plots in the report based on sample identifiers. Partial matches with sample/group names are accepted. Can be \code{"all"}, a string vector, or a list of string vectors.
#'    \item \code{mean.conc}: Define concentrations to combine into common plots in the  report. Can be a numeric vector, or a list of numeric vectors.
#'    \item \code{export}: Shall all plots generated in the report be exported as individual PDF and PNG files \code{TRUE} or not \code{FALSE}?
#' }
#'
#' @export
#' @importFrom ggplot2 aes annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
#' @importFrom doParallel registerDoParallel
#' @import foreach
#' @import kableExtra
#' @import knitr
#' @include general_misc_utils.R
growth.report <- function(grofit, report.dir = NULL, ...)
  {
  # results an object of class grofit
  if(class(grofit) != "grofit") stop("grofit needs to be an object created with growth.workflow().")

  args <- list(...)
  for(i in 1:length(args)){
    assign(names(args)[i], args[[i]])
  }
  gcFit <- grofit$gcFit
  drFit <- grofit$data
  control <- grofit$control
  time <- grofit$gcFit$raw.time
  data <- grofit$gcFit$raw.data
  if(!is.null(report.dir)){
    wd <- paste0(getwd(), "/", report.dir)
  } else {
    wd <- paste(getwd(), "/Report.growth_", format(Sys.time(),
                                                   "%Y%m%d_%H%M%S"), sep = "")
  }
  message("Save RData object")
  save(grofit, file = paste(wd, "results.RData", sep = "/"))
  message("Render reports...")

  dir.create(wd, showWarnings = F)
  for(i in 1:length(.libPaths())){
    QurvE.ndx <- grep("QurvE", list.files(.libPaths()[i]))
    if(length(QurvE.ndx)>0){
      Report.wd <- paste0(.libPaths()[i], "/QurvE")
    }
  }
  file <- paste0(Report.wd, "/Report_Growth.Rmd")
  rmarkdown::render(file, output_format = "all", output_dir = wd,
                    quiet = TRUE)
  message(paste0("Files saved in: '", wd, "'"))
  unlink(paste0(tempdir(), "/Plots"), recursive = TRUE)
}

#' Perform a growth curve analysis on all samples in a provided dataset
#'
#' \code{growth.gcFit} performs all computational growth fitting operations based on the user input.
#'
#' @param time (optional) A matrix containing time values for each sample.
#' @param data Either a \code{grodata} object created with \code{growth.read_data()}, a list containing a \code{'time'} matrix as well as \code{'density'} and, if appropriate, \code{'fluorescence1'} and \code{'fluorescence2'} dataframes, or a dataframe containing growth data (if a \code{time} matrix is provided as separate argument).
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param control A \code{grofit.control} object created with
#' @param lin.h (Numeric) Manually define the size of the sliding window used in \code{growth.gcFitLinear()}. If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{growth.gcFitLinear()}.
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for \code{growth.gcFitLinear()}.
#' @param lin.dY (Numeric) Threshold for the minimum fraction of density increase a linear regression window should cover. Default: 0.05 (5%).
#'
#' @return A \code{gcFit} object that contains all growth fitting results, compatible with various plotting functions of the QurvE package.
#'
#' @export
#' @importFrom ggplot2 aes annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
growth.gcFit <- function(time, data, control=growth.control(), t0 = 0, lin.h = NULL, lin.R2 = 0.95, lin.RSD = 0.05, lin.dY = 0.05)
{
  # /// check if start density values are above min.density in all samples
  max.density <- unlist(lapply(1:nrow(data), function (x) max(as.numeric(as.matrix(data[x,-1:-3]))[!is.na(as.numeric(as.matrix(data[x,-1:-3])))])))
  if(is.numeric(control$min.density) && control$min.density != 0){
    if(!is.na(control$min.density) && all(as.numeric(max.density) < control$min.density)){
      stop(paste0("The chosen global start density value (min.density) is larger than every value in your dataset.\nThe maximum start value in your dataset is: ",
                  max(as.numeric(max.density))))
    }
  }
  # /// check input parameters
  if (is(control)!="grofit.control") stop("control must be of class grofit.control!")

  # /// check number of datasets
  if ( (dim(time)[1])!=(dim(data)[1]) ) stop("gcFit: Different number of datasets in data and time")

  # /// check fitting options
  if (!all(control$fit.opt %in% c("s","m","a", "l"))){
    options(warn=1)
    warning("fit.opt must contain 's', 'm', 'l', or 'a'. Changed to 'a' (all fit methods)!")
    fit.opt="a"
    options(warn=0)
  }

  # /// Initialize some parameters
  out.table       <- NULL
  used.model      <- NULL
  fitpara.all     <- list()
  fitnonpara.all  <- list()
  fitlinear.all <- list()
  boot.all        <- list()
  fitted.param    <- NULL
  fitted.nonparam <- NULL
  bootstrap.param <- NULL
  reliability_tag_linear <- NA
  reliability_tag_param <- NA
  reliability_tag_nonpara <- NA

  # /// loop over all wells
  for (i in 1:dim(data)[1]){
    # /// conversion, to handle even data.frame inputs
    acttime    <-
      as.numeric(as.matrix(time[i, ]))[!is.na(as.numeric(as.matrix(time[i, ])))][!is.na(as.numeric(as.matrix((data[i, -1:-3]))))]
    actwell <-
      as.numeric(as.matrix((data[i, -1:-3])))[!is.na(as.numeric(as.matrix(time[i, ])))][!is.na(as.numeric(as.matrix((data[i, -1:-3]))))]

    gcID    <- as.matrix(data[i,1:3])
    wellname <- paste(as.character(data[i,1]), as.character(data[i,2]),as.character(data[i,3]), sep=" | ")
    if ((control$suppress.messages==FALSE)){
      cat("\n\n")
      cat(paste("=== ", as.character(i), ". [", wellname, "] growth curve =================================\n", sep=""))
      cat("----------------------------------------------------\n")
    }
    # /// Linear regression on log-transformed data
    if (("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
      fitlinear          <- growth.gcFitLinear(acttime, actwell, gcID = gcID, h = lin.h, control = control, t0 = t0, R2 = lin.R2, RSD = lin.RSD, fit.dY = lin.dY)
      fitlinear.all[[i]] <- fitlinear
    }
    else{
      # /// generate empty object
      fitlinear          <- list(raw.time = acttime,
                                 raw.data = actwell,
                                 gcID = gcID,
                                 FUN = NA, fit = NA,
                                 par = c(y0 = NA, y0_lm = NA, mumax = NA, lag = NA, tmax_start = NA, tmax_end = NA),
                                 ndx = NA,
                                 rsquared = NA,
                                 control = control,
                                 fitFlag = FALSE)
      class(fitlinear)   <- "gcFitLinear"
      fitlinear.all[[i]] <- fitlinear
    }
    # /// plot linear fit
    if ((control$interactive == TRUE)) {
      if (("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt)) {
          answer_satisfied <- "n"
          reliability_tag_linear <- NA
          while ("n" %in% answer_satisfied) {
            try(plot(fitlinear, log = "y"))
            mtext(side = 3, line = 0, adj = 0,
                  outer = F,
                  cex = 1,
                  wellname)
            answer_satisfied <- readline("Are you satisfied with the linear fit (y/n)?\n\n")
            if ("n" %in% answer_satisfied) {
              test_answer <- readline("Enter: t0, h, quota, min.density, R2, RSD                         >>>>\n\n [Skip (enter 'n'), or adjust fit parameters (see ?growth.gcFitLinear).\n Leave {blank} at a given position if standard parameters are desired.]\n\n")
              if ("n" %in% test_answer) {
                cat("\n Tagged the linear fit of this sample as unreliable !\n\n")
                reliability_tag_linear              <- FALSE
                fitlinear$reliable <- FALSE
                fitlinear.all[[i]]$reliable    <- FALSE
                answer_satisfied <- "y"
              } # end if ("n" %in% test_answer)
              else {
                new_params <- unlist(strsplit(test_answer, split = ","))
                t0_new <- dplyr::if_else(!is.na(as.numeric(new_params[1])), as.numeric(new_params[1]), t0)
                h_new <- dplyr::if_else(!is.na(as.numeric(new_params[2])), as.numeric(new_params[2]), lin.h)
                quota_new <- as.numeric(new_params[3])
                min.density_new <- as.numeric(new_params[4])
                R2_new <- dplyr::if_else(!is.na(as.numeric(new_params[5])), as.numeric(new_params[5]), lin.R2)
                RSD_new <- dplyr::if_else(!is.na(as.numeric(new_params[6])), as.numeric(new_params[6]), lin.RSD)
                control_new <- control
                if(is.numeric(min.density_new)){
                  if(!is.na(min.density_new) && all(as.vector(actwell) < min.density_new)){
                    message(paste0("Start density values need to be greater than 'min.density'.\nThe minimum start value in your dataset is: ",
                                min(as.vector(actwell)),". 'min.density' was not adjusted."), call. = FALSE)
                  } else if(!is.na(min.density_new)){
                    control_new$min.density <- min.density_new
                  }
                }
                fitlinear <- growth.gcFitLinear(acttime, actwell,
                                                         gcID = gcID, control = control_new,
                                                         t0 = t0_new, h = h_new, quota = quota_new,
                                                R2 = R2_new, RSD = RSD_new)
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
    }
    # /// Parametric fit
    if (("m" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
      fitpara          <- growth.gcFitModel(acttime, actwell, gcID, control)
      fitpara.all[[i]] <- fitpara
    }
    else{
      # /// generate empty object
      fitpara          <- list(raw.time = acttime, raw.data = actwell, gcID = gcID, fit.time = NA, fit.data = NA, parameters = list(A=NA, mu=NA, lambda=NA, integral=NA),
                               model = NA, nls = NA, reliable=NULL, fitFlag=FALSE, control = control)
      class(fitpara)   <- "gcFitModel"
      fitpara.all[[i]] <- fitpara
    }

    # /// Non parametric fit
    if (("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
      nonpara             <- growth.gcFitSpline(acttime, actwell, gcID, control, t0 = t0)
      fitnonpara.all[[i]] <- nonpara
    }
    else{
      # /// generate empty object
      nonpara             <- list(raw.time = acttime, raw.data = actwell, gcID = gcID, fit.time = NA, fit.data = NA, parameters = list(A= NA, mu=NA, lambda=NA, integral=NA),
                                  parametersLowess=list(A= NA, mu=NA, lambda=NA), spline = NA, reliable=NULL, fitFlag=FALSE, control = control)
      class(nonpara)      <- "gcFitSpline"
      fitnonpara.all[[i]] <- nonpara
    }
    # /// plotting parametric fit
    if ((control$interactive == TRUE)) {
      if ((("m" %in% control$fit.opt) || ("a"  %in% control$fit.opt))) {
        if (fitpara$fitFlag == TRUE) {
          plot.gcFitModel(fitpara, colData=1, colModel=2, colLag = 3, cex=1.0, raw=T)
          legend(x="bottomright", legend=fitpara$model, col="red", lty=1)
          title("Parametric fit")
          mtext(line = 0.5, side=3, outer = F, cex=1, wellname)
          }
        # /// here a manual reliability tag is set in the interactive mode
        reliability_tag_paarm <- NA
        answer <- readline("Are you satisfied with the model fit (y/n)?\n\n")
        if ("n" %in% answer) {
          cat("\n Tagged the parametric fit of this sample as unreliable !\n\n")
          reliability_tag_param              <- FALSE
          fitpara$reliable <- FALSE
          fitpara.all[[i]]$reliable    <- FALSE
        }
        else{
          reliability_tag_param <- TRUE
          fitpara$reliable <- TRUE
          fitpara.all[[i]]$reliable    <- TRUE
          cat("Sample was (more or less) o.k.\n")
        }
      } # if ((("m" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ) && fitpara$fitFlag == TRUE)
      else {
        reliability_tag_param <- FALSE
        fitpara$reliable <- FALSE
        fitpara.all[[i]]$reliable    <- FALSE
      }
      # /// plotting nonparametric fit
      if (("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt)) {
        if (nonpara$fitFlag == TRUE) {
          answer_satisfied <- "n"
          reliability_tag_nonpara <- NA
          while ("n" %in% answer_satisfied) {
            plot.gcFitSpline(nonpara, add=FALSE, raw=TRUE,slope = T, colData=1, cex=1, plot=T, export=F)
            answer_satisfied <- readline("Are you satisfied with the spline fit (y/n)?\n\n")
            if ("n" %in% answer_satisfied) {
                  test_answer <- readline("Enter: smooth.gc, t0, min.density                                        >>>> \n\n [Skip (enter 'n'), or smooth.gc, t0, and min.density (see ?growth.control).\n Leave {blank} at a given position if standard parameters are desired.]\n\n ")
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
                    t0_new <- t0
                  }
                  smooth.gc_new <- as.numeric(new_params[1])
                  control_new <- control
                  if(!is.na(smooth.gc_new) && smooth.gc_new != ""){
                    control_new$smooth.gc <- smooth.gc_new
                  }
                  min.density_new <- as.numeric(new_params[3])
                  if(!is.na(min.density_new)){
                    if(is.numeric(min.density_new) && min.density_new != 0 && all(as.vector(actwell) < min.density_new)){
                      message(paste0("Start density values need to be below 'min.density'.\nThe minimum start value in your dataset is: ",
                                     min(as.vector(data[,4])),". 'min.density' was not adjusted."), call. = FALSE)
                    } else if(!is.na(min.density_new)){
                      control_new$min.density <- min.density_new
                    }
                  }
                  nonpara <- growth.gcFitSpline(acttime, actwell, gcID, control_new, t0=t0_new)
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
      reliability_tag_param <- TRUE
      reliability_tag_nonpara <- TRUE
    }
    # /// Beginn Bootstrap
    if ((("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ) &&
        (control$nboot.gc > 0) && (reliability_tag_nonpara ==TRUE) && nonpara$fitFlag == TRUE){
      bt            <- growth.gcBootSpline(acttime, actwell, gcID, control)
      boot.all[[i]] <- bt
    } # /// end of if (control$nboot.gc ...)
    else{
      # /// create empty gcBootSpline  object
      bt            <- list(raw.time=acttime, raw.data=actwell, gcID =gcID, boot.x=NA, boot.y=NA, boot.gcSpline=NA,
                            lambda=NA, mu=NA, A=NA, integral=NA, bootFlag=FALSE, control=control)
      class(bt)     <- "gcBootSpline"
      boot.all[[i]] <- bt
    }
    reliability_tag <- any(reliability_tag_linear, reliability_tag_nonpara, reliability_tag_param)
    # create output table
    description     <- data.frame(TestId=data[i,1], AddId=data[i,2],concentration=data[i,3],
                                  reliability_tag=reliability_tag, used.model=fitpara$model,
                                  log.x=control$log.x.gc, log.y=control$log.y.gc, nboot.gc=control$nboot.gc)

    fitted          <- cbind(description, summary.gcFitLinear(fitlinear), summary.gcFitModel(fitpara), summary.gcFitSpline(nonpara), summary.gcBootSpline(bt))

    out.table       <- rbind(out.table, fitted)
    class(out.table) <- c("data.frame", "gcTable")

  } # /// end of for (i in 1:dim(data)[1])
  names(fitlinear.all) <- names(fitpara.all) <- names(fitnonpara.all) <- names(boot.all) <- paste0(as.character(data[,1]), " | ", as.character(data[,2]), " | ", as.character(data[,3]))

  gcFit           <- list(raw.time = time, raw.data = data, gcTable = out.table, gcFittedLinear = fitlinear.all, gcFittedModels = fitpara.all, gcFittedSplines = fitnonpara.all, gcBootSplines = boot.all, control=control)

  class(gcFit)    <- "gcFit"
  gcFit



}

#' Fit different growth models to density data
#'
#' @param time
#' @param data
#' @param gcID
#' @param control
#'
#' @export
#'
growth.gcFitModel <- function(time, data, gcID ="undefined", control=growth.control())
{
  # /// check input parameters
  if (is(control)!="grofit.control") stop("control must be of class grofit.control!")
  if (!any(c("m","a") %in% control$fit.opt)) stop("Fit option is not set for a model fit. See growth.control()")

  # /// conversion to handle even data.frame inputs
  time <- as.vector(as.numeric(as.matrix(time)))
  data    <- as.vector(as.numeric(as.matrix(data)))

  # /// check length of input data
  if (length(time)!=length(data)) stop("gcFitModel: length of time and data input vectors differ!")
  if(max(data) < 1.5*data[1]){
    if(control$suppress.messages==F) message("No significant growth detected (with all values below 1.5 * start_value).")
    gcFitModel   <- list(raw.time = time, raw.data = data, gcID = gcID, fit.time = NA,
                         fit.data = NA, parameters = list(A=NA, mu=0, lambda=NA, integral=NA),
                         model = NA, nls = NA, reliable=NULL, fitFlag=FALSE, control = control)
    class(gcFitModel) <- "gcFitModel"
    return(gcFitModel)
  }
  # /// check if there are enough data points
  if (length(data)<5){
    warning("gcFitModel: There is not enough valid data. Must have at least 5 unique values!")
    gcFitModel   <- list(raw.time = time, raw.data = data, gcID = gcID, fit.time = NA,
                         fit.data = NA, parameters = list(A=NA, mu=NA, lambda=NA, integral=NA),
                         model = NA, nls = NA, reliable=NULL, fitFlag=FALSE, control = control)
    class(gcFitModel) <- "gcFitModel"
    return(gcFitModel)
  }
  else{
      gcFitModel <- grofit.param(time, data, gcID, control)
  }
  return(gcFitModel)
}

#' internal function called within \code{growth.gcFitModel()}.
#'
#' @param time
#' @param data
#' @param gcID
#' @param control
#'
#' @return
#'
#' @examples
grofit.param <- function(time, data, gcID = "undefined", control)
{
    time.in <- time
    data.in <- data
    # Perform log transformation of data
    if (control$log.y.model == TRUE) {
      data <- log(data/data[1])
    }

    # /// determine which values are not valid
    bad.values <- (is.na(time))|(time<0)|(is.na(data))|(data<0)

    # /// remove bad values or stop program
    if (TRUE%in%bad.values){
      if (control$neg.nan.act==FALSE){
        time    <- time[!bad.values]
        data    <- data[!bad.values]
      }
      else{
        stop("Bad values in gcFitModel")
      }
    }

    # fitting parametric growth curves
    y.model     <- NULL
    bestAIC     <- NULL
    best        <- NULL
    used        <- NULL

    # starting values for parametric fitting from spline fit
    control.tmp <- control
    control.tmp$fit.opt <- "s"
    nonpara     <- growth.gcFitSpline(time.in, data.in, gcID, control.tmp)
    mu.low      <- nonpara$parametersLowess$mu
    lambda.low  <- nonpara$parametersLowess$lambda
    A.low       <- nonpara$parametersLowess$A

    # /// determine length of model names
    l               <- 10
    for (i in 1:length(control$model.type)) {
      l[i] <- nchar((control$model.type)[i])
    }
    lmax <- max(l)

    # /// loop over all parametric models requested
    for (i in 1:length(control$model.type)) {
      if (control$suppress.messages == FALSE) {
        cat(paste("--> Try to fit model", (control$model.type)[i]))
      }
      initmodel    <- paste("init", (control$model.type)[i], sep = "")
      formulamodel <-
        as.formula(paste(
          "data ~ ",
          (control$model.type)[i],
          "(time, A, mu, lambda, addpar)",
          sep = ""
        ))
      if ((exists((control$model.type)[i])) && (exists(initmodel))) {
        init.model  <-
          do.call(initmodel,
                  list(
                    y = data,
                    time = time,
                    A = A.low,
                    mu = mu.low,
                    lambda = lambda.low
                  ))
        try(y.model <-
              nls(formulamodel, start = init.model), silent = TRUE)
        if (!(TRUE %in% is.null(y.model))) {
          AIC       <- AIC(y.model)
        }

        if (control$suppress.messages == FALSE) {
          if (class(y.model) == "nls") {
            if (y.model$convInfo$isConv == TRUE) {
              message(paste(rep(".", lmax + 3 - l[i])), " OK")
            }
            else{
              warning(paste(
                rep(".", lmax + 3 - l[i]),
                " nls() failed to converge with stopCode ",
                as.character(y.model$convInfo$stopCode)
              ))
            }
          }
          else{
            message(paste(rep(".", lmax + 3 - l[i])),
                    " ERROR in nls(). For further information see help(growth.gcFitModel)")
          }
        }
        if (FALSE %in% is.null(AIC)) {
          if (is.null(best) || AIC < bestAIC) {
            bestAIC <- AIC
            best    <- y.model
            used    <- (control$model.type)[i]
          }
        }
      } # of if ( (exists((control$model.type)[i])) ...
      else{
        cat((control$model.type)[i])
        cat("\n")
        cat(initmodel)
        cat("\n")
        stop("The model definition above does not exist! Spelling error?")
      }
      y.model <- NULL
    }

    if (control$suppress.messages == FALSE){
      cat("\n")
      cat(paste0("Best fitting model: ", sub("\\(.+", "", sub("data", "", paste(formula(best), collapse = "")))))
    }
    # /// extract parameters from data fit
    if (is.null(best) == FALSE) {
      Abest      <- summary(best)$parameters["A", 1:2]
      mubest     <- summary(best)$parameters["mu", 1:2]
      if(any(grepl("addpar", rownames(summary(best)$parameters)))){
        fitparbest <- summary(best)$parameters[grep("addpar", rownames(summary(best)$parameters)), 1:2]
        if(summary(best)[["formula"]][[3]][[1]] == "richards"){
          fitparbest <- list(nu = as.data.frame(t(fitparbest)))
        } else if (summary(best)[["formula"]][[3]][[1]] == "gompertz.exp"){
          fitparbest <- list(alpha = fitparbest[1,], t_shift = fitparbest[2,])
        }

      }
      fitFlag    <- TRUE
      lambdabest <- summary(best)$parameters["lambda", 1:2]

      best.spline <- stats::smooth.spline(time, fitted.values(best), spar = 0)
      best.deriv1 <-  stats::predict(best.spline, deriv=1)
      mumax.index <- which.max(best.deriv1$y)

      y.max <- fitted.values(best)[mumax.index]
      t.max <- time[mumax.index]
      b.tangent <- y.max - max(best.deriv1$y) * t.max

      if (length(time) == length(as.numeric(fitted.values(best)))) {
        Integralbest <-
          low.integrate(time, as.numeric(fitted.values(best)))
      }
      else{
        Integralbest <- NA
      }
    }
    else{
      warning("gcFitModel: Unable to fit this curve parametrically!")
      Abest        <- c(NA, NA)
      mubest       <- c(NA, NA)
      lambdabest   <- c(NA, NA)
      Integralbest <- NA
      fitFlag      <- FALSE
      b.tangent <- NA
    }

    gcFitModel <-
      list(
        raw.time = time,
        raw.data = data,
        gcID = gcID,
        fit.time = time,
        fit.data = as.numeric(fitted.values(best)),
        parameters = list(
          A = Abest,
          mu = mubest,
          lambda = lambdabest,
          b.tangent = b.tangent,
          fitpar = if(exists("fitparbest")){
            fitparbest
          } else {
            NULL
          },
          integral = Integralbest
        ),
        model = used,
        nls = best,
        reliable = NULL,
        fitFlag = fitFlag,
        control = control
      )

    class(gcFitModel) <- "gcFitModel"

    invisible(gcFitModel)
  }

#' Model a smooth spline fit for density data.
#'
#' @param time
#' @param data
#' @param gcID
#' @param control
#' @param t0
#'
#' @export
#'
growth.gcFitSpline <- function (time, data, gcID = "undefined", control = growth.control(), t0 = 0)
{
  if(!is.null(t0) && !is.na(t0) && t0 != ""){
    t0 <- as.numeric(t0)
  } else {
    t0 <- 0
  }

  if (is(control) != "grofit.control")
    stop("control must be of class grofit.control!")
  if (!any(control$fit.opt %in% c("s","a")))
    stop("Fit option is not set for a spline fit. See growth.control()")
  if(length(data[data<0]) > 0){
    data <- data + abs(min(data[data<0])) # add the absolute value of the minimum negative density to the data
  }
  time.in <- time <- as.vector(as.numeric(as.matrix(time)))
  data.in <- data <- as.vector(as.numeric(as.matrix(data)))

  if (length(time) != length(data))
    stop("gcFitSpline: length of input vectors differ!")
  if(control$log.y.gc == TRUE){
    bad.values <- (is.na(time)) | (is.na(data)) |
      (!is.numeric(time)) | (!is.numeric(data) )
  } else {
  bad.values <- (is.na(time)) | (is.na(data)) |
    (!is.numeric(time)) | (!is.numeric(data))
  }
  if (TRUE %in% bad.values) {
    if (control$neg.nan.act == FALSE) {
      time <- time[!bad.values]
      data <- data[!bad.values]
    }
    else {
      stop("Bad values in gcFitSpline")
    }
  }
  if(max(data) < 1.5*data[1]){
    if(control$suppress.messages==F) message("No significant growth detected (with all values below 1.5 * start_value).")
    gcFitSpline <- list(time.in = time.in, data.in = data.in, raw.time = time, raw.data = data,
                        fit.time = rep(NA, length(time.in)), fit.data = rep(NA, length(data.in)), parameters = list(A = 0, dY = NA,
                                                                                                              mu = 0, lambda = Inf, integral = NA), spline = NA,
                        parametersLowess = list(A = 0, mu = 0, lambda = Inf),
                        spline = NA, reliable = NULL, fitFlag = FALSE,
                        control = control)
    class(gcFitSpline) <- "gcFitSpline"
    return(gcFitSpline)
  }
  if (length(data) < 5) {
    cat("gcFitSpline: There is not enough valid data. Must have at least 5!")
    gcFitSpline <- list(time.in = time.in, data.in = data.in, raw.time = time, raw.data = data,
                        gcID = gcID, fit.time = NA, fit.data = NA, parameters = list(A = NA, dY = NA,
                                                                                     mu = NA, lambda = NA, integral = NA), parametersLowess = list(A = NA,
                                                                                                                                                   mu = NA, lambda = NA), spline = NA, reliable = NULL,
                        fitFlag = FALSE, control = control)
    class(gcFitSpline) <- "gcFitSpline"
    return(gcFitSpline)
  }
  else {
    if (control$log.x.gc == TRUE) {
      # bad.values <- (time < 0)
      if (TRUE %in% bad.values) {
        if (control$neg.nan.act == FALSE) {
          time <- time[!bad.values]
          data <- data[!bad.values]
        }
        else {
          stop("Bad values in gcFitSpline")
        }
      }
      time <- log(1 + time)
    }
    if (control$log.y.gc == TRUE) {
      data.log <- log(data/data[1])
      # bad.values <- (data.log < 0)
      # if (TRUE %in% bad.values) {
      #   if (control$neg.nan.act == FALSE) {
      #     time <- time[!bad.values]
      #     data.log <- data.log[!bad.values]
      #   }
      #   else {
      #     stop("Bad values in gcFitSpline")
      #   }
      # }
    }
    time.raw <- time
    data.raw <- if (control$log.y.gc == TRUE) {
      data.log
    } else {
      data
    }
    # Implement min.density into dataset
    if(!is.null(control$min.density)) {
      if (!is.na(control$min.density) && control$min.density != 0) {
        if (control$log.y.gc == TRUE) {
          # perfom log transformation on min.density (Ln(y/y0))
          min.density <- log(control$min.density / data[1])
          time <- time[max(which.min(abs(time - t0)), which.min(abs(data.log - min.density))):length(time)]
          data.log <- data.log[max(which.min(abs(time.raw - t0)), which.min(abs(data.log - min.density))):length(data.log)]
        } else {
          min.density <- control$min.density
          time <-
            time[max(which.min(abs(time - t0)), which.min(abs(data - min.density))):length(data)]
          data <-
            data[max(which.min(abs(time.raw - t0)), which.min(abs(data - min.density))):length(data)]
        }
      }
    }
    # Implement t0 into dataset
    if(is.numeric(t0) && t0 > 0){
      if (control$log.y.gc == TRUE) {
        data.log <- data.log[which.min(abs(time-t0)):length(data.log)]
      } else{
        data <- data[which.min(abs(time.raw-t0)):length(data)]
      }
      time <- time[which.min(abs(time.raw-t0)):length(time)]
    }
    halftime <- (min(time) + max(time))/2
    try(y.spl <- smooth.spline(time, y = if(control$log.y.gc == TRUE){
      data.log
    } else {
      data
    }, spar = control$smooth.gc))
    if (!exists("y.spl") || is.null(y.spl) == TRUE) {
      warning("Spline could not be fitted to data!")
      if (is.null(control$smooth.gc) == TRUE)
        cat("This might be caused by usage of smoothing parameter 'smooth.gc = NULL'.\n")

      gcFitSpline <- list(time.in = time.in, data.in = data.in, raw.time = time, raw.data = data,
                          fit.time = rep(NA, length(time.in)), fit.data = rep(NA, length(data.in)), parameters = list(A = NA, dY = NA,
                                                                    mu = NA, lambda = NA, integral = NA), spline = NA,
                          parametersLowess = list(A = NA, mu = NA, lambda = NA),
                          spline = NA, reliable = NULL, fitFlag = FALSE,
                          control = control)
      class(gcFitSpline) <- "gcFitSpline"
      return(gcFitSpline)
    } # if(!exists("y.spl") || is.null(y.spl) == TRUE)
    else {
      dydt.spl <- predict(y.spl, time, deriv = 1)
      mumax.index <- which.max(dydt.spl$y) # index of data point with maximum growth rate in first derivative fit
      mumax.index.spl <- which(y.spl$x == dydt.spl$x[mumax.index]) # index of data point with maximum growth rate in spline fit
      t.max <- dydt.spl$x[mumax.index] # time of maximum growth rate
      dydt.max <- max(dydt.spl$y) # maximum value of first derivative of spline fit (i.e., greatest slope in growth curve spline fit)
      mu.spl <- dydt.max # maximum growth rate
      y.max <- y.spl$y[mumax.index.spl] # cell density at time of max growth rate
      b.spl <- y.max - mu.spl * t.max # the y-intercept of the tangent at µmax
      lambda.spl <- -b.spl/mu.spl  # lag time
      integral <- low.integrate(y.spl$x, y.spl$y)
      low <- lowess(time, y = if(control$log.y.gc == TRUE){
        data.log
      } else {
        data
      }, f = 0.25)
      y.low <- low$y
      x.low <- low$x
      dydt.low <- diff(y.low)/diff(time)
      mu.low <- max(dydt.low)
      mumax.index.low <- which.max(dydt.low)
      t.max.low <- x.low[mumax.index.low]
      y.max.low <- y.low[mumax.index.low]
      b.low <- y.max.low - mu.low * t.max.low
      lambda.low <- (-1) * b.low/mu.low + t0
    } # else of if (!exists("y.spl") || is.null(y.spl) == TRUE)
  } # else of if (length(data) < 5)
    gcFitSpline <-
      list(
        time.in = time.in,
        data.in = data.in,
        raw.time = time.raw,
        raw.data = data.raw,
        gcID = gcID,
        fit.time = y.spl$x,
        fit.data = y.spl$y,
        parameters = list(
          A = if (control$log.y.gc == TRUE) {
            # Correct ln(N/N0) transformation for max density value
            data[1] * exp(max(y.spl$y))
          } else {
           max(y.spl$y)
          },
          dY = if (control$log.y.gc == TRUE) {
            data[1] * exp(max(y.spl$y)) -  data[1] * exp(y.spl$y[1])
          } else {
            max(y.spl$y) - y.spl$y[1]
          },
          mu = mu.spl,
          lambda = lambda.spl,
          b.tangent = b.spl,
          integral = integral),
        parametersLowess = list(
          A = if (control$log.y.gc == TRUE) {
            # Correct ln(N/N0) transformation for max density value
            data[1] * exp(max(y.low))
          } else {
            max(y.low)
          },
          mu = mu.low,
          lambda = lambda.low
        ),
        spline = y.spl,
        spline.deriv1 = dydt.spl,
        reliable = NULL,
        fitFlag = TRUE,
        control = control
      )
    class(gcFitSpline) <- "gcFitSpline"
  gcFitSpline
}

#' Fit Exponential Growth Model with a Heuristic Linear Method
#'
#' Determine maximum growth rates from the log-linear part of a growth curve using
#' a heuristic approach similar to the ``growth rates made easy''-method of
#' Hall et al. (2013).
#'
#' The algorithm works as follows:
#' \enumerate{
#'   \item Fit linear regressions to all subsets of \code{h} consecutive data
#'     points (sliding window). If for example \eqn{h=5}, fit a linear regression to points
#'     1 \dots 5, 2 \dots 6, 3 \dots 7 and so on. The method seeks the highest
#'     rate of exponential growth, so the dependent variable is of course
#'     log-transformed.
#'   \item Find the subset with the highest slope \eqn{b_{max}} and
#'     include also the data points of adjacent subsets that have a slope of
#'     at least \eqn{quota \cdot b_{max}},
#'     e.g. all data sets that have at least 95\% of the maximum slope.
#'   \item Fit a new linear model to the extended data window identified in step 2.
#' }
#'
#' @param time vector of independent variable.
#' @param data vector of dependent variable (concentration of organisms).
#' @param h width of the window (number of data).
#' @param quota part of window fits considered for the overall linear fit
#'   (relative to max. growth rate)
#' @param gcID
#' @param t0
#' @param R2
#' @param RSD
#' @param control
#' @param fit.dY (Numeric) Enter the minimum percentage of density increase that a linear regression should cover.
#'
#' @return object with parameters of the fit. The lag time is currently estimated
#' as the intersection between the fit and the horizontal line with \eqn{y=y_0},
#' where \code{y0} is the first value of the dependent variable. The intersection
#' of the fit with the abscissa is indicated as \code{y0_lm} (lm for linear model).
#' These identifieres and their assumptions may change in future versions.
#'
#' @references Hall, BG., Acar, H, Nandipati, A and Barlow, M (2014) Growth Rates Made Easy.
#' Mol. Biol. Evol. 31: 232-38, \doi{10.1093/molbev/mst187}
#'
#' @family fitting functions
#'
#' @export
#'
growth.gcFitLinear <- function(time, data, gcID = "undefined", t0 = 0, h = NULL, quota = 0.95, R2 = 0.95, RSD = 0.05, fit.dY = 0.05, control)
  {
  bad.values <- ((is.na(time))|(is.na(data)) | data < 0)
  data.in <- data <- data[!bad.values]
  time.in <- time <- time[!bad.values]
  if(!is.null(t0) && !is.na(t0) && t0 != ""){
    t0 <- as.numeric(t0)
  } else {
    t0 <- 0
  }

  if(!is.null(h) && !is.na(h) && h != ""){
    h <- as.numeric(h)
  } else {
    # extract period of growth (from defined t0)
    t.growth <- time[which.min(abs(time-t0)):which.max(data)]
    # determine number of data points in period until maximum density
    n.spl <- length(t.growth)
    # Calculate h via log-transformation of the number of data points
    if(n.spl <= 100){
      h <- round((log(n.spl+5, base=2.1))/0.88)
    } else {
      h <- round((log(n.spl, base=1.6)))
    }
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
  data.log <- log(data.in/data.in[1])
  if(!is.null(control$min.density) && !is.na(control$min.density)){
    if(control$min.density != 0){
      min.density <- log(control$min.density / data[1])
    } else {
      min.density <- log(min(data))
    }
  } else {
    min.density <- log(min(data))
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
  max.density <- max(obs$data)
  dY.total <- max.density - obs$data[1]

  if(max(data.in) < 1.5*data.in[1]){
    if(control$suppress.messages==F) message("No significant growth detected (with all values below 1.5 * start_value).")
    gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                        log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, dY = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA ),
                        ndx = NA, rsquared = NA, control = control, fitFlag = FALSE
    )
    class(gcFitLinear) <- "gcFitLinear"
    if(control$suppress.messages==F) message("No data range in accordance with the chosen parameters identified with appropriate linearity.")
    return(gcFitLinear)
  }
  ## number of values
  N <- nrow(obs)

  if(N > h && N>3){
      # Perform linear regression for all N windows and save results in 'ret'
      ret <- matrix(0, nrow = N - h, ncol = 6)
      for(i in 1:(N - h)) {
        ret[i, ] <- c(i, with(obs, (lm_parms(lm_window(time, ylog, i0 = i, h = h)))))
      }
      colnames(ret) <- c("index", "y-intersect", "slope", "X4", "R2", "RSD")
      # add time and density values as columns in ret
      ret <- data.frame(ret, time = time[ret[,1]], data = obs$ylog[ret[,1]])
      # add dY, i.e., the percentage of density that a regression window covers, to ret
      ret <- data.frame(ret, dY = ((obs$data[match(ret[, "data"], obs$ylog)+(h-1)] - obs$data[match(ret[, "data"], obs$ylog)]) / dY.total))

      bad <- is.na(ret[,5]) | is.na(ret[,6])
      ret <- ret[!bad,]
      if(nrow(ret)<2){
        gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
          log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
            y0 = NA, dY = NA, y0_lm = NA, mumax = NA, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA ),
          ndx = NA, rsquared = NA, control = control, fitFlag = FALSE
        )
        class(gcFitLinear) <- "gcFitLinear"
        if(control$suppress.messages==F) message("No data range in accordance with the chosen parameters identified with appropriate linearity.")
        return(gcFitLinear)
      }
      else{
          # duplicate ret for further tuning of fit
          if(exists("min.density")){
            ret.check <- ret[max(which.min(abs(time-t0)), which.min(abs(ret$data-min.density))) : nrow(ret),] # consider only slopes from defined t0 and min.density
          } else{
            ret.check <- ret[which.min(abs(time-t0)):nrow(ret),] # consider only slopes from defined t0
          }

        #Consider only slopes that span at least fit.dY
        ret.check <- ret.check[ret.check[,"dY"]>=fit.dY, ]


          if(nrow(ret.check)<2){
            gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                                log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                                  y0 = NA, dY = NA, y0_lm = NA, mumax = NA, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA ),
                                ndx = NA, rsquared = NA, control = control, fitFlag = FALSE
            )
            class(gcFitLinear) <- "gcFitLinear"
            if(control$suppress.messages==F) message("No data range in accordance with the chosen parameters identified with appropriate linearity.")
            return(gcFitLinear)
          } else {

          ## Determine index of window with maximum growth rate, iterate until regression is found that meets R2 and RSD criterion
          success <- FALSE
          # apply min.density to list of linear regressions
          ret.check <- ret.check[ret.check[, 8] >= min.density, ]
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
            if(exists("min.density")){
              candidates <- which(ret[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                    ret[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                    abs(ret[, 6]) <= 1.02 * RSD & # RSD criterion for candidates
                                    ret[, 7] >= t0 & # consider only slopes after defined t0
                                    ret[, 8] >= min.density # consider only slopes at densities higher than "min.density"
                                  )
            } else{
              candidates <- which(ret[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                    ret[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                    abs(ret[, 6]) <= 1.02 * RSD & # RSD criterion for candidates
                                    ret[, 7] >= t0) # consider only slopes after defined t0
            }
            #consider only candidate windows next to index.max.ret
            candidate_intervals <- split(candidates, cumsum(c(1, diff(candidates) != 1)))
            if(index.max.ret %in% unlist(candidate_intervals)){
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
              #perform linear regression with candidate data points
              tp <- seq(min(candidates), max(candidates) + h-1)
              m <- lm_window(obs$time, obs$ylog, min(tp), length(tp)) # linear model
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
              y0_data  <- obs$ylog[1] # y0 in dataset
              mumax <- unname(coef(m)[2])

              ## estimate lag phase
              lambda <- (y0_data - y0_lm) / mumax

              # correct y0 values for Ln(y(t)/y0)
              y0_lm <- obs$data[1] * exp(y0_lm)
              y0_data <- obs$data[1]

              # get indices of time points used in linear fit
              ndx <- seq(min(match(ret[candidates, "time"], time.in)),
                         max(match(ret[candidates, "time"], time.in)) + h-1)

              mu.se <- as.numeric(p[3]) # standard error of slope
              fitFlag <- TRUE

            }
            else { # of if(length(candidates) > 0)
              gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                                  log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                                    y0 = NA, dY = NA, y0_lm = NA, mumax = NA, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA ),
                                  ndx = NA, rsquared = NA, control = control, fitFlag = FALSE
              )
              class(gcFitLinear) <- "gcFitLinear"
              if(!control$suppress.messages) message(paste0("No linear fit in accordance with the chosen parameters identified with: R2 >= ", R2, ", RSD <= ", RSD, ", t0 = ", t0, ", and min.density = ", control$min.density, "."))
              return(gcFitLinear)
            }
          } # if(any(ret.check[,5] >= R2 & ret.check[,6] <= RSD))
          else{
            gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                                log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                                  y0 = NA, dY = NA, y0_lm = NA, mumax = NA, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA ),
                                ndx = NA, rsquared = NA, control = control, fitFlag = FALSE
            )
            class(gcFitLinear) <- "gcFitLinear"
            if(!control$suppress.messages) message(paste0("No linear fit in accordance with the chosen parameters identified with an R2 value of >= ", R2, " and an RSD of <= ", RSD, "."))
            return(gcFitLinear)
          }
        } # else of if(nrow(ret.check)<2)
      } # else of if(nrow(ret)<1)
  } # if(N >= 3)
  else {
    message("Not enough observations in the dataset to perform exponential fit. growth.gcFitLinear requires at least 3 data points between t0 and the time of maximum density.")
    gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                        log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, y0_lm = NA, mumax = NA, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA ),
                        ndx = NA, rsquared = NA, control = control, fitFlag = FALSE
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
    FUN = grow_exponential,
    fit = m,
    par = c(
      y0 = y0_data,
      dY = max(obs$data)-obs$data[1],
      A = max(obs$data),
      y0_lm = y0_lm,
      mumax = mumax,
      mu.se = mu.se,
      lag = lambda,
      tmax_start = tmax_start,
      tmax_end = tmax_end
    ),
    ndx = ndx,
    rsquared = p["r2"],
    control = control,
    fitFlag = fitFlag
  )

  class(gcFitLinear) <- "gcFitLinear"

  invisible(gcFitLinear)
}

#' gcBootSpline: Function to generate a bootstrap
#' @param time
#' @param data
#' @param gcID
#' @param control
#'
#' @export
#'
growth.gcBootSpline <- function (time, data, gcID = "undefined", control = growth.control())
{
  if (is(control) != "grofit.control")
    stop("control must be of class grofit.control!")
  if (control$nboot.gc == 0)
    stop("Number of bootstrap samples is zero! See growth.control()")
  time <- as.vector(as.numeric(as.matrix(time)))
  data <- as.vector(as.numeric(as.matrix(data)))
  if (length(time) != length(data))
    stop("gcBootSpline: length of input vectors differ!")
  if(control$log.y.gc == TRUE){
    bad.values <- (is.na(time)) | (is.na(data)) |
      (!is.numeric(time)) | (!is.numeric(data) | (data<=0))
  } else {
  bad.values <- (is.na(time)) | (is.na(data)) | is.infinite(data) |
    (!is.numeric(time)) | (!is.numeric(data))
  }
  if (TRUE %in% bad.values) {
    if (control$neg.nan.act == FALSE) {
      time <- time[!bad.values]
      data <- data[!bad.values]
    }
    else stop("Bad values in gcBootSpline")
  }
  if (length(data) < 6) {
    warning("gcBootSpline: There is not enough valid data. Must have at least 6 unique values!")
    gcBootSpline <- list(raw.time = time, raw.data = data,
                         gcID = gcID, boot.time = NA, boot.data = NA, boot.gcSpline = NA,
                         lambda = NA, mu = NA, A = NA, integral = NA, bootFlag = FALSE,
                         control = control)
    class(gcBootSpline) <- "gcBootSpline"
    return(gcBootSpline)
  }
  A <- NA
  mu <- NA
  lambda <- NA
  integral <- NA
  boot.y <- array(NA, c(control$nboot.gc, length(time)))
  boot.x <- array(NA, c(control$nboot.gc, length(time)))
  nonpara <- list()
  control.change <- control
  control.change$fit.opt <- "s"
  if (control$nboot.gc > 0) {
    for (j in 1:control$nboot.gc) {
      choose <- sort(sample(1:length(time), length(time), replace = TRUE))
      while (length(unique(choose)) < 5) {
        choose <- sort(sample(1:length(time), length(time),
                         replace = TRUE))
      }
      time.cur <- time[choose]
      data.cur <- data[choose]
      if(IQR(time.cur) > 0){
        nonpara[[j]] <- growth.gcFitSpline(time.cur, data.cur, gcID,
                                    control.change)
        if(nonpara[[j]]$fitFlag==FALSE | is.na(nonpara[[j]]$fit.data[1])){
          boot.y[j, 1:length(nonpara[[j]]$fit.data)] <- rep(NA, length(nonpara[[j]]$fit.data))
          boot.x[j, 1:length(nonpara[[j]]$fit.time)] <- rep(NA, length(nonpara[[j]]$fit.time))
        }
        else{
          boot.y[j, 1:length(nonpara[[j]]$fit.data)] <- nonpara[[j]]$fit.data
          boot.x[j, 1:length(nonpara[[j]]$fit.time)] <- nonpara[[j]]$fit.time
        }
        lambda[j] <- nonpara[[j]]$parameters$lambda
        mu[j] <- nonpara[[j]]$parameters$mu
        A[j] <- nonpara[[j]]$parameters$A
        integral[j] <- nonpara[[j]]$parameters$integral
      }
    }
    lambda[which(!is.finite(lambda))] <- NA
    mu[which(!is.finite(lambda))] <- NA
    A[which(!is.finite(lambda))] <- NA
    integral[which(!is.finite(lambda))] <- NA
    if (control$clean.bootstrap == TRUE) {
      lambda[which(lambda < 0)] <- NA
      mu[which(mu < 0)] <- NA
      A[which(A < 0)] <- NA
      integral[which(integral < 0)] <- NA
    }
  }
  if (control$log.x.gc == TRUE) {
    bad.values <- (time < 0)
    if (TRUE %in% bad.values) {
        time <- time[!bad.values]
        data <- data[!bad.values]
    }
    time.log <- log(1 + time)
  }
  if (control$log.y.gc == TRUE) {
    data.log <- log(data/data[1])
    bad.values <- (data.log < 0)
    if (TRUE %in% bad.values) {
        time <- time[!bad.values]
        data.log <- data.log[!bad.values]
    }
  }
  gcBootSpline <- list(
    raw.time = if (control$log.x.gc == TRUE) {
      time.log
    } else {
      time
    },
    time,
    raw.data = if (control$log.y.gc == TRUE) {
      data.log
    } else {
      data
    },
    gcID = gcID,
    boot.time = boot.x,
    boot.data = boot.y,
    boot.gcSpline = nonpara,
    lambda = lambda,
    mu = mu,
    A = A,
    integral = integral,
    bootFlag = TRUE,
    control = control
  )
  class(gcBootSpline) <- "gcBootSpline"
  gcBootSpline
}

#'
#' @param gcFitData
#' @param control
#'
#' @export
#'
growth.drFit <- function (gcFitData, control = growth.control())
{
  if (is(control) != "grofit.control")
    stop("control must be of class grofit.control!")
  EC50.table <- NULL
  all.EC50 <- NA
  table.tests <- table((gcFitData[, 1])[which((gcFitData[,
                                                         4] == TRUE) & (is.na(gcFitData[, control$parameter]) ==
                                                                          FALSE))])
  distinct <- names(table.tests)
  EC50 <- list()
  EC50.boot <- list()
  validdata <- cbind(as.character(distinct), table.tests)
  colnames(validdata) <- c("TestID", "Number")
  rownames(validdata) <- rep("     ", times = dim(validdata)[1])
  if (control$suppress.messages == FALSE) {
    cat("\n")
    cat("=== EC 50 Estimation ==============================\n")
    cat("---------------------------------------------------\n")
    cat("--> Checking data ...\n")
    cat(paste("--> Number of distinct tests found:", as.character(length(distinct))),
        "\n")
    cat("--> Valid datasets per test: \n")
    print(validdata, quote = FALSE)
  }
  if (TRUE %in% (table.tests < control$have.atleast)) {
    cat(paste("Warning: following tests have not enough ( <",
              as.character(control$have.atleast - 1), ") datasets:\n"))
    cat(distinct[(table.tests < control$have.atleast)])
    cat("These tests will not be regarded\n")
    distinct <- distinct[table.tests >= control$have.atleast]
  }
  if ((length(distinct)) == 0) {
    cat(paste("There are no tests having enough ( >", as.character(control$have.atleast -
                                                                     1), ") datasets!\n"))
  }
  else {
    for (i in 1:length(distinct)) {
      conc <- (gcFitData[, 3])[which(gcFitData[, 1] ==
                                       distinct[i])]
      test <- (gcFitData[, control$parameter])[gcFitData[, 1] == distinct[i]]
      names(test) <- rep(names(gcFitData)[control$parameter], length(test))
      drID <- distinct[i]
      EC50[[i]] <- growth.drFitSpline(conc, test, drID, control)
      if (control$nboot.dr > 0) {
        EC50.boot[[i]] <- growth.drBootSpline(conc, test, drID,
                                       control)
      }
      else {
        EC50.boot[[i]] <- list(raw.time = conc, raw.data = test,
                               drID = drID, boot.x = NA, boot.y = NA, boot.drSpline = NA,
                               ec50.boot = NA, bootFlag = FALSE, control = control)
        class(EC50.boot[[i]]) <- "drBootSpline"
      }
      description <- data.frame(Test = distinct[i], log.x = control$log.x.dr,
                                log.y = control$log.y.dr, Samples = control$nboot.dr)
      out.row <- cbind(description, summary.drFitSpline(EC50[[i]]),
                       summary.drBootSpline(EC50.boot[[i]]))
      EC50.table <- rbind(EC50.table, out.row)
    }
  }
  names(EC50) <- names(EC50.boot) <- distinct
  drFit <- list(raw.data = gcFitData, drTable = EC50.table,
                drBootSplines = EC50.boot, drFittedSplines = EC50, control = control)
  class(drFit) <- "drFit"
  drFit
}

#'
#' @param conc
#' @param test
#' @param drID
#' @param control
#'
#' @export
#'
growth.drFitSpline <- function (conc, test, drID = "undefined", control = growth.control())
{
  if (is(control) != "grofit.control")
    stop("control must be of class grofit.control!")
  test.nm <- names(test)[1]
  test <- as.vector(as.numeric(as.matrix(test)))
  conc <- as.vector(as.numeric(as.matrix(conc)))
  if (is.vector(conc) == FALSE || is.vector(test) == FALSE)
    stop("drFitSpline: dose or response data must be a vector !")
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
      stop("drFitSpline: NA values encountered. Program terminated")
    if ((sum((conc < 0)) > 0) | (sum((test < 0)) > 0))
      stop("drFitSpline: Negative values encountered. Program terminated")
    if ((FALSE %in% is.numeric(conc)) || (FALSE %in% is.numeric(test)))
      stop("drFitSpline: Non numeric values encountered. Program terminated")
  }
  if (length(test) < 6) {
    warning("drFitSpline: There is not enough valid data. Must have at least 6 unique values!")
    drFitSpline <- list(raw.conc = conc, raw.test = test,
                        drID = drID, fit.conc = NA, fit.test = NA, spline = NA,
                        parameters = list(EC50 = NA, yEC50 = NA, EC50.orig = NA,
                                          yEC50.orig = NA), fitFlag = FALSE, reliable = NULL,
                        control = control)
    class(drFitSpline) <- "drFitSpline"
    return(drFitSpline)
  }
  if (length(test) < control$have.atleast) {
    warning("drFitSpline: number of valid data points is below the number specified in 'have.atleast'. See growth.control().")
    drFitSpline <- list(raw.conc = conc, raw.test = test,
                        drID = drID, fit.conc = NA, fit.test = NA, spline = NA,
                        parameters = list(EC50 = NA, yEC50 = NA, EC50.orig = NA,
                                          yEC50.orig = NA), fitFlag = FALSE, reliable = NULL,
                        control = control)
    class(drFitSpline) <- "drFitSpline"
    return(drFitSpline)
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
  spltest <- NULL
  fitFlag <- TRUE
  try(spltest <- smooth.spline(conc.fit, test.fit, spar = control$smooth.dr))
  if (is.null(spltest) == TRUE) {
    cat("Spline could not be fitted in dose-response analysis!\n")
    fitFlag <- FALSE
    if (is.null(control$smooth.dr) == TRUE) {
      cat("This might be caused by usage of smoothing parameter 'smooth.dr = NULL'.\n")
    }
    stop("Error in drFitSpline")
  }
  conc.min <- min(conc.fit)
  conc.max <- max(conc.fit)
  c.pred <- seq(conc.min, conc.max, length.out = 1000)
  ytest <- predict(spltest, c.pred)
  yEC.test <- (max(ytest$y) - min(ytest$y))/2 + min(ytest$y)
  last.test <- max(ytest$y)
  kec.test <- 1
  for (k in 1:(length(c.pred) - 1)) {
    d1 <- (ytest$y[k] - yEC.test)
    d2 <- (ytest$y[k + 1] - yEC.test)
    if (((d1 <= 0) && (d2 >= 0)) | ((d1 >= 0) && (d2 <= 0))) {
      kec.test <- k
      break
    }
  }
  EC.test <- c.pred[kec.test]
  if (control$suppress.messages == FALSE) {
    cat("\n\n=== Dose response curve estimation ================\n")
    cat("--- EC 50 -----------------------------------------\n")
    cat(paste("-->", as.character(drID)))
    cat("\n")
    cat(paste(c("xEC50", "yEC50"), c(EC.test, yEC.test)))
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
  drFitSpline <- list(raw.conc = conc, raw.test = test, drID = drID,
                      fit.conc = ytest$x, fit.test = ytest$y, spline = spltest,
                      parameters = list(EC50 = EC.test[1], yEC50 = yEC.test,
                                        EC50.orig = EC.orig[1], yEC50.orig = EC.orig[2], test = test.nm),
                      fitFlag = fitFlag, reliable = NULL, control = control)
  class(drFitSpline) <- "drFitSpline"
  drFitSpline
}

#'
#' @param conc
#' @param test
#' @param drID
#' @param control
#'
#' @export
#'
growth.drBootSpline <- function (conc, test, drID = "undefined", control = growth.control())
{
  test <- as.vector(as.numeric(as.matrix(test)))
  conc <- as.vector(as.numeric(as.matrix(conc)))
  if (is.vector(conc) == FALSE || is.vector(test) == FALSE)
    stop("Need concentration and treatment !")
  if (is(control) != "grofit.control")
    stop("control must be of class grofit.control!")
  if (control$nboot.dr == 0)
    stop("Number of bootstrap samples is zero! See growth.control()")
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
      stop("NA values encountered. Program terminated")
    if ((sum((conc < 0)) > 0) | (sum((test < 0)) > 0))
      stop("drFitSpline: Negative values encountered. Program terminated")
    if ((FALSE %in% is.numeric(conc)) || (FALSE %in% is.numeric(test)))
      stop("drFitSpline: Non numeric values encountered. Program terminated")
  }
  if (length(test) < 6) {
    warning("drBootSpline: There is not enough valid data. Must have at least 6 unique values!")
    drBootSpline <- list(raw.conc = conc, raw.test = test,
                         drID = drID, boot.conc = NA, boot.test = NA, boot.drSpline = NA,
                         ec50.boot = NA, bootFlag = FALSE, control = control)
    class(drBootSpline) <- "drBootSpline"
    return(drBootSpline)
  }
  if (length(test) < control$have.atleast) {
    warning("drBootSpline: number of valid data points is below the number specified in 'have.atleast'. See growth.control().")
    drBootSpline <- list(raw.conc = conc, raw.test = test,
                         drID = drID, boot.conc = NA, boot.test = NA, boot.drSpline = NA,
                         ec50.boot = NA, bootFlag = FALSE, control = control)
    class(drBootSpline) <- "drBootSpline"
    return(drBootSpline)
  }
  if (control$log.x.dr == TRUE)
    conc.log <- log(conc + 1)
  if (control$log.y.dr == TRUE)
    test.log <- log(test + 1)
  if (control$log.x.dr == TRUE)
    conc.boot <- log(conc + 1)
  else conc.boot <- conc
  if (control$log.y.dr == TRUE)
    test.boot <- log(test + 1)
  else test.boot <- test
  boot.x <- array(NA, c(control$nboot.dr, 1000))
  boot.y <- array(NA, c(control$nboot.dr, 1000))
  ECtest.boot <- seq(0, 0, length.out = control$nboot.dr)
  y.EC50.boot <- seq(0, 0, length.out = control$nboot.dr)
  splinefit <- list()
  sa <- seq(1, length(conc.boot))
  for (b in 1:control$nboot.dr) {
    s <- sample(sa, length(conc.boot), replace = TRUE)
    s.conc <- conc.boot[s]
    while (length(unique(s.conc)) < 5) {
      s <- sample(sa, length(conc.boot), replace = TRUE)
      s.conc <- conc.boot[s]
    }
    s.test <- test.boot[s]
    spltest <- NULL
    control.changed <- control
    control.changed$suppress.messages <- TRUE
    splinefit[[b]] <- growth.drFitSpline(s.conc, s.test, drID, control.changed)
    spltest <- splinefit[[b]]$spline
    boot.x[b, 1:length(splinefit[[b]]$fit.conc)] <- splinefit[[b]]$fit.conc
    boot.y[b, 1:length(splinefit[[b]]$fit.test)] <- splinefit[[b]]$fit.test
    if (is.null(spltest) == TRUE) {
      cat("Spline could not be fitted in dose-response analysis!!\n")
      if (is.null(control$smooth.dr) == TRUE) {
        cat("This might be caused by usage of smoothing parameter 'smooth.dr = NULL'.\n")
      }
      stop("Error in drBootSpline")
    }
    ECtest.boot[b] <- splinefit[[b]]$parameters$EC50
    y.EC50.boot[b] <- splinefit[[b]]$parameters$yEC50
  }
  ECtest.boot[which(!is.finite(ECtest.boot))] <- NA
  if (control$clean.bootstrap == TRUE)
    ECtest.boot[which(ECtest.boot < 0)] <- NA
  m.test <- mean(ECtest.boot, na.rm = TRUE)
  s.test <- sd(ECtest.boot, na.rm = TRUE)
  if (control$suppress.messages == FALSE) {
    cat("=== Bootstrapping of dose response curve ==========\n")
    cat("--- EC 50 -----------------------------------------\n")
    cat("\n")
    cat(paste("Mean  : ", as.character(m.test), "StDev : ",
              as.character(s.test), "\n"))
    cat(paste("90% CI: ", as.character(c(m.test - 1.645 *
                                           s.test, m.test + 1.645 * s.test))))
    cat("\n")
    cat(paste("95% CI: ", as.character(c(m.test - 1.96 *
                                           s.test, m.test + 1.96 * s.test))))
    cat("\n\n")
  }
  EC50 <- data.frame(EC50.boot = m.test, EC50.sd = s.test,
                     CI.90.lo = m.test - 1.645 * s.test, CI.90.up = m.test +
                       1.645 * s.test, CI.95.lo = m.test - 1.96 * s.test,
                     CI.95.up = m.test + 1.96 * s.test)
  if (control$log.x.dr == TRUE && control$suppress.messages ==
      FALSE) {
    cat("\n")
    cat("--- EC 50 in original scale -----------------------\n")
    cat("\n")
    cat(paste("Mean  : ", as.character(exp(m.test) - 1),
              "\n"))
    cat(paste("90% CI: ", as.character(c(exp(m.test - 1.645 *
                                               s.test) - 1, exp(m.test + 1.645 * s.test) - 1))))
    cat("\n")
    cat(paste("95% CI: ", as.character(c(exp(m.test - 1.96 *
                                               s.test) - 1, exp(m.test + 1.96 * s.test) - 1))))
    cat("\n\n")
  }
  drBootSpline <- list(raw.conc = conc, raw.test = test, drID = drID,
                       boot.conc = boot.x, boot.test = boot.y, boot.drSpline = splinefit,
                       ec50.boot = ECtest.boot, ec50y.boot = y.EC50.boot, bootFlag = TRUE, control = control)
  class(drBootSpline) <- "drBootSpline"
  drBootSpline
}

lm_parms <- function (m)
{
  sm <- summary(m)
  a <- sm$coefficients[1, 1]
  b <- sm$coefficients[2, 1]
  b.se <- sm$coefficients[2, 2]
  r2 <- sm$r.squared
  c(a = a, b = b, b.se = b.se, r2 = r2, b.rsd = b.se/b)
}

lm_window <- function (x, y, i0, h = 5)
{
  x <- x[i0 - 1 + (1:h)]
  y <- y[i0 - 1 + (1:h)]
  m <- lm(y ~ x)
  return(m)
}

grow_exponential <- function (time, parms)
{
  if (is.null(names(parms))) {
    y0 <- parms[1]
    mumax <- parms[2]
  }
  else {
    y0 <- parms["y0"]
    mumax <- parms["mumax"]
  }
  y <- y0 * exp(mumax * time)
  return(as.matrix(data.frame(time = time, y = y)))
}

low.integrate <- function (x, y)
{
  if (is.vector(x) == FALSE || is.vector(y) == FALSE)
    stop("low.integrate: two vectors x and y are needed !")
  if (length(x) != length(y))
    stop("low.integrate: x and y have to be of same length !")
  y.spl <- NULL
  try(y.spl <- smooth.spline(x, y))
  if (is.null(y.spl) == TRUE) {
    warning("Spline could not be fitted to data!")
    stop("Error in low.integrate")
  }
  f <- function(t) {
    p <- predict(y.spl, t)
    f <- p$y
  }
  low.integrate <- integrate(f, min(x), max(x))$value
}

logistic <- function (time, A, mu, lambda, addpar = NULL)
{
  A <- A[1]
  mu <- mu[1]
  lambda <- lambda[1]
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  y <- A/(1 + exp(4 * mu * (lambda - time)/A + 2))
  logistic <- y
}

initlogistic <- function (time, y, A, mu, lambda)
{
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(y) == FALSE)
    stop("Need numeric vector for: y")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  A <- max(y)
  mu <- mu[1]
  lambda <- lambda[1]
  initlogistic <- list(A = A, mu = mu, lambda = lambda, addpar = NULL)
}

initrichards <- function (time, y, A, mu, lambda)
{
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(y) == FALSE)
    stop("Need numeric vector for: y")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  nu <- 0.1
  A <- max(y)
  mu <- mu[1]
  lambda <- lambda[1]
  initrichards <- list(A = A, mu = mu, lambda = lambda, addpar = nu)
}

richards <- function (time, A, mu, lambda, addpar)
{
  A <- A[1]
  mu <- mu[1]
  lambda <- lambda[1]
  nu <- addpar[1]
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  if (is.numeric(nu) == FALSE)
    stop("Need numeric vector for: addpar[1]")
  y <- A * (1 + nu * exp(1 + nu) * exp(mu * (1 + nu)^(1 + 1/nu) *
                                         (lambda - time)/A))^(-1/nu)
  richards <- y
}

gompertz <- function (time, A, mu, lambda, addpar = NULL)
{
  A <- A[1]
  mu <- mu[1]
  lambda <- lambda[1]
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  e <- exp(1)
  y <- A * exp(-exp(mu * e * (lambda - time)/A + 1))
  gompertz <- y
}

initgompertz <- function (time, y, A, mu, lambda)
{
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(y) == FALSE)
    stop("Need numeric vector for: y")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  A <- max(y)
  mu <- mu[1]
  lambda <- lambda[1]
  initgompertz <- list(A = A, mu = mu, lambda = lambda, addpar = NULL)
}

initgompertz.exp <- function (time, y, A, mu, lambda)
{
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(y) == FALSE)
    stop("Need numeric vector for: y")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  alpha <- 0.1
  t_shift <- max(time)/10
  A <- max(y)
  mu <- mu[1]
  lambda <- lambda[1]
  initgompertz.exp <- list(A = A, mu = mu, lambda = lambda,
                           addpar = c(alpha, t_shift))
}

gompertz.exp <- function (time, A, mu, lambda, addpar)
{
  A <- A[1]
  mu <- mu[1]
  lambda <- lambda[1]
  alpha <- addpar[1]
  t_shift <- addpar[2]
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  if (is.numeric(alpha) == FALSE)
    stop("Need numeric vector for: addpar[1]")
  if (is.numeric(t_shift) == FALSE)
    stop("Need numeric vector for: addpar[2]")
  e <- exp(1)
  y <- A * exp(-exp(mu * e * (lambda - time)/A + 1)) + A *
    exp(alpha * (time - t_shift))
  gompertz.exp <- y
}

