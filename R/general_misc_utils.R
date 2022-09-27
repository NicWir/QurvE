#' Run upon attaching package QurvE
#'
#' Changes debug option for package \code{rgl} to avoid Rstudio crashing upon attaching it and prints welcome message
#'
#' @param libname library name
#' @param pkgname package name
.onAttach <- function (libname, pkgname){
  options(rgl.debug = TRUE)
  k1 <- paste("QurvE",utils::packageVersion( "QurvE"),"initialized Successfully !")
  k0 <- "\n"
  k2 <- paste("https://github.com/NicWir/QurvE")
  packageStartupMessage(c(k1,k0,k2))
  if(!tinytex::is_tinytex()){
    packageStartupMessage("TinyTex was not found on your system. To ensure full functionality of QurvE, please execute tinytex::install_tinytex().")
  }
}

#' Format font color for Markdown reports
#'
#' \code{colFmt} formats the input depending on PDF or HTML output to give colored text in reports.
#'
#' @param x A character string. The text to be colored.
#' @param color (Character) A color.
colFmt <- function(x, color) {
  outputFormat <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  if (outputFormat == "latex") {
    ret <- paste("\\textcolor{", color, "}{", gsub("%", "\\\\%", gsub("_", "\\\\_", x)), "}", sep = "")
  } else if (outputFormat == "html") {
    ret <- paste("<font color='", color, "'>", x, "</font>", sep = "")
  } else {
    ret <- x
  }
  return(ret)
}

#' Write object in CSV file
#'
#' @param dat An R data object (e.g., list, data frame)
#' @param file (Character) The name of the CSV file.
#' @param row.names (Logical) Add row names as column (\code{TRUE}) or not (\code{FALSE}).
fast.write.csv <- function(dat, file, row.names = TRUE) {
  tryCatch(
    {
      if (is.data.frame(dat)) {
        # there is a rare bug in data.table (R 3.6) which kill the R process in some cases
        data.table::fwrite(dat, file, row.names = row.names)
      } else {
        utils::write.csv(dat, file, row.names = row.names)
      }
    },
    error = function(e) {
      print(e)
      utils::write.csv(dat, file, row.names = row.names)
    },
    warning = function(w) {
      print(w)
      utils::write.csv(dat, file, row.names = row.names)
    }
  )
}

#' Call the appropriate function required to read a table file and return the table as a dataframe object.
#'
#' @param filename (Character) Name or path of the table file to read. Can be of type CSV, XLS, XLSX, TSV, or TXT.
#' @param csvsep (Character) separator used in CSV file (ignored for other file types).
#' @param dec (Character) decimal separator used in CSV, TSV and TXT files.
#' @param sheet (Numeric or Character) Number or name of a sheet in XLS or XLSX files (_optional_). Default: \code{";"}
#'
#' @return A dataframe object
#' @export
#'
read_file <- function(filename, csvsep = ";", dec = ".", sheet = 1){
  if (file.exists(filename)) {
    if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "csv") {
      dat <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = csvsep,
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
               stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx") {
      dat <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = F, sheet = sheet, progress = TRUE)))
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
      dat <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = "\t",
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "txt") {
      dat <-
        utils::read.table(
          filename,
          dec = dec,
          sep = "\t",
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else {
      stop(
        "No compatible file format provided.
             Supported formats are: \\.txt (tab delimited), \\.csv (delimiters can be specified with the argument \"csvsep = \", \\.tsv, \\.xls, and \\.xlsx"
      )
    }
  } else {
    stop(paste0("File \"", filename, "\" does not exist."), call. = F)
  }
  return(dat)
}

suppress_warnings <- function(.expr, .f, ...) {
  eval.parent(
    substitute(
      withCallingHandlers( .expr, warning = function (w) {
        cm   <- conditionMessage(w)
        cond <- if (is.character(.f)) grepl(.f, cm) else rlang::as_function(.f)(cm, ...)
        if (cond) invokeRestart("muffleWarning")
      })
    )
  )
}

base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

inflect <- function(x, threshold = 1){
  up   <- sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
  down <-  sapply(-1:-threshold, function(n) c(rep(NA,abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
  a    <- cbind(x,up,down)
  list(minima = which(apply(a, 1, min) == a[,1]), maxima = which(apply(a, 1, max) == a[,1]))
}

zipFastener <- function(df1, df2, along=2)
{
  # parameter checking
  if(!is.element(along, c(1,2))){
    stop("along must be 1 or 2 for rows and columns
                                              respectively")
  }
  # if merged by using zip feeding along the columns, the
  # same no. of rows is required and vice versa
  if(along==1 & (ncol(df1)!= ncol(df2))) {
    stop ("the no. of columns has to be equal to merge
               them by zip feeding")
  }
  if(along==2 & (nrow(df1)!= nrow(df2))) {
    stop ("the no. of rows has to be equal to merge them by
               zip feeding")
  }

  # zip fastener preperations
  d1 <- dim(df1)[along]
  d2 <- dim(df2)[along]
  i1 <- 1:d1           # index vector 1
  i2 <- 1:d2 + d1      # index vector 2

  # set biggest dimension dMax
  if(d1==d2) {
    dMax <- d1
  } else if (d1 > d2) {
    length(i2) <- length(i1)    # make vectors same length,
    dMax <- d1                  # fill blanks with NAs
  } else  if(d1 < d2){
    length(i1) <- length(i2)    # make vectors same length,
    dMax <- d2                  # fill blanks with NAs
  }

  # zip fastener operations
  index <- as.vector(matrix(c(i1, i2), ncol=dMax, byrow=T))
  index <- index[!is.na(index)]         # remove NAs

  if(along==1){
    colnames(df2) <- colnames(df1)   # keep 1st colnames
    res <- rbind(df1,df2)[ index, ]  # reorder data frame
  }
  if(along==2) res <- cbind(df1,df2)[ , index]

  return(res)
}




#' Run Shiny QurvE App
#'
#' @export
#'
#' @importFrom DT dataTableOutput renderDT datatable
#' @importFrom readxl read_excel excel_sheets
#'
#' @import shiny doParallel knitr kableExtra
run_app <- function() {

  # Locate all the shiny apps that exist
  valid_apps <- list.files(system.file("shiny_app", package = "QurvE"))



  # Launch the app
  appDir <- system.file("shiny_app", package = "QurvE")
  suppressWarnings(shiny::runApp(appDir, display.mode = "normal"))
}

parse_Gen5Gen6 <- function(input)
{
  # get row numbers for "time" in column 2
  time.ndx <- grep("\\btime\\b", input[[2]], ignore.case = T)
  # extract different read data in dataset
  reads <- unname(unlist(lapply(1:length(time.ndx), function(x) input[time.ndx[x]-2, 1])))
  read.ndx <- time.ndx[!is.na(reads)]
  reads <- reads[!is.na(reads)]

  read.data <- list()
  ncol <- length(input[read.ndx[1],][!is.na(input[read.ndx[1],])])
  if(length(read.ndx)>1){
    # Extract all read tables except the last
    read.data <- lapply(1:(length(read.ndx)-1), function(x) input[read.ndx[x]:(read.ndx[x+1]-3),2:(ncol)])
    read.data <- lapply(1:length(read.data), function(x) as.data.frame(read.data[[x]])[1:length(read.data[[x]][,1][read.data[[x]][,1]!=0][!is.na(read.data[[x]][,1][read.data[[x]][,1]!=0])]),])
    # Extract last read table
    read.data[[length(read.ndx)]] <- data.frame(input[read.ndx[length(read.ndx)]:(read.ndx[length(read.ndx)]+length(read.data[[1]][[1]])-1),2:(ncol)])
    read.data[[length(read.ndx)]] <- as.data.frame(read.data[[length(read.ndx)]])[1:length(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0][!is.na(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0])]),]
  } else {
    read.data[[1]] <- data.frame(input[read.ndx:(read.ndx + match(NA, input[read.ndx:nrow(input),3])-2),2:(1+ncol)])
  }
  # Remove time points with NA in all samples
  for(i in 1:length(read.data))
    read.data[[i]] <- cbind(read.data[[i]][,1][1:length(read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ][, 2])],
                            read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ])
  if(length(read.ndx)>1){
    # give all reads the same time values as the first read
    for(i in 2:length(read.data)){
      read.data[[i]][[1]] <- read.data[[1]][[1]]
    }
  }
  names(read.data) <- reads
  data.ls <- list()
  if(length(reads)>1){

    answer <- readline(paste0("Indicate where the density data is stored?\n",
                              paste(unlist(lapply(1:length(reads), function (i)
                                paste0("[", i, "] ", reads[i]))),
                                collapse = "\n"), "\n[", length(reads)+1, "] Disregard density data\n"))
    if(as.numeric(answer) == length(reads)+1){
      density <- NA
    } else {
      density <- read.data[[as.numeric(answer)]]
    }

    answer <- readline(paste0("Indicate where the fluorescence 1 data is stored?\n",
                              paste(unlist(lapply(1:length(reads), function (i)
                                paste0("[", i, "] ", reads[i]))),
                                collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 1 data\n"))
    if(as.numeric(answer) == length(reads)+1){
      fluorescence1 <- NA
    } else {
      fluorescence1 <- read.data[[as.numeric(answer)]]
      fluorescence1[which(fluorescence1 == "OVRFLW", arr.ind = TRUE)] <- NA
    }
    data.ls[[1]] <- density
    data.ls[[2]] <- fluorescence1

    if(length(reads)>2){
      answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
                                paste(unlist(lapply(1:length(reads), function (i)
                                  paste0("[", i, "] ", reads[i]))),
                                  collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
      if(as.numeric(answer) == length(reads)+1){
        fluorescence2 <- NA
      } else {
        fluorescence2 <- read.data[[as.numeric(answer)]]
        fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
      }
      data.ls[[3]] <- fluorescence2
    }
  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    data.ls[[3]] <- NA
  }
  return(list(data.ls, read.data))
}

biosensor.eq <- function (x, y.min, y.max, K, n)
{
  y.min <- y.min[1]
  y.max <- y.max[1]
  K <- K[1]
  n <- n[1]
  if (is.numeric(x) == FALSE || length(x) < 4)
    stop("Need numeric vector with at least four elements for: x (i.e., inducer concentrations)")
  if (is.numeric(y.min) == FALSE)
    stop("Need numeric vector for: y.min")
  if (is.numeric(y.max) == FALSE)
    stop("Need numeric vector for: y.max")
  if (is.numeric(K) == FALSE)
    stop("Need numeric vector for: K")
  if (is.numeric(n) == FALSE)
    stop("Need numeric vector for: n")
  y <- y.min + (y.max - y.min) * ( x^n / (K^n + x^n) )
  biosensor.eq <- y
}



initbiosensor <- function (x, y, n)
{
  if (is.numeric(x) == FALSE || length(x) < 4)
    stop("Need numeric vector with at least four elements for: x (i.e., inducer concentrations)")
  if (is.numeric(y) == FALSE || length(y) < 4)
    stop("Need numeric vector with at least four elements for: y (i.e., promoter response)")
  y.min <- min(y)
  y.max <- max(y)
  lo <- suppressWarnings(loess(y~x, span = 0.5))
  xl <- seq(min(x),max(x), (max(x) - min(x))/500)
  yl <- predict(lo,xl)
  K <- xl[sample(1:(which.max(yl)*0.25), 1)]

  return(list(y.max = y.max, K = K, n = n))
}

export_Table <- function(table, out.dir = getwd(), out.nm = deparse(substitute(table)))
{
  out.nm <- gsub("\\.txt$", "", out.nm)
  dir.create(out.dir, showWarnings = F)
  utils::write.table(x = table,
                     file = paste(out.dir, paste0(out.nm, ".txt"), sep = "/"),
                     row.names = FALSE,
                     sep = "\t"
                     )
}

export_RData <- function(object, out.dir = getwd(), out.nm = class(object))
{
  # # an object of class grofit or flFitRes
  # if(is(grofit) != "grofit" || is(grofit) != "flFitRes") stop("grofit needs to be an object created with growth.workflow() or fl.workflow().")
  dir.create(out.dir, showWarnings = F)
  out.nm <- gsub("\\.RData$", "", out.nm)
  message(paste0("Save RData object to: '", gsub(getwd(), "", out.dir), "/", out.nm, ".RData'"))
  save(object, file = paste(out.dir, paste0(out.nm, ".RData"), sep = "/"))
}

# from the 'RobustLinearReg' package
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
  y1 <- matrix(y, n, n, byrow = T)
  y2 <- t(y1)
  x1 <- matrix(x, n, n, byrow = T)
  x2 <- t(x1)
  aux <- (y1 - y2)/(x1 - x2)
  a <- median(aux, na.rm = T)
  aux <- y - a * x
  b <- median(aux, na.rm = T)
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

# from the 'RobustLinearReg' package
siegel_regression <- function (formula, data = NULL)
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
  y1 <- matrix(y, n, n, byrow = T)
  y2 <- t(y1)
  x1 <- matrix(x, n, n, byrow = T)
  x2 <- t(x1)
  aux <- (y1 - y2)/(x1 - x2)
  a <- median(apply(aux, 1, median, na.rm = T))
  aux <- (x1 * y2 - x2 * y1)/(x1 - x2)
  b <- median(apply(aux, 1, median, na.rm = T))
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
