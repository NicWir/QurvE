utils::globalVariables(c("shiny"))

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

  # github_version <- gsub("Version: ", "", unlist(str_split(remotes:::github_DESCRIPTION(username = "NicWir", repo = "QurvE", pat = "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"), "\\n"))[grep("Version", unlist(str_split(remotes:::github_DESCRIPTION(username = "NicWir", repo = "QurvE", pat = "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"), "\\n")))])
  # installed_version <- paste(utils::packageVersion("QurvE"))
  # if(github_version > installed_version){
  #   packageStartupMessage(
  #     paste0(
  #       "Your installed QurvE is outdated! A new version (",
  #       github_version,
  #       ") is available on Github. You can install the most recent development version by executing: devtools::install_github('NicWir/QurvE')."
  #     )
  #   )
  # }
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


#' Pretty ticks for log scale for ggplot2
#'
#' @param n Number of breaks
#'
#' @return

base_breaks <- function(n = 10){
  function(x) {
    grDevices::axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
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

#' Internal function used to fit a biosensor response model with minpack.lm::nlsLM()
#'
#' @param x A vector of concentration values
#' @param y.min The minimum fluorescence value
#' @param y.max The maximum fluorescence value
#' @param K Sensitivity parameter
#' @param n Cooperativity parameter
#'
#' @references Meyer, A.J., Segall-Shapiro, T.H., Glassey, E. et al. _Escherichia coli “Marionette” strains with 12 highly optimized small-molecule sensors._ Nat Chem Biol 15, 196–204 (2019). DOI: 10.1038/s41589-018-0168-3
#'
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
  message(paste0("Save RData object to: \n'", "...", gsub(getwd(), "", out.dir), "/", out.nm, ".RData'"))
  save(object, file = paste(out.dir, paste0(out.nm, ".RData"), sep = "/"))
}

#' match arg to choices
#'
#' Wrapper around \link[base]{match.arg} that defaults to ignoring case and
#'   trimming white space
#'
#' @param arg a character vector (of length one unless several.ok is TRUE) or
#'   NULL
#' @param choices a character vector of candidate values
#' @param multiple logical specifying if arg should be allowed to have more than
#'   one element. Defaults to FALSE
#' @param ignore_case logical indicating whether to ignore capitalization.
#'   Defaults to TRUE
#' @param trim_ws logical indicating whether to trim surrounding white space.
#'   Defaults to TRUE
#' @return Value(s) matched via partial matching.
#'
#' @author Alban Sagouis
match_arg <- function(arg, choices,
                      multiple = FALSE,
                      ignore_case = TRUE,
                      trim_ws = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]],
                    envir = sys.frame(sysP))
  }
  if (is.null(arg))
    return(choices[1L])
  else if (!is.character(arg))
    stop("'arg' must be NULL or a character vector")
  if (!multiple) {
    if (identical(arg, choices))
      return(arg[1L])
    if (length(arg) > 1L)
      stop("'arg' must be of length 1")
  }
  else if (length(arg) == 0L)
    stop("'arg' must be of length >= 1")
  if (trim_ws) {
    arg <- trim_ws(arg)
  }
  if (ignore_case) {
    arg <- tolower(arg)
    choices_ <- choices
    choices <- tolower(choices)
  }
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L))
    stop(gettextf("'arg' should be one of %s",
                  paste(dQuote(choices), collapse = ", ")),
         domain = NA)
  i <- i[i > 0L]
  if (!multiple && length(i) > 1)
    stop("there is more than one match in 'match.arg'")
  if (ignore_case) {
    choices <- choices_
  }
  choices[i]
}



#' Sets the default breaks for log10 (from R package 'xgxr')
#'
#' \code{xgx_breaks_log10} sets nice breaks for log10 scale.
#' it's better than the default function because it ensures there is at least
#' 2 breaks
#' and also, it will try to go by 3s (i.e. 1,3,10,30,100) if it makes sense
#'
#' for the extended breaks function, weights is a set of 4 weights for
#' \enumerate{
#' \item simplicity - how early in the Q order are you
#' \item coverage - labelings that don't extend outside the data:
#' range(data) / range(labels)
#' \item density (previously granularity) - how close to the number of
#' ticks do you get (default is 5)
#' \item legibility - has to do with fontsize and formatting to prevent
#' label overlap
#' }
#'
#' @references Talbot, Justin, Sharon Lin, and Pat Hanrahan.
#' "An extension of Wilkinson’s algorithm for positioning tick labels on axes." IEEE Transactions
#' on visualization and computer graphics 16.6 (2010): 1036-1043.
#'
#' @param data_range range of the data
#'
#' @return numeric vector of breaks
#'
#' @importFrom labeling extended
#'
xgx_breaks_log10 <-  function(data_range) {
  data_min <- min(log10(data_range))
  data_max <- max(log10(data_range))
  n_breaks <- 5   # number of breaks to aim for
  # preferred breaks, in log10-space
  preferred_increment <- c(1, 0.5)

  breaks <- labeling::extended(data_min, data_max, n_breaks, Q = preferred_increment)
  breaks <- 10^breaks

  # ensure that there are at least 2 breaks
  # but also try to present "nice" breaks with only one significant digit
  breaks1 <- unique(signif(breaks, 1))
  breaks2 <- unique(signif(breaks, 2))
  breaks3 <- unique(signif(breaks, 3))
  if (length(breaks1) >= 2) {
    breaks_out <- breaks1
  } else if (length(breaks2) >= 2) {
    breaks_out <- breaks2
  } else if (length(breaks3) >= 2) {
    breaks_out <- breaks3
  } else {
    breaks_out <- unique(breaks)
  }
  return(breaks_out)
}

#' Sets the default minor_breaks for log10 scales (from R package 'xgxr')
#'
#' \code{xgx_minor_breaks_log10} sets nice minor_breaks for log10 scale.
#'
#'
#' @param data_range range of the data
#'
#' @return numeric vector of breaks
#'
#' @importFrom labeling extended
#'
xgx_minor_breaks_log10 <-  function(data_range) {
  r1 <- range(log10(data_range))
  r <-  r1
  r[1] <-  floor(r[1])
  r[2] <-  ceiling(r[2]) + 1
  minor_breaks <- c()
  for (i in seq(r[1], r[2])) {
    minor_breaks <-  c(minor_breaks, seq(2 * 10^(i - 1), 10^i - 10^(i - 1),
                                         by = 10^(i - 1)))
  }
  minor_breaks <-  minor_breaks[minor_breaks <= 10^r1[2]]
  minor_breaks <-  minor_breaks[minor_breaks >= 10^r1[1]]
  return(minor_breaks)
}


