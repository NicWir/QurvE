utils::globalVariables(c("shiny"))

#' Run upon attaching package QurvE
#'
#'
#' @param libname library name
#' @param pkgname package name
#'
#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname)
    {
    k1 <- paste(
        "QurvE", utils::packageVersion("QurvE"),
        "initialized Successfully ! \nPlease cite:\nWirth, N.T., Funk, J., Donati, S. et al. QurvE: user-friendly software for the analysis of biological growth and fluorescence data. Nat Protoc (2023). https://doi.org/10.1038/s41596-023-00850-7"
    )
    k0 <- "\n\n"
    k2 <- paste("https://github.com/NicWir/QurvE")
    packageStartupMessage(c(k1, k0, k2))

    # github_version <- gsub('Version: ', '',
    # unlist(str_split(remotes:::github_DESCRIPTION(username
    # = 'NicWir', repo = 'QurvE', pat =
    # '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'),
    # '\\n'))[grep('Version',
    # unlist(str_split(remotes:::github_DESCRIPTION(username
    # = 'NicWir', repo = 'QurvE', pat =
    # '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'),
    # '\\n')))]) installed_version <-
    # paste(utils::packageVersion('QurvE'))
    # if(github_version > installed_version){
    # packageStartupMessage( paste0( 'Your
    # installed QurvE is outdated! A new version
    # (', github_version, ') is available on
    # Github. You can install the most recent
    # development version by installing package
    # 'devtools' and running:
    # devtools::install_github('NicWir/QurvE').'
    # ) ) }
}

suppress_warnings <- function(.expr, .f, ...)
    {
    eval.parent(
        substitute(
            withCallingHandlers(
                .expr, warning = function(w)
                  {
                  cm <- conditionMessage(w)
                  cond <- if (is.character(.f)) grepl(.f, cm) else (rlang::as_function(.f))(cm,
                    ...)
                  if (cond) invokeRestart("muffleWarning")
                }
            )
        )
    )
}

#' Apply data transformation/calibration to measurement values in a dataframe
#'
#' @param df An R dataframe object data in QurvE custom format.
#' The first three table rows contain
#' \enumerate{
#'    \item sample description
#'    \item replicate number (_optional_: followed by a letter to indicate technical replicates)
#'    \item concentration value (_optional_)
#' }
#' @param equation An equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert values.
#'
#' @return Dataframe \code{df} with values adjusted based on the provided equation.
#'
#' @keywords internal
#' @noRd
#'
calibrate <- function(df, equation)
  {
  #test if more than one time entity is present
  time.ndx <- grep("time", unlist(df[,1]), ignore.case = TRUE)
  calib <- parse(text = equation)
  if(length(time.ndx)==1){
    if(!is.null(nrow(df[-time.ndx, -(1:3)]))){
      x <- matrix(as.numeric(unlist(df[-time.ndx, -(1:3)])), nrow = nrow(df[-time.ndx, -(1:3)]))
    } else {
      x <- as.numeric(unlist(df[-time.ndx, -(1:3)]))
    }
    df[-time.ndx, -(1:3)] <- eval(calib)
  } else { # identify different datasets based on the occurence of multiple 'time' entities
    if(!is.null(nrow(df[-time.ndx, -(1:3)]))){
      x <-  matrix(as.numeric(unlist(df[(time.ndx[1]+1) : (time.ndx[2]-1), -(1:3)])),
                   nrow = nrow(df[(time.ndx[1]+1) : (time.ndx[2]-1), -(1:3)]))
    } else {
      x <- as.numeric(unlist(df[(time.ndx[1]+1) : (time.ndx[2]-1), -(1:3)]))
    }

    df[(time.ndx[1]+1) : (time.ndx[2]-1), -(1:3)] <- eval(calib)
    for (i in 2:(length(time.ndx))){
      x <- matrix(as.numeric(unlist(df[if (is.na(time.ndx[i + 1])) {
        (time.ndx[i] + 1):nrow(df)
      } else {
        (time.ndx[i] + 1):(time.ndx[i + 1] - 1)
      }, -(1:3)])), nrow = nrow(df[if (is.na(time.ndx[i + 1])) {
        (time.ndx[i] + 1):nrow(df)
      } else {
        (time.ndx[i] + 1):(time.ndx[i + 1] - 1)
      }, -(1:3)]))

      df[if (is.na(time.ndx[i + 1])) {
        (time.ndx[i] + 1):nrow(df)
      } else {
        (time.ndx[i] + 1):(time.ndx[i + 1] - 1)
      }, -(1:3)] <- eval(calib)
    } # end of for (i in 2:(length(time.ndx)))
  } # end of else of if(length(time.ndx)==1)
  return(df)
}


#' Pretty ticks for log scale in ggplot
#'
#' @param n Number of breaks
#'
#' @return a call to\code{\link[grDevices]{axisTicks}} to create pretty axis ticks.
#'
#' @keywords internal
#' @noRd
#'
base_breaks <- function(n = 10)
    {
    function(x)
        {
        grDevices::axisTicks(
            log10(range(x, na.rm = TRUE)),
            log = TRUE, nint = n
        )
    }
}



#' Find indices of maxima an minima in a data series
#'
#' @param x vector of values with minima and maxima
#' @param threshold Threshold to consider minima or maxima
#'
#' @return a list with 1. a vector of minima and 2. a vector of maxima.
#' @export
#' @author Evan Friedland
#' @examples
#' # Pick a desired threshold to plot up to
#' n <- 3
#' # Generate Data
#' randomwalk <- 100 + cumsum(rnorm(50, 0.2, 1)) # climbs upwards most of the time
#' bottoms <- lapply(1:n, function(x) inflect(randomwalk, threshold = x)$minima)
#' tops <- lapply(1:n, function(x) inflect(randomwalk, threshold = x)$maxima)
#' # Color functions
#' cf.1 <- grDevices::colorRampPalette(c('pink','red'))
#' cf.2 <- grDevices::colorRampPalette(c('cyan','blue'))
#' plot(randomwalk, type = 'l', main = 'Minima & Maxima\nVariable Thresholds')
#' for(i in 1:n){
#'   points(bottoms[[i]], randomwalk[bottoms[[i]]], pch = 16, col = cf.1(n)[i], cex = i/1.5)
#' }
#' for(i in 1:n){
#'   points(tops[[i]], randomwalk[tops[[i]]], pch = 16, col = cf.2(n)[i], cex = i/1.5)
#' }
#' legend('topleft', legend = c('Minima',1:n,'Maxima',1:n),
#'        pch = rep(c(NA, rep(16,n)), 2), col = c(1, cf.1(n),1, cf.2(n)),
#'        pt.cex =  c(rep(c(1, c(1:n) / 1.5), 2)), cex = .75, ncol = 2)
#'
inflect <- function(x, threshold = 1)
    {
    up <- sapply(
        1:threshold, function(n) c(
            x[-(seq(n))],
            rep(NA, n)
        )
    )
    down <- sapply(
        -1:-threshold, function(n) c(
            rep(NA, abs(n)),
            x[-seq(
                length(x),
                length(x) -
                  abs(n) +
                  1
            )]
        )
    )
    a <- cbind(x, up, down)
    list(
        minima = which(
            apply(a, 1, min) ==
                a[, 1]
        ),
        maxima = which(
            apply(a, 1, max) ==
                a[, 1]
        )
    )
}

#' Combine two dataframes like a zip-fastener
#'
#' Combine rows or columns of two dataframes in an alternating manner
#'
#' @param df1 A first dataframe.
#' @param df2 A second dataframe with the same dimensions as \code{df1}.
#' @param along \code{1} to alternate rows or \code{2} to alternate columns.
#'
#' @return A dataframe with combined rows (or columns) of df1 and df2.
#' @export
#' @author Mark Heckmann
#'
#' @examples
#' # data frames equal dimensions
#' df1 <- plyr::rdply(3, rep('o',4))[ ,-1]
#' df2 <- plyr::rdply(3, rep('X',4))[ ,-1]
#'
#' zipFastener(df1, df2)
#' zipFastener(df1, df2, 2)
#' zipFastener(df1, df2, 1)
#'
#' # data frames unequal in no. of rows
#' df1 <- plyr::rdply(10, rep('o',4))[ ,-1]
#' zipFastener(df1, df2, 1)
#' zipFastener(df2, df1, 1)
#'
#' # data frames unequal in no. of columns
#' df2 <- plyr::rdply(10, rep('X',3))[ ,-1]
#' zipFastener(df1, df2)
#' zipFastener(df2, df1, 2)
#'
zipFastener <- function(df1, df2, along = 2)
    {
    # parameter checking
    if (!is.element(along, c(1, 2)))
        {
        stop(
            "along must be 1 or 2 for rows and columns
                                              respectively"
        )
    }
    # if merged by using zip feeding along the
    # columns, the same no. of rows is required
    # and vice versa
    if (along == 1 & (ncol(df1) !=
        ncol(df2)))
            {
        stop(
            "the no. of columns has to be equal to merge
               them by zip feeding"
        )
    }
    if (along == 2 & (nrow(df1) !=
        nrow(df2)))
            {
        stop(
            "the no. of rows has to be equal to merge them by
               zip feeding"
        )
    }

    # zip fastener preperations
    d1 <- dim(df1)[along]
    d2 <- dim(df2)[along]
    i1 <- 1:d1  # index vector 1
    i2 <- 1:d2 + d1  # index vector 2

    # set biggest dimension dMax
    if (d1 == d2)
    {
        dMax <- d1
    } else if (d1 > d2)
    {
        length(i2) <- length(i1)  # make vectors same length,
        dMax <- d1  # fill blanks with NAs
    } else if (d1 < d2)
    {
        length(i1) <- length(i2)  # make vectors same length,
        dMax <- d2  # fill blanks with NAs
    }

    # zip fastener operations
    index <- as.vector(
        matrix(
            c(i1, i2),
            ncol = dMax, byrow = T
        )
    )
    index <- index[!is.na(index)]  # remove NAs

    if (along == 1)
    {
        colnames(df2) <- colnames(df1)  # keep 1st colnames
        res <- rbind(df1, df2)[index,
            ]  # reorder data frame
    }
    if (along == 2)
        res <- cbind(df1, df2)[,
            index]

    return(res)
}

#' Internal function used to fit a biosensor response model with \code{\link[minpack.lm]{nlsLM}}
#'
#' Calculates the values of biosensor response model for given time points and response parameters.
#'
#' @param x A vector of concentration values
#' @param y.min The minimum fluorescence value
#' @param y.max The maximum fluorescence value
#' @param K Sensitivity parameter
#' @param n Cooperativity parameter
#'
#' @export
#' @return A vector of fluorescence values
#'
#' @references Meyer, A.J., Segall-Shapiro, T.H., Glassey, E. et al. _Escherichia coli “Marionette” strains with 12 highly optimized small-molecule sensors._ Nat Chem Biol 15, 196–204 (2019). DOI: 10.1038/s41589-018-0168-3
#' @examples
#' n <- seq(1:10)
#' conc <- rev(10*(1/2)^n)
#' fit <- biosensor.eq(conc, 300, 82000, 0.85, 2)
#'
biosensor.eq <- function(x, y.min, y.max, K, n)
    {
    y.min <- y.min[1]
    y.max <- y.max[1]
    K <- K[1]
    n <- n[1]
    if (is.numeric(x) ==
        FALSE || length(x) <
        4)
        stop(
            "Need numeric vector with at least four elements for: x (i.e., inducer concentrations)"
        )
    if (is.numeric(y.min) ==
        FALSE)
        stop("Need numeric vector for: y.min")
    if (is.numeric(y.max) ==
        FALSE)
        stop("Need numeric vector for: y.max")
    if (is.numeric(K) ==
        FALSE)
        stop("Need numeric vector for: K")
    if (is.numeric(n) ==
        FALSE)
        stop("Need numeric vector for: n")
    y <- y.min + (y.max - y.min) * (x^n/(K^n + x^n))
    biosensor.eq <- y
}

#' Generate initial values for parameter estimation with the biosensor response model
#'
#' @param x A vector of concentration values with at least four elements.
#' @param y A vector of response values with at least four elements.
#' @param n Cooperativity parameter
#'
#' @return A list:
#' \item{y.max}{Maximum response value.}
#' \item{K}{Sensitivity parameter.}
#' \item{n}{The initially defined cooperativity parameter.}
#'
#'
#' @keywords internal
#' @noRd
#'
initbiosensor <- function(x, y, n)
    {
    if (is.numeric(x) ==
        FALSE || length(x) <
        4)
        stop(
            "Need numeric vector with at least four elements for: x (i.e., inducer concentrations)"
        )
    if (is.numeric(y) ==
        FALSE || length(y) <
        4)
        stop(
            "Need numeric vector with at least four elements for: y (i.e., promoter response)"
        )
    y.min <- min(y)
    y.max <- max(y)
    lo <- suppressWarnings(loess(y ~ x, span = 0.5))
    xl <- seq(
        min(x),
        max(x),
        (max(x) -
            min(x))/500
    )
    yl <- predict(lo, xl)
    K <- xl[sample(
        1:(which.max(yl) *
            0.25), 1
    )]

    return(list(y.max = y.max, K = K, n = n))
}

#' Export a tabular object as tab-separated .txt file
#'
#' @param table A tabular R object (dataframe, matrix, array)
#' @param out.dir The path to the output directory. Default: the working directory
#' @param out.nm The output filename (with or without '.txt' ending). Default: the name of \code{table} followed by '.txt'.
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' if(interactive()){
#' df <- data.frame('A' = seq(1:10), 'B' = rev(seq(1:10)))
#'
#' export_Table(df)
#' }
export_Table <- function(
    table, out.dir = tempdir(), out.nm = deparse(substitute(table))
)
    {
    out.nm <- gsub("\\.txt$", "", out.nm)
    dir.create(out.dir, showWarnings = FALSE)
    utils::write.table(
        x = table, file = paste(
            out.dir, paste0(out.nm, ".txt"),
            sep = "/"
        ),
        row.names = FALSE, sep = "\t"
    )
}

#' Export an R object as .RData file
#'
#' @param object An R object.
#' @param out.dir The path to the output directory. Default: the working directory
#' @param out.nm The output filename (with or without '.RData' ending). Default: the class of \code{object} followed by '.RData'.
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' if(interactive()){
#' df <- data.frame('A' = seq(1:10), 'B' = rev(seq(1:10)))
#'
#' export_RData(df)
#' }
export_RData <- function(
    object, out.dir = tempdir(), out.nm = class(object)
)
    {
    # # an object of class grofit or flFitRes
    # if(is(grofit) != 'grofit' || is(grofit) !=
    # 'flFitRes') stop('grofit needs to be an
    # object created with growth.workflow() or
    # fl.workflow().')
    dir.create(out.dir, showWarnings = FALSE)
    out.nm <- gsub("\\.RData$", "", out.nm)
    message(
        paste0(
            "Save RData object to: \n'", "...", gsub(getwd(), "", out.dir),
            "/", out.nm, ".RData'"
        )
    )
    save(
        object, file = paste(
            out.dir, paste0(out.nm, ".RData"),
            sep = "/"
        )
    )
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
#' @keywords internal
#' @noRd
match_arg <- function(
    arg, choices, multiple = FALSE, ignore_case = TRUE,
    trim_ws = FALSE
)
    {
    if (missing(choices))
        {
        formal.args <- formals(sys.function(sysP <- sys.parent()))
        choices <- eval(
            formal.args[[as.character(substitute(arg))]],
            envir = sys.frame(sysP)
        )
    }
    if (is.null(arg))
        return(choices[1L]) else if (!is.character(arg))
        stop("'arg' must be NULL or a character vector")
    if (!multiple)
    {
        if (identical(arg, choices))
            return(arg[1L])
        if (length(arg) >
            1L)
            stop("'arg' must be of length 1")
    } else if (length(arg) ==
        0L)
        stop("'arg' must be of length >= 1")
    if (trim_ws)
    {
        arg <- trim_ws(arg)
    }
    if (ignore_case)
    {
        arg <- tolower(arg)
        choices_ <- choices
        choices <- tolower(choices)
    }
    i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
    if (all(i == 0L))
        stop(
            gettextf(
                "'arg' should be one of %s", paste(
                  dQuote(choices),
                  collapse = ", "
              )
            ),
            domain = NA
        )
    i <- i[i > 0L]
    if (!multiple && length(i) >
        1)
        stop("there is more than one match in 'match.arg'")
    if (ignore_case)
    {
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
#' \item growth (previously granularity) - how close to the number of
#' ticks do you get (default is 5)
#' \item legibility - has to do with fontsize and formatting to prevent
#' label overlap
#' }
#'
#' @references Talbot, Justin, Sharon Lin, and Pat Hanrahan.
#' 'An extension of Wilkinson’s algorithm for positioning tick labels on axes.' IEEE Transactions
#' on visualization and computer graphics 16.6 (2010): 1036-1043.
#'
#' @param data_range range of the data
#'
#' @return numeric vector of breaks
#'
#' @author Andrew Stein
#'
#' @importFrom labeling extended
#'
#' @keywords internal
#' @noRd
xgx_breaks_log10 <- function(data_range)
    {
    data_min <- min(log10(data_range))
    data_max <- max(log10(data_range))
    n_breaks <- 5  # number of breaks to aim for
    # preferred breaks, in log10-space
    preferred_increment <- c(1, 0.5)

    breaks <- labeling::extended(
        data_min, data_max, n_breaks, Q = preferred_increment
    )
    breaks <- 10^breaks

    # ensure that there are at least 2 breaks but
    # also try to present 'nice' breaks with only
    # one significant digit
    breaks1 <- unique(signif(breaks, 1))
    breaks2 <- unique(signif(breaks, 2))
    breaks3 <- unique(signif(breaks, 3))
    if (length(breaks1) >=
        2)
        {
        breaks_out <- breaks1
    } else if (length(breaks2) >=
        2)
        {
        breaks_out <- breaks2
    } else if (length(breaks3) >=
        2)
        {
        breaks_out <- breaks3
    } else
    {
        breaks_out <- unique(breaks)
    }
    return(breaks_out)
}

#' Sets the default minor_breaks for log10 scales (from R package 'xgxr')
#'
#' \code{xgx_minor_breaks_log10} sets nice minor_breaks for log10 scale.
#'
#' @param data_range range of the data
#'
#' @return numeric vector of breaks
#'
#' @importFrom labeling extended
#'
#' @author Andrew Stein
#'
#' @keywords internal
#' @noRd
xgx_minor_breaks_log10 <- function(data_range)
    {
    r1 <- range(log10(data_range))
    r <- r1
    r[1] <- floor(r[1])
    r[2] <- ceiling(r[2]) +
        1
    minor_breaks <- c()
    for (i in seq(r[1], r[2]))
        {
        minor_breaks <- c(
            minor_breaks, seq(
                2 * 10^(i - 1), 10^i - 10^(i - 1),
                by = 10^(i - 1)
            )
        )
    }
    minor_breaks <- minor_breaks[minor_breaks <= 10^r1[2]]
    minor_breaks <- minor_breaks[minor_breaks >= 10^r1[1]]
    return(minor_breaks)
}

#' The function calls the \code{baranyi} function to generate curves between time zero and \code{t} and adds some random noise to the x- and y-axes. The three growth parameters given as input values will be slightly changed to produce different growth curves. The resulting datasets can be used to test the \code{\link{growth.workflow}} function.
#'
#' @param d  Numeric value, number of data sets. If \code{d} is a vector, only the first entry is used.
#' @param y0  Numeric value, start growth. If \code{t} is a vector, only the first entry is used.
#' @param tmax  Numeric value, number of time points per data set. If \code{t} is a vector, only the first entry is used.
#' @param mu Numeric value, maximum slope. If \code{mu} is a vector, only the first entry is used.
#' @param lambda Numeric value, lag-phase. If \code{lambda} is a vector, only the first entry is used.
#' @param A Numeric value, maximum growth. If \code{A} is a vector, only the first entry is used.
#' @param label Character string, condition label  If \code{label} is a vector, only the first entry is used.
#'
#' @return A list containing simulated data for three tests (e.g., 'organisms'):
#' \item{time}{numeric matrix of size \code{d}x\code{t}, each row represent the time points for which growth data is simulated and stored in each row of \code{data}.}
#' \item{data}{data.frame of size \code{d}x(3+\code{t}), 1. column, character as an experiment identifier; 2. column: Replicate number; 3. column: concentration of substrate of a compound under which the experiment is obtained; 4.-(3+t). column: growth data corresponding to the time points in \code{time}.}
#'
#' @export
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @examples
#' # Create random growth data set
#' rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = 'Test1')
#' rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = 'Test2')
#'
#' rnd.data <- list()
#' rnd.data[['time']] <- rbind(rnd.data1$time, rnd.data2$time)
#' rnd.data[['data']] <- rbind(rnd.data1$data, rnd.data2$data)
#'
#' # Run growth curve analysis workflow
#' gcFit <- growth.gcFit(time = rnd.data$time,
#'                        data = rnd.data$data,
#'                        parallelize = FALSE,
#'                        control = growth.control(fit.opt = 's',
#'                                                 suppress.messages = TRUE))
#'
#' \donttest{
#' # Perform dose-response analysis
#' drFit <- growth.drFit(gcTable = gcFit$gcTable,
#'              control = growth.control(dr.parameter = 'mu.spline'))
#'
#' # Inspect results
#' summary(drFit)
#' plot(drFit)
#' }
#'
rdm.data <- function(
    d, y0 = 0.05, tmax = 24, mu = 0.6, lambda = 5,
    A = 3, label = "Test1"
)
    {
    many.data <- d <- d[1]
    max.time <- tmax <- tmax[1]
    mu <- mu[1]
    y0 <- y0[1]
    lambda <- lambda[1]
    A <- A[1]
    label <- label[1]

    # /// check input
    if (is.numeric(many.data) ==
        FALSE)
        stop("Need numeric value for: d")
    if (many.data < 30)
        stop("Need number of datasets d > 30")
    if (is.numeric(max.time) ==
        FALSE)
        stop("Need numeric value for: t")
    if (is.numeric(mu) ==
        FALSE)
        stop("Need numeric value for: mu")
    if (is.numeric(y0) ==
        FALSE)
        stop("Need numeric value for: y0")
    if (is.numeric(lambda) ==
        FALSE)
        stop("Need numeric value for: lambda")
    if (is.numeric(A) ==
        FALSE)
        stop("Need numeric value for: A")

    # /// set up time matrix
    time <- t(
        array(
            data = rep(
                seq(from = 0, to = max.time, by = 0.25),
                many.data
            ),
            dim = c(max.time * 4 + 1, many.data)
        )
    )

    # /// set up growth data array
    data <- data.frame(
        array(
            data = 0, dim = c(many.data, (max.time * 4 + 1 + 3))
        )
    )

    # /// number of datasets per test
    nT1 <- round(d/3)
    nT2 <- round(d/3)
    nT3 <- d - nT1 - nT2
    test1 <- rep(label, nT1)
    test2 <- rep(label, nT2)
    test3 <- rep(label, nT3)

    # /// concentrations + noise
    conc1 <- signif(
        c(
            0, rev(
                unlist(
                  lapply(
                    1:(nT1 - 2), function(x) 1 *
                      (2/3)^x
                )
              )
            ),
            1
        ),
        digits = 2
    )
    conc2 <- signif(
        c(
            0, rev(
                unlist(
                  lapply(
                    1:(nT2 - 2), function(x) 1 *
                      (2/3)^x
                )
              )
            ),
            1
        ),
        digits = 2
    )
    conc3 <- signif(
        c(
            0, rev(
                unlist(
                  lapply(
                    1:(nT3 - 2), function(x) 1 *
                      (2/3)^x
                )
              )
            ),
            1
        ),
        digits = 2
    )

    data[, 1] <- (c(test1, test2, test3))
    data[, 2] <- (c(
        rep(1, length(conc1)),
        rep(2, length(conc2)),
        rep(3, length(conc3))
    ))
    data[, 3] <- (c(conc1, conc2, conc3))

    # /// set parameter changes
    mu.loop <- mu - mu * data[, 3]^1.5/((mu/2)^2 +
        data[, 3]^2) + stats::runif(d, -0.01, 0.01)
    mu.loop <- abs(mu.loop)
    mu.loop[c(
        length(test1),
        (length(test1) +
            length(test2)),
        (length(test1) +
            length(test2) +
            length(test3))
    )] <- 0.5 *
        mu.loop[c(
            length(test1) -
                1, (length(test1) +
                length(test2)) -
                1, (length(test1) +
                length(test2) +
                length(test3)) -
                1
        )]
    A.loop <- abs(
        A - 1 * A * (0.001 + data[, 3])^(0.5) + stats::runif(d, -0.1, 0.1)
    )
    lambda.loop <- lambda + 0.6 * lambda * data[, 3]^(0.7) +
        stats::runif(d, -0.3, 0.3)
    y0.loop <- rep(y0, nrow(data)) +
        stats::runif(
            nrow(data),
            -0.005, 0.005
        )


    # /// create growth data
    for (i in 1:many.data)
    {
        data[i, 4:ncol(data)] <- baranyi(
            time = time[i, ], A = A.loop[i], mu = mu.loop[i],
            lambda = lambda.loop[i], addpar = y0.loop[i]
        ) +
            0.003 * A * stats::rnorm(max.time * 4 + 1)
    }

    # /// add information columns
    data <- data.frame(data)

    # make non-log transformed data
    for (i in 1:many.data)
    {
        data[i, 4:ncol(data)] <- y0.loop[i] *
            exp(data[i, 4:ncol(data)])
    }
    # for (i in 1:many.data){
    # data[i,4:ncol(data)] <-
    # data[i,4:ncol(data)] *
    # (A.loop[i]*data[i,4:ncol(data)]) + (1/70 *
    # A.loop[i]) }


    # data[,2] <- (rep(c('A', 'B', 'C', 'D', 'E',
    # 'F', 'G'), many.data ))[1:many.data]

    rdm.data <- list(data = data, time = time)

}

single_hue_palettes <- list(
    Green = RColorBrewer::brewer.pal(9, "Greens")[c(2:9)],
    Orange = RColorBrewer::brewer.pal(9, "Oranges")[c(2:8)],
    Purple = RColorBrewer::brewer.pal(9, "Purples")[c(3:9)],
    Magenta = c(
        "#FFCAF3", "#ffb4e2", "#ff67c4", "#ff009b",
        "#c30076"
    ),
    Grey = RColorBrewer::brewer.pal(9, "Greys")[c(2:8)],
    Blue = RColorBrewer::brewer.pal(9, "Blues")[c(2:8)],
    Grey = RColorBrewer::brewer.pal(9, "Greys")[c(2:8)],
    Red = rev(grDevices::hcl.colors(n = 10, palette = "Reds"))[-(1:2)],
    Cyan = c(
        "#CCFFFC", "#AAf2EF", "#75E9E5", "#3CDDDC",
        "#00d5d6", "#00ccd3", "#00bcc0", "#00a6a7",
        "#00918f", "#009999"
    ),
    Brown = rev(grDevices::hcl.colors(n = 8, palette = "BrwnYl")),
    Mint = grDevices::hcl.colors(n = 7, palette = "Mint")
)

big_palette <- c(
    "dodgerblue2", "#E31A1C", "green4", "#6A3D9A",
    "#FF7F00", "black", "gold1", "skyblue2", "#FB9A99",
    "palegreen2", "#CAB2D6", "#FDBF6F", "gray70", "khaki2",
    "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
    "darkturquoise", "green1", "yellow4", "yellow3",
    "darkorange4", "brown", "dodgerblue2", "#E31A1C",
    "green4", "#6A3D9A", "#FF7F00", "black", "gold1",
    "skyblue2", "#FB9A99", "palegreen2", "#CAB2D6",
    "#FDBF6F", "gray70", "khaki2", "maroon", "orchid1",
    "deeppink1", "blue1", "steelblue4", "darkturquoise",
    "green1", "yellow4", "yellow3", "darkorange4", "brown")
