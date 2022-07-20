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
    ret <- paste("\\textcolor{", color, "}{", gsub("_", "\\\\_", x), "}", sep = "")
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
      write.csv(dat, file, row.names = row.names)
    },
    warning = function(w) {
      print(w)
      write.csv(dat, file, row.names = row.names)
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
      dat <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = F, sheet = sheet)))
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


