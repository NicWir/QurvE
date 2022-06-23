#' Run upon attaching package VisomX
#'
#' Changes debug option for package \code{rgl} to avoid Rstudio crashing upon attaching it and prints welcome message
#'
#' @param libname library name
#' @param pkgname package name
.onAttach <- function (libname, pkgname){
  options(rgl.debug = TRUE)
  k1 <- paste("curvE",utils::packageVersion( "curvE"),"initialized Successfully !")
  k0 <- "\n"
  k2 <- paste("https://github.com/NicWir/curvE")
  packageStartupMessage(c(k1,k0,k2))
  if(!tinytex::is_tinytex()){
    packageStartupMessage("TinyTex was not found on your system. To ensure full functionality of Visomx, please execute tinytex::install_tinytex().")
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

read_file <- function(filename, csvsep = ";"){
  if (file.exists(filename)) {
    if (str_replace_all(filename, ".{1,}\\.", "") == "csv") {
      dat <-
        utils::read.csv(
          filename,
          sep = csvsep,
          header = T,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (str_replace_all(filename, ".{1,}\\.", "") == "xls" |
               str_replace(filename, ".{1,}\\.", "") == "xlsx") {
      dat <- readxl::read_excel(filename)
    } else if (str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
      dat <-
        utils::read.csv(
          filename,
          sep = "\t",
          header = T,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F
        )
    } else if (str_replace_all(filename, ".{1,}\\.", "") == "txt") {
      dat <-
        utils::read.table(
          filename,
          sep = "\t",
          header = T,
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
  } # if (file.exists(filename))
  else {
    stop(paste0("File \"", filename, "\" does not exist."), call. = F)
  }
  return(dat)
}

