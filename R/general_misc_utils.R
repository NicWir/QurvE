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
  if(!tinytex::is_tinytex()){
    packageStartupMessage("TinyTex was not found on your system. To ensure full functionality of QurvE, please execute tinytex::install_tinytex().")
  }

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
      ncols <- max(utils::count.fields(filename, sep = csvsep))
      dat <-
        utils::read.csv(
          filename,
          dec = dec,
          sep = csvsep,
          blank.lines.skip = FALSE,
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F,
          col.names = paste0("V", seq_len(ncols))
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
               stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx") {
      dat <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = F, sheet = sheet, progress = TRUE)))
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
      ncols <- max(utils::count.fields(filename))
      dat <-
        utils::read.csv(
          filename,
          dec = dec,
          blank.lines.skip = FALSE,
          sep = "\t",
          header = F,
          stringsAsFactors = F,
          fill = T,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = F,
          col.names = paste0("V", seq_len(ncols))
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "txt") {
      ncols <- max(utils::count.fields(filename))
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
          check.names = F,
          col.names = paste0("V", seq_len(ncols))
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
#' @importFrom readxl read_excel excel_sheets
#'
#' @import shiny doParallel knitr
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

  well_format <- str_extract(input[grep("Plate Type", input[,1]), 2], pattern = "[[:digit:]]+")

  if(as.numeric(well_format) > 96){
    # Combine data tables with identical read name (for > 96-well plates )
    unique_reads <- unique(reads)
    read.data.combined <- list()
    for(i in 1:length(unique_reads)){
      identical.ndx <- which(names(read.data) %in% unique_reads[i])
      selected_reads <- lapply(1:length(read.data[identical.ndx]), function(x)
        read.data[[identical.ndx[x]]][, -(1:2)])
      read.data.combined[[i]] <- do.call(cbind, selected_reads)
      read.data.combined[[i]] <- cbind(read.data[[identical.ndx[1]]][,1:2], read.data.combined[[i]])
    }
    names(read.data.combined) <- unique_reads
    read.data <- read.data.combined
  }

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

    answer <- readline(paste0("Indicate where the fluorescence data is stored?\n",
                              paste(unlist(lapply(1:length(reads), function (i)
                                paste0("[", i, "] ", reads[i]))),
                                collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence data\n"))
    if(as.numeric(answer) == length(reads)+1){
      fluorescence <- NA
    } else {
      fluorescence <- read.data[[as.numeric(answer)]]
      fluorescence[which(fluorescence == "OVRFLW", arr.ind = TRUE)] <- NA
    }
    data.ls[[1]] <- density
    data.ls[[2]] <- fluorescence

    # if(length(reads)>2){
    #   answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
    #                             paste(unlist(lapply(1:length(reads), function (i)
    #                               paste0("[", i, "] ", reads[i]))),
    #                               collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
    #   if(as.numeric(answer) == length(reads)+1){
    #     fluorescence2 <- NA
    #   } else {
    #     fluorescence2 <- read.data[[as.numeric(answer)]]
    #     fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
    #   }
    #   data.ls[[3]] <- fluorescence2
    # }
  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }
  return(list(data.ls))
}

parse_chibio <- function(input)
{
  time.ndx <- grep("time", input[1,], ignore.case = T)
  read.ndx <- grep("measured|emit", input[1,], ignore.case = T)
  reads <- input[1, read.ndx]

  data.ls <- list()
  if(length(reads)>1){

    answer <- readline(paste0("Indicate where the density data is stored?\n",
                              paste(unlist(lapply(1:length(reads), function (i)
                                paste0("[", i, "] ", reads[i]))),
                                collapse = "\n"), "\n[", length(reads)+1, "] Disregard density data\n"))
    if(as.numeric(answer) == length(reads)+1){
      density <- NA
    } else {
      density <- data.frame("time" = input[, time.ndx], "density" = c(input[1, read.ndx[as.numeric(answer)]], as.numeric(input[-1, read.ndx[as.numeric(answer)]])))
      if(all(as.numeric(density[-1,2]) == 0) | all(is.na(density[-1,2]))){
        density <- NA
      }
    }

    answer <- readline(paste0("Indicate where the fluorescence data is stored?\n",
                              paste(unlist(lapply(1:length(reads), function (i)
                                paste0("[", i, "] ", reads[i]))),
                                collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence data\n"))
    if(as.numeric(answer) == length(reads)+1){
      fluorescence <- NA
    } else {
      fluorescence <- data.frame("time" = input[, time.ndx], "fluorescence" = c(input[1, read.ndx[as.numeric(answer)]], as.numeric(input[-1, read.ndx[as.numeric(answer)]])))
      fluorescence[which(fluorescence == "OVRFLW", arr.ind = TRUE)] <- NA
      if(all(as.numeric(fluorescence[-1,2]) == 0) || all(is.na(fluorescence[-1,2]))){
        fluorescence <- NA
      }
    }
    data.ls[[1]] <- density
    data.ls[[2]] <- fluorescence

    # if(length(reads)>2){
    #   answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
    #                             paste(unlist(lapply(1:length(reads), function (i)
    #                               paste0("[", i, "] ", reads[i]))),
    #                               collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
    #   if(as.numeric(answer) == length(reads)+1){
    #     fluorescence2 <- NA
    #   } else {
    #     fluorescence2 <- data.frame("time" = input[, time.ndx], "fluorescence2" = c(input[1, read.ndx[as.numeric(answer)]], as.numeric(input[-1, read.ndx[as.numeric(answer)]])))
    #     fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
    #     if(all(as.numeric(fluorescence2[-1,2]) == 0) || all(is.na(fluorescence2[-1,2]))){
    #       fluorescence2 <- NA
    #     }
    #   }
    #   data.ls[[3]] <- fluorescence2
    # }
  } else {
    density <- data.frame("time" = input[, time.ndx], "density" = c(input[1, read.ndx], as.numeric(input[-1, read.ndx])))
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }

  return(list(data.ls))
}

parse_growthprofiler <- function(input)
{
  # get row numbers for "time" in column 1
  time.ndx <- grep("^\\btime\\b", input[[1]], ignore.case = T)
  # get index of empty column at the and of the data series
  na.ndx <- which(is.na(input[time.ndx,]))

  density <- input[time.ndx:nrow(input), 1:(na.ndx-1)]

  data.ls <- list()
  data.ls[[1]] <- density
  data.ls[[2]] <- NA
  # data.ls[[3]] <- NA

  return(list(data.ls))
}

parse_tecan <- function(input)
{
  # get row numbers for "time" in column 2
  time.ndx <- grep("^\\btime\\b", input[[1]], ignore.case = T)
  time.ndx <- time.ndx[-1]
  # extract different read data in dataset
  reads <- unname(unlist(lapply(1:length(time.ndx), function(x) input[time.ndx[x]-2, 1])))
  read.ndx <- time.ndx[!is.na(reads)]
  reads <- reads[!is.na(reads)]

  read.data <- list()
  ncol <- length(input[read.ndx[1],][!is.na(input[read.ndx[1],])])
  if(length(read.ndx)>1){
    # Extract all read tables except the last
    read.data <- lapply(1:(length(read.ndx)-1), function(x) t(input[read.ndx[x]:(read.ndx[x+1]-4), 1:(ncol)]))
    read.data <- lapply(1:length(read.data), function(x) as.data.frame(read.data[[x]])[1:length(read.data[[x]][,1][read.data[[x]][,1]!=0][!is.na(read.data[[x]][,1][read.data[[x]][,1]!=0])]), ])
    # Extract last read table
    read.data[[length(read.ndx)]] <- t(data.frame(input[read.ndx[length(read.ndx)]:(read.ndx[length(read.ndx)]+length(read.data[[1]][[1]])-1), 1:(ncol)]))
    read.data[[length(read.ndx)]] <- as.data.frame(read.data[[length(read.ndx)]])[1:length(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0][!is.na(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0])]),]
  } else {
    read.data[[1]] <- t(data.frame(input[read.ndx:(read.ndx + match(NA, input[read.ndx:nrow(input),3])-2), 1:(ncol)]))
  }
  # Remove temperature columns
  for(i in 1:length(read.data))
    read.data[[i]] <- read.data[[i]][ ,-(grep("^Temp.", read.data[[i]][1,]))]

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

    answer <- readline(paste0("Indicate where the fluorescence data is stored?\n",
                              paste(unlist(lapply(1:length(reads), function (i)
                                paste0("[", i, "] ", reads[i]))),
                                collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence data\n"))
    if(as.numeric(answer) == length(reads)+1){
      fluorescence <- NA
    } else {
      fluorescence <- read.data[[as.numeric(answer)]]
      fluorescence[which(fluorescence == "OVRFLW", arr.ind = TRUE)] <- NA
    }
    data.ls[[1]] <- density
    data.ls[[2]] <- fluorescence

    # if(length(reads)>2){
    #   answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
    #                             paste(unlist(lapply(1:length(reads), function (i)
    #                               paste0("[", i, "] ", reads[i]))),
    #                               collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
    #   if(as.numeric(answer) == length(reads)+1){
    #     fluorescence2 <- NA
    #   } else {
    #     fluorescence2 <- read.data[[as.numeric(answer)]]
    #     fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
    #   }
    #   data.ls[[3]] <- fluorescence2
    # }
  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }
  return(list(data.ls))
}

parse_biolector <- function(input)
{
  # get index (row,column) for "Time:"
  time.ndx <- c(grep("^\\bWell\\b", input[,1], ignore.case = T)+2, grep("^\\bChannel\\b", input[grep("^\\bWell\\b", input[,1], ignore.case = T),], ignore.case = T))
  # extract different read data in dataset
  reads <- unique(input[,time.ndx[2]][grep("Biomass", input[,time.ndx[2]])])
  reads <- reads[!is.na(reads)]
  read.ndx <- lapply(1:length(reads), function(x) which(input[,time.ndx[2]] %in% reads[x]))

  read.data <- list()
  n.time <- length(input[time.ndx[1], -(1:time.ndx[2])])
  if(length(read.ndx)>1){
    # Extract read tables
    read.data <- lapply(1:length(read.ndx), function(x) input[read.ndx[[x]], -(1:time.ndx[2])])
    read.data <- lapply(1:length(read.data), function(x) t(as.data.frame(read.data[[x]])[1:length(read.data[[x]][,1][read.data[[x]][,1]!=0][!is.na(read.data[[x]][,1][read.data[[x]][,1]!=0])]), ]))
    # add Well or Content name
    read.data <- lapply(1:length(read.data), function(x) if(all(gsub("[[:digit:]]+", "", input[read.ndx[[x]], 2]) == "X")){
      rbind(t(data.frame(input[read.ndx[[x]], 1])), read.data[[x]])
    } else {
      rbind(t(data.frame(input[read.ndx[[x]], 2])), read.data[[x]])
    }
    )
    # add time column
    read.data <- lapply(1:length(read.data), function(x) cbind(t(data.frame(input[time.ndx[1], -(1:(time.ndx[2]-1))])), read.data[[x]]))
  } else {
    read.data[[1]] <- t(data.frame(input[read.ndx[[1]], -(1:time.ndx[2])]))
    # add Well or Content name
    if(all(gsub("[[:digit:]]+", "", input[read.ndx[[1]], 2]) == "X")){
      read.data[[1]] <- rbind(t(data.frame(input[read.ndx[[1]], 1])), read.data[[1]])
    } else {
      read.data[[1]] <- rbind(t(data.frame(input[read.ndx[[1]], 2])), read.data[[1]])
    }
    # add time column
    read.data[[1]] <- cbind(t(data.frame(input[time.ndx[1], -(1:(time.ndx[2]-1))])), read.data[[1]])
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

    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA

  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }
  return(list(data.ls))
}

parse_victornivo <- function(input)
{
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[,2], ignore.case = T)
  # extract different read data in dataset
  reads <- unlist(lapply(1:length(time.ndx), function(x) input[time.ndx-6, 2]))
  reads <- reads[!is.na(reads)]

  read.data <- list()
  n.sample <- as.numeric(gsub(" wells.+", "", input[grep("PLATE FORMAT", input[,1]),2]))
  if(length(time.ndx)>1){
    # Extract read tables
    read.data <- lapply(1:length(time.ndx), function(x) t(data.frame(input[(time.ndx[x]+1):(time.ndx[x]+n.sample), -(1:2)])))
    # add Well
    read.data <- lapply(1:length(read.data), function(x) rbind(t(data.frame(input[(time.ndx[x]+1):(time.ndx[x]+n.sample), 1])), read.data[[x]]))
    # add time column
    read.data <- lapply(1:length(read.data), function(x) cbind(t(data.frame(input[time.ndx[x], -1])), read.data[[x]]))
  } else {
    read.data[[1]] <- t(data.frame(input[(time.ndx[1]+1):(time.ndx[1]+n.sample), -(1:2)]))
    # add Well or Content name
    read.data[[1]] <- rbind(t(data.frame(input[(time.ndx[[1]]+1):(time.ndx[[1]]+n.sample), 1])), read.data[[1]])
    # add time column
    read.data[[1]] <-cbind(t(data.frame(input[time.ndx[1], -1])), read.data[[1]])
  }

  # Remove time points with NA in all samples
  for(i in 1:length(read.data))
    read.data[[i]] <- cbind(read.data[[i]][,1][1:length(read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ][, 2])],
                            read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ])
  if(length(time.ndx)>1){
    # give all reads the same time values as the first read
    for(i in 2:length(time.ndx)){
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

    answer <- readline(paste0("Indicate where the fluorescence data is stored?\n",
                              paste(unlist(lapply(1:length(reads), function (i)
                                paste0("[", i, "] ", reads[i]))),
                                collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence data\n"))
    if(as.numeric(answer) == length(reads)+1){
      fluorescence <- NA
    } else {
      fluorescence <- read.data[[as.numeric(answer)]]
      fluorescence[which(fluorescence == "OVRFLW", arr.ind = TRUE)] <- NA
    }
    data.ls[[1]] <- density
    data.ls[[2]] <- fluorescence

    # if(length(reads)>2){
    #   answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
    #                             paste(unlist(lapply(1:length(reads), function (i)
    #                               paste0("[", i, "] ", reads[i]))),
    #                               collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
    #   if(as.numeric(answer) == length(reads)+1){
    #     fluorescence2 <- NA
    #   } else {
    #     fluorescence2 <- read.data[[as.numeric(answer)]]
    #     fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
    #   }
    #   data.ls[[3]] <- fluorescence2
    # }
  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }
  return(list(data.ls))
}

parse_victorx3 <- function(input)
{
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[1,], ignore.case = T)
  # extract different read data in dataset
  reads <- unlist(lapply(1:length(time.ndx), function(x) input[1, time.ndx[x]+1]))
  reads <- reads[!is.na(reads)]

  read.data <- list()
  nrow <- suppressWarnings(match(NA, as.numeric(input[-1,1]) ))
  if(length(time.ndx)>1){
    # Extract read tables
    read.data <- lapply(1:length(reads), function(x)
      input[2:nrow, c(2, 3,time.ndx[x], time.ndx[x]+1)]
    )
    # assign column names
    read.data <- lapply(read.data, setNames, c("repeat", "well", "time", "read"))
    # round time values to full minutes
    time <- lapply(1:length(read.data), function(x)
      round(c(
        as.matrix(
          utils::read.table(text = read.data[[x]][,3], sep = ":")
        ) %*% c(60, 1, 1/60)
      ), digits = 0)
    )
    # assign all measurements the first time value of the respective repeat
    for(x in 1:length(read.data)){
      for(i in 1:length(unique(read.data[[x]][ , "repeat"])) ){
        time[[x]][read.data[[x]][ , "repeat"] == unique(read.data[[x]][ , "repeat"])[i]] <-
          time[[x]][read.data[[x]][ , "repeat"] == unique(read.data[[x]][ , "repeat"])[i]][1]
      }
    }
    # replace time values in data tables
    for(x in 1:length(read.data)){
      read.data[[x]][,3] <- time[[x]]
    }
    # remove "repeat" column
    read.data <- lapply(1:length(read.data), function(x) read.data[[x]][,-1])
    # convert to wide format
    read.data <- lapply(1:length(read.data), function(x)
      pivot_wider(data = read.data[[x]], names_from= "well", values_from = "read"))
    # change list element names
    names(read.data) <- reads
    # add column names as first row
    read.data <- lapply(1:length(read.data), function(x) rbind(colnames(read.data[[x]]), read.data[[x]]))
  } else {
    # Extract read table
    read.data[[1]] <- input[2:nrow, c(2, 3,time.ndx, time.ndx+1)]
    # assign column names
    read.data[[1]] <- setNames(object = read.data[[1]], nm = c("well", "time", "read"))
    # round time values to full minutes
    time <- round(c(
      as.matrix(
        utils::read.table(text = read.data[[1]][,2], sep = ":")
      ) %*% c(60, 1, 1/60)
    ), digits = 0)
    # assign all measurements the first time value of the respective repeat
    for(i in 1:length(unique(read.data[[1]][ , "repeat"])) ){
      time[read.data[[1]][ , "repeat"] == unique(read.data[[1]][ , "repeat"])[i]] <-
        time[read.data[[1]][ , "repeat"] == unique(read.data[[1]][ , "repeat"])[i]][1]
    }
    # replace time values in data table
    read.data[[1]][,2] <- time
    # remove "repeat" column
    read.data[[1]] <- read.data[[1]][,-1]
    # convert to wide format
    read.data[[1]] <- pivot_wider(data = read.data[[1]], names_from= "well", values_from = "read")
    # change list element names
    names(read.data)[1] <- reads
    # add column names as first row
    read.data[[1]] <- rbind(colnames(read.data[[1]]), read.data[[1]])
  }

  # Remove time points with NA in all samples
  for(i in 1:length(read.data))
    read.data[[i]] <- cbind(read.data[[i]][,1][1:length(read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ][, 2])],
                            read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ])
  if(length(time.ndx)>1){
    # give all reads the same time values as the first read
    for(i in 2:length(time.ndx)){
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

    answer <- readline(paste0("Indicate where the fluorescence data is stored?\n",
                              paste(unlist(lapply(1:length(reads), function (i)
                                paste0("[", i, "] ", reads[i]))),
                                collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence data\n"))
    if(as.numeric(answer) == length(reads)+1){
      fluorescence <- NA
    } else {
      fluorescence <- read.data[[as.numeric(answer)]]
      fluorescence[which(fluorescence == "OVRFLW", arr.ind = TRUE)] <- NA
    }
    data.ls[[1]] <- density
    data.ls[[2]] <- fluorescence

    # if(length(reads)>2){
    #   answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
    #                             paste(unlist(lapply(1:length(reads), function (i)
    #                               paste0("[", i, "] ", reads[i]))),
    #                               collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
    #   if(as.numeric(answer) == length(reads)+1){
    #     fluorescence2 <- NA
    #   } else {
    #     fluorescence2 <- read.data[[as.numeric(answer)]]
    #     fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
    #   }
    #   data.ls[[3]] <- fluorescence2
    # }
  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }
  return(list(data.ls))
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

#from tfse package
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
#' @export
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

get_avg_param <- function(table = data.frame(), ndx.rep = list(), param1, param2 = NULL)
{
  if(!is.null(param2)){
    avg <- ifelse(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ) == 0 |
                    is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) )),
                  "", ifelse(is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param2]), na.rm = T)) )),
                             round(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ), 3),
                             paste0("<strong>", round(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ), 3),
                                    "</strong>", " (", round(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param2]), na.rm = T)) ), 3), ")")))
  } else {
    avg <- ifelse(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ) == 0 |
                    is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) )),
                  "", round(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ), 3)
    )
  }
  avg <- unlist(lapply(1:length(avg), function (x) ifelse(is.na(avg[x]), "", avg[x])) )
  avg
}

get_sd_param <- function(table = data.frame(), ndx.rep = list(), param1, param2 = NULL)
{
  if(!is.null(param2)){
    sd <- ifelse(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ) == 0 |
                   is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) )),
                 "", ifelse(is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param2]), na.rm = T)) )),
                            round(unlist(lapply(1:length(ndx.rep), function (x) sd(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ), 3),
                            paste0("<strong>", round(unlist(lapply(1:length(ndx.rep), function (x) sd(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ), 3),
                                   "</strong>", " (", round(unlist(lapply(1:length(ndx.rep), function (x) sd(as.numeric(table[ndx.rep[[x]], param2]), na.rm = T)) ), 3), ")")))
  } else {
    sd <- ifelse(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ) == 0 |
                   is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) )),
                 "", round(unlist(lapply(1:length(ndx.rep), function (x) sd(as.numeric(table[ndx.rep[[x]], param1]), na.rm = T)) ), 3)
    )
  }
  sd <- unlist(lapply(1:length(sd), function (x) ifelse(is.na(sd[x]), "", sd[x])) )
  sd
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
#' "An extension of Wilkinson’s
#' algorithm for positioning tick labels on axes." IEEE Transactions
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
