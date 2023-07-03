#' Run Shiny QurvE App
#'
#' @export
#'
#' @importFrom readxl read_excel excel_sheets
#'
#' @import shiny doParallel knitr
#' @return Launches a browser with the shiny app
#' @examples
#' if(interactive()){
#' # Run the app
#' run_app()
#' }
run_app <- function() {

  # Locate all the shiny apps that exist
  valid_apps <- list.files(system.file("shiny_app", package = "QurvE"))



  # Launch the app
  appDir <- system.file("shiny_app", package = "QurvE")
  suppressWarnings(shiny::runApp(appDir, display.mode = "normal"))
}

#' Extract names of reads from experimental data created with the "Gen5" or "Gen6" software
#'
#' @param file path to file with experiment data
#' @param csvsep CSV separator as string.
#' @param dec Decimal separator as string (for CSV, TXT, and TSV files)
#' @param sheet Sheet number or name (as string) for XLS and XLSX files.
#'
#' @return A vector of read name strings
#'
#' @keywords internal shiny_app
#' @noRd
#'
parse_properties_Gen5Gen6 <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get row numbers for "time" in column 2
  time.ndx <- grep("\\btime\\b", input[[2]], ignore.case = TRUE)
  # extract different read data in dataset
  reads <- unname(unlist(lapply(1:length(time.ndx), function(x) input[time.ndx[x]-2, 1])))
  suppressWarnings(
    reads[!is.na(as.numeric(reads))] <- gsub("\\.", ",", as.numeric(reads[!is.na(as.numeric(reads))]))
  )
  reads <- reads[!is.na(reads)]
  reads <- unique(reads)
  invisible(reads)
}

#' Extract names of reads from experimental data created with a Chi.Bio bioreactor setup
#'
#' @param file path to file with experiment data
#' @param csvsep CSV separator as string.
#' @param dec Decimal separator as string (for CSV, TXT, and TSV files)
#' @param sheet Sheet number or name (as string) for XLS and XLSX files.
#'
#' @return A vector of read name strings
#'
#' @keywords internal shiny_app
#' @noRd
parse_properties_chibio <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)

  time.ndx <- grep("time", input[1,], ignore.case = TRUE)
  # extract different read data in dataset
  read.ndx <- grep("measured|emit", input[1,], ignore.case = TRUE)
  reads <- input[1, read.ndx]
  suppressWarnings(
    reads[!is.na(as.numeric(reads))] <- gsub("\\.", ",", as.numeric(reads[!is.na(as.numeric(reads))]))
  )
  reads <- reads[!is.na(reads)]
  reads <- unique(reads)
  invisible(reads)
}

#' Extract names of reads from experimental data created with a Tecan plate reader
#'
#' @param file path to file with experiment data
#' @param csvsep CSV separator as string.
#' @param dec Decimal separator as string (for CSV, TXT, and TSV files)
#' @param sheet Sheet number or name (as string) for XLS and XLSX files.
#'
#' @return A vector of read name strings
#'
#' @keywords internal shiny_app
#' @noRd
parse_properties_tecan <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get row numbers for "time" in column 2
  time.ndx <- grep("^\\btime\\b", input[[1]], ignore.case = TRUE)
  time.ndx <- time.ndx[-1]
  # extract different read data in dataset
  reads <- unname(unlist(lapply(1:length(time.ndx), function(x) input[time.ndx[x]-2, 1])))
  suppressWarnings(
    reads[!is.na(as.numeric(reads))] <- gsub("\\.", ",", as.numeric(reads[!is.na(as.numeric(reads))]))
  )
  reads <- reads[!is.na(reads)]
  reads <- unique(reads)
  invisible(reads)
}

#' Extract names of biomass reads from experimental data created with a Biolector plate reader
#'
#' @param file path to file with experiment data
#' @param csvsep CSV separator as string.
#' @param dec Decimal separator as string (for CSV, TXT, and TSV files)
#' @param sheet Sheet number or name (as string) for XLS and XLSX files.
#'
#' @return A vector of read name strings
#'
#' @keywords internal shiny_app
#' @noRd
parse_properties_biolector <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get row numbers for "time" in column 2
  time.ndx <- grep("\\btime\\b", input[[2]], ignore.case = TRUE)
  # extract different read data in dataset
  time.ndx <- c(grep("^\\bWell\\b", input[,1], ignore.case = TRUE)+2, grep("^\\bChannel\\b", input[grep("^\\bWell\\b", input[,1], ignore.case = TRUE),], ignore.case = TRUE))
  # extract different read data in dataset
  reads <- unique(input[,time.ndx[2]][grep("Biomass", input[,time.ndx[2]])])
  reads <- reads[!is.na(reads)]
  invisible(reads)
}

#' Extract names of reads from experimental data created with Pelkin Elmer's Victor Nivo plate readers
#'
#' @param file path to file with experiment data
#' @param csvsep CSV separator as string.
#' @param dec Decimal separator as string (for CSV, TXT, and TSV files)
#' @param sheet Sheet number or name (as string) for XLS and XLSX files.
#'
#' @return A vector of read name strings
#'
#' @keywords internal shiny_app
#' @noRd
parse_properties_victornivo <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[,2], ignore.case = TRUE)
  # extract different read data in dataset
  reads <- lapply(1:length(time.ndx), function(x) input[time.ndx-6, 2])
  reads <- reads[!is.na(reads)]
  invisible(reads)
}

#' Extract names of reads from experimental data created with Pelkin Elmer's Victor X3 plate readers
#'
#' @param file path to file with experiment data
#' @param csvsep CSV separator as string.
#' @param dec Decimal separator as string (for CSV, TXT, and TSV files)
#' @param sheet Sheet number or name (as string) for XLS and XLSX files.
#'
#' @return A vector of read name strings
#'
#' @keywords internal shiny_app
#' @noRd
parse_properties_victorx3 <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[1,], ignore.case = TRUE)
  # extract different read data in dataset
  reads <- unlist(lapply(1:length(time.ndx), function(x) input[1, time.ndx[x]+1]))
  reads <- reads[!is.na(reads)]
  invisible(reads)
}


#' @param data.file (Character) A table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing raw plate reader (or similar device) data.
#' @param map.file (Character) A table file in column format with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt'  with 'well', 'ID', 'replicate', and 'concentration' in the first row. Used to assign sample information to wells in a plate.
#' @param software (Character) The name of the software/device used to export the plate reader data.
#' @param convert.time (\code{NULL} or string) Convert time values with a formula provided in the form \code{'y = function(x)'}.
#' For example: \code{convert.time = 'y = 24 * x'}
#' @param sheet.data,sheet.map (Numeric or Character) Number or name of the sheets in XLS or XLSX files containing experimental data or mapping information, respectively (_optional_).
#' @param csvsep.data,csvsep.map (Character) separator used in CSV data files (ignored for other file types).  Default: \code{";"}
#' @param dec.data,dec.map (Character) decimal separator used in CSV, TSV or TXT files with measurements and mapping information, respectively.
#' @param subtract.blank (Logical) Shall blank values be subtracted from values within the same experiment ([TRUE], the default) or not ([FALSE]).
#' @param growth.nm Name of read corresponding to growth rate
#' @param fl.nm,fl2.nm Name of read corresponding to fluorescence and fluorescence2 data
#' @param calib.growth,calib.fl,calib.fl2 (Character or \code{NULL}) Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert growth and fluorescence values. This can be used to, e.g., convert plate reader absorbance values into \ifelse{html}{\out{OD<sub>600</sub>}}{\eqn{OD_{600}}} or fluorescence intensity into molecule concentrations.
#' @param fl.normtype (Character string) Normalize fluorescence values by either diving by \code{'growth'} or by fluorescence2 values (\code{'fl2'}).
#'
#' @rdname parse_data
#'
#' @return A \code{grodata} object suitable to run \code{\link{growth.workflow}}. See \code{\link{read_data}} for its structure.
#'
#' @keywords internal shiny_app
#' @noRd
#'
parse_data_shiny <-
  function(data.file = NULL,
           map.file = NULL,
           software = "Gen5",
           convert.time = NULL,
           sheet.data = 1,
           sheet.map = 1,
           csvsep.data = ";",
           dec.data = ".",
           csvsep.map = ";",
           dec.map = ".",
           subtract.blank  = TRUE,
           growth.nm = NULL,
           fl.nm = NULL,
           fl2.nm = NULL,
           calib.growth = NULL,
           calib.fl = NULL,
           calib.fl2 = NULL,
           fl.normtype = c("growth", "fl2")
  ) {
    if(!is.null(fl.nm) && is.na(fl.nm)) fl.nm <- NULL
    if(!is.null(fl2.nm) && is.na(fl2.nm)) fl2.nm <- NULL
    if(is.null(data.file)) stop("Please provide the name or path to a table file containing plate reader data in the 'data.file' argument.")
    if(is.null(map.file)) warning("No mapping file was provided. The samples will be identified based on their well position (A1, A2, A3, etc.). Grouping options will not be available if you run any further analysis with QurvE.")
    # Read table file
    if (file.exists(data.file)) {
      # Read table file
      input <- read_file(data.file, csvsep=csvsep.data, dec=dec.data, sheet=sheet.data)
    } else {
      stop(paste0("File \"", data.file, "\" does not exist."), call. = FALSE)
    }
    if(!is.null(map.file)){
      if (file.exists(map.file)) {
        mapping <- read_file(map.file, csvsep=csvsep.map, dec=dec.map, sheet=sheet.map)
      } else {
        stop(paste0("File \"", map.file, "\" does not exist."), call. = FALSE)
      }
    } else {
      mapping <- NULL
    }
    if(any(grep("Gen5|Gen6", software, ignore.case = TRUE))){
      parsed.ls <- parse_Gen5Gen6_shiny(data = input, growth.nm = growth.nm, fl.nm = fl.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    } # if("Gen5" %in% software)
    if(any(grep("Chi.Bio", software, ignore.case = TRUE))){
      parsed.ls <- parse_chibio_shiny(input, growth.nm = growth.nm, fl.nm = fl.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    }
    if(any(grep("GrowthProfiler", software, ignore.case = TRUE))){
      parsed.ls <- parse_growthprofiler(input)
      data.ls <- parsed.ls[[1]]
    }
    if(any(grep("Tecan", software, ignore.case = TRUE))){
      parsed.ls <- parse_tecan_shiny(input, growth.nm = growth.nm, fl.nm = fl.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("Biolector", software, ignore.case = TRUE))){
      parsed.ls <- parse_biolector_shiny(input, growth.nm = growth.nm)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("VictorNivo", software, ignore.case = TRUE))){
      parsed.ls <- parse_victornivo_shiny(input, growth.nm = growth.nm, fl.nm = fl.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("VictorX3", software, ignore.case = TRUE))){
      parsed.ls <- parse_victorx3_shiny(input, growth.nm = growth.nm, fl.nm = fl.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    }

    noNA.ndx <- which(!is.na(data.ls))

    # Convert time values
    if(!is.null(convert.time)){

      conversion <- parse(text = convert.time)

      for(i in noNA.ndx){
        x <- as.numeric(data.ls[[i]][2:nrow(data.ls[[1]]),1])
        time_converted <- eval(conversion)
        data.ls[[i]][2:nrow(data.ls[[1]]),1] <- time_converted
      }
    }

    if(any(c("Gen5", "Gen6") %in% software)){
      # Remove any columns between time and 'A1' (for plate readers)
      A1.ndx <- match("A1", data.ls[[noNA.ndx[1]]][1,])
      if(A1.ndx>2){
        data.ls <- lapply(noNA.ndx, function(x) data.ls[[x]][ ,c(1,A1.ndx:ncol(data.ls[[x]]))])
      }
    } else {
      data.ls <-data.ls[noNA.ndx]
    }

    # apply identifiers specified in mapping file
    for(i in noNA.ndx){
      if(!is.null(mapping)){
        # assign names to samples
        map.ndx <- match(data.ls[[i]][1,1:ncol( data.ls[[i]])], mapping[2:nrow(mapping),1])+1
        if(!all(is.na(map.ndx))){
          names <- mapping[map.ndx, 2]
          names <- names[2:length(names)]
          data.ls[[i]][1,2:ncol( data.ls[[i]])] <- names
          # assign replicate numbers to samples
          replicates <- mapping[map.ndx, 3]
          replicates <- as.numeric(replicates[2:length(replicates)])
          data.ls[[i]] <- rbind(data.ls[[i]][1,], c(NA,replicates), data.ls[[i]][-1,])
          # assign concentrations to samples
          # assign replicate numbers to samples
          conc <- mapping[map.ndx, 4]
          conc <- as.numeric(conc[2:length(conc)])
          data.ls[[i]] <- rbind(data.ls[[i]][1:2,], c(NA,conc), data.ls[[i]][-(1:2),])
        }
        else {
          data.ls[[i]] <- rbind(data.ls[[i]][1,], rep(NA, ncol(data.ls[[i]])), data.ls[[i]][-1,])
          data.ls[[i]] <- rbind(data.ls[[i]][1,], rep(NA, ncol(data.ls[[i]])), data.ls[[i]][-1,])
        }
      } else {
        data.ls[[i]] <- rbind(data.ls[[i]][1,], rep(NA, ncol(data.ls[[i]])), data.ls[[i]][-1,])
        data.ls[[i]] <- rbind(data.ls[[i]][1,], rep(NA, ncol(data.ls[[i]])), data.ls[[i]][-1,])
      }
    }
    # Remove samples with "NA" identifier
    for(i in 1:length(data.ls)){
      if(length(data.ls[[i]]) > 1){
        data.ls[[i]] <- data.ls[[i]][,!is.na(data.ls[[i]][1,])]
      }
    }
    if(length(data.ls)==1) {
      names(data.ls) <- "growth"
      grodata <- read_data(
        data.growth = data.ls[[1]],
        data.fl = NA,
        subtract.blank = subtract.blank,
        calib.growth = calib.growth,
        calib.fl = calib.fl,
        calib.fl2 = calib.fl2
      )
    } else if (length(data.ls) == 2) {
      names(data.ls) <- c("growth", "fluorescence")
      grodata <-
        read_data(
          data.growth = data.ls[[1]],
          data.fl = data.ls[[2]],
          subtract.blank = subtract.blank,
          calib.growth = calib.growth,
          calib.fl = calib.fl,
          calib.fl2 = calib.fl2,
          fl.normtype = fl.normtype
        )
    }
    else {
      names(data.ls) <- c("growth", "fluorescence", "fluorescence2")
      grodata <-
        read_data(
          data.growth = data.ls[[1]],
          data.fl = data.ls[[2]],
          data.fl2 = data.ls[[3]],
          subtract.blank = subtract.blank,
          calib.growth = calib.growth,
          calib.fl = calib.fl,
          calib.fl2 = calib.fl2,
          fl.normtype = fl.normtype
        )
    }

    invisible(grodata)
  }

#' Extract relevant data from a raw data export file generated with the "Gen5" or "Gen6" software.
#'
#' @param data A dataframe created by reading a table file with \code{\link{read_file}}
#' @param growth.nm Name of read corresponding to growth rate
#' @param fl.nm,fl2.nm Name of read corresponding to fluorescence and fluorescence2 data
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector, the remainder the measurements.
#'
#' @keywords internal shiny_app
#' @noRd
parse_Gen5Gen6_shiny <- function(data, growth.nm, fl.nm, fl2.nm)
{
  # get row numbers for "time" in column 2
  time.ndx <- grep("\\btime\\b", data[[2]], ignore.case = TRUE)
  # extract different read data in dataset
  reads <- unname(unlist(lapply(1:length(time.ndx), function(x) data[time.ndx[x]-2, 1])))
  read.ndx <- time.ndx[!is.na(reads)]
  reads <- reads[!is.na(reads)]
  suppressWarnings(
    reads[!is.na(as.numeric(reads))] <- gsub("\\.", ",", as.numeric(reads[!is.na(as.numeric(reads))]))
  )
  read.data <- list()
  ncol <- length(data[read.ndx[1],][!is.na(data[read.ndx[1],])])
  if(length(read.ndx)>1){
    # Extract all read tables except the last
    read.data <- lapply(1:(length(read.ndx)-1), function(x) data[read.ndx[x]:(read.ndx[x+1]-3),2:(ncol)])
    read.data <- lapply(1:length(read.data), function(x) as.data.frame(read.data[[x]])[1:length(read.data[[x]][,1][read.data[[x]][,1]!=0][!is.na(read.data[[x]][,1][read.data[[x]][,1]!=0])]),])
    # Extract last read table
    read.data[[length(read.ndx)]] <- data.frame(data[read.ndx[length(read.ndx)]:(read.ndx[length(read.ndx)]+length(read.data[[1]][[1]])-1),2:(ncol)])
    #read.data[[length(read.ndx)]] <- as.data.frame(read.data[[length(read.ndx)]])[1:length(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0][!is.na(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0])]),]
    for( i in 1:length(read.data) ){
      if(any(is.na(suppressWarnings(as.numeric(read.data[[i]][-1,2]))))){
        read.data[[i]] <- suppressWarnings(read.data[[i]][1:which(is.na(as.numeric(read.data[[i]][-1,2])))[1], ])
      }
    }
  } else {
    if(!any(is.na(data[read.ndx:nrow(data),3]))){
      read.data[[1]] <- data.frame(data[read.ndx:nrow(data),2:(1+ncol)])
    }
    else {
      read.data[[1]] <- data.frame(data[read.ndx:(read.ndx + match(NA, data[read.ndx:nrow(data),3])-2),2:(1+ncol)])
    }
  }
  # Remove time points with NA in all samples
  for(i in 1:length(read.data))
    read.data[[i]] <- cbind(read.data[[i]][,1][1:length(read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ][, 2])],
                            read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ])
  # give all reads the same time values as the first read
  if(length(read.ndx)>1){
    for(i in 2:length(read.data)){
      read.data[[i]][[1]] <- read.data[[1]][[1]]
    }
  }
  names(read.data) <- reads
  well_format <- str_extract(data[grep("Plate Type", data[,1]), 2], pattern = "[[:digit:]]+")
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
  if (!is.null(growth.nm) && growth.nm != "Ignore")
    growth <- read.data[[match(growth.nm, reads)]]
  else
    growth  <- NA

  if(!is.null(fl.nm) && fl.nm != "Ignore"){
    fluorescence <-  read.data[[match(fl.nm, reads)]]
    fluorescence[which(fluorescence == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence <- NA

  if(!is.null(fl2.nm) && fl2.nm != "Ignore"){
    fluorescence2 <-  read.data[[match(fl2.nm, reads)]]
    fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence2 <- NA


  # growth <- read.data[[1]]
  data.ls[[1]] <- growth
  data.ls[[2]] <- fluorescence
  data.ls[[3]] <- fluorescence2

  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of "Chi.Bio" bioreactors.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#' @param growth.nm Name of read corresponding to growth rate
#' @param fl.nm,fl2.nm Name of read corresponding to fluorescence and fluorescence2 data
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @keywords internal shiny_app
#' @noRd
parse_chibio_shiny <- function(input, growth.nm, fl.nm, fl2.nm)
{
  time.ndx <- grep("time", input[1,], ignore.case = TRUE)
  read.ndx <- grep("measured|emit", input[1,], ignore.case = TRUE)
  reads <- input[1, read.ndx]

  data.ls <- list()
  if(length(reads)>1){
    if (!is.null(growth.nm) && growth.nm != "Ignore"){
      growth <- data.frame("time" = input[, time.ndx], "growth" = c(input[1,read.ndx[match(growth.nm, reads)]], as.numeric(input[-1, read.ndx[match(growth.nm, reads)]])))
      if(all(as.numeric(growth[-1,2]) == 0) || all(is.na(growth[-1,2]))){
        growth <- NA
      }
    }
    else
      growth  <- NA

    if (!is.null(fl.nm) && fl.nm != "Ignore"){
      fluorescence <- data.frame("time" = input[, time.ndx], "growth" = c(input[1,read.ndx[match(fl.nm, reads)]], as.numeric(input[-1, read.ndx[match(fl.nm, reads)]])))
      if(all(as.numeric(fluorescence[-1,2]) == 0) || all(is.na(fluorescence[-1,2]))){
        fluorescence <- NA
      }
    }
    else
      fluorescence  <- NA

    if (!is.null(fl2.nm) && fl2.nm != "Ignore"){
      fluorescence2 <- data.frame("time" = input[, time.ndx], "growth" = c(input[1,read.ndx[match(fl2.nm, reads)]], as.numeric(input[-1, read.ndx[match(fl2.nm, reads)]])))
      if(all(as.numeric(fluorescence2[-1,2]) == 0) || all(is.na(fluorescence2[-1,2]))){
        fluorescence2 <- NA
      }
    }
    else
      fluorescence2  <- NA
  } else {
    growth <- data.frame("time" = input[, time.ndx], "growth" = c(input[1, read.ndx], as.numeric(input[-1, read.ndx])))
    fluorescence <- NA
    fluorescence2 <- NA
  }

  data.ls[[1]] <- growth
  data.ls[[2]] <- fluorescence
  data.ls[[3]] <- fluorescence2

  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of "Tecan" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#' @param growth.nm Name of read corresponding to growth rate
#' @param fl.nm,fl2.nm Name of read corresponding to fluorescence and fluorescence2 data
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @keywords internal shiny_app
#' @noRd
parse_tecan_shiny <- function(input, growth.nm, fl.nm, fl2.nm)
{
  # get row numbers for "time" in column 2
  time.ndx <- grep("^\\btime\\b", input[[1]], ignore.case = TRUE)
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
    #read.data[[length(read.ndx)]] <- as.data.frame(read.data[[length(read.ndx)]])[1:length(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0][!is.na(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0])]),]
    for( i in 1:length(read.data) ){
      if(any(is.na(suppressWarnings(as.numeric(read.data[[i]][-1,2]))))){
        read.data[[i]] <- suppressWarnings(read.data[[i]][1:which(is.na(as.numeric(read.data[[i]][-1,2])))[1], ])
      }
    }
  } else {
    if(!any(is.na(input[read.ndx:nrow(input),3]))){
      read.data[[1]] <- t(data.frame(input[read.ndx:nrow(input),2:(1+ncol)]))
    }
    else {
      read.data[[1]] <- t(data.frame(input[read.ndx:(read.ndx + match(NA, input[read.ndx:nrow(input),3])-2), 1:(ncol)]))
    }
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

  if (!is.null(growth.nm) && growth.nm != "Ignore")
    growth <- read.data[[match(growth.nm, reads)]]
  else
    growth  <- NA

  if(!is.null(fl.nm) && fl.nm != "Ignore"){
    fluorescence <-  read.data[[match(fl.nm, reads)]]
    fluorescence[which(fluorescence == "OVER", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence <- NA

  if(!is.null(fl2.nm) && fl2.nm != "Ignore"){
    fluorescence2 <-  read.data[[match(fl2.nm, reads)]]
    fluorescence2[which(fluorescence2 == "OVER", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence2 <- NA

  data.ls[[1]] <- growth
  data.ls[[2]] <- fluorescence
  data.ls[[3]] <- fluorescence2

  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of "Biolector" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#' @param growth.nm Name of read corresponding to growth rate
#'
#' @return a list of length two containing a growth dataframe in the first element and \code{NA} in the second. The first column in the dataframe represents a time vector.
#'
#' @keywords internal shiny_app
#' @noRd
parse_biolector_shiny <- function(input, growth.nm)
{
  # get index (row,column) for "Time:"
  time.ndx <- c(grep("^\\bWell\\b", input[,1], ignore.case = TRUE)+2, grep("^\\bChannel\\b", input[grep("^\\bWell\\b", input[,1], ignore.case = TRUE),], ignore.case = TRUE))
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

    growth <- read.data[[match(growth.nm, reads)]]

    data.ls[[1]] <- growth
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA

  } else {
    growth <- read.data[[1]]
    data.ls[[1]] <- growth
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }
  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of Perkin Elmer's "Victor Nivo" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#' @param growth.nm Name of read corresponding to growth rate
#' @param fl.nm,fl2.nm Name of read corresponding to fluorescence and fluorescence2 data
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @keywords internal shiny_app
#' @noRd
parse_victornivo_shiny <- function(input, growth.nm, fl.nm, fl2.nm)
{
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[,2], ignore.case = TRUE)
  # extract different read data in dataset
  reads <- lapply(1:length(time.ndx), function(x) input[time.ndx-6, 2])
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
  if (!is.null(growth.nm) && growth.nm != "Ignore")
    growth <- read.data[[match(growth.nm, reads)]]
  else
    growth  <- NA

  if(!is.null(fl.nm) && fl.nm != "Ignore"){
    fluorescence <-  read.data[[match(fl.nm, reads)]]
    fluorescence[which(fluorescence == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence <- NA

  if(!is.null(fl2.nm) && fl2.nm != "Ignore"){
    fluorescence2 <-  read.data[[match(fl2.nm, reads)]]
    fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence2 <- NA


  # growth <- read.data[[1]]
  data.ls[[1]] <- growth
  data.ls[[2]] <- fluorescence
  data.ls[[3]] <- fluorescence2

  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of Perkin Elmer's "Victor X3" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#' @param growth.nm Name of read corresponding to growth rate
#' @param fl.nm,fl2.nm Name of read corresponding to fluorescence and fluorescence2 data
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @keywords internal shiny_app
#' @noRd
parse_victorx3_shiny <- function(input, growth.nm, fl.nm, fl2.nm)
{
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[1,], ignore.case = TRUE)
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
  if (!is.null(growth.nm) && growth.nm != "Ignore")
    growth <- read.data[[match(growth.nm, reads)]]
  else
    growth  <- NA

  if(!is.null(fl.nm) && fl.nm != "Ignore"){
    fluorescence <-  read.data[[match(fl.nm, reads)]]
    fluorescence[which(fluorescence == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence <- NA

  if(!is.null(fl2.nm) && fl2.nm != "Ignore"){
    fluorescence2 <-  read.data[[match(fl2.nm, reads)]]
    fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence2 <- NA


  # growth <- read.data[[1]]
  data.ls[[1]] <- growth
  data.ls[[2]] <- fluorescence
  data.ls[[3]] <- fluorescence2

  invisible(list(data.ls))
}

write.csv.utf8.BOM <- function(df, filename)
{
  con <- file(filename, "w")
  tryCatch({
    for (i in 1:ncol(df))
      df[,i] = iconv(df[,i], to = "UTF-8")
    writeChar(iconv("\ufeff", to = "UTF-8"), con, eos = NULL)
    utils::write.csv(df, file = con, na = "", row.names = FALSE, quote = FALSE)
  },finally = {close(con)})
}


#' Adjusted modalDialog()  function to include classes
#'
#' @param ... UI elements for the body of the modal dialog box.
#' @param title An optional title for the dialog.
#' @param footer UI for footer. Use NULL for no footer.
#' @param size One of "s" for small, "m" (the default) for medium, or "l" for large.
#' @param easyClose If \code{TRUE}, the modal dialog can be dismissed by clicking outside the dialog box, or be pressing the Escape key. If \code{FALSE} (the default), the modal dialog can't be dismissed in those ways; instead it must be dismissed by clicking on a modalButton(), or from a call to removeModal() on the server.
#' @param fade If \code{FALSE}, the modal dialog will have no fade-in animation (it will simply appear rather than fade in to view).
#' @param idcss CSS class of the modal
#'
#' @return \code{div} HTML tag for a modal dialog
#'
#' @author Stackoverflow user \code{mfindinge}
#'
#' @keywords internal shiny_app
#' @noRd
#'
help_modal <- function (..., title = NULL, footer = NULL,
                     size = c("m", "s", "l"), easyClose = TRUE, fade = TRUE, idcss = "")
{
  size <- match.arg(size)
  cls <- if (fade)
    "modal fade"
  else "modal"
  div(id = "shiny-modal", class = cls, tabindex = "-1", `data-backdrop` = if (!easyClose)
    "static", `data-keyboard` = if (!easyClose)
      "false", div(class = paste("modal-dialog", idcss), class = switch(size,
                                                                        s = "modal-sm",
                                                                        m = NULL,
                                                                        l = "modal-lg"),
                   div(class = "modal-content",
                       if (!is.null(title))
                         div(class = "modal-header", tags$h4(class = "modal-title",
                                                             title)
                         ),
                       div(class = "modal-body", ...),
                       if (!is.null(footer))
                         div(class = "modal-footer", footer))
      ),
    tags$script("$('#shiny-modal').modal().focus();"))
}

#' Create a numeric input
#'
#' Create an input control for entry of numeric values. This is identical to
#' [shiny::numericInput()] but is more flexible in **not** requiring an initial
#' value and in allowing placeholders.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param value Initial value. NULL by default.
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`; see
#'   [validateCssUnit()].
#' @param placeholder A character string giving the user a hint as to what can
#'   be entered into the control. Internet Explorer 8 and 9 do not support this
#'   option.
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param step Interval to use when stepping between min and max
#'
#' @return A numeric input control that can be added to a UI definition.
#'
#' @seealso \code{\link[shiny]{updateNumericInput}}
#'
#' @keywords internal shiny_app
#' @noRd
#'
numberInput <- function(inputId, label, value = NULL, min = NA, max = NA, step = NA,
                        placeholder = NULL, width = NULL) {

  inputTag <- shiny::tags$input(id = inputId, type = "number",
                                class = "form-control",
                                placeholder = placeholder)

  if (!is.na(min))
    inputTag$attribs$min <- min
  if (!is.na(max))
    inputTag$attribs$max <- max
  if (!is.na(step))
    inputTag$attribs$step <- step
  if (!is.null(value))
    inputTag$attribs$value <- value

  shiny::tagList(
    shiny::div(class = "surveyNumericInput form-group shiny-input-container",
               style = htmltools::css(width = shiny::validateCssUnit(width)),
               shinyInputLabel(inputId, label), inputTag)

  )
}

#' Create an update-resistant popover for a Shiny element
#'
#' This function creates a popover that is resistant to updates in the associated Shiny element.
#' It adds an event listener to the specified element, which reinstalls the popover whenever a child
#' of the element changes.
#'
#' @param id The id of the Shiny element to which the popover is attached.
#' @param title The title of the popover.
#' @param content The content of the popover.
#' @param placement The placement of the popover relative to the Shiny element (default: "bottom").
#'                  Possible values are "top", "bottom", "left", and "right".
#' @param trigger The event that triggers the display of the popover (default: "hover").
#'                Possible values are "hover", "focus", and "click".
#' @param options A list of additional options for the popover.
#'
#' @return A Shiny HTML tag that contains the JavaScript code for creating the update-resistant popover.
#' @author K. Rohde (stack overflow)
#' @keywords internal shiny_app
#' @importFrom utils packageVersion
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinyBS)
#'
#' ui <- shinyUI(fluidPage(
#'   selectInput("Main2_1","Label","abc",  selectize = TRUE, multiple = TRUE),
#'   updateResistantPopover("Main2_1", "Label", "content", placement = "right", trigger = "focus"),
#'   actionButton("destroy", "destroy!")
#' ))
#'
#' server <- function(input, output, session){
#'   observeEvent(input$destroy, {
#'     updateSelectInput(session, "Main2_1", choices="foo")
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
updateResistantPopover <- function(id, title, content, placement = "bottom", trigger = "hover", options = NULL){
  options = buildTooltipOrPopoverOptionsList(title, placement, trigger, options, content)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      var target = document.querySelector('#", id, "');
      var observer = new MutationObserver(function(mutations) {
        setTimeout(function() {
          shinyBS.addTooltip('", id, "', 'popover', ", options, ");
        }, 200);
      });
      observer.observe(target, { childList: true });
    });
  ")))
  htmltools::attachDependencies(bsTag, htmltools::htmlDependency("shinyBS", utils::packageVersion("shinyBS"), src = c("href" = "sbs"), script = "shinyBS.js", stylesheet = "shinyBS.css"))
}

#' Custom tooltip function
#'
#' This function creates a custom tooltip for a given element in a Shiny application.
#' The implementation is based on the shinyBS package.
#'
#' @param title The text for the tooltip's title.
#' @param placement Placement of the tooltip. One of 'top', 'bottom', 'left', or 'right'.
#' @param trigger The events that trigger the tooltip. One or more of 'click', 'hover', 'focus', or 'manual'.
#' @param options A list of additional options for the tooltip.
#' @param content Optional HTML content for the tooltip.
#' @keywords internal shiny_app
#' @return A list of tooltip options to be used in the Shiny application.
#'
#' @seealso \url{https://CRAN.R-project.org/package=shinyBS}
#' @importFrom shiny HTML
#'
#' @examples
#' \dontrun{
#' tooltip_options <- custom_tooltip(
#'   title = "Sample tooltip",
#'   placement = "top",
#'   trigger = "hover",
#'   options = list(delay = 100),
#'   content = "This is a custom tooltip."
#' )
#'
#' # In a Shiny app
#' # shiny::tags$span("Hover me!", `data-toggle` = "tooltip",
#'                    `data-placement` = "top", `data-trigger` = "hover",
#'                    `title` = "Hello, tooltip!")
#' }
buildTooltipOrPopoverOptionsList <- function (title, placement, trigger, options, content)
{
  if (is.null(options)) {
    options = list()
  }
  if (!missing(content)) {
    if (is.null(options$content)) {
      options$content = shiny::HTML(content)
    }
  }
  if (is.null(options$placement)) {
    options$placement = placement
  }
  if (is.null(options$trigger)) {
    if (length(trigger) > 1)
      trigger = paste(trigger, collapse = " ")
    options$trigger = trigger
  }
  if (is.null(options$title)) {
    options$title = title
  }
  return(options)
}

shinyInputLabel <- utils::getFromNamespace("shinyInputLabel", "shiny")
