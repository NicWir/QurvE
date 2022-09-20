parse_properties_Gen5Gen6 <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get row numbers for "time" in column 2
  time.ndx <- grep("\\btime\\b", input[[2]], ignore.case = T)
  # extract different read data in dataset
  reads <- unname(unlist(lapply(1:length(time.ndx), function(x) input[time.ndx[x]-2, 1])))
  suppressWarnings(
    reads[!is.na(as.numeric(reads))] <- gsub("\\.", ",", as.numeric(reads[!is.na(as.numeric(reads))]))
  )
  reads <- reads[!is.na(reads)]
  reads <- unique(reads)
  return(reads)
}

parse_data_shiny <-
  function(data.file = NULL,
           map.file = NULL,
           software = "Gen5",
           convert.time = TRUE,
           data.sheet = 1,
           map.sheet = 1,
           csvsep.data = ";",
           dec.data = ".",
           csvsep.map = ";",
           dec.map = ".",
           subtract.blank  = T,
           density.nm = NULL,
           fl1.nm = NULL,
           fl2.nm = NULL,
           calibration = NULL
  ) {
    if(!is.null(fl1.nm) && is.na(fl1.nm)) fl1.nm <- NULL
    if(!is.null(fl2.nm) && is.na(fl2.nm)) fl2.nm <- NULL
    if(is.null(data.file)) stop("Please provide the name or path to a table file containing plate reader data in the 'data.file' argument.")
    if(is.null(map.file)) warning("No mapping file was provided. The samples will be identified based on their well position (A1, A2, A3, etc.). Grouping options will not be available if you run any further analysis with QurvE.")
    if(!(software %in% c("Gen5", "Gen6"))) stop("The plate reader control software you provided as 'software' is currently not supported by parse_data(). Supported options are:\n 'Gen5', 'Gen6'.")
    # Read table file
    if (file.exists(data.file)) {
      # Read table file
      input <- read_file(data.file, csvsep=csvsep.data, dec=dec.data, sheet=data.sheet)
    } else {
      stop(paste0("File \"", data.file, "\" does not exist."), call. = F)
    }
    if(!is.null(map.file)){
      if (file.exists(map.file)) {
        mapping <- read_file(map.file, csvsep=csvsep.map, dec=dec.map, sheet=map.sheet)
      } else {
        stop(paste0("File \"", map.file, "\" does not exist."), call. = F)
      }
    } else {
      mapping <- NULL
    }
    if(any(c("Gen5", "Gen6") %in% software)){
      parsed.ls <- parse_Gen5Gen6_shiny(data = input, density.nm = density.nm, fl1.nm = fl1.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
      read.data <- parsed.ls[[2]]
    } # if("Gen5" %in% software)
    # Convert time values to hours
    if(convert.time){
      for(i in 1:length(data.ls)){
        if(length(data.ls[[i]]) > 1) data.ls[[i]][2:nrow(data.ls[[i]]),1] <- as.numeric(data.ls[[i]][2:nrow(data.ls[[i]]),1])*24
      }
    }
    noNA.ndx <- which(!is.na(data.ls))

    # Remove any columns between time and 'A1'
    A1.ndx <- match("A1", data.ls[[noNA.ndx[1]]][1,])
    if(A1.ndx>2){
      for(i in 1:length(data.ls)){
        if(length(data.ls[[i]]) > 1){
          data.ls[[i]] <- data.ls[[i]][ ,c(1,A1.ndx:ncol(read.data[[i]]))]
        }
      }
    }
    # apply identifiers specified in mapping file
    for(i in noNA.ndx){
      if(!is.null(mapping)){
        # assign names to samples
        map.ndx <- match(data.ls[[i]][1,1:ncol( data.ls[[i]])], mapping[2:nrow(mapping),1])+1
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
    names(data.ls) <- c("density", "fluorescence1", "fluorescence2")
    grodata <- read_data(data.density = data.ls[[1]], data.fluoro1 = data.ls[[2]], data.fluoro2 = data.ls[[3]], subtract.blank = subtract.blank, calibration = calibration)

    return(grodata)
  }

parse_Gen5Gen6_shiny <- function(data, density.nm, fl1.nm, fl2.nm)
{
  # get row numbers for "time" in column 2
  time.ndx <- grep("\\btime\\b", data[[2]], ignore.case = T)
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
    read.data[[length(read.ndx)]] <- as.data.frame(read.data[[length(read.ndx)]])[1:length(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0][!is.na(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0])]),]
  } else {
    read.data[[1]] <- data.frame(data[read.ndx:(read.ndx + match(NA, data[read.ndx:nrow(data),3])-2),2:(1+ncol)])
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
  well_format <- gsub("[[:space:]].+", "", data[grep("Plate Type", data[,1]), 2])
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
  if (!is.null(density.nm) && density.nm != "Ignore")
    density <- read.data[[match(density.nm, reads)]]
  else
    density  <- NA

  if(!is.null(fl1.nm) && fl1.nm != "Ignore"){
    fluorescence1 <-  read.data[[match(fl1.nm, reads)]]
    fluorescence1[which(fluorescence1 == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence1 <- NA

  if(!is.null(fl2.nm) && fl2.nm != "Ignore"){
    fluorescence2 <-  read.data[[match(fl2.nm, reads)]]
    fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence2 <- NA


  # density <- read.data[[1]]
  data.ls[[1]] <- density
  data.ls[[2]] <- fluorescence1
  data.ls[[3]] <- fluorescence2

  return(list(data.ls, read.data))
}

write.csv.utf8.BOM <- function(df, filename)
{
  con <- file(filename, "w")
  tryCatch({
    for (i in 1:ncol(df))
      df[,i] = iconv(df[,i], to = "UTF-8")
    writeChar(iconv("\ufeff", to = "UTF-8"), con, eos = NULL)
    write.csv(df, file = con)
  },finally = {close(con)})
}

get_avg_param <- function(table = data.frame(), ndx.rep = list(), param1, param2)
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

get_sd_param <- function(table = data.frame(), ndx.rep = list(), param1, param2)
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

# based on print.help_files_with_topic() in the sources of R-2.10.0.
Rd_fun <- function(x, topic, pkgname   = ""
                   , help_type        = "text"
                   , verbose          = FALSE
                   , try.all.packages = FALSE
                   , keep_section     = TRUE
)
  {
  rdo <- NULL         # prepare the "Rd" object rdo; # is it better to check with "inherit"?
  if(methods::is(x) == "Rd"){  # if(inherits(file, "Rd")) ...
    rdo <- x
  }else{
    if(methods::is(x) != "help_files_with_topic" ){
      # The following comments baffle me now. Does `do.call' resolve the issues?
      #
      # help returns an object of class "help_files_with_topic" the
      #  eval(substitute()) wrapper (I saw it in tkGUI, vzh sasto help.R, sasto:
      #  .tryHelp in question.R) is needed to cover the case when x is a
      #  function. Without this wrapper the result is not correct.

      # Izglezhda, che bez substitute() argumentat se evvaluate-va na nepodochodyasto
      #  myasto.  If x is a name of a function, then the wrapper is not needed.

      # wrk <- eval(substitute(help(x, help_type=help_type
      #            , verbose=verbose
      #            , try.all.packages=try.all.packages)))

      # cat("KUKUKUUUU: ", substitute(x), "   methods::is(x): ", methods::is(x), "\n\n" )

      wrk <- do.call("help",list(x, help_type=help_type
                                 , verbose=verbose
                                 , try.all.packages=try.all.packages))
      x <- wrk
    }
    ## Check for errors! ???

    if(methods::is(x) == "help_files_with_topic"){
      # from print.help_files_with_topic in help.R
      #
      # browser <- getOption("browser")
      topic <- attr(x, "topic")
      type <- attr(x, "type")
      paths <- as.character(x) # removes attributes of x.
      # If more matches are found will `paths' have length > 1?
      file <- paths

      # !!! check for length(paths)==0  !!!! ??
      # but no error is raized, rdo simply remain NULL.
      # the following commands are probably copied from utils:::.getHelpFile
      path <- dirname(file)
      dirpath <- dirname(path)
      pkgname <- basename(dirpath)
      RdDB <- file.path(path, pkgname)

      # cat("\n\nx is: "    ,unclass(x)                      ,"\n\n\n")
      # cat("paths is: ",paths                      ,"\n")
      # cat("file is: ", file                       ,"\n")
      # cat("path is: ", path                       ,"\n")
      # cat("RdDB is: ", paste(RdDB, "rdx", sep="."),"\n")

      if(file.exists(paste(RdDB, "rdx", sep="."))) {
        rdo <- fetchRdDB(RdDB, basename(file))
        # a debugging message, remove later!
        # cat("Class of object returned by \"tools:::fetchRdDB: ", class(rdo),"\n")
        # really returns "Rd".
      }
    }
  }
  if(is.null(rdo))                             # todo: should someting less radical be done?
    stop("rdo object is NULL!")

  if(is.character(keep_section) && length(keep_section)>0){
    tags <- RdTags(rdo)
    keep_tags <- unique(c("\\title","\\name",keep_section))
    rdo[which(!(tags %in% keep_tags))] <-  NULL
  }

  rdo
}

fetchRdDB <- utils::getFromNamespace("fetchRdDB", "tools")
RdTags <- utils::getFromNamespace("RdTags", "tools")





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


