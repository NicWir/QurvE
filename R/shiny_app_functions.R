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

parse_properties_chibio <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)

  time.ndx <- grep("time", input[1,], ignore.case = T)
  # extract different read data in dataset
  read.ndx <- grep("measured|emit", input[1,], ignore.case = T)
  reads <- input[1, read.ndx]
  suppressWarnings(
    reads[!is.na(as.numeric(reads))] <- gsub("\\.", ",", as.numeric(reads[!is.na(as.numeric(reads))]))
  )
  reads <- reads[!is.na(reads)]
  reads <- unique(reads)
  return(reads)
}

parse_properties_tecan <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get row numbers for "time" in column 2
  time.ndx <- grep("^\\btime\\b", input[[1]], ignore.case = T)
  time.ndx <- time.ndx[-1]
  # extract different read data in dataset
  reads <- unname(unlist(lapply(1:length(time.ndx), function(x) input[time.ndx[x]-2, 1])))
  suppressWarnings(
    reads[!is.na(as.numeric(reads))] <- gsub("\\.", ",", as.numeric(reads[!is.na(as.numeric(reads))]))
  )
  reads <- reads[!is.na(reads)]
  reads <- unique(reads)
  return(reads)
}

parse_properties_biolector <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get row numbers for "time" in column 2
  time.ndx <- grep("\\btime\\b", input[[2]], ignore.case = T)
  # extract different read data in dataset
  time.ndx <- c(grep("^\\bWell\\b", input[,1], ignore.case = T)+2, grep("^\\bChannel\\b", input[grep("^\\bWell\\b", input[,1], ignore.case = T),], ignore.case = T))
  # extract different read data in dataset
  reads <- unique(input[,time.ndx[2]][grep("Biomass", input[,time.ndx[2]])])
  reads <- reads[!is.na(reads)]
  return(reads)
}

parse_properties_victornivo <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[,2], ignore.case = T)
  # extract different read data in dataset
  reads <- lapply(1:length(time.ndx), function(x) input[time.ndx-6, 2])
  reads <- reads[!is.na(reads)]
  return(reads)
}

parse_properties_victorx3 <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[1,], ignore.case = T)
  # extract different read data in dataset
  reads <- unlist(lapply(1:length(time.ndx), function(x) input[1, time.ndx[x]+1]))
  reads <- reads[!is.na(reads)]
  return(reads)
}

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
    # Read table file
    if (file.exists(data.file)) {
      # Read table file
      input <- read_file(data.file, csvsep=csvsep.data, dec=dec.data, sheet=sheet.data)
    } else {
      stop(paste0("File \"", data.file, "\" does not exist."), call. = F)
    }
    if(!is.null(map.file)){
      if (file.exists(map.file)) {
        mapping <- read_file(map.file, csvsep=csvsep.map, dec=dec.map, sheet=sheet.map)
      } else {
        stop(paste0("File \"", map.file, "\" does not exist."), call. = F)
      }
    } else {
      mapping <- NULL
    }
    if(any(grep("Gen5|Gen6", software, ignore.case = T))){
      parsed.ls <- parse_Gen5Gen6_shiny(data = input, density.nm = density.nm, fl1.nm = fl1.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    } # if("Gen5" %in% software)
    if(any(grep("Chi.Bio", software, ignore.case = T))){
      parsed.ls <- parse_chibio_shiny(input, density.nm = density.nm, fl1.nm = fl1.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    }
    if(any(grep("GrowthProfiler", software, ignore.case = T))){
      parsed.ls <- parse_growthprofiler(input)
      data.ls <- parsed.ls[[1]]
    }
    if(any(grep("Tecan", software, ignore.case = T))){
      parsed.ls <- parse_tecan_shiny(input, density.nm = density.nm, fl1.nm = fl1.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("Biolector", software, ignore.case = T))){
      parsed.ls <- parse_biolector_shiny(input, density.nm = density.nm)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("VictorNivo", software, ignore.case = T))){
      parsed.ls <- parse_victornivo_shiny(input, density.nm = density.nm, fl1.nm = fl1.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("VictorX3", software, ignore.case = T))){
      parsed.ls <- parse_victorx3_shiny(input, density.nm = density.nm, fl1.nm = fl1.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
    }

    noNA.ndx <- which(!is.na(data.ls))

    # Convert time values
    if(!is.null(convert.time)){

      conversion <- parse(text = convert.time)

      for(i in 1:length(data.ls[!is.na(data.ls)])){
        x <- as.numeric(data.ls[[i]][2:nrow(data.ls[[1]]),1])
        time_converted <- eval(conversion)
        data.ls[[i]][2:nrow(data.ls[[1]]),1] <- time_converted
      }
    }

    if(any(c("Gen5", "Gen6") %in% software)){
      # Remove any columns between time and 'A1' (for plate readers)
      A1.ndx <- match("A1", data.ls[[noNA.ndx[1]]][1,])
      if(A1.ndx>2){
        for(i in 1:length(data.ls)){
          if(length(data.ls[[i]]) > 1){
            data.ls[[i]] <- data.ls[[i]][ ,c(1,A1.ndx:ncol(data.ls[[i]]))]
          }
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

  return(list(data.ls))
}

parse_chibio_shiny <- function(input, density.nm, fl1.nm, fl2.nm)
{
  time.ndx <- grep("time", input[1,], ignore.case = T)
  read.ndx <- grep("measured|emit", input[1,], ignore.case = T)
  reads <- input[1, read.ndx]

  data.ls <- list()
  if(length(reads)>1){
    if (!is.null(density.nm) && density.nm != "Ignore"){
      density <- data.frame("time" = input[, time.ndx], "density" = c(input[1,read.ndx[match(density.nm, reads)]], as.numeric(input[-1, read.ndx[match(density.nm, reads)]])))
      if(all(as.numeric(density[-1,2]) == 0) || all(is.na(density[-1,2]))){
        density <- NA
      }
    }
    else
      density  <- NA

    if (!is.null(fl1.nm) && fl1.nm != "Ignore"){
      fluorescence1 <- data.frame("time" = input[, time.ndx], "density" = c(input[1,read.ndx[match(fl1.nm, reads)]], as.numeric(input[-1, read.ndx[match(fl1.nm, reads)]])))
      if(all(as.numeric(fluorescence1[-1,2]) == 0) || all(is.na(fluorescence1[-1,2]))){
        fluorescence1 <- NA
      }
    }
    else
      fluorescence1  <- NA

    if (!is.null(fl2.nm) && fl2.nm != "Ignore"){
      fluorescence2 <- data.frame("time" = input[, time.ndx], "density" = c(input[1,read.ndx[match(fl2.nm, reads)]], as.numeric(input[-1, read.ndx[match(fl2.nm, reads)]])))
      if(all(as.numeric(fluorescence2[-1,2]) == 0) || all(is.na(fluorescence2[-1,2]))){
        fluorescence2 <- NA
      }
    }
    else
      fluorescence2  <- NA
  } else {
    density <- data.frame("time" = input[, time.ndx], "density" = c(input[1, read.ndx], as.numeric(input[-1, read.ndx])))
    fluorescence1 <- NA
    fluorescence2 <- NA
  }

  data.ls[[1]] <- density
  data.ls[[2]] <- fluorescence1
  data.ls[[3]] <- fluorescence2

  return(list(data.ls))
}

parse_tecan_shiny <- function(input, density.nm, fl1.nm, fl2.nm)
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

  data.ls[[1]] <- density
  data.ls[[2]] <- fluorescence1
  data.ls[[3]] <- fluorescence2

  return(list(data.ls))
}

parse_biolector_shiny <- function(input, density.nm)
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

    density <- read.data[[match(density.nm, reads)]]

    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    data.ls[[3]] <- NA

  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    data.ls[[3]] <- NA
  }
  return(list(data.ls))
}

parse_victornivo_shiny <- function(input, density.nm, fl1.nm, fl2.nm)
{
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[,2], ignore.case = T)
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

  return(list(data.ls))
}

parse_victorx3_shiny <- function(input, density.nm, fl1.nm, fl2.nm)
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
          read.table(text = read.data[[x]][,3], sep = ":")
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
      pivot_wider(data = read.data[[x]], names_from= well, values_from = read))
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
        read.table(text = read.data[[1]][,2], sep = ":")
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
    read.data[[1]] <- pivot_wider(data = read.data[[1]], names_from= well, values_from = read)
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

  return(list(data.ls))
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


shinyInputLabel <- utils::getFromNamespace("shinyInputLabel", "shiny")

#' Create a numeric input
#'
#' Create an input control for entry of numeric values. This is identical to
#' [shiny::numericInput()] but is more flexible in **not** requiring an initial
#' value and in allowing placeholders.
#'
#'
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
#' @seealso [shiny::updateNumericInput()]
#'
#' @examples
#'
#' if (interactive()) {
#' library(shiny)
#' library(shinysurveys)
#'
#' ui <- fluidPage(
#'   numberInput("obs", "Observations:", placeholder = "How many do you see?", min = 1, max = 100),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output) {
#'   output$value <- renderText({ input$obs })
#' }
#' shinyApp(ui, server)
#' }
#'
#' @section Server value: A numeric vector of length 1.
#'
#' @export
#'
#' @references Trattner, J. (2021) shinysurveys: Create and Deploy Surveys in 'Shiny' (R package version 0.2.0)
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
