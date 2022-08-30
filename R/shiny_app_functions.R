parse_properties_Gen5Gen6 <- function(file, csvsep=";", dec=".", sheet=1)
{
  # Read table file
  input <- read_file(file, csvsep=csvsep, dec=dec, sheet=sheet)
  # get row numbers for "time" in column 2
  time.ndx <- grep("\\btime\\b", input[[2]], ignore.case = T)
  # extract different read data in dataset
  reads <- unname(unlist(lapply(1:length(time.ndx), function(x) input[time.ndx[x]-2, 1])))
  read.ndx <- time.ndx[!is.na(reads)]
  reads <- reads[!is.na(reads)]
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
           fl2.nm = NULL
  ) {
    if(is.null(data.file)) stop("Please provide the name or path to a table file containing plate reader data in the 'data.file' argument.")
    if(is.null(map.file)) warning("No mapping file was provided. The samples will be identified based on their well position (A1, A2, A3, etc.). Grouping options will not be available if you run any further analysis with QurvE.")
    if(!(software %in% c("Gen5", "Gen6"))) stop("The plate reader control software you provided as 'software' is currently not supported by parse_data(). Supported options are:\n 'Gen5', 'Gen6'.")
    # Read table file
    if (file.exists(data.file)) {
      # Read table file
      input <- read_file(data.file, csvsep=csvsep, dec=dec, sheet=data.sheet)
    } else {
      stop(paste0("File \"", data.file, "\" does not exist."), call. = F)
    }
    if(!is.null(map.file)){
      if (file.exists(map.file)) {
        mapping <- read_file(map.file, csvsep=csvsep, dec=dec, sheet=map.sheet)
      } else {
        stop(paste0("File \"", map.file, "\" does not exist."), call. = F)
      }
    }
    if(any(c("Gen5", "Gen6") %in% software)){
      parsed.ls <- parse_Gen5Gen6_shiny(data = input, density.nm = density.nm, fl1.nm = fl1.nm, fl2.nm = fl2.nm)
      data.ls <- parsed.ls[[1]]
      read.data <- parsed.ls[[2]]
    } # if("Gen5" %in% software)
    # Convert time values to hours
    if(convert.time){
      for(i in 1:length(data.ls[!is.na(data.ls)])){
        data.ls[[i]][2:nrow(data.ls[[1]]),1] <- as.numeric(data.ls[[i]][2:nrow(data.ls[[1]]),1])*24
      }
    }
    noNA.ndx <- which(!is.na(data.ls))

    # Remove any columns between time and 'A1'
    A1.ndx <- match("A1", data.ls[[1]][1,])
    if(A1.ndx>2){
      data.ls <- lapply(noNA.ndx, function(x) data.ls[[x]][ ,c(1,A1.ndx:ncol(read.data[[x]]))])
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
    if(length(data.ls)==1){
      names(data.ls) <- "density"
      grodata <- read_data(data.density = data.ls[[1]], data.fluoro1 = NA, data.fluoro2 = NA, subtract.blank = subtract.blank)
    } else if(length(data.ls)==2){
      names(data.ls) <- c("density", "fluorescence1")
      grodata <- read_data(data.density = data.ls[[1]], data.fluoro1 = data.ls[[2]], data.fluoro2 = NA, subtract.blank = subtract.blank)
    } else {
      names(data.ls) <- c("density", "fluorescence1", "fluorescence2")
      grodata <- read_data(data.density = data.ls[[1]], data.fluoro1 = data.ls[[2]], data.fluoro2 = data.ls[[3]], subtract.blank = subtract.blank)
    }
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
  read.data <- list()
  ncol <- length(data[read.ndx[1],][!is.na(data[read.ndx[1],])])
  # Extract all read tables except the last
  read.data <- lapply(1:(length(read.ndx)-1), function(x) data[read.ndx[x]:(read.ndx[x+1]-3),2:(ncol)])
  read.data <- lapply(1:length(read.data), function(x) as.data.frame(read.data[[x]])[1:length(read.data[[x]][,1][read.data[[x]][,1]!=0][!is.na(read.data[[x]][,1][read.data[[x]][,1]!=0])]),])
  # Extract last read table
  read.data[[length(read.ndx)]] <- data.frame(data[read.ndx[length(read.ndx)]:(read.ndx[length(read.ndx)]+length(read.data[[1]][[1]])-1),2:(ncol)])
  read.data[[length(read.ndx)]] <- as.data.frame(read.data[[length(read.ndx)]])[1:length(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0][!is.na(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0])]),]
  # Remove time points with NA in all samples
  for(i in 1:length(read.data))
    read.data[[i]] <- cbind(read.data[[i]][,1][1:length(read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ][, 2])],
                            read.data[[i]][,2:ncol(read.data[[i]])][rowSums(is.na(read.data[[i]][,2:ncol(read.data[[i]])]))<ncol(read.data[[i]][,2:ncol(read.data[[i]])]), ])
  # give all reads the same time values as the first read
  for(i in 2:length(read.data)){
    read.data[[i]][[1]] <- read.data[[1]][[1]]
  }
  names(read.data) <- reads
  data.ls <- list()
  if (!is.null(density.nm) && density.nm != "Ignore")
    density <- read.data[[match(density.nm, reads)]]
  else
    density <-  read.data[[match(density.nm, reads)]] <- NULL

  if(!is.null(fl1.nm) && fl1.nm != "Ignore"){
    fluorescence1 <-  read.data[[match(fl1.nm, reads)]]
    fluorescence1[which(fluorescence1 == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence1 <- NULL

  if(!is.null(fl2.nm) && fl2.nm != "Ignore"){
    fluorescence2 <-  read.data[[match(fl2.nm, reads)]]
    fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
  }
  else
    fluorescence2 <- NULL


  density <- read.data[[1]]
  data.ls[[1]] <- density
  data.ls[[2]] <- fluorescence1
  data.ls[[3]] <- fluorescence2

  return(list(data.ls, read.data))
}
