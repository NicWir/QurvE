#' Read growth and fluorescence data in table format
#'
#' \code{read_data} reads table files or R dataframe objects containing growth and fluorescence data and extracts datasets, sample and group information, performs blank correction, applies data transformation (calibration), and combines technical replicates.
#'
#' @param data.growth An R dataframe object or a table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing growth data. The data must be either in the '`QurvE` custom layout' or in 'tidy' (long) format.
#' The first three table rows in the 'custom `QurvE` layout' contain:
#' \enumerate{
#'    \item Sample description
#'    \item Replicate number (_optional_: followed by a letter to indicate technical replicates)
#'    \item Concentration value (_optional_)
#' }
#' Data in 'tidy' format requires the following column headers:
#' \enumerate{
#'    \item "Time": time values
#'    \item "Description": sample description
#'    \item "Replicate": replicate number (_optional_)
#'    \item "Concentration": concentration value (_optional_)
#'    \item "Values": growth values (e.g., optical density)
#' }
#' @param data.fl (optional) An R dataframe object or a table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing fluorescence data. Table layout must mimic that of \code{data.growth}.
#' @param data.fl2 (optional) An R dataframe object or a table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing measurements from a second fluorescence channel (used only to normalize \code{fluorescence} data). Table layout must mimic that of \code{data.growth}.
#' @param data.format (Character) "col" for samples in columns, or "row" for samples in rows. Default: \code{"col"}
#' @param csvsep (Character) separator used in CSV file storing growth data (ignored for other file types). Default: \code{";"}
#' @param csvsep.fl,csvsep.fl2 (Character) separator used in CSV file storing fluorescence data (ignored for other file types). Default: \code{";"}
#' @param dec (Character) decimal separator used in CSV, TSV or TXT file storing growth data. Default: \code{"."}
#' @param dec.fl,dec.fl2 (Character) decimal separator used in CSV, TSV or TXT file storing fluorescence data. Default: \code{"."}
#' @param subtract.blank (Logical) Shall blank values be subtracted from values within the same experiment ([TRUE], the default) or not ([FALSE]).
#' @param sheet.growth,sheet.fl,sheet.fl2 (Numeric or Character) Number or name of the sheet with the respective data type in XLS or XLSX files (_optional_).
#' @param fl.normtype (Character string) Normalize fluorescence values by either diving by \code{'growth'} or by fluorescence2 values (\code{'fl2'}).
#' @param convert.time (\code{NULL} or string) Convert time values with a formula provided in the form \code{'y = function(x)'}.
#' For example: \code{convert.time = 'y = 24 * x'}
#' @param calib.growth,calib.fl,calib.fl2 (Character or \code{NULL}) Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert growth and fluorescence values. This can be used to, e.g., convert plate reader absorbance values into \ifelse{html}{\out{OD<sub>600</sub>}}{\eqn{OD_{600}}} or fluorescence intensity into molecule concentrations.
#' Caution!: When utilizing calibration, carefully consider whether or not blanks were subtracted to determine the calibration before selecting the input \code{subtract.blank = TRUE}.
#'
#' @details
#' \figure{Data-layout.jpg}
#'
#' @return An R list object of class \code{grodata} containing a \code{time} matrix, dataframes with growth and fluorescence data (if applicable),
#' and an experimental design table. The \code{grodata} object can be directly
#'   used to run \code{\link{growth.workflow}}/\code{\link{fl.workflow}} or, together with a \code{growth.control}/\code{fl.control}
#'   object, in \code{\link{growth.gcFit}}/\code{\link{flFit}}.
#' \item{time}{Matrix with raw time values extracted from \code{data.growth}.}
#' \item{growth}{Dataframe with raw growth values and sample identifiers extracted from \code{data.growth}.}
#' \item{fluorescence}{Dataframe with raw fluorescence values and sample identifiers extracted from \code{data.fl}. \code{NA}, if no fluorescence data is provided.}
#' \item{norm.fluorescence}{fluorescence data divided by growth values. \code{NA}, if no fluorescence data is provided.}
#' \item{expdesign}{Experimental design table created from the first three identifier rows/columns (see argument \code{data.format}) (\code{data.growth}.}
#'
#' @export
#' @importFrom methods is
#' @import dplyr stringr tidyr
#' @md
#' @examples
#' # Load CSV file containing only growth data
#' data_growth <- read_data(data.growth = system.file("2-FMA_toxicity.csv",
#'                          package = "QurvE"), csvsep = ";" )
#'
#' # Load XLS file containing both growth and fluorescence data
#' data_growth_fl <- read_data(
#'                     data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
#'                     data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
#'                     csvsep = "\t",
#'                     csvsep.fl = "\t")
read_data <-
  function(data.growth = NA,
           data.fl = NA,
           data.fl2 = NA,
           data.format = "col",
           csvsep = ";",
           dec = ".",
           csvsep.fl = ";",
           dec.fl = ".",
           csvsep.fl2 = ";",
           dec.fl2 = ".",
           sheet.growth = 1,
           sheet.fl = 1,
           sheet.fl2 = 1,
           fl.normtype = c("growth", "fl2"),
           subtract.blank  = TRUE,
           convert.time = NULL,
           calib.growth = NULL,
           calib.fl = NULL,
           calib.fl2 = NULL)
  {
    if(is.null(data.growth)) data.growth <- NA
    if(is.null(data.fl)) data.fl <- NA
    if(is.null(data.fl2)) data.fl2 <- NA
    if(!is.null(calib.growth) && calib.growth == "") calib.growth <- NULL
    if(!is.null(calib.fl) && calib.fl == "") calib.fl <- NULL
    if(!is.null(calib.fl2) && calib.fl2 == "") calib.fl2 <- NULL

    fl.normtype <- match.arg(fl.normtype)

    # Load growth data
    if (any(is(data.growth) %in% c("matrix", "list", "array")) || !is.character(data.growth)) {
      dat <- data.growth
    } else {
      # Read table file
      if(!is.na(data.growth))
        dat <- read_file(data.growth, csvsep=csvsep, dec=dec, sheet=sheet.growth)
    }
    # Test if growth data is in tidy format and convert into QurvE custom format
    if(length(dat) > 1)
      dat <- tidy_to_custom(df = dat, data.format = data.format)
    # Remove explicit quotes
    #dat <- gsub('\"', "", dat)

    # Convert time values
    if(!is.null(convert.time)){

      conversion <- parse(text = convert.time)

      x <- as.numeric(dat[1, -(1:3)])
      time_converted <- eval(conversion)
      dat[1, -(1:3)] <- time_converted
    }
    # Remove all-NA data series
    if(length(dat) > 1){
      allNA.ndx <- which(unlist(lapply(1:nrow(dat), function(x) all(is.na(dat[x, -(1:3)])))))
      if(length(allNA.ndx) > 0)
        dat <- dat[-allNA.ndx, ]
    }

    #remove leading and trailing zeros
    if(length(dat)>0 && !all(is.na(dat)))
      dat[,3] <- suppressWarnings(
        as.character(as.numeric(dat[,3]))
      )

    if(data.format == "col"){
      message("Sample data are stored in columns. If they are stored in row format, please run read_data() with data.format = 'row'.")
    } else {
      message("Sample data are stored in rows. If they are stored in column format, please run read_data() with data.format = 'col'.")
    }
    # Load fluorescence data
    if((length(data.fl) > 1 ) || !all(is.na(data.fl))){
      if (!is.character(data.fl)) {
        fl <- data.fl
      } else {
        # Read table file
        fl <- read_file(data.fl, csvsep=csvsep.fl, dec=dec.fl, sheet=sheet.fl)
      }
      # Test if fluorescence data is in tidy format and convert into QurvE custom format
      fl <- tidy_to_custom(df = fl, data.format = data.format)

      if(!(any(grepl("time", unlist(fl[,1]), ignore.case = TRUE)))){
        if(data.format == "col"){
          stop("Could not find 'time' in column 1 of data.fl")
        } else {
          stop("Could not find 'time' in row 1 of data.fl")
        }
      }
      # Remove all-NA data series
      allNA.ndx <- which(unlist(lapply(1:nrow(fl), function(x) all(is.na(fl[x, -(1:3)])))))
      if(length(allNA.ndx) > 0)
        fl <- fl[-allNA.ndx, ]

      #remove leading and trailing zeros
      fl[,3] <- as.character(as.numeric(fl[,3]))

      # Convert time values
      if(!is.null(convert.time)){

        conversion <- parse(text = convert.time)

        x <- as.numeric(fl[1, -(1:3)])
        time_converted <- eval(conversion)
        fl[1, -(1:3)] <- time_converted
      }
      # add minimum negative value + 1 to all fluorescence data
    } else {
      fl <- NA
    }
    # Load fluorescence 2 data
    if((length(data.fl2) > 1 ) || !is.na(data.fl2)){
      if (!is.character(data.fl2)) {
        fl2 <- data.fl2
      } else {
        # Read table file
        fl2 <- read_file(data.fl2, csvsep=csvsep.fl2, dec=dec.fl2, sheet=sheet.fl2)
      }
      # Test if fluorescence data is in tidy format and convert into QurvE custom format
      fl2 <- tidy_to_custom(df = fl2, data.format = data.format)

      if(!(any(grepl("time", unlist(fl2[,1]), ignore.case = TRUE)))){
        if(data.format == "col"){
          stop("Could not find 'time' in column 1 of data.fl2")
        } else {
          stop("Could not find 'time' in row 1 of data.fl2")
        }
      }
    } else {
      fl2 <- NA
    }
    if((length(dat) > 1 && !(any(grepl("time", unlist(dat[,1]), ignore.case = TRUE)))) &&
       (length(fl) > 1 && !(any(grepl("time", unlist(fl[,1]), ignore.case = TRUE)))) ){ #&& (length(fl2) > 1 && !(any(grepl("time", unlist(fl2[,1]), ignore.case = TRUE))))
      if(data.format == "col"){
        stop("Could not find 'time' in column 1 of any provided 'data.growth' or 'data.fl'.")
      } else {
        stop("Could not find 'time' in row 1 of any provided 'data.growth' or 'data.fl'.")
      }
    }

    if(!is.null(calib.growth)){
      if(length(dat)>1)
        dat <- calibrate(dat, calib.growth)
    }
    if(!is.null(calib.fl)){
      if((length(fl) > 1 ) || !is.na(data.fl))
        fl <- calibrate(fl, calib.fl)
    }
    if(!is.null(calib.fl2)){
      if((length(fl2) > 1 ) || !is.na(data.fl2))
        fl2 <- calibrate(fl2, calib.fl2)
    }

    # subtract blank
    if(subtract.blank){
      subtract_blank <- function(df){
        #test if more than one time entity is present
        time.ndx <- grep("time", unlist(df[,1]), ignore.case = TRUE)
        if(length(time.ndx)==1){
          blank.ndx <- grep("blank", df[1:nrow(df),1], ignore.case = TRUE)
          if(length(blank.ndx)>0){
            if(length(blank.ndx)>1){
              blank <- rowMeans(apply(df[blank.ndx, 4:ncol(df)], 1, as.numeric), na.rm = TRUE)
            } else {
              blank <- as.numeric(df[blank.ndx, 4:ncol(df)])
            }
            df[(2:nrow(df))[!((2:nrow(df)) %in% blank.ndx)], 4:ncol(df)] <- t(sweep(apply(df[(2:nrow(df))[!((2:nrow(df)) %in% blank.ndx)], 4:ncol(df)], 1, as.numeric), 1, blank))
          }
        } else { # identify different datasets based on the occurence of multiple 'time' entities
          # identify additional time entities
          blank.ndx <- grep("blank", df[(time.ndx[1]) : (time.ndx[2]-1),1], ignore.case = TRUE)
          if(length(blank.ndx)>0){
            if(length(blank.ndx)>1){
              blank <- rowMeans(apply(df[blank.ndx, 4:ncol(df)], 1, as.numeric))
            }else {
              blank <- as.numeric(df[blank.ndx, 4:ncol(df)])
            }
            df[((time.ndx[1] + 1):(time.ndx[2] - 1))[!(((time.ndx[1] + 1):(time.ndx[2] - 1)) %in% blank.ndx)], 4:ncol(df)] <-
              t(sweep(apply(df[((time.ndx[1] + 1):(time.ndx[2] - 1))[!(((time.ndx[1] + 1):(time.ndx[2] - 1)) %in% blank.ndx)], 4:ncol(df)], 1, as.numeric), 1, blank))
          }
          for (i in 2:(length(time.ndx))){
            blank.ndx <- grep("blank", df[if (is.na(time.ndx[i + 1])) {
              (time.ndx[i] + 1):nrow(df)
            } else {
              (time.ndx[i] + 1):(time.ndx[i + 1] - 1)
            }, 1], ignore.case = TRUE) + time.ndx[i]

            if(length(blank.ndx)>0){
              if(length(blank.ndx)>1){
                blank <- rowMeans(apply(df[blank.ndx, 4:ncol(df)], 1, as.numeric))
              } else {
                blank <- as.numeric(df[blank.ndx, 4:ncol(df)])
              }
              df[if (is.na(time.ndx[i + 1])) {
                ((time.ndx[i] + 1):nrow(df))[!((time.ndx[i] + 1):nrow(df) %in% blank.ndx)]
              } else {
                ((time.ndx[i] + 1):(time.ndx[i + 1] - 1))[!(((time.ndx[i] + 1):(time.ndx[i + 1] - 1)) %in% blank.ndx)]
              }, 4:ncol(df)] <-
                t(sweep(apply(df[if (is.na(time.ndx[i + 1])) {
                  ((time.ndx[i] + 1):nrow(df))[!((time.ndx[i] + 1):nrow(df) %in% blank.ndx)]
                } else {
                  ((time.ndx[i] + 1):(time.ndx[i + 1] - 1))[!(((time.ndx[i] + 1):(time.ndx[i + 1] - 1)) %in% blank.ndx)]
                }, 4:ncol(df)], 1, as.numeric), 1, blank))
            }
          } # end of for (i in 2:(length(time.ndx)))
        } # end of else of if(length(time.ndx)==1)
        return(df)
      }
      if(length(dat)>1)             dat <- subtract_blank(dat)
      if((length(fl) > 1 ) || !is.na(data.fl))    fl <- subtract_blank(df=fl)
      if((length(fl2) > 1 ) || !is.na(data.fl2))    fl2 <- subtract_blank(df=fl2)
    }

    ### Combine technical replicates
    combine_techrep <- function(df){
      sample_names <- as.character(paste0(df[2:nrow(df),1], "...", df[2:nrow(df),2], "___", df[2:nrow(df),3]))
      conditions <-
        unique(gsub("\\.\\.\\..+___", "___", sample_names))
      # remove "time" from samples in case of several time entities
      time.ndx <- grep("time", unlist(df[,1]), ignore.case = TRUE)
      if(length(time.ndx)>1){
        conditions <- conditions[-grep("time", gsub("___.+", "", conditions), ignore.case = TRUE)]
      }
      # remove blanks from conditions
      blankcond.ndx <- grep("blank", gsub("___.+", "", conditions), ignore.case = TRUE)
      if(length(blankcond.ndx)>1){
        conditions <- conditions[-blankcond.ndx]
      }

      remove <- c()
      for(i in 1:length(conditions)){
        ndx.cond <-  which(gsub("\\.\\.\\..+___", "___", sample_names) %in% conditions[i])
        name <- df[ndx.cond[1]+1,1]
        conc <- df[ndx.cond[1]+1,3]
        tech.rep <- suppressWarnings(as.numeric(unique(gsub("___.+", "", gsub(".+\\.\\.\\.", "", sample_names[ndx.cond])))))
        tech.rep <- tech.rep[!is.na(tech.rep)]
        if(length(tech.rep)>1){
          for(j in 1:length(tech.rep)){
            ndx.rep <- ndx.cond[which(gsub("___.+", "", gsub(".+\\.\\.\\.", "", sample_names[ndx.cond])) %in% tech.rep[j])]
            if(length(ndx.rep)>1){
              values <- apply(df[ndx.rep+1, 4:ncol(df)], 1, as.numeric)
              means <- rowMeans(values)
              df[ndx.rep[1]+1, 4:ncol(df)] <- means
              df[ndx.rep[1]+1, 2] <- as.numeric(tech.rep[j])
              remove <- c(remove, ndx.rep[-1]+1)
            } else {
              df[ndx.rep[1]+1, 2] <- as.numeric(tech.rep[j])
            }
          }
        }
      }
      if(length(remove)>1){
        df <- df[-remove,]
      }
      return(df)
    }
    if(length(dat)>1)              dat <- combine_techrep(dat)
    if((length(fl) > 1 ) || !is.na(data.fl))     fl <- combine_techrep(df=fl)
    if((length(fl2) > 1 ) || !is.na(data.fl2))     fl2 <- combine_techrep(df=fl2)


    # remove blank columns from dataset
    remove_blank <- function(df){
      blank.ndx <- grep("blank", df[1:nrow(df),1], ignore.case = TRUE)
      if(length(blank.ndx)>0){
        df <- df[-blank.ndx, ]
      }
      return(df)
    }
    if(length(dat)>1)              dat <- remove_blank(dat)
    if((length(fl) > 1 ) || !is.na(data.fl))     fl <- remove_blank(df=fl)
    if((length(fl2) > 1 ) || !is.na(data.fl2))     fl2 <- remove_blank(df=fl2)

    # Remove columns with NA measurements in all samples
    if(length(dat)>1)              dat <- dat[, c(1:3, which(unlist(lapply(4:ncol(dat), function(x)!all(is.na(dat[2:nrow(dat),x])))))+3)]
    if((length(fl) > 1 ) || !is.na(data.fl))     fl <- fl[, c(1:3, which(unlist(lapply(4:ncol(fl), function(x)!all(is.na(fl[2:nrow(fl),x])))))+3)]
    if((length(fl2) > 1 ) || !is.na(data.fl2))     fl2 <- fl2[, c(1:3, which(unlist(lapply(4:ncol(fl2), function(x)!all(is.na(fl2[2:nrow(fl2),x])))))+3)]

    # add minimum negative value + 1 to all fluorescence data
    if((length(fl) > 1 ) || !is.na(data.fl)){
      num.fl <- t(apply(fl[2:nrow(fl), 4:ncol(fl)], 1, as.numeric))
      min.F1 <- unique(num.fl[which(num.fl == min(num.fl, na.rm = TRUE), arr.ind = TRUE)])
      if(min.F1 <=0){
        fl[2:nrow(fl), 4:ncol(fl)] <- num.fl+(-min.F1)+1
      }
    }
    if((length(fl2) > 1 ) || !is.na(data.fl2)){
      num.fl2 <- t(apply(fl2[2:nrow(fl2), 4:ncol(fl2)], 1, as.numeric))
      min.F2 <- unique(num.fl2[which(num.fl2 == min(num.fl2, na.rm = TRUE), arr.ind = TRUE)])
      if(min.F2 <=0){
        fl2[2:nrow(fl2), 4:ncol(fl2)] <- num.fl2+(-min.F2)+1
      }
    }

    # normalize fluorescence
    if(fl.normtype == "growth"){
      if(((length(fl) > 1 ) || !is.na(data.fl)) && length(dat)>1){
        fl.norm <- fl
        time.ndx <- grep("time", unlist(fl.norm[,1]), ignore.case = TRUE)
        fl.norm[-time.ndx, 4:ncol(fl.norm)] <-
          t(apply(fl.norm[-time.ndx, 4:ncol(fl.norm)], 1, as.numeric))/t(apply(dat[-time.ndx, 4:ncol(dat)], 1, as.numeric))
      }
    }
    if(fl.normtype == "fl2"){
      if(((length(fl) > 1 ) || !is.na(data.fl2)) && length(fl2)>1){
        fl.norm <- fl
        time.ndx <- grep("time", unlist(fl.norm[,1]), ignore.case = TRUE)
        fl.norm[-time.ndx, 4:ncol(fl.norm)] <-
          t(apply(fl.norm[-time.ndx, 4:ncol(fl.norm)], 1, as.numeric))/t(apply(fl2[-time.ndx, 4:ncol(fl2)], 1, as.numeric))
      }
    }
    # if(((length(fl2) > 1 ) || !is.na(data.fl2)) && length(dat)>1){
    #   fl2.norm <- fl
    #   time.ndx <- grep("time", unlist(fl2.norm[,1]), ignore.case = TRUE)
    #   fl2.norm[-time.ndx, 4:ncol(fl2.norm)] <-
    #     t(apply(fl2.norm[-time.ndx, 4:ncol(fl2.norm)], 1, as.numeric))/t(apply(dat[-time.ndx, 4:ncol(dat)], 1, as.numeric))
    # }

    # Create time matrix
    if(length(dat) > 1) {
      time.ndx <- grep("time", unlist(dat[,1]), ignore.case = TRUE)
      dat.time <- dat
    } else if(length(fl) > 1){
      time.ndx <- grep("time", unlist(fl[,1]), ignore.case = TRUE)
      dat.time <- fl
    }
    # else if(length(fl2) > 1){
    #   time.ndx <- grep("time", unlist(fl2[,1]), ignore.case = TRUE)
    #   dat.time <- fl2
    # }

    if(length(time.ndx)==1){
      time <- as.numeric(unlist(dat.time[time.ndx[1],4:ncol(dat.time)]))
      t.mat <- data.matrix(data.frame(matrix(
        data = rep(time, nrow(dat.time)-1),
        nrow = nrow(dat.time)-1,
        byrow = T
      )))
    } else { # identify different datasets based on the occurence of multiple 'time' entities
      time <- list()
      time[[1]] <- as.numeric(unlist(dat.time[time.ndx[1],4:ncol(dat.time)]))
      t.mat <- data.matrix(data.frame(matrix(
        data = rep(time[[1]], time.ndx[2]-time.ndx[1]-1),
        nrow = time.ndx[2]-time.ndx[1]-1,
        byrow = T
      )))
      for (i in 2:(length(time.ndx))){
        time[[i]] <- as.numeric(unlist(dat.time[time.ndx[i],4:ncol(dat.time)]))
        t.mat <- rbind(t.mat,
                       data.matrix(data.frame(
                         matrix(
                           data = rep(time[[i]], times = if (is.na(time.ndx[i + 1])) {
                             nrow(dat.time) - time.ndx[i]
                           } else {
                             time.ndx[i + 1] - time.ndx[i] - 1
                           }),
                           nrow = if (is.na(time.ndx[i + 1])) {
                             nrow(dat.time) - time.ndx[i]
                           } else {
                             time.ndx[i + 1] - time.ndx[i] - 1
                           },
                           byrow = TRUE)
                       )
                       )
        )
      } # end of for (i in 2:(length(time.ndx)))
    } # end of else {}

    # Create data matrices for growth and fluorescence values
    create_datmat <- function(df, time.ndx){
      if(length(time.ndx)==1){
        df.mat <- data.frame(df[(time.ndx[1]+1):nrow(df),])
      } else { # identify different datasets based on the occurence of multiple 'time' entities
        df.mat <- data.frame(df[(time.ndx[1]+1) : (time.ndx[2]-1), ])
        for (i in 2:(length(time.ndx))){
          df.mat <- rbind(df.mat,
                          data.frame(df[ if (is.na(time.ndx[i + 1])) {
                            (time.ndx[i]+1) : nrow(df)
                          } else {
                            (time.ndx[i]+1) : (time.ndx[i+1] - 1)
                          } , ])
          )
        } # end of for (i in 2:(length(time.ndx)))
      } # end of else {}
      return(df.mat)
    }
    if(length(dat)>1){
      dat.mat <- create_datmat(dat, time.ndx=time.ndx)
      if(ncol(dat.mat) == 1) dat.mat <- t(dat.mat)
      dat.mat[,1] <- gsub("\\n\\r|\\n|\\r", "", dat.mat[,1])
    } else{
      dat.mat <- NA
    }
    if((length(fl) > 1 ) || !is.na(data.fl)){fl.mat <- create_datmat(df=fl, time.ndx=time.ndx);  fl.mat[,1] <- gsub("\\n\\r|\\n|\\r", "", fl.mat[,1])}else{fl.mat <- NA}
    # if((length(fl2) > 1 ) || !is.na(data.fl2)){fl2.mat <- create_datmat(df=fl2, time.ndx=time.ndx);  fl2.mat[,1] <- gsub("\\n\\r|\\n|\\r", "", fl2.mat[,1])}else{fl2.mat <- NA}
    if(((length(fl) > 1 ) || !is.na(data.fl)) && length(dat)>1){fl.norm.mat <- create_datmat(df=fl.norm, time.ndx=time.ndx);  fl.norm.mat[,1] <- gsub("\\n\\r|\\n|\\r", "", fl.norm.mat[,1])}else{fl.norm.mat <- NA}
    # if(((length(fl2) > 1 ) || !is.na(data.fl2)) && length(dat)>1){fl2.norm.mat <- create_datmat(df=fl2.norm, time.ndx=time.ndx);  fl2.norm.mat[,1] <- gsub("\\n\\r|\\n|\\r", "", fl2.norm.mat[,1])}else{fl2.norm.mat <- NA}

    if(length(dat)>1)             colnames(dat.mat)[1:3] <- c("condition", "replicate", "concentration")
    if((length(fl) > 1 ) || !is.na(data.fl))    colnames(fl.mat)[1:3] <- c("condition", "replicate", "concentration")
    # if((length(fl2) > 1 ) || !is.na(data.fl2))    colnames(fl2.mat)[1:3] <- c("condition", "replicate", "concentration")
    if(((length(fl) > 1 ) || !is.na(data.fl)) && length(dat)>1)  colnames(fl.norm.mat)[1:3] <- c("condition", "replicate", "concentration")
    # if(((length(fl2) > 1 ) || !is.na(data.fl2)) && length(dat)>1)  colnames(fl2.norm.mat)[1:3] <- c("condition", "replicate", "concentration")

    if(length(dat) > 1) {
      label <- unlist(lapply(1:nrow(dat.mat), function(x) paste(dat.mat[x,1], dat.mat[x,2], dat.mat[x,3], sep = " | ")))
      condition <- dat.mat[, 1]
      replicate <- dat.mat[, 2]
      concentration <- dat.mat[, 3]
    } else if(length(fl) > 1){
      label <- unlist(lapply(1:nrow(fl.mat), function(x) paste(fl.mat[x,1], fl.mat[x,2], fl.mat[x,3], sep = " | ")))
      condition <- fl.mat[, 1]
      replicate <- fl.mat[, 2]
      concentration <- fl.mat[, 3]
    }
    # else if(length(fl2) > 1){
    #   label <- unlist(lapply(1:nrow(fl2.mat), function(x) paste(fl2.mat[x,1], fl2.mat[x,2], fl2.mat[x,3], sep = " | ")))
    #   condition <- fl2.mat[, 1]
    #   replicate <- fl2.mat[, 2]
    #   concentration <- fl2.mat[, 3]
    # }


    expdesign <- data.frame(label, condition, replicate, concentration, check.names = FALSE)

    if(length(dat)>1)             dat.mat <- as.data.frame(unclass(dat.mat), stringsAsFactors = TRUE)
    if((length(data.fl) > 1 ) || !is.na(data.fl))    fl.mat <- as.data.frame(unclass(fl.mat), stringsAsFactors = TRUE)
    # if((length(data.fl2) > 1 ) || !is.na(data.fl2))    fl2.mat <- as.data.frame(unclass(fl2.mat), stringsAsFactors = TRUE)
    if(((length(data.fl) > 1 ) || !is.na(data.fl)) && length(dat)>1)  fl.norm.mat <- as.data.frame(unclass(fl.norm.mat), stringsAsFactors = TRUE)
    # if(((length(data.fl2) > 1 ) || !is.na(data.fl2)) && length(dat)>1)  fl2.norm.mat <- as.data.frame(unclass(fl2.norm.mat), stringsAsFactors = TRUE)
    #convert values from factor to numeric
    if(length(dat)>1)             dat.mat[, -(1:3)] <- as.numeric(as.matrix(dat.mat[, -(1:3)]))
    if((length(data.fl) > 1 ) || !is.na(data.fl))    fl.mat[, -(1:3)] <- as.numeric(as.matrix(fl.mat[, -(1:3)]))
    # if((length(data.fl2) > 1 ) || !is.na(data.fl2))    fl2.mat[, -(1:3)] <- as.numeric(as.matrix(fl2.mat[, -(1:3)]))
    if(((length(data.fl) > 1 ) || !is.na(data.fl)) && length(dat)>1)  fl.norm.mat[, -(1:3)] <- as.numeric(as.matrix(fl.norm.mat[, -(1:3)]))
    # if(((length(data.fl2) > 1 ) || !is.na(data.fl2)) && length(dat)>1)  fl2.norm.mat[, -(1:3)] <- as.numeric(as.matrix(fl2.norm.mat[, -(1:3)]))

    # if identical time values are present, combine the respective measurement as their mean
    ## create list with unique time values and their frequencies
    t.mat.freq <- lapply(1:nrow(t.mat), function(i) data.frame(table(t.mat[i,])))

    if(any(unlist(t.mat.freq)[grep("Freq", names(unlist(t.mat.freq))) ] > 1)){ #does any time value exist more than once in the same sample?
      ## create list with duplicated time values
      t.mult <- lapply(1:length(t.mat.freq), function(i) t.mat.freq[[i]][t.mat.freq[[i]]$Freq>1, ])
      t.mult <- lapply(1:length(t.mult), function(i) t.mat[i,][t.mat[i,] %in% t.mult[[i]]$Var1 ])
      unique.t.mult <- lapply(1:length(t.mult), function(i) unique(t.mult[[i]]))

      ## combine measurements with duplicated time entries
      t.ls <- list()
      dat.ls <- list()
      f1.ls <- list()
      f2.ls <- list()
      f1.norm.ls <- list()
      f2.norm.ls <- list()

      for(i in 1:length(t.mult)){
        if(length(t.mult[[i]])>1){
          col.nm <- lapply(1:length(unique.t.mult[[i]]), function(j)
            names( t.mult[[i]][which(t.mult[[i]] %in% unique.t.mult[[i]][j]) ] )
          )
          col.ndx <- lapply(1:length(col.nm), function(j) which(colnames(t.mat) %in% col.nm[[j]])
          )
          # add column with average

          ###### MAKE PER ROW AND ALSO IN TIME MATRIX!!!
          combine.ndx <- c(1:col.ndx[[1]][1]) # time values of sample up to first duplicated time
          if(length(col.ndx) == 2){
            if(col.ndx[[1]][length(col.ndx[[1]])] == length(t.mat[i, ])){
              combine.ndx <- c(combine.ndx,
                               (col.ndx[[1]][length(col.ndx[[1]])]+1):col.ndx[[2]][1])
            } else {
              combine.ndx <- c(combine.ndx,
                               (col.ndx[[1]][length(col.ndx[[1]])]+1):col.ndx[[2]][1],
                               (col.ndx[[2]][length(col.ndx[[2]])]+1):length(t.mat[i, ]))
            }
          } else if(length(col.ndx) > 2){
            for(j in 2:(length(col.ndx)-1)){
              combine.ndx <- c(combine.ndx,
                               (col.ndx[[j-1]][length(col.ndx[[j-1]])]+1):col.ndx[[j]][1],
                               (col.ndx[[j]][length(col.ndx[[j]])]+1):col.ndx[[j+1]][1]
              )
            }
            if(!col.ndx[[length(col.ndx)]][length(col.ndx[[length(col.ndx)]])] == length(t.mat[i, ])){
              combine.ndx <- c(combine.ndx,
                               (col.ndx[[length(col.ndx)]][length(col.ndx[[length(col.ndx)]])]+1):length(t.mat[i, ]))
            }
          }

          t.ls[[i]] <- t.mat[i, combine.ndx] # time values of sample up to duplicated time

          combine.ndx.dat <- combine.ndx + 3 # account for three identifier columns
          ndx.average.first <- which(diff(combine.ndx.dat) > 1)+3

          if(length(dat.mat) > 1){
            dat.ls[[i]] <- c(dat.mat[i, 1:3], dat.mat[i, combine.ndx.dat]) # remove entries with duplicated time values except the first ones
            for(j in 1:length(ndx.average.first)){
              dat.ls[[i]][ndx.average.first[j]] <- mean(as.numeric(dat.mat[i, (col.ndx[[j]]+3)]))
            }
          }

          if(length(fl.mat) > 1){
            f1.ls[[i]] <- c(fl.mat[i, 1:3], fl.mat[i, combine.ndx.dat]) # remove entries with duplicated time values except the first ones
            for(j in 1:length(ndx.average.first)){
              f1.ls[[i]][ndx.average.first[j]] <- mean(as.numeric(fl.mat[i, (col.ndx[[j]]+3)]))
            }
          }

          # if(length(fl2.mat) > 1){
          #   f2.ls[[i]] <- c(fl2.mat[i, 1:3], fl2.mat[i, combine.ndx.dat]) # remove entries with duplicated time values except the first ones
          #   for(j in 1:length(ndx.average.first)){
          #     f2.ls[[i]][ndx.average.first[j]] <- mean(as.numeric(fl2.mat[i, (col.ndx[[j]]+3)]))
          #   }
          # }

          if(length(fl.norm.mat) > 1){
            f1.norm.ls[[i]] <- c(fl.norm.mat[i, 1:3], fl.norm.mat[i, combine.ndx.dat]) # remove entries with duplicated time values except the first ones
            for(j in 1:length(ndx.average.first)){
              f1.norm.ls[[i]][ndx.average.first[j]] <- mean(as.numeric(fl.norm.mat[i, (col.ndx[[j]]+3)]))
            }
          }

          # if(length(fl2.norm.mat) > 1){
          #   f2.norm.ls[[i]] <- c(fl2.norm.mat[i, 1:3], fl2.norm.mat[i, combine.ndx.dat]) # remove entries with duplicated time values except the first ones
          #   for(j in 1:length(ndx.average.first)){
          #     f2.norm.ls[[i]][ndx.average.first[j]] <- mean(as.numeric(fl2.norm.mat[i, (col.ndx[[j]]+3)]))
          #   }
          # }
        } # if(length(t.mult[[i]])>1)
      } # for(i in 1:length(t.mult))

      t.mat <- do.call(rbind, t.ls)
      if(length(dat.mat) > 1) dat.mat <- as.data.frame(unclass(do.call(rbind, dat.ls)), stringsAsFactors = TRUE)
      if(length(fl.mat) > 1) fl.mat <- as.data.frame(unclass(do.call(rbind, f1.ls)), stringsAsFactors = TRUE)
      # if(length(fl2.mat) > 1) fl2.mat <- as.data.frame(unclass(do.call(rbind, f2.ls)), stringsAsFactors = TRUE)
      if(length(fl.norm.mat) > 1) fl.norm.mat <- as.data.frame(unclass(do.call(rbind, f1.norm.ls)), stringsAsFactors = TRUE)
      # if(length(fl2.norm.mat) > 1) fl2.norm.mat <- as.data.frame(unclass(do.call(rbind, f2.norm.ls)), stringsAsFactors = TRUE)

      #convert values from factor to numeric
      if(length(dat.mat) > 1)             dat.mat[, -(1:3)] <- as.numeric(as.matrix(dat.mat[, -(1:3)]))
      if(length(fl.mat) > 1)    fl.mat[, -(1:3)] <- as.numeric(as.matrix(fl.mat[, -(1:3)]))
      # if(length(fl2.mat) > 1)    fl2.mat[, -(1:3)] <- as.numeric(as.matrix(fl2.mat[, -(1:3)]))
      if(length(fl.norm.mat) > 1)  fl.norm.mat[, -(1:3)] <- as.numeric(as.matrix(fl.norm.mat[, -(1:3)]))
      # if(length(fl2.norm.mat) > 1)  fl2.norm.mat[, -(1:3)] <- as.numeric(as.matrix(fl2.norm.mat[, -(1:3)]))

    } # if(any(unlist(t.mat.freq)[grep("Freq", names(unlist(t.mat.freq))) ] > 1))

    dataset <- list("time" = t.mat,
                    "growth" = dat.mat,
                    "fluorescence" = fl.mat,
                    # "fluorescence2" = fl2.mat,
                    "norm.fluorescence" = fl.norm.mat,
                    # "norm.fluorescence2" = fl2.norm.mat,
                    "expdesign" = expdesign)

    class(dataset) <- "grodata"
    invisible(dataset)
  }

#' Convert a tidy data frame to a custom QurvE format
#'
#' This function converts a data frame in "tidy" format into the custom format used by QurvE (row format).
#' The provided "tidy" data has columns for "Description", "Concentration", "Replicate", and "Values", with one
#' row per time point and sample. Alternatively, the function converts data in custom QurvE column format into
#' row format (if \code{data.format = "col"}).
#'
#' @param df A data frame in tidy format, containing "Time", "Description", and either "Values" or "Value" columns. Optionally, meta information provided in columns "Replicate" and "Concentration" is used.
#' @param data.format (Character string) \code{"col"} (the default) or \code{"row"}. Only relevant if data is not provided in "tidy" format but has been prepared into the custom QurvE data format.
#'
#' @return A data frame in the custom format (row format) used by QurvE.
#'
#' @examples
#' # Create a tidy data frame with two samples, five concentrations, three
#' # replicates, and five time points
#' samples <- c("Sample 1", "Sample 2")
#' concentrations <- c(0.1, 0.5, 1, 2, 5)
#' time_points <- c(1, 2, 3, 4, 5)
#' n_replicates <- 3
#'
#' df <- expand.grid(
#'    Description = c("Sample 1", "Sample 2"),
#'    Concentration = c(0.1, 0.5, 1, 2, 5),
#'    Time = c(1, 2, 3, 4, 5),
#'    Replicate = 1:3)
#'
#' df$Value <- abs(rnorm(nrow(df)))
#'
#' df_formatted <- tidy_to_custom(df)
#'
#' @keywords internal
#' @export
tidy_to_custom <- function(df, data.format = "col"){
  # Check if data is in "tidy" format
  # Check if data contains the required headers in colnames or the first row
  if((any(grepl("Time", colnames(df), ignore.case = T)) &&
      any(grepl("Description", colnames(df), ignore.case = T)) &&
      any(grepl("Values|Value", colnames(df), ignore.case = T))) ||
     (any(grepl("Time", df[1,], ignore.case = T)) &&
      any(grepl("Description", df[1,], ignore.case = T)) &&
      any(grepl("Values|Value", df[1,], ignore.case = T)))
  ){
    tidy <- TRUE
    # If identifiers in first row, convert to colnames
    if(any(grepl("Time", df[1,], ignore.case = T)) ||
       any(grepl("Description", df[1,], ignore.case = T)) ||
       any(grepl("Values|Value", df[1,], ignore.case = T))){
      colnames(df) <- df[1, ]
      df <- df[-1, ]
    }
    colnames(df)[grep("Value", colnames(df))] <- "Values"
    # If missing, add columns for "Replicate" and "Concentration"
    if(!any(grepl("Replicate", colnames(df), ignore.case = T))){
      df[, "Replicate"] <- rep(NA, nrow(df))
    }
    if(!any(grepl("Concentration", colnames(df), ignore.case = T))){
      df[, "Concentration"] <- rep(NA, nrow(df))
    }


    # Convert tidy format to the custom QurvE format

    # Create a unique identifier for each combination of Description, Concentration, and Replicate
    df[["Group"]] <- paste(df$Description, df$Concentration, df$Replicate, sep = "_")

    # Split the 'Time' column based on the unique identifier
    time_split <- split(df[["Time"]], df[["Group"]])

    # Create a list of subsets of 'df' based on the unique identifiers in 'time_split'
    subsets_list <- lapply(unique(df[["Group"]]), function(x) {
      subset(df, df[["Group"]] == x)
    })

    # Helper function to check if two data frames have identical 'Time' values
    identical_time <- function(df1, df2) {
      identical(df1$Time, df2$Time)
    }

    # Initialize an empty list to store combined data frames
    combined_list <- list()

    # Initialize a vector to track combined data frames
    combined_flag <- rep(FALSE, length(subsets_list))

    for (i in seq_along(subsets_list)) {
      # Skip if the data frame is already combined
      if (combined_flag[i]) {
        next
      }

      df1 <- subsets_list[[i]]
      matching_indices <- c(i)

      # Check for matching 'Time' values in other data frames
      for (j in (i+1):length(subsets_list)) {
        if (!combined_flag[j] && identical_time(df1, subsets_list[[j]])) {
          matching_indices <- c(matching_indices, j)
          combined_flag[j] <- TRUE
        }
      }

      # Combine the matching data frames
      combined_df <- do.call(rbind, subsets_list[matching_indices])

      # Add the combined data frame to the combined_list
      combined_list[[length(combined_list) + 1]] <- combined_df

      # Mark the current data frame as combined
      combined_flag[i] <- TRUE
    }

    # Define helper function to convert into wide format
    convert_to_wide <- function(df) {
      df <- df[!is.na(df$Time), ]
      # Find unique groups
      unique_groups <- unique(df[["Group"]])

      # Create an empty data frame with the required structure
      df_wide <- data.frame(matrix(ncol = length(unique_groups) + 1, nrow = nrow(df)/length(unique_groups) + 2))
      colnames(df_wide) <- c("Time", unique_groups)

      # Add Time, Replicate, and Concentration values
      time <- unique(df$Time)
      time <- time[!is.na(time)]
      df_wide$Time <- c(NA, NA, time)

      for (group in unique_groups) {
        group_df <- df[df[["Group"]] == group, ]
        description <- as.character(unique(group_df$Description))
        replicate <- as.integer(unique(group_df$Replicate))
        concentration <- unique(group_df$Concentration)

        # Add Description, Replicate, Concentration, and Values
        df_wide[[group]] <- c(replicate, concentration, group_df$Values)
        colnames(df_wide)[colnames(df_wide) == group] <- description
      }

      return(df_wide)
    }


    wide_list <- lapply(combined_list, convert_to_wide)

    # Combine dataframes into a single dataframe in QurvE custom format
    # Find the maximum number of rows among the data frames
    max_rows <- max(sapply(wide_list, nrow))

    # Add NA rows to each data frame to make them equal in length
    equalize_rows <- function(df, max_rows) {
      if (nrow(df) < max_rows) {
        missing_rows <- max_rows - nrow(df)
        na_rows <- data.frame(matrix(NA, ncol = ncol(df), nrow = missing_rows))
        colnames(na_rows) <- colnames(df)
        df <- rbind(df, na_rows)
      }
      return(df)
    }

    wide_list_equal_rows <- lapply(wide_list, equalize_rows, max_rows = max_rows)

    # Combine data frames into a single data frame
    # Custom function to merge two data frames with different column names
    merge_data_frames <- function(df1, df2) {
      common_rows <- min(nrow(df1), nrow(df2))
      missing_rows_df1 <- nrow(df2) - nrow(df1)
      missing_rows_df2 <- nrow(df1) - nrow(df2)

      # Add NA rows to the shorter data frame
      if (missing_rows_df1 > 0) {
        na_rows_df1 <- data.frame(matrix(NA, ncol = ncol(df1), nrow = missing_rows_df1))
        colnames(na_rows_df1) <- colnames(df1)
        df1 <- rbind(df1, na_rows_df1)
      } else if (missing_rows_df2 > 0) {
        na_rows_df2 <- data.frame(matrix(NA, ncol = ncol(df2), nrow = missing_rows_df2))
        colnames(na_rows_df2) <- colnames(df2)
        df2 <- rbind(df2, na_rows_df2)
      }

      # Combine the data frames
      combined_df <- cbind(df1, df2)

      return(combined_df)
    }

    combined_wide_df <- Reduce(merge_data_frames, wide_list_equal_rows)

    # Create a new data frame with the column headers as its first row
    header_df <- data.frame(matrix(colnames(combined_wide_df), ncol = ncol(combined_wide_df), nrow = 1))
    colnames(header_df) <- colnames(combined_wide_df)

    # Bind the original data frame below the header data frame
    df <- rbind(header_df, combined_wide_df)

    df <- t(df)
  }
  else if(data.format == "col"){
    df <- t(df)
  }
  return(df)
}

#'  Parse raw plate reader data and convert it to a format compatible with QurvE
#'
#' \code{parse_data} takes a raw export file from a plate reader experiment (or similar device), extracts relevant information and parses it into the format required to run \code{\link{growth.workflow}}. If more than one read type is found the user is prompted to assign the correct reads to \code{growth} or \code{fluorescence}.
#'
#' @param data.file (Character) A table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing raw plate reader (or similar device) data.
#' @param map.file (Character) A table file in column format with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt'  with 'well', 'ID', 'replicate', and 'concentration' in the first row. Used to assign sample information to wells in a plate.
#' @param software (Character) The name of the software/device used to export the plate reader data.
#' @param convert.time (\code{NULL} or string) Convert time values with a formula provided in the form \code{'y = function(x)'}.
#' For example: \code{convert.time = 'y = 24 * x'}
#' @param sheet.data,sheet.map (Numeric or Character) Number or name of the sheets in XLS or XLSX files containing experimental data or mapping information, respectively (_optional_).
#' @param csvsep.data,csvsep.map (Character) separator used in CSV data files (ignored for other file types).  Default: \code{";"}
#' @param dec.data,dec.map (Character) decimal separator used in CSV, TSV or TXT files with measurements and mapping information, respectively.
#' @param subtract.blank (Logical) Shall blank values be subtracted from values within the same experiment ([TRUE], the default) or not ([FALSE]).
#' @param calib.growth,calib.fl,calib.fl2 (Character or \code{NULL}) Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert growth and fluorescence values. This can be used to, e.g., convert plate reader absorbance values into \ifelse{html}{\out{OD<sub>600</sub>}}{\eqn{OD_{600}}} or fluorescence intensity into molecule concentrations.
#' Caution!: When utilizing calibration, carefully consider whether or not blanks were subtracted to determine the calibration before selecting the input \code{subtract.blank = TRUE}.
#' @param fl.normtype (Character string) Normalize fluorescence values by either diving by \code{'growth'} or by fluorescence2 values (\code{'fl2'}).
#'
#' @return A \code{grodata} object suitable to run \code{\link{growth.workflow}}. See \code{\link{read_data}} for its structure.
#'
#' @details Metadata provided as \code{map.file} needs to have the following layout:
#' \figure{mapping-layout.png}
#'
#' @export
#'
#' @examples
#' if(interactive()){
#' grodata <- parse_data(data.file = system.file("fluorescence_test_Gen5.xlsx", package = "QurvE"),
#'                       sheet.data = 1,
#'                       map.file = system.file("fluorescence_test_Gen5.xlsx", package = "QurvE"),
#'                       sheet.map = "mapping",
#'                       software = "Gen5",
#'                       convert.time = "y = x * 24", # convert days to hours
#'                       calib.growth = "y = x * 3.058") # convert absorbance to OD values
#' }
parse_data <-
  function(data.file = NULL,
           map.file = NULL,
           software = c("Gen5", "Gen6", "Biolector", "Chi.Bio", "GrowthProfiler", "Tecan", "VictorNivo", "VictorX3"),
           convert.time = NULL,
           sheet.data = 1,
           sheet.map = 1,
           csvsep.data = ";",
           dec.data = ".",
           csvsep.map = ";",
           dec.map = ".",
           subtract.blank  = TRUE,
           calib.growth = NULL,
           calib.fl = NULL,
           calib.fl2 = NULL,
           fl.normtype = c("growth", "fl2")
  ) {
    software <- match_arg(software)
    if(is.null(data.file)) stop("Please provide the name or path to a table file containing plate reader data in the 'data.file' argument.")
    if(is.null(map.file)) warning("No mapping file was provided. The samples will be identified based on their well position (A1, A2, A3, etc.). Grouping options will not be available if you run any further analysis with QurvE.")
    if(!any(grep("Gen5|Gen6|Biolector|Chi\\.Bio|GrowthProfiler|Tecan|VictorNivo|VictorX3", software, ignore.case=T))) stop("The plate reader control software you provided as 'software' is currently not supported by parse_data(). Supported options are:\n 'Gen5', 'Gen6', 'Chi.Bio', and 'GrowthProfiler'.")
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
    }
    if(any(grep("Gen5|Gen6", software, ignore.case = TRUE))){
      parsed.ls <- parse_Gen5Gen6(input)
      data.ls <- parsed.ls[[1]]
    } # if("Gen5" %in% software)
    if(any(grep("Chi.Bio", software, ignore.case = TRUE))){
      parsed.ls <- parse_chibio(input)
      data.ls <- parsed.ls[[1]]
    }
    if(any(grep("GrowthProfiler", software, ignore.case = TRUE))){
      parsed.ls <- parse_growthprofiler(input)
      data.ls <- parsed.ls[[1]]
    }
    if(any(grep("Tecan", software, ignore.case = TRUE))){
      parsed.ls <- parse_tecan(input)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("Biolector", software, ignore.case = TRUE))){
      parsed.ls <- parse_biolector(input)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("VictorNivo", software, ignore.case = TRUE))){
      parsed.ls <- parse_victornivo(input)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("VictorX3", software, ignore.case = TRUE))){
      parsed.ls <- parse_victorx3(input)
      data.ls <- parsed.ls[[1]]
    }

    # Convert time values
    if(!is.null(convert.time)){

      conversion <- parse(text = convert.time)

      for(i in 1:length(data.ls[!is.na(data.ls)])){
        x <- as.numeric(data.ls[[i]][2:nrow(data.ls[[1]]),1])
        time_converted <- eval(conversion)
        data.ls[[i]][2:nrow(data.ls[[1]]),1] <- time_converted
      }
    }

    noNA.ndx <- which(!is.na(data.ls))

    if(any(c("Gen5", "Gen6") %in% software)){
      # Remove any columns between time and 'A1' (for plate readers)
      A1.ndx <- match("A1", data.ls[[1]][1,])
      if(A1.ndx>2){
        data.ls <- lapply(noNA.ndx, function(x) data.ls[[x]][ ,c(1,A1.ndx:ncol(data.ls[[x]]))])
      }
    } else {
      data.ls <-data.ls[noNA.ndx]
    }
    # apply identifiers specified in mapping file
    for(i in noNA.ndx){
      if(!is.null(map.file)){
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
    if(length(data.ls)==1) {
      names(data.ls) <- "growth"
      grodata <-
        read_data(
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
          fl.normtype = fl.normtype,
          calib.fl = calib.fl,
          calib.fl2 = calib.fl2
        )
    }
    invisible(grodata
    )
  }

#' Call the appropriate function required to read a table file and return the table as a dataframe object.
#'
#' \code{read_file} automatically detects the format of a file provided as \code{filename} and calls the appropriate function to read the table file.
#'
#' @param filename (Character) Name or path of the table file to read. Can be of type CSV, XLS, XLSX, TSV, or TXT.
#' @param csvsep (Character) separator used in CSV file (ignored for other file types).
#' @param dec (Character) decimal separator used in CSV, TSV and TXT files.
#' @param sheet (Numeric or Character) Number or name of a sheet in XLS or XLSX files (_optional_). Default: \code{";"}
#'
#' @return A dataframe object with headers in the first row.
#' @export
#'
#' @examples
#' input <- read_file(filename = system.file("2-FMA_toxicity.csv", package = "QurvE"), csvsep = ";" )
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
          header = FALSE,
          stringsAsFactors = FALSE,
          fill = TRUE,
          na.strings = "",
          quote = '',
          comment.char = "",
          check.names = FALSE,
          col.names = paste0("V", seq_len(ncols))
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "xls" |
               stringr::str_replace(filename, ".{1,}\\.", "") == "xlsx") {
      dat <- data.frame(suppressMessages(readxl::read_excel(filename, col_names = FALSE, sheet = sheet, progress = TRUE)))
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "tsv") {
      ncols <- max(utils::count.fields(filename))
      dat <-
        utils::read.csv(
          filename,
          dec = dec,
          blank.lines.skip = FALSE,
          sep = "\t",
          header = FALSE,
          stringsAsFactors = FALSE,
          fill = TRUE,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = FALSE,
          col.names = paste0("V", seq_len(ncols))
        )
    } else if (stringr::str_replace_all(filename, ".{1,}\\.", "") == "txt") {
      ncols <- max(utils::count.fields(filename))
      dat <-
        utils::read.table(
          filename,
          dec = dec,
          sep = "\t",
          header = FALSE,
          stringsAsFactors = FALSE,
          fill = TRUE,
          na.strings = "",
          quote = "",
          comment.char = "",
          check.names = FALSE,
          col.names = paste0("V", seq_len(ncols))
        )
    } else {
      stop(
        "No compatible file format provided.
             Supported formats are: \\.txt (tab delimited), \\.csv (delimiters can be specified with the argument \"csvsep = \", \\.tsv, \\.xls, and \\.xlsx"
      )
    }
  } else {
    stop(paste0("File \"", filename, "\" does not exist."), call. = FALSE)
  }
  return(dat)
}

#' Extract relevant data from a raw data export file generated with the "Gen5" or "Gen6" software.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @export
#'
#' @examples
#' if(interactive()){
#' input <- read_file(filename = system.file("fluorescence_test_Gen5.xlsx", package = "QurvE") )
#' parsed <- parse_Gen5Gen6(input)
#' }
parse_Gen5Gen6 <- function(input)
{
  # get row numbers for "time" in column 2
  time.ndx <- grep("\\btime\\b", input[[2]], ignore.case = TRUE)
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
    read.data[[length(read.ndx)]] <- data.frame(input[read.ndx[length(read.ndx)]:(read.ndx[length(read.ndx)]+length(read.data[[1]][[1]])),2:(ncol)])
    #read.data[[length(read.ndx)]] <- as.data.frame(read.data[[length(read.ndx)]])[1:length(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0][!is.na(read.data[[length(read.ndx)]][,1][read.data[[length(read.ndx)]][,1]!=0])]),]
    for( i in 1:length(read.data) ){
      if(any(is.na(suppressWarnings(as.numeric(read.data[[i]][-1,2]))))){
        read.data[[i]] <- suppressWarnings(read.data[[i]][1:which(is.na(as.numeric(read.data[[i]][-1,2])))[1], ])
      }
    }
  } else {
    if(!any(is.na(input[read.ndx:nrow(input),3]))){
      read.data[[1]] <- data.frame(input[read.ndx:nrow(input),2:(1+ncol)])
    }
    else {
      read.data[[1]] <- data.frame(input[read.ndx:(read.ndx + match(NA, input[read.ndx:nrow(input),3])-2),2:(1+ncol)])
    }
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

  data.ls <- list(NA, NA, NA)

  for(i in 1:length(reads)){
    if(length(reads) == 1){
      answer <- readline(paste0(
        "Assign data type for read: ",
        reads[i],
        "?\n",
        paste("[1] Growth",
              "[2] Fluorescence",
              sep = "\n")
      ))

     if(as.numeric(answer) == 1)
       data.ls[[1]] <-  read.data[[1]]
     if(as.numeric(answer) == 2)
       data.ls[[2]] <-  read.data[[1]]
    }
    if(length(reads) > 1){
      answer <- readline(paste0(
        "Assign data type for read: ",
        reads[i],
        "?\n",
        paste("[1] Growth",
              "[2] Fluorescence",
              "[3] Fluorescence 2 (to normalize Fluorescence)",
              "[4] Ignore",
              sep = "\n")
      ))

      if(as.numeric(answer) < 4){
        data.ls[[as.numeric(answer)]] <-  read.data[[i]]

        # replace "OVRFLW" with NA in fluorescence data
        if(as.numeric(answer) %in% c(2,3))
          data.ls[[as.numeric(answer)]][which(data.ls[[as.numeric(answer)]] == "OVRFLW", arr.ind = TRUE)] <- NA
      }
    }
  }

  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of "Chi.Bio" bioreactors.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @keywords internal
#' @noRd
parse_chibio <- function(input)
{
  time.ndx <- grep("time", input[1,], ignore.case = TRUE)
  read.ndx <- grep("measured|emit", input[1,], ignore.case = TRUE)
  reads <- input[1, read.ndx]

  data.ls <- list(NA, NA, NA)
  assigned <- c()


    for(i in 1:length(reads)){
      if(length(reads) == 1){
        answer <- readline(paste0(
          "Assign data type for read: ",
          reads[i],
          "?\n",
          paste("[1] Growth",
                "[2] Fluorescence",
                sep = "\n")
        ))

        if(as.numeric(answer) == 1){
          data.ls[[1]] <-  data.frame("time" = input[, time.ndx], "growth" = c(input[1, read.ndx[as.numeric(answer)]], as.numeric(input[-1, read.ndx[as.numeric(answer)]])))
          if(all(as.numeric(data.ls[[1]][-1,2]) == 0) | all(is.na(data.ls[[1]][-1,2]))){
            data.ls[[1]] <- NA
          }
        }

        if(as.numeric(answer) == 2){
          data.ls[[2]] <-  data.frame("time" = input[, time.ndx], "fluorescence" = c(input[1, read.ndx[as.numeric(answer)]], as.numeric(input[-1, read.ndx[as.numeric(answer)]])))
          data.ls[[2]][which(data.ls[[2]] == "OVRFLW", arr.ind = TRUE)] <- NA
          if(all(as.numeric(data.ls[[2]][-1,2]) == 0) || all(is.na(data.ls[[2]][-1,2]))){
            fluorescence <- NA
          }
        }
      } # if(length(reads) == 1)
      if(length(reads) > 1){
        answer <- readline(paste0(
          "Assign data type for read: ",
          reads[i],
          "?\n",
          paste("[1] Growth",
                "[2] Fluorescence",
                "[3] Fluorescence 2 (to normalize Fluorescence)",
                "[4] Ignore",
                sep = "\n")
        ))

        if(as.numeric(answer) < 4)
          assigned <- c(assigned, TRUE)

        if(as.numeric(answer) == 1){
          data.ls[[1]] <-  data.frame("time" = input[, time.ndx], "growth" = c(input[1, read.ndx[as.numeric(answer)]], as.numeric(input[-1, read.ndx[as.numeric(answer)]])))
        }
        if(as.numeric(answer) == 2){
          data.ls[[2]] <-  data.frame("time" = input[, time.ndx], "fluorescence" = c(input[1, read.ndx[as.numeric(answer)]], as.numeric(input[-1, read.ndx[as.numeric(answer)]])))
          data.ls[[as.numeric(answer)]][which(data.ls[[as.numeric(answer)]] == "OVRFLW", arr.ind = TRUE)] <- NA
        }
        if(as.numeric(answer) == 3){
          data.ls[[3]] <-  data.frame("time" = input[, time.ndx], "fluorescence" = c(input[1, read.ndx[as.numeric(answer)]], as.numeric(input[-1, read.ndx[as.numeric(answer)]])))
          data.ls[[as.numeric(answer)]][which(data.ls[[as.numeric(answer)]] == "OVRFLW", arr.ind = TRUE)] <- NA
        }
        if(length(assigned) == 3)
          break
      } #if(length(reads) > 1)
  }

  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of the "Growth Profiler".
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing a growth dataframe in the first element and \code{NA} in the second. The first column in the dataframe represents a time vector.
#'
#' @keywords internal
#' @noRd
parse_growthprofiler <- function(input)
{
  # get row numbers for "time" in column 1
  time.ndx <- grep("^\\btime\\b", input[[1]], ignore.case = TRUE)
  # get index of empty column at the and of the data series
  na.ndx <- which(is.na(input[time.ndx,]))

  growth <- input[time.ndx:nrow(input), 1:(na.ndx-1)]

  data.ls <- list()
  data.ls[[1]] <- growth
  data.ls[[2]] <- NA
  data.ls[[3]] <- NA

  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of "Tecan" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @keywords internal
#' @noRd
parse_tecan <- function(input)
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

  data.ls <- list(NA, NA, NA)

  for(i in 1:length(reads)){
    if(length(reads) == 1){
      answer <- readline(paste0(
        "Assign data type for read: ",
        reads[i],
        "?\n",
        paste("[1] Growth",
              "[2] Fluorescence",
              sep = "\n")
      ))

      if(as.numeric(answer) == 1)
        data.ls[[1]] <-  read.data[[1]]
      if(as.numeric(answer) == 2)
        data.ls[[2]] <-  read.data[[1]]
    }
    if(length(reads) > 1){
      answer <- readline(paste0(
        "Assign data type for read: ",
        reads[i],
        "?\n",
        paste("[1] Growth",
              "[2] Fluorescence",
              "[3] Fluorescence 2 (to normalize Fluorescence)",
              "[4] Ignore",
              sep = "\n")
      ))

      if(as.numeric(answer) < 4){
        data.ls[[as.numeric(answer)]] <-  read.data[[i]]

        # replace "OVRFLW" with NA in fluorescence data
        if(as.numeric(answer) %in% c(2,3))
          data.ls[[as.numeric(answer)]][which(data.ls[[as.numeric(answer)]] == "OVER", arr.ind = TRUE)] <- NA
      }
    }
  }
  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of "Biolector" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing a growth dataframe in the first element and \code{NA} in the second. The first column in the dataframe represents a time vector.
#'
#' @keywords internal
#' @noRd
parse_biolector <- function(input)
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

    answer <- readline(paste0("Indicate where the growth data is stored?\n",
                              paste(unlist(lapply(1:length(reads), function (i)
                                paste0("[", i, "] ", reads[i]))),
                                collapse = "\n"), "\n[", length(reads)+1, "] Disregard growth data\n"))
    if(as.numeric(answer) == length(reads)+1){
      growth <- NA
    } else {
      growth <- read.data[[as.numeric(answer)]]
    }

    data.ls[[1]] <- growth
    data.ls[[2]] <- NA
    data.ls[[3]] <- NA

  } else {
    growth <- read.data[[1]]
    data.ls[[1]] <- growth
    data.ls[[2]] <- NA
    data.ls[[3]] <- NA
  }
  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of Perkin Elmer's "Victor Nivo" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @export
#'
#' @examples
#' if(interactive()){
#' input <- read_file(filename = system.file("nivo_output.csv", package = "QurvE"), csvsep = "," )
#' parsed <- parse_victornivo(input)
#' }
parse_victornivo <- function(input)
{
  # get index (row,column) for "Time:"
  time.ndx <- grep("^\\bTime\\b", input[,2], ignore.case = TRUE)
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

  data.ls <- list(NA, NA, NA)

  for(i in 1:length(reads)){
    if(length(reads) == 1){
      answer <- readline(paste0(
        "Assign data type for read: ",
        reads[i],
        "?\n",
        paste("[1] Growth",
              "[2] Fluorescence",
              sep = "\n")
      ))

      if(as.numeric(answer) == 1)
        data.ls[[1]] <-  read.data[[1]]
      if(as.numeric(answer) == 2)
        data.ls[[2]] <-  read.data[[1]]
    }
    if(length(reads) > 1){
      answer <- readline(paste0(
        "Assign data type for read: ",
        reads[i],
        "?\n",
        paste("[1] Growth",
              "[2] Fluorescence",
              "[3] Fluorescence 2 (to normalize Fluorescence)",
              "[4] Ignore",
              sep = "\n")
      ))

      if(as.numeric(answer) < 4){
        data.ls[[as.numeric(answer)]] <-  read.data[[i]]

        # replace "OVRFLW" with NA in fluorescence data
        if(as.numeric(answer) %in% c(2,3))
          data.ls[[as.numeric(answer)]][which(data.ls[[as.numeric(answer)]] == "OVRFLW", arr.ind = TRUE)] <- NA
      }
    }
  }

  invisible(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of Perkin Elmer's "Victor X3" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing growth and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @export
#'
#' @examples
#' if(interactive()){
#' input <- read_file(filename = system.file("victorx3_output.txt", package = "QurvE") )
#' parsed <- parse_victorx3(input)
#' }
parse_victorx3 <- function(input)
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

  data.ls <- list(NA, NA, NA)

  for(i in 1:length(reads)){
    if(length(reads) == 1){
      answer <- readline(paste0(
        "Assign data type for read: ",
        reads[i],
        "?\n",
        paste("[1] Growth",
              "[2] Fluorescence",
              sep = "\n")
      ))

      if(as.numeric(answer) == 1)
        data.ls[[1]] <-  read.data[[1]]
      if(as.numeric(answer) == 2)
        data.ls[[2]] <-  read.data[[1]]
    }
    if(length(reads) > 1){
      answer <- readline(paste0(
        "Assign data type for read: ",
        reads[i],
        "?\n",
        paste("[1] Growth",
              "[2] Fluorescence",
              "[3] Fluorescence 2 (to normalize Fluorescence)",
              "[4] Ignore",
              sep = "\n")
      ))

      if(as.numeric(answer) < 4){
        data.ls[[as.numeric(answer)]] <-  read.data[[i]]

        # replace "OVRFLW" with NA in fluorescence data
        if(as.numeric(answer) %in% c(2,3))
          data.ls[[as.numeric(answer)]][which(data.ls[[as.numeric(answer)]] == "OVRFLW", arr.ind = TRUE)] <- NA
      }
    }
  }

  invisible(list(data.ls))
}
