#' Read growth and fluorescence data in table format
#'
#' \code{read_data} reads table files or R dataframe objects containing growth and fluorescence data and extracts datasets, sample and group information, performs blank correction, applies data transformation (calibration), and combines technical replicates.
#'
#' @param data.density An R dataframe object or a table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing density data.
#' In column format, the first three table rows contain
#' \enumerate{
#'    \item sample description
#'    \item replicate number (_optional_: followed by a letter to indicate technical replicates)
#'    \item concentration value (_optional_)
#' }
#' @param data.fl (optional) An R dataframe object or a table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing fluorescence data. Table layout must mimic that of \code{data.density}.
#' @param data.fl2 (optional) An R dataframe object or a table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing fluorescence2 data (used only to normalize \code{fluorescence} data). Table layout must mimic that of \code{data.density}.
#' @param data.format (Character) "col" for samples in columns, or "row" for samples in rows. Default: \code{"col"}
#' @param csvsep (Character) separator used in CSV file storing density data (ignored for other file types). Default: \code{";"}
#' @param dec (Character) decimal separator used in CSV, TSV or TXT file storing density data. Default: \code{"."}
#' @param csvsep.fl (Character) separator used in CSV file storing fluorescence data (ignored for other file types). Default: \code{";"}
#' @param dec.fl (Character) decimal separator used in CSV, TSV or TXT file storing fluorescence2 data. Default: \code{"."}
#' @param csvsep.fl2 (Character) separator used in CSV file storing fluorescence data (ignored for other file types). Default: \code{";"}
#' @param dec.fl2 (Character) decimal separator used in CSV, TSV or TXT file storing fluorescence2 data. Default: \code{"."}
#' @param subtract.blank (Logical) Shall blank values be subtracted from values within the same experiment ([TRUE], the default) or not ([FALSE]). If \code{calibration = TRUE}, blanks are subtracted after value conversion.
#' @param sheet.density (Numeric or Character) Number or name of the sheet with density data in XLS or XLSX files (_optional_).
#' @param sheet.fl (Numeric or Character) Number or name of the sheet with fluorescence data in XLS or XLSX files (_optional_).
#' @param sheet.fl2 (Numeric or Character) Number or name of the sheet with fluorescence2 data (used for normalization of fluorescence data) in XLS or XLSX files (_optional_).
#' @param fl.normtype (Character string) Normalize fluorescence values by either diving by \code{'density'} or by fluorescence2 values (\code{'fl2'}).
#' @param convert.time (\code{NULL} or string) Convert time values with a formula provided in the form \code{'y = function(x)'}.
#' For example: \code{convert.time = 'y = 24 * x'}
#' @param calibration (Character or \code{NULL}) Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert density and fluorescence values. This can be used to, e.g., convert plate reader absorbance values into \ifelse{html}{\out{OD<sub>600</sub>}}{\eqn{OD_{600}}}.
#' Caution!: When utilizing calibration, carefully consider whether or not blanks were subtracted to determine the calibration before selecting the input \code{subtract.blank = TRUE}.
#'
#' @details
#' \figure{Data_layout.jpg}
#'
#' @return An R list object of class \code{grodata} containing a \code{time} matrix, dataframes with density and fluorescence data (if applicable),
#' and an experimental design table. The \code{grodata} object can be directly
#'   used to run \code{\link{growth.workflow}}/\code{\link{fl.workflow}} or, together with a \code{growth.control}/\code{fl.control}
#'   object, in \code{\link{growth.gcFit}}/\code{\link{flFit}}.
#' \item{time}{Matrix with raw time values extracted from \code{data.density}.}
#' \item{density}{Dataframe with raw density values and sample identifiers extracted from \code{data.density}.}
#' \item{fluorescence}{Dataframe with raw fluorescence values and sample identifiers extracted from \code{data.fl}. \code{NA}, if no fluorescence data is provided.}
#' \item{norm.fluorescence}{fluorescence data divided by density values. \code{NA}, if no fluorescence data is provided.}
#' \item{expdesign}{Experimental design table created from the first three identifier rows/columns (see argument \code{data.format}) (\code{data.density}.}
#'
#' @export
#' @importFrom methods is
#' @import dplyr stringr tidyr
#' @md
#' @examples
#' # Load CSV file containing only growth data
#' data_growth <- read_data(data.density = system.file("2-FMA_toxicity.csv",
#'                          package = "QurvE"), csvsep = ";" )
#'
#' # Load XLSX file containing both density and fluorescence data
#' data_growth_fl <- read_data(data.density = system.file("lac_promoters.xlsx", package = "QurvE"),
#'                             sheet.density = "OD",
#'                             data.fl = system.file("lac_promoters.xlsx", package = "QurvE"),
#'                             sheet.fl = 2)
read_data <-
  function(data.density = NA,
           data.fl = NA,
           data.fl2 = NA,
           data.format = "col",
           csvsep = ";",
           dec = ".",
           csvsep.fl = ";",
           dec.fl = ".",
           csvsep.fl2 = ";",
           dec.fl2 = ".",
           sheet.density = 1,
           sheet.fl = 1,
           sheet.fl2 = 1,
           fl.normtype = c("density", "fl2"),
           subtract.blank  = T,
           convert.time = NULL,
           calibration = NULL)
  {
    if(is.null(data.density)) data.density <- NA
    if(is.null(data.fl)) data.fl <- NA
    if(is.null(data.fl2)) data.fl2 <- NA
    if(is.null(data.fl2)) data.fl2 <- NA
    if(!is.null(calibration) && calibration == "") calibration <- NULL

    fl.normtype <- match.arg(fl.normtype)

    # Load density data
    if (any(is(data.density) %in% c("matrix", "list", "array")) || !is.character(data.density)) {
      dat <- data.density
    } else {
      # Read table file
      dat <- read_file(data.density, csvsep=csvsep, dec=dec, sheet=sheet.density)
    }
    if(data.format == "col"){
      dat <- t(dat)
    }
    # Convert time values
    if(!is.null(convert.time)){

      conversion <- parse(text = convert.time)

      x <- as.numeric(dat[1, -(1:3)])
      time_converted <- eval(conversion)
      dat[1, -(1:3)] <- time_converted
    }
    # Remove all-NA data series
    allNA.ndx <- which(unlist(lapply(1:nrow(dat), function(x) all(is.na(dat[x, -(1:3)])))))
    if(length(allNA.ndx) > 0)
      dat <- dat[-allNA.ndx, ]

    #remove leading and trailing zeros
    if(length(dat)>0)
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
      if(data.format == "col"){
        fl <- t(fl)
      }
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
      if(data.format == "col"){
        fl2 <- t(fl2)
      }
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
        stop("Could not find 'time' in column 1 of any provided 'data.density' or 'data.fl'.")
      } else {
        stop("Could not find 'time' in row 1 of any provided 'data.density' or 'data.fl'.")
      }
    }

    if(!is.null(calibration)){
      calibrate <- function(df){
        #test if more than one time entity is present
        time.ndx <- grep("time", unlist(df[,1]), ignore.case = TRUE)
        calib <- parse(text = calibration)
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
      if(length(dat)>1)             dat <- calibrate(df = dat)
      if((length(fl) > 1 ) || !is.na(data.fl))    fl <- calibrate(df=fl)
      if((length(fl2) > 1 ) || !is.na(data.fl2))    fl2 <- calibrate(df=fl2)

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
        tech.rep <- suppressWarnings(as.numeric(unique(gsub("[[:alpha:]]___.+", "", gsub(".+\\.\\.\\.", "", sample_names[ndx.cond])))))
        tech.rep <- tech.rep[!is.na(tech.rep)]
        if(length(tech.rep)>1){
          for(j in 1:length(tech.rep)){
            ndx.rep <- ndx.cond[which(gsub("[[:alpha:]]___.+", "", gsub(".+\\.\\.\\.", "", sample_names[ndx.cond])) %in% tech.rep[j])]
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
        else{
          df[1+ndx.cond, 2] <- gsub("[[:alpha:]]", "", df[1+ndx.cond, 2])
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
    if(fl.normtype == "density"){
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

    # Create data matrices for density and fluorescence values
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
    }
    else{
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
                    "density" = dat.mat,
                    "fluorescence" = fl.mat,
                    # "fluorescence2" = fl2.mat,
                    "norm.fluorescence" = fl.norm.mat,
                    # "norm.fluorescence2" = fl2.norm.mat,
                    "expdesign" = expdesign)

    class(dataset) <- "grodata"
    invisible(dataset)
  }

#'  (Experimental) Parse raw plate reader data and convert it to a format compatible with QurvE
#'
#' \code{parse_data} takes a raw export file from a plate reader experiment (or similar device), extracts relevant information and parses it into the format required to run \code{\link{growth.workflow}}. If more than one read type is found the user is prompted to assign the correct reads to \code{density} or \code{fluorescence}.
#'
#' @param data.file (Character) A table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing raw plate reader (or similar device) data.
#' @param map.file (Character) A table file in column format with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt'  with 'well', 'ID', 'replicate', and 'concentration' in the first row. Used to assign sample information to wells in a plate.
#' @param software (Character) The name of the software/device used to export the plate reader data.
#' @param convert.time (\code{NULL} or string) Convert time values with a formula provided in the form \code{'y = function(x)'}.
#' For example: \code{convert.time = 'y = 24 * x'}
#' @param sheet.data (Numeric or Character) Number or name of a sheet in XLS or XLSX files containing experimental data (_optional_).
#' @param sheet.map (Numeric or Character) Number or name of a sheet in XLS or XLSX files containing experimental data (_optional_).
#' @param csvsep.data (Character) separator used in CSV data file (ignored for other file types).  Default: \code{";"}
#' @param dec.data (Character) decimal separator used in CSV, TSV or TXT data file.
#' @param csvsep.map (Character) separator used in CSV mapping file (ignored for other file types).  Default: \code{";"}
#' @param dec.map (Character) decimal separator used in CSV, TSV or TXT mapping file.
#' @param subtract.blank (Logical) Shall blank values be subtracted from values within the same experiment ([TRUE], the default) or not ([FALSE]).
#' @param calibration (Character or \code{NULL}) Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert density and fluorescence values. This can be used to, e.g., convert plate reader absorbance values into \ifelse{html}{\out{OD<sub>600</sub>}}{\eqn{OD_{600}}}.
#' Caution!: When utilizing calibration, carefully consider whether or not blanks were subtracted to determine the calibration before selecting the input \code{subtract.blank = TRUE}.
#' @param fl.normtype (Character string) Normalize fluorescence values by either diving by \code{'density'} or by fluorescence2 values (\code{'fl2'}).
#'
#' @return A \code{grodata} object suitable to run \code{\link{growth.workflow}}. See \code{\link{read_data}} for its structure.
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
#'                       calibration = "y = x * 3.058") # convert absorbance to OD values
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
           subtract.blank  = T,
           calibration = NULL,
           fl.normtype = c("density", "fl2")
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
      stop(paste0("File \"", data.file, "\" does not exist."), call. = F)
    }
    if(!is.null(map.file)){
      if (file.exists(map.file)) {
        mapping <- read_file(map.file, csvsep=csvsep.map, dec=dec.map, sheet=sheet.map)
      } else {
        stop(paste0("File \"", map.file, "\" does not exist."), call. = F)
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
    if(length(data.ls)==1){
      names(data.ls) <- "density"
      grodata <- read_data(data.density = data.ls[[1]], data.fl = NA, subtract.blank = subtract.blank, calibration = calibration)
    } else if(length(data.ls)==2){
      names(data.ls) <- c("density", "fluorescence")
      grodata <- read_data(data.density = data.ls[[1]], data.fl = data.ls[[2]], subtract.blank = subtract.blank, calibration = calibration, fl.normtype = fl.normtype)
    }
    else {
      names(data.ls) <- c("density", "fluorescence", "fluorescence2")
      grodata <- read_data(data.density = data.ls[[1]], data.fl = data.ls[[2]], data.fl2 = data.ls[[3]],
                           subtract.blank = subtract.blank, calibration = calibration, fl.normtype = fl.normtype)
    }
    invisible(grodata)
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

#' Extract relevant data from a raw data export file generated with the "Gen5" or "Gen6" software.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing density and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @examples
#' \dontrun{
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

    if(length(reads)>2){
     answer <- readline(paste0("Indicate where the fluorescence 2 data is stored (used only for normalization of fluorescence)?\n",
                               paste(unlist(lapply(1:length(reads), function (i)
                                 paste0("[", i, "] ", reads[i]))),
                                 collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
     if(as.numeric(answer) == length(reads)+1){
       fluorescence2 <- NA
     } else {
       fluorescence2 <- read.data[[as.numeric(answer)]]
       fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
     }
     data.ls[[3]] <- fluorescence2
    }
  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
  }
  return(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of "Chi.Bio" bioreactors.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing density and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @examples
#' \dontrun{
#' input <- read_file(filename = system.file("ChiBio.csv", package = "QurvE"), csvsep = "," )
#' parsed <- parse_chibio(input)
#' }
parse_chibio <- function(input)
{
  time.ndx <- grep("time", input[1,], ignore.case = TRUE)
  read.ndx <- grep("measured|emit", input[1,], ignore.case = TRUE)
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

    if(length(reads)>2){
      answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
                                paste(unlist(lapply(1:length(reads), function (i)
                                  paste0("[", i, "] ", reads[i]))),
                                  collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
      if(as.numeric(answer) == length(reads)+1){
        fluorescence2 <- NA
      } else {
        fluorescence2 <- data.frame("time" = input[, time.ndx], "fluorescence2" = c(input[1, read.ndx[as.numeric(answer)]], as.numeric(input[-1, read.ndx[as.numeric(answer)]])))
        fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
        if(all(as.numeric(fluorescence2[-1,2]) == 0) || all(is.na(fluorescence2[-1,2]))){
          fluorescence2 <- NA
        }
      }
      data.ls[[3]] <- fluorescence2
    }
  } else {
    density <- data.frame("time" = input[, time.ndx], "density" = c(input[1, read.ndx], as.numeric(input[-1, read.ndx])))
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }

  return(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of the "Growth Profiler".
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing a density dataframe in the first element and \code{NA} in the second. The first column in the dataframe represents a time vector.
#'
#' @examples
#' \dontrun{
#' input <- read_file(filename = system.file("GrowthProfiler.csv", package = "QurvE"), csvsep = "," )
#' parsed <- parse_growthprofiler(input)
#' }
parse_growthprofiler <- function(input)
{
  # get row numbers for "time" in column 1
  time.ndx <- grep("^\\btime\\b", input[[1]], ignore.case = TRUE)
  # get index of empty column at the and of the data series
  na.ndx <- which(is.na(input[time.ndx,]))

  density <- input[time.ndx:nrow(input), 1:(na.ndx-1)]

  data.ls <- list()
  data.ls[[1]] <- density
  data.ls[[2]] <- NA
  # data.ls[[3]] <- NA

  return(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of "Tecan" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing density and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @examples
#' \dontrun{
#' input <- read_file(filename = system.file("Tecan.csv", package = "QurvE"), csvsep = "," )
#' parsed <- parse_tecan(input)
#' }
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

    if(length(reads)>2){
      answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
                                paste(unlist(lapply(1:length(reads), function (i)
                                  paste0("[", i, "] ", reads[i]))),
                                  collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
      if(as.numeric(answer) == length(reads)+1){
        fluorescence2 <- NA
      } else {
        fluorescence2 <- read.data[[as.numeric(answer)]]
        fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
      }
      data.ls[[3]] <- fluorescence2
    }
  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }
  return(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of "Biolector" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing a density dataframe in the first element and \code{NA} in the second. The first column in the dataframe represents a time vector.
#'
#' @examples
#' \dontrun{
#' input <- read_file(filename = system.file("biolector", package = "QurvE"), csvsep = "," )
#' parsed <- parse_biolector(input)
#' }
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

#' Extract relevant data from a raw data export file generated from the software of Perkin Elmer's "Victor Nivo" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing density and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @examples
#' \dontrun{
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

    if(length(reads)>2){
      answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
                                paste(unlist(lapply(1:length(reads), function (i)
                                  paste0("[", i, "] ", reads[i]))),
                                  collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
      if(as.numeric(answer) == length(reads)+1){
        fluorescence2 <- NA
      } else {
        fluorescence2 <- read.data[[as.numeric(answer)]]
        fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
      }
      data.ls[[3]] <- fluorescence2
    }
  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }
  return(list(data.ls))
}

#' Extract relevant data from a raw data export file generated from the software of Perkin Elmer's "Victor X3" plate readers.
#'
#' @param input A dataframe created by reading a table file with \code{\link{read_file}}
#'
#' @return a list of length two containing density and/or fluorescence dataframes in the first and second element, respectively. The first column in these dataframes represents a time vector.
#'
#' @examples
#' \dontrun{
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

    if(length(reads)>2){
      answer <- readline(paste0("Indicate where the fluorescence 2 data is stored?\n",
                                paste(unlist(lapply(1:length(reads), function (i)
                                  paste0("[", i, "] ", reads[i]))),
                                  collapse = "\n"), "\n[", length(reads)+1, "] Disregard fluorescence 2 data\n"))
      if(as.numeric(answer) == length(reads)+1){
        fluorescence2 <- NA
      } else {
        fluorescence2 <- read.data[[as.numeric(answer)]]
        fluorescence2[which(fluorescence2 == "OVRFLW", arr.ind = TRUE)] <- NA
      }
      data.ls[[3]] <- fluorescence2
    }
  } else {
    density <- read.data[[1]]
    data.ls[[1]] <- density
    data.ls[[2]] <- NA
    # data.ls[[3]] <- NA
  }
  return(list(data.ls))
}
