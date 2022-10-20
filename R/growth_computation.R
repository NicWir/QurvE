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
#' @param data.format (Character) "col" for samples in columns, or "row" for samples in rows. Default: \code{"col"}
#' @param csvsep (Character) separator used in CSV file storing density data (ignored for other file types). Default: \code{";"}
#' @param dec (Character) decimal separator used in CSV, TSV or TXT file storing density data. Default: \code{"."}
#' @param csvsep.fl (Character) separator used in CSV file storing fluorescence data (ignored for other file types). Default: \code{";"}
#' @param dec.fl (Character) decimal separator used in CSV, TSV or TXT file storing fluorescence data. Default: \code{"."}
#' @param subtract.blank (Logical) Shall blank values be subtracted from values within the same experiment ([TRUE], the default) or not ([FALSE]). If \code{calibration = TRUE}, blanks are subtracted after value conversion.
#' @param sheet.density (Numeric or Character) Number or name of the sheet with density data in XLS or XLSX files (_optional_).
#' @param sheet.fl (Numeric or Character) Number or name of the sheet with fluorescence data in XLS or XLSX files (_optional_).
#' @param convert.time (\code{NULL} or string) Convert time values with a formula provided in the form \code{'y = function(x)'}.
#' For example: \code{convert.time = 'y = 24 * x'}
#' @param calibration (Character or \code{NULL}) Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert density and fluorescence values. This can be used to, e.g., convert plate reader absorbance values into \ifelse{html}{\out{OD<sub>600</sub>}}{\eqn{OD_{600}}}.
#' Caution!: When utilizing calibration, carefully consider whether or not blanks were subtracted to determine the calibration before selecting the input \code{subtract.blank = TRUE}.
#'
#' @details
#' \figure{Data_layout.png}
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
read_data <-
  function(data.density,
           data.fl = NA,
           data.format = "col",
           csvsep = ";",
           dec = ".",
           csvsep.fl = ";",
           dec.fl = ".",
           sheet.density = 1,
           sheet.fl = 1,
           subtract.blank  = T,
           convert.time = NULL,
           calibration = NULL)
  {
    if(is.null(data.density)) data.density <- NA
    if(is.null(data.fl)) data.fl <- NA
    # if(is.null(data.fluoro2)) data.fluoro2 <- NA
    if(!is.null(calibration) && calibration == "") calibration <- NULL

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
    # # Load fluorescence 2 data
    # if((length(data.fluoro2) > 1 ) || !is.na(data.fluoro2)){
    #   if (!is.character(data.fluoro2)) {
    #     fluoro2 <- data.fluoro2
    #   } else {
    #     # Read table file
    #     fluoro2 <- read_file(data.fluoro2, csvsep=csvsep.fl2, dec=dec.fl2, sheet=sheet.fluoro2)
    #   }
    #   if(data.format == "col"){
    #     fluoro2 <- t(fluoro2)
    #   }
    #   if(!(any(grepl("time", unlist(fluoro2[,1]), ignore.case = TRUE)))){
    #     if(data.format == "col"){
    #       stop("Could not find 'time' in column 1 of data.fluoro2")
    #     } else {
    #       stop("Could not find 'time' in row 1 of data.fluoro2")
    #     }
    #   }
    # } else {
    #   fluoro2 <- NA
    # }
    if((length(dat) > 1 && !(any(grepl("time", unlist(dat[,1]), ignore.case = TRUE)))) &&
       (length(fl) > 1 && !(any(grepl("time", unlist(fl[,1]), ignore.case = TRUE)))) ){ #&& (length(fluoro2) > 1 && !(any(grepl("time", unlist(fluoro2[,1]), ignore.case = TRUE))))
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
      # if((length(fluoro2) > 1 ) || !is.na(data.fluoro2))    fluoro2 <- calibrate(df=fluoro2)

    }

    # subtract blank
    if(subtract.blank){
      subtract_blank <- function(df){
        #test if more than one time entity is present
        time.ndx <- grep("time", unlist(df[,1]), ignore.case = TRUE)
        if(length(time.ndx)==1){
          blank.ndx <- grep("blank", df[1:nrow(df),1], ignore.case = T)
          if(length(blank.ndx)>0){
            if(length(blank.ndx)>1){
              blank <- rowMeans(apply(df[blank.ndx, 4:ncol(df)], 1, as.numeric), na.rm = T)
            } else {
              blank <- as.numeric(df[blank.ndx, 4:ncol(df)])
            }
            df[(2:nrow(df))[!((2:nrow(df)) %in% blank.ndx)], 4:ncol(df)] <- t(sweep(apply(df[(2:nrow(df))[!((2:nrow(df)) %in% blank.ndx)], 4:ncol(df)], 1, as.numeric), 1, blank))
          }
        } else { # identify different datasets based on the occurence of multiple 'time' entities
          # identify additional time entities
          blank.ndx <- grep("blank", df[(time.ndx[1]) : (time.ndx[2]-1),1], ignore.case = T)
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
            }, 1], ignore.case = T) + time.ndx[i]

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
      # if((length(fluoro2) > 1 ) || !is.na(data.fluoro2))    fluoro2 <- subtract_blank(df=fluoro2)
    }

    ### Combine technical replicates
    combine_techrep <- function(df){
      sample_names <- as.character(paste0(df[2:nrow(df),1], "...", df[2:nrow(df),2], "___", df[2:nrow(df),3]))
      conditions <-
        unique(gsub("\\.\\.\\..+___", "___", sample_names))
      # remove "time" from samples in case of several time entities
      time.ndx <- grep("time", unlist(df[,1]), ignore.case = TRUE)
      if(length(time.ndx)>1){
        conditions <- conditions[-grep("time", gsub("___.+", "", conditions), ignore.case = T)]
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
    # if((length(fluoro2) > 1 ) || !is.na(data.fluoro2))     fluoro2 <- combine_techrep(df=fluoro2)


    # remove blank columns from dataset
    remove_blank <- function(df){
      blank.ndx <- grep("blank", df[1:nrow(df),1], ignore.case = T)
      if(length(blank.ndx)>0){
        df <- df[-blank.ndx, ]
      }
      return(df)
    }
    if(length(dat)>1)              dat <- remove_blank(dat)
    if((length(fl) > 1 ) || !is.na(data.fl))     fl <- remove_blank(df=fl)
    # if((length(fluoro2) > 1 ) || !is.na(data.fluoro2))     fluoro2 <- remove_blank(df=fluoro2)

    # Remove columns with NA measurements in all samples
    if(length(dat)>1)              dat <- dat[, c(1:3, which(unlist(lapply(4:ncol(dat), function(x)!all(is.na(dat[2:nrow(dat),x])))))+3)]
    if((length(fl) > 1 ) || !is.na(data.fl))     fl <- fl[, c(1:3, which(unlist(lapply(4:ncol(fl), function(x)!all(is.na(fl[2:nrow(fl),x])))))+3)]
    # if((length(fluoro2) > 1 ) || !is.na(data.fluoro2))     fluoro2 <- fluoro2[, c(1:3, which(unlist(lapply(4:ncol(fluoro2), function(x)!all(is.na(fluoro2[2:nrow(fluoro2),x])))))+3)]

    # add minimum negative value + 1 to all fluorescence data
    if((length(fl) > 1 ) || !is.na(data.fl)){
      num.fl <- t(apply(fl[2:nrow(fl), 4:ncol(fl)], 1, as.numeric))
      min.F1 <- unique(num.fl[which(num.fl == min(num.fl, na.rm = TRUE), arr.ind = TRUE)])
      if(min.F1 <=0){
        fl[2:nrow(fl), 4:ncol(fl)] <- num.fl+(-min.F1)+1
      }
    }
    # if((length(fluoro2) > 1 ) || !is.na(data.fluoro2)){
    #   num.fluoro2 <- t(apply(fluoro2[2:nrow(fluoro2), 4:ncol(fluoro2)], 1, as.numeric))
    #   min.F2 <- unique(num.fluoro2[which(num.fluoro2 == min(num.fluoro2, na.rm = TRUE), arr.ind = TRUE)])
    #   if(min.F2 <=0){
    #     fluoro2[2:nrow(fluoro2), 4:ncol(fluoro2)] <- num.fluoro2+(-min.F2)+1
    #   }
    # }

    # normalize fluorescence
    if(((length(fl) > 1 ) || !is.na(data.fl)) && length(dat)>1){
      fl.norm <- fl
      time.ndx <- grep("time", unlist(fl.norm[,1]), ignore.case = TRUE)
      fl.norm[-time.ndx, 4:ncol(fl.norm)] <-
        t(apply(fl.norm[-time.ndx, 4:ncol(fl.norm)], 1, as.numeric))/t(apply(dat[-time.ndx, 4:ncol(dat)], 1, as.numeric))
    }
    # if(((length(fluoro2) > 1 ) || !is.na(data.fluoro2)) && length(dat)>1){
    #   fluoro2.norm <- fl
    #   time.ndx <- grep("time", unlist(fluoro2.norm[,1]), ignore.case = TRUE)
    #   fluoro2.norm[-time.ndx, 4:ncol(fluoro2.norm)] <-
    #     t(apply(fluoro2.norm[-time.ndx, 4:ncol(fluoro2.norm)], 1, as.numeric))/t(apply(dat[-time.ndx, 4:ncol(dat)], 1, as.numeric))
    # }

    # Create time matrix
    if(length(dat) > 1) {
      time.ndx <- grep("time", unlist(dat[,1]), ignore.case = TRUE)
      dat.time <- dat
    } else if(length(fl) > 1){
      time.ndx <- grep("time", unlist(fl[,1]), ignore.case = TRUE)
      dat.time <- fl
    }
    # else if(length(fluoro2) > 1){
    #   time.ndx <- grep("time", unlist(fluoro2[,1]), ignore.case = TRUE)
    #   dat.time <- fluoro2
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
                         byrow = T)
                     )
                     )
      )
    } # end of for (i in 2:(length(time.ndx)))
  } # end of else {}

  # Create data matrix for density and fluorescence values
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
  # if((length(fluoro2) > 1 ) || !is.na(data.fluoro2)){fluoro2.mat <- create_datmat(df=fluoro2, time.ndx=time.ndx);  fluoro2.mat[,1] <- gsub("\\n\\r|\\n|\\r", "", fluoro2.mat[,1])}else{fluoro2.mat <- NA}
  if(((length(fl) > 1 ) || !is.na(data.fl)) && length(dat)>1){fl.norm.mat <- create_datmat(df=fl.norm, time.ndx=time.ndx);  fl.norm.mat[,1] <- gsub("\\n\\r|\\n|\\r", "", fl.norm.mat[,1])}else{fl.norm.mat <- NA}
  # if(((length(fluoro2) > 1 ) || !is.na(data.fluoro2)) && length(dat)>1){fluoro2.norm.mat <- create_datmat(df=fluoro2.norm, time.ndx=time.ndx);  fluoro2.norm.mat[,1] <- gsub("\\n\\r|\\n|\\r", "", fluoro2.norm.mat[,1])}else{fluoro2.norm.mat <- NA}

  if(length(dat)>1)             colnames(dat.mat)[1:3] <- c("condition", "replicate", "concentration")
  if((length(fl) > 1 ) || !is.na(data.fl))    colnames(fl.mat)[1:3] <- c("condition", "replicate", "concentration")
  # if((length(fluoro2) > 1 ) || !is.na(data.fluoro2))    colnames(fluoro2.mat)[1:3] <- c("condition", "replicate", "concentration")
  if(((length(fl) > 1 ) || !is.na(data.fl)) && length(dat)>1)  colnames(fl.norm.mat)[1:3] <- c("condition", "replicate", "concentration")
  # if(((length(fluoro2) > 1 ) || !is.na(data.fluoro2)) && length(dat)>1)  colnames(fluoro2.norm.mat)[1:3] <- c("condition", "replicate", "concentration")

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
  # else if(length(fluoro2) > 1){
  #   label <- unlist(lapply(1:nrow(fluoro2.mat), function(x) paste(fluoro2.mat[x,1], fluoro2.mat[x,2], fluoro2.mat[x,3], sep = " | ")))
  #   condition <- fluoro2.mat[, 1]
  #   replicate <- fluoro2.mat[, 2]
  #   concentration <- fluoro2.mat[, 3]
  # }


  expdesign <- data.frame(label, condition, replicate, concentration, check.names = FALSE)

  if(length(dat)>1)             dat.mat <- as.data.frame(unclass(dat.mat), stringsAsFactors = TRUE)
  if((length(data.fl) > 1 ) || !is.na(data.fl))    fl.mat <- as.data.frame(unclass(fl.mat), stringsAsFactors = TRUE)
  # if((length(data.fluoro2) > 1 ) || !is.na(data.fluoro2))    fluoro2.mat <- as.data.frame(unclass(fluoro2.mat), stringsAsFactors = TRUE)
  if(((length(data.fl) > 1 ) || !is.na(data.fl)) && length(dat)>1)  fl.norm.mat <- as.data.frame(unclass(fl.norm.mat), stringsAsFactors = TRUE)
  # if(((length(data.fluoro2) > 1 ) || !is.na(data.fluoro2)) && length(dat)>1)  fluoro2.norm.mat <- as.data.frame(unclass(fluoro2.norm.mat), stringsAsFactors = TRUE)
  #convert values from factor to numeric
  if(length(dat)>1)             dat.mat[, -(1:3)] <- as.numeric(as.matrix(dat.mat[, -(1:3)]))
  if((length(data.fl) > 1 ) || !is.na(data.fl))    fl.mat[, -(1:3)] <- as.numeric(as.matrix(fl.mat[, -(1:3)]))
  # if((length(data.fluoro2) > 1 ) || !is.na(data.fluoro2))    fluoro2.mat[, -(1:3)] <- as.numeric(as.matrix(fluoro2.mat[, -(1:3)]))
  if(((length(data.fl) > 1 ) || !is.na(data.fl)) && length(dat)>1)  fl.norm.mat[, -(1:3)] <- as.numeric(as.matrix(fl.norm.mat[, -(1:3)]))
  # if(((length(data.fluoro2) > 1 ) || !is.na(data.fluoro2)) && length(dat)>1)  fluoro2.norm.mat[, -(1:3)] <- as.numeric(as.matrix(fluoro2.norm.mat[, -(1:3)]))

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

        # if(length(fluoro2.mat) > 1){
        #   f2.ls[[i]] <- c(fluoro2.mat[i, 1:3], fluoro2.mat[i, combine.ndx.dat]) # remove entries with duplicated time values except the first ones
        #   for(j in 1:length(ndx.average.first)){
        #     f2.ls[[i]][ndx.average.first[j]] <- mean(as.numeric(fluoro2.mat[i, (col.ndx[[j]]+3)]))
        #   }
        # }

        if(length(fl.norm.mat) > 1){
          f1.norm.ls[[i]] <- c(fl.norm.mat[i, 1:3], fl.norm.mat[i, combine.ndx.dat]) # remove entries with duplicated time values except the first ones
          for(j in 1:length(ndx.average.first)){
            f1.norm.ls[[i]][ndx.average.first[j]] <- mean(as.numeric(fl.norm.mat[i, (col.ndx[[j]]+3)]))
          }
        }

        # if(length(fluoro2.norm.mat) > 1){
        #   f2.norm.ls[[i]] <- c(fluoro2.norm.mat[i, 1:3], fluoro2.norm.mat[i, combine.ndx.dat]) # remove entries with duplicated time values except the first ones
        #   for(j in 1:length(ndx.average.first)){
        #     f2.norm.ls[[i]][ndx.average.first[j]] <- mean(as.numeric(fluoro2.norm.mat[i, (col.ndx[[j]]+3)]))
        #   }
        # }
      } # if(length(t.mult[[i]])>1)
    } # for(i in 1:length(t.mult))

    t.mat <- do.call(rbind, t.ls)
    if(length(dat.mat) > 1) dat.mat <- as.data.frame(unclass(do.call(rbind, dat.ls)), stringsAsFactors = TRUE)
    if(length(fl.mat) > 1) fl.mat <- as.data.frame(unclass(do.call(rbind, f1.ls)), stringsAsFactors = TRUE)
    # if(length(fluoro2.mat) > 1) fluoro2.mat <- as.data.frame(unclass(do.call(rbind, f2.ls)), stringsAsFactors = TRUE)
    if(length(fl.norm.mat) > 1) fl.norm.mat <- as.data.frame(unclass(do.call(rbind, f1.norm.ls)), stringsAsFactors = TRUE)
    # if(length(fluoro2.norm.mat) > 1) fluoro2.norm.mat <- as.data.frame(unclass(do.call(rbind, f2.norm.ls)), stringsAsFactors = TRUE)

    #convert values from factor to numeric
    if(length(dat.mat) > 1)             dat.mat[, -(1:3)] <- as.numeric(as.matrix(dat.mat[, -(1:3)]))
    if(length(fl.mat) > 1)    fl.mat[, -(1:3)] <- as.numeric(as.matrix(fl.mat[, -(1:3)]))
    # if(length(fluoro2.mat) > 1)    fluoro2.mat[, -(1:3)] <- as.numeric(as.matrix(fluoro2.mat[, -(1:3)]))
    if(length(fl.norm.mat) > 1)  fl.norm.mat[, -(1:3)] <- as.numeric(as.matrix(fl.norm.mat[, -(1:3)]))
    # if(length(fluoro2.norm.mat) > 1)  fluoro2.norm.mat[, -(1:3)] <- as.numeric(as.matrix(fluoro2.norm.mat[, -(1:3)]))

  } # if(any(unlist(t.mat.freq)[grep("Freq", names(unlist(t.mat.freq))) ] > 1))

  dataset <- list("time" = t.mat,
                  "density" = dat.mat,
                  "fluorescence" = fl.mat,
                  # "fluorescence2" = fluoro2.mat,
                  "norm.fluorescence" = fl.norm.mat,
                  # "norm.fluorescence2" = fluoro2.norm.mat,
                  "expdesign" = expdesign)

  class(dataset) <- "grodata"
  invisible(dataset)
  }

#' Parse raw plate reader data and convert it to a format compatible with QurvE
#'
#' \code{parse_data} takes a raw export file from a plate reader experiment, extracts relevant information and parses it into the format required to run \code{\link{growth.workflow}}.
#'
#' @param data.file (Character) A table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing plate reader data.
#' @param map.file (Character) A table file with extension '.xlsx', '.xls', '.csv', '.tsv', or '.txt' containing plate reader data.
#' @param software (Character) The name of the software/device used to export the plate reader data.
#' @param convert.time (\code{NULL} or string) Convert time values with a formula provided in the form \code{'y = function(x)'}.
#' For example: \code{convert.time = 'y = 24 * x'}
#' @param sheet.data (Numeric or Character) Number or name of a sheet in XLS or XLSX files containing experimental data (_optional_).
#' @param sheet.map (Numeric or Character) Number or name of a sheet in XLS or XLSX files containing experimental data (_optional_).
#' @param csvsep.data (Character) separator used in CSV data file (ignored for other file types).  Default: \code{";"}
#' @param dec.data (Character) decimal separator used in CSV, TSV or TXT data file.
#' @param csvsep.map (Character) separator used in CSV mapping file (ignored for other file types).  Default: \code{";"}
#' @param dec.map (Character) decimal separator used in CSV, TSV or TXT mapping file.
#' @param map.file (Character) A table file in column format with 'well', 'ID', 'replicate', and 'concentration' in the first row. Used to assign sample information to wells in a plate.
#' @param subtract.blank (Logical) Shall blank values be subtracted from values within the same experiment ([TRUE], the default) or not ([FALSE]).
#' @param calibration (Character or \code{NULL}) Provide an equation in the form 'y = function(x)' (for example: 'y = x^2 * 0.3 - 0.5') to convert density and fluorescence values. This can be used to, e.g., convert plate reader absorbance values into \ifelse{html}{\out{OD<sub>600</sub>}}{\eqn{OD_{600}}}.
#' Caution!: When utilizing calibration, carefully consider whether or not blanks were subtracted to determine the calibration before selecting the input \code{subtract.blank = TRUE}.
#'
#' @return A \code{grodata} object suitable to run \code{\link{growth.workflow}}. See \code{\link{read_data}} for object structure.
#'
#' @export
#'
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
           calibration = NULL
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
    if(any(grep("Gen5|Gen6", software, ignore.case = T))){
      parsed.ls <- parse_Gen5Gen6(input)
      data.ls <- parsed.ls[[1]]
    } # if("Gen5" %in% software)
    if(any(grep("Chi.Bio", software, ignore.case = T))){
      parsed.ls <- parse_chibio(input)
      data.ls <- parsed.ls[[1]]
    }
    if(any(grep("GrowthProfiler", software, ignore.case = T))){
      parsed.ls <- parse_growthprofiler(input)
      data.ls <- parsed.ls[[1]]
    }
    if(any(grep("Tecan", software, ignore.case = T))){
      parsed.ls <- parse_tecan(input)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("Biolector", software, ignore.case = T))){
      parsed.ls <- parse_biolector(input)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("VictorNivo", software, ignore.case = T))){
      parsed.ls <- parse_victornivo(input)
      data.ls <- parsed.ls[[1]]
    }

    if(any(grep("VictorX3", software, ignore.case = T))){
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
    } else {
      names(data.ls) <- c("density", "fluorescence")
      grodata <- read_data(data.density = data.ls[[1]], data.fl = data.ls[[2]], subtract.blank = subtract.blank, calibration = calibration)
    }
    # else {
    #   names(data.ls) <- c("density", "fluorescence", "fluorescence2")
    #   grodata <- read_data(data.density = data.ls[[1]], data.fl = data.ls[[2]], subtract.blank = subtract.blank, calibration = calibration)
    # }
    return(grodata)
  }

#' Create a \code{grofit.control} object.
#'
#' A \code{grofit.control} object is required to perform various computations on growth data stored within \code{grodata} objects (created with \code{\link{read_data}} or \code{\link{parse_data}}). A \code{grofit.control} object is created automatically as part of \code{\link{growth.workflow}}.
#'
#' @param neg.nan.act (Logical) Indicates whether the program should stop when negative growth values or NA values appear (\code{TRUE}). Otherwise, the program removes these values silently (\code{FALSE}). Improper values may be caused by incorrect data or input errors. Default: \code{FALSE}.
#' @param clean.bootstrap (Logical) Determines if negative values which occur during bootstrap should be removed (TRUE) or kept (FALSE). Note: Infinite values are always removed. Default: TRUE.
#' @param suppress.messages (Logical) Indicates whether messages (information about current growth curve, EC50 values etc.) should be displayed (\code{FALSE}) or not (\code{TRUE}). This option is meant to speed up the processing of high throughput data. Note: warnings are still displayed. Default: \code{FALSE}.
#' @param fit.opt (Character or character vector) Indicates whether the program should perform a linear regression (\code{"l"}), model fit (\code{"m"}), spline fit (\code{"s"}), or all (\code{"a"}). Combinations can be freely chosen by providing a character vector, e.g. \code{fit.opt = c("l", "s")} Default:  \code{fit.opt = c("l", "s")}.
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param tmax (Numeric) Maximum time value considered for linear and spline fits.
#' @param min.density (Numeric) Indicate whether only density values above a certain threshold should be considered for linear regressions or spline fits.
#' @param max.density (Numeric) Indicate whether only density values below a certain threshold should be considered for linear regressions or spline fits.
#' @param log.x.gc (Logical) Indicates whether _ln(x+1)_ should be applied to the time data for _linear_ and _spline_ fits. Default: \code{FALSE}.
#' @param log.y.lin (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ fits. Default: \code{TRUE}
#' @param log.y.spline (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _spline_ fits. Default: \code{TRUE}
#' @param log.y.model (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _model_ fits. Default: \code{TRUE}
#' @param biphasic (Logical) Shall \code{\link{growth.gcFitLinear}} and \code{\link{growth.gcFitSpline}} try to extract growth parameters for two different growth phases (as observed with, e.g., diauxic shifts) (\code{TRUE}) or not (\code{FALSE})?
#' @param lin.h (Numeric) Manually define the size of the sliding window used in \code{\link{growth.gcFitLinear}} If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{\link{growth.gcFitLinear}}
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for the calculated slope in \code{\link{growth.gcFitLinear}}
#' @param lin.dY (Numeric) Threshold for the minimum fraction of density increase a linear regression window should cover. Default: 0.05 (5%).
#' @param interactive (Logical) Controls whether the fit of each growth curve and method is controlled manually by the user. If \code{TRUE}, each fit is visualized in the _Plots_ pane and the user can adjust fitting parameters and confirm the reliability of each fit per sample. Default: \code{TRUE}.
#' @param nboot.gc (Numeric) Number of bootstrap samples used for nonparametric growth curve fitting with \code{\link{growth.gcBootSpline}}. Use \code{nboot.gc = 0} to disable the bootstrap. Default: \code{0}
#' @param smooth.gc (Numeric) Parameter describing the smoothness of the spline fit; usually (not necessary) within (0;1]. \code{smooth.gc=NULL} causes the program to query an optimal value via cross validation techniques. Especially for datasets with few data points the option \code{NULL} might cause a too small smoothing parameter. This can result a too tight fit that is susceptible to measurement errors (thus overestimating growth rates) or produce an error in \code{\link{smooth.spline}} or lead to overfitting. The usage of a fixed value is recommended for reproducible results across samples. See \code{\link{smooth.spline}} for further details. Default: \code{0.55}
#' @param model.type (Character) Vector providing the names of the parametric models which should be fitted to the data. Default: \code{c("gompertz", "logistic", "gompertz.exp", "richards")}.
#' @param dr.method (Character) Define the method used to perform a dose-responde analysis: smooth spline fit (\code{"spline"}) or model fitting (\code{"model"}).
#' @param dr.model (Character) Provide a list of models from the R package 'drc' to include in the dose-response analysis (if \code{dr.method = "model"}). If more than one model is provided, the best-fitting model will be chosen based on the Akaike Information Criterion.
#' @param dr.have.atleast (Numeric) Minimum number of different values for the response parameter one should have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: \code{6}.
#' @param dr.parameter (Character or numeric) The response parameter in the output table to be used for creating a dose response curve. See \code{\link{growth.drFit}} for further details. Default: \code{"mu.linfit"}, which represents the maximum slope of the linear regression. Typical options include: \code{"mu.linfit"}, \code{"lambda.linfit"}, \code{"dY.linfit"}, \code{"mu.spline"}, and \code{"dY.spline"}.
#' @param smooth.dr (Numeric) Smoothing parameter used in the spline fit by smooth.spline during dose response curve estimation. Usually (not necessesary) in (0; 1]. See \code{\link{smooth.spline}} for further details. Default: \code{NULL}.
#' @param log.x.dr (Logical) Indicates whether \code{ln(x+1)} should be applied to the concentration data of the dose response curves. Default: \code{FALSE}.
#' @param log.y.dr (Logical) Indicates whether \code{ln(y+1)} should be applied to the response data of the dose response curves. Default: \code{FALSE}.
#' @param nboot.dr (Numeric) Defines the number of bootstrap samples for EC50 estimation. Use \code{nboot.dr = 0} to disable bootstrapping. Default: \code{0}.
#' @param growth.thresh (Numeric) Define a threshold for growth. Only if any density value in a sample is greater than \code{growth.thresh} (default: 1.5) times the start density, further computations are performed. Else, a message is returned.
#'
#' @return Generates a list with all arguments described above as entries.
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @export
#'
growth.control <- function (neg.nan.act = FALSE,
                            clean.bootstrap = TRUE,
                            suppress.messages = FALSE,
                            fit.opt = c("a"),
                            t0 = 0,
                            tmax = NA,
                            min.density = NA,
                            max.density = NA,
                            log.x.gc = FALSE,
                            log.y.lin = TRUE,
                            log.y.spline = TRUE,
                            log.y.model = TRUE,
                            lin.h = NULL,
                            lin.R2 = 0.97,
                            lin.RSD = 0.1,
                            lin.dY = 0.05,
                            biphasic = FALSE,
                            interactive = FALSE,
                            nboot.gc = 0,
                            smooth.gc = 0.55,
                            model.type = c("logistic", "richards", "gompertz", "gompertz.exp", "huang", "baranyi"),
                            dr.method = c("model", "spline"),
                            dr.model = c('gammadr', 'multi2',
                                         'LL.2', 'LL.3', 'LL.4', 'LL.5', 'W1.2',
                                         'W1.3', 'W1.4', 'W2.2', 'W2.3', 'W2.4', 'LL.3u',
                                         'LL2.2', 'LL2.3', 'LL2.3u', 'LL2.4',
                                         'LL2.5', 'AR.2', 'AR.3'),
                            dr.have.atleast = 6, # Minimum number of different values for the response parameter one shoud have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: 6.
                            dr.parameter = "mu.linfit", # parameter used for creating dose response curve. # 34 is µ determined with spline fit
                            smooth.dr = NULL,
                            log.x.dr = FALSE,
                            log.y.dr = FALSE,
                            nboot.dr = 0,
                            growth.thresh = 1.5)
{
  if(!is.null(lin.h) && (lin.h == "" || lin.h == "NULL" || lin.h == 0)) lin.h <- NULL
  if(nboot.gc == "" || is.null(nboot.gc)) nboot.gc <- 0
  if ((is.character(fit.opt) == FALSE) | !any(fit.opt %in% c("l", "s", "m", "a")))
    stop("value of fit.opt must be character and contain one or more of 'l', 's', or 'm', or be 'a' (for all).")
  if (is.character(model.type) == FALSE)
    stop("value of model.type must be character")
  if (is.character(dr.model) == FALSE)
    stop("value of dr.model must be character")
  if ((is.logical(neg.nan.act) == FALSE) | (length(neg.nan.act) != 1))
    stop("value of neg.nan.act must be logical and of one element")
  if ((is.logical(clean.bootstrap) == FALSE) | (length(clean.bootstrap) !=1))
    stop("value of clean.bootstrap must be logical and of one element")
  if ((is.logical(suppress.messages) == FALSE) | (length(suppress.messages) != 1))
    stop("value of suppress.messages must be logical and of one element")
  if ((is.logical(log.x.gc) == FALSE) | (length(log.x.gc) != 1))
    stop("value of log.x.gc must be logical and of one element")
  if ((is.logical(log.y.lin) == FALSE) | (length(log.y.lin) != 1))
    stop("value of log.y.lin must be logical and of one element")
  if ((is.logical(log.y.spline) == FALSE) | (length(log.y.spline) != 1))
    stop("value of log.y.spline must be logical and of one element")
  if ((is.logical(interactive) == FALSE) | (length(interactive) != 1))
    stop("value of interactive must be logical and of one element")
  if ((is.logical(log.x.dr) == FALSE) | (length(log.x.dr) != 1))
    stop("value of log.x.dr must be logical and of one element")
  if ((is.logical(log.y.dr) == FALSE) | (length(log.y.dr) !=1))
    stop("value of log.y.dr must be logical and of one element")
  if ((is.numeric(nboot.gc) == FALSE) | (length(nboot.gc) !=1) | (nboot.gc < 0))
    stop("value of nboot.gc must be numeric (>=0) and of one element")
  if ((is.numeric(dr.have.atleast) == FALSE) | (length(dr.have.atleast) != 1) | (dr.have.atleast < 6))
    stop("value of dr.have.atleast must be numeric (>=6) and of one element")
  if (((is.character(dr.parameter) == FALSE) && (is.numeric(dr.parameter) == FALSE)) | (length(dr.parameter) !=1))
    stop("value of dr.parameter must be a string or numeric and of one element")
  if ((is.character(dr.method) == FALSE) | (length(dr.parameter) !=1))
    stop("value of dr.method must be a string and of one element")
  if ((is.numeric(nboot.dr) == FALSE) | (length(nboot.dr) != 1) | (nboot.dr < 0))
    stop("value of nboot.dr must be numeric (>=0) and of one element")
  if (((is.numeric(smooth.gc) == FALSE)))
    stop("value of smooth.gc must be numeric")
  if (((is.numeric(smooth.dr) == FALSE) && (is.null(smooth.dr) == FALSE)))
    stop("value of smooth.dr must be numeric or NULL")
  if ((is.logical(biphasic) == FALSE) | (length(biphasic) != 1))
    stop("value of biphasic must be logical and of one element")
  if ((is.numeric(lin.dY) == FALSE) | (length(lin.dY) != 1) | (lin.dY < 0))
    stop("value of lin.dY must be numeric (>=0) and of one element")
  if (((is.numeric(lin.R2) == FALSE) |  (length(lin.R2) != 1) | !(0 < lin.R2 && lin.R2 < 1) ))
    stop("value of lin.R2 must be numeric (0 < lin.R2 < 1) and of one element")
  if (((is.numeric(lin.RSD) == FALSE) |  (length(lin.RSD) != 1) | !(0 < lin.RSD) ))
    stop("value of lin.RSD must be numeric (0 < lin.RSD) and of one element")
  if (((is.numeric(lin.h) == FALSE) && (is.null(lin.h) == FALSE)))
    stop("value of lin.h must be numeric (> 0) and of one element, or NULL")
  if (((is.numeric(growth.thresh) == FALSE) && (is.na(growth.thresh) == FALSE)))
    stop("value of growth.thresh must be numeric (one element) or NA")
  if ((is.numeric(t0) == FALSE) | (length(t0) != 1) | (t0 < 0))
    stop("value of t0 must be numeric (>=0) and of one element")

  dr.parameters.opt <- c('TestId', 'AddId', 'concentration', 'reliability_tag', 'used.model', 'log.x',
                      'log.y', 'nboot.gc', 'mu.linfit', 'lambda.linfit', 'stdmu.linfit', 'dY.linfit',
                      'A.linfit', 'tmu.start.linfit', 'tmu.end.linfit', 'r2mu.linfit',
                      'reliable_fit.linfit', 'mu.model', 'lambda.model', 'A.model', 'integral.model',
                      'parameter_nu.model', 'parameter_alpha.model', 'parameter_t_shift.model',
                      'stdmu.model', 'stdlambda.model', 'stdA.model', 'reliable_fit.model',
                      'ci90.mu.model.lo', 'ci90.mu.model.up', 'ci90.lambda.model.lo',
                      'ci90.lambda.model.up', 'ci90.A.model.lo', 'ci90.A.model.up', 'ci95.mu.model.lo',
                      'ci95.mu.model.up', 'ci95.lambda.model.lo', 'ci95.lambda.model.up',
                      'ci95.A.model.lo', 'ci95.A.model.up', 'mu.spline', 'lambda.spline', 'y0.spline',
                      'A.spline', 'dY.spline', 'integral.spline', 'reliable_fit.spline', 'smooth.spline',
                      'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt', 'stdmu.bt', 'stdlambda.bt', 'stdA.bt',
                      'stdintegral.bt', 'reliable_fit.bt', 'ci90.mu.bt.lo', 'ci90.mu.bt.up',
                      'ci90.lambda.bt.lo', 'ci90.lambda.bt.up', 'ci90.A.bt.lo', 'ci90.A.bt.up',
                      'ci90.integral.bt.lo', 'ci90.integral.bt.up', 'ci95.mu.bt.lo', 'ci95.mu.bt.up',
                      'ci95.lambda.bt.lo', 'ci95.lambda.bt.up', 'ci95.A.bt.lo', 'ci95.A.bt.up',
                      'ci95.integral.bt.lo', 'ci95.integral.bt.up')
  parameter.in <- dr.parameter
  if(is.character(dr.parameter)){
    if(is.na(dr.parameter)){
      stop(paste0(parameter.in, " is not a valid parameter for the dose-response analysis. See ?growth.drFit for possible options"))
    }
  }
  grofit.control <- list(neg.nan.act = neg.nan.act, clean.bootstrap = clean.bootstrap,
                         suppress.messages = suppress.messages, fit.opt = fit.opt, t0 = t0, tmax = tmax, min.density = min.density, max.density = max.density,
                         log.x.gc = log.x.gc, log.y.lin = log.y.lin, log.y.spline = log.y.spline, log.y.model=log.y.model, biphasic = biphasic,
                         lin.h = lin.h, lin.R2 = lin.R2, lin.RSD = lin.RSD, lin.dY = lin.dY, interactive = interactive,
                         nboot.gc = round(nboot.gc), smooth.gc = smooth.gc, smooth.dr = smooth.dr, dr.method = dr.method, dr.model = dr.model,
                         dr.have.atleast = round(dr.have.atleast), dr.parameter = dr.parameter,
                         log.x.dr = log.x.dr, log.y.dr = log.y.dr, nboot.dr = round(nboot.dr),
                         model.type = model.type, growth.thresh = growth.thresh)
  class(grofit.control) <- "grofit.control"
  grofit.control
}

#' Run a complete growth curve analysis and dose-reponse analysis workflow.
#'
#' \code{growth.workflow} runs \code{\link{growth.control}} to create a \code{grofit.control} object and then performs all computational fitting operations based on the user input. Finally, if desired, a final report is created in PDF or HTML format that summarizes all results obtained.
#'
#' @param grodata A \code{grodata} object created with \code{\link{read_data}} or \code{\link{parse_data}}, or a list containing a \code{'time'} matrix as well as a \code{'density'} dataframe.
#' @param time (optional) A matrix containing time values for each sample.
#' @param data (optional) A dataframe containing growth data (if a \code{time} matrix is provided as separate argument).
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param tmax (Numeric) Maximum time value considered for linear and spline fits.
#' @param ec50 (Logical) Perform dose-response analysis (\code{TRUE}) or not (\code{FALSE}).
#' @param mean.grp (\code{"all"}, a string vector, or a list of string vectors) Define groups to combine into common plots in the final report based on sample identifiers (if \code{report == TRUE}). Partial matches with sample/group names are accepted. Note: The maximum number of sample groups (with unique condition/concentration indicators) is 50. If you have more than 50 groups, option \code{"all"} will produce the error \code{! Insufficient values in manual scale. [Number] needed but only 50 provided}.
#' @param mean.conc (A numeric vector, or a list of numeric vectors) Define concentrations to combine into common plots in the final report (if \code{report == TRUE}).
#' @param neg.nan.act (Logical) Indicates whether the program should stop when negative growth values or NA values appear (\code{TRUE}). Otherwise, the program removes these values silently (\code{FALSE}). Improper values may be caused by incorrect data or input errors. Default: \code{FALSE}.
#' @param clean.bootstrap (Logical) Determines if negative values which occur during bootstrap should be removed (TRUE) or kept (FALSE). Note: Infinite values are always removed. Default: TRUE.
#' @param suppress.messages (Logical) Indicates whether grofit messages (information about current growth curve, EC50 values etc.) should be displayed (\code{FALSE}) or not (\code{TRUE}). This option is meant to speed up the high-throughput processing data. Note: warnings are still displayed. Default: \code{FALSE}.
#' @param fit.opt (Character or character vector) Indicates whether the program should perform a linear regression (\code{"l"}), model fit (\code{"m"}), spline fit (\code{"s"}), or all (\code{"a"}). Combinations can be freely chosen by providing a character vector, e.g. \code{fit.opt = c("l", "s")} Default:  \code{fit.opt = c("l", "s")}.
#' @param min.density (Numeric) Indicate whether only density values above a certain threshold should be considered for linear regressions or spline fits.
#' @param max.density (Numeric) Indicate whether only density values below a certain threshold should be considered for linear regressions or spline fits.
#' @param log.x.gc (Logical) Indicates whether _ln(x+1)_ should be applied to the time data for _linear_ and _spline_ fits. Default: \code{FALSE}.
#' @param log.y.lin (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ fits. Default: \code{TRUE}
#' @param log.y.spline (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _spline_ fits. Default: \code{TRUE}
#' @param log.y.model (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _model_ fits. Default: \code{TRUE}
#' @param biphasic (Logical) Shall \code{\link{growth.gcFitLinear}} and \code{\link{growth.gcFitSpline}} try to extract growth parameters for two different growth phases (as observed with, e.g., diauxic shifts) (\code{TRUE}) or not (\code{FALSE})?
#' @param lin.h (Numeric) Manually define the size of the sliding window used in \code{\link{growth.gcFitLinear}} If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{\link{growth.gcFitLinear}}
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for calculated slope in \code{\link{growth.gcFitLinear}}
#' @param lin.dY (Numeric) Threshold for the minimum fraction of density increase a linear regression window should cover. Default: 0.05 (5%).
#' @param interactive (Logical) Controls whether the fit of each growth curve and method is controlled manually by the user. If \code{TRUE}, each fit is visualized in the _Plots_ pane and the user can adjust fitting parameters and confirm the reliability of each fit per sample. Default: \code{TRUE}.
#' @param nboot.gc (Numeric) Number of bootstrap samples used for nonparametric growth curve fitting with \code{\link{growth.gcBootSpline}}. Use \code{nboot.gc = 0} to disable the bootstrap. Default: \code{0}
#' @param smooth.gc (Numeric) Parameter describing the smoothness of the spline fit; usually (not necessary) within (0;1]. \code{smooth.gc=NULL} causes the program to query an optimal value via cross validation techniques. Especially for datasets with few data points the option NULL might cause a too small smoothing parameter. This can result a too tight fit that is susceptible to measurement errors (thus overestimating growth rates) or produce an error in \code{smooth.spline} or lead to an overestimation. The usage of a fixed value is recommended for reproducible results across samples. See \code{?smooth.spline} for further details. Default: \code{0.55}
#' @param model.type (Character) Vector providing the names of the parametric models which should be fitted to the data. Default: \code{c("gompertz", "logistic", "gompertz.exp", "richards")}.
#' @param dr.method (Character) Define the method used to perform a dose-responde analysis: smooth spline fit (\code{"spline"}) or model fitting (\code{"model"}).
#' @param dr.model (Character) Provide a list of models from the R package 'drc' to include in the dose-response analysis (if \code{dr.method = "model"}). If more than one model is provided, the best-fitting model will be chosen based on the Akaike Information Criterion.
#' @param growth.thresh (Numeric) Define a threshold for growth. Only if any density value in a sample is greater than \code{growth.thresh} (default: 1.5) times the start density, further computations are performed. Else, a message is returned.
#' @param dr.have.atleast (Numeric) Minimum number of different values for the response parameter one should have for estimating a dose response curve. Note: All fit procedures require at least six unique values. Default: \code{6}.
#' @param dr.parameter (Character or numeric) The response parameter in the output table to be used for creating a dose response curve. See \code{\link{growth.drFit}} for further details. Default: \code{"mu.linfit"}, which represents the maximum slope of the linear regression. Typical options include: \code{"mu.linfit"}, \code{"lambda.linfit"}, \code{"dY.linfit"}, \code{"mu.spline"}, and \code{"dY.spline"}.
#' @param smooth.dr (Numeric) Smoothing parameter used in the spline fit by smooth.spline during dose response curve estimation. Usually (not necessesary) in (0; 1]. See documentation of smooth.spline for further details. Default: \code{NULL}.
#' @param log.x.dr (Logical) Indicates whether \code{ln(x+1)} should be applied to the concentration data of the dose response curves. Default: \code{FALSE}.
#' @param log.y.dr (Logical) Indicates whether \code{ln(y+1)} should be applied to the response data of the dose response curves. Default: \code{FALSE}.
#' @param nboot.dr (Numeric) Defines the number of bootstrap samples for EC50 estimation. Use \code{nboot.dr = 0} to disable bootstrapping. Default: \code{0}.
#' @param report (Character or NULL) Create a PDF (\code{'pdf'}) and/or HTML (\code{'html'}) report after running all computations. Define \code{NULL} if no report should be created. Default: (\code{c('pdf', 'html')})
#' @param out.dir {Character or \code{NULL}} Define the name of a folder in which all result files are stored. If \code{NULL}, the folder will be named with a combination of "GrowthResults_" and the current date and time.
#' @param out.nm {Character or \code{NULL}} Define the name of the report files. If \code{NULL}, the files will be named with a combination of "GrowthReport_" and the current date and time.
#' @param export.fig (Logical) Export all figures created in the report as separate PNG and PDF files (\code{TRUE}) or not (\code{FALSE}).
#' @param ... Further arguments passed to the shiny app.
#'
#' @family workflows
#' @family growth fitting functions
#' @family dose-response analysis functions
#'
#' @return A \code{grofit} object that contains all computation results, compatible with various plotting functions of the QurvE package and with \code{\link{growth.report}}.
#' \item{time}{Raw time matrix passed to the function as \code{time} (if no \code{grofit} object is provided).}
#' \item{data}{Raw density dataframe passed to the function as \code{data} (if no \code{grofit} object is provided).}
#' \item{gcFit}{\code{gcFit} object created with the call of \code{\link{growth.gcFit}}.}
#' \item{drFit}{\code{drFit} object created with the call of \code{\link{growth.drFit}}.}
#' \item{expdesign}{Experimental design table inherited from \code{grodata} or created from the identifier columns (columns 1-3) in \code{data}.}
#' \item{control}{Object of class \code{grofit.control} created with the call of \code{\link{growth.control}}.}
#'
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
growth.workflow <- function (grodata = NULL,
                             time = NULL,
                             data = NULL,
                             ec50 = FALSE,
                             mean.grp = NA,
                             mean.conc = NA,
                             neg.nan.act = FALSE,
                             clean.bootstrap = TRUE,
                             suppress.messages = FALSE,
                             fit.opt = c("a"),
                             t0 = 0,
                             tmax = NA,
                             min.density = NA,
                             max.density = NA,
                             log.x.gc = FALSE,
                             log.y.lin = TRUE,
                             log.y.spline = TRUE,
                             log.y.model = TRUE,
                             biphasic = FALSE,
                             lin.h = NULL,
                             lin.R2 = 0.97,
                             lin.RSD = 0.1,
                             lin.dY = 0.05,
                             interactive = FALSE,
                             nboot.gc = 0,
                             smooth.gc = 0.55,
                             model.type = c("logistic", "richards", "gompertz", "gompertz.exp", "huang", "baranyi"),
                             dr.method = c("model", "spline"),
                             dr.model = c('gammadr', 'multi2',
                                          'LL.2', 'LL.3', 'LL.4', 'LL.5', 'W1.2',
                                          'W1.3', 'W1.4', 'W2.2', 'W2.3', 'W2.4', 'LL.3u',
                                          'LL2.2', 'LL2.3', 'LL2.3u', 'LL2.4',
                                          'LL2.5', 'AR.2', 'AR.3'),
                             growth.thresh = 1.5,
                             dr.have.atleast = 6,
                             dr.parameter = 34,
                             smooth.dr = 0.1,
                             log.x.dr = FALSE,
                             log.y.dr = FALSE,
                             nboot.dr = 0,
                             report = c('pdf', 'html'),
                             out.dir = NULL,
                             out.nm = NULL,
                             export.fig = FALSE,
                             ...
)
{
  if(exists("lin.h") && !is.null(lin.h) && (is.na(lin.h) || lin.h == "")) lin.h <- NULL
  # Define objects based on additional function calls
  call <- match.call()

  ## remove strictly defined arguments
  call$grodata <- call$time <- call$data <- call$ec50 <- call$mean.grp <- call$mean.conc <- call$neg.nan.act <- call$clean.bootstrap <- call$suppress.messages <-
    call$fit.opt <- call$t0 <- call$min.density <- call$log.x.gc <- call$log.y.spline <- call$log.y.lin <- call$log.y.model <- call$biphasic <- call$tmax <- call$max.density <-
    call$lin.h <- call$lin.R2 <- call$lin.RSD <- call$lin.dY <- call$interactive <- call$nboot.gc <- call$smooth.gc <- call$model.type <- call$growth.thresh <- call$dr.method <- call$dr.model <-
    call$dr.have.atleast <- call$dr.parameter  <- call$smooth.dr  <- call$log.x.dr  <- call$log.y.dr <- call$nboot.dr <- call$report <- call$out.dir <- call$out.nm <- call$export.fig <- NULL


  arglist <- sapply(call, function(x) x)
  arglist <- unlist(arglist)[-1]
  ## Assign additional arguments (...) as R objects
  if(length(arglist) > 0){
    for(i in 1:length(arglist)){
      assign(names(arglist)[i], arglist[[i]])
    }
  }

  # Test input
  if(is.null(grodata) || !(is(grodata)=="list") && !(is(grodata)=="grodata")){
    if (is.numeric(as.matrix(time)) == FALSE)
      stop("Need a numeric matrix for 'time' or a grodata object created with read_data() or parse_data().")
    if (is.numeric(as.matrix(data[-1:-3])) == FALSE)
      stop("Need a numeric matrix for 'data' or a grodata object created with read_data() or parse_data().")
    if (is.logical(ec50) == FALSE)
      stop("Need a logical value for 'ec50'")
  } else if(!is.null(grodata)){
    time <- grodata$time
    if(!is.null(grodata$expdesign)) expdesign <- grodata$expdesign
    data <- grodata$density
  } else {
    dat.mat <- as.matrix(data)
    label <- unlist(lapply(1:nrow(dat.mat), function(x) paste(dat.mat[x,1], dat.mat[x,2], dat.mat[x,3], sep = " | ")))
    condition <- dat.mat[, 1]
    replicate <- dat.mat[, 2]
    concentration <- dat.mat[, 3]
    expdesign <- data.frame(label, condition, replicate, concentration, check.names = FALSE)
  }
  control <- growth.control(neg.nan.act = neg.nan.act, clean.bootstrap = clean.bootstrap,
                            suppress.messages = suppress.messages, fit.opt = fit.opt, t0 = t0, min.density = min.density, tmax = tmax, max.density = max.density,
                            log.x.gc = log.x.gc, log.y.lin = log.y.lin, log.y.spline = log.y.spline, log.y.model = log.y.model, biphasic = biphasic,
                            lin.h = lin.h, lin.R2 = lin.R2, lin.RSD = lin.RSD, lin.dY = lin.dY, interactive = interactive,
                            nboot.gc = round(nboot.gc), smooth.gc = smooth.gc, smooth.dr = smooth.dr, dr.method = dr.method, dr.model = dr.model,
                            dr.have.atleast = round(dr.have.atleast), dr.parameter = dr.parameter,
                            log.x.dr = log.x.dr, log.y.dr = log.y.dr, nboot.dr = round(nboot.dr),
                            model.type = model.type, growth.thresh = growth.thresh)
  nboot.gc <- control$nboot.gc
  nboot.dr <- control$nboot.dr
  out.gcFit <- NA
  out.drFit <- NA
  class(out.drFit) <- "drFit"

  # /// fit of growth curves -----------------------------------
  if(exists("shiny") && shiny == TRUE) out.gcFit <- growth.gcFit(time, data, control, shiny = TRUE)
  else out.gcFit <- growth.gcFit(time, data, control)

  # /// Estimate EC50 values
  if (ec50 == TRUE) {
    out.drFit <- growth.drFit(summary.gcFit(out.gcFit), control)
    EC50.table <- out.drFit$drTable
    boot.ec <- out.drFit$boot.ec
  }
  # ///
  grofit <- list(time = time, data = data, gcFit = out.gcFit,
                 drFit = out.drFit, expdesign = expdesign, control = control)
  class(grofit) <- "grofit"
  if(!exists("shiny") || shiny != TRUE){
    if(!is.null(out.dir)){
      wd <- paste0(out.dir)
    } else {
      wd <- paste(getwd(), "/GrowthResults_", format(Sys.time(),
                                                     "%Y%m%d_%H%M%S"), sep = "")
    }
    dir.create(wd, showWarnings = F)

    gcTable <- data.frame(apply(grofit[["gcFit"]][["gcTable"]],2,as.character))
    res.table.gc <- cbind(gcTable[,1:3], Filter(function(x) !all(is.na(x)),gcTable[,-(1:3)]))
    export_Table(table = res.table.gc, out.dir = wd, out.nm = "results.gc")
    # res.table.gc[, c(8:14, 20:27, 29:44)] <- apply(res.table.gc[, c(8:16, 20:27, 29:44)], 2, as.numeric)
    message(paste0("\n\nResults of growth fit analysis saved as tab-delimited text file in: '",
                   wd, "/results.gc.txt'"))

    # Export grouped results table
    if(("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ){
      table_linear_group <- table_group_growth_linear(res.table.gc)
      names <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table_linear_group))))
      table_linear_group <- as.data.frame(lapply(1:ncol(table_linear_group), function(x) gsub("<strong>", "", gsub("</strong>", "", table_linear_group[,x]))))
      colnames(table_linear_group) <- names
      export_Table(table = table_linear_group, out.dir = wd, out.nm = "grouped_results_linear")
    }

    if(("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ){
      table_spline_group <- table_group_growth_spline(res.table.gc)
      names <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table_spline_group))))
      table_spline_group <- as.data.frame(lapply(1:ncol(table_spline_group), function(x) gsub("<strong>", "", gsub("</strong>", "", table_spline_group[,x]))))
      colnames(table_spline_group) <- names
      export_Table(table = table_spline_group, out.dir = wd, out.nm = "grouped_results_spline")
    }

    if(("m" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ){
      table_model_group <- table_group_growth_model(res.table.gc)
      names <- gsub("<sub>", "_", gsub("</sub>|<sup>|</sup>", "", gsub("<br>", " ", colnames(table_model_group))))
      table_model_group <- as.data.frame(lapply(1:ncol(table_model_group), function(x) gsub("<strong>", "", gsub("</strong>", "", table_model_group[,x]))))
      colnames(table_model_group) <- names
      export_Table(table = table_model_group, out.dir = wd, out.nm = "grouped_results_model")
    }
    #   # export table
    #   utils::write.table(combined.df, paste(wd, "mean_results.gc.txt",
    #                                          sep = "/"), row.names = FALSE, sep = "\t")
    #   cat(paste0("Per-group average results of growth fit analysis saved as tab-delimited text file in:\n",
    #              wd, "/mean_results.gc.txt\n\n"))

    if (ec50 == TRUE) {
      res.table.dr <- Filter(function(x) !all(is.na(x)),EC50.table)
      export_Table(table = res.table.dr, out.dir = wd, out.nm = "results.dr")
      cat(paste0("Results of EC50 analysis saved as tab-delimited text file in:\n'",
                 wd, "/results.dr.txt'\n"))
    } else {
      res.table.dr <- NULL
    }
    # Export RData object
    export_RData(grofit, out.dir = wd)

    if(any(report %in% c('pdf', 'html'))){
      try(growth.report(grofit, out.dir = gsub(paste0(getwd(), "/"), "", wd), ec50 = ec50, mean.grp = mean.grp, mean.conc = mean.conc,
                        export = export.fig, format = report, out.nm = out.nm))
    }
  } # if(!exists("shiny") || shiny != TRUE)

  grofit
}

#' Create a PDF and HTML report with results from a growth curve analysis workflow
#'
#' \code{growth.report} requires a \code{grofit} object and creates a report in PDF and HTML format that summarizes all results.
#'
#' @param grofit A \code{grofit} object created with \code{\link{growth.workflow}}.
#' @param out.dir (Character) The path or name of the folder in which the report files are created.  If \code{NULL}, the folder will be named with a combination of "Report.growth_" and the current date and time.
#' @param out.nm {Character or \code{NULL}} Define the name of the report files. If \code{NULL}, the files will be named with a combination of "GrowthReport_" and the current date and time.
#' @param ec50 (Logical) Was a dose-response analysis performed in \code{\link{growth.workflow}} \code{TRUE} or not \code{FALSE}?
#' @param export (Logical) Shall all plots generated in the report be exported as individual PDF and PNG files \code{TRUE} or not \code{FALSE}?
#' @param ... Further arguments passed to create a report. Currently supported:
#' \itemize{
#'    \item \code{mean.grp}: Define groups to combine into common plots in the report based on sample identifiers. Partial matches with sample/group names are accepted. Can be \code{"all"}, a string vector, or a list of string vectors. Note: The maximum number of sample groups (with unique condition/concentration indicators) is 50. If you have more than 50 groups, option \code{"all"} will produce the error \code{! Insufficient values in manual scale. [Number] needed but only 50 provided}.
#'    \item \code{mean.conc}: Define concentrations to combine into common plots in the  report. Can be a numeric vector, or a list of numeric vectors.
#' }
#' @param ec50 (Logical) Display results of dose-response analysis (\code{TRUE}) or not (\code{FALSE}).
#' @param format (Character) Define the file format for the report, PDF (\code{'pdf'}) and/or HTML (\code{'html'}). Default: (\code{c('pdf', 'html')})
#'
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
#' @importFrom foreach %dopar%
#' @importFrom DT datatable
#' @importFrom stats AIC as.formula coef fitted.values formula integrate lm loess lowess median nls predict sd setNames smooth.spline terms time
#' @importFrom magrittr %>%
#' @import knitr
#' @importFrom plyr rbind.fill
#' @importFrom kableExtra kable_styling column_spec linebreak
#' @include general_misc_utils.R
#' @family reports
growth.report <- function(grofit, out.dir = NULL, out.nm = NULL, ec50 = FALSE, format = c('pdf', 'html'), export = FALSE, ...)
  {
  try(showModal(modalDialog("Rendering report...\n(This can take up to several minutes)", footer=NULL)), silent = TRUE)
  # results an object of class grofit
  if(methods::is(grofit) != "grofit") stop("grofit needs to be an object created with growth.workflow().")

  # Define objects based on additional function calls
  call <- match.call()
  ## remove strictly defined arguments
  call$grofit <- call$out.dir <- call$out.nm <- call$ec50 <- call$format <- call$export <- NULL

  arglist <- sapply(call, function(x) x)
  arglist <- unlist(arglist)[-1]
  ## Assign additional arguments (...) as R objects
  if(length(arglist) > 0){
    for(i in 1:length(arglist)){
      assign(names(arglist)[i], arglist[[i]])
    }
  }

  if(!exists("mean.grp") || mean.grp==mean.grp) mean.grp <- NA
  if(!exists("mean.conc") || mean.conc==mean.conc) mean.conc <- NA

  gcFit <- grofit$gcFit
  drFit <- grofit$drFit
  control <- grofit$control
  time <- grofit$gcFit$raw.time
  data <- grofit$gcFit$raw.data
  if(!exists("res.table.gc")){
    res.table.gc <- grofit$gcFit$gcTable
  }
  if(!exists("res.table.dr")){
    if(length(grofit$drFit) >1 ) res.table.dr <- grofit$drFit$drTable
  }
  if(any(c("a", "b", "s") %in% grofit$control$fit.opt)){
    # find minimum and maximum mu values in whole dataset to equilibrate derivative plots for spline fits
    mu.min <- suppressWarnings(min(sapply(1:length(grofit$gcFit$gcFittedSplines), function(x) ifelse(all(is.na(grofit$gcFit$gcFittedSplines[[x]]$spline.deriv1)), NA, min(grofit$gcFit$gcFittedSplines[[x]]$spline.deriv1$y))), na.rm = TRUE))*1.05
    if(mu.min >0) mu.min <- 0
    mu.max <- suppressWarnings(max(sapply(1:length(grofit$gcFit$gcFittedSplines), function(x) ifelse(all(is.na(grofit$gcFit$gcFittedSplines[[x]]$spline.deriv1)), NA, max(grofit$gcFit$gcFittedSplines[[x]]$spline.deriv1$y))), na.rm = TRUE))*1.05
  }
  if(!is.null(out.dir)){
    wd <-  out.dir
  } else {
    wd <- paste(getwd(), "/Report.growth_", format(Sys.time(),
                                                   "%Y%m%d_%H%M%S"), sep = "")
  }
  if(is.null(out.nm)){
    out.nm <- paste("/GrowthReport_", format(Sys.time(),
                                                   "%Y%m%d_%H%M%S"), sep = "")
  }
  dir.create(wd, showWarnings = F)

  message("Render reports...")

  for(i in 1:length(.libPaths())){
    QurvE.ndx <- grep("QurvE", list.files(.libPaths()[i]))
    if(length(QurvE.ndx)>0){
      Report.wd <- paste0(.libPaths()[i], "/QurvE")
    }
  }
  file <- paste0(Report.wd, "/Report_Growth.Rmd")
  if(all(c('pdf', 'html') %in% format)){
    format <- c('html_document', 'pdf_document')
  } else if ('pdf' %in% format){
    format <- 'pdf_document'
  } else if ('html' %in% format){
    format <- 'html_document'
  } else {
    stop("Please define a valid report format, either 'pdf', 'html', or c('pdf', 'html').")
  }
  rmarkdown::render(file, output_format = format, output_dir = wd, output_file = out.nm,
                    quiet = TRUE)
  message(paste0("Report files saved in: '/", wd, "'"))
  unlink(paste0(tempdir(), "/Plots"), recursive = TRUE)
  try(removeModal(), silent = TRUE)
}


#' Perform a growth curve analysis on all samples in the provided dataset.
#'
#' \code{growth.gcFit} performs all computational growth fitting operations based on the
#' user input.
#'
#' @param time (optional) A matrix containing time values for each sample.
#' @param data  Either... \enumerate{ \item a \code{grodata} object created with \code{\link{read_data}} or \code{\link{parse_data}},
#'   \item a list containing a \code{'time'} matrix as well as \code{'density'} and, if appropriate, a \code{'fluorescence'} dataframes,
#'   or \item a dataframe containing density values (if a \code{time} matrix is provided as separate argument).}
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}},
#'   defining relevant fitting options.
#' @param ... Further arguments passed to the shiny app.
#'
#' @return A \code{gcFit} object that contains all growth fitting results, compatible with
#'   various plotting functions of the QurvE package.
#' \item{raw.time}{Raw time matrix passed to the function as \code{time}.}
#' \item{raw.data}{Raw density dataframe passed to the function as \code{data}.}
#' \item{gcTable}{Table with growth parameters and related statistics for each growth curve evaluation performed by the function. This table, which is also returned by the generic \code{summary.gcFit} method applied to a \code{gcFit} object, is used as an input for \code{\link{growth.drFit}}.}
#' \item{gcFittedLinear}{List of all \code{gcFitLinear} objects, generated by the call of \code{\link{growth.gcFitLinear}}. Note: access to each object in the list via double brace: gcFittedLinear\[\[#n\]\].}
#' \item{gcFittedModels}{List of all \code{gcFitModel} objects, generated by the call of \code{\link{growth.gcFitModel}}. Note: access to each object in the list via double brace: gcFittedModels\[\[#n\]\].}
#' \item{gcFittedSplines}{List of all \code{gcFitSpline} objects, generated by the call of \code{\link{growth.gcFitSpline}}. Note: access to each object via double brace: gcFittedSplines\[\[#n\]\].}
#' \item{gcBootSplines}{List of all \code{gcBootSpline} objects, generated by the call of \code{\link{growth.gcBootSpline}}. Note: access to each object via double brace: gcFittedSplines\[\[#n\]\].}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#'
#' @family workflows
#' @family growth fitting functions
#' @family dose-response analysis functions
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text
#'   geom_bar geom_errorbar geom_line geom_point geom_ribbon geom_segment ggplot
#'   ggplot_build ggplot ggtitle labs position_dodge scale_color_manual scale_fill_brewer
#'   scale_color_brewer scale_fill_manual scale_x_continuous scale_y_continuous
#'   scale_y_log10 theme theme_classic theme_minimal xlab ylab
#' @import foreach
growth.gcFit <- function(time, data, control= growth.control(), ...)
{
  # Define objects based on additional function calls
  call <- match.call()

  ## remove strictly defined arguments
  call$time <- call$data <- call$control <- NULL


  arglist <- sapply(call, function(x) x)
  arglist <- unlist(arglist)[-1]
  ## Assign additional arguments (...) as R objects
  if(length(arglist) > 0){
    for(i in 1:length(arglist)){
      assign(names(arglist)[i], arglist[[i]])
    }
  }

  if(!(any(is(data)=="grodata"))){
    if (is.numeric(as.matrix(time)) == FALSE)
      stop("Need a numeric matrix for 'time' or a grodata object created with read_data() or parse_data().")
    if (is.numeric(as.matrix(data[-1:-3])) == FALSE)
      stop("Need a numeric matrix for 'data' or a grodata object created with read_data() or parse_data().")
  } else if(!is.null(data)){
    time <- data$time
    data <- data$density
  }
  # /// check if start density values are above min.density in all samples
  max.density <- unlist(lapply(1:nrow(data), function (x) max(as.numeric(as.matrix(data[x,-1:-3]))[!is.na(as.numeric(as.matrix(data[x,-1:-3])))])))
  if(is.numeric(control$min.density) && control$min.density != 0){
    if(!is.na(control$min.density) && all(as.numeric(max.density) < control$min.density)){
      stop(paste0("The chosen global start density value (min.density) is larger than every value in your dataset.\nThe maximum value in your dataset is: ",
                  max(as.numeric(max.density))))
    }
  }
  # /// check input parameters
  if (methods::is(control)!="grofit.control") stop("control must be of class grofit.control!")

  # /// check number of datasets
  if ( (dim(time)[1])!=(dim(data)[1]) ) stop("gcFit: Different number of datasets in data and time")

  # /// check fitting options
  if (!all(control$fit.opt %in% c("s","m","a", "l"))){
    options(warn=1)
    warning("fit.opt must contain 's', 'm', 'l', or 'a'. Changed to 'a' (all fit methods)!")
    fit.opt="a"
    options(warn=0)
  }

  # /// Initialize some parameters
  out.table       <- NULL
  used.model      <- NULL
  fitpara.all     <- list()
  fitnonpara.all  <- list()
  fitlinear.all <- list()
  boot.all        <- list()
  fitted.param    <- NULL
  fitted.nonparam <- NULL
  bootstrap.param <- NULL
  reliability_tag_linear <- NA
  reliability_tag_param <- NA
  reliability_tag_nonpara <- NA

  if(control$interactive == FALSE &&
     dim(data)[1] > 30 &&
     (
       ("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ||
       ("s" %in% control$fit.opt && control$nboot.gc > 0)
     )
  ){
    times.ls    <- lapply(1:nrow(time), function(x) time[x, ][!is.na(time[x, ])][!is.na(data[x, -1:-3])])
    wells.ls <- lapply(1:nrow(data), function(x) as.numeric(data[x, -1:-3][!is.na(time[x, ])][!is.na(data[x, -1:-3])]))
    gcIDs.ls    <- lapply(1:nrow(data), function(x) as.matrix(data[x, 1:3]))
    wellnames.ls <- lapply(1:nrow(data), function(x) paste(as.character(data[x,1]), as.character(data[x,2]),as.character(data[x,3]), sep=" | "))

    # Set up computing clusters (all available processor cores - 1)
    cl <- parallel::makeCluster(parallel::detectCores(all.tests = FALSE, logical = TRUE)-1)
    doParallel::registerDoParallel(cl)

    # Perform linear fits in parallel
    if (("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
      fitlinear.all <- foreach::foreach(i = 1:dim(data)[1]
      ) %dopar% {
        QurvE::growth.gcFitLinear(times.ls[[i]], wells.ls[[i]], gcID = gcIDs.ls[[i]], control = control)
      }
    } else {
      # /// generate list with empty objects
      fitlinear.all <- lapply(1:nrow(data), function(x) list(raw.time = times.ls[[x]],
                                                             raw.data = wells.ls[[x]],
                                                             filt.time = NA,
                                                             filt.data = NA,
                                                             log.data = NA,
                                                             gcID = gcIDs.ls[[x]],
                                                             FUN = NA,
                                                             fit = NA,
                                                             par = c(y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                                                               t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                                                               tmax2_end = NA),
                                                             ndx = NA, ndx2 = NA,
                                                             quota = NA,
                                                             rsquared = NA, rsquared2 = NA,
                                                             control = control,
                                                             fitFlag = FALSE, fitFlag2 = FALSE)
                              )
    }

    # # Perform model fits in parallel
    # if (("m" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
    #   fitpara.all <- foreach::foreach(i = 1:dim(data)[1]
    #   ) %dopar% {
    #     QurvE::growth.gcFitModel(times.ls[[i]], wells.ls[[i]], gcID = gcIDs.ls[[i]], control = control)
    #   }
    # } else {
    #   # /// generate list with empty objects
    #   fitpara.all <- lapply(1:nrow(data), function(x) list(time.in = times.ls[[x]],
    #                                                        data.in = wells.ls[[x]],
    #                                                        raw.time = times.ls[[x]],
    #                                                        raw.data = wells.ls[[x]],
    #                                                        gcID = gcIDs.ls[[x]],
    #                                                        fit.time = NA,
    #                                                        fit.data = NA,
    #                                                        parameters = list(A=NA, mu=NA, lambda=NA, integral=NA),
    #                                                        model = NA,
    #                                                        nls = NA,
    #                                                        reliable=NULL,
    #                                                        fitFlag=FALSE,
    #                                                        control = control)
    #                         )
    # }
    #
    # # Perform spline fits in parallel
    # if (("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
    #   fitnonpara.all <- foreach::foreach(i = 1:dim(data)[1]
    #   ) %dopar% {
    #     QurvE::growth.gcFitSpline(times.ls[[i]], wells.ls[[i]], gcID = gcIDs.ls[[i]], control = control)
    #   }
    # } else {
    #   # /// generate list with empty objects
    #   fitnonpara.all <- lapply(1:nrow(data), function(x) list(raw.time = times.ls[[x]],
    #                                                           raw.data = wells.ls[[x]],
    #                                                           gcID = gcIDs.ls[[x]],
    #                                                           fit.time = NA, fit.data = NA,
    #                                                           parameters = list(A = NA, dY = NA, mu = NA, t.max = NA, lambda = NA, b.tangent = NA, mu2 = NA, t.max2 = NA,
    #                                                                             lambda2 = NA, b.tangent2 = NA, integral = NA),
    #                                                           spline = NA,
    #                                                           parametersLowess = list(A = NA, mu = NA, lambda = NA),
    #                                                           reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
    #                                                           control = control)
    #                            )
    #
    # }

    # Perform spline bootstrappings in parallel
    if ((("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ) &&
        (control$nboot.gc > 10) ){
      boot.all <- foreach::foreach(i = 1:dim(data)[1]
      ) %dopar% {
        QurvE::growth.gcBootSpline(times.ls[[i]], wells.ls[[i]], gcIDs.ls[[i]], control)
      }
    }
    else{
      # /// create empty gcBootSpline  object
      boot.all            <- lapply(1:nrow(data), function(x) list(raw.time=times.ls[[x]],
                                                                   raw.data=wells.ls[[x]],
                                                                   gcID =gcIDs.ls[[x]],
                                                                   boot.x=NA,
                                                                   boot.y=NA,
                                                                   boot.gcSpline=NA,
                                                                   lambda=NA, mu=NA, A=NA, integral=NA,
                                                                   bootFlag=FALSE, control=control
                                                                   )
      )
    }
    parallel::stopCluster(cl = cl)

    # Assign classes to list elements
    for(i in 1:length(fitlinear.all)){
      class(fitlinear.all[[i]]) <- "gcFitLinear"
    }
    # for(i in 1:length(fitpara.all)){
    #   class(fitpara.all[[i]]) <- "gcFitModel"
    # }
    # for(i in 1:length(fitnonpara.all)){
    #   class(fitnonpara.all[[i]]) <- "gcFitSpline"
    # }
    for(i in 1:length(boot.all)){
      class(boot.all[[i]]) <- "gcBootSpline"
    }
  }

  reliability_tag <- c()
  # /// loop over all wells
    for (i in 1:dim(data)[1]){
      # Progress indicator for shiny app
      if(exists("shiny") && shiny == TRUE){
        shiny::incProgress(
          amount = 1/(dim(data)[1]),
          message = "Computations completed")
      }
      # /// conversion, to handle even data.frame inputs
      acttime    <-
        as.numeric(as.matrix(time[i, ]))[!is.na(as.numeric(as.matrix(time[i, ])))][!is.na(as.numeric(as.matrix((data[i, -1:-3]))))]
      actwell <-
        as.numeric(as.matrix((data[i, -1:-3])))[!is.na(as.numeric(as.matrix(time[i, ])))][!is.na(as.numeric(as.matrix((data[i, -1:-3]))))]

      gcID    <- as.matrix(data[i,1:3])
      wellname <- paste(as.character(data[i,1]), as.character(data[i,2]),as.character(data[i,3]), sep=" | ")
      if ((control$suppress.messages==FALSE)){
        cat("\n\n")
        cat(paste("=== ", as.character(i), ". [", wellname, "] growth curve =================================\n", sep=""))
        cat("----------------------------------------------------\n")
      }
      if(control$interactive == TRUE ||
         dim(data)[1] <= 30 ||
         !("l" %in% control$fit.opt || "a" %in% control$fit.opt || ("s" %in% control$fit.opt && control$nboot.gc > 10))
         ){
        # /// Linear regression on log-transformed data
        if (("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
          fitlinear          <- growth.gcFitLinear(acttime, actwell, gcID = gcID, control = control)
          fitlinear.all[[i]] <- fitlinear
        }
        else{
          # /// generate empty object
          fitlinear          <- list(raw.time = acttime, raw.data = actwell, filt.time = NA, filt.data = NA,
                                     log.data = NA, gcID = gcID, FUN = NA, fit = NA, par = c(
                                       y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                                       t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                                       tmax2_end = NA), ndx = NA, ndx2 = NA, quota = NA, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE)
          class(fitlinear)   <- "gcFitLinear"
          fitlinear.all[[i]] <- fitlinear
        }
        # /// plot linear fit
        if ((control$interactive == TRUE)) {
          if (("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt)) {
            answer_satisfied <- "n"
            reliability_tag_linear <- NA
            while ("n" %in% answer_satisfied) {
              if(control$log.y.lin){
                try(plot(fitlinear, log = "y"))
              } else {
                try(plot(fitlinear, log = ""))
              }
              graphics::mtext(side = 3, line = 3, adj = 0,
                    outer = F,
                    cex = 1,
                    wellname)
              answer_satisfied <- readline("Are you satisfied with the linear fit (y/n)?\n\n")
              if ("n" %in% answer_satisfied) {
                test_answer <- readline("Enter: t0, h, quota, min.density, R2, RSD                         >>>>\n\n [Skip (enter 'n'), or adjust fit parameters (see ?growth.gcFitLinear).\n Leave {blank} at a given position if standard parameters are desired.]\n\n")
                if ("n" %in% test_answer) {
                  cat("\n Tagged the linear fit of this sample as unreliable !\n\n")
                  reliability_tag_linear              <- FALSE
                  fitlinear$reliable <- FALSE
                  fitlinear.all[[i]]$reliable    <- FALSE
                  answer_satisfied <- "y"
                } # end if ("n" %in% test_answer)
                else {
                  new_params <- unlist(strsplit(test_answer, split = ","))
                  t0_new <- ifelse(!is.na(as.numeric(new_params[1])), as.numeric(new_params[1]), control$t0)
                  h_new <- ifelse(!is.na(as.numeric(new_params[2])), as.numeric(new_params[2]), control$lin.h)
                  quota_new <- ifelse(!is.na(as.numeric(new_params[3])), as.numeric(new_params[3]), 0.95)
                  min.density_new <- ifelse(!is.na(as.numeric(new_params[4])), as.numeric(new_params[4]), control$min.density)
                  R2_new <- ifelse(!is.na(as.numeric(new_params[5])), as.numeric(new_params[5]), control$lin.R2)
                  RSD_new <- ifelse(!is.na(as.numeric(new_params[6])), as.numeric(new_params[6]), control$lin.RSD)
                  control_new <- control
                  control_new$t0 <- t0_new
                  if(!is.na(h_new)) control_new$lin.h <- h_new
                  control_new$lin.R2 <- R2_new
                  control_new$lin.RSD <- RSD_new
                  if(is.numeric(min.density_new)){
                    if(!is.na(min.density_new) && all(as.vector(actwell) < min.density_new)){
                      message(paste0("Start density values need to be greater than 'min.density'.\nThe minimum start value in your dataset is: ",
                                     min(as.vector(actwell)),". 'min.density' was not adjusted."), call. = FALSE)
                    } else if(!is.na(min.density_new)){
                      control_new$min.density <- min.density_new
                    }
                  }
                  fitlinear <- growth.gcFitLinear(acttime, actwell, gcID = gcID, control = control_new, quota = quota_new)
                  fitlinear.all[[i]] <- fitlinear
                } #end else
              } # end if ("n" %in% test_answer)
              else{
                reliability_tag_linear <- TRUE
                fitlinear$reliable <- TRUE
                fitlinear.all[[i]]$reliable <- TRUE
                cat("Sample was (more or less) o.k.\n")
              } # end else
            } # end while ("n" %in% answer_satisfied)
          } # end if (("l" %in% control$fit.opt) || ("a"  %in% control$fit.opt))
        } # end if ((control$interactive == TRUE))
        else {
          reliability_tag_linear <- TRUE
          fitlinear$reliable <- TRUE
          fitlinear.all[[i]]$reliable <- TRUE
        }
      } # control$interactive == TRUE || dim(data)[1] <= 30
      # /// Parametric fit
      if (("m" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
        fitpara          <- growth.gcFitModel(acttime, actwell, gcID, control)
        fitpara.all[[i]] <- fitpara
      }
      else{
        # /// generate empty object
        fitpara          <- list(time.in =  acttime, data.in = actwell, raw.time = acttime,
                                 raw.data = actwell, gcID = gcID, fit.time = NA, fit.data = NA,
                                 parameters = list(A=NA, mu=NA, tD = NA, lambda=NA, integral=NA),
                                 model = NA, nls = NA, reliable=NULL, fitFlag=FALSE, control = control)
        class(fitpara)   <- "gcFitModel"
        fitpara.all[[i]] <- fitpara
      }

      # /// Non parametric fit
      if (("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt)){
        nonpara             <- growth.gcFitSpline(acttime, actwell, gcID, control)
        fitnonpara.all[[i]] <- nonpara
      }
      else{
        # /// generate empty object
        nonpara             <- list(raw.time = acttime, raw.data = actwell, gcID = gcID, fit.time = NA, fit.data = NA,
                                    parameters = list(A = NA, dY = NA, mu = NA, t.max = NA, lambda = NA, b.tangent = NA, mu2 = NA, t.max2 = NA,
                                                      lambda2 = NA, b.tangent2 = NA, integral = NA),
                                    parametersLowess = list(A = NA, mu = NA, lambda = NA),
                                    spline = NA, spline.deriv1 = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                                    control = control)
        class(nonpara)      <- "gcFitSpline"
        fitnonpara.all[[i]] <- nonpara
      }
      # /// plotting parametric fit
      if ((control$interactive == TRUE)) {
        if ((("m" %in% control$fit.opt) || ("a"  %in% control$fit.opt))) {
          if (fitpara$fitFlag == TRUE) {
            plot.gcFitModel(fitpara, colData=1, colModel=2, colLag = 3, cex.point=2.0, raw=T)
            # legend(x="bottomright", legend=fitpara$model, col="red", lty=1)
            # title("Parametric fit")
            # graphics::mtext(line = 0.5, side=3, outer = F, cex=1, wellname)
            }
          # /// here a manual reliability tag is set in the interactive mode
          reliability_tag_param <- NA
          answer <- readline("Are you satisfied with the model fit (y/n)?\n\n")
          if ("n" %in% answer) {
            cat("\n Tagged the parametric fit of this sample as unreliable !\n\n")
            reliability_tag_param              <- FALSE
            fitpara$reliable <- FALSE
            fitpara.all[[i]]$reliable    <- FALSE
          }
          else{
            reliability_tag_param <- TRUE
            fitpara$reliable <- TRUE
            fitpara.all[[i]]$reliable    <- TRUE
            cat("Sample was (more or less) o.k.\n")
          }
        } # if ((("m" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ) && fitpara$fitFlag == TRUE)
        else {
          reliability_tag_param <- FALSE
          fitpara$reliable <- FALSE
          fitpara.all[[i]]$reliable    <- FALSE
        }
        # /// plotting nonparametric fit
        if (("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt)) {
          if (nonpara$fitFlag == TRUE) {
            answer_satisfied <- "n"
            reliability_tag_nonpara <- NA
            while ("n" %in% answer_satisfied) {
              if(control$log.y.spline){
                plot.gcFitSpline(nonpara, add=FALSE, raw=TRUE, slope = T, colData=1, cex.point = 2, plot=T, export=F)
              } else {
                plot.gcFitSpline(nonpara, add=FALSE, raw=TRUE, slope = T, log.y = FALSE, colData=1, cex.point = 2, plot=T, export=F)
              }
              answer_satisfied <- readline("Are you satisfied with the spline fit (y/n)?\n\n")
              if ("n" %in% answer_satisfied) {
                    test_answer <- readline("Enter: smooth.gc, t0, min.density                                        >>>> \n\n [Skip (enter 'n'), or smooth.gc, t0, and min.density (see ?growth.control).\n Leave {blank} at a given position if standard parameters are desired.]\n\n ")
                    if ("n" %in% test_answer) {
                      cat("\n Tagged the linear fit of this sample as unreliable !\n\n")
                      reliability_tag_nonpara              <- FALSE
                      nonpara$reliable <- FALSE
                      fitnonpara.all[[i]]$reliable    <- FALSE
                      fitnonpara.all[[i]]$FitFlag    <- FALSE
                      answer_satisfied <- "y"
                    } # end if ("n" %in% test_answer)
                    else{
                    new_params <- unlist(strsplit(test_answer, split = ","))
                    if(!is.na(as.numeric(new_params[2])) && as.numeric(new_params[2]) != ""){
                      t0_new <- as.numeric(new_params[2])
                    } else {
                      t0_new <- control$t0
                    }
                    smooth.gc_new <- as.numeric(new_params[1])
                    control_new <- control
                    if(!is.na(smooth.gc_new) && smooth.gc_new != ""){
                      control_new$smooth.gc <- smooth.gc_new
                    }
                    control_new$t0 <- t0_new
                    min.density_new <- as.numeric(new_params[3])
                    if(!is.na(min.density_new)){
                      if(is.numeric(min.density_new) && min.density_new != 0 && all(as.vector(actwell) < min.density_new)){
                        message(paste0("Start density values need to be below 'min.density'.\nThe minimum start value in your dataset is: ",
                                       min(as.vector(data[,4])),". 'min.density' was not adjusted."), call. = FALSE)
                      } else if(!is.na(min.density_new)){
                        control_new$min.density <- min.density_new
                      }
                    }
                    nonpara <- growth.gcFitSpline(acttime, actwell, gcID, control_new)
                    fitnonpara.all[[i]] <- nonpara
                  } #end else
                } # end if ("n" %in% answer_satisfied)
                else{
                  reliability_tag_nonpara <- TRUE
                  nonpara$reliable <- TRUE
                  fitnonpara.all[[i]]$reliable <- TRUE
                  fitnonpara.all[[i]]$FitFlag <- TRUE
                  cat("Sample was (more or less) o.k.\n")
                } # end else
              } # end while ("n" %in% answer_satisfied)
            } # end if (nonpara$fitFlag == TRUE)
          } # end if (("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt) )
      } # end of if((control$interactive == TRUE))
      else{
        reliability_tag_param <- TRUE
        reliability_tag_nonpara <- TRUE
        nonpara$reliable <- TRUE
        fitpara$reliable <- TRUE
        fitnonpara.all[[i]]$reliable <- TRUE
        fitpara.all[[i]]$reliable <- TRUE
      }
      if(control$interactive == TRUE ||
         dim(data)[1] <= 30 ||
         !("l" %in% control$fit.opt || "a" %in% control$fit.opt || ("s" %in% control$fit.opt && control$nboot.gc > 10))
      ){
        # /// Beginn Bootstrap
        if ((("s" %in% control$fit.opt) || ("a"  %in% control$fit.opt) ) &&
            (control$nboot.gc > 0) && (reliability_tag_nonpara ==TRUE) && nonpara$fitFlag == TRUE){
          bt            <- growth.gcBootSpline(acttime, actwell, gcID, control)
          boot.all[[i]] <- bt
        } # /// end of if (control$nboot.gc ...)
        else{
          # /// create empty gcBootSpline  object
          bt            <- list(raw.time=acttime, raw.data=actwell, gcID =gcID, boot.x=NA, boot.y=NA, boot.gcSpline=NA,
                                lambda=NA, mu=NA, A=NA, integral=NA, bootFlag=FALSE, control=control)
          class(bt)     <- "gcBootSpline"
          boot.all[[i]] <- bt
        }
      } # if(interactive == TRUE || 1:dim(data)[1] <= 30 ||
      reliability_tag <- c(reliability_tag, any(reliability_tag_linear, reliability_tag_nonpara, reliability_tag_param))
      # create output table
      # description     <- data.frame(TestId=data[i,1], AddId=data[i,2],concentration=data[i,3],
      #                               reliability_tag=reliability_tag, used.model=fitpara$model,
      #                               log.x=control$log.x.gc, log.y=control$log.y.spline, nboot.gc=control$nboot.gc)

      # fitted          <- cbind(description, summary.gcFitLinear(fitlinear), summary.gcFitModel(fitpara), summary.gcFitSpline(nonpara), summary.gcBootSpline(bt))

      # out.table       <- rbind(out.table, fitted)
      # class(out.table) <- c("data.frame", "gcTable")

    } # /// end of for (i in 1:dim(data)[1])
  # Assign names to list elements
  names(fitlinear.all) <- names(fitpara.all) <- names(fitnonpara.all) <- names(boot.all) <-  paste0(as.character(data[,1]), " | ", as.character(data[,2]), " | ", as.character(data[,3]))

  # create output table
  description     <- lapply(1:nrow(data), function(x) data.frame(TestId = data[x,1], AddId = data[x,2],concentration = data[x,3],
                                                                 reliability_tag = reliability_tag[x],
                                                                 used.model = ifelse(is.null(fitpara.all[[x]]$model), NA, fitpara.all[[x]]$model),
                                                                 log.x = control$log.x.gc,
                                                                 log.y.lin = control$log.y.lin,
                                                                 log.y.spline = control$log.y.spline,
                                                                 log.y.model = control$log.y.model,
                                                                 nboot.gc = control$nboot.gc
                                                                 )
                            )

  fitted          <- lapply(1:length(fitlinear.all), function(x) cbind(description[[x]],
                                                                       summary.gcFitLinear(fitlinear.all[[x]]),
                                                                       summary.gcFitModel(fitpara.all[[x]]),
                                                                       summary.gcFitSpline(fitnonpara.all[[x]]),
                                                                       summary.gcBootSpline(boot.all[[x]])
                                                                       )
  )

  out.table       <- do.call(rbind, fitted)
  class(out.table) <- c("data.frame", "gcTable")
  # Combine results into list 'gcFit'
  gcFit           <- list(raw.time = time, raw.data = data, gcTable = out.table, gcFittedLinear = fitlinear.all, gcFittedModels = fitpara.all, gcFittedSplines = fitnonpara.all, gcBootSplines = boot.all, control=control)
  class(gcFit)    <- "gcFit"
  return(gcFit)
  # names(fitlinear.all) <- names(fitpara.all) <- names(fitnonpara.all) <- names(boot.all) <- paste0(as.character(data[,1]), " | ", as.character(data[,2]), " | ", as.character(data[,3]))
  # out.table <- data.frame(as.matrix(out.table))
  # gcFit           <- list(raw.time = time, raw.data = data, gcTable = out.table, gcFittedLinear = fitlinear.all, gcFittedModels = fitpara.all, gcFittedSplines = fitnonpara.all, gcBootSplines = boot.all, control=control)
  #
  # class(gcFit)    <- "gcFit"
  # gcFit
}

#' Fit nonlinear growth models to density data
#'
#' \code{growth.gcFitModel} determines a parametric growth model that best describes the data.
#'
#' @param time Vector of the independent variable (usually time).
#' @param data Vector of dependent variable (usually density values).
#' @param gcID (Character) The name of the analyzed sample.
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#'
#' @return A \code{gcFitModel} object that contains physiological parameters and information about the best fit. Use \code{\link{plot.gcFitModel}} to visualize the parametric fit and growth equation.
#' \item{raw.time}{Raw time values provided to the function as \code{time}.}
#' \item{raw.data}{Raw density data provided to the function as \code{data}.}
#' \item{gcID}{(Character) Identifies the tested sample.}
#' \item{fit.time}{Fitted time values.}
#' \item{fit.data}{Fitted density values.}
#' \item{parameters}{List of determined growth parameters.}
#' \itemize{
#' \item \code{A}: {Maximum density.}
#' \item \code{dY}: {Difference in maximum density and minimum density of the fitted model.}
#' \item \code{mu}: {Maximum growth rate (i.e., maximum in first derivative of the spline).}
#' \item \code{lambda}: {Lag time.}
#' \item \code{b.tangent}: {Intersection of the tangent at the maximum growth rate with the abscissa.}
#' \item \code{fitpar}: {For some models: list of additional parameters used in the equations describing the growth curve.}
#' \item \code{integral}: {Area under the curve of the parametric fit.}
#' }
#' \item{model}{(Character) The model that obtained the fit with the lowest AIC, determined by \code{\link{AIC}}.}
#' \item{nls}{\code{nls} object for the chosen model generated by the \code{\link{nls}} function.}
#' \item{reliable}{(Logical) Indicates whether the performed fit is reliable (to be set manually).}
#' \item{fitFlag}{(Logical) Indicates whether a parametric model was successfully fitted on the data.}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @family growth fitting functions
#'
#' @export
#'
growth.gcFitModel <- function(time, data, gcID ="undefined", control=growth.control())
{
  # /// check input parameters
  if (methods::is(control)!="grofit.control") stop("control must be of class grofit.control!")
  if (!any(c("m","a") %in% control$fit.opt)) stop("Fit option is not set for a model fit. See growth.control()")

  # /// conversion to handle even data.frame inputs
  time <- as.vector(as.numeric(as.matrix(time)))[!is.na(time)][!is.na(data)]
  data    <- as.vector(as.numeric(as.matrix(data)))[!is.na(time)][!is.na(data)]

  if(length(data[data<0]) > 0){
    data <- data + abs(min(data[data<0]))+0.01 # add the absolute value of the minimum negative density (+ 0.01) to the data
  }

  # /// check length of input data
  if (length(time)!=length(data)) stop("gcFitModel: length of time and data input vectors differ!")
  if(max(data) < control$growth.thresh * data[1]){
    if(control$suppress.messages==F) message(paste0("Parametric fit: No significant growth detected (with all values below ", control$growth.thresh, " * start_value)."))
    gcFitModel   <- list(time.in =  time, data.in = data, raw.time = time, raw.data = data, gcID = gcID, fit.time = NA,
                         fit.data = NA, parameters = list(A=NA, mu=0, tD = NA, lambda=NA, integral=NA),
                         model = NA, nls = NA, reliable=NULL, fitFlag=FALSE, control = control)
    class(gcFitModel) <- "gcFitModel"
    return(gcFitModel)
  }
  # /// check if there are enough data points
  if (length(data)<5){
    warning("gcFitModel: There is not enough valid data. Must have at least 5 unique values!")
    gcFitModel <- list(time.in =  time, data.in = data, raw.time = time, raw.data = data, gcID = gcID, fit.time = NA,
                         fit.data = NA, parameters = list(A=NA, mu=NA, tD = NA, lambda=NA, integral=NA),
                         model = NA, nls = NA, reliable=NULL, fitFlag=FALSE, control = control)
    class(gcFitModel) <- "gcFitModel"
    return(gcFitModel)
  }
  else{
      gcFitModel <- grofit.param(time, data, gcID, control)
  }
  return(gcFitModel)
}

#' Internal function called within \code{\link{growth.gcFitModel}}.
#'
#' @param time Vector of the independent variable (usually time).
#' @param data Vector of dependent variable (usually density values).
#' @param gcID (Character) The name of the analyzed sample.
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
grofit.param <- function(time, data, gcID = "undefined", control)
{
  time.in <- time
  data.in <- data

  if(!is.null(control$t0) && !is.na(control$t0) && control$t0 != ""){
    t0 <- as.numeric(control$t0)
  } else {
    t0 <- 0
  }

  # Implement min.density into dataset
  if(!is.null(control$min.density)) {
    if (!is.na(control$min.density) && control$min.density != 0) {
      min.density <- control$min.density
      time <- time[max(which.min(abs(time.in - t0)), which.min(abs(data - min.density))):length(time)]
      data <- data[max(which.min(abs(time.in - t0)), which.min(abs(data - min.density))):length(data)]
    }
  }

  # Perform log transformation of data
  if (control$log.y.model == TRUE) {
    data <- log(data/data[1])
  }

  #apply t0 to dataset
  if(is.numeric(t0) && t0 > 0){
    data <- data[which.min(abs(time-t0)):length(data)]
    time <- time[which.min(abs(time-t0)):length(time)]
  }

  # /// determine which values are not valid
  bad.values <- (is.na(time))|(time<0)|(is.na(data))|(data<0)

  # /// remove bad values or stop program
  if (TRUE%in%bad.values){
    if (control$neg.nan.act==FALSE){
      time    <- time[!bad.values]
      data    <- data[!bad.values]
    }
    else{
      stop("Bad values in gcFitModel")
    }
  }

  # fitting parametric growth curves
  y.model     <- NULL
  bestAIC     <- NULL
  best        <- NULL
  used        <- NULL

  # starting values for parametric fitting from spline fit
  control.tmp <- control
  control.tmp$fit.opt <- "s"
  control.tmp$log.y.spline <- control$log.y.model
  nonpara     <- growth.gcFitSpline(time.in, data.in, gcID, control.tmp)
  mu.start     <- nonpara$parameters$mu
  lambda.start  <- nonpara$parameters$lambda
  A.start       <- nonpara$parameters$A

  # /// determine length of model names
  l               <- 10
  for (i in 1:length(control$model.type)) {
    l[i] <- nchar((control$model.type)[i])
  }
  lmax <- max(l)

  # /// loop over all parametric models requested
  for (i in 1:length(control$model.type)) {
    if (control$suppress.messages == FALSE) {
      cat(paste("--> Try to fit model", (control$model.type)[i]))
    }
    initmodel    <- paste("init", (control$model.type)[i], sep = "")

    formulamodel <-
      as.formula(paste(
        "data ~ ",
        (control$model.type)[i],
        "(time, A, mu, lambda, addpar)",
        sep = ""
      ))
    if ((exists((control$model.type)[i])) && (exists(initmodel))) {
      init.model  <-
        do.call(initmodel,
                list(
                  y = data,
                  time = time,
                  A = A.start,
                  mu = mu.start,
                  lambda = lambda.start
                ))

      y.model <-
        try(nls(formulamodel, start = init.model), silent = TRUE)

      if (methods::is(y.model) == "nls") {
        AIC       <- stats::AIC(y.model)
      }

      if (control$suppress.messages == FALSE) {
        if (methods::is(y.model) == "nls") {
          if (y.model$convInfo$isConv == TRUE) {
            message(paste(rep(".", lmax + 3 - l[i])), " OK")
          }
          else{
            warning(paste(
              rep(".", lmax + 3 - l[i]),
              " nls() failed to converge with stopCode ",
              as.character(y.model$convInfo$stopCode)
            ))
          }
        }
        else{
          message(paste(rep(".", lmax + 3 - l[i])),
                  " ERROR in nls(). For further information see help(growth.gcFitModel)")
        }
      }
      if (exists("AIC", inherits = FALSE) && FALSE %in% is.null(AIC)) {
        if (is.null(best) || AIC < bestAIC) {
          bestAIC <- AIC
          best    <- y.model
          used    <- (control$model.type)[i]
        }
      }
    } # of if ( (exists((control$model.type)[i])) ...
    else{
      cat((control$model.type)[i])
      cat("\n")
      cat(initmodel)
      cat("\n")
      stop("The model definition above does not exist! Spelling error?")
    }
    y.model <- NULL
  }

  if (control$suppress.messages == FALSE){
    cat("\n")
    cat(paste0("Best fitting model: ", sub("\\(.+", "", sub("data", "", paste(formula(best), collapse = "")))))
  }
  # /// extract parameters from data fit
  if (is.null(best) == FALSE) {
    Abest      <- summary(best)$parameters["A", 1:2]
    mubest     <- summary(best)$parameters["mu", 1:2]

    if(any(grepl("addpar", rownames(summary(best)$parameters)))){
      fitparbest <- summary(best)$parameters[grep("addpar", rownames(summary(best)$parameters)), 1:2]
      if(summary(best)[["formula"]][[3]][[1]] == "richards"){
        fitparbest <- list(nu = as.data.frame(t(fitparbest)))
      } else if (summary(best)[["formula"]][[3]][[1]] == "gompertz.exp"){
        fitparbest <- list(alpha = fitparbest[1,], t_shift = fitparbest[2,])
      } else if (summary(best)[["formula"]][[3]][[1]] == "huang"){
        fitparbest <- list(y0 = as.data.frame(t(fitparbest)))
      } else if (summary(best)[["formula"]][[3]][[1]] == "baranyi"){
        fitparbest <- list(y0 = as.data.frame(t(fitparbest)))
      }
    }
    fitFlag    <- TRUE
    lambdabest <- summary(best)$parameters["lambda", 1:2]

    best.spline <- stats::smooth.spline(time, fitted.values(best), spar = 0, keep.data = FALSE)
    best.deriv1 <-  stats::predict(best.spline, deriv=1)
    mumax.index <- which.max(best.deriv1$y)

    y.max <- fitted.values(best)[mumax.index]
    t.max <- time[mumax.index]
    b.tangent <- y.max - max(best.deriv1$y) * t.max

    if (length(time) == length(as.numeric(fitted.values(best)))) {
      Integralbest <-
        low.integrate(time, as.numeric(fitted.values(best)))
    }
    else{
      Integralbest <- NA
    }
  }
  else{
    if (control$suppress.messages == FALSE){
      warning("gcFitModel: Unable to fit this curve parametrically!")
    }
    Abest        <- c(NA, NA)
    mubest       <- c(NA, NA)
    lambdabest   <- c(NA, NA)
    Integralbest <- NA
    fitFlag      <- FALSE
    b.tangent <- NA
  }

  gcFitModel <-
    list(
      time.in = time.in,
      data.in = data.in,
      raw.time = time,
      raw.data = data,
      gcID = gcID,
      fit.time = time,
      fit.data = if(is.null(best)){
        NA
        } else {
          as.numeric(fitted.values(best))
          },
      parameters = list(
        A = Abest,
        dY = ifelse(is.null(best), NA, max(fitted.values(best))-min(fitted.values(best))),
        mu = mubest,
        tD = log(2)/as.numeric(mubest),
        lambda = lambdabest,
        b.tangent = b.tangent,
        fitpar = if(exists("fitparbest")){
          fitparbest
        } else {
          NULL
        },
        integral = Integralbest
      ),
      model = used,
      nls = best,
      reliable = NULL,
      fitFlag = fitFlag,
      control = control
    )

  class(gcFitModel) <- "gcFitModel"

  invisible(gcFitModel)
}

#' Perform a smooth spline fit on growth data
#'
#' \code{growth.gcFitSpline} performs a smooth spline fit on the dataset and determines
#' the highest growth rate as the global maximum in the first derivative of the spline.
#'
#' @param time Vector of the independent variable (usually time).
#' @param data Vector of dependent variable (usually: density values).
#' @param gcID (Character) The name of the analyzed sample.
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}},
#'   defining relevant fitting options.
#' @param biphasic (Logical) Shall \code{growth.gcFitSpline} try to extract growth
#'   parameters for two different growth phases (as observed with, e.g., diauxic shifts)
#'   (\code{TRUE}) or not (\code{FALSE})?
#'
#' @details If \code{biphasic = TRUE}, the following steps are performed to define a
#'   second growth phase: \enumerate{ \item Determine local minima within the first
#'   derivative of the smooth spline fit. \item Remove the 'peak' containing the highest
#'   value of the first derivative (i.e., \eqn{mu_{max}}) that is flanked by two local
#'   minima. \item Repeat the smooth spline fit and identification of maximum slope for
#'   later time values than the local minimum after \eqn{mu_{max}}. \item Repeat the
#'   smooth spline fit and identification of maximum slope for earlier time values than
#'   the local minimum before \eqn{mu_{max}}. \item Choose the greater of the two
#'   independently determined slopes as \eqn{mu_{max}2}. }
#'
#' @return A \code{gcFitSpline} object. The lag time is estimated as the intersection between the
#'   tangent at the maximum slope and the horizontal line with \eqn{y = y_0}, where
#'   \code{y0} is the first value of the dependent variable. Use \code{\link{plot.gcFitSpline}} to
#'   visualize the spline fit and derivative over time.
#' \item{time.in}{Raw time values provided to the function as \code{time}.}
#' \item{data.in}{Raw density data provided to the function as \code{data}.}
#' \item{raw.time}{Filtered time values used for the spline fit.}
#' \item{raw.data}{Filtered density values used for the spline fit.}
#' \item{gcID}{(Character) Identifies the tested sample.}
#' \item{fit.time}{Fitted time values.}
#' \item{fit.data}{Fitted density values.}
#' \item{parameters}{List of determined growth parameters.}
#' \itemize{
#' \item \code{A}: {Maximum density.}
#' \item \code{dY}: {Difference in maximum density and minimum density.}
#' \item \code{mu}: {Maximum growth rate (i.e., maximum in first derivative of the spline).}
#' \item \code{tD}: {Doubling time.}
#' \item \code{t.max}: {Time at the maximum growth rate.}
#' \item \code{lambda}: {Lag time.}
#' \item \code{b.tangent}: {Intersection of the tangent at the maximum growth rate with the abscissa.}
#' \item \code{mu2}: {For biphasic growth: Growth rate of the second growth phase.}
#' \item \code{tD2}: {Doubling time of the second growth phase.}
#' \item \code{lambda2}: {For biphasic growth: Lag time determined for the second growth phase.}
#' \item \code{t.max2}: {For biphasic growth: Time at the maximum growth rate of the second growth phase.}
#' \item \code{b.tangent2}: {For biphasic growth: Intersection of the tangent at the maximum growth rate of the second growth phase with the abscissa.}
#' \item \code{integral}: {Area under the curve of the spline fit.}
#' }
#' \item{spline}{\code{smooth.spline} object generated by the \code{\link{smooth.spline}} function.}
#' \item{spline.deriv1}{list of time ('x') and density ('y') values describing the first derivative of the spline fit.}
#' \item{reliable}{(Logical) Indicates whether the performed fit is reliable (to be set manually).}
#' \item{fitFlag}{(Logical) Indicates whether a spline fit was successfully performed on the data.}
#' \item{fitFlag2}{(Logical) Indicates whether a second growth phase was identified.}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#'
#' @family growth fitting functions
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @export
#'
growth.gcFitSpline <- function (time, data, gcID = "undefined", control = growth.control(biphasic = FALSE))
{
  if(!is.null(control$t0) && !is.na(control$t0) && control$t0 != ""){
    t0 <- as.numeric(control$t0)
  } else {
    t0 <- 0
  }
  tmax <- control$tmax
  max.density <- control$max.density

  if (methods::is(control) != "grofit.control")
    stop("control must be of class grofit.control!")
  if (!any(control$fit.opt %in% c("s","a")))
    stop("Fit option is not set for a spline fit. See growth.control()")
  if(length(data[data<0]) > 0){
    data <- data + abs(min(data[data<0]))+0.01 # add the absolute value of the minimum negative density (+ 0.01) to the data
  }
  time.in <- time <- as.vector(as.numeric(as.matrix(time)))[!is.na(time)][!is.na(data)]
  data.in <- data <- as.vector(as.numeric(as.matrix(data)))[!is.na(time)][!is.na(data)]

  if (length(time) != length(data))
    stop("gcFitSpline: length of input vectors differ!")
  bad.values <- (is.na(time)) | (is.na(data)) |
    (!is.numeric(time)) | (!is.numeric(data) )
  if (TRUE %in% bad.values) {
    if (control$neg.nan.act == FALSE) {
      time <- time[!bad.values]
      data <- data[!bad.values]
    }
    else {
      stop("Bad values in gcFitSpline")
    }
  }
  if (control$log.y.spline == TRUE) {
    bad.values <- (data <= 0)
    if (TRUE %in% bad.values) {
      if (control$neg.nan.act == FALSE) {
        time <- time[!bad.values]
        data <- data[!bad.values]
      }
      else {
        stop("Bad values in gcFitSpline")
      }
    }
  }
  if(max(data) < control$growth.thresh * data[1]){
    if(control$suppress.messages==F) message(paste0("gcFitSpline: No significant growth detected (with all values below ", control$growth.thresh, " * start_value)."))
    gcFitSpline <- list(time.in = time.in, data.in = data.in, raw.time = time, raw.data = data,
                        fit.time = rep(NA, length(time.in)), fit.data = rep(NA, length(data.in)), parameters = list(A = 0, dY = 0,
                                                                                                                    mu = 0, t.max = NA, lambda = NA, b.tangent = NA, mu2 = NA, t.max2 = NA,
                                                                                                                    lambda2 = NA, b.tangent2 = NA, integral = NA),
                        spline = NA, spline.deriv1 = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                        control = control)
    class(gcFitSpline) <- "gcFitSpline"
    return(gcFitSpline)
  }
  if (length(data) < 5) {
    cat("gcFitSpline: There is not enough valid data. Must have at least 5!")
    gcFitSpline <- list(time.in = time.in, data.in = data.in, raw.time = time, raw.data = data,
                        gcID = gcID, fit.time = NA, fit.data = NA, parameters = list(A = NA, dY = NA,
                                                                                     mu = NA, t.max = NA, lambda = NA, b.tangent = NA, mu2 = NA, t.max2 = NA,
                                                                                     lambda2 = NA, b.tangent2 = NA, integral = NA),
                        spline = NA, spline.deriv1 = NA, reliable = NULL, fitFlag = TRUE, fitFlag2 = FALSE,
                        control = control)
    class(gcFitSpline) <- "gcFitSpline"
    return(gcFitSpline)
  }
  else {
    if (control$log.x.gc == TRUE) {
      bad.values <- (time <= 0)
      if (TRUE %in% bad.values) {
        if (control$neg.nan.act == FALSE) {
          time <- time[!bad.values]
          data <- data[!bad.values]
        }
        else {
          stop("Bad values in gcFitSpline")
        }
      }
      time <- log(1 + time)
    }
    if (control$log.y.spline == TRUE) {
      data.log <- log(data/data[1])
    }
    time.raw <- time
    data.raw <- if (control$log.y.spline == TRUE) {
      data.log
    } else {
      data
    }
    # Implement min.density into dataset
    if(!is.null(control$min.density)) {
      if (!is.na(control$min.density) && control$min.density != 0) {
        if (control$log.y.spline == TRUE) {
          # perfom log transformation on min.density (Ln(y/y0))
          min.density <- log(control$min.density / data[1])
          time <- time[max(which.min(abs(time - t0)), which.min(abs(data.log - min.density))):length(time)]
          data.log <- data.log[max(which.min(abs(time.raw - t0)), which.min(abs(data.log - min.density))):length(data.log)]
        } else {
          min.density <- control$min.density
          time <-
            time[max(which.min(abs(time - t0)), which.min(abs(data - min.density))):length(data)]
          data <-
            data[max(which.min(abs(time.raw - t0)), which.min(abs(data - min.density))):length(data)]
        }
      }
    }
    # Implement max.density into dataset
    if(!is.null(control$max.density)) {
      if (!is.na(control$max.density)) {
        if (control$log.y.spline == TRUE) {
          # perfom log transformation on max.density (Ln(y/y0))
          max.density <- log(control$max.density / data[1])
          time <- time[data.log <= max.density]
          data.log <- data.log[data.log <= max.density]
        } else {
          max.density <- control$max.density
          time <-
            time[data <= max.density]
          data <-
            data[data <= max.density]
        }
      }
    }
    # Implement t0 into dataset
    if(is.numeric(t0) && t0 > 0){
      if (control$log.y.spline == TRUE) {
        data.log <- data.log[which.min(abs(time-t0)):length(data.log)]
      } else{
        data <- data[which.min(abs(time.raw-t0)):length(data)]
      }
      time <- time[which.min(abs(time.raw-t0)):length(time)]
    }
    # Implement tmax into dataset
    if(is.numeric(tmax) && tmax > t0){
      if (control$log.y.spline == TRUE) {
        data.log <- data.log[time <= tmax]
      } else{
        data <- data[time <= tmax]
      }
      time <- time[time <= tmax]
    }
    # Run spline fit
    try(spline <- smooth.spline(time, y = if(control$log.y.spline == TRUE){
      data.log
    } else {
      data
    }, spar = control$smooth.gc, cv = NA, keep.data = FALSE))
    if (!exists("spline") || is.null(spline) == TRUE) {
      warning("gcFitSpline: Spline could not be fitted to data!")

      gcFitSpline <- list(time.in = time.in, data.in = data.in, raw.time = time, raw.data = data,
                          fit.time = rep(NA, length(time.in)), fit.data = rep(NA, length(data.in)), parameters = list(A = NA, dY = NA,
                                                                                                                      mu = NA, t.max = NA, lambda = NA, b.tangent = NA, mu2 = NA, t.max2 = NA,
                                                                                                                      lambda2 = NA, b.tangent2 = NA, integral = NA),
                          spline = NA, spline.deriv1 = NA, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                          control = control)
      class(gcFitSpline) <- "gcFitSpline"
      return(gcFitSpline)
    } # if(!exists("spline") || is.null(spline) == TRUE)
    else {
      # Perform first derivative and extract parameters
      deriv1 <- stats::predict(spline, time, deriv = 1)
      #consider only slopes at density values greater than the initial value
      deriv1.growth <- deriv1; deriv1.growth$x <- deriv1.growth$x[spline$y > spline$y[1]]; deriv1.growth$y <- deriv1.growth$y[spline$y > spline$y[1]]
      if(length(deriv1.growth$y) < 3){
        warning("gcFitSpline: No significant amount of density values above the start value!")

        gcFitSpline <- list(time.in = time.in, data.in = data.in, raw.time = time, raw.data = data,
                            fit.time = spline$x, fit.data = spline$y, parameters = list(A = NA, dY = NA,
                                                                                        mu = NA, t.max = NA, lambda = NA, b.tangent = NA, mu2 = NA, t.max2 = NA,
                                                                                        lambda2 = NA, b.tangent2 = NA, integral = NA),
                            spline = spline, spline.deriv1 = deriv1, reliable = NULL, fitFlag = FALSE, fitFlag2 = FALSE,
                            control = control)
        class(gcFitSpline) <- "gcFitSpline"
        return(gcFitSpline)
      }
      mumax.index <- which.max(deriv1.growth$y) # index of data point with maximum growth rate in first derivative fit
      mumax.index.spl <- which(spline$x == deriv1.growth$x[mumax.index]) # index of data point with maximum growth rate in spline fit
      t.max <- deriv1.growth$x[mumax.index] # time of maximum growth rate
      mumax <- max(deriv1.growth$y) # maximum value of first derivative of spline fit (i.e., greatest slope in growth curve spline fit)
      y.max <- spline$y[which(deriv1$y == deriv1.growth$y[mumax.index])] # cell density at time of max growth rate
      b.spl <- y.max - mumax * t.max # the y-intercept of the tangent at µmax
      lambda.spl <- (spline$y[1] - b.spl)/mumax  # lag time
      integral <- low.integrate(spline$x, spline$y)

      if(control$biphasic) {
        # determine number of data points in period until maximum density
        n.spl <- length(time[which.min(abs(time-t0)):which.max(data)])
        # Find local minima that frame µmax and remove the 'peak' from deriv1
        n <- round((log(n.spl+4, base=2.1))/0.75)/2
        minima <- inflect(deriv1$y, threshold = n)$minima
        # consider only minima with a slope value of <= 0.75 * µmax
        minima <- minima[deriv1$y[minima] <= 0.75 * mumax]

        minima <- c(1, minima)
        for(i in 1:(length(minima)-1)){
          if(any(minima[i]:minima[i+1] %in% mumax.index.spl)){
            min.ndx <- c(minima[i], minima[i+1])
          }
        }
        if(exists("min.ndx")){
          deriv1.2 <- deriv1
          deriv1.2$y[min.ndx[1]:min.ndx[2]] <- 0
          # find second mumax
          mumax2 <- max(deriv1.2$y) # maximum value of first derivative of spline fit (i.e., greatest slope in growth curve spline fit)
          # accept second mumax only if it is at least 10% of the global mumax
          if(mumax2 >= 0.1*mumax){
            mumax2.index <- which.max(deriv1.2$y) # index of data point with maximum growth rate in first derivative fit
            mumax2.index.spl <- which(spline$x == deriv1.2$x[mumax2.index]) # index of data point with maximum growth rate in spline fit
            t.max2 <- deriv1.2$x[mumax2.index] # time of maximum growth rate
            y.max2 <- spline$y[mumax2.index.spl] # cell density at time of max growth rate
            b.spl2 <- y.max2 - mumax2 * t.max2 # the y-intercept of the tangent at µmax
            lambda.spl2 <- (spline$y[1] - b.spl2)/mumax2  # lag time
            fitFlag2 <- TRUE
          } else {
            mumax2.index <- NA
            mumax2.index.spl <- NA
            t.max2 <- NA
            mumax2 <- NA
            y.max2 <- NA
            b.spl2 <- NA
            lambda.spl2 <- NA
            fitFlag2 <- FALSE
          }
        } else {
          mumax2.index <- NA
          mumax2.index.spl <- NA
          t.max2 <- NA
          mumax2 <- NA
          y.max2 <- NA
          b.spl2 <- NA
          lambda.spl2 <- NA
          fitFlag2 <- FALSE
        }
      } # if(control$biphasic)
      else {
        mumax2.index <- NA
        mumax2.index.spl <- NA
        t.max2 <- NA
        mumax2 <- NA
        y.max2 <- NA
        b.spl2 <- NA
        lambda.spl2 <- NA
        fitFlag2 <- FALSE
      }
      #   # extract local minima and maxima of deriv2
      #   n <- round((log(n.spl+4, base=2.1))/0.75)/2
      #   minima <- inflect(deriv2_spline$y, threshold = n)$minima
      #   maxima <- inflect(deriv2_spline$y, threshold = n)$maxima
      #   # Regard only (negative) minima and (positive) maxima with a deriv2 value of >= 5% mumax
      #   minima <- minima[deriv2_spline$y[minima] <= 0.05 * (-mumax)]
      #   maxima <- maxima[deriv2_spline$y[maxima] >= 0.05 * mumax]
      #   # Find roots with positive slope in deriv2 after µmax
      #     # Find adjacent min - max pairs
      #     minmax <- c(minima, maxima)[order(c(minima, maxima))] # combine minima and maxima in ascending order
      #     minmax.min <- as.list(as.numeric(minmax %in% minima)) # list for each element in minmax vector, value 0 if maximum, value 1 if minimum
      #     # get indices of minima in minmax that are followed by a maximum
      #     ndx.minmax <- c()
      #     for(i in 1:(length(minmax.min)-1)){
      #       if(minmax.min[[i]]==1 & minmax.min[[i+1]]==0){
      #         ndx.minmax <- c(ndx.minmax, i)
      #       }
      #     }
      #     # define pair candidates; store in list
      #     pairs.post <- list()
      #     for(i in 1:length(ndx.minmax)){
      #       pairs.post[[i]] <- c(minmax[[ndx.minmax[[i]]]], minmax[[ndx.minmax[[i]]+1]])
      #     }
      #     # extract candidate that is closest to µmax, and respective index of minimum and maximum
      #     ndx.minima_cand <- c()
      #     ndx.minima_cand <- unlist(lapply(1:length(pairs.post), function(x) c(ndx.minima_cand, pairs.post[[x]][1])))
      #     min.ndx <- ndx.minima_cand[which.min((ndx.minima_cand- mumax.index.spl)[(ndx.minima_cand- mumax.index.spl)>0])]
      #     max.ndx <- pairs.post[[which.min((ndx.minima_cand- mumax.index.spl)[(ndx.minima_cand- mumax.index.spl)>0])]][2]
      #     # Linear interpolation between minimum and maximum to find root
      #     interpol <- approxfun(y = c(deriv2$x[min.ndx], deriv2$x[max.ndx]), x = c(deriv2$y[min.ndx], deriv2$y[max.ndx]))
      #     t.turn <- interpol(0)
      #     t.turn_post.ndx <- which.min(abs(deriv2$x-t.turn)) # The time index closest to the turning point after µmax
      #
      #   # Find roots with negative slope in deriv2 before µmax
      #     # get indices of maxima in minmax that are followed by a minimum
      #     ndx.maxmin <- c()
      #     for(i in 1:(length(minmax.min)-1)){
      #       if(minmax.min[[i]]==0 & minmax.min[[i+1]]==1){
      #         ndx.maxmin <- c(ndx.maxmin, i)
      #       }
      #     }
      #     # define pair candidates; store in list
      #     pairs.pre <- list()
      #     for(i in 1:length(ndx.maxmin)){
      #       pairs.pre[[i]] <- c(minmax[[ndx.maxmin[[i]]]], minmax[[ndx.maxmin[[i]]+1]])
      #     }
      #     # extract candidate that is closest to µmax, and respective index of minimum and maximum
      #     ndx.maxima_cand <- c()
      #     ndx.maxima_cand <- unlist(lapply(1:length(pairs.pre), function(x) c(ndx.maxima_cand, pairs.pre[[x]][1])))
      #     min.ndx <- ndx.maxima_cand[which.min((ndx.maxima_cand- mumax.index.spl)[(ndx.maxima_cand- mumax.index.spl)>0])]
      #     max.ndx <- pairs.pre[[which.min((ndx.maxima_cand- mumax.index.spl)[(ndx.maxima_cand- mumax.index.spl)>0])]][2]
      #     # Linear interpolation between minimum and maximum to find root
      #     interpol <- approxfun(y = c(deriv2$x[min.ndx], deriv2$x[max.ndx]), x = c(deriv2$y[min.ndx], deriv2$y[max.ndx]))
      #     t.turn <- interpol(0)
      #     t.turn_pre.ndx <- which.min(abs(deriv2$x-t.turn)) # The time index closest to the turning point after µmax
      #
      #   # Remove
      #
      #     plot(deriv1.2$x, deriv1.2$y, type = 'l')
      #     points(
      #       deriv1.2$x[minima],
      #       deriv1.2$y[minima],
      #       pch = 16,
      #       col = "Blue",
      #       cex = 1.5
      #     )
      #     points(deriv1.2$x[mumax2.index], deriv1.2$y[mumax2.index], pch = 16, col = "Red", cex = 1.5)
      #     points(deriv1$x[mumax.index], deriv1$y[mumax.index], pch = 16, col = "Green", cex = 1.5)
      #     points(deriv1$x[t.turn_pre.ndx], deriv1$y[t.turn_pre.ndx], pch = 16, col = "Black", cex = 1.5)
      #     points(deriv1$x[t.turn_post.ndx], deriv1$y[t.turn_post.ndx], pch = 16, col = "Black", cex = 1.5)
      #
      #
      #
      #     plot(deriv2$x, deriv2$y, type = 'l', main = "Minima \nVariable Thresholds")
      #     points(
      #         deriv2$x[minima],
      #         deriv2$y[minima],
      #         pch = 16,
      #         col = "Blue",
      #         cex = 1.5
      #       )
      #     points(deriv2$x[maxima], deriv2$y[maxima], pch = 16, col = "Red", cex = 1.5)
      #     points(deriv2$x[mumax.index], deriv2$y[mumax.index], pch = 16, col = "Green", cex = 1.5)
      #     points(deriv2$x[t.turn_pre.ndx], deriv2$y[t.turn_pre.ndx], pch = 16, col = "Black", cex = 1.5)
      #     points(deriv2$x[t.turn_post.ndx], deriv2$y[t.turn_post.ndx], pch = 16, col = "Black", cex = 1.5)
      #
      #
      #
      #
      #     plot(spline$x, spline$y, type = "l")
      #     points(spline$x[minima], spline$y[minima], pch = 16, col = cf.2(1)[1], cex = 1.5)
      #     points(spline$x[maxima], spline$y[maxima], pch = 16, col = cf.1(1)[1], cex = 1.5)
      #
      #     plot(deriv2_spline$x, deriv2_spline$y, type = 'l', main = "Minima \nVariable Thresholds")
      #     points(
      #       deriv2_spline$x[minima],
      #       deriv2_spline$y[minima],
      #       pch = 16,
      #       col = cf.2(1)[1],
      #       cex = 1.5
      #     )
      #     points(deriv2_spline$x[maxima], deriv2_spline$y[maxima], pch = 16, col = cf.1(1)[1], cex = 1.5)
      # }

    } # else of if (!exists("spline") || is.null(spline) == TRUE)
  } # else of if (length(data) < 5)
  gcFitSpline <-
    list(
      time.in = time.in,
      data.in = data.in,
      raw.time = time.raw,
      raw.data = data.raw,
      gcID = gcID,
      fit.time = spline$x,
      fit.data = spline$y,
      parameters = list(
        A = if (control$log.y.spline == TRUE) {
          # Correct ln(N/N0) transformation for max density value
          data[1] * exp(max(spline$y))
        } else {
          max(spline$y)
        },
        dY = if (control$log.y.spline == TRUE) {
          data[1] * exp(max(spline$y)) -  data[1] * exp(spline$y[1])
        } else {
          max(spline$y) - spline$y[1]
        },
        mu = mumax,
        tD = log(2)/mumax,
        t.max = t.max,
        lambda = lambda.spl,
        b.tangent = b.spl,
        mu2 = mumax2,
        tD2 = log(2)/mumax2,
        t.max2 = t.max2,
        lambda2 = lambda.spl2,
        b.tangent2 = b.spl2,
        integral = integral),
      spline = spline,
      spline.deriv1 = deriv1,
      fitFlag = TRUE,
      fitFlag2 = fitFlag2,
      control = control
    )
  class(gcFitSpline) <- "gcFitSpline"
  gcFitSpline
}

#' Fit an exponential growth model with a heuristic linear method
#'
#' Determine maximum growth rates from the log-linear part of a growth curve using
#' a heuristic approach similar to the ``growth rates made easy''-method of
#' Hall et al. (2013).
#'
#' The algorithm works as follows:
#' \enumerate{
#'   \item Fit linear regressions (Theil-Sen estimator) to all subsets of \code{h} consecutive, log-transformed data
#'     points (sliding window of size \code{h}). If for example \eqn{h=5}, fit a linear regression to points
#'     1 \dots 5, 2 \dots 6, 3 \dots 7 and so on.
#'   \item Find the subset with the highest slope \eqn{mu_{max}}. Do the \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} and relative standard deviation (RSD) values of the regression meet the in \code{lin.R2} and \code{lin.RSD} defined thresholds and do the data points within the regression window account for a fraction of at least \code{lin.dY} of the total density increase? If not, evaluate the subset with the second highest slope, and so on.
#'   \item Include also the data points of adjacent subsets that have a slope of at least \eqn{quota \cdot mu{max}}, e.g., all regression windows that have at least 95% of the maximum slope.
#'   \item Fit a new linear model to the extended data window identified in step 3.
#' }
#' If \code{biphasic = TRUE}, the following steps are performed to define a second growth phase:
#' \enumerate{
#'   \item Perform a smooth spline fit on the data with a smoothing factor of 0.5.
#'   \item Calculate the second derivative of the spline fit and perform a smooth spline fit of the derivative with a smoothing factor of 0.4.
#'   \item Determine local maxima and minima in the second derivative.
#'   \item Find the local minimum following \eqn{mu_{max}} and repeat the heuristic linear method for later time values.
#'   \item Find the local maximum before \eqn{mu_{max}} and repeat the heuristic linear method for earlier time values.
#'   \item Choose the greater of the two independently determined slopes as \eqn{mu_{max}2}.
#' }
#'
#' @param time Vector of the independent variable (usually: time).
#' @param data Vector of dependent variable (usually: density values).
#' @param quota (Numeric, between 0 an 1) Define what fraction of \eqn{mu_{max}} the slope of regression windows adjacent to the window with highest slope should have to be included in the overall linear fit.
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#' @param gcID (Character) The name of the analyzed sample.
#' @param log.x.gc (Logical) Indicates whether _ln(x+1)_ should be applied to the time data for _linear_ and _spline_ fits. Default: \code{FALSE}.
#' @param log.y.lin (Logical) Indicates whether _ln(y/y0)_ should be applied to the growth data for _linear_ fits. Default: \code{TRUE}
#' @param min.density (Numeric) Indicate whether only density values above a certain threshold should be considered for linear regressions.
#' @param max.density (Numeric) Indicate whether only density values below a certain threshold should be considered for linear regressions.
#' @param t0 (Numeric) Minimum time value considered for linear and spline fits.
#' @param tmax (Numeric) Minimum time value considered for linear and spline fits.
#' @param lin.h (Numeric) Manually define the size of the sliding window . If \code{NULL}, h is calculated for each samples based on the number of measurements in the growth phase of the plot.
#' @param lin.R2 (Numeric) \ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} threshold for \code{\link{growth.gcFitLinear}}
#' @param lin.RSD (Numeric) Relative standard deviation (RSD) threshold for calculated slope in \code{\link{growth.gcFitLinear}}
#' @param lin.dY (Numeric) Enter the minimum percentage of density increase that a linear regression should cover.
#' @param biphasic (Logical) Shall \code{\link{growth.gcFitLinear}} try to extract growth parameters for two different growth phases (as observed with, e.g., diauxic shifts) (\code{TRUE}) or not (\code{FALSE})?
#'
#' @return A \code{gcFitLinear} object with parameters of the fit. The lag time is
#'   estimated as the intersection between the fit and the horizontal line with
#'   \eqn{y=y_0}, where \code{y0} is the first value of the dependent variable.
#'   Use \code{\link{plot.gcFitSpline}} to visualize the linear fit.
#' \item{raw.time}{Raw time values provided to the function as \code{time}.}
#' \item{raw.data}{Raw density data provided to the function as \code{data}.}
#' \item{filt.time}{Filtered time values used for the heuristic linear method.}
#' \item{filt.data}{Filtered density values.}
#' \item{log.data}{Log-transformed, filtered density values used for the heuristic linear method.}
#' \item{gcID}{(Character) Identifies the tested sample.}
#' \item{FUN}{Linear _function_ used for plotting the tangent at mumax.}
#' \item{fit}{\code{lm} object; result of the final call of \code{\link{lm}} to perform the linear regression.}
#' \item{par}{List of determined growth parameters.}
#' \itemize{
#' \item \code{y0}: {Minimum density value considered for the heuristic linear method.}
#' \item \code{dY}: {Difference in maximum density and minimum density.}
#' \item \code{A}: {Maximum density.}
#' \item \code{y0_lm}: {Intersection of the linear fit with the abscissa.}
#' \item \code{mumax}: {Maximum growth rate (i.e., slope of the linear fit).}
#' \item \code{tD}: {Doubling time.}
#' \item \code{mu.se}: {Standard error of the maximum growth rate.}
#' \item \code{lag}: {Lag time.}
#' \item \code{tmax_start}: {Time value of the first data point within the window used for the linear regression.}
#' \item \code{tmax_end}: {Time value of the last data point within the window used for the linear regression.}
#' \item \code{t_turn}: {For biphasic growth: Time of the inflection point that separates two growth phases.}
#' \item \code{mumax2}: {For biphasic growth: Growth rate of the second growth phase.}
#' \item \code{tD2}: {Doubling time of the second growth phase.}
#' \item \code{y0_lm2}: {For biphasic growth: Intersection of the linear fit of the second growth phase with the abscissa.}
#' \item \code{lag2}: {For biphasic growth: Lag time determined for the second growth phase..}
#' \item \code{tmax2_start}: {For biphasic growth: Time value of the first data point within the window used for the linear regression of the second growth phase.}
#' \item \code{tmax2_end}: {For biphasic growth: Time value of the last data point within the window used for the linear regression of the second growth phase.}
#' }
#' \item{ndx}{Index of data points used for the linear regression.}
#' \item{ndx2}{Index of data points used for the linear regression for the second growth phase.}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#' \item{rsquared}{\ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} of the linear regression.}
#' \item{rsquared2}{\ifelse{html}{\out{R<sup>2</sup>}}{\eqn{R^2}} of the linear regression for the second growth phase.}
#' \item{fitFlag}{(Logical) Indicates whether linear regression was successfully performed on the data.}
#' \item{fitFlag2}{(Logical) Indicates whether a second growth phase was identified.}
#' \item{reliable}{(Logical) Indicates whether the performed fit is reliable (to be set manually).}
#'
#' @references Hall, BG., Acar, H, Nandipati, A and Barlow, M (2014) Growth Rates Made Easy. _Mol. Biol. Evol._ 31: 232-38, DOI: 10.1093/molbev/mst187
#' @references Petzoldt T (2022). growthrates: Estimate Growth Rates from Experimental Data. R package version 0.8.3, <https://CRAN.R-project.org/package=growthrates>.
#' @references Theil, H.(1992). A rank-invariant method of linear and polynomial regression analysis. In: Henri Theil’s contributions to economics and econometrics. Springer, pp. 345–381. DOI: 10.1007/978-94-011-2546-8_20
#'
#' @family growth fitting functions
#'
#' @export
#'
growth.gcFitLinear <- function(time, data, gcID = "undefined", quota = 0.95,
                               control = growth.control(t0 = 0, tmax = NA, log.x.gc = FALSE, log.y.lin = TRUE, min.density = NA, max.density = NA, lin.h = NULL, lin.R2 = 0.97, lin.RSD = 0.1, lin.dY = 0.05, biphasic = FALSE))
{
  R2 <- control$lin.R2
  RSD <- control$lin.RSD
  h <- control$lin.h
  fit.dY <- control$lin.dY
  t0 <- control$t0
  tmax <- control$tmax
  min.density <- control$min.density
  max.density <- control$max.density

  if(length(data[data<0]) > 0){
    data <- data + abs(min(data[data<0]))+0.01 # add the absolute value of the minimum negative density (+ 0.01) to the data
  }

  bad.values <- ((is.na(time))|(is.na(data)) | time < 0 | data <=0)
  data.in <- data <- data[!bad.values]
  time.in <- time <- time[!bad.values]
  if(!is.null(t0) && !is.na(t0) && t0 != ""){
    t0 <- as.numeric(t0)
  } else {
    t0 <- 0
  }
  if(!is.null(tmax) && !is.na(tmax) && tmax != ""){
    tmax <- as.numeric(tmax)
  } else {
    tmax <- NA
  }

  if(length(data) < 4){
    if(control$suppress.messages==F) message(paste0("Linear fit: Not enough valid values in sample to perform fit."))
    gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = NA, filt.data = NA,
                        log.data = NA, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                          t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                          tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
    )
    class(gcFitLinear) <- "gcFitLinear"
    return(gcFitLinear)
  }

  # extract period of growth (from defined t0 to tmax)
  t.growth <- time[which.min(abs(time-t0)):which.max(data)]
  if(!is.na(tmax)){
   t.growth <-  t.growth[t.growth <= tmax]
  }
  if(!is.null(h) && !is.na(h) && h != ""){
    h <- as.numeric(h)
  } else {
    # determine number of data points in period until maximum density
    n.spl <- length(t.growth)
    # Calculate h via log-transformation of the number of data points
    # if(n.spl <= 100){
    h <- round((log(n.spl+4, base=2.1))/0.75)
    #test h calculation
    # s <- c(1:500)
    # plot(s, round((log(s+4, base=2.1))/0.75))
    # } else {
    #   h <- round((log(n.spl/30, base=1.1))*0.75)
    #   #test h calculation
    #    s <- c(100:500)
    #    plot(s, round((log(s/20, base=1.15))*0.75))
    # }
  }
  control$lin.h <- h

  if(!is.null(quota) && !is.na(quota) && quota != ""){
    if(quota > 1){
      quota <- as.numeric(quota)/100
    } else {
      quota <- as.numeric(quota)
    }
  } else {
    quota <- 0.95
  }
  if(!is.null(R2) && !is.na(R2) && R2 != ""){
    R2 <- as.numeric(R2)
  } else {
    R2 <- 0.95
  }
  if(!is.null(RSD) && !is.na(RSD) && RSD != ""){
    RSD <- as.numeric(RSD)
  } else {
    RSD <- 0.05
  }
  data.log <- log(data.in/data.in[1])
  if(!is.null(control$min.density) && !is.na(control$min.density)){
    if(control$min.density != 0 && control$log.y.lin == TRUE){
      min.density <- log(control$min.density / data[1])
    } else {
      min.density <- 0
    }
  } else {
    min.density <- 0
  }
  if(!is.null(control$max.density) && !is.na(control$max.density)){
    if(control$log.y.lin == TRUE){
      max.density <- log(max.density / data[1])
    }
  } else {
    max.density <- NA
  }
  bad.values <- ((is.na(data.log))|(is.infinite(data.log))|(is.na(time))|(is.na(data.log)))

  # /// remove bad values or stop program
  if (TRUE%in%bad.values){
    if (control$neg.nan.act==FALSE){
      time    <- time[!bad.values]
      data.log    <- data.log[!bad.values]
      data <- data[!bad.values]
    }
    else{
      stop("Bad values in gcFitModel")
    }
  }
  if (any(duplicated(time))) stop("time variable must not contain duplicated values")

  # store filtered and transformed data
  obs <- data.frame(time, data)
  obs$ylog <- data.log
  obs.max.density <- max(obs$data)
  dY.total <- obs.max.density - obs$data[1]

  if(max(data.in) < control$growth.thresh * data.in[1]){
    if(control$suppress.messages==F) message(paste0("Linear fit: No significant growth detected (with all values below ", control$growth.thresh, " * start_value)."))
    gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                        log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                          t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                          tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
    )
    class(gcFitLinear) <- "gcFitLinear"
    if(control$suppress.messages==F) message("Linear fit: No data range in accordance with the chosen parameters identified with appropriate linearity.")
    return(gcFitLinear)
  }
  ## number of values
  N <- nrow(obs)

  if(N > h && N>3){
    # Perform linear regression for all N windows and save results in 'ret'
    ret <- matrix(0, nrow = N - h, ncol = 6)
    if (control$log.y.lin == TRUE) {
      for(i in 1:(N - h)) {
        ret[i, ] <- c(i, with(obs, (lm_parms(lm_window(time, ylog, i0 = i, h = h)))))
      }
    } else {
      for(i in 1:(N - h)) {
        ret[i, ] <- c(i, with(obs, (lm_parms(lm_window(time, data, i0 = i, h = h)))))
      }
    }
    colnames(ret) <- c("index", "y-intersect", "slope", "X4", "R2", "RSD")
    # add time and density values as columns in ret
    if (control$log.y.lin == TRUE) {
      ret <- data.frame(ret, time = time[ret[,1]], data = obs$ylog[ret[,1]])
    } else {
      ret <- data.frame(ret, time = time[ret[,1]], data = obs$data[ret[,1]])
    }

    # add dY, i.e., the percentage of density that a regression window covers, to ret
    ret <- data.frame(ret, dY = ((obs$data[match(ret[, "time"], obs$time)+(h-1)] - obs$data[match(ret[, "time"], obs$time)]) / dY.total))

    bad <- is.na(ret[,5]) | is.na(ret[,6])
    ret <- ret[!bad,]
    # Consider only regressions within the growth phase (from start to maximum density)
    ret <- ret[ret$time <= t.growth[length(t.growth)], ]
    if(nrow(ret)<2){
      gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                          log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                            y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                            t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                            tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
      )
      class(gcFitLinear) <- "gcFitLinear"
      if(control$suppress.messages==F) message("No data range in accordance with the chosen parameters identified with appropriate linearity.")
      return(gcFitLinear)
    }
    else{
      # duplicate ret for further tuning of fit
      if(exists("min.density")){
        ret.check <- ret[max(which.min(abs(time-t0)), which.min(abs(ret$data-min.density))) : nrow(ret),] # consider only slopes from defined t0 and min.density
      } else {
        ret.check <- ret[which.min(abs(time-t0)):nrow(ret),] # consider only slopes from defined t0
      }
      if(!is.na(tmax)){
        ret.check <- ret.check[(ret.check[,"time"]+(h*time[2]-time[1])) <= tmax, ] # consider only slopes up to defined t0
      }
      if(!is.na(max.density)){
        if (control$log.y.lin) {
          ret.check <- ret.check[unlist(lapply(1:nrow(ret.check), function(x) obs$ylog[ret.check[x, 1]+(h-1)])) <= max.density, ] # consider only slopes up to defined max.density
        } else {
          ret.check <- ret.check[unlist(lapply(1:nrow(ret.check), function(x) obs$data[ret.check[x, 1]+(h-1)])) <= max.density, ] # consider only slopes up to defined max.density
        }
      }

      #Consider only slopes that span at least fit.dY
      ret.check <- ret.check[ret.check[,"dY"]>=fit.dY, ]

      # Consider only positive slopes
      ret.check <- ret.check[ret.check[,"slope"]>0, ]

      if(nrow(ret.check)<2){
        gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                            log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                              y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                              t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                              tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
        )
        class(gcFitLinear) <- "gcFitLinear"
        if(control$suppress.messages==F) message("No data range in accordance with the chosen parameters identified with appropriate linearity.")
        return(gcFitLinear)
      } else {
        ## Determine index of window with maximum growth rate, iterate until regression is found that meets R2 and RSD criterion
        success <- FALSE
        # apply min.density to list of linear regressions
        ret.check <- ret.check[ret.check[, 8] >= min.density, ]
        ret.check2 <- ret.check
        if(any(ret.check2[,5] >= R2 & abs(ret.check2[,6]) <= RSD)){
          while (!success){
            index.max <- which.max(ret.check2[, 3])
            if(ret.check2[index.max,5] >= R2 && abs(ret.check2[index.max,6]) <= RSD && !is.na(ret.check2[index.max,6]) ){ # prerequisites for suitable µmax candidate: R2 and RSD
              slope.max <- ret.check2[index.max,3]
              success <- TRUE
            } else {
              ret.check2 <- ret.check2[-index.max,]
            }
          }
          index.max.ret <- ret.check[which(ret.check[,3]==slope.max),1] # index of maximum slope in fit table
          slope.quota <- quota * slope.max
          if(exists("min.density")){
            candidates <- ret.check[which(ret.check[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                            ret.check[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                            abs(ret.check[, 6]) <= 1.02 * RSD & # RSD criterion for candidates
                                            ret.check[, 7] >= t0 & # consider only slopes after defined t0
                                            ret.check[, 8] >= min.density # consider only slopes at densities higher than "min.density"
            ), 1]
          } else{
            candidates <- ret.check[which(ret.check[, 3] >= slope.quota & # indices of slopes greater than slope.quota
                                            ret.check[, 5] >= 0.98*R2 & # R2 criterion for candidates
                                            abs(ret.check[, 6]) <= 1.02 * RSD & # RSD criterion for candidates
                                            ret.check[, 7] >= t0), 1] # consider only slopes after defined t0
          }
          #consider only candidate windows next to index.max.ret
          candidate_intervals <- split(candidates, cumsum(c(1, diff(candidates) != 1))) # split candidates into consecutive intervals
          if(any(index.max.ret %in% unlist(candidate_intervals))){
            # which interval contains maximum slope?
            ndx <-  as.numeric(which(sapply(
              candidate_intervals,
              FUN = function(X)
                any(X %in% index.max.ret)
            )))

            candidates <-
              unlist(candidate_intervals[ndx])
          }


          if(length(candidates) > 0) {
            #perform linear regression with candidate data points
            tp <- seq(min(candidates), max(candidates) + h-1)
            if (control$log.y.lin == TRUE) {
              m <- lm_window(obs$time, obs$ylog, min(tp), length(tp)) # linear model
            } else {
              m <- lm_window(obs$time, obs$data, min(tp), length(tp)) # linear model
            }
            p  <- c(lm_parms(m), n=length(tp)) # # slope equation parameters (linear model)
          } else {
            p <- c(a=0, b=0, se=0, r2=0, cv=0, n=0)
            m = NULL
          }

          if(length(candidates) > 0) {
            ## get time window of exponential fit
            tmax_start <- obs$time[tp[1]]
            tmax_end <- obs$time[tp[length(tp)]]

            y0_lm    <- unname(coef(m)[1]) # y-intercept of tangent
            if (control$log.y.lin == TRUE) {
              y0_data  <- obs$ylog[1] # y0 in dataset
            } else {
              y0_data  <- obs$data[1] # y0 in dataset
            }
            mumax <- unname(coef(m)[2])

            ## estimate lag phase
            lambda <- (y0_data - y0_lm) / mumax

            # correct y0 values for Ln(y(t)/y0)
            if (control$log.y.lin == TRUE) {
              y0_lm <- obs$data[1] * exp(y0_lm)
              y0_data <- obs$data[1]
            }


            # get indices of time points used in linear fit
            ndx <- seq(min(match(ret.check[match(candidates, ret.check[,1]), "time"], time.in)),
                       max(match(ret.check[match(candidates, ret.check[,1]), "time"], time.in)) + h-1)

            mu.se <- as.numeric(p[3]) # standard error of slope
            fitFlag <- TRUE

          }
          else { # of if(length(candidates) > 0)
            gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                                log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                                  y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                                  t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                                  tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
            )
            class(gcFitLinear) <- "gcFitLinear"
            if(!control$suppress.messages) message(paste0("No linear fit in accordance with the chosen parameters identified with: R2 >= ", R2, ", RSD <= ", RSD, ", t0 = ", t0, ", and min.density = ", control$min.density, "."))
            return(gcFitLinear)
          }
          if(control$biphasic) {
            if (control$log.y.lin == TRUE) {
              spline <- stats::smooth.spline(obs$time, obs$ylog, spar = 0.5, cv = NA, keep.data = FALSE)
            } else {
              spline <- stats::smooth.spline(obs$time, obs$data, spar = 0.5, cv = NA, keep.data = FALSE)
            }
            deriv2 <- stats::predict(spline, deriv = 2)
            deriv2_spline <- stats::smooth.spline(deriv2$x, deriv2$y, spar = 0.4, cv = NA, keep.data = FALSE)
            # extract local minima and maxima of deriv2
            n = ceiling(h/2)
            minima <- inflect(deriv2_spline$y, threshold = n)$minima
            maxima <- inflect(deriv2_spline$y, threshold = n)$maxima
            # Regard only (negative) minima and (positive) maxima with a deriv2 value of >= 10% mumax
            minima <- minima[deriv2_spline$y[minima] <= 0.1 * (-mumax)]
            maxima <- maxima[deriv2_spline$y[maxima] >= 0.1 * mumax]

            # expand mumax window with more relaxed quota
            slope.quota.ext <- 0.8 * slope.max
            if(exists("min.density")){
              candidates.ext <- ret[which(ret[, 3] >= slope.quota.ext & # indices of slopes greater than slope.quota
                                            ret[, 5] >= 0.95*R2 & # R2 criterion for candidates
                                            abs(ret[, 6]) <= 1.1 * RSD & # RSD criterion for candidates
                                            ret[, 7] >= t0 & # consider only slopes after defined t0
                                            ret[, 8] >= min.density # consider only slopes at densities higher than "min.density"
              ), 1]
            } else{
              candidates.ext <- ret[which(ret[, 3] >= slope.quota.ext & # indices of slopes greater than slope.quota
                                            ret[, 5] >= 0.95*R2 & # R2 criterion for candidates
                                            abs(ret[, 6]) <= 1.1 * RSD & # RSD criterion for candidates
                                            ret[, 7] >= t0), 1] # consider only slopes after defined t0
            }



            #consider only candidate windows next to index.max.ret
            candidate_intervals.ext <- split(candidates.ext, cumsum(c(1, diff(candidates.ext) != 1)))
            if(any(index.max.ret %in% unlist(unname(candidate_intervals.ext)))){
              ndx.ext <-  as.numeric(which(sapply(
                candidate_intervals.ext,
                FUN = function(X)
                  any(X %in% index.max.ret)
              )))

              candidates.ext <-
                candidate_intervals.ext[[ndx.ext]]
            }
            tp.ext <- seq(min(candidates.ext), max(candidates.ext) + h-1)



            # # # Color functions
            #   cf.1 <- grDevices::colorRampPalette(c("red", "red"))
            #   cf.2 <- grDevices::colorRampPalette(c("blue", "blue"))
            #   plot(deriv2$x, deriv2$y, type = 'l', main = "Minima \nVariable Thresholds")
            #   points(
            #       deriv2$x[minima],
            #       deriv2$y[minima],
            #       pch = 16,
            #       col = cf.2(1)[1],
            #       cex = 1.5
            #     )
            #   points(deriv2$x[maxima], deriv2$y[maxima], pch = 16, col = cf.1(1)[1], cex = 1.5)
            #
            #   plot(spline$x, spline$y, type = "l")
            #   points(spline$x[minima], spline$y[minima], pch = 16, col = cf.2(1)[1], cex = 1.5)
            #   points(spline$x[maxima], spline$y[maxima], pch = 16, col = cf.1(1)[1], cex = 1.5)
            #
            #   plot(deriv2_spline$x, deriv2_spline$y, type = 'l', main = "Minima \nVariable Thresholds")
            #   points(
            #     deriv2_spline$x[minima],
            #     deriv2_spline$y[minima],
            #     pch = 16,
            #     col = cf.2(1)[1],
            #     cex = 1.5
            #   )
            #   points(deriv2_spline$x[maxima], deriv2_spline$y[maxima], pch = 16, col = cf.1(1)[1], cex = 1.5)


            # get local deriv2-minimum after mumax
            # postmin.ndx <- minima[(minima - tp.ext[length(tp.ext)]) >= 0][which.min(minima[(minima - tp.ext[length(tp.ext)]) >= 0])]
            postmin.ndx <- minima[which.min(abs(minima - tp.ext[length(tp.ext)]))]
            # extract linear regression results after post-mumax turning point
            ret.postmin <- ret[ret$time >= obs$time[postmin.ndx],]
            # remove indices included in extended mumax regression
            ret.postmin <- ret.postmin[!(ret.postmin[,1] %in% tp.ext),]
            #Consider only slopes that span at least fit.dY
            ret.postmin <- ret.postmin[ret.postmin[, "dY"] >= fit.dY, ]
            # Consider only positive slopes
            ret.postmin <- ret.postmin[ret.postmin[, "slope"] > 0, ]
            ## Determine index of window with maximum growth rate, iterate until regression is found that meets R2 and RSD criterion
            success <- FALSE
            # apply min.density to list of linear regressions
            ret.postmin.check <- ret.postmin[ret.postmin[, 8] >= min.density, ]
            # apply max.density to list of linear regressions
            if(!is.na(max.density)){
              ret.postmin.check <- ret.postmin.check[ret.postmin.check[,"data"] <= max.density, ] # consider only slopes up to defined max.density
            }
            # apply max. time to list of linear regeressions
            if(!is.na(tmax)){
              ret.postmin.check <- ret.postmin.check[ret.postmin.check[,"time"] <= tmax, ] # consider only slopes up to defined t0
            }

            if (any(ret.postmin.check[, 5] >= R2 &
                    abs(ret.postmin.check[, 6]) <= RSD)) {
              while (!success) {
                index.max <- which.max(ret.postmin.check[, 3])
                if (ret.postmin.check[index.max, 5] >= R2 &&
                    abs(ret.postmin.check[index.max, 6]) <= RSD &&
                    !is.na(ret.postmin.check[index.max, 6])) {
                  # prerequisites for suitable µmax candidate: R2 and RSD
                  slope.max.postmin <- ret.postmin.check[index.max, 3]
                  success <- TRUE
                } else {
                  ret.postmin.check <- ret.postmin.check[-index.max,]
                }
              }
              index.max.ret.postmin <- ret.postmin[which(ret.postmin[, 3] == slope.max.postmin), 1] # index of maximum slope in fit table
              slope.quota <- quota * slope.max.postmin
              candidates.postmin <- ret.postmin[which(
                ret.postmin[, 3] >= slope.quota &
                  ret.postmin[, 5] >= 0.98 * R2 &
                  abs(ret.postmin[, 6]) <= 1.02 * RSD &
                  ret.postmin[, 7] >= t0), 1]
              #consider only candidate windows next to index.max.ret.postmin
              candidate_intervals.postmin <- split(candidates.postmin, cumsum(c(1, diff(candidates.postmin) != 1)))
              if (any(index.max.ret.postmin %in% unlist(candidate_intervals.postmin))) {
                ndx.postmin <-  as.numeric(which(sapply(
                  candidate_intervals.postmin,
                  FUN = function(X)
                    any(X %in% index.max.ret.postmin)
                )))

                candidates.postmin <-
                  candidate_intervals.postmin[[ndx.postmin]]
              }

              if (length(candidates.postmin) > 0) {
                #perform linear regression with candidate data points
                tp.postmin <- seq(min(candidates.postmin), max(candidates.postmin) + h - 1)
                if (control$log.y.lin == TRUE) {
                  m.postmin <- lm_window(obs$time, obs$ylog, min(tp.postmin), length(tp.postmin)) # linear model
                } else {
                  m.postmin <- lm_window(obs$time, obs$data, min(tp.postmin), length(tp.postmin)) # linear model
                }
                p.postmin  <- c(lm_parms(m.postmin), n = length(tp.postmin)) # # slope equation parameters (linear model)
              } else {
                p.postmin <- c(
                  a = 0,
                  b = 0,
                  se = 0,
                  r2 = 0,
                  cv = 0,
                  n = 0
                )
                m = NULL
              }

              if (length(candidates.postmin) > 0) {
                ## get time window of exponential fit
                tmax_start.postmin <- obs$time[tp.postmin[1]]
                tmax_end.postmin <- obs$time[tp.postmin[length(tp.postmin)]]
                t_turn.postmin <- obs$time[postmin.ndx]
                y0_lm.postmin <- unname(coef(m.postmin)[1]) # y-intercept of tangent
                mumax.postmin <- unname(coef(m.postmin)[2])

                ## estimate lag phase between first and second growth phase
                if (control$log.y.lin == TRUE) {
                  lambda.postmin <- (obs$ylog[1] - y0_lm.postmin) / mumax.postmin
                } else {
                  lambda.postmin <- (obs$data[1] - y0_lm.postmin) / mumax.postmin
                }

                # correct y0 values for Ln(y(t)/y0)
                if (control$log.y.lin == TRUE) {
                  y0_lm.postmin <- obs$data[1] * exp(y0_lm.postmin)
                }

                # get indices of time points used in linear fit
                ndx.postmin <- seq(min(match(ret[match(candidates.postmin, ret[,1]), "time"], time.in)),
                                   max(match(ret[match(candidates.postmin, ret[,1]), "time"], time.in)) + h -
                                     1)
                mu.se.postmin <- as.numeric(p.postmin[3]) # standard error of slope
                rsquared.postmin <- p.postmin["r2"]
                fitFlag.postmin <- TRUE
              }
              else {
                # of if(length(candidates.postmin) > 0)
                y0_lm.postmin = NA
                mumax.postmin = NA
                mu.se.postmin = NA
                lag.postmin = NA
                tmax_start.postmin = NA
                tmax_end.postmin = NA
                ndx.postmin = NA
                rsquared.postmin = NA
                fitFlag.postmin = FALSE
              }
            } # if (any(ret.postmin.check[, 5] >= R2 & abs(ret.postmin.check[, 6]) <= RSD))
            else {
              y0_lm.postmin = NA
              mumax.postmin = NA
              mu.se.postmin = NA
              lag.postmin = NA
              tmax_start.postmin = NA
              tmax_end.postmin = NA
              ndx.postmin = NA
              rsquared.postmin = NA
              fitFlag.postmin = FALSE
            }

            # get local deriv2-maximum before mumax
            # premin.ndx <- maxima[(maxima - tp.ext[1]) <= 0][which.max(maxima[(maxima - tp.ext[1]) <= 0])]
            premin.ndx <- maxima[which.min(abs(maxima - tp.ext[1]))]
            # extract linear regression results before pre-mumax turning point
            ret.premin <- ret[ret$time <= obs$time[premin.ndx-h],]
            #Consider only slopes that span at least fit.dY
            ret.premin <- ret.premin[ret.premin[, "dY"] >= fit.dY, ]
            # remove indices included in extended mumax regression
            ret.premin <- ret.premin[!(ret.premin[,1] %in% tp.ext),]
            # Consider only positive slopes
            ret.premin <- ret.premin[ret.premin[, "slope"] > 0, ]
            ## Determine index of window with maximum growth rate, iterate until regression is found that meets R2 and RSD criterion
            success <- FALSE
            # apply min.density to list of linear regressions
            ret.premin.check <- ret.premin[ret.premin[, 8] >= min.density, ]
            # apply max.density to list of linear regressions
            if(!is.na(max.density)){
              ret.premin.check <- ret.premin.check[ret.premin.check[,"data"] <= max.density, ] # consider only slopes up to defined max.density
            }
            # apply max. time to list of linear regeressions
            if(!is.na(tmax)){
              ret.premin.check <- ret.premin.check[ret.premin.check[,"time"] <= tmax, ] # consider only slopes up to defined t0
            }
            if (any(ret.premin.check[, 5] >= R2 &
                    abs(ret.premin.check[, 6]) <= RSD)) {
              while (!success) {
                index.max <- which.max(ret.premin.check[, 3])
                if (ret.premin.check[index.max, 5] >= R2 &&
                    abs(ret.premin.check[index.max, 6]) <= RSD &&
                    !is.na(ret.premin.check[index.max, 6])) {
                  # prerequisites for suitable µmax candidate: R2 and RSD
                  slope.max.premin <- ret.premin.check[index.max, 3]
                  success <- TRUE
                } else {
                  ret.premin.check <- ret.premin.check[-index.max,]
                }
              }
              index.max.ret.premin <- ret.premin[which(ret.premin[, 3] == slope.max.premin), 1] # index of maximum slope in fit table
              slope.quota <- quota * slope.max.premin
              candidates.premin <- ret.premin[which(
                ret.premin[, 3] >= slope.quota &
                  ret.premin[, 5] >= 0.98 * R2 &
                  abs(ret.premin[, 6]) <= 1.02 * RSD &
                  ret.premin[, 7] >= t0), 1]

              #consider only candidate windows next to index.max.ret.premin
              candidate_intervals.premin <- split(candidates.premin, cumsum(c(1, diff(candidates.premin) != 1)))
              if (any(unlist(candidate_intervals.premin) %in% index.max.ret.premin)) {
                ndx.premin <-  as.numeric(which(sapply(
                  candidate_intervals.premin,
                  FUN = function(X)
                    any(X %in% index.max.ret.premin)
                )))

                candidates.premin <-
                  candidate_intervals.premin[[ndx.premin]]
              }


              if (length(candidates.premin) > 0) {
                #perform linear regression with candidate data points
                tp.premin <- seq(min(candidates.premin), max(candidates.premin) + h - 1)
                if (control$log.y.lin == TRUE) {
                  m.premin <- lm_window(obs$time, obs$ylog, min(tp.premin), length(tp.premin)) # linear model
                } else {
                  m.premin <- lm_window(obs$time, obs$data, min(tp.premin), length(tp.premin)) # linear model
                }
                p.premin  <- c(lm_parms(m.premin), n = length(tp.premin)) # # slope equation parameters (linear model)
              } else {
                p.premin <- c(
                  a = 0,
                  b = 0,
                  se = 0,
                  r2 = 0,
                  cv = 0,
                  n = 0
                )
                m = NULL
              }

              if (length(candidates.premin) > 0) {
                ## get time window of exponential fit
                tmax_start.premin <- obs$time[tp.premin[1]]
                tmax_end.premin <- obs$time[tp.premin[length(tp.premin)]]
                t_turn.premin <- obs$time[premin.ndx]
                y0_lm.premin    <- unname(coef(m.premin)[1]) # y-intercept of tangent
                mumax.premin <- unname(coef(m.premin)[2])

                ## estimate lag phase between first and second growth phase
                if (control$log.y.lin == TRUE) {
                  lambda.premin <- (obs$ylog[1] - y0_lm.premin) / mumax.premin
                } else {
                  lambda.premin <- (obs$data[1] - y0_lm.premin) / mumax.premin
                }

                # correct y0 values for Ln(y(t)/y0)
                if (control$log.y.lin == TRUE) {
                  y0_lm.premin <-  obs$data[1] * exp(y0_lm.premin)
                }

                # get indices of time points used in linear fit
                ndx.premin <- seq(min(match(ret[match(candidates.premin, ret[,1]), "time"], time.in)),
                                  max(match(ret[match(candidates.premin, ret[,1]), "time"], time.in)) + h -
                                    1)
                mu.se.premin <- as.numeric(p.premin[3]) # standard error of slope
                rsquared.premin <- p.premin["r2"]
                fitFlag.premin <- TRUE
              }
              else {
                # of if(length(candidates.premin) > 0)
                y0_lm.premin = NA
                mumax.premin = NA
                mu.se.premin = NA
                lag.premin = NA
                tmax_start.premin = NA
                tmax_end.premin = NA
                ndx.premin = NA
                rsquared.premin = NA
                fitFlag.premin = FALSE
              }
            } # if (any(ret.premin.check[, 5] >= R2 & abs(ret.premin.check[, 6]) <= RSD))
            else{
              y0_lm.premin = NA
              mumax.premin = NA
              mu.se.premin = NA
              lag.premin = NA
              tmax_start.premin = NA
              tmax_end.premin = NA
              ndx.premin = NA
              rsquared.premin = NA
              fitFlag.premin = FALSE
            }

            # Choose regression before or after mumax as second growth phase based on second growth rate
            if(!is.na(mumax.premin) && !is.na(mumax.postmin)){
              mumax2 <- ifelse(mumax.premin > mumax.postmin, mumax.premin, mumax.postmin)
              y0_2 <- ifelse(mumax.premin > mumax.postmin, y0_lm.premin, y0_lm.postmin)
              lag2 <- ifelse(mumax.premin > mumax.postmin, lambda.premin, lambda.postmin)
              tmax2_start <- ifelse(mumax.premin > mumax.postmin, tmax_start.premin, tmax_start.postmin)
              tmax2_end <- ifelse(mumax.premin > mumax.postmin, tmax_end.premin, tmax_end.postmin)
              rsquared2 <- ifelse(mumax.premin > mumax.postmin, rsquared.premin, rsquared.postmin)
              ndx2 <- if(mumax.premin > mumax.postmin){ndx.premin} else{ndx.postmin}
              t_turn <- ifelse(mumax.premin > mumax.postmin, t_turn.premin, t_turn.postmin)
              fitFlag2 <- TRUE
            } else if (!is.na(mumax.premin)){
              mumax2 <- mumax.premin
              y0_2 <- y0_lm.premin
              lag2 <- lambda.premin
              tmax2_start <- tmax_start.premin
              tmax2_end <- tmax_end.premin
              t_turn <- t_turn.premin
              rsquared2 <- rsquared.premin
              ndx2 <- ndx.premin
              fitFlag2 <- TRUE
            } else if (!is.na(mumax.postmin)){
              mumax2 <- mumax.postmin
              y0_2 <- y0_lm.postmin
              lag2 <- lambda.postmin
              tmax2_start <- tmax_start.postmin
              tmax2_end <- tmax_end.postmin
              t_turn <- t_turn.postmin
              rsquared2 <- rsquared.postmin
              ndx2 <- ndx.postmin
              fitFlag2 <- TRUE
            } else {
              mumax2 <- NA
              y0_2 <- NA
              lag2 <- NA
              tmax2_start <- NA
              tmax2_end <- NA
              t_turn <- NA
              rsquared2 <- NA
              ndx2 <- NA
              fitFlag2 <- FALSE
            }
          } # if(control$biphasic)
          else{
            mumax2 <- NA
            y0_2 <- NA
            lag2 <- NA
            tmax2_start <- NA
            tmax2_end <- NA
            t_turn <- NA
            rsquared2 <- NA
            ndx2 <- NA
            fitFlag2 <- FALSE
          }
        } # if(any(ret.check[,5] >= R2 & ret.check[,6] <= RSD))
        else{
          gcFitLinear <-
            list(
              raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
              log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
            )
          class(gcFitLinear) <- "gcFitLinear"
          if (!control$suppress.messages)
            message(
              paste0(
                "No linear fit in accordance with the chosen parameters identified with an R2 value of >= ",
                R2,
                " and an RSD of <= ",
                RSD,
                "."
              )
            )
          return(gcFitLinear)
        }
      } # else of if(nrow(ret.check)<2)
    } # else of if(nrow(ret)<2)
  } # if(N >= 3)
  else {
    message("Not enough observations in the dataset to perform linear fit. growth.gcFitLinear requires at least 3 data points between t0 and the time of maximum density.")
    gcFitLinear <- list(raw.time = time.in, raw.data = data.in, filt.time = obs$time, filt.data = obs$data,
                        log.data = obs$ylog, gcID = gcID, FUN = grow_exponential, fit = NA, par = c(
                          y0 = NA, y0_lm = NA, mumax = 0, mu.se = NA, lag = NA, tmax_start = NA, tmax_end = NA,
                          t_turn = NA, mumax2 = NA, y0_lm2 = NA, lag2 = NA, tmax2_start = NA,
                          tmax2_end = NA), ndx = NA, ndx2 = NA, quota = quota, rsquared = NA, rsquared2 = NA, control = control, fitFlag = FALSE, fitFlag2 = FALSE
    )
    class(gcFitLinear) <- "gcFitLinear"
    return(gcFitLinear)
  }

  gcFitLinear <- list(
    raw.time = time.in,
    raw.data = data.in,
    filt.time = obs$time,
    filt.data = obs$data,
    log.data = obs$ylog,
    gcID = gcID,
    FUN = if (control$log.y.lin == TRUE) {
      grow_exponential
    } else {
      grow_linear
    },
    fit = m,
    par = c(
      y0 = y0_data,
      dY = max(obs$data)-obs$data[1],
      A = max(obs$data),
      y0_lm = y0_lm,
      mumax = mumax,
      tD = log(2)/mumax,
      mu.se = mu.se,
      lag = lambda,
      tmax_start = tmax_start,
      tmax_end = tmax_end,
      t_turn = t_turn,
      mumax2 = mumax2,
      tD2 = log(2)/mumax2,
      y0_lm2 = y0_2,
      lag2 = lag2,
      tmax2_start = tmax2_start,
      tmax2_end = tmax2_end
    ),
    ndx = ndx,
    ndx2 = ndx2,
    quota = quota,
    rsquared = p["r2"],
    rsquared2 = rsquared2,
    control = control,
    fitFlag = fitFlag,
    fitFlag2 = fitFlag2,
    reliable = NULL
  )

  class(gcFitLinear) <- "gcFitLinear"

  invisible(gcFitLinear)
}

#' Perform a bootstrap on density vs. time data followed by spline fits for each resample
#'
#' \code{growth.gcBootSpline} resamples the density-time value pairs in a dataset with replacement and performs a spline fit for each bootstrap sample.
#'
#' @param time Vector of the independent variable (usually: time).
#' @param data Vector of dependent variable (usually: density values).
#' @param gcID (Character) The name of the analyzed sample.
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#'
#' @family growth fitting functions
#'
#' @return A \code{gcBootSpline} object containing a distribution of growth parameters and
#'   a \code{gcFitSpline} object for each bootstrap sample. Use \code{\link{plot.gcBootSpline}}
#'   to visualize all bootstrapping splines as well as the distribution of
#'   physiological parameters.
#' \item{raw.time}{Raw time values provided to the function as \code{time}.}
#' \item{raw.data}{Raw density data provided to the function as \code{data}.}
#' \item{gcID}{(Character) Identifies the tested sample.}
#' \item{boot.time}{Table of time values per column, resulting from each spline fit of the bootstrap.}
#' \item{boot.data}{Table of density values per column, resulting from each spline fit of the bootstrap.}
#' \item{boot.gcSpline}{List of \code{gcFitSpline} object, created by \code{\link{growth.gcFitSpline}} for each resample of the bootstrap.}
#' \item{lambda}{Vector of estimated lambda (lag time) values from each bootstrap entry.}
#' \item{mu}{Vector of estimated mu (maximum growth rate) values from each bootstrap entry.}
#' \item{A}{Vector of estimated A (maximum density) values from each bootstrap entry.}
#' \item{integral}{Vector of estimated integral values from each bootstrap entry.}
#' \item{bootFlag}{(Logical) Indicates the success of the bootstrapping operation.}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @export
#'
growth.gcBootSpline <- function (time, data, gcID = "undefined", control = growth.control())
{
  if (methods::is(control) != "grofit.control")
    stop("control must be of class grofit.control!")
  if (control$nboot.gc == 0)
    stop("Number of bootstrap samples is zero! See growth.control()")
  time <- as.vector(as.numeric(as.matrix(time)))[!is.na(time)][!is.na(data)]
  data <- as.vector(as.numeric(as.matrix(data)))[!is.na(time)][!is.na(data)]
  if (length(time) != length(data))
    stop("gcBootSpline: length of input vectors differ!")
  if(control$log.y.spline == TRUE){
    bad.values <- (is.na(time)) | (is.na(data)) |
      (!is.numeric(time)) | (!is.numeric(data) | (data<=0))
  } else {
    bad.values <- (is.na(time)) | (is.na(data)) | is.infinite(data) |
      (!is.numeric(time)) | (!is.numeric(data))
  }
  if (TRUE %in% bad.values) {
    if (control$neg.nan.act == FALSE) {
      time <- time[!bad.values]
      data <- data[!bad.values]
    }
    else stop("Bad values in gcBootSpline")
  }
  if (length(data) < 6) {
    warning("gcBootSpline: There is not enough valid data. Must have at least 6 unique values!")
    gcBootSpline <- list(raw.time = time, raw.data = data,
                         gcID = gcID, boot.time = NA, boot.data = NA, boot.gcSpline = NA,
                         lambda = NA, mu = NA, A = NA, integral = NA, bootFlag = FALSE,
                         control = control)
    class(gcBootSpline) <- "gcBootSpline"
    return(gcBootSpline)
  }
  A <- NA
  mu <- NA
  lambda <- NA
  integral <- NA
  dY <- NA
  boot.y <- array(NA, c(control$nboot.gc, length(time)))
  boot.x <- array(NA, c(control$nboot.gc, length(time)))
  nonpara <- list()
  control.change <- control
  control.change$fit.opt <- "s"
  if (control$nboot.gc > 0) {
    for (j in 1:control$nboot.gc) {
      choose <- sort(sample(1:length(time), length(time), replace = TRUE))
      while (length(unique(choose)) < 5) {
        choose <- sort(sample(1:length(time), length(time),
                              replace = TRUE))
      }
      time.cur <- time[choose]
      data.cur <- data[choose]
      if(stats::IQR(time.cur) > 0){
        nonpara[[j]] <- growth.gcFitSpline(time.cur, data.cur, gcID,
                                           control.change)
        if(nonpara[[j]]$fitFlag==FALSE | is.na(nonpara[[j]]$fit.data[1])){
          boot.y[j, 1:length(nonpara[[j]]$fit.data)] <- rep(NA, length(nonpara[[j]]$fit.data))
          boot.x[j, 1:length(nonpara[[j]]$fit.time)] <- rep(NA, length(nonpara[[j]]$fit.time))
        }
        else{
          boot.y[j, 1:length(nonpara[[j]]$fit.data)] <- nonpara[[j]]$fit.data
          boot.x[j, 1:length(nonpara[[j]]$fit.time)] <- nonpara[[j]]$fit.time
        }
        lambda[j] <- nonpara[[j]]$parameters$lambda
        mu[j] <- nonpara[[j]]$parameters$mu
        A[j] <- nonpara[[j]]$parameters$A
        integral[j] <- nonpara[[j]]$parameters$integral
        dY[j] <- nonpara[[j]]$parameters$dY
      }
    }
    lambda[which(!is.finite(lambda))] <- NA
    mu[which(!is.finite(lambda))] <- NA
    A[which(!is.finite(lambda))] <- NA
    integral[which(!is.finite(lambda))] <- NA
    dY[which(!is.finite(dY))] <- NA
    if (control$clean.bootstrap == TRUE) {
      lambda[which(lambda < 0)] <- NA
      mu[which(mu < 0)] <- NA
      A[which(A < 0)] <- NA
      integral[which(integral < 0)] <- NA
      dY[which(dY < 0)] <- NA
    }
  }
  if (control$log.x.gc == TRUE) {
    bad.values <- (time <= -1)
    if (TRUE %in% bad.values) {
      time <- time[!bad.values]
      data <- data[!bad.values]
    }
    time.log <- log(1 + time)
  }
  if (control$log.y.spline == TRUE) {
    data.log <- log(data/data[1])
    bad.values <- (data.log <= 0)
    if (TRUE %in% bad.values) {
      time <- time[!bad.values]
      data.log <- data.log[!bad.values]
    }
  }
  gcBootSpline <- list(
    raw.time = if (control$log.x.gc == TRUE) {
      time.log
    } else {
      time
    },
    raw.data = if (control$log.y.spline == TRUE) {
      data.log
    } else {
      data
    },
    gcID = gcID,
    boot.time = boot.x,
    boot.data = boot.y,
    boot.gcSpline = nonpara,
    lambda = lambda,
    mu = mu,
    A = A,
    dY = dY,
    integral = integral,
    bootFlag = TRUE,
    control = control
  )
  class(gcBootSpline) <- "gcBootSpline"
  gcBootSpline
}

#' Perform smooth spline fits on response vs. concentration data
#'
#' \code{growth.drFit} serves to determine dose-response curves on every condition in a
#' dataset. The response parameter can be chosen from every physiological parameter in a
#' \code{gcTable} table which is obtained via \code{\link{growth.gcFit}}. \code{\link{growth.drFit}}
#' calls the functions \code{\link{growth.drFitSpline}} and \code{\link{growth.drBootSpline}} to
#' generate a table with estimates for EC50 and respecting statistics.
#'
#' @param gcTable A dataframe containing the data for the dose-response curve estimation. Such table of class \code{gcTable} can be obtained by running \code{\link{growth.gcFit}}.
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#'
#' @family growth fitting functions
#'
#' @return An object of class \code{drFit}.
#' \item{raw.data}{Data that passed to the function as \code{gcTable}.}
#' \item{drTable}{Dataframe containing condition identifiers, fit options, and results of the dose-response analysis.}
#' \item{drBootSplines}{List of all \code{drBootSpline} objects generated by the call of \code{\link{growth.drBootSpline}} for each distinct experiment.}
#' \item{drFittedSplines}{List of all \code{drFitSpline} objects generated by the call of \code{\link{growth.drFitSpline}} for each distinct experiment.}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#'
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @export
#'
growth.drFit <- function (gcTable, control = growth.control())
{
  if (methods::is(control) != "grofit.control" && methods::is(control) != "fl.control")
    stop("control must be of class grofit.control or fl.control!")
  EC50.table <- NULL
  all.EC50 <- NA
  if(is.character(control$dr.parameter)){
    dr.parameter <- match(control$dr.parameter, colnames(gcTable))
  }
  FitData <- gcTable[!is.na(gcTable[,1]), ]
  table.tests <- table((FitData[, 1])[which((FitData[,
                                                     4] == TRUE) & (is.na(FitData[, dr.parameter]) ==
                                                                      FALSE))])
  distinct <- names(table.tests)
  EC50 <- list()
  EC50.boot <- list()
  validdata <- cbind(as.character(distinct), table.tests)
  colnames(validdata) <- c("TestID", "Number")
  rownames(validdata) <- rep("     ", times = dim(validdata)[1])
  if (control$suppress.messages == FALSE) {
    cat("\n")
    cat("=== EC 50 Estimation ==============================\n")
    cat("---------------------------------------------------\n")
    cat("--> Checking data ...\n")
    cat(paste("--> Number of distinct tests found:", as.character(length(distinct))),
        "\n")
    cat("--> Valid datasets per test: \n")
    print(validdata, quote = FALSE)
  }
  if (TRUE %in% (table.tests < control$dr.have.atleast)) {
    cat(paste("Warning: following tests have not enough ( <",
              as.character(control$dr.have.atleast - 1), ") datasets:\n"))
    cat(distinct[(table.tests < control$dr.have.atleast)])
    cat("These tests will not be regarded\n")
    distinct <- distinct[table.tests >= control$dr.have.atleast]
  }
  if ((length(distinct)) == 0) {
    cat(paste("There are no tests having enough ( >", as.character(control$dr.have.atleast -
                                                                     1), ") datasets!\n"))
  }
  else {
    skip <- c()

    for (i in 1:length(distinct)) {
      conc <- factor((FitData[, 3])[which(FitData[, 1] ==
                                            distinct[i])])
      test <- (as.numeric(FitData[, dr.parameter]))[FitData[, 1] == distinct[i]]
      conc <- as.factor(as.character(conc[!is.na(test)]))
      test <- test[!is.na(test)]

      if(length(levels(conc)) <4){
        message(paste0(distinct[i], " does not have enough unique concentrations. A condition must have at least 4 different concentrations to be considered for dose-response analysis."))
        skip <- c(skip, i)
        next
      }

      names(test) <- rep(names(FitData)[dr.parameter], length(test))
      drID <- distinct[i]
      if(control$dr.method == "spline"){
        EC50[[i]] <- growth.drFitSpline(conc, test, drID, control)
        if (control$nboot.dr > 0) {
          EC50.boot[[i]] <- growth.drBootSpline(conc, test, drID,
                                                control)
        }
        else {
          EC50.boot[[i]] <- list(raw.time = conc, raw.data = test,
                                 drID = drID, boot.x = NA, boot.y = NA, boot.drSpline = NA,
                                 ec50.boot = NA, bootFlag = FALSE, control = control)
          class(EC50.boot[[i]]) <- "drBootSpline"
        }
      } else {
        EC50[[i]] <- growth.drFitModel(conc, test, drID, control)
      }
      description <- data.frame(Test = distinct[i], log.x = control$log.x.dr,
                                log.y = control$log.y.dr, Samples = control$nboot.dr)
      if(control$dr.method == "spline")
        out.row <- cbind(description, summary.drFitSpline(EC50[[i]]),
                       summary.drBootSpline(EC50.boot[[i]]))
      else
        out.row <- cbind(description, summary.drFitModel(EC50[[i]]))
      EC50.table <- rbind(as.data.frame(EC50.table), out.row)
      class(EC50.table) <- c("drTable", "list")
    }
  }
  if(exists("skip") && !is.null(skip)){
    distinct <- distinct[-skip]
    EC50 <- EC50[-skip]
    EC50.boot <- EC50.boot[-skip]
  }
  if(control$dr.method == "spline"){
    names(EC50) <- names(EC50.boot) <- distinct
    drFit <- list(raw.data = FitData, drTable = EC50.table,
                  drBootSplines = EC50.boot, drFittedSplines = EC50, control = control)
  } else {
    names(EC50) <- distinct
    drFit <- list(raw.data = FitData, drTable = EC50.table,
                  drFittedModels = EC50, control = control)
  }

  class(drFit) <- "drFit"
  drFit
}

#' Perform a smooth spline fit on response vs. concentration data of a single sample to determine the EC50.
#'
#' \code{growth.drFitSpline} performs a smooth spline fit determines the EC50 as the concentration
#' at the half-maximum value of the response parameter of the spline.
#'
#' @param conc Vector of concentration values.
#' @param test Vector of response parameter values of the same length as \code{conc}.
#' @param drID (Character) The name of the analyzed condition
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#'
#' @family dose-response analysis functions
#'
#' @return A \code{drFitSpline} object.
#' \item{raw.conc}{Raw data provided to the function as \code{conc}.}
#' \item{raw.test}{Raw data for the response parameter provided to the function as \code{test}.}
#' \item{drID}{(Character) Identifies the tested condition}
#' \item{fit.conc}{Fitted concentration values.}
#' \item{fit.test}{Fitted response values.}
#' \item{spline}{\code{smooth.spline} object generated by the \code{\link{smooth.spline}} function.}
#' \item{spline.low}{\code{x} and {y} values of \code{\link{lowess}} spline fit on raw data. Used to call \code{\link{smooth.spline}}.}
#' \item{parameters}{List of parameters estimated from dose response curve fit.}
#' \itemize{
#' \item \code{EC50}: {Concentration at half-maximal response.}
#' \item \code{yEC50}: {Response value related to EC50.}
#' \item \code{EC50.orig}: {EC50 value in original scale, if a transformation was applied.}
#' \item \code{yEC50.orig}: {Response value for EC50 in original scale, if a transformation was applied.}
#' }
#' \item{fitFlag}{(Logical) Indicates whether a spline could fitted successfully to data.}
#' \item{reliable}{(Logical) Indicates whether the performed fit is reliable (to be set manually).}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#' Use \code{\link{plot.drFitSpline}} to visualize the spline fit.
#'
#' @details During the spline fit with \code{\link{smooth.spline}}, higher weights are
#'   assigned to data points with a concentration value of 0, as well as to x-y pairs with
#'   a response parameter value of 0 and pairs at concentration values before
#'   zero-response parameter values.
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @export
#'
growth.drFitSpline <- function(conc, test, drID = "undefined", control = growth.control())
{
  if (methods::is(control) != "grofit.control" && methods::is(control) != "fl.control")
    stop("control must be of class grofit.control or fl.control!")
  test.nm <- names(test)[1]
  test <- as.vector(as.numeric(as.matrix(test)))
  conc <- as.vector(as.numeric(as.matrix(conc)))
  if (is.vector(conc) == FALSE || is.vector(test) == FALSE)
    stop("growth.drFitSpline: dose or response data must be a vector !")
  if (control$neg.nan.act == FALSE) {
    missings <- is.na(conc) | is.na(test) | !is.numeric(conc) |
      !is.numeric(test)
    conc <- conc[!missings]
    test <- test[!missings]
    negs <- (conc < 0) | (test < 0)
    conc <- conc[!negs]
    test <- test[!negs]
  }
  else {
    if (sum(is.na(conc) | is.na(test)))
      stop("growth.drFitSpline: NA values encountered. Program terminated")
    if ((sum((conc < 0)) > 0) | (sum((test < 0)) > 0))
      stop("growth.drFitSpline: Negative values encountered. Program terminated")
    if ((FALSE %in% is.numeric(conc)) || (FALSE %in% is.numeric(test)))
      stop("growth.drFitSpline: Non numeric values encountered. Program terminated")
  }
  if (length(test) < 6) {
    warning("drFitSpline: There is not enough valid data. Must have at least 6 unique values!")
    drFitSpline <- list(raw.conc = conc, raw.test = test,
                        drID = drID, fit.conc = NA, fit.test = NA, spline = NA,
                        parameters = list(EC50 = NA, yEC50 = NA, EC50.orig = NA,
                                          yEC50.orig = NA, test = test.nm), fitFlag = FALSE, reliable = NULL,
                        control = control)
    class(drFitSpline) <- "drFitSpline"
    return(drFitSpline)
  }
  if (length(test) < control$dr.have.atleast) {
    warning("drFitSpline: number of valid data points is below the number specified in 'dr.have.atleast'. See growth.control().")
    drFitSpline <- list(raw.conc = conc, raw.test = test,
                        drID = drID, fit.conc = NA, fit.test = NA, spline = NA,
                        parameters = list(EC50 = NA, yEC50 = NA, EC50.orig = NA,
                                          yEC50.orig = NA, test = test.nm), fitFlag = FALSE, reliable = NULL,
                        control = control)
    class(drFitSpline) <- "drFitSpline"
    return(drFitSpline)
  }
  if (control$log.x.dr == TRUE)
    conc.log <- log(conc + 1)
  if (control$log.y.dr == TRUE)
    test.log <- log(test + 1)
  if (control$log.x.dr == TRUE) {
    conc.fit <- log(conc + 1)
  }
  else {
    conc.fit <- conc
  }
  if (control$log.y.dr == TRUE) {
    test.fit <- log(test + 1)
  }
  else {
    test.fit <- test
  }
  spltest <- NULL
  fitFlag <- TRUE

  # perform first lowess spline fit followed by smooth.spline onf lowess results
  try(spltest.low <- lowess(conc.fit, test.fit, f = 0.1), silent = T)
  try(spltest.low$y <- spltest.low$y[!duplicated(spltest.low$x)], silent = T)
  try(spltest.low$x <- spltest.low$x[!duplicated(spltest.low$x)], silent = T)

  # assign high weights to concentration values of 0, response values of 0, and response values immediately before 0
  weights <- sapply(1:(length(spltest.low$x)-1), function(i) ifelse(spltest.low$x[[i]] == 0 | spltest.low$y[[i]]==0 | spltest.low$y[[i+1]]==0, 1, 0.05))
  weights <- c(weights, ifelse(spltest.low$x[length(spltest.low$x)] == 0 | spltest.low$y[length(spltest.low$y)]==0, 1, 0.05))

  try(spltest.smooth <- smooth.spline(spltest.low$x, spltest.low$y, spar = control$smooth.dr, keep.data = FALSE, w = weights), silent = T)

  if (!exists("spltest.smooth") || is.null(spltest.smooth) == TRUE) {
    cat("Spline could not be fitted in dose-response analysis!\n")
    fitFlag <- FALSE
    if (is.null(control$smooth.dr) == TRUE) {
      cat("This might be caused by usage of smoothing parameter 'smooth.dr = NULL'. Re-running the function might solve the problem. If not, please specify 'smooth.dr'.\n")
    }
    drFitSpline <- list(raw.conc = conc, raw.test = test,
                        drID = drID, fit.conc = NA, fit.test = NA, spline = NA,
                        parameters = list(EC50 = NA, yEC50 = NA, EC50.orig = NA,
                                          yEC50.orig = NA, test = test.nm), fitFlag = FALSE, reliable = NULL,
                        control = control)
    class(drFitSpline) <- "drFitSpline"
    return(drFitSpline)
  }
  conc.min <- min(conc.fit)
  conc.max <- max(conc.fit)
  c.pred <- seq(conc.min, conc.max, length.out = 1000)
  ytest <- stats::predict(spltest.smooth, c.pred)
  yEC.test <- (max(ytest$y) - min(ytest$y))/2 + min(ytest$y)
  last.test <- max(ytest$y)
  kec.test <- 1
  for (k in 1:(length(c.pred) - 1)) {
    d1 <- (ytest$y[k] - yEC.test)
    d2 <- (ytest$y[k + 1] - yEC.test)
    if (((d1 <= 0) && (d2 >= 0)) | ((d1 >= 0) && (d2 <= 0))) {
      kec.test <- k
      break
    }
  }
  EC.test <- c.pred[kec.test]
  if (control$suppress.messages == FALSE) {
    cat("\n\n=== Dose response curve estimation ================\n")
    cat("--- EC 50 -----------------------------------------\n")
    cat(paste("-->", as.character(drID)))
    cat("\n")
    cat(paste(c("xEC50", "yEC50"), c(EC.test, yEC.test)))
  }
  if ((control$log.x.dr == TRUE) && (control$log.y.dr == FALSE)) {
    if (control$suppress.messages == FALSE) {
      cat("\n--> Original scale \n")
      cat(paste(c("xEC50", "yEC50"), c(exp(EC.test) - 1,
                                       yEC.test)))
    }
    EC.orig <- c(exp(EC.test) - 1, yEC.test)
  }
  else {
    if ((control$log.x.dr == FALSE) && (control$log.y.dr ==
                                        TRUE)) {
      if (control$suppress.messages == FALSE) {
        cat("\n--> Original scale \n")
        cat(paste(c("xEC50", "yEC50"), c(EC.test, exp(yEC.test) -
                                           1)))
      }
      EC.orig <- c(EC.test, exp(yEC.test) - 1)
    }
    else {
      if ((control$log.x.dr == TRUE) && (control$log.y.dr ==
                                         TRUE)) {
        if (control$suppress.messages == FALSE) {
          cat("\n--> Original scale \n")
          cat(paste(c("xEC50", "yEC50"), c(exp(EC.test) -
                                             1, exp(yEC.test) - 1)))
        }
        EC.orig <- c(exp(EC.test) - 1, exp(yEC.test) -
                       1)
      }
      else {
        if ((control$log.x.dr == FALSE) && (control$log.y.dr ==
                                            FALSE)) {
          EC.orig <- c(EC.test, yEC.test)
        }
      }
    }
  }
  if (control$suppress.messages == FALSE) {
    cat("\n\n\n")
  }
  drFitSpline <- list(raw.conc = conc, raw.test = test, drID = drID,
                      fit.conc = ytest$x, fit.test = ytest$y, spline = spltest.smooth, spline.low = spltest.low,
                      parameters = list(EC50 = EC.test[1], yEC50 = yEC.test,
                                        EC50.orig = EC.orig[1], yEC50.orig = EC.orig[2], test = test.nm),
                      fitFlag = fitFlag, reliable = NULL, control = control)
  class(drFitSpline) <- "drFitSpline"
  drFitSpline
}


#' Fit various models to response vs. concentration data of a single sample to determine the EC50.
#'
#' @param conc Vector of concentration values.
#' @param test Vector of response parameter values of the same length as \code{conc}.
#' @param drID (Character) The name of the analyzed condition
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#'
#' @return A \code{drFitModel} object.
#' @references Christian Ritz, Florent Baty, Jens C. Streibig, Daniel Gerhard (2015). _Dose-Response Analysis Using R_. PLoS ONE 10(12): e0146021. DOI: 10.1371/journal.pone.0146021
#' @export
#'
#' @import drc
growth.drFitModel <- function(conc, test, drID = "undefined", control = growth.control())
{
  if (methods::is(control) != "grofit.control" && methods::is(control) != "fl.control")
    stop("growth.drFitModel: control must be of class grofit.control or fl.control!")
  test.nm <- control$dr.parameter
  if(test.nm != control$dr.parameter && control$dr.parameter != "mu.linfit")
    test.nm <-  control$dr.parameter
  test <- as.vector(as.numeric(as.matrix(test)))
  conc <- as.vector(as.numeric(as.matrix(conc)))
  models <- control$dr.model
  if (is.vector(conc) == FALSE || is.vector(test) == FALSE)
    stop("growth.drFitModel: dose or response data must be a vector !")
  if (control$neg.nan.act == FALSE) {
    missings <- is.na(conc) | is.na(test) | !is.numeric(conc) |
      !is.numeric(test)
    conc <- conc[!missings]
    test <- test[!missings]
    negs <- (conc < 0) | (test < 0)
    conc <- conc[!negs]
    test <- test[!negs]
  }
  else {
    if (sum(is.na(conc) | is.na(test)))
      stop("growth.drFitModel: NA values encountered. Program terminated")
    if ((sum((conc < 0)) > 0) | (sum((test < 0)) > 0))
      stop("growth.drFitModel: Negative values encountered. Program terminated")
    if ((FALSE %in% is.numeric(conc)) || (FALSE %in% is.numeric(test)))
      stop("growth.drFitModel: Non numeric values encountered. Program terminated")
  }
  if (length(test) < 6) {
    warning("growth.drFitModel: There is not enough valid data. Must have at least 6 unique values!")
    drFitModel <- list(raw.conc = conc, raw.test = test,
                        drID = drID, fit.conc = NA, fit.test = NA, spline = NA,
                        parameters = list(EC50 = NA, yEC50 = NA, EC50.orig = NA,
                                          yEC50.orig = NA, test = test.nm), fitFlag = FALSE, reliable = NULL,
                        control = control)
    class(drFitModel) <- "drFitModel"
    return(drFitModel)
  }
  if (length(test) < control$dr.have.atleast) {
    warning("growth.drFitModel: number of valid data points is below the number specified in 'dr.have.atleast'. See growth.control().")
    drFitModel <- list(raw.conc = conc, raw.test = test,
                        drID = drID, fit.conc = NA, fit.test = NA, spline = NA,
                        parameters = list(EC50 = NA, yEC50 = NA, EC50.orig = NA,
                                          yEC50.orig = NA, test = test.nm), fitFlag = FALSE, reliable = NULL,
                        control = control)
    class(drFitModel) <- "drFitModel"
    return(drFitModel)
  }
  # Perform model fits
  model.fits <- list()
  for(i in 1:length(models)){
    model.fits[[i]] <- try(
      invisible(
        drc::drm(
          test~as.numeric(as.character(conc)),
          fct = get(models[i])()
        )
      ), silent = T
    )
  }
  models <- models[unlist(lapply(1:length(model.fits), function(x) class(model.fits[[x]]))) != "try-error"]
  model.fits <- model.fits[unlist(lapply(1:length(model.fits), function(x) class(model.fits[[x]]))) != "try-error"]
  names(model.fits) <- models

  # select best fitting model
  model.AIC <- lapply(1:length(model.fits), function(x) AIC(model.fits[[x]]))
  names(model.AIC) <- models

  best.model.ndx <- which.min(unlist(model.AIC))
  best.model.nm <- models[best.model.ndx]
  best.model <- model.fits[[best.model.ndx]]

  # get EC50 value
  if(any(grep("BC", best.model.nm)) ){
    ec50 <- drc::ED(best.model, 50, lower = 0.1, upper = 1000, interval = "fls" )
  } else if(any(grep("NEC", best.model.nm))){
    ec50 <- best.model$coefficients[4]
  } else {
    ec50 <- drc::ED(best.model, 50, interval = "delta")
  }
  # get response at ec50
  dataList <- best.model[["dataList"]]
  dose <- dataList[["dose"]]

  concgrid <- seq(min(dose), max(dose), length = 200)
  respgrid <- best.model$curve[[1]](concgrid)

  y.ec50 <- drc::PR(object = best.model, xVec = ec50[1])

  # Plot best fit
  drc.models <- drc::getMeanFunctions(display = FALSE)
  drc.models.nm <- c(unlist(lapply(1:length(drc.models), function(x) drc.models[[x]][1])), "NEC.4")
  drc.models.descr <- c(unlist(lapply(1:length(drc.models), function(x) drc.models[[x]][2])), "model for estimation of\nno effect concentration (NEC)")

  drFitModel <- list(raw.conc = conc, raw.test = test,
                     drID = drID, fit.conc = concgrid, fit.test = respgrid, model = best.model,
                     parameters = list(EC50 = ec50, yEC50 = y.ec50, test = test.nm, model = best.model.nm), fitFlag = TRUE, reliable = NULL,
                     control = control)

  class(drFitModel) <- "drFitModel"
  return(drFitModel)
}

#' Perform a smooth spline fit on response vs. concentration data of a single sample
#'
#' \code{growth.drBootSpline} resamples the values in a dataset with replacement and performs a spline fit for each bootstrap sample to determine the EC50.
#'
#' @param conc Vector of concentration values.
#' @param test Vector of response parameter values of the same length as \code{conc}.
#' @param drID (Character) The name of the analyzed sample.
#' @param control A \code{grofit.control} object created with \code{\link{growth.control}}, defining relevant fitting options.
#'
#' @family dose-response analysis functions
#'
#' @return An object of class \code{drBootSpline} containing a distribution of growth parameters and
#'   a \code{drFitSpline} object for each bootstrap sample. Use \code{\link{plot.drBootSpline}}
#'   to visualize all bootstrapping splines as well as the distribution of EC50.
#' \item{raw.conc}{Raw data provided to the function as \code{conc}.}
#' \item{raw.test}{Raw data for the response parameter provided to the function as \code{test}.}
#' \item{drID}{(Character) Identifies the tested condition.}
#' \item{boot.conc}{Table of concentration values per column, resulting from each spline fit of the bootstrap.}
#' \item{boot.test}{Table of response values per column, resulting from each spline fit of the bootstrap.}
#' \item{boot.drSpline}{List containing all \code{drFitSpline} objects generated by the call of \code{\link{growth.drFitSpline}}.}
#' \item{ec50.boot}{Vector of estimated EC50 values from each bootstrap entry.}
#' \item{ec50y.boot}{Vector of estimated response at EC50 values from each bootstrap entry.}
#' \item{BootFlag}{(Logical) Indicates the success of the bootstrapping operation.}
#' \item{control}{Object of class \code{grofit.control} containing list of options passed to the function as \code{control}.}
#'
#' @references Matthias Kahm, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost Ludwig, Maik Kschischo (2010). _grofit: Fitting Biological Growth Curves with R_. Journal of Statistical Software, 33(7), 1-21. DOI: 10.18637/jss.v033.i07
#'
#' @export
#'
growth.drBootSpline <- function (conc, test, drID = "undefined", control = growth.control())
{
  test <- as.vector(as.numeric(as.matrix(test)))
  conc <- as.vector(as.numeric(as.matrix(conc)))
  if (is.vector(conc) == FALSE || is.vector(test) == FALSE)
    stop("Need concentration and treatment !")
  if (methods::is(control) != "grofit.control" && methods::is(control) != "fl.control")
    stop("control must be of class grofit.control or fl.control!")
  if (control$nboot.dr == 0)
    stop("Number of bootstrap samples is zero! See ?growth.control or ?fl.control.")
  if (control$neg.nan.act == FALSE) {
    missings <- is.na(conc) | is.na(test) | !is.numeric(conc) |
      !is.numeric(test)
    conc <- conc[!missings]
    test <- test[!missings]
    negs <- (conc < 0) | (test < 0)
    conc <- conc[!negs]
    test <- test[!negs]
  }
  # Test if there are enough unique x-values (conc) to perform spline fit
  if (length(unique(conc)) < 4){
    warning("drBootSpline: There are not enough concentration values. Must have at least 4 unique values!")
    drBootSpline <- list(raw.conc = conc, raw.test = test,
                         drID = drID, boot.conc = NA, boot.test = NA, boot.drSpline = NA,
                         ec50.boot = NA, ec50y.boot = NA, bootFlag = FALSE, control = control)
    class(drBootSpline) <- "drBootSpline"
    return(drBootSpline)
  }
  else {
    if (sum(is.na(conc) | is.na(test)))
      stop("NA values encountered. Program terminated")
    if ((sum((conc < 0)) > 0) | (sum((test < 0)) > 0))
      stop("growth.drFitSpline: Negative values encountered. Program terminated")
    if ((FALSE %in% is.numeric(conc)) || (FALSE %in% is.numeric(test)))
      stop("growth.drFitSpline: Non numeric values encountered. Program terminated")
  }
  if (length(test) < 6) {
    warning("drBootSpline: There is not enough valid data. Must have at least 6 unique values!")
    drBootSpline <- list(raw.conc = conc, raw.test = test,
                         drID = drID, boot.conc = NA, boot.test = NA, boot.drSpline = NA,
                         ec50.boot = NA, ec50y.boot = NA, bootFlag = FALSE, control = control)
    class(drBootSpline) <- "drBootSpline"
    return(drBootSpline)
  }
  if (length(test) < control$dr.have.atleast) {
    warning("drBootSpline: number of valid data points is below the number specified in 'dr.have.atleast'. See growth.control().")
    drBootSpline <- list(raw.conc = conc, raw.test = test,
                         drID = drID, boot.conc = NA, boot.test = NA, boot.drSpline = NA,
                         ec50.boot = NA, ec50y.boot = NA, bootFlag = FALSE, control = control)
    class(drBootSpline) <- "drBootSpline"
    return(drBootSpline)
  }

  # /// transformation of data...
  if (control$log.x.dr == TRUE)
    conc.log <- log(conc + 1)
  if (control$log.y.dr == TRUE)
    test.log <- log(test + 1)
  if (control$log.x.dr == TRUE)
    conc.boot <- log(conc + 1)
  else conc.boot <- conc
  if (control$log.y.dr == TRUE)
    test.boot <- log(test + 1)
  else test.boot <- test

  # /// Initialize some variables
  boot.x <- array(NA, c(control$nboot.dr, 1000))
  boot.y <- array(NA, c(control$nboot.dr, 1000))
  ECtest.boot <- seq(0, 0, length.out = control$nboot.dr)
  y.EC50.boot <- seq(0, 0, length.out = control$nboot.dr)
  splinefit <- list()
  sa <- seq(1, length(conc.boot))

  # /// begin bootstrapping
  for (b in 1:control$nboot.dr) {
    s <- sample(sa, length(conc.boot), replace = TRUE)
    s.conc <- conc.boot[s]
    while (length(unique(s.conc)) < 4) {
      s <- sample(sa, length(conc.boot), replace = TRUE)
      s.conc <- conc.boot[s]
    }
    s.test <- test.boot[s]
    spltest <- NULL
    control.changed <- control
    control.changed$suppress.messages <- TRUE
    splinefit[[b]] <- growth.drFitSpline(s.conc, s.test, drID, control.changed)
    spltest <- splinefit[[b]]$spline
    boot.x[b, 1:length(splinefit[[b]]$fit.conc)] <- splinefit[[b]]$fit.conc
    boot.y[b, 1:length(splinefit[[b]]$fit.test)] <- splinefit[[b]]$fit.test
    if (is.null(spltest) == TRUE) {
      cat("Spline could not be fitted in dose-response analysis!!\n")
      if (is.null(control$smooth.dr) == TRUE) {
        cat("This might be caused by usage of smoothing parameter 'smooth.dr = NULL'.\n")
      }
      stop("Error in drBootSpline")
    }
    ECtest.boot[b] <- splinefit[[b]]$parameters$EC50
    y.EC50.boot[b] <- splinefit[[b]]$parameters$yEC50
  }
  ECtest.boot[which(!is.finite(ECtest.boot))] <- NA
  if (control$clean.bootstrap == TRUE)
    ECtest.boot[which(ECtest.boot < 0)] <- NA
  m.test <- mean(ECtest.boot, na.rm = TRUE)
  s.test <- sd(ECtest.boot, na.rm = TRUE)
  if (control$suppress.messages == FALSE) {
    cat("=== Bootstrapping of dose response curve ==========\n")
    cat("--- EC 50 -----------------------------------------\n")
    cat("\n")
    cat(paste("Mean  : ", as.character(m.test), "StDev : ",
              as.character(s.test), "\n"))
    cat(paste("90% CI: ", as.character(c(m.test - 1.645 *
                                           s.test / control$nboot.dr, m.test + 1.645 * s.test / control$nboot.dr))))
    cat("\n")
    cat(paste("95% CI: ", as.character(c(m.test - 1.96 *
                                           s.test / control$nboot.dr, m.test + 1.96 * s.test / control$nboot.dr))))
    cat("\n\n")
  }
  EC50 <- data.frame(EC50.boot = m.test, EC50.sd = s.test,
                     CI.90.lo = m.test - 1.645 * s.test, CI.90.up = m.test +
                       1.645 * s.test, CI.95.lo = m.test - 1.96 * s.test,
                     CI.95.up = m.test + 1.96 * s.test)
  if (control$log.x.dr == TRUE && control$suppress.messages ==
      FALSE) {
    cat("\n")
    cat("--- EC 50 in original scale -----------------------\n")
    cat("\n")
    cat(paste("Mean  : ", as.character(exp(m.test) - 1),
              "\n"))
    cat(paste("90% CI: ", as.character(c(exp(m.test - 1.645 *
                                               s.test / control$nboot.dr) - 1, exp(m.test + 1.645 * s.test / control$nboot.dr) - 1))))
    cat("\n")
    cat(paste("95% CI: ", as.character(c(exp(m.test - 1.96 *
                                               s.test / control$nboot.dr) - 1, exp(m.test + 1.96 * s.test / control$nboot.dr) - 1))))
    cat("\n\n")
  }
  drBootSpline <- list(raw.conc = conc, raw.test = test, drID = drID,
                       boot.conc = boot.x, boot.test = boot.y, boot.drSpline = splinefit,
                       ec50.boot = ECtest.boot, ec50y.boot = y.EC50.boot, bootFlag = TRUE, control = control)
  class(drBootSpline) <- "drBootSpline"
  drBootSpline
}

lm_parms <- function (m)
{
  suppressWarnings(sm <- summary(m))
  if(dim(sm$coefficients)[1] >1 ){
    a <- sm$coefficients[1, 1]
    b <- sm$coefficients[2, 1]
    b.se <- sm$coefficients[2, 2]
    r2 <- sm$r.squared
    c(a = a, b = b, b.se = b.se, r2 = r2, b.rsd = b.se/b)
  }
  else {
    c(a = NA, b = NA, b.se = NA, r2 = NA, b.rsd = NA)
  }
}

lm_window <- function (x, y, i0, h = 5)
{
  x <- x[i0 - 1 + (1:h)]
  y <- y[i0 - 1 + (1:h)]
  m <- theil_sen_regression(y ~ x)
  return(m)
}

grow_exponential <- function (time, parms)
{
  if (is.null(names(parms))) {
    y0 <- parms[1]
    mumax <- parms[2]
  }
  else {
    y0 <- parms["y0_lm"]
    mumax <- ifelse(!is.na(parms["max_slope"]), parms["max_slope"], parms["mumax"])
  }
  y <- y0 * exp(mumax * time)
  return(as.matrix(data.frame(time = time, y = y)))
}

grow_linear <- function(time, parms)
{
  if (length(parms)>2) {
    y0 <- parms[4]
    mumax <- parms[5]
  }
  else {
    y0 <- parms[1]
    mumax <- parms[2]
  }
  y <- y0 + mumax * time
  return(as.matrix(data.frame(time = time, y = y)))
}

low.integrate <- function (x, y)
{
  if (is.vector(x) == FALSE || is.vector(y) == FALSE)
    stop("low.integrate: two vectors x and y are needed !")
  if (length(x) != length(y))
    stop("low.integrate: x and y have to be of same length !")
  spline <- NULL
  try(spline <- smooth.spline(x, y, keep.data = FALSE))
  if (is.null(spline)){
    try(spline <- smooth.spline(x, y, keep.data = FALSE, spar = 0.1))
  }
  if (is.null(spline)){
    try(spline <- smooth.spline(x, y, keep.data = FALSE, spar = 0.1))
    warning("Spline could not be fitted to data!")
    stop()
  }
  f <- function(t) {
    p <- stats::predict(spline, t)
    f <- p$y
  }
  low.integrate <- integrate(f, min(x), max(x))$value
}

logistic <- function (time, A, mu, lambda, addpar = NULL)
{
  A <- A[1]
  mu <- mu[1]
  lambda <- lambda[1]
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  y <- A/(1 + exp(4 * mu * (lambda - time)/A + 2))
  logistic <- y
}

initlogistic <- function (time, y, A, mu, lambda)
{
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(y) == FALSE)
    stop("Need numeric vector for: y")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  A <- max(y)
  mu <- mu[1]
  lambda <- lambda[1]
  initlogistic <- list(A = A, mu = mu, lambda = lambda, addpar = NULL)
}

initrichards <- function (time, y, A, mu, lambda)
{
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(y) == FALSE)
    stop("Need numeric vector for: y")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  nu <- 0.1
  A <- max(y)
  mu <- mu[1]
  lambda <- lambda[1]
  initrichards <- list(A = A, mu = mu, lambda = lambda, addpar = nu)
}

inithuang <- function(time, y, A, mu, lambda)
{
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(y) == FALSE)
    stop("Need numeric vector for: y")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  y0 <- y[1]
  A <- max(y)
  mu <- mu[1]
  lambda <- lambda[1]
  inithuang <- list(A = A, mu = mu, lambda = lambda, addpar = y0)
}

huang <- function (time, A, mu, lambda, addpar)
{
  A <- A[1]
  mu <- mu[1]
  lambda <- lambda[1]
  y0 <- addpar[1]
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")

  y <- y0 + A - log( exp(y0) + (exp(A) - exp(y0)) * exp(-mu*(time+0.25*log((1+exp(-4*(time-lambda)))/(1+exp(4*lambda))))) )
  huang <- y
}

 baranyi <- function(time, A, mu, lambda, addpar)
 {
   A <- A[1]
   mu <- mu[1]
   lambda <- lambda[1]
   y0 <- addpar[1]
   if (is.numeric(time) == FALSE)
     stop("Need numeric vector for: time")
   if (is.numeric(mu) == FALSE)
     stop("Need numeric vector for: mu")
   if (is.numeric(lambda) == FALSE)
     stop("Need numeric vector for: lambda")
   if (is.numeric(A) == FALSE)
     stop("Need numeric vector for: A")
   B <- time + 1/mu * log(exp(-mu * time) + exp(-mu * lambda) - exp(-mu * (time + lambda)))
   y <- y0 + mu * B - log(1 + (exp(mu * B) - 1)/exp(A - y0))
   baranyi <- y
 }

 initbaranyi <- function(time, y, A, mu, lambda)
 {
   if (is.numeric(time) == FALSE)
     stop("Need numeric vector for: time")
   if (is.numeric(y) == FALSE)
     stop("Need numeric vector for: y")
   if (is.numeric(mu) == FALSE)
     stop("Need numeric vector for: mu")
   if (is.numeric(lambda) == FALSE)
     stop("Need numeric vector for: lambda")
   if (is.numeric(A) == FALSE)
     stop("Need numeric vector for: A")
   y0 <- y[1]
   A <- max(y)
   mu <- mu[1]
   lambda <- lambda[1]
   initbaranyi <- list(A = A, mu = mu, lambda = lambda, addpar = y0)
 }


# liquori <- function (time, A, mu, addpar)
# {
#   A <- A[1]
#   mu <- mu[1]
#   y0 <- addpar[1]
#   t_a1 <- addpar[2]
#   t_a2 <- addpar[3]
#   t_b1 <- addpar[4]
#   t_b2 <- addpar[5]
#   x <- addpar[6]
#   if (is.numeric(time) == FALSE)
#     stop("Need numeric vector for: time")
#   if (is.numeric(mu) == FALSE)
#     stop("Need numeric vector for: mu")
#   if (is.numeric(y0) == FALSE)
#     stop("Need numeric vector for: y0")
#   if (is.numeric(A) == FALSE)
#     stop("Need numeric vector for: A")
#   if (is.numeric(t_a1) == FALSE)
#     stop("Need numeric vector for: addpar[2]")
#   if (is.numeric(t_a2) == FALSE)
#     stop("Need numeric vector for: addpar[3]")
#   if (is.numeric(t_b1) == FALSE)
#     stop("Need numeric vector for: addpar[4]")
#   if (is.numeric(t_b2) == FALSE)
#     stop("Need numeric vector for: addpar[5]")
#   t <- time
#   y1 <- (1-exp(-(t/t_a1)))/(1-exp(-(t/t_a1))+exp(-(t/t_a2)))
#   y2 <- (1-exp(-(t/t_b1)))/(1-exp(-(t/t_b1))+exp(-(t/t_b2)))
#   y <- y0 + A*x*y1 + A*(1-x)*y2
#   liquori <- y
# }

richards <- function (time, A, mu, lambda, addpar)
{
  A <- A[1]
  mu <- mu[1]
  lambda <- lambda[1]
  nu <- addpar[1]
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  if (is.numeric(nu) == FALSE)
    stop("Need numeric vector for: addpar[1]")
  y <- A * (1 + nu * exp(1 + nu) * exp(mu * (1 + nu)^(1 + 1/nu) *
                                         (lambda - time)/A))^(-1/nu)
  richards <- y
}

gompertz <- function (time, A, mu, lambda, addpar = NULL)
{
  A <- A[1]
  mu <- mu[1]
  lambda <- lambda[1]
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  e <- exp(1)
  y <- A * exp(-exp(mu * e * (lambda - time)/A + 1))
  gompertz <- y
}

initgompertz <- function (time, y, A, mu, lambda)
{
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(y) == FALSE)
    stop("Need numeric vector for: y")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  A <- max(y)
  mu <- mu[1]
  lambda <- lambda[1]
  initgompertz <- list(A = A, mu = mu, lambda = lambda, addpar = NULL)
}

initgompertz.exp <- function (time, y, A, mu, lambda)
{
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(y) == FALSE)
    stop("Need numeric vector for: y")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  alpha <- 0.1
  t_shift <- max(time)/10
  A <- max(y)
  mu <- mu[1]
  lambda <- lambda[1]
  initgompertz.exp <- list(A = A, mu = mu, lambda = lambda,
                           addpar = c(alpha, t_shift))
}

gompertz.exp <- function (time, A, mu, lambda, addpar)
{
  A <- A[1]
  mu <- mu[1]
  lambda <- lambda[1]
  alpha <- addpar[1]
  t_shift <- addpar[2]
  if (is.numeric(time) == FALSE)
    stop("Need numeric vector for: time")
  if (is.numeric(mu) == FALSE)
    stop("Need numeric vector for: mu")
  if (is.numeric(lambda) == FALSE)
    stop("Need numeric vector for: lambda")
  if (is.numeric(A) == FALSE)
    stop("Need numeric vector for: A")
  if (is.numeric(alpha) == FALSE)
    stop("Need numeric vector for: addpar[1]")
  if (is.numeric(t_shift) == FALSE)
    stop("Need numeric vector for: addpar[2]")
  e <- exp(1)
  y <- A * exp(-exp(mu * e * (lambda - time)/A + 1)) + A *
    exp(alpha * (time - t_shift))
  gompertz.exp <- y
}

#' Generate a grouped results table for linear fits with average and standard deviations
#'
#' @param gcTable An object of class \code{gcTable}
#'
#' @return A data frame with grouped linear fit results
#' @export
#'
table_group_growth_linear <- function(gcTable)
{
  nm <- as.character(paste(gcTable[,1], gcTable[,2], gcTable[,3], sep = " | "))

  ndx.filt.rep <- unique(lapply(1:length(nm), function(i)which(gsub("\\| ([[:punct:]]|[[:digit:]]|NA)+ \\|", "|", nm) %in% (paste(unlist(str_split(nm[i], " \\| "))[-2], collapse = " | ")))))
  filter.ls <- list()
  for(j in 1:length(ndx.filt.rep)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.rep[[j]]), function(i) ndx.filt.rep[[j]][grep(paste0("^",
                                                                                                           gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
                                                                                                                unlist(str_split(nm[ndx.filt.rep[[j]][i]], " \\| "))[1]),
                                                                                                           ".+[[:space:]]",
                                                                                                           unlist(str_split(nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                           "$"), nm[ndx.filt.rep[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = F)
  ndx.filt <- ndx.filt[lapply(ndx.filt, length)>0]

  names(ndx.filt) <- unlist(lapply(1:length(ndx.filt), function (x) nm[ndx.filt[[x]][1]]) )

  # calculate average param values
  mu.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "mu.linfit", param2 = "mu2.linfit")
  mu.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "mu.linfit", param2 = "mu2.linfit")

  tD.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tD.linfit", param2 = "tD2.linfit")
  tD.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tD.linfit", param2 = "tD2.linfit")

  lambda.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "lambda.linfit", param2 = "lambda2.linfit")
  lambda.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "lambda.linfit", param2 = "lambda2.linfit")

  dY.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "dY.linfit", param2 = "dY2.linfit")
  dY.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "dY.linfit", param2 = "dY2.linfit")

  A.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "A.linfit", param2 = "A2.linfit")
  A.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "A.linfit", param2 = "A2.linfit")

  tmu.start.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tmu.start.linfit", param2 = "tmu2.start.linfit")
  tmu.start.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tmu.start.linfit", param2 = "tmu2.start.linfit")

  tmu.end.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tmu.end.linfit", param2 = "tmu2.end.linfit")
  tmu.end.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tmu.end.linfit", param2 = "tmu2.end.linfit")


  labels <- gsub(" \\| NA", "", gsub(" \\| [[:digit:]]+ \\| ", " | ", names(ndx.filt))) # condition names

  table_linear_group <- data.frame("Sample|Conc." = labels,
                                   "\u00b5<sub>max</sub>" = paste0(mu.mean,
                                                              unlist(lapply(1:length(mu.mean), function (x)
                                                                ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                                         mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                                       "", " \u00B1 ") ) ),
                                                              unlist(lapply(1:length(mu.mean), function (x)
                                                                ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                                         mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                                       "", mu.sd[x])))),

                                   "t<sub>D</sub>" = paste0(tD.mean,
                                                            unlist(lapply(1:length(tD.mean), function (x)
                                                              ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                                       tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                                     "", " \u00B1 ") ) ),
                                                            unlist(lapply(1:length(tD.mean), function (x)
                                                              ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                                       tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                                     "", tD.sd[x])))),
                                   "\u03bb" =  paste0(lambda.mean,
                                                 unlist(lapply(1:length(lambda.mean), function (x)
                                                   ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                            lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(lambda.mean), function (x)
                                                   ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                            lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                          "", lambda.sd[x])))),
                                   "\u0394Y" = paste0(dY.mean,
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", dY.sd[x])))),
                                   "y<sub>max</sub>" = paste0(A.mean,
                                                              unlist(lapply(1:length(A.mean), function (x)
                                                                ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                                         A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                                       "", " \u00B1 ") ) ),
                                                              unlist(lapply(1:length(A.mean), function (x)
                                                                ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                                         A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                                       "", A.sd[x])))),
                                   "t<sub>start</sub><br>(\u00b5<sub>max</sub>)" = paste0(tmu.start.mean,
                                                                                     unlist(lapply(1:length(tmu.start.mean), function (x)
                                                                                       ifelse(tmu.start.mean[x] == 0 || tmu.start.mean[x] == "" || tmu.start.mean[x] == "" ||
                                                                                                tmu.start.sd[x] == 0 || tmu.start.sd[x] == "" || tmu.start.sd[x] == "",
                                                                                              "", " \u00B1 ") ) ),
                                                                                     unlist(lapply(1:length(tmu.start.mean), function (x)
                                                                                       ifelse(tmu.start.mean[x] == 0 || tmu.start.mean[x] == "" || tmu.start.mean[x] == "" ||
                                                                                                tmu.start.sd[x] == 0 || tmu.start.sd[x] == "" || tmu.start.sd[x] == "",
                                                                                              "", tmu.start.sd[x])))),

                                   "t<sub>end</sub><br>(\u00b5<sub>max</sub>)" = paste0(tmu.end.mean,
                                                                                   unlist(lapply(1:length(tmu.end.mean), function (x)
                                                                                     ifelse(tmu.end.mean[x] == 0 || tmu.end.mean[x] == "" || tmu.end.mean[x] == "" ||
                                                                                              tmu.end.sd[x] == 0 || tmu.end.sd[x] == "" || tmu.end.sd[x] == "",
                                                                                            "", " \u00B1 ") ) ),
                                                                                   unlist(lapply(1:length(tmu.end.mean), function (x)
                                                                                     ifelse(tmu.end.mean[x] == 0 || tmu.end.mean[x] == "" || tmu.end.mean[x] == "" ||
                                                                                              tmu.end.sd[x] == 0 || tmu.end.sd[x] == "" || tmu.end.sd[x] == "",
                                                                                            "", tmu.end.sd[x])))),
                                   stringsAsFactors = F, check.names = F)
  return(table_linear_group)
}

#' Generate a grouped results table for spline fits with average and standard deviations
#'
#' @param gcTable An object of class \code{gcTable}
#'
#' @return A data frame with grouped spline fit results
#' @export
#'
table_group_growth_spline <- function(gcTable)
{
  nm <- as.character(paste(gcTable[,1], gcTable[,2], gcTable[,3], sep = " | "))

  ndx.filt.rep <- unique(lapply(1:length(nm), function(i)which(gsub("\\| ([[:punct:]]|[[:digit:]]|NA)+ \\|", "|", nm) %in% (paste(unlist(str_split(nm[i], " \\| "))[-2], collapse = " | ")))))
  filter.ls <- list()
  for(j in 1:length(ndx.filt.rep)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.rep[[j]]), function(i) ndx.filt.rep[[j]][grep(paste0("^",
                                                                                                           gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
                                                                                                                unlist(str_split(nm[ndx.filt.rep[[j]][i]], " \\| "))[1]),
                                                                                                           ".+[[:space:]]",
                                                                                                           unlist(str_split(nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                           "$"), nm[ndx.filt.rep[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = F)
  ndx.filt <- ndx.filt[lapply(ndx.filt, length)>0]

  names(ndx.filt) <- unlist(lapply(1:length(ndx.filt), function (x) nm[ndx.filt[[x]][1]]) )

  # calculate average param values
  mu.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "mu.spline", param2 = "mu2.spline")
  mu.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "mu.spline", param2 = "mu2.spline")

  tD.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tD.spline", param2 = "tD2.spline")
  tD.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tD.spline", param2 = "tD2.spline")

  lambda.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "lambda.spline", param2 = "lambda2.spline")
  lambda.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "lambda.spline", param2 = "lambda2.spline")

  dY.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "dY.spline", param2 = "dY2.spline")
  dY.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "dY.spline", param2 = "dY2.spline")

  A.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "A.spline", param2 = "A2.spline")
  A.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "A.spline", param2 = "A2.spline")

  tmax.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tmax.spline", param2 = "tmax2.spline")
  tmax.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tmax.spline", param2 = "tmax2.spline")

  labels <- gsub(" \\| NA", "", gsub(" \\| [[:digit:]]+ \\| ", " | ", names(ndx.filt))) # condition names

  table_spline_group <- data.frame("Sample|Conc." = labels,
                                   "\u00b5<sub>max</sub>" = paste0(mu.mean,
                                                              unlist(lapply(1:length(mu.mean), function (x)
                                                                ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                                         mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                                       "", " \u00B1 ") ) ),
                                                              unlist(lapply(1:length(mu.mean), function (x)
                                                                ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                                         mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                                       "", mu.sd[x])))),
                                   "t<sub>D</sub>" = paste0(tD.mean,
                                                            unlist(lapply(1:length(tD.mean), function (x)
                                                              ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                                       tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                                     "", " \u00B1 ") ) ),
                                                            unlist(lapply(1:length(tD.mean), function (x)
                                                              ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                                       tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                                     "", tD.sd[x])))),
                                   "\u03bb" = paste0(lambda.mean,
                                                unlist(lapply(1:length(lambda.mean), function (x)
                                                  ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                           lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                         "", " \u00B1 ") ) ),
                                                unlist(lapply(1:length(lambda.mean), function (x)
                                                  ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                           lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                         "", lambda.sd[x])))),
                                   "y<sub>max</sub>" = paste0(A.mean,
                                                              unlist(lapply(1:length(A.mean), function (x)
                                                                ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                                         A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                                       "", " \u00B1 ") ) ),
                                                              unlist(lapply(1:length(A.mean), function (x)
                                                                ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                                         A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                                       "", A.sd[x])))),
                                   "\u0394Y" = paste0(dY.mean,
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", dY.sd[x])))),
                                   "t<sub>max</sub>" = paste0(tmax.mean,
                                                              unlist(lapply(1:length(tmax.mean), function (x)
                                                                ifelse(tmax.mean[x] == 0 || tmax.mean[x] == "" || tmax.mean[x] == "" ||
                                                                         tmax.sd[x] == 0 || tmax.sd[x] == "" || tmax.sd[x] == "",
                                                                       "", " \u00B1 ") ) ),
                                                              unlist(lapply(1:length(tmax.mean), function (x)
                                                                ifelse(tmax.mean[x] == 0 || tmax.mean[x] == "" || tmax.mean[x] == "" ||
                                                                         tmax.sd[x] == 0 || tmax.sd[x] == "" || tmax.sd[x] == "",
                                                                       "", tmax.sd[x])))),
                                   check.names = F)
  return(table_spline_group)
}

#' Generate a grouped results table for parametric fits with average and standard deviations
#'
#' @param gcTable An object of class \code{gcTable}
#'
#' @return A data frame with grouped model fit results
#' @export
#'
table_group_growth_model <- function(gcTable)
{
  nm <- as.character(paste(gcTable[,1], gcTable[,2], gcTable[,3], sep = " | "))

  ndx.filt.rep <- unique(lapply(1:length(nm), function(i)which(gsub("\\| ([[:punct:]]|[[:digit:]]|NA)+ \\|", "|", nm) %in% (paste(unlist(str_split(nm[i], " \\| "))[-2], collapse = " | ")))))
  filter.ls <- list()
  for(j in 1:length(ndx.filt.rep)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.rep[[j]]), function(i) ndx.filt.rep[[j]][grep(paste0("^",
                                                                                                           gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",
                                                                                                                unlist(str_split(nm[ndx.filt.rep[[j]][i]], " \\| "))[1]),
                                                                                                           ".+[[:space:]]",
                                                                                                           unlist(str_split(nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                           "$"), nm[ndx.filt.rep[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = F)
  ndx.filt <- ndx.filt[lapply(ndx.filt, length)>0]

  names(ndx.filt) <- unlist(lapply(1:length(ndx.filt), function (x) nm[ndx.filt[[x]][1]]) )

  # calculate average param values
  mu.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "mu.model")
  mu.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "mu.model")

  tD.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tD.model")
  tD.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "tD.model")

  lambda.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "lambda.model")
  lambda.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "lambda.model")

  dY.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "dY.model")
  dY.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "dY.model")

  A.mean <- get_avg_param(table = gcTable, ndx.rep = ndx.filt, param1 = "A.model")
  A.sd <- get_sd_param(table = gcTable, ndx.rep = ndx.filt, param1 = "A.model")

  labels <- gsub(" \\| NA", "", gsub(" \\| [[:digit:]]+ \\| ", " | ", names(ndx.filt))) # condition names

  table_model_group <- data.frame("Sample|Conc." = labels,
                                   "\u00b5<sub>max</sub>" = paste0(mu.mean,
                                                              unlist(lapply(1:length(mu.mean), function (x)
                                                                ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                                         mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                                       "", " \u00B1 ") ) ),
                                                              unlist(lapply(1:length(mu.mean), function (x)
                                                                ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                                         mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                                       "", mu.sd[x])))),
                                   "t<sub>D</sub>" = paste0(tD.mean,
                                                            unlist(lapply(1:length(tD.mean), function (x)
                                                              ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                                       tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                                     "", " \u00B1 ") ) ),
                                                            unlist(lapply(1:length(tD.mean), function (x)
                                                              ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                                       tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                                     "", tD.sd[x])))),
                                   "\u03bb" = paste0(lambda.mean,
                                                unlist(lapply(1:length(lambda.mean), function (x)
                                                  ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                           lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                         "", " \u00B1 ") ) ),
                                                unlist(lapply(1:length(lambda.mean), function (x)
                                                  ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                           lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                         "", lambda.sd[x])))),
                                   "y<sub>max</sub>" = paste0(A.mean,
                                                              unlist(lapply(1:length(A.mean), function (x)
                                                                ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                                         A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                                       "", " \u00B1 ") ) ),
                                                              unlist(lapply(1:length(A.mean), function (x)
                                                                ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                                         A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                                       "", A.sd[x])))),
                                   "\u0394Y" = paste0(dY.mean,
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", dY.sd[x])))),
                                   check.names = F)
  return(table_model_group)
}


