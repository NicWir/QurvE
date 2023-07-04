#' Generate a grouped results table for linear fits with average and standard deviations
#'
#' @param flTable An object of class \code{flTable}
#' @param html (Logical) Should column headers contain html formatting?
#'
#' @return A data frame with grouped linear fit results. Empty cells indicate that no reliable fit could be determined.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # load example dataset
#' input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
#'                    data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
#'                    csvsep = "\t",
#'                    csvsep.fl = "\t")
#'
#' # Run workflow
#' res <- fl.workflow(grodata = input, ec50 = FALSE, fit.opt = "l",
#'                    x_type = "time", norm_fl = TRUE,
#'                    dr.parameter = "max_slope.spline",
#'                    suppress.messages = TRUE,
#'                    parallelize = FALSE)
#'
#' table_group_fluorescence_linear(res$flFit$flTable)
#'
#' # with HTML formatting
#' DT::datatable(table_group_fluorescence_linear(res$flFit$flTable, html = TRUE),
#'               escape = FALSE) # Do not escape HTML entities
#' }
table_group_fluorescence_linear <- function(flTable, html = FALSE)
{
  nm <- as.character(paste(flTable[,1], flTable[,2], flTable[,3], sep = " | "))

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
  ndx.filt <- unlist(filter.ls, recursive = FALSE)
  ndx.filt <- ndx.filt[lapply(ndx.filt, length)>0]

  names(ndx.filt) <- unlist(lapply(1:length(ndx.filt), function (x) nm[ndx.filt[[x]][1]]) )
  # calculate average param values
  max_slope.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "max_slope.linfit", param2 = "max_slope2.linfit")
  max_slope.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "max_slope.linfit", param2 = "max_slope2.linfit")

  lambda.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "lambda.linfit", param2 = "lambda2.linfit")
  lambda.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "lambda.linfit", param2 = "lambda2.linfit")

  dY.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "dY.linfit", param2 = "dY2.linfit")
  dY.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "dY.linfit", param2 = "dY2.linfit")

  A.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "A.linfit", param2 = "A2.linfit")
  A.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "A.linfit", param2 = "A2.linfit")

  tmu.start.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "x.mu.start.linfit", param2 = "x.mu2.start.linfit")
  tmu.start.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "x.mu.start.linfit", param2 = "x.mu2.start.linfit")

  tmu.end.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "x.mu.end.linfit", param2 = "x.mu2.end.linfit")
  tmu.end.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "x.mu.end.linfit", param2 = "x.mu2.end.linfit")

  labels <- gsub(" \\| NA", "", gsub(" \\| [[:digit:]]+ \\| ", " | ", names(ndx.filt))) # condition names

  table_linear_group <- data.frame("Sample|Conc." = labels,
                                   "slope_max" = paste0(max_slope.mean,
                                                        unlist(lapply(1:length(max_slope.mean), function (x)
                                                          ifelse(max_slope.mean[x] == 0 || max_slope.mean[x] == "" || max_slope.mean[x] == "" ||
                                                                   max_slope.sd[x] == 0 || max_slope.sd[x] == "" || max_slope.sd[x] == "",
                                                                 "", " \u00B1 ") ) ),
                                                        unlist(lapply(1:length(max_slope.mean), function (x)
                                                          ifelse(max_slope.mean[x] == 0 || max_slope.mean[x] == "" || max_slope.mean[x] == "" ||
                                                                   max_slope.sd[x] == 0 || max_slope.sd[x] == "" || max_slope.sd[x] == "",
                                                                 "", max_slope.sd[x])))),

                                   "lagtime" =  paste0(lambda.mean,
                                                       unlist(lapply(1:length(lambda.mean), function (x)
                                                         ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                  lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                                "", " \u00B1 ") ) ),
                                                       unlist(lapply(1:length(lambda.mean), function (x)
                                                         ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                  lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                                "", lambda.sd[x])))),
                                   "dY" = paste0(dY.mean,
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", dY.sd[x])))),
                                   "Y_max" = paste0(A.mean,
                                                    unlist(lapply(1:length(A.mean), function (x)
                                                      ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                               A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                             "", " \u00B1 ") ) ),
                                                    unlist(lapply(1:length(A.mean), function (x)
                                                      ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                               A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                             "", A.sd[x])))),
                                   "x_start(mumax)" = paste0(tmu.start.mean,
                                                             unlist(lapply(1:length(tmu.start.mean), function (x)
                                                               ifelse(tmu.start.mean[x] == 0 || tmu.start.mean[x] == "" || tmu.start.mean[x] == "" ||
                                                                        tmu.start.sd[x] == 0 || tmu.start.sd[x] == "" || tmu.start.sd[x] == "",
                                                                      "", " \u00B1 ") ) ),
                                                             unlist(lapply(1:length(tmu.start.mean), function (x)
                                                               ifelse(tmu.start.mean[x] == 0 || tmu.start.mean[x] == "" || tmu.start.mean[x] == "" ||
                                                                        tmu.start.sd[x] == 0 || tmu.start.sd[x] == "" || tmu.start.sd[x] == "",
                                                                      "", tmu.start.sd[x])))),

                                   "x_end(mumax)" = paste0(tmu.end.mean,
                                                           unlist(lapply(1:length(tmu.end.mean), function (x)
                                                             ifelse(tmu.end.mean[x] == 0 || tmu.end.mean[x] == "" || tmu.end.mean[x] == "" ||
                                                                      tmu.end.sd[x] == 0 || tmu.end.sd[x] == "" || tmu.end.sd[x] == "",
                                                                    "", " \u00B1 ") ) ),
                                                           unlist(lapply(1:length(tmu.end.mean), function (x)
                                                             ifelse(tmu.end.mean[x] == 0 || tmu.end.mean[x] == "" || tmu.end.mean[x] == "" ||
                                                                      tmu.end.sd[x] == 0 || tmu.end.sd[x] == "" || tmu.end.sd[x] == "",
                                                                    "", tmu.end.sd[x])))),
                                   stringsAsFactors = FALSE, check.names = FALSE)

  if(html == TRUE){
    colnames(table_linear_group) <- c("Sample|Conc.", "slope<sub>max</sub>", "lagtime",
                                      "dY", "y<sub>max</sub>", "x<sub>start</sub><br>(mu<sub>max</sub>)",
                                      "x<sub>end</sub><br>(mu<sub>max</sub>)")
  }

  return(table_linear_group)
}


#' Generate a grouped results table for spline fits with average and standard deviations
#'
#' @param flTable An object of class \code{flTable}
#' @param html (Logical) Should column headers contain html formatting?
#'
#' @return A data frame with grouped spline fit results. Empty cells indicate that no reliable fit could be determined.
#'
#' @export
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
#'                    data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
#'                    csvsep = "\t",
#'                    csvsep.fl = "\t")
#'
#' # Run workflow
#' res <- fl.workflow(grodata = input, ec50 = FALSE, fit.opt = "s",
#'                    x_type = "time", norm_fl = TRUE,
#'                    dr.parameter = "max_slope.spline",
#'                    suppress.messages = TRUE,
#'                    parallelize = FALSE)
#'
#' table_group_fluorescence_spline(res$flFit$flTable)
#'
#' # with HTML formatting
#' DT::datatable(table_group_fluorescence_spline(res$flFit$flTable, html = TRUE),
#'               escape = FALSE) # Do not escape HTML entities
#'
table_group_fluorescence_spline <- function(flTable, html = FALSE)
{
  nm <- as.character(paste(flTable[,1], flTable[,2], flTable[,3], sep = " | "))

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
  ndx.filt <- unlist(filter.ls, recursive = FALSE)
  ndx.filt <- ndx.filt[lapply(ndx.filt, length)>0]

  names(ndx.filt) <- unlist(lapply(1:length(ndx.filt), function (x) nm[ndx.filt[[x]][1]]) )

  # calculate average param values
  max_slope.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "max_slope.spline", param2 = "max_slope2.spline")
  max_slope.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "max_slope.spline", param2 = "max_slope2.spline")

  lambda.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "lambda.spline", param2 = "lambda2.spline")
  lambda.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "lambda.spline", param2 = "lambda2.spline")

  dY.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "dY.spline", param2 = "dY2.spline")
  dY.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "dY.spline", param2 = "dY2.spline")

  A.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "A.spline", param2 = "A2.spline")
  A.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "A.spline", param2 = "A2.spline")

  tmax.mean <- get_avg_param(table = flTable, ndx.rep = ndx.filt, param1 = "x.max.spline", param2 = "x.max2.spline")
  tmax.sd <- get_sd_param(table = flTable, ndx.rep = ndx.filt, param1 = "x.max.spline", param2 = "x.max2.spline")

  labels <- gsub(" \\| NA", "", gsub(" \\| [[:digit:]]+ \\| ", " | ", names(ndx.filt))) # condition names

  table_spline_group <- data.frame("Sample | Conc." = labels,
                                   "slope_max" = paste0(max_slope.mean,
                                                        unlist(lapply(1:length(max_slope.mean), function (x)
                                                          ifelse(max_slope.mean[x] == 0 || max_slope.mean[x] == "" || max_slope.mean[x] == "" ||
                                                                   max_slope.sd[x] == 0 || max_slope.sd[x] == "" || max_slope.sd[x] == "",
                                                                 "", " \u00B1 ") ) ),
                                                        unlist(lapply(1:length(max_slope.mean), function (x)
                                                          ifelse(max_slope.mean[x] == 0 || max_slope.mean[x] == "" || max_slope.mean[x] == "" ||
                                                                   max_slope.sd[x] == 0 || max_slope.sd[x] == "" || max_slope.sd[x] == "",
                                                                 "", max_slope.sd[x])))),
                                   "lagtime" = paste0(lambda.mean,
                                                      unlist(lapply(1:length(lambda.mean), function (x)
                                                        ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                 lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                               "", " \u00B1 ") ) ),
                                                      unlist(lapply(1:length(lambda.mean), function (x)
                                                        ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                 lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                               "", lambda.sd[x])))),
                                   "Y_max" = paste0(A.mean,
                                                    unlist(lapply(1:length(A.mean), function (x)
                                                      ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                               A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                             "", " \u00B1 ") ) ),
                                                    unlist(lapply(1:length(A.mean), function (x)
                                                      ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                               A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                             "", A.sd[x])))),
                                   "dY" = paste0(dY.mean,
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", dY.sd)))),
                                   "x(slope_max)" = paste0(tmax.mean,
                                                           unlist(lapply(1:length(tmax.mean), function (x)
                                                             ifelse(tmax.mean[x] == 0 || tmax.mean[x] == "" || tmax.mean[x] == "" ||
                                                                      tmax.sd[x] == 0 || tmax.sd[x] == "" || tmax.sd[x] == "",
                                                                    "", " \u00B1 ") ) ),
                                                           unlist(lapply(1:length(tmax.mean), function (x)
                                                             ifelse(tmax.mean[x] == 0 || tmax.mean[x] == "" || tmax.mean[x] == "" ||
                                                                      tmax.sd[x] == 0 || tmax.sd[x] == "" || tmax.sd[x] == "",
                                                                    "", tmax.sd[x])))),
                                   check.names = FALSE)

  if(html == TRUE){
    colnames(table_spline_group) <- c("Sample|Conc.", "slope<sub>max</sub>", "lagtime",
                                      "dY", "y<sub>max</sub>", "x(slope<sub>max</sub>)")
  }

  return(table_spline_group)
}

#' Generate a grouped results table for linear fits with average and standard deviations
#'
#' @param gcTable An object of class \code{gcTable}
#' @param html (Logical) Should column headers contain html formatting?
#'
#' @return A data frame with grouped linear fit results. Empty cells indicate that no reliable fit could be determined.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create random growth data set
#' rnd.data <- rdm.data(d = 30, mu = 0.6, A = 4.5, label = "Test2")
#'
#'
#' # Run growth curve analysis workflow
#' res <- growth.workflow(time = rnd.data$time,
#'                        data = rnd.data$data,
#'                        fit.opt = "l",
#'                        ec50 = FALSE,
#'                        export.res = FALSE,
#'                        parallelize = FALSE,
#'                        suppress.messages = TRUE)
#'
#' table_group_growth_linear(res$gcFit$gcTable)
#'
#' # with HTML formatting
#' DT::datatable(table_group_growth_linear(res$gcFit$gcTable, html = TRUE),
#'               escape = FALSE) # Do not escape HTML entities
#' }
table_group_growth_linear <- function(gcTable, html = FALSE)
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
  ndx.filt <- unlist(filter.ls, recursive = FALSE)
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
                                   "mumax" = paste0(mu.mean,
                                                    unlist(lapply(1:length(mu.mean), function (x)
                                                      ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                               mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                             "", " \u00B1 ") ) ),
                                                    unlist(lapply(1:length(mu.mean), function (x)
                                                      ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                               mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                             "", mu.sd[x])))),

                                   "tD" = paste0(tD.mean,
                                                 unlist(lapply(1:length(tD.mean), function (x)
                                                   ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                            tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(tD.mean), function (x)
                                                   ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                            tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                          "", tD.sd[x])))),
                                   "lagtime" =  paste0(lambda.mean,
                                                       unlist(lapply(1:length(lambda.mean), function (x)
                                                         ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                  lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                                "", " \u00B1 ") ) ),
                                                       unlist(lapply(1:length(lambda.mean), function (x)
                                                         ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                  lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                                "", lambda.sd[x])))),
                                   "dY" = paste0(dY.mean,
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", dY.sd[x])))),
                                   "Y_max" = paste0(A.mean,
                                                    unlist(lapply(1:length(A.mean), function (x)
                                                      ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                               A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                             "", " \u00B1 ") ) ),
                                                    unlist(lapply(1:length(A.mean), function (x)
                                                      ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                               A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                             "", A.sd[x])))),
                                   "t_start(mumax)" = paste0(tmu.start.mean,
                                                             unlist(lapply(1:length(tmu.start.mean), function (x)
                                                               ifelse(tmu.start.mean[x] == 0 || tmu.start.mean[x] == "" || tmu.start.mean[x] == "" ||
                                                                        tmu.start.sd[x] == 0 || tmu.start.sd[x] == "" || tmu.start.sd[x] == "",
                                                                      "", " \u00B1 ") ) ),
                                                             unlist(lapply(1:length(tmu.start.mean), function (x)
                                                               ifelse(tmu.start.mean[x] == 0 || tmu.start.mean[x] == "" || tmu.start.mean[x] == "" ||
                                                                        tmu.start.sd[x] == 0 || tmu.start.sd[x] == "" || tmu.start.sd[x] == "",
                                                                      "", tmu.start.sd[x])))),

                                   "t_end(mumax)" = paste0(tmu.end.mean,
                                                           unlist(lapply(1:length(tmu.end.mean), function (x)
                                                             ifelse(tmu.end.mean[x] == 0 || tmu.end.mean[x] == "" || tmu.end.mean[x] == "" ||
                                                                      tmu.end.sd[x] == 0 || tmu.end.sd[x] == "" || tmu.end.sd[x] == "",
                                                                    "", " \u00B1 ") ) ),
                                                           unlist(lapply(1:length(tmu.end.mean), function (x)
                                                             ifelse(tmu.end.mean[x] == 0 || tmu.end.mean[x] == "" || tmu.end.mean[x] == "" ||
                                                                      tmu.end.sd[x] == 0 || tmu.end.sd[x] == "" || tmu.end.sd[x] == "",
                                                                    "", tmu.end.sd[x])))),
                                   stringsAsFactors = FALSE, check.names = FALSE)

  if(html == TRUE){
    colnames(table_linear_group) <- c("Sample|Conc.", "mu<sub>max</sub>", "t<sub>D</sub>", "lagtime",
                                      "dY", "y<sub>max</sub>", "t<sub>start</sub><br>(mu<sub>max</sub>)",
                                      "t<sub>end</sub><br>(mu<sub>max</sub>)")
  }
  return(table_linear_group)
}

#' Generate a grouped results table for spline fits with average and standard deviations
#'
#' @param gcTable An object of class \code{gcTable}
#' @param html (Logical) Should column headers contain html formatting?
#'
#' @return A data frame with grouped spline fit results. Empty cells indicate that no reliable fit could be determined.
#'
#' @export
#'
#' @examples
#' # Create random growth data set
#' rnd.data <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#'
#' # Run growth curve analysis workflow
#' res <- growth.workflow(time = rnd.data$time,
#'                        data = rnd.data$data,
#'                        fit.opt = "s",
#'                        ec50 = FALSE,
#'                        export.res = FALSE,
#'                        parallelize = FALSE,
#'                        suppress.messages = TRUE)
#'
#' table_group_growth_spline(res$gcFit$gcTable)
#'
#' # with HTML formatting
#' DT::datatable(table_group_growth_spline(res$gcFit$gcTable, html = TRUE),
#'               escape = FALSE) # Do not escape HTML entities
#'
table_group_growth_spline <- function(gcTable, html = FALSE)
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
  ndx.filt <- unlist(filter.ls, recursive = FALSE)
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
                                   "mumax" = paste0(mu.mean,
                                                    unlist(lapply(1:length(mu.mean), function (x)
                                                      ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                               mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                             "", " \u00B1 ") ) ),
                                                    unlist(lapply(1:length(mu.mean), function (x)
                                                      ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                               mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                             "", mu.sd[x])))),
                                   "tD" = paste0(tD.mean,
                                                 unlist(lapply(1:length(tD.mean), function (x)
                                                   ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                            tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(tD.mean), function (x)
                                                   ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                            tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                          "", tD.sd[x])))),
                                   "lagtime" = paste0(lambda.mean,
                                                      unlist(lapply(1:length(lambda.mean), function (x)
                                                        ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                 lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                               "", " \u00B1 ") ) ),
                                                      unlist(lapply(1:length(lambda.mean), function (x)
                                                        ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                 lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                               "", lambda.sd[x])))),
                                   "Y_max" = paste0(A.mean,
                                                    unlist(lapply(1:length(A.mean), function (x)
                                                      ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                               A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                             "", " \u00B1 ") ) ),
                                                    unlist(lapply(1:length(A.mean), function (x)
                                                      ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                               A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                             "", A.sd[x])))),
                                   "dY" = paste0(dY.mean,
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", " \u00B1 ") ) ),
                                                 unlist(lapply(1:length(dY.mean), function (x)
                                                   ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                            dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                          "", dY.sd[x])))),
                                   "t(mumax)" = paste0(tmax.mean,
                                                       unlist(lapply(1:length(tmax.mean), function (x)
                                                         ifelse(tmax.mean[x] == 0 || tmax.mean[x] == "" || tmax.mean[x] == "" ||
                                                                  tmax.sd[x] == 0 || tmax.sd[x] == "" || tmax.sd[x] == "",
                                                                "", " \u00B1 ") ) ),
                                                       unlist(lapply(1:length(tmax.mean), function (x)
                                                         ifelse(tmax.mean[x] == 0 || tmax.mean[x] == "" || tmax.mean[x] == "" ||
                                                                  tmax.sd[x] == 0 || tmax.sd[x] == "" || tmax.sd[x] == "",
                                                                "", tmax.sd[x])))),
                                   check.names = FALSE)

  if(html == TRUE){
    colnames(table_spline_group) <- c("Sample|Conc.", "mu<sub>max</sub>", "t<sub>D</sub>", "lagtime",
                                      "dY", "y<sub>max</sub>", "t(mu<sub>max</sub>)")
  }

  return(table_spline_group)
}

#' Generate a grouped results table for parametric fits with average and standard deviations
#'
#' @param gcTable An object of class \code{gcTable}
#' @param html (Logical) Should column headers contain html formatting?
#'
#' @return A data frame with grouped model fit results. Empty cells indicate that no reliable fit could be determined.
#'
#' @export
#'
#' @examples
#' # Create random growth data set
#' rnd.data <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#'
#'
#' # Run growth curve analysis workflow
#' res <- growth.workflow(time = rnd.data$time,
#'                        data = rnd.data$data,
#'                        fit.opt = "m",
#'                        ec50 = FALSE,
#'                        export.res = FALSE,
#'                        parallelize = FALSE,
#'                        suppress.messages = TRUE)
#'
#' table_group_growth_model(res$gcFit$gcTable)
#'
#' # with HTML formatting
#' DT::datatable(table_group_growth_model(res$gcFit$gcTable, html = TRUE),
#'               escape = FALSE) # Do not escape HTML entities
#'
table_group_growth_model <- function(gcTable, html = FALSE)
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
  ndx.filt <- unlist(filter.ls, recursive = FALSE)
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
                                  "mumax" = paste0(mu.mean,
                                                   unlist(lapply(1:length(mu.mean), function (x)
                                                     ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                              mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                            "", " \u00B1 ") ) ),
                                                   unlist(lapply(1:length(mu.mean), function (x)
                                                     ifelse(mu.mean[x] == 0 || mu.mean[x] == "" || mu.mean[x] == "" ||
                                                              mu.sd[x] == 0 || mu.sd[x] == "" || mu.sd[x] == "",
                                                            "", mu.sd[x])))),
                                  "tD" = paste0(tD.mean,
                                                unlist(lapply(1:length(tD.mean), function (x)
                                                  ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                           tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                         "", " \u00B1 ") ) ),
                                                unlist(lapply(1:length(tD.mean), function (x)
                                                  ifelse(tD.mean[x] == 0 || tD.mean[x] == "" || tD.mean[x] == "" ||
                                                           tD.sd[x] == 0 || tD.sd[x] == "" || tD.sd[x] == "",
                                                         "", tD.sd[x])))),
                                  "lagtime" = paste0(lambda.mean,
                                                     unlist(lapply(1:length(lambda.mean), function (x)
                                                       ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                              "", " \u00B1 ") ) ),
                                                     unlist(lapply(1:length(lambda.mean), function (x)
                                                       ifelse(lambda.mean[x] == 0 || lambda.mean[x] == "" || lambda.mean[x] == "" ||
                                                                lambda.sd[x] == 0 || lambda.sd[x] == "" || lambda.sd[x] == "",
                                                              "", lambda.sd[x])))),
                                  "Y_max" = paste0(A.mean,
                                                   unlist(lapply(1:length(A.mean), function (x)
                                                     ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                              A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                            "", " \u00B1 ") ) ),
                                                   unlist(lapply(1:length(A.mean), function (x)
                                                     ifelse(A.mean[x] == 0 || A.mean[x] == "" || A.mean[x] == "" ||
                                                              A.sd[x] == 0 || A.sd[x] == "" || A.sd[x] == "",
                                                            "", A.sd[x])))),
                                  "dY" = paste0(dY.mean,
                                                unlist(lapply(1:length(dY.mean), function (x)
                                                  ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                           dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                         "", " \u00B1 ") ) ),
                                                unlist(lapply(1:length(dY.mean), function (x)
                                                  ifelse(dY.mean[x] == 0 || dY.mean[x] == "" || dY.mean[x] == "" ||
                                                           dY.sd[x] == 0 || dY.sd[x] == "" || dY.sd[x] == "",
                                                         "", dY.sd[x])))),
                                  check.names = FALSE)

  if(html == TRUE){
    colnames(table_model_group) <- c("Sample|Conc.", "mu<sub>max</sub>", "t<sub>D</sub>", "lagtime",
                                     "y<sub>max</sub>", "dY")
  }

  return(table_model_group)
}

#' Calculate parameter averages for several samples
#'
#' Internal function used within the \code{table_group} set of functions.
#'
#' @param table A dataframe with class \code{gcTable} or \code{flTable}, generated with \code{\link{growth.gcFit}} or \code{\link{flFit}}, respectively.
#' @param ndx.rep A list with each element being a vector of row indices for samples (replicates) for which to calculate averages.
#' @param param1 Name of the parameter for which to calculate averages.
#' @param param2 (optional) Name of a second parameter for which to calculate averages. The results for the second parameter will be attached in parentheses after the first parameter.
#'
#' @return A vector of strings containing parameter averages.
#'
#' @keywords internal
#' @noRd
#'
get_avg_param <- function(table = data.frame(), ndx.rep = list(), param1, param2 = NULL)
{
  if(!is.null(param2)){
    avg <- ifelse(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ) == 0 |
                    is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) )),
                  "", ifelse(is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param2]), na.rm = TRUE)) )),
                             round(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ), 3),
                             paste0("<strong>", round(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ), 3),
                                    "</strong>", " (", round(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param2]), na.rm = TRUE)) ), 3), ")")))
  } else {
    avg <- ifelse(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ) == 0 |
                    is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) )),
                  "", round(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ), 3)
    )
  }
  avg <- unlist(lapply(1:length(avg), function (x) ifelse(is.na(avg[x]), "", avg[x])) )
  avg
}

#' Calculate parameter standard deviations for several samples
#'
#' Internal function used within the \code{table_group} set of functions.
#'
#' @param table A dataframe with class \code{gcTable} or \code{flTable}, generated with \code{\link{growth.gcFit}} or \code{\link{flFit}}, respectively.
#' @param ndx.rep A list with each element being a vector of row indices for samples (replicates) for which to calculate standard deviations.
#' @param param1 Name of the parameter for which to calculate standard deviations
#' @param param2 (optional) Name of a second parameter for which to calculate standard deviations The results for the second parameter will be attached in parentheses after the first parameter.
#'
#' @return A vector of strings containing parameter standard deviations.
#'
#' @keywords internal
#' @noRd
#'
get_sd_param <- function(table = data.frame(), ndx.rep = list(), param1, param2 = NULL)
{
  if(!is.null(param2)){
    sd <- ifelse(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ) == 0 |
                   is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) )),
                 "", ifelse(is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param2]), na.rm = TRUE)) )),
                            round(unlist(lapply(1:length(ndx.rep), function (x) sd(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ), 3),
                            paste0("<strong>", round(unlist(lapply(1:length(ndx.rep), function (x) sd(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ), 3),
                                   "</strong>", " (", round(unlist(lapply(1:length(ndx.rep), function (x) sd(as.numeric(table[ndx.rep[[x]], param2]), na.rm = TRUE)) ), 3), ")")))
  } else {
    sd <- ifelse(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ) == 0 |
                   is.na(unlist(lapply(1:length(ndx.rep), function (x) mean(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) )),
                 "", round(unlist(lapply(1:length(ndx.rep), function (x) sd(as.numeric(table[ndx.rep[[x]], param1]), na.rm = TRUE)) ), 3)
    )
  }
  sd <- unlist(lapply(1:length(sd), function (x) ifelse(is.na(sd[x]), "", sd[x])) )
  sd
}
