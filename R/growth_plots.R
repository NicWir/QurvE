#' Generic plot function for \code{gcFittedLinear} objects. Plot the results of a linear regression on ln-transformed data
#'
#' \code{plot.gcFitLinear} shows the results of a linear regression on log-transformed data and visualizes raw data, data points included in the fit, the tangent obtained by linear regression, and the lag time.
#'
#' @param x A \code{gcFittedLinear} object created with \code{\link{growth.gcFitLinear}} or stored within a \code{grofit} or \code{gcFit} object created with \code{\link{growth.workflow}} or \code{\link{growth.gcFit}}, respectively.
#' @param log ("x" or "y") Display the x- or y-axis on a logarithmic scale.
#' @param which ("fit" or "diagnostics") Display either the results of the linear fit on the raw data or statistical evaluation of the linear regression.
#' @param pch (Numeric) Shape of the raw data symbols.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param cex.lab (Numeric) Font size of axis titles.
#' @param cex.axis (Numeric) Font size of axis annotations.
#' @param lwd (Numeric) Line width.
#' @param color (Character string) Enter color either by name (e.g., red, blue, coral3) or via their hexadecimal code (e.g., #AE4371, #CCFF00FF, #0066FFFF). A full list of colors available by name can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on y-axis as a vector in the form \code{c(l, u)}.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis as a vector in the form \code{c(l, u)}.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Further arguments to refine the generated base R plot.
#'
#' @importFrom graphics abline arrows axis hist layout lines mtext par points title
#' @importFrom grDevices colors dev.new
#'
#' @export plot.gcFitLinear
#' @export
#'
#' @return A plot with the linear fit.
#'
#' @examples
#' # Create random growth dataset
#' rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#'
#' # Extract time and growth data for single sample
#' time <- rnd.dataset$time[1,]
#' data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns
#'
#' # Perform linear fit
#' TestFit <- growth.gcFitLinear(time, data, gcID = "TestFit",
#'                  control = growth.control(fit.opt = "l"))
#'
#' plot(TestFit)
#'
plot.gcFitLinear <- function(x, log="y", which=c("fit", "diagnostics", "fit_diagnostics"), pch = 21, cex.point = 1, cex.lab = 1.5,
                             cex.axis = 1.3, lwd = 2, color = "firebrick3", y.lim = NULL, x.lim = NULL,
                             plot = TRUE, export = FALSE, height = ifelse(which=="fit", 7, 5),
                             width = ifelse(which=="fit", 9, 9), out.dir = NULL, ...)
{
  gcFittedLinear <- x
  if(!is.null(color))
    color <- toupper(color)
  if(methods::is(gcFittedLinear) != "gcFitLinear") stop("x needs to be an object created with growth.gcFitLinear().")
  which <- match.arg(which)

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  p <- function(){
    switch(which,
           fit = {

             par(mar=c(2.1+cex.lab + 0.5*cex.axis, 2.1+1.3*cex.lab+1.2*cex.axis, 4.1, 3.1), cex.lab = cex.lab, cex.axis = cex.axis)

             plot(gcFittedLinear$"raw.data" ~ gcFittedLinear$"raw.time", xlab="", ylab = "", pch = pch,
                  log=log, las=1, yaxt="n", xaxt="n", type = "n", xlim = x.lim, ylim = y.lim, ...)
             title(ylab = "Density", line = 0.9 + 0.5*cex.lab+1.5*cex.axis, cex.lab = cex.lab)
             title(xlab = "Time", line = 0.5 + 0.9*cex.lab + 0.6*cex.axis, cex.lab = cex.lab)

             points(gcFittedLinear$"raw.data" ~ gcFittedLinear$"raw.time", cex = cex.point, pch=pch)
             axis(1, mgp=c(3,0.5+0.5*cex.axis,0))
             axis(2, las=1, mgp = c(3,1,0))
             try(points(gcFittedLinear$raw.data[gcFittedLinear$ndx] ~ gcFittedLinear$raw.time[gcFittedLinear$ndx], pch=pch, cex = cex.point*1.15, col="black", bg=color))

             ## lag phase
             lag <- gcFittedLinear$par["lag"]
             coef_ <- gcFittedLinear$par


             if(gcFittedLinear$fitFlag2){
               try(points(gcFittedLinear$raw.data[gcFittedLinear$ndx2] ~ gcFittedLinear$raw.time[gcFittedLinear$ndx2], pch=pch, cex = cex.point*1.15, col="black", bg=ggplot2::alpha("magenta3", 1)))
               lag2 <- gcFittedLinear$par["lag2"]
               if(lag2 < lag && lag2 > gcFittedLinear$raw.time[1]){
                 try(time2 <- seq(lag2, max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
                 try(time <- seq(coef_["tmax_start"]-0.25*(coef_["tmax_end"]-coef_["tmax_start"]), max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
                 try(lines(time2, gcFittedLinear$FUN(time2, unname(c(coef_["y0_lm2"], coef_["mumax2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = TRUE)
                 try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag2), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7)), silent = TRUE)
                 try(lines(time, gcFittedLinear$FUN(time, unname(c(coef_["y0_lm"], coef_["mumax"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               } else {
                 try(time2 <- seq(coef_["tmax2_start"]-0.25*(coef_["tmax2_start"]), max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
                 try(time <- seq(lag, max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
                 try(lines(time, gcFittedLinear$FUN(time, parms = coef_)[,"y"], lty=2, lwd=2, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
                 try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7)), silent = TRUE)
                 try(lines(time2, gcFittedLinear$FUN(time2, parms = unname(c(coef_["y0_lm2"], coef_["mumax2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = TRUE)

               }
             } else {
               try(time <- seq(lag, max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
               try(lines(time, gcFittedLinear$FUN(time, unname(c(coef_["y0_lm"], coef_["mumax"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7)), silent = TRUE)
             }

             graphics::mtext(bquote(mu: ~ .(round(gcFittedLinear$par[["mumax"]], digits = 3))~~~~
                                      lambda: ~ .(round(gcFittedLinear$par[["lag"]], digits = 3))~~~~
                                      t[max]: ~ .(round(gcFittedLinear$par[["tmax_start"]], digits = 2))-.(round(gcFittedLinear$par[["tmax_end"]], digits = 2))~~~~
                                      R2:~ .(round(gcFittedLinear$rsquared, digits = 3))),
                             side = 4 , adj = 0.45, line = -2.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab*0.7, srt = 90)

             graphics::mtext(paste("h:", ifelse(is.null(gcFittedLinear$control$lin.h), "NULL", gcFittedLinear$control$lin.h),
                                   "   R2-thresh.:",  gcFittedLinear$control$lin.R2,
                                   "   RSD-thresh.:",  gcFittedLinear$control$lin.RSD,
                                   "t0:", gcFittedLinear$control$t0,
                                   "  min.growth:", gcFittedLinear$control$min.growth,
                                   "   dY-thresh.:",  gcFittedLinear$control$lin.dY),
                             cex = cex.lab*0.7, side = 3, line = -2.5, adj = 0.05, outer = TRUE)

           },
           diagnostics = {
             par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), cex.lab = cex.lab, cex.axis = cex.axis, mfrow=c(1,2))

             ## residuals vs. fitted
             obs <- gcFittedLinear$log.data
             sim <- gcFittedLinear$FUN(gcFittedLinear$"raw.time", gcFittedLinear$par)
             plot(gcFittedLinear$fit[["residuals"]] ~ fitted(gcFittedLinear$fit), xlab="", ylab="", type = "n", pch = pch, xaxt="n", yaxt="n")
             points(gcFittedLinear$fit[["residuals"]] ~ fitted(gcFittedLinear$fit), cex = cex.point, pch=pch)
             abline(h=0, col="grey")
             title(ylab = "Density", line = 0.9 + 0.5*cex.lab+1.5*cex.axis, cex.lab = cex.lab)
             title(xlab = "Time", line = 0.5 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,0.5+0.5*cex.axis,0))
             axis(2, las=1, mgp = c(3,1,0))
             ## normal q-q-plot
             stats::qqnorm(gcFittedLinear$fit[["residuals"]], cex = cex.point, xlab="", ylab="", xaxt="n", yaxt="n", main = "")
             stats::qqline(gcFittedLinear$fit[["residuals"]])
             title("Normal Q-Q Plot", line = 1, cex.main = cex.lab)
             title(ylab = "Sample quantiles", line = 0.9 + 0.5*cex.lab+1.5*cex.axis, cex.lab = cex.lab)
             title(xlab = "Theoretical quantiles", line = 0.5 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,0.5+0.5*cex.axis,0))
             axis(2, las=1, mgp = c(3,1,0))
           },
           fit_diagnostics = {
             layout(matrix(c(1,1,2,3), nrow=2, byrow=TRUE))
             par(mar=c(2.1+cex.lab + 0.5*cex.axis, 2.6+1.3*cex.lab+1.2*cex.axis, 4.1+0.2*cex.lab, 3.1),
                 cex.lab = cex.lab, cex.axis = cex.axis)
             # mai = c(0.5 + 0.05*cex.lab + 0.07*cex.axis, 0.7 + 0.2*cex.lab + 0.2*cex.axis, 0.5, 0.5),

             plot(gcFittedLinear$"raw.data" ~ gcFittedLinear$"raw.time", xlab="", ylab = "", pch = pch,
                  log=log, las=1, yaxt="n", xaxt="n", type = "n", xlim = x.lim, ylim = y.lim, ...)
             title(ylab = "Density", line = 0.9 + 0.5*cex.lab+1.5*cex.axis, cex.lab = cex.lab)
             title(xlab = "Time", line = 0.5 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)

             points(gcFittedLinear$"raw.data" ~ gcFittedLinear$"raw.time", cex = cex.point, pch=pch)
             axis(1, mgp=c(3,0.5+0.5*cex.axis,0))
             axis(2, las=1, mgp = c(3,1,0))
             try(points(gcFittedLinear$raw.data[gcFittedLinear$ndx] ~ gcFittedLinear$raw.time[gcFittedLinear$ndx], pch=pch, cex = cex.point*1.15, col="black", bg=color))

             ## lag phase
             lag <- gcFittedLinear$par["lag"]
             coef_ <- gcFittedLinear$par


             if(gcFittedLinear$fitFlag2){
               try(points(gcFittedLinear$raw.data[gcFittedLinear$ndx2] ~ gcFittedLinear$raw.time[gcFittedLinear$ndx2], pch=pch, cex = cex.point*1.15, col="black", bg=ggplot2::alpha("magenta3", 1)))
               lag2 <- gcFittedLinear$par["lag2"]
               if(lag2 < lag && lag2 > gcFittedLinear$raw.time[1]){
                 try(time2 <- seq(lag2, max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
                 try(time <- seq(coef_["tmax_start"]-0.25*(coef_["tmax_end"]-coef_["tmax_start"]), max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
                 try(lines(time2, gcFittedLinear$FUN(time2, unname(c(coef_["y0_lm2"], coef_["mumax2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = TRUE)
                 try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag2), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7)), silent = TRUE)
                 try(lines(time, gcFittedLinear$FUN(time, unname(c(coef_["y0_lm"], coef_["mumax"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               } else {
                 try(time2 <- seq(coef_["tmax2_start"]-0.25*(coef_["tmax2_start"]), max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
                 try(time <- seq(lag, max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
                 try(lines(time, gcFittedLinear$FUN(time, parms = coef_)[,"y"], lty=2, lwd=2, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
                 try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7)), silent = TRUE)
                 try(lines(time2, gcFittedLinear$FUN(time2, parms = unname(c(coef_["y0_lm2"], coef_["mumax2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = TRUE)

               }
             } else {
               try(time <- seq(lag, max(gcFittedLinear$"raw.time"), length=200), silent = TRUE)
               try(lines(time, gcFittedLinear$FUN(time, unname(c(coef_["y0_lm"], coef_["mumax"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7)), silent = TRUE)
             }
             graphics::mtext(bquote(mu: ~ .(round(gcFittedLinear$par[["mumax"]], digits = 3))~~~~
                                      lambda: ~ .(round(gcFittedLinear$par[["lag"]], digits = 3))~~~~
                                      t[max]: ~ .(round(gcFittedLinear$par[["tmax_start"]], digits = 2))-.(round(gcFittedLinear$par[["tmax_end"]], digits = 2))~~~~
                                      R2:~ .(round(gcFittedLinear$rsquared, digits = 3))),
                             side = 4 , adj = 0.55, line = -2.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab*0.7, srt = 90)

             graphics::mtext(paste("h:", ifelse(is.null(gcFittedLinear$control$lin.h), "NULL", gcFittedLinear$control$lin.h),
                         "   R2-thresh.:",  gcFittedLinear$control$lin.R2,
                         "   RSD-thresh.:",  gcFittedLinear$control$lin.RSD,
                         "t0:", gcFittedLinear$control$t0,
                         "  min.growth:", gcFittedLinear$control$min.growth,
                         "   dY-thresh.:",  gcFittedLinear$control$lin.dY),
                   cex = cex.lab*0.7, side = 3, line = -2.5, adj = 0.05, outer = TRUE)

             # graphics::mtext(paste("R2:", round(gcFittedLinear$rsquared, digits = 3)), side = 3 , adj = 0.95, line = -1, outer = TRUE)
             # graphics::mtext(paste("h:", gcFittedLinear$control4lin.h,
             #             "   R2-thresh.:",  gcFittedLinear$control$lin.R2,
             #             "   RSD-thresh.:",  gcFittedLinear$control$lin.RSD,
             #             "   dY-thresh.:",  gcFittedLinear$control$lin.dY),
             #       side = 4, line = -2, outer = TRUE)

             ## residuals vs. fitted
             obs <- gcFittedLinear$log.data
             sim <- gcFittedLinear$FUN(gcFittedLinear$"raw.time", gcFittedLinear$par)
             plot(gcFittedLinear$fit[["residuals"]] ~ fitted(gcFittedLinear$fit), xlab="", ylab="", type = "n", pch = pch, xaxt="n", yaxt="n")
             points(gcFittedLinear$fit[["residuals"]] ~ fitted(gcFittedLinear$fit), cex = cex.point, pch=pch)
             abline(h=0, col="grey")
             title(ylab = "residuals", line = 0.9 + 0.5*cex.lab+1.5*cex.axis, cex.lab = cex.lab)
             title(xlab = "fitted", line = 0.5 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,0.5+0.5*cex.axis,0))
             axis(2, las=1, mgp = c(3,1,0))
             ## normal q-q-plot
             stats::qqnorm(gcFittedLinear$fit[["residuals"]], cex = cex.point, xlab="", ylab="", xaxt="n", yaxt="n", main = "")
             stats::qqline(gcFittedLinear$fit[["residuals"]])
             title("Normal Q-Q Plot", line = 1, cex.main = cex.lab)
             title(ylab = "Sample quantiles", line = 0.9 + 0.5*cex.lab+1.5*cex.axis, cex.lab = cex.lab)
             title(xlab = "Theoretical quantiles", line = 0.5 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,0.5+0.5*cex.axis,0))
             axis(2, las=1, mgp = c(3,1,0))
           }
    )
  }
  if (export == TRUE){
    w <- width
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = FALSE)
    grDevices::png(paste0(out.dir, "/", paste(gcFittedLinear$gcID, collapse = "_"), "_LinFitPlot.png"),
                   width = w, height = h, units = 'in', res = 300)
    p()
    grDevices::dev.off()
    if (requireNamespace("Cairo", quietly = TRUE)) {
      Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(gcFittedLinear$gcID, collapse = "_"), "_LinFitPlot.pdf"))
    } else {
      message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
      grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", paste(gcFittedLinear$gcID, collapse = "_"), "_LinFitPlot.pdf"))
    }
    p()
    grDevices::dev.off()
  }
  if (plot == TRUE){
    p()
  }
}

#' Generic plot function for \code{gcFitModel} objects.
#'
#' Plot the results of a parametric model fit on growth vs. time data
#'
#' @param x A \code{gcFittedModel} object created with \code{\link{growth.gcFitModel}} or stored within a \code{grofit} or \code{gcFit} object created with \code{\link{growth.workflow}} or \code{\link{growth.gcFit}}, respectively.
#' @param raw (Logical) Show the raw data within the plot (\code{TRUE}) or not (\code{FALSE}).
#' @param pch (Numeric) Symbol used to plot data points.
#' @param colData (Numeric or Character) Color used to plot the raw data.
#' @param equation (Logical) Show the equation of the fitted model within the plot (\code{TRUE}) or not (\code{FALSE}).
#' @param eq.size (Numeric) Provide a value to scale the size of the displayed equation.
#' @param colModel (Numeric or Character) Color used to plot the fitted model.
#' @param basesize (Numeric) Base font size.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param lwd (Numeric) Spline line width.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on y-axis of the growth curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Further arguments to refine the generated \code{ggplot2} plot.
#'
#' @return A plot with the parametric fit.
#'
#' @export plot.gcFitModel
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggtitle labs guides
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
#'
#' @examples
#' # Create random growth dataset
#' rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#'
#' # Extract time and growth data for single sample
#' time <- rnd.dataset$time[1,]
#' data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns
#'
#' # Perform parametric fit
#' TestFit <- growth.gcFitModel(time, data, gcID = "TestFit",
#'                  control = growth.control(fit.opt = "m"))
#'
#' plot(TestFit, basesize = 18, eq.size = 1.5)
#'
plot.gcFitModel <- function(x, raw = TRUE,
                            pch=1,
                            colData=1,
                            equation = TRUE,
                            eq.size = 1,
                            colModel="forestgreen",
                            basesize=16,
                            cex.point = 2,
                            lwd = 0.7,
                            x.lim = NULL,
                            y.lim = NULL,
                            n.ybreaks = 6,
                            plot = TRUE,
                            export = FALSE,
                            height = 6,
                            width = 8,
                            out.dir = NULL,...)
{
  gcFittedModel <- x
  if(!is.null(colData))
    colData <- toupper(colData)
  if(!is.null(colModel))
    colModel <- toupper(colModel)
  colModel <- ggplot2::alpha(colModel, 0.85)
  # /// check input parameters
  if (is.logical(raw)==FALSE)   stop("Need logical value for: raw")
  # if (is.logical(slope)==FALSE) stop("Need logical value for: slope")
  if (is.logical(equation)==FALSE)   stop("Need logical value for: equation")
  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (!(methods::is(gcFittedModel)=="gcFitModel"))   stop("x needs to be an object created with growth.gcFitModel().")


  # /// check if a data fit is available
  if (gcFittedModel$fitFlag==FALSE||is.na(gcFittedModel$fitFlag)){
    warning("plot.gcFitModel: no data fit available!")

    if(!is.null(x.lim)){
      dat <- gcFittedModel[["raw.data"]][gcFittedModel[["raw.time"]]>= x.lim[1] & gcFittedModel[["raw.time"]] <= x.lim[2]]
      time <- gcFittedModel[["raw.time"]][gcFittedModel[["raw.time"]]>= x.lim[1] & gcFittedModel[["raw.time"]] <= x.lim[2]]
    } else {
      dat <- gcFittedModel[["raw.data"]]
      time <- gcFittedModel[["raw.time"]]
    }

    if(!is.null(y.lim)){
      time <- time[dat >= y.lim[1] & dat <= y.lim[2] ]
      dat <- dat[dat >= y.lim[1] & dat <= y.lim[2] ]
    }

    df <- data.frame("time" = time,
                     "data" = dat)

    p <-    ggplot(df, aes(x=.data$time, y=.data$data)) +
      xlab("Time") +
      ylab(label = ifelse(gcFittedModel$control$log.y.model == TRUE, "Growth [Ln(y(t)/y0)]", "Growth [y(t)]")) +
      theme_classic(base_size = basesize) +
      ggtitle(gsub(" \\| NA", "", paste(gcFittedModel$gcID, collapse=" | "))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks)) +
      theme(plot.title = element_text(size=basesize),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      geom_point(shape=pch, size = cex.point,alpha = 0.6, stroke=0.15*cex.point, color = colData)

    if(!is.null(y.lim)){
      p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks))
    } else {
      p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks))
    }
  }
  else{
    coef <- gcFittedModel[["parameters"]]
    lagtime <- coef["lambda"][[1]][1]
    model <- as.character(gcFittedModel$model)
    # assign(paste(model.vec), gcFittedModel[["fit.data"]])


    if(!is.null(x.lim)){
      dat <- gcFittedModel[["raw.data"]][gcFittedModel[["raw.time"]]>= x.lim[1] & gcFittedModel[["raw.time"]] <= x.lim[2]]
      dat.fit <- gcFittedModel[["fit.data"]][gcFittedModel[["fit.time"]]>= x.lim[1] & gcFittedModel[["fit.time"]] <= x.lim[2]]
      time <- gcFittedModel[["raw.time"]][gcFittedModel[["raw.time"]]>= x.lim[1] & gcFittedModel[["raw.time"]] <= x.lim[2]]
      time.fit <- gcFittedModel[["fit.time"]][gcFittedModel[["fit.time"]]>= x.lim[1] & gcFittedModel[["fit.time"]] <= x.lim[2]]
    } else {
      dat <- gcFittedModel[["raw.data"]]
      dat.fit <- gcFittedModel[["fit.data"]]
      time <- gcFittedModel[["raw.time"]]
      time.fit <- gcFittedModel[["fit.time"]]
    }

    if(!is.null(y.lim)){
      time <- time[dat >= y.lim[1] & dat <= y.lim[2] & dat.fit >= y.lim[1] & dat.fit <= y.lim[2]]
      time.fit <- time.fit[dat >= y.lim[1] & dat <= y.lim[2] & dat.fit >= y.lim[1] & dat.fit <= y.lim[2]]
      dat <- dat[dat >= y.lim[1] & dat <= y.lim[2] & dat.fit >= y.lim[1] & dat.fit <= y.lim[2]]
      dat.fit <- dat.fit[dat.fit >= y.lim[1] & dat.fit <= y.lim[2]]
    }

    df <- data.frame("time" = time,
                     "data" = dat,
                     "fit.time" = time.fit,
                     "fit.data" = dat.fit)

    p <-    ggplot(df, aes(x=.data$time, y=.data$data)) +
      geom_line(aes_(x=as.name(names(df)[3]), y = as.name(names(df)[4]), color = "model"), linewidth = lwd) +
      xlab("Time") +
      ylab(label = ifelse(gcFittedModel$control$log.y.model == TRUE, "Growth [Ln(y(t)/y0)]", "Growth [y(t)]")) +
      theme_classic(base_size = basesize) +
      ggtitle(gsub(" \\| NA", "", paste(gcFittedModel$gcID, collapse=" | "))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks)) +
      theme(plot.title = element_text(size=basesize),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    if(raw == TRUE){
      p <- p + geom_point(shape=pch, size = cex.point,alpha = 0.6, stroke=0.15*cex.point, color = colData)
    }
    # if(!is.null(x.lim)){
    #   p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
    # } else {
    #   p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    # }
    #
   if(!is.null(y.lim)){
     p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks))
   } else {
     p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks))
   }
    if(equation == TRUE){
      if(gcFittedModel$model == "logistic"){
        p <- p + annotate(
          "text",
          label = "y(t) == frac(A , 1+exp(frac(4 %*% mu, A) %*% (lambda - t) + 2))",
          x = 1.08 * time[length(time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.2*eq.size) +
          annotate("text",
                   label = list(bquote(A == .(round(gcFittedModel$parameters$A[[1]],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[[1]],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[[1]],3)) )),
                   x = (1 + 0.13+ log(eq.size)*0.1) * time[length(time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = TRUE, size = 2.5*eq.size) +
          scale_color_manual(name='Growth Model', breaks = "Logistic",
                             values=c("model" = colModel, "Logistic" = colModel)) +
          ggplot2::scale_color_discrete(labels="Logistic", type = colModel)

      }
      if(gcFittedModel$model == "richards"){
        p <- p + annotate(
          "text",
          label = "y(t) == A%*%(1.0+nu%*%italic(e)^{1+nu}%*%exp(frac(mu,A)%*%(1+nu)^(1+frac(1,nu))%*%( lambda - t )))^(-1/nu)",
          x = 1.17 * time[length(time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.2*eq.size) +
          annotate("text",
                   label = list(bquote(A == .(round(gcFittedModel$parameters$A[[1]],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[[1]],3)) ~~~~
                                         lambda == .(round(gcFittedModel$parameters$lambda[[1]],3)) ~~~~ nu == .(round(as.numeric(gcFittedModel$parameters$fitpar$nu[[1]],3))))),
                   x = (1 + 0.22 + log(eq.size)*0.1) * time[length(time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = TRUE, size = 2.5*eq.size) +
          scale_color_manual(name='Growth Model',
                             values=c("model" = colModel, "Richards" = colModel)) +
          ggplot2::scale_color_discrete(labels="Richards", type = colModel)

      }
      if(gcFittedModel$model == "baranyi"){
        p <- p + annotate(
          "text",
          label = "atop(B == t + frac(1,mu) %*% log(symbol(e)^{-mu%*%time} + symbol(e)^{-mu%*%lambda} - symbol(e)^{-mu%*%(time + lambda)}),
          y == y0 + mu%*%B - log(1 + (symbol(e)^{mu %*% B} - 1)/symbol(e)^{A - y0}))",
          x = 1.17 * time[length(time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.2*eq.size) +
          annotate("text",
                   label = list(bquote(y0 == .(round(gcFittedModel$parameters$fitpar$y0[1,1],3)) ~~~~
                                    A == .(round(gcFittedModel$parameters$A[[1]],3)) ~~~~
                                    mu == .(round(gcFittedModel$parameters$mu[[1]],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[[1]],3)) )),
                   x = (1 + 0.25 + log(eq.size)*0.1) * time[length(time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = TRUE, size = 2.5*eq.size) +
          scale_color_manual(name='Growth Model',
                             values=c("model" = colModel, "Baranyi" = colModel)) +
          ggplot2::scale_color_discrete(labels="Baranyi", type = colModel)

      }
      if(gcFittedModel$model == "gompertz"){
        p <- p + annotate(
          "text",
          label = "y(t) == A%*%exp(-exp(frac(mu%*%italic(e),A)%*%(lambda-t) +1))",
          x = 1.08 * time[length(time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.4*eq.size) +
          annotate("text",
                   label = list(bquote(A == .(round(gcFittedModel$parameters$A[[1]],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[[1]],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[[1]],3)) )),
                   x = (1 + 0.13 + log(eq.size)*0.1) * time[length(time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = TRUE, size = 2.5*eq.size) +
          scale_color_manual(name='Growth Model',
                             values=c("model" = colModel, "Gompertz" = colModel)) +
          ggplot2::scale_color_discrete(labels="Gompertz", type = colModel)

      }
      if(gcFittedModel$model == "gompertz.exp"){
        lagtime <- lagtime - gcFittedModel$parameters$A[1]*exp(gcFittedModel$parameters$fitpar$alpha[1]*(gcFittedModel$parameters$lambda[1]-gcFittedModel$parameters$fitpar$t_shift[1]))
        p <- p + annotate(
          "text",
          label = "y(t) == A%*%exp(-exp(frac(mu%*%italic(e),A)%*%(lambda-t) +1)) + A%*%exp(alpha%*%(t-t[shift]))",
          x = 1.16 * time[length(time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.2*eq.size) +
          annotate("text",
                   label = list(bquote(A == .(round(gcFittedModel$parameters$A[[1]],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[[1]],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[[1]],2)) ~~~~ alpha == .(round(gcFittedModel$parameters$fitpar$alpha[1],3))  ~~~~
                                    t[shift] == .(round(gcFittedModel$parameters$fitpar$t_shift[[1]],2)) )),
                   x = (1 + 0.21 + log(eq.size)*0.1) * time[length(time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = TRUE, size = 2.5*eq.size) +
          scale_color_manual(name='Growth Model',
                             values=c("model" = colModel, "Gompertz.exp" = colModel)) +
          ggplot2::scale_color_discrete(labels="Gompertz.exp", type = colModel)

      }
      if(gcFittedModel$model == "huang"){
        p <- p + annotate(
          "text",
          label = "y(t) == y0 + A - log( exp(y0) + (exp(A) - exp(y0)) * exp(-mu%*%(t+0.25%*%log(frac(1+exp(-4%*%(t-lambda)),1+exp(4%*%lambda))))) )",
          x = 1.16 * time[length(time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.0*eq.size) +
          annotate("text",
                   label = list(bquote(y0 == .(round(gcFittedModel$parameters$fitpar$y0[1,1],3)) ~~~~
                                    A == .(round(gcFittedModel$parameters$A[[1]],3)) ~~~~
                                    mu == .(round(gcFittedModel$parameters$mu[[1]],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[[1]],2)))),
                   x = (1 + 0.21 + log(eq.size)*0.1) * time[length(time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = TRUE, size = 2.3*eq.size) +
          scale_color_manual(name='Growth Model',
                             values=c("model" = colModel, "Huang" = colModel)) +
          ggplot2::scale_color_discrete(labels="Huang", type = colModel)

      }
    } # if(equation == TRUE)
    else{
      if(gcFittedModel$model == "logistic"){
        p <- p + scale_color_manual(name='Growth Model',
                                    values=c("model" = colModel, "Logistic" = colModel)) +
          ggplot2::scale_color_discrete(labels="Logistic", type = colModel)

      }
      if(gcFittedModel$model == "richards"){
        p <- p + scale_color_manual(name='Growth Model',
                                    values=c("model" = colModel, "Richards" = colModel)) +
          ggplot2::scale_color_discrete(labels="Richards", type = colModel)

      }
      if(gcFittedModel$model == "gompertz"){
        p <- p + scale_color_manual(name='Growth Model',
                             values=c("model" = colModel, "Gompertz" = colModel)) +
          ggplot2::scale_color_discrete(labels="Gompertz", type = colModel)
      }
      if(gcFittedModel$model == "gompertz.exp"){
        p <- p + scale_color_manual(name='Growth Model',
                                    values=c("model" = colModel, "Gompertz.exp" = colModel)) +
          ggplot2::scale_color_discrete(labels="Gompertz.exp", type = colModel)

      }
      if(gcFittedModel$model == "baranyi"){
        p <- p + scale_color_manual(name='Growth Model',
                                    values=c("model" = colModel, "Baranyi" = colModel)) +
          ggplot2::scale_color_discrete(labels="Baranyi", type = colModel)

      }
      if(gcFittedModel$model == "huang"){
        p <- p + scale_color_manual(name='Growth Model',
                                    values=c("model" = colModel, "Huang" = colModel)) +
          ggplot2::scale_color_discrete(labels="Huang", type = colModel)

      }
    }
    p <- p + theme(legend.key = element_blank(),
                   legend.background=element_blank(),
                   legend.title = element_blank(),
                   legend.position = if(equation == TRUE){
                     c(0.75, 0.1)
                   } else {
                     c(0.85, 0.1)
                   },
                   plot.margin = unit(c(1, 5, 1, 1), "lines")) +
      coord_cartesian(
        xlim = c(
          ggplot_build(p)$layout$panel_params[[1]]$x.range[1],
          ggplot_build(p)$layout$panel_params[[1]]$x.range[2]
        ),
        ylim = c(
          ggplot_build(p)$layout$panel_params[[1]]$y.range[1],
          ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
        ),
        clip = "off",
        expand = F
      )
  }

  # if(slope == TRUE){
  #   # /// add tangent at maximum slope
  #   mu     <- as.numeric(coef$mu[1])
  #   bla    <- (gcFittedModel$fit.time)*mu
  #   bla    <- bla+(-mu*lagtime)
  #   tangent.df <- data.frame("time" = gcFittedModel$fit.time,
  #                            "y" = bla)
  #   df.horizontal <- data.frame("time" = c(gcFittedModel$fit.time[1], lagtime),
  #                               "y" = gcFittedModel[["fit.data"]][1])
  #   p.yrange.end <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
  #   p <- p + geom_segment(aes(x = time[which.min(abs(bla))], y = y[which.min(abs(bla))],
  #                             xend = time[which.min(abs(y - p.yrange.end))],
  #                             yend = y[which.min(abs(y - p.yrange.end))]),
  #                         data = tangent.df, linetype = "dashed", color = colModel, linewidth = lwd) +
  #     geom_segment(aes(x = time[1], y = y[1], xend = time[2], yend = y[2]), data = df.horizontal,
  #                  linetype = "dashed", color = colModel, linewidth = lwd)
  # }
  if(export == FALSE && plot == FALSE){
    return(p)
  }
  if (export == TRUE){
    w <- width
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = FALSE)

    grDevices::png(paste0(out.dir, "/", paste(gcFittedModel$gcID, collapse = "_"), "_ModelFitPlot.png"),
                   width = w, height = h, units = 'in', res = 300)
    suppressWarnings( print(p) )
    grDevices::dev.off()

    grDevices::cairo_pdf(width = w, height = h, file = paste0(out.dir, "/", paste(gcFittedModel$gcID, collapse = "_"), "_ModelFitPlot.pdf"))
    suppressWarnings( print(p) )
    grDevices::dev.off()
  }
  if (plot == TRUE){
    suppressWarnings( print(p) )
  } else {
    return(p)
  }
}

#' Generic plot function for \code{gcBootSpline} objects.
#'
#' @param x A \code{drBootSpline} object created with \code{\link{growth.drBootSpline}} or stored within a \code{grofit} or \code{drFit} object created with \code{\link{growth.workflow}} or \code{\link{growth.drFit}}, respectively.
#' @param pch (Numeric) Shape of the raw data symbols.
#' @param colData (Numeric or Character) Color used to plot the raw data.
#' @param colSpline (Numeric or Character) Color used to plot the splines.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param cex.lab (Numeric) Font size of axis titles.
#' @param cex.axis (Numeric) Font size of axis annotations.
#' @param lwd (Numeric) Spline line width.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param combine (Logical) Indicate whether both dose-response curves and parameter plots shall be shown within the same window.
#' @param ... Further arguments to refine the generated base R plot.
#'
#' @return A plot with the all dose-response spline fits from the bootstrapping operation.
#'
#' @export plot.drBootSpline
#' @export
#'
#' @examples
#' conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
#' response <- c(1/(1+exp(-0.7*(4-conc[-20])))+stats::rnorm(19)/50, 0)
#'
#' TestRun <- growth.drBootSpline(conc, response, drID = "test",
#'         control = growth.control(log.x.dr = TRUE, smooth.dr = 0.8, nboot.dr = 50))
#'
#' print(summary(TestRun))
#' plot(TestRun, combine = TRUE)
#'
plot.drBootSpline <- function (x,
                               pch = 19,
                               colData = 1,
                               colSpline = "black",
                               cex.point = 1, cex.lab = 1.5, cex.axis = 1.3,
                               lwd = 2, plot = TRUE, export = FALSE,
                               height = 7, width = 9, out.dir = NULL, combine = FALSE,
                               ...)
{
  drBootSpline <- x
  if(!is.null(colSpline))
    colSpline <- toupper(colSpline)
  if(!is.null(colData))
    colData <- toupper(colData)
  colSpline <- scales::alpha(colSpline, 0.15)

  # drBootSpline an object of class drBootSpline
  if(methods::is(drBootSpline) != "drBootSpline") stop("x needs to be an object created with growth.drBootSpline.")
  # /// initialize "Empty Plot" function
  empty.plot  <- function(text = "Empty plot", main = "") {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    par(cex.lab = cex.lab, cex.axis = cex.axis)
    plot(c(0, 1, 0, 1, 0), c(0, 1, 1, 0, 0),
      type = "l", axes = FALSE, xlab = "", ylab = "", lwd = lwd,
      col = "gray", main = main)
    lines(c(0, 0), c(0, 1),
          type = "l", lwd = lwd, col = "gray")
    lines(c(1, 1), c(1, 0),
          type = "l", lwd = lwd, col = "gray")
    text(0.5, 0.1, text, col = "gray")
  }

  # /// check input parameters
  if (FALSE %in% (colData %in% c(colors(), 0:8)))
    stop("colData needs to be numeric from 0:8 or a string from colors()")
  if (is.numeric(pch) == FALSE)
    stop("Need numeric value for: pch")
  if (is.numeric(cex.point) == FALSE)
    stop("Need numeric value for: cex.point")

  if (drBootSpline$bootFlag == FALSE) {
    empty.plot()
  }
  else{
    p1 <- function(){
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))

      par(mar = c(2.1+cex.lab + 0.5*cex.axis, 2.1 + 1.3*cex.lab + 1.2*cex.axis, 4.1, 3.1),
          cex.lab = cex.lab,
          cex.axis = cex.axis)

      colSpline   <-
        rep(colSpline, (drBootSpline$control$nboot.dr %/% length(colSpline)) + 1)
      conc.log    <- log(drBootSpline$raw.conc + 1)
      test.log    <- log(drBootSpline$raw.test + 1)
      conc        <- drBootSpline$raw.conc
      test        <- drBootSpline$raw.test

      global.minx <- min(min(drBootSpline$boot.conc))
      global.maxx <- max(max(drBootSpline$boot.conc))
      global.miny <- min(min(drBootSpline$boot.test))
      global.maxy <- max(max(drBootSpline$boot.test))

      # initialize plot
      if ((drBootSpline$control$log.x.dr == TRUE) &&
          (drBootSpline$control$log.y.dr == FALSE)) {
        plot(
          c(global.minx, global.maxx),
          c(global.miny, global.maxy),
          type = "n", xlab="", ylab="", xaxt="n", main="")

        title(ylab = paste0("Response (", drBootSpline$control$dr.parameter, ")"), line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
        title(xlab = "Ln(1+concentration)", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
      }
      else{
        if ((drBootSpline$control$log.x.dr == FALSE) &&
            (drBootSpline$control$log.y.dr == FALSE)) {
          plot(
            c(global.minx, global.maxx),
            c(global.miny, global.maxy),
            type = "n", xlab="", ylab="", xaxt="n", main="")

          title(ylab = paste0("Response (", drBootSpline$control$dr.parameter, ")"), line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
          title(xlab = "Concentration", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
        }
        else{
          if ((drBootSpline$control$log.x.dr == TRUE) && (drBootSpline$control$log.y.dr == TRUE)) {
            plot(
              c(global.minx, global.maxx),
              c(global.miny, global.maxy),
              type = "n", xlab="", ylab="", xaxt="n", main="")

            title(ylab = paste0("Ln(1+Response", drBootSpline$control$dr.parameter, ")"), line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
            title(xlab = "Ln(1+Concentration)", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
          }
          else{
            if ((drBootSpline$control$log.x.dr == FALSE) && (drBootSpline$control$log.y.dr == TRUE)) {
              plot(
                c(global.minx, global.maxx),
                c(global.miny, global.maxy),
                type = "n", xlab="", ylab="", xaxt="n", main="")

              title(ylab = paste0("Ln(1+Response", drBootSpline$control$dr.parameter, ")"), line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
              title(xlab = "Concentration", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
            }
          }
        }
      }
      axis(1, mgp=c(3,1+0.5*cex.axis,0))


      # /// plot raw data
      points(
        drBootSpline$raw.conc,
        drBootSpline$raw.test,
        col = colData, bg = colData,
        pch = pch,
        cex = cex.point
      )
      title(drBootSpline$drID, line = 1, cex.main = cex.lab)

      # /// loop over all fitted splines and plot drFitSpline objects
      for (i in 1:drBootSpline$control$nboot.dr) {
        plot(
          drBootSpline$boot.drSpline[[i]],
          add = TRUE,
          ec50line = FALSE,
          pch = pch,
          bg = colData,
          colSpline = colSpline[i],
          colData = colData,
          cex.point = cex.point,
          lwd = lwd
        )
      }
    }
    p2 <- function(){
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))

      layout(matrix(c(1,2), nrow = 1, ncol = 2))
      if (sum(!is.na(drBootSpline$ec50.boot)) > 5) {
        hist(
          drBootSpline$ec50.boot,
          col = "gray",
          xaxt = "n",xlab="",ylab="", main=""
        )
        title(xlab = "EC50", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      }
      else{
        empty.plot()
      }
      if (sum(!is.na(drBootSpline$ec50y.boot)) > 5) {
        hist(
          drBootSpline$ec50y.boot,
          col = "gray",
          xaxt = "n",xlab="",ylab="", main=""
        )
        title(xlab = "yEC50", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      }
      else{
        empty.plot()
      }
    } # p2 <- function()
    p3 <- function(){
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))
      on.exit(par(mfrow = c(1, 1)))

      layout(matrix(c(1,1,1,2,2, 1,1,1,3,3), nrow = 5, ncol = 2))

      par(cex.lab = cex.lab, cex.axis = cex.axis)
      par(mar=c(2.1+cex.lab + 0.5*cex.axis, 2.6+1.3*cex.lab+1.2*cex.axis, 4.1+0.2*cex.lab, 3.1), mgp=c(3, 1, 0), las=0)


      colSpline   <-
        rep(colSpline, (drBootSpline$control$nboot.dr %/% length(colSpline)) + 1)
      conc.log    <- log(drBootSpline$raw.conc + 1)
      test.log    <- log(drBootSpline$raw.test + 1)
      conc        <- drBootSpline$raw.conc
      test        <- drBootSpline$raw.test

      global.minx <- min(drBootSpline$boot.conc[!is.na(drBootSpline$boot.conc)])
      global.maxx <- max(drBootSpline$boot.conc[!is.na(drBootSpline$boot.conc)])
      global.miny <- min(drBootSpline$boot.test[!is.na(drBootSpline$boot.test)])
      global.maxy <- max(drBootSpline$boot.test[!is.na(drBootSpline$boot.test)])

      # initialize plot
      if ((drBootSpline$control$log.x.dr == TRUE) &&
          (drBootSpline$control$log.y.dr == FALSE)) {
        plot(
          c(global.minx, global.maxx),
          c(global.miny, global.maxy),
          type = "n", xlab="", ylab="", xaxt="n", main="")

        title(ylab = paste0("Response (", drBootSpline$control$dr.parameter, ")"), line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
        title(xlab = "Ln(1+concentration)", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
      }
      else{
        if ((drBootSpline$control$log.x.dr == FALSE) &&
            (drBootSpline$control$log.y.dr == FALSE)) {
          plot(
            c(global.minx, global.maxx),
            c(global.miny, global.maxy),
            type = "n", xlab="", ylab="", xaxt="n", main="")

          title(ylab = paste0("Response (", drBootSpline$control$dr.parameter, ")"), line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
          title(xlab = "Concentration", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
        }
        else{
          if ((drBootSpline$control$log.x.dr == TRUE) && (drBootSpline$control$log.y.dr == TRUE)) {
            plot(
              c(global.minx, global.maxx),
              c(global.miny, global.maxy),
              type = "n", xlab="", ylab="", xaxt="n", main="")

            title(ylab = paste0("Ln(1+Response", drBootSpline$control$dr.parameter, ")"), line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
            title(xlab = "Ln(1+Concentration)", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
          }
          else{
            if ((drBootSpline$control$log.x.dr == FALSE) && (drBootSpline$control$log.y.dr == TRUE)) {
              plot(
                c(global.minx, global.maxx),
                c(global.miny, global.maxy),
                type = "n", xlab="", ylab="", xaxt="n", main="")

              title(ylab = paste0("Ln(1+Response", drBootSpline$control$dr.parameter, ")"), line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
              title(xlab = "Concentration", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
            }
          }
        }
      }
      axis(1, mgp=c(3,1+0.5*cex.axis,0))


      # /// plot raw data
      points(
        drBootSpline$raw.conc,
        drBootSpline$raw.test,
        col = colData, bg = colData,
        pch = pch,
        cex = cex.point
      )
      title(drBootSpline$drID, line = 1, cex.main = cex.lab)

      # /// loop over all fitted splines and plot drFitSpline objects
      for (i in 1:drBootSpline$control$nboot.dr) {
        plot(
          drBootSpline$boot.drSpline[[i]],
          add = TRUE,
          ec50line = FALSE,
          pch = pch,
          bg = colData,
          colSpline = colSpline[i],
          colData = colData,
          cex.point = cex.point,
          lwd = lwd
        )
      }

      if (sum(!is.na(drBootSpline$ec50.boot)) > 5) {
        hist(
          drBootSpline$ec50.boot,
          col = "gray",
          xaxt = "n",xlab="",ylab="", main=""
        )
        title(xlab = "EC50", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      }
      else{
        empty.plot()
      }
      if (sum(!is.na(drBootSpline$ec50y.boot)) > 5) {
        hist(
          drBootSpline$ec50y.boot,
          col = "gray",
          xaxt = "n",xlab="",ylab="", main=""
        )
        title(xlab = "yEC50", line = 1 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1.1 + 0.8*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      }
      else{
        empty.plot()
      }
    }
    if (export == TRUE){
      out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
      dir.create(out.dir, showWarnings = FALSE)
      if(!combine){
        w1 <- width
        h1 <- height
        grDevices::png(paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplines.png"),
                       width = w1, height = h1, units = 'in', res = 300)
        p1()
        grDevices::dev.off()
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(width = w1, height = h1, file = paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplines.pdf"))
        } else {
          message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
          grDevices::pdf(width = w1, height = h1, file = paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplines.pdf"))
        }
        p1()
        grDevices::dev.off()

        w2 <- width
        h2 <- height
        grDevices::png(paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplinesEC50.png"),
                       width = w2, height = h2, units = 'in', res = 300)
        p2()
        grDevices::dev.off()
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(width = w2, height = h2, file = paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplinesEC50.pdf"))
        } else {
          message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
          grDevices::pdf(width = w2, height = h2, file = paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplinesEC50.pdf"))
        }
        p2()
        grDevices::dev.off()
      } else {
        w <- width
        h <- height
        grDevices::png(paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplines.png"),
                       width = w, height = h, units = 'in', res = 300)
        p3()
        grDevices::dev.off()
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplines.pdf"))
        } else {
          message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
          grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplines.pdf"))
        }
        p3()
        grDevices::dev.off()
      }
    }

    if (plot == TRUE){
      if(!combine){
        p1()
        p2()
      } else {
        p3()
      }
    }
  } # /// else of if (drBootSpline$bootFlag==FALSE){
}

#' Generic plot function for \code{drFit} objects.
#'
#' code{plot.drFit} calls code{plot.drFitSpline} for each group used in a dose-response analysis
#'
#' @param x object of class \code{drFit}, created with \code{\link{growth.drFit}}.
#' @param combine (Logical) Combine the dose-response analysis results of all conditions into a single plot (\code{TRUE}) or not (\code{FALSE}).
#' @param names (String or vector of strings) Define conditions to combine into a single plot (if \code{combine = TRUE}). Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define conditions to exclude from the plot (if \code{combine = TRUE}). Partial matches with sample/group names are accepted.
#' @param pch (Numeric) Shape of the raw data symbols.
#' @param basesize (Numeric) Base font size.
#' @param colors (Numeric or character) Define colors for different conditions.
#' @param lwd (Numeric) Line width of the individual splines.
#' @param ec50line (Logical) Show pointed horizontal and vertical lines at the EC50 values (\code{TRUE}) or not (\code{FALSE}).
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.title (Character) Optional: Provide a title for the y-axis.
#' @param x.title (Character) Optional: Provide a title for the x-axis.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param log.y (Logical) Log-transform the y-axis of the plot (\code{TRUE}) or not (\code{FALSE})?
#' @param log.x (Logical) Log-transform the x-axis of the plot (\code{TRUE}) or not (\code{FALSE})?
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return One plot per condition tested in the dose-response analysis or a single plot showing all conditions if \code{control = growth.control(dr.method = "spline")} was used in \code{\link{growth.drFit}} and \code{combine = TRUE}.
#'
#' @export plot.drFit
#' @export
#' @importFrom ggplot2 aes element_text geom_errorbar geom_line
#'   geom_point geom_segment ggplot ggtitle labs guides
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous theme theme_classic theme_minimal xlab ylab
#'
#' @examples
#' \donttest{
#' # Create random growth data set
#' rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#' rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = "Test2")
#'
#' rnd.data <- list()
#' rnd.data[["time"]] <- rbind(rnd.data1$time, rnd.data2$time)
#' rnd.data[["data"]] <- rbind(rnd.data1$data, rnd.data2$data)
#'
#' # Run growth curve analysis workflow
#' gcFit <- growth.gcFit(time = rnd.data$time,
#'                        data = rnd.data$data,
#'                        parallelize = FALSE,
#'                        control = growth.control(fit.opt = "s",
#'                                                 suppress.messages = TRUE))
#'
#' # Perform dose-response analysis
#' drFit <- growth.drFit(gcTable = gcFit$gcTable,
#'                  control = growth.control(dr.parameter = "mu.spline"))
#'
#' # Inspect results
#' summary(drFit)
#'
#' plot(drFit)
#' }
plot.drFit <- function(x, combine = TRUE, names = NULL, exclude.nm = NULL, pch = 16, cex.point = 2, basesize = 15, colors = NULL, lwd = 0.7, ec50line = TRUE,
                       y.lim = NULL, x.lim = NULL, y.title = NULL, x.title = NULL, log.y = FALSE, log.x = FALSE,
                       plot = TRUE, export = FALSE, height = NULL, width = NULL, out.dir = NULL, out.nm = NULL, ...)
{
  drFit <- x
  if(!is.null(colors))
    colSpline <- toupper(colors)

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # x an object of class drFit
  if(methods::is(drFit) != "drFit") stop("x needs to be an object of class 'drFit', created with growth.drFit() or fl.drFit(control=fl.control(dr.method='spline').")
  if(length(drFit) == 1) stop("drFit is NA. Please run growth.drFit() with valid data input or growth.workflow() with 'ec50 = T'.")
  if(drFit$control$dr.method == "spline"){
    n <- length(drFit$drFittedSplines)
    if(combine == FALSE || n < 2){
      # /// plot all drFitSpline objects
      for (i in 1:n) {
        try(plot.drFitSpline(drFit$drFittedSplines[[i]], ec50line = ec50line, pch = pch,
                 y.lim = y.lim, x.lim = x.lim, y.title = NULL, x.title = NULL,
                 cex.point = cex.point, export = export, lwd = lwd,
                 plot = plot, height = 7, width = 9, out.dir = out.dir))
      }
    } else {
      if ((drFit$control$log.x.dr == TRUE) && (drFit$control$log.y.dr == TRUE)) {
        raw.x <- lapply(1:length(drFit$drFittedSplines), function(x) log(drFit$drFittedSplines[[x]]$raw.conc + 1))
        raw.y <- lapply(1:length(drFit$drFittedSplines), function(x) log(drFit$drFittedSplines[[x]]$raw.test + 1))
      } else if ((drFit$control$log.x.dr == FALSE) && (drFit$control$log.y.dr == TRUE)) {
        raw.x <- lapply(1:length(drFit$drFittedSplines), function(x) (drFit$drFittedSplines[[x]]$raw.conc))
        raw.y <- lapply(1:length(drFit$drFittedSplines), function(x) log(drFit$drFittedSplines[[x]]$raw.test + 1))
      } else if ((drFit$control$log.x.dr == TRUE) && (drFit$control$log.y.dr == FALSE)) {
        raw.x <- lapply(1:length(drFit$drFittedSplines), function(x) log(drFit$drFittedSplines[[x]]$raw.conc + 1))
        raw.y <- lapply(1:length(drFit$drFittedSplines), function(x) (drFit$drFittedSplines[[x]]$raw.test))
      } else if ((drFit$control$log.x.dr == FALSE) && (drFit$control$log.y.dr == FALSE)) {
        raw.x <- lapply(1:length(drFit$drFittedSplines), function(x) (drFit$drFittedSplines[[x]]$raw.conc))
        raw.y <- lapply(1:length(drFit$drFittedSplines), function(x) (drFit$drFittedSplines[[x]]$raw.test))
      }
      sample.nm <- nm <- names(raw.x) <- names(raw.y) <- names(drFit$drFittedSplines)

      # Convert range  and selecting arguments
      names <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", names)), pattern = ";"))
      exclude.nm <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.nm)), pattern = ";"))

      # Get name of conditions with multiple replicates; apply selecting arguments
      if(!is.null(names)  && length(names) > 0){
        if(!is.na(names) && names != ""){
          names <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", names)
          nm <- nm[grep(paste(names, collapse="|"), nm)]
        }
      }
      if(!is.null(names)  && !is.na(exclude.nm) && exclude.nm != ""){
        names.excl <- gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", exclude.nm))
        nm <- nm[!grepl(paste(names.excl, collapse="|"), gsub(" \\|.+", "", nm))]
      }
      if(length(nm)==0){
        stop("Please run plot.drFit() with valid 'names' or 'conc' argument.")
      }
      raw.x <- raw.x[nm]
      raw.y <- raw.y[nm]
      raw.df <- lapply(1:length(raw.x), function(x) data.frame("x" = raw.x[[x]], "y" = raw.y[[x]], "Condition" = rep(names(raw.x)[[x]], length(raw.x[[x]]))))
      # raw.df <- do.call("rbind", raw.df)

      n <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) length(raw.y[[i]][raw.x[[i]]==unique(raw.x[[i]])[x]])))
      conc <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) unique(raw.x[[i]])[x]))
      mean <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) mean(raw.y[[i]][raw.x[[i]]==unique(raw.x[[i]])[x]])))
      sd <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) sd(raw.y[[i]][raw.x[[i]]==unique(raw.x[[i]])[x]])))
      names <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) rep(names(drFit$drFittedSplines)[match(names(raw.x)[[i]], names(drFit$drFittedSplines))], length_out=length(raw.y[[i]][raw.x[[i]]==unique(raw.x[[i]])[x]]))))
      error <- stats::qnorm(0.975) * sd / sqrt(n) # standard error
      CI.L <- mean - error #left confidence interval
      CI.R <- mean + error #right confidence interval

      raw.df <- data.frame("Condition" = as.vector(names), "conc" = as.vector(conc), "mean" = as.vector(mean), "CI.L" = as.vector(CI.L), "CI.R" = as.vector(CI.R))
      if(log.x == TRUE) raw.df[raw.df[, "conc"] == 0, "conc"] <- 0.001
      # raw.df$Condition <- factor(raw.df$Condition, levels = raw.df$Condition)
      # raw.df$group <- gsub(" \\|.+", "", raw.df$name)
      # raw.df$mean[is.na(raw.df$mean)] <- 0
      # raw.df$CI.L[is.na(raw.df$CI.L)] <- 0
      # raw.df$CI.R[is.na(raw.df$CI.R)] <- 0


      res.df <- lapply(1:length(raw.x), function(x) data.frame("Condition" = names(drFit$drFittedSplines)[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]],
                                                               "ec50" = drFit$drFittedSplines[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]][["parameters"]][["EC50"]],
                                                               "yEC50" = drFit$drFittedSplines[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]][["parameters"]][["yEC50"]]))
      res.df <- do.call("rbind", res.df)

      spline.df  <- lapply(1:length(raw.x), function(x) data.frame("x" = drFit$drFittedSplines[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]][["fit.conc"]],
                                                                   "y" = drFit$drFittedSplines[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]][["fit.test"]],
                                                                   "Condition" = rep(names(drFit$drFittedSplines)[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]], length(drFit$drFittedSplines[[x]][["fit.conc"]]))))
      spline.df <- do.call("rbind", spline.df)

      if(log.x == TRUE) spline.df[spline.df[, "x"] == 0, "x"] <- 0.001

      nrow <- ceiling(length(drFit$drFittedSplines)/2)
      p <- ggplot(data = raw.df, aes(.data$conc, .data$mean, colour = .data$Condition)) +
        geom_point(size=cex.point, position = ggplot2::position_dodge( 0.015*max(conc)), shape = pch) +
        geom_errorbar(aes(ymin = .data$CI.L, ymax = .data$CI.R), width = 0.05*max(conc), position = ggplot2::position_dodge( 0.015*max(conc))) +
        geom_line(data = spline.df, aes(.data$x, .data$y, colour = .data$Condition), linewidth = lwd) +
        theme_classic(base_size = basesize) +
        theme(legend.position="bottom") +
        ggplot2::guides(color=ggplot2::guide_legend(nrow=nrow, byrow=TRUE))

      if(log.y == TRUE){
        if(!is.null(y.lim)){
          p <- p + scale_y_continuous(limits = y.lim, base_breaks(n = 5), trans = "log10")
        } else {
          p <- p + scale_y_continuous(breaks = base_breaks(n = 5), trans = "log10")
        }
      } else {
        if(!is.null(y.lim)){
          p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks())
        } else {
          p <- p + scale_y_continuous(breaks = scales::pretty_breaks())
        }
      }

      if(is.null(y.title) || y.title == ""){
        p <- p + ylab(label = ifelse(drFit$control$log.y.dr == TRUE, paste0("Ln(", drFit$control$dr.parameter, " + 1)"), paste0(drFit$control$dr.parameter)))
      } else {
        p <- p + ylab(label = y.title)
      }

      if(is.null(x.title) || x.title == ""){
        p <- p + xlab(ifelse(drFit$control$log.x.dr == TRUE, "Ln(concentration + 1)", "Concentration"))
      } else {
        p <- p + xlab(label = x.title)
      }

      if(ec50line){
        plot.xmin <- ggplot_build(p)$layout$panel_params[[1]]$x.range[1]
        plot.ymin <- ggplot_build(p)$layout$panel_params[[1]]$y.range[1]
        p <- p + geom_segment(data = res.df, aes(x = plot.xmin, xend = .data$ec50, y = .data$yEC50, yend = .data$yEC50), alpha = 0.7, linetype = 3, linewidth = 0.7*lwd) +
          geom_segment(data = res.df, aes(x = .data$ec50, xend = .data$ec50, y = plot.ymin, yend = .data$yEC50), alpha = 0.7, linetype = 3, linewidth = 0.7*lwd)

        if(log.y == TRUE){
          if(!is.null(y.lim)){
            p <- p + scale_y_continuous(limits = y.lim, expand = ggplot2::expansion(mult = c(0, 0.05)),breaks = base_breaks(n = 5), trans = "log10")
          } else {
            p <- p + scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)), breaks = base_breaks(n = 5), trans = "log10")
          }
        } else {
          if(!is.null(y.lim)){
            p <- p + scale_y_continuous(limits = y.lim, expand = ggplot2::expansion(mult = c(0, 0.05)),breaks = scales::pretty_breaks())
          } else {
            p <- p + scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)), breaks = scales::pretty_breaks())
          }
        }

        if(log.x == TRUE){
          if(!is.null(x.lim)){
            p <- p + scale_x_continuous(limits = x.lim, expand = ggplot2::expansion(mult = c(0, 0.05)), breaks = scales::pretty_breaks(), trans = "log10")
          } else {
            p <- p + scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)), breaks = scales::pretty_breaks(), trans = "log10")
          }
        } else {
          if(!is.null(x.lim)){
            p <- p + scale_x_continuous(limits = x.lim, expand = ggplot2::expansion(mult = c(0, 0.05)), breaks = scales::pretty_breaks())
          } else {
            p <- p + scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)), breaks = scales::pretty_breaks())
          }
        }

      } else {
        if(log.x == TRUE){
          if(!is.null(x.lim)){
            p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(), trans = "log10")
          } else {
            p <- p + scale_x_continuous(breaks = scales::pretty_breaks(), trans = "log10")
          }
        } else {
          if(!is.null(x.lim)){
            p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks())
          } else {
            p <- p + scale_x_continuous(breaks = scales::pretty_breaks())
          }
        }
      }




      if(is.null(colors)){
        if (length(drFit$drFittedSplines) <= 8) {
          p <- p + scale_fill_brewer(name = "Condition", palette = "Dark2") + scale_color_brewer(name = "Condition", palette = "Dark2")
        } else if (length(drFit$drFittedSplines) <= 50){
          p <- p + scale_fill_manual(name = "Condition",
                                     values = big_palette
          ) + scale_color_manual(name = "Condition",
                                 values = big_palette
          )
        }
      }
      if (export == TRUE){
        out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
        if(is.null(out.nm)) out.nm <- paste0("drFitPlot")
        if(is.null(width)){
          w <- 7 + ifelse(combine==TRUE,length(unique(names)), length(unique(names)))/15
        } else {
          w <- width
        }
        if(is.null(height)){
          h <- 6
        } else {
          h <- height
        }
        dir.create(out.dir, showWarnings = FALSE)
        grDevices::png(paste0(out.dir, "/", out.nm, ".png"),
                       width = w, height = h, units = 'in', res = 300)
        print(p)
        grDevices::dev.off()
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
        } else {
          message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
          grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
        }
        print(p)
        grDevices::dev.off()

        message(paste0("drFit plots exported to: ", out.dir, "/", out.nm))
      }
      if (plot == TRUE){
        print(p)
      } else {
        return(p)
      }
    } # else of if(combine == FALSE || n < 2)
  } # if(drFit$control$dr.method == "spline")
  else {
    n <- length(drFit$drFittedModels)
    for (i in 1:n) {
      try(plot.drFitModel(drFit$drFittedModels[[i]], ec50line = ec50line, pch = pch,
               y.lim = y.lim, x.lim = x.lim, y.title = NULL, x.title = NULL, lwd = lwd,
               cex.point = cex.point, export = export, cex.axis = basesize/15, cex.lab = basesize*1.2/15,
               plot = plot, height = 7, width = 9, out.dir = out.dir))
    }
  }
}

#' Generic plot function for \code{drFitSpline} objects.
#'
#' code{plot.drFitSpline} generates the spline fit plot for response-parameter vs. concentration data
#'
#' @param x object of class \code{drFitSpline}, created with \code{\link{growth.drFitSpline}}.
#' @param add (Logical) Shall the fitted spline be added to an existing plot? \code{TRUE} is used internally by \code{\link{plot.drBootSpline}}.
#' @param ec50line (Logical) Show pointed horizontal and vertical lines at the EC50 value (\code{TRUE}) or not (\code{FALSE}).
#' @param log ("x", "y", or "xy") Display the x- or y-axis on a logarithmic scale.
#' @param pch (Numeric) Shape of the raw data symbols.
#' @param colData (Numeric or character) Contour color of the raw data circles.
#' @param colSpline (Numeric or character) Spline line colour.
#' @param cex.point (Numeric) Size of the raw data symbols.
#' @param cex.lab (Numeric) Font size of axis titles.
#' @param cex.axis (Numeric) Font size of axis annotations.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.title (Character) Optional: Provide a title for the y-axis.
#' @param x.title (Character) Optional: Provide a title for the x-axis.
#' @param lwd (Numeric) Line width of spline.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Further arguments to refine the generated base R plot.
#'
#' @return A plot with the nonparametric dose-response fit.
#'
#' @export plot.drFitSpline
#' @export
#'
#' @examples
#' conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
#' response <- c(1/(1+exp(-0.7*(4-conc[-20])))+stats::rnorm(19)/50, 0)
#'
#' TestRun <- growth.drFitSpline(conc, response, drID = "test",
#'                      control = growth.control(log.x.dr = TRUE, smooth.dr = 0.8))
#'
#' print(summary(TestRun))
#' plot(TestRun)
plot.drFitSpline <- function (x,
                              add = FALSE,
                              ec50line = TRUE,
                              log = "",
                              pch = 16,
                              colSpline = 1,
                              colData = 1,
                              cex.point = 1, cex.lab = 1.5, cex.axis = 1.3,
                              y.lim = NULL, x.lim = NULL,
                              y.title = NULL, x.title = NULL,
                              lwd = 2, plot = TRUE, export = FALSE,
                              height = 7, width = 9, out.dir = NULL,
                              ...)
{
  drFitSpline <- x
  if(!is.null(colSpline))
    colSpline <- toupper(colSpline)
  if(!is.null(colData))
    colData <- toupper(colData)

  if(any(is.na(x.lim)))
    x.lim <- NULL
  if(any(is.na(y.lim)))
    y.lim <- NULL
  # drFitSpline an object of class drFitSpline
  if(methods::is(drFitSpline) != "drFitSpline") stop("x needs to be an object created with growth.drFitSpline.")
  # /// check input parameters
  if (is.logical(add) == FALSE)
    stop("Need logical value for: add")
  if (is.logical(ec50line) == FALSE)
    stop("Need logical value for: ec50line")
  if (is.numeric(pch) == FALSE)
    stop("Need numeric value for: pch")
  if (is.numeric(cex.point) == FALSE)
    stop("Need numeric value for: cex.point")

  p <- function(){
    if(drFitSpline$control$log.x.dr == TRUE){
      x_data <- log(drFitSpline$raw.conc + 1)
    } else {
      x_data <- drFitSpline$raw.conc
    }

    if(drFitSpline$control$log.y.dr == TRUE){
      y_data <- log(drFitSpline$raw.test + 1)
    } else {
      y_data <- drFitSpline$raw.test
    }

    if (add == FALSE) {
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))
      par(mar=c(2.1+cex.lab + 0.5*cex.axis, 2.1+1.3*cex.lab+1.2*cex.axis, 4.1, 3.1), mgp=c(3, 1, 0), las=0)
      par(cex.lab = cex.lab, cex.axis = cex.axis)

      if(drFitSpline$control$log.x.dr == TRUE){
        xlab = ifelse(!is.null(x.title) && x.title != "", x.title, "ln(1+concentration)")
      } else {
        xlab = ifelse(!is.null(x.title) && x.title != "", x.title, "concentration")
      }

      if(drFitSpline$control$log.y.dr == TRUE){
        ylab = ifelse(!is.null(y.title) && y.title != "", y.title, paste0("ln[1+", "Response", ifelse(!is.na(drFitSpline$parameters$test), paste0(" (", drFitSpline$parameters$test, ")"), ""), "]"))
      } else {
        ylab = ifelse(!is.null(y.title) && y.title != "", y.title, paste0("Response", ifelse(!is.na(drFitSpline$parameters$test), paste0(" (", drFitSpline$parameters$test, ")"), "")))
      }


      plot(
        x_data,
        y_data,
        log = log,
        pch = pch, bg = colData,
        cex = cex.point,
        col = colData,
        xlab="", ylab="",
        xaxt="n",
        ylim = y.lim,
        xlim = x.lim)

      axis(1, mgp=c(3,1+0.5*cex.axis,0))

      title(xlab = xlab, line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
      title(ylab = ylab, line = 1 + 0.5*cex.lab + 0.5*cex.axis, cex.lab = cex.lab)

      title(main = drFitSpline$drID, line = 1, cex.main = cex.lab)

      graphics::mtext(bquote(EC[50]: ~ .(round(x$parameters[["EC50.orig"]][[1]], digits = 3)) ~~~~
                               response(EC[50]): ~ .(round(x$parameters[["yEC50.orig"]], digits = 3)) ),
                      side = 4 , adj = 0.55, line = -2.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab*0.7)
    }
    else{

      points(
        x_data,
        y_data,
        pch = pch, bg = colData,
        cex = cex.point,
        col = colData
      )

    }

    try(lines(
      drFitSpline$fit.conc,
      drFitSpline$fit.test,
      type = "l",
      lwd = lwd,
      col = colSpline
    ))

    if (ec50line == TRUE) {
      #vertical lines
      totmin = min(min(drFitSpline$fit.conc), min(drFitSpline$fit.test))
      lines(c(drFitSpline$parameters$EC50, drFitSpline$parameters$EC50),
            c(totmin - 1, drFitSpline$parameters$yEC50),
            lty = 2)
      #horizontal
      lines(c(-1, drFitSpline$parameters$EC50),
            c(drFitSpline$parameters$yEC50, drFitSpline$parameters$yEC50),
            lty = 2)
    }
  } # p <- function()
  if (export == TRUE){
    w <- width
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = FALSE)
    grDevices::png(paste0(out.dir, "/", paste(drFitSpline$drID, collapse = "_"), "_drFitSpline.png"),
                   width = w, height = h, units = 'in', res = 300)
    p()
    grDevices::dev.off()
    if (requireNamespace("Cairo", quietly = TRUE)) {
      Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(drFitSpline$drID, collapse = "_"), "_drFitSpline.pdf"))
    } else {
      message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
      grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", paste(drFitSpline$drID, collapse = "_"), "_drFitSpline.pdf"))
    }
    p()
    grDevices::dev.off()
  }

  if (plot == TRUE){
    p()
  }
}

#' Generic plot function for \code{drFitModel} objects.
#'
#' @param x object of class \code{drFitModel}, created with \code{\link{growth.drFitModel}}.
#' @param type (Character) Specify how to plot the data. There are currently 5 options: "average" (averages and fitted curve(s); default), "none" (only the fitted curve(s)), "obs" (only the data points), "all" (all data points and fitted curve(s)), "bars" (averages and fitted curve(s) with model-based standard errors (see Details)), and "confidence" (confidence bands for fitted curve(s)).
#' @param ec50line (Logical) Show pointed horizontal and vertical lines at the EC50 values (\code{TRUE}) or not (\code{FALSE}).
#' @param add (Logical) If \code{TRUE} then add to already existing plot.
#' @param broken (Logical) If TRUE the x axis is broken provided this axis is logarithmic (using functionality in the CRAN package 'plotrix').
#' @param bp (Numeric) Specifying the break point below which the dose is zero (the amount of stretching on the dose axis above zero in order to create the visual illusion of a logarithmic scale including 0). The default is the base-10 value corresponding to the rounded value of the minimum of the log10 values of all positive dose values. This argument is only working for logarithmic dose axes.
#' @param gridsize (Numeric) Number of points in the grid used for plotting the fitted curves.
#' @param log (Character) String which contains '"x"' if the x axis is to be logarithmic, '"y"' if the y axis is to be logarithmic and '"xy"' or '"yx"' if both axes are to be logarithmic. The default is "x". The empty string "" yields the original axes.
#' @param n.xbreaks (Numeric) Number of breaks on the x-axis (if not log-transformed). The breaks are generated using \code{pretty}. Thus, the final number of breaks can deviate from the user input.
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis (if not log-transformed). The breaks are generated using \code{pretty}. Thus, the final number of breaks can deviate from the user input.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis of both growth curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on y-axis of the growth curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param pch (Numeric) Symbol used to plot data points.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param cex.axis (Numeric) Font size of axis annotations.
#' @param cex.lab (Numeric) Font size of axis titles.
#' @param col (Logical or a vector of colors) If \code{TRUE} default colours are used. If \code{FALSE} (default) no colors are used.
#' @param lwd (Numeric) Line width.
#' @param lty (Numeric) Specify the line type.
#' @param xlab (Character) An optional label for the x axis.
#' @param ylab (Character) An optional label for the y axis.
#' @param legend (Logical) If \code{TRUE} a legend is displayed.
#' @param legendText (Character) Specify the legend text (the position of the upper right corner of the legend box).
#' @param legendPos (Numeric) Vector of length 2 giving the position of the legend.
#' @param cex.legend numeric specifying the legend text size.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return A plot with the dose-response model fit.
#'
#' @export plot.drFitModel
#' @export
#'
#' @import drc
#'
#' @references Christian Ritz, Florent Baty, Jens C. Streibig, Daniel Gerhard (2015). _Dose-Response Analysis Using R_. PLoS ONE 10(12): e0146021. DOI: 10.1371/journal.pone.0146021
#'
#' @examples
#' conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
#' response <- c(1/(1+exp(-0.7*(4-conc[-20])))+stats::rnorm(19)/50, 0)
#'
#' TestRun <- growth.drFitModel(conc, response, drID = "test")
#'
#' print(summary(TestRun))
#' plot(TestRun)
plot.drFitModel <- function(x,
                            type = c("confidence", "all", "bars", "none", "obs", "average"),
                            ec50line = TRUE,
                            add = FALSE,
                            broken = TRUE,
                            bp,
                            gridsize = 200,
                            log = "x",
                            n.xbreaks,
                            n.ybreaks,
                            x.lim,
                            y.lim,
                            pch = 1,
                            cex.point,
                            cex.axis = 1,
                            cex.lab = 1.3,
                            col = 1,
                            lwd = 2,
                            lty = 2,
                            xlab,
                            ylab,
                            legend = TRUE,
                            legendText,
                            legendPos,
                            cex.legend = NULL,
                            plot = TRUE,
                            export = FALSE,
                            height = 7,
                            width = 9,
                            out.dir = NULL,
                            ...)
{
  drFitModel <- x
  type <- match.arg(type)
  model <- drFitModel$model
  conc <- drFitModel$raw.conc
  test <- drFitModel$raw.test
  bp <- ifelse(missing(bp)||!exists("bp")||bp == "", rlang::missing_arg(), bp)

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  if(missing(bp)){
    log10cl <- round(log10(min(conc[conc > 0]))) - 1
    bp <- 10^(log10cl)
  }
  if(missing(n.xbreaks)){
    xt <- NULL
    if(any(grep("x", log))){
      if(min(conc) <= 0){
        xt <- xgx_breaks_log10(c(bp, max(conc)))
        xt.minor <- xgx_minor_breaks_log10(c(bp, max(conc)))[-1]
      }
      else{
        xt <- xgx_breaks_log10(c(min(conc), max(conc)))
        xt.minor <- xgx_minor_breaks_log10(c(min(conc), max(conc)))
      }
    }
  } else {
    if(any(grep("x", log))){
      if(min(conc) <= 0){
        xt <- xgx_breaks_log10(c(bp, max(conc)))
        xt.minor <- xgx_minor_breaks_log10(c(bp, max(conc)))[-1]
      }
      else{
        xt <- xgx_breaks_log10(c(min(conc), max(conc)))
        xt.minor <- xgx_minor_breaks_log10(c(min(conc), max(conc)))
      }

    } else {
      if(is.null(n.xbreaks)){
        xt <- pretty(conc)
      } else {
        xt <- pretty(conc, n.xbreaks)
      }
    }
  }


    if(any(grep("y", log))){
      if(min(test) <= 0){
        log10cl.y <- round(log10(min(test[test > 0]))) - 2
        bp.y <- 10^(log10cl.y)

        yt <- xgx_breaks_log10(c(bp.y, max(test)))
        yt.minor <- xgx_minor_breaks_log10(c(bp.y, max(test)))[-1]
      }
      else{
        yt <- xgx_breaks_log10(c(min(test), max(test)))
        yt.minor <- xgx_minor_breaks_log10(c(min(test), max(test)))
      }
    } else {
      if(missing(n.ybreaks) || is.null(n.ybreaks)){
        yt <- pretty(test)
      } else {
        yt <- pretty(test, n.ybreaks)
      }
    }

  if(missing(legendText))
    legendText <- drFitModel$parameters$model
  if(missing(xlab) || xlab == "")
    xlab <- "Concentration"
  if(missing(ylab) || ylab == "")
    ylab <- paste0("Response (", drFitModel$parameters$test, ")")

  if(missing(legendPos)){
    legendPos <- c(max(conc), max(test) )
  }

  if(is.null(cex.legend)){
    cex.legend <- cex.lab
  }

  x.lim <- if(missing(x.lim) || !exists("x.lim") || x.lim == "" || is.null(x.lim) ){
    rlang::missing_arg()
  } else {
    x.lim
  }
  y.lim <- if(missing(y.lim) || !exists("y.lim") || y.lim == "" || is.null(y.lim) ){
    rlang::missing_arg()
  }else {
    y.lim
  }

  requireNamespace("drc", quietly = TRUE)
  if (export == TRUE){
    w <- width
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = FALSE)
    iter <- c(1,2,3)
  } else {
    iter <- 3
  }

  for(i in iter){
    if(i == 1){
      grDevices::png(paste0(out.dir, "/", paste(x$drID, collapse = "_"), "_drFitModel.png"),
                     width = w, height = h, units = 'in', res = 300)
    }
    if(i == 2){
      if (requireNamespace("Cairo", quietly = TRUE)) {
        Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(x$drID, collapse = "_"), "_drFitModel.pdf"))
      } else {
        message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
        grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", paste(x$drID, collapse = "_"), "_drFitModel.pdf"))
      }
    }

    par(mar=c(2.1+cex.lab + 0.5*cex.axis, 2.1+1.3*cex.lab+1.2*cex.axis, 4.1, 3.1), cex.lab = cex.lab, cex.axis = cex.axis)

    if(any(grep("x", log))){
      try(
        suppressWarnings(
          plot(
            x = model,
            broken = broken,
            type = "all",
            add = add,
            pch = pch,
            col = col,
            lwd = lwd,
            lty = lty,
            axes = FALSE,
            xt = xt,
            yt = yt,
            log = log,
            xlab = "",
            ylab = "",
            xlim = x.lim,
            ylim = y.lim,
            cex = cex.point,
            cex.axis = cex.axis,
            legend = legend,
            legendText = legendText,
            cex.legend = cex.legend,
            gridsize = gridsize,
            legendPos = legendPos,
            bp = bp
          )
        )
      )
      axis(1, lwd = 0, lwd.ticks = 1, at = xt, mgp=c(3,0.5+0.5*cex.axis,0), labels = c(0, as.character(as.numeric(xt))[-1]))
      axis(2, at = yt, las=1, line = 0, labels = as.character(as.numeric(yt)), mgp = c(3,1,0))
    } else {
      try(
        suppressWarnings(
          plot(
            x = model,
            broken = FALSE,
            type = "all",
            add = add,
            pch = pch,
            col = col,
            lwd = lwd,
            lty = lty,
            axes = FALSE,
            xt = xt,
            log = log,
            xlab = "",
            ylab = "",
            xlim = x.lim,
            ylim = y.lim,
            cex = cex.point,
            cex.axis = cex.axis,
            legend = legend,
            legendText = legendText,
            cex.legend = cex.legend,
            gridsize = gridsize,
            legendPos = legendPos
          )
        )
      )
      axis(1, lwd = 0, lwd.ticks = 1, at = xt, mgp=c(3,0.5+0.5*cex.axis,0), labels = as.character(as.numeric(xt)))
      axis(2, at = yt, las=1, line = 0, labels = as.character(as.numeric(yt)), mgp = c(3,1,0))
    }
    # axis(1, lwd = 0, lwd.ticks = 1, at = xt, mgp=c(3,1+0.5*cex.axis,0), line = 0, labels = as.character(as.numeric(xt)))

    # axis(2, at = yt, las=1, line = 0, labels = as.character(as.numeric(yt)))
    title(ylab = ylab, line = 1.1 + 0.2*cex.lab+1.2*cex.axis, cex.lab = cex.lab)
    title(xlab = xlab, line = 0.5 + 0.9*cex.lab + 0.5*cex.axis, cex.lab = cex.lab, cex.lab = cex.lab)
    if(any(grep("x", log))){
      axis(side=1, lwd = 0, lwd.ticks = 1, at=xt.minor, las=0, tck=-0.01, labels=FALSE, line = 0)
    }
    if(any(grep("y", log))){
      axis(side=2, at=yt.minor, las=0, tck=-0.01, labels=FALSE, line = 0)
    }
    try(
      suppressWarnings(
        plot(
          model,
          pch = pch,
          xlim = x.lim,
          ylim = y.lim,
          broken = broken,
          type = type,
          add = TRUE,
          col = col,
          lwd = lwd,
          lty = lty,
          log = log,
          cex = cex.point,
          gridsize = gridsize,
          bp = bp,
          legend = FALSE)
      )
    )
    # add title with condition
    title(main = drFitModel$drID)
    graphics::mtext(bquote(EC[50]: ~ .(round(x$parameters[["EC50"]][[1]], digits = 3)) ~ "\u00B1" ~ .(round(x$parameters[["EC50"]][[2]], digits = 3)) ~~~~
                             response(EC[50]): ~ .(round(x$parameters[["yEC50"]], digits = 3)) ),
                    side = 4 , adj = 0.55, line = -2.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab*0.9)
    if (ec50line == TRUE) {
      #vertical lines
      totmin = min(min(drFitModel$fit.conc), min(drFitModel$fit.test))
      lines(c(drFitModel$parameters$EC50[1], drFitModel$parameters$EC50[1]),
            c(totmin - 1, drFitModel$parameters$yEC50[1]),
            lty = 2, col = col, lwd = 0.5*lwd)
      #horizontal
      if(any(grep("x", log))){
        lines(c(bp, drFitModel$parameters$EC50[1]),
              c(drFitModel$parameters$yEC50[1], drFitModel$parameters$yEC50[1]),
              lty = 2, col = col, lwd = 0.5*lwd)
      } else {
        lines(c(totmin, drFitModel$parameters$EC50[1]),
              c(drFitModel$parameters$yEC50[1], drFitModel$parameters$yEC50[1]),
              lty = 2, col = col, lwd = 0.5*lwd)
      }

    }
    if(i < 3){
      grDevices::dev.off()
    }
  }
}

#' Generic plot function for \code{gcBootSpline} objects.
#'
#' @param x object of class \code{gcBootSpline}, created with \code{\link{growth.gcBootSpline}}.
#' @param pch (Numeric) Symbol used to plot data points.
#' @param colData (Numeric or character) Contour color of the raw data circles.
#' @param deriv (Logical) Show the derivatives (i.e., slope) over time in a secondary plot (\code{TRUE}) or not (\code{FALSE}).
#' @param colSpline (Numeric or character) Spline line colour.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param cex.lab (Numeric) Font size of axis titles.
#' @param cex.axis (Numeric) Font size of axis annotations.
#' @param lwd (Numeric) Spline line width.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on y-axis of the growth curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis of both growth curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim.deriv (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis of the derivative plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param combine (Logical) Indicate whether both growth curves and parameter plots shall be shown within the same window.
#' @param ... Further arguments to refine the generated base R plot.
#'
#' @return A single plot with the all spline growth fits from the bootstrapping operation and statistical distribution of growth parameters if \code{combine = TRUE} or separate plots for growth fits and parameter distributions (if \code{combine = FALSE}).
#'
#' @export plot.gcBootSpline
#' @export
#'
#' @examples
#' # Create random growth dataset
#' rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#'
#' # Extract time and growth data for single sample
#' time <- rnd.dataset$time[1,]
#' data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns
#'
#' # Introduce some noise into the measurements
#' data <- data + stats::runif(97, -0.01, 0.09)
#'
#' # Perform bootstrapping spline fit
#' TestFit <- growth.gcBootSpline(time, data, gcID = "TestFit",
#'               control = growth.control(fit.opt = "s", nboot.gc = 50))
#'
#' plot(TestFit, combine = TRUE, lwd = 0.5)
plot.gcBootSpline <- function(x, pch = 1, colData=1, deriv = TRUE,
                              colSpline = "dodgerblue3",
                              cex.point = 1, cex.lab = 1.5, cex.axis = 1.3,
                              lwd = 2, y.lim = NULL, x.lim = NULL, y.lim.deriv = NULL,
                              plot = TRUE, export = FALSE,
                              height = 7, width = 9, out.dir = NULL, combine = FALSE, ...)
{
  gcBootSpline <- x
  if(!is.null(colSpline))
    colSpline <- toupper(colSpline)
  if(!is.null(colData))
    colData <- toupper(colData)
  colSpline <- ggplot2::alpha(colSpline, 0.2)
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  # gcBootSpline an object of class gcBootSpline
  if(methods::is(gcBootSpline) != "gcBootSpline") stop("gcBootSpline needs to be an object created with growth.gcBootSpline.")
  # /// initialize "Empty Plot" function
  empty.plot <- function(text="Empty plot",main=""){
    plot(c(0,1,0,1,0),c(0,1,1,0,0), type="l", axes=FALSE, xlab="", ylab="", lwd=lwd, col="gray",main=main)
    lines(c(0,0),c(0,1), type="l", lwd=lwd, col="gray")
    lines(c(1,1),c(1,0), type="l", lwd=lwd, col="gray")
    text(0.5,0.1,text, col="gray")
  }

  # /// check input parameters
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (is.numeric(cex.point)==FALSE)   stop("Need numeric value for: cex")
  if (gcBootSpline$bootFlag==FALSE){
    try(showModal(modalDialog("Could not find successful bootstrapping operations for the chosen sample.", easyClose = TRUE)))
    message("Could not find successful bootstrapping operations for the provided gcBootSpline object. Did you define 'nboot.gc' in the control object when running computations?")
    par(cex.lab = cex.lab, cex.axis = cex.axis)
    par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), mgp=c(3, 1, 0), las=0)

    fit.log.x     <- gcBootSpline$control$log.x.gc
    fit.log.y     <- gcBootSpline$control$log.y.spline

    # /// plot data
    plot(gcBootSpline$raw.time, gcBootSpline$raw.data, col=colData, pch=pch, cex=cex.point,xlab="", ylab="")
    if (fit.log.y==FALSE){
      title(ylab = "Growth y(t) ", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
    }
    else if (fit.log.y==TRUE){
      title(ylab = "Growth [Ln(y(t)/y0)]", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
    }
    if (fit.log.x==TRUE){
      title(xlab = "Ln(1+time)", line = 1+0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
    }
    else if(fit.log.x==FALSE){
      title(xlab = "Time", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
    }
  }
  else{
    p1 <- function()
      {
      par(cex.lab = cex.lab, cex.axis = cex.axis)
      par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
      colSpline <- rep(colSpline, (gcBootSpline$control$nboot.gc%/%length(colSpline))+1)

      fit.log.x     <- gcBootSpline$control$log.x.gc
      fit.log.y     <- gcBootSpline$control$log.y.spline

      global.minx <- min(min(gcBootSpline$boot.time,na.rm=TRUE),na.rm=TRUE)
      global.maxx <- max(max(gcBootSpline$boot.time,na.rm=TRUE),na.rm=TRUE)
      global.miny <- min(min(gcBootSpline$boot.data,na.rm=TRUE),na.rm=TRUE)
      global.maxy <- max(max(gcBootSpline$boot.data,na.rm=TRUE),na.rm=TRUE)

      # initialize plot
      if(deriv == TRUE){
        layout(mat = matrix(c(1, 2), nrow = 2, ncol = 1),
               heights = c(2, 1.3), # Heights of the two rows
               widths = c(1, 1)) # Widths of the two columns
        par(mai=c(0.35,0.8,0.5,0))
        plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim, xaxt="n")
      } else {
        par(mai=c(0.7,0.8,0.5,0))
        plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim, xaxt="n")
      }

      # /// plot data
      points(gcBootSpline$raw.time, gcBootSpline$raw.data, col=colData, pch=pch, cex=cex.point)

      # /// plot all gcFittedSpline objects
      for(i in 1:gcBootSpline$control$nboot.gc){
       plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = TRUE, lwd=lwd,
                        deriv = FALSE, plot = FALSE, export = FALSE, pch=0, colSpline=colSpline[i], cex.point = cex.point)
      }
      # add plot title
      title(paste(gcBootSpline$gcID, collapse = "_"), line = ifelse(deriv==T, 0.8, 1), cex.main = cex.lab)
      #add axis titles
      if (fit.log.y==FALSE){
        title(ylab = "Growth y(t) ", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      else if (fit.log.y==TRUE){
        title(ylab = "Growth [Ln(y(t)/y0)]", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      # add second plot with slope over time
      if(deriv == TRUE){
        par(cex.axis = cex.axis)
        par(mai=c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.2,0))
        y.max <- ceiling(max(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) max(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        y.min <- floor(min(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) min(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        if(is.null(y.lim.deriv)){
          y.lim.deriv <- c(y.min, y.max)
        }
        if ((gcBootSpline$control$log.x.gc==FALSE)){
          try( plot(gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y,
                    xlab="", ylab="", type = "l", lwd=lwd, col = colSpline, ylim = y.lim.deriv, xlim = x.lim, xaxt = "n") )
        }
        if ((gcBootSpline$control$log.x.gc==TRUE)){
          try( lines(gcBootSpline$boot.gcSpline[[1]]$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y, lwd=lwd, xlab="Ln(1+time)", ylab="Growth rate", type = "l") )
        }
        for(i in 2:gcBootSpline$control$nboot.gc){
          plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = FALSE, lwd=lwd, xlim = x.lim,
                           deriv = TRUE, plot = FALSE, export = FALSE, pch=0, colSpline=colSpline[i], cex.point=cex.point)
        }
        title(ylab = "Growth rate", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      if (fit.log.x==TRUE){
        title(xlab = "Ln(1+time)", line = 1+0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      else if(fit.log.x==FALSE){
        title(xlab = "Time", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
      }
      axis(1, mgp=c(3,1+0.5*cex.axis,0))

      par(mfrow=c(1,1))
    } # p1 <- function()
    p2 <- function()
      {
      lambda    <- gcBootSpline$lambda
      mu        <- gcBootSpline$mu
      dY         <- gcBootSpline$dY
      integral  <- gcBootSpline$integral

      # /// plot histograms of growth parameters
      par(mfrow=c(2,2))

      if (sum(!is.na(lambda))>1){
        try(hist(lambda, col="gray",main=expression(bold(lambda)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "lambda", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else{ empty.plot("Empty plot!") }
      if (sum(!is.na(mu))>1){
        try(hist(mu , col="gray", main=expression(bold(mu)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "mu", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else { empty.plot("Empty plot!", main=expression(mu)) }
      if (sum(!is.na(dY))>1){
        try(hist(dY, col="gray", main=expression(bold(dY)),cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "dY", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else { empty.plot("Empty plot!", main=expression(dY)) }
      if (sum(!is.na(integral))>1){
        try(hist(integral, col="gray", main=expression(bold(Integral)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "integral", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else { empty.plot("Empty plot!", main=expression(Integral))}
      graphics::mtext(paste(gcBootSpline$gcID, collapse = "_"), side = 3, line = -1, outer = TRUE)
    } # p2 <- function()
    p3 <- function()
    {
      if(deriv) layout(matrix(c(1,2,3,5,1,2,4,6), nrow = 4, ncol = 2))
      else layout(matrix(c(1,2,4,1,3,5), nrow = 3, ncol = 2))

      par(cex.lab = cex.lab, cex.axis = cex.axis)
      par(mar=c(5.1, 4.1, 4.1, 2.1), mai = c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.7 + 0.2*cex.lab + 0.2*cex.axis, 0.5, 0.3), mgp=c(3, 1, 0), las=0)
      colSpline <- rep(colSpline, (gcBootSpline$control$nboot.gc%/%length(colSpline))+1)

      fit.log.x     <- gcBootSpline$control$log.x.gc
      fit.log.y     <- gcBootSpline$control$log.y.spline

      global.minx <- min(min(gcBootSpline$boot.time,na.rm=TRUE),na.rm=TRUE)
      global.maxx <- max(max(gcBootSpline$boot.time,na.rm=TRUE),na.rm=TRUE)
      global.miny <- min(min(gcBootSpline$boot.data,na.rm=TRUE),na.rm=TRUE)
      global.maxy <- max(max(gcBootSpline$boot.data,na.rm=TRUE),na.rm=TRUE)

      # initialize plot
      if(deriv == TRUE){
        par(mai=c(0.35 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.5,0))
        plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim, xaxt="n")
      } else {
        par(mai=c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.5,0))
        plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim, xaxt="n")
      }

      # /// plot data
      points(gcBootSpline$raw.time, gcBootSpline$raw.data, col=colData, pch=pch, cex=cex.point)

      # /// plot all gcFittedSpline objects
      for(i in 1:gcBootSpline$control$nboot.gc){
        plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = TRUE, lwd=lwd,
                         deriv = FALSE, plot = FALSE, export = FALSE, pch=0, colSpline=colSpline[i], cex.point=cex.point)
      }
      # add plot title
      title(paste(gcBootSpline$gcID, collapse = "_"), line = ifelse(deriv==T, 0.8, 1), cex.main = cex.lab)
      #add axis titles
      if (fit.log.y==FALSE){
        title(ylab = "Growth y(t)", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      else if (fit.log.y==TRUE){
        title(ylab = "Growth [Ln(y(t)/y0)]", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      # add second plot with slope over time
      if(deriv == TRUE){
        par(cex.axis = cex.axis)
        par(mai=c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.2,0))
        y.max <- ceiling(max(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) max(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        y.min <- floor(min(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) min(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        if(is.null(y.lim.deriv)){
          y.lim.deriv <- c(y.min, y.max)
        }
        if ((gcBootSpline$control$log.x.gc==FALSE)){
          try( plot(gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y,
                    xlab="", ylab="", type = "l", lwd=lwd, col = colSpline, ylim = y.lim.deriv, xlim = x.lim, xaxt = "n") )
        }
        if ((gcBootSpline$control$log.x.gc==TRUE)){
          try( lines(gcBootSpline$boot.gcSpline[[1]]$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y, lwd=lwd, xlab="Ln(1+time)", ylab="Growth rate", type = "l") )
        }
        for(i in 2:gcBootSpline$control$nboot.gc){
          plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = FALSE, lwd=lwd, xlim = x.lim,
                           deriv = TRUE, plot = FALSE, export = FALSE, pch=0, colSpline=colSpline[i], cex.point=cex.point)
        }
        title(ylab = "Growth rate", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      if (fit.log.x==TRUE){
        title(xlab = "Ln(1+time)", line = 1+0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      else if(fit.log.x==FALSE){
        title(xlab = "Time", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
      }
      axis(1, mgp=c(3,1+0.5*cex.axis,0))

      lambda    <- gcBootSpline$lambda
      mu        <- gcBootSpline$mu
      dY         <- gcBootSpline$dY
      integral  <- gcBootSpline$integral

      # /// plot histograms of growth parameters
      par(mai=c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.2*cex.lab,0))

      if (sum(!is.na(lambda))>1){
        try(hist(lambda, col="gray",main=expression(bold(lambda)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "lambda", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else{ empty.plot("Empty plot!") }
      if (sum(!is.na(mu))>1){
        try(hist(mu , col="gray", main=expression(bold(mu)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "mu", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else { empty.plot("Empty plot!", main=expression(mu)) }
      if (sum(!is.na(dY))>1){
        try(hist(dY, col="gray", main=expression(bold(dY)),cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "dY", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else { empty.plot("Empty plot!", main=expression(dY)) }
      if (sum(!is.na(integral))>1){
        try(hist(integral, col="gray", main=expression(bold(Integral)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "integral", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else { empty.plot("Empty plot!", main=expression(Integral))}
    } # p3 <- function()
  }
  if (export == TRUE && (gcBootSpline$bootFlag==TRUE)){
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = FALSE)
    if(!combine){
      w1 <- width
      h1 <- height

      grDevices::png(paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSpline.png"),
                     width = w1, height = h1, units = 'in', res = 300)
      p1()
      grDevices::dev.off()
      if (requireNamespace("Cairo", quietly = TRUE)) {
        Cairo::CairoPDF(width = w1, height = h1, file = paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSpline.pdf"))
      } else {
        message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
        grDevices::pdf(width = w1, height = h1, file = paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSpline.pdf"))
      }
      p1()
      grDevices::dev.off()

      w2 <- width
      h2 <- width
      grDevices::png(paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSplineParam.png"),
                     width = w2, height = h2, units = 'in', res = 300)
      p2()
      grDevices::dev.off()
      if (requireNamespace("Cairo", quietly = TRUE)) {
        Cairo::CairoPDF(width = w2, height = h2, file = paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSplineParam.pdf"))
      } else {
        message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
        grDevices::pdf(width = w2, height = h2, file = paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSplineParam.pdf"))
      }
      p2()
      grDevices::dev.off()
    } else {
      w <- width
      h <- width

      grDevices::png(paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSpline.png"),
                     width = w, height = h, units = 'in', res = 300)
      p3()
      grDevices::dev.off()
      if (requireNamespace("Cairo", quietly = TRUE)) {
        Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSpline.pdf"))
      } else {
        message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
        grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSpline.pdf"))
      }
      p3()
      grDevices::dev.off()
    }
  }

  if (plot == TRUE && (gcBootSpline$bootFlag==TRUE)){
    if(!combine){
      p1()
      dev.new()
      p2()
    } else {
      p3()
    }

  }
}

#' Generic plot function for \code{gcFitSpline} objects.
#'
#' code{plot.gcFitSpline} generates the spline fit plot for a single sample.
#'
#' @param x object of class \code{gcFitSpline}, created with \code{\link{growth.gcFitSpline}}.
#' @param add (Logical) Shall the fitted spline be added to an existing plot? \code{TRUE} is used internally by \code{\link{plot.gcBootSpline}}.
#' @param raw (Logical) Display raw growth as circles (\code{TRUE}) or not (\code{FALSE}).
#' @param slope (Logical) Show the slope at the maximum growth rate (\code{TRUE}) or not (\code{FALSE}).
#' @param deriv (Logical) Show the derivative (i.e., slope) over time in a secondary plot (\code{TRUE}) or not (\code{FALSE}).
#' @param spline (Logical) Only for \code{add = TRUE}: add the current spline to the existing plot (\code{FALSE}).
#' @param log.y (Logical) Log-transform the y-axis (\code{TRUE}) or not (\code{FALSE}).
#' @param pch (Numeric) Symbol used to plot data points.
#' @param colData (Numeric or character) Contour color of the raw data circles.
#' @param colSpline (Numeric or character) Spline line colour.
#' @param basesize (Numeric) Base font size.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param lwd (Numeric) Spline line width.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on y-axis of the growth curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis of both growth curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim.deriv (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis of the derivative plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param y.title (Character) Optional: Provide a title for the y-axis of the growth curve plot.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both growth curve and derivative plots.
#' @param y.title.deriv (Character) Optional: Provide a title for the y-axis of the derivative plot.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Further arguments to refine the generated base R plot (if \code{add = TRUE}.
#'
#' @return A plot with the nonparametric fit.
#'
#' @export plot.gcFitSpline
#' @export
#'
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggtitle labs guides
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab .data
#'
#' @examples
#' # Create random growth dataset
#' rnd.dataset <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#'
#' # Extract time and growth data for single sample
#' time <- rnd.dataset$time[1,]
#' data <- rnd.dataset$data[1,-(1:3)] # Remove identifier columns
#'
#' # Perform spline fit
#' TestFit <- growth.gcFitSpline(time, data, gcID = "TestFit",
#'                  control = growth.control(fit.opt = "s"))
#'
#' plot(TestFit)
#'
plot.gcFitSpline <- function(x, add=FALSE, raw = TRUE, slope=TRUE, deriv = TRUE, spline = TRUE, log.y = TRUE,
                             pch=1, colData=1, colSpline="dodgerblue3", basesize=16, cex.point = 2, lwd = 0.7,
                             y.lim = NULL, x.lim = NULL, y.lim.deriv = NULL, n.ybreaks = 6,
                             y.title = NULL, x.title = NULL, y.title.deriv = NULL,
                             plot = TRUE, export = FALSE, width = 8, height = ifelse(deriv == TRUE, 8, 6),
                             out.dir = NULL, ...)
{
  gcFittedSpline <- x
  if(!is.null(colSpline))
    colSpline <- toupper(colSpline)
  if(!is.null(colData))
    colData <- toupper(colData)
  n.ybreaks <- as.numeric(n.ybreaks)
  # x an object of class gcFittedSpline
  if(methods::is(gcFittedSpline) != "gcFitSpline") stop("x needs to be an object created with growth.gcFitSpline().")
  # /// check input parameters
  if (is.logical(add)==FALSE)   stop("Need logical value for: add")
  if (is.logical(slope)==FALSE) stop("Need logical value for: slope")
  if (is.logical(deriv)==FALSE) stop("Need logical value for: deriv")
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")

  suppressWarnings(assign("x.lim" ,as.numeric(x.lim)))
  if(all(is.na(x.lim))) x.lim <- NULL
  suppressWarnings(assign("y.lim" ,as.numeric(y.lim)))
  if(all(is.na(y.lim))) y.lim <- NULL
  suppressWarnings(assign("y.lim.deriv" ,as.numeric(y.lim.deriv)))
  if(all(is.na(y.lim.deriv))) y.lim.deriv <- NULL

  # /// check if a data fit is available
  if ((is.na(gcFittedSpline$fitFlag)==TRUE)|(gcFittedSpline$fitFlag==FALSE)){
    warning("plot.gcFitSpline: no data fit available!")
  }

  if (add==TRUE){
    if(spline == TRUE){
      # /// try to plot data fit
      if ((gcFittedSpline$control$log.x.gc==FALSE) && (gcFittedSpline$control$log.y.spline==FALSE)){
        try( lines(gcFittedSpline$fit.time, gcFittedSpline$fit.data, sub=gcFittedSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd) )
      }

      if ((gcFittedSpline$control$log.x.gc==FALSE) && (gcFittedSpline$control$log.y.spline==TRUE)){
        try( lines(gcFittedSpline$fit.time, gcFittedSpline$fit.data, sub=gcFittedSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd) )
      }

      if ((gcFittedSpline$control$log.x.gc==TRUE)  && (gcFittedSpline$control$log.y.spline==FALSE)){
        try( lines(gcFittedSpline$fit.time, gcFittedSpline$fit.data, sub=gcFittedSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd ) )
      }

      if ((gcFittedSpline$control$log.x.gc==TRUE)  && (gcFittedSpline$control$log.y.spline==TRUE)){
        try( lines(gcFittedSpline$fit.time, gcFittedSpline$fit.data, sub=gcFittedSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd) )
      }
      # /// add tangent at maximum slope
      if (slope==TRUE){
        mu     <- as.numeric(gcFittedSpline$parameters$mu)
        lambda <- as.numeric(gcFittedSpline$parameters$lambda)

        time <- seq(lambda, max(gcFittedSpline$"fit.time"), length=200)
        y_tangent <- gcFittedSpline$parameters["b.tangent"][[1]]+time*mu
        try(lines(time, y_tangent, lty=2, lwd= 2.8*lwd, col=ggplot2::alpha(colSpline, 0.85), ...))
        try(lines(c(min(gcFittedSpline$"raw.time"[1]), lambda), rep(gcFittedSpline$"raw.data"[1], 2), lty=2, lwd=2.8*lwd, col=ggplot2::alpha(colSpline, 0.7)))
      }
    }
    if (deriv  == TRUE){
      if ((gcFittedSpline$control$log.x.gc==FALSE)){
        try( lines(gcFittedSpline$spline.deriv1$x, gcFittedSpline$spline.deriv1$y, lwd=2.8*lwd, xlab="", ylab="", col = colSpline) )
      }
      if ((gcFittedSpline$control$log.x.gc==TRUE)){
        try( lines(gcFittedSpline$spline.deriv1$x, gcFittedSpline$spline.deriv1$y, lwd=2.8*lwd, xlab="", ylab="", col = colSpline) )
      }
    }
  } # if (add == TRUE)
  else {
    if(gcFittedSpline$fitFlag == TRUE){
      coef <- gcFittedSpline[["parameters"]]
      lagtime <- coef["lambda"][[1]][1]
      # correct for log transformation
      if(gcFittedSpline$control$log.y.spline == TRUE){
        fit.data <-
          c(rep(NA, length(gcFittedSpline[["raw.data"]]) - length(gcFittedSpline[["fit.data"]])), exp(gcFittedSpline[["fit.data"]]) *
              gcFittedSpline[["data.in"]][1])
      } else {
        fit.data <- c(rep(NA, length(gcFittedSpline[["raw.data"]]) - length(gcFittedSpline[["fit.data"]])), gcFittedSpline[["fit.data"]])
      }
      if(gcFittedSpline$control$log.y.spline == TRUE){
        df <- data.frame("time" = gcFittedSpline[["raw.time"]],
                         "data" = exp(gcFittedSpline[["raw.data"]])*gcFittedSpline[["data.in"]][1],
                         "fit.time" = c(rep(NA, length(gcFittedSpline[["raw.time"]])-length(gcFittedSpline[["fit.time"]])), gcFittedSpline[["fit.time"]]),
                         "fit.data" = fit.data)
      } else{
        df <- data.frame("time" = gcFittedSpline[["raw.time"]],
                         "data" = gcFittedSpline[["raw.data"]],
                         "fit.time" = c(rep(NA, length(gcFittedSpline[["raw.time"]])-length(gcFittedSpline[["fit.time"]])), gcFittedSpline[["fit.time"]]),
                         "fit.data" = fit.data)
      }


      p <- ggplot(df, aes(x=.data$time, y=.data$data)) +
        geom_point(shape=pch, size = cex.point,alpha = 0.6, stroke=0.15*cex.point, color = colData) +
        geom_line(aes(x=.data$fit.time, y = .data$fit.data, color = "spline"), linewidth = lwd) +
        xlab(ifelse(is.null(x.title), "Time", x.title)) +
        ylab(ifelse(is.null(y.title), "Growth [y(t)]", y.title)) +
        theme_classic(base_size = basesize) +
        ggtitle(gsub(" \\| NA", "", paste(gcFittedSpline$gcID, collapse=" | "))) +
        theme(legend.key = element_blank(),
              legend.background=element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.90, 0.08),
              plot.title = element_text(size = basesize*1.1, face = "bold", vjust = 3),
              plot.subtitle = element_text(size = basesize*0.8),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = unit(c(1,2,1,1), "lines")) +
        scale_color_manual(name='Growth fit',
                           breaks = "Spline fit",
                           values=c("spline" = ggplot2::alpha(colSpline, 0.7), "Spline fit" = ggplot2::alpha(colSpline, 0.7)))

      if(log.y == TRUE){
        if(!is.null(y.lim)){
          if(!is.na(y.lim[1]) && y.lim[1] <= 0){
            message("A lower y axis limit of <= 0 is not supported for log scaling. Lower limit set to 0.001")
            y.lim[1] <- 0.001
          }
          p <- p + scale_y_continuous(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10), trans = 'log')
        } else {
          p <- p + scale_y_continuous(breaks = scales::log_breaks(n = n.ybreaks, base = 10), trans = 'log')
        }
      } else {
        if(!is.null(y.lim)){
          p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks))
        } else {
          p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks))
        }
      }


      p.yrange.end <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]

      if(!is.null(x.lim)){
        p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
      } else {
        p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      }

      x_limit <- ggplot_build(p)$layout$panel_params[[1]]$x.range
      y_limit <- c(min(df$data), max(df$data))

      p <- p +
        # annotate(
        #   "text",
        #   label = paste("t0:", gcFittedSpline$control$t0, "  min.growth:", gcFittedSpline$control$min.growth, "  smoothing:", gcFittedSpline$control$smooth.gc),
        #   x = 0.5*x_limit[2],
        #   y = ifelse(!is.null(y.lim) && !is.na(y.lim[2]), 1.05 * y.lim[2], 1.3 * y_limit[2]),
        #   angle = 0, parse = FALSE, size = basesize*3.2/12) +
        labs(subtitle = paste("t0:", gcFittedSpline$control$t0,
                              " tmax:", gcFittedSpline$control$tmax,
                              "  min.growth:", gcFittedSpline$control$min.growth,
                              "  max.growth:", gcFittedSpline$control$max.growth,
                              "  smoothing:", gcFittedSpline$control$smooth.gc)
               ) +
        annotate(
          "text",
          label = list(bquote(mu: ~ .(round(gcFittedSpline$parameters$mu, digits = 3))~~~~
                           lambda: ~ .(round(gcFittedSpline$parameters$lambda, digits = 3))~~~~
                           t[max]: ~ .(round(gcFittedSpline$parameters$t.max, digits = 2)))),
          x = 1.02*x_limit[2],
          y = ifelse(deriv==TRUE, ifelse(log.y == TRUE, 0.5, -0.3), 0.9) * ifelse(!is.null(y.lim) && !is.na(y.lim[1]), y.lim[1], y_limit[1]),
          hjust = 0,
          angle = 90, parse = TRUE, size = basesize*3.2/12)
      if(!is.null(y.lim) && !is.na(y.lim[2])){
        p <- p + coord_cartesian(xlim = c(0, x_limit[2]*0.96), ylim = c(y_limit[1], y.lim[2]), clip = "off")
      } else {
        p <- p + coord_cartesian(xlim = c(0, x_limit[2]*0.96), ylim = c(y_limit[1], y_limit[2]), clip = "off")
      }

      # annotate(
      #   "text",
      #   label = paste("t0:", gcFittedSpline$control$t0, "  min.growth:", gcFittedSpline$control$min.growth, "  smoothing:", gcFittedSpline$control$smooth.gc),
      #   x = 19,
      #   y = 0.1 * y_limit[2],
      #   angle = 90, parse = FALSE, size = basesize*3.2/12) +
      #   coord_cartesian(xlim = c(x_limit[1], x_limit[2]), clip = "off")

      # /// add tangent at maximum slope
      if(slope == TRUE && log.y == TRUE){
        mu     <- as.numeric(coef$mu[1])
        if(gcFittedSpline$fitFlag2){
          lagtime2 <- coef$lambda2
          growth.time <- gcFittedSpline$fit.time[which.max(gcFittedSpline$fit.data)]
          mu2 <- coef$mu2
          if(lagtime2 < lagtime && lagtime2 > gcFittedSpline$raw.time[1]){
            # time values for tangent at mumax
            time_start.ndx <- which.min(abs(gcFittedSpline$fit.time-(coef$t.max-0.15*growth.time)))
            time_start <- gcFittedSpline$fit.time[time_start.ndx]
            time <- seq(time_start, max(gcFittedSpline$fit.time), length=200)
            # y values for tangent at mumax
            if(gcFittedSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (mu*time)
            }
            time <- time[bla >= 0.6* gcFittedSpline$data.in[1]][bla <= 1.15 * max(gcFittedSpline$fit.data)]
            bla <- bla[bla >= 0.6* gcFittedSpline$data.in[1]][bla <= 1.15 * max(gcFittedSpline$fit.data)]
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)
            # time values for tangent at mumax2
            time2 <- seq(ifelse(lagtime2<0, 0, lagtime2), max(gcFittedSpline$"fit.time"), length=200)
            # y values for tangent at mumax
            if(gcFittedSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu2*time2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (mu2*time2)
            }
            time2 <- time2[bla2 <= 1.15 * max(gcFittedSpline$fit.data)]
            bla2 <- bla2[bla2 <= 1.15 * max(gcFittedSpline$fit.data)]

            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)
            df.horizontal2 <- data.frame("time" = c(gcFittedSpline[["raw.time"]][1], lagtime2),
                                         "y" = if(gcFittedSpline$control$log.y.spline){
                                           exp(gcFittedSpline[["fit.data"]][1])*gcFittedSpline[["data.in"]][1]
                                         } else {
                                           gcFittedSpline[["fit.data"]][1]
                                         })

            if(gcFittedSpline$control$log.y.spline){
              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla))], y = .data$y[which.min(abs(bla))],
                                        xend = .data$time[which.min(abs(.data$y - 1.1*p.yrange.end))],
                                        yend = .data$y[which.min(abs(.data$y - 1.1*p.yrange.end))]),
                                    data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = 0.7*lwd)

              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla2))], y = .data$y[which.min(abs(bla2))],
                                        xend = .data$time[which.min(abs(.data$y - 1.1*p.yrange.end))],
                                        yend = .data$y[which.min(abs(.data$y - 1.1*p.yrange.end))]),
                                    data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = 0.7*lwd)
            }
            else {
              p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
              p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = lwd)
            }

            if(!(lagtime2 <0)){
              p <- p + geom_segment(aes(x = .data$time[1], y = .data$y[1], xend = .data$time[2], yend = .data$y[2]), data = df.horizontal2,
                                    linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = lwd)
            }
          } # if(lagtime2 < lagtime)
          else {
            # time values for tangent at mumax
            time <- seq(ifelse(lagtime<0, 0, lagtime), max(gcFittedSpline$"fit.time"), length=200)
            # y values for tangent at mumax
            if(gcFittedSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (mu*time)
            }
            time <- time[bla <= 1.15 * max(gcFittedSpline$fit.data)]
            bla <- bla[bla <= 1.15 * max(gcFittedSpline$fit.data)]
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)

            df.horizontal <- data.frame("time" = c(gcFittedSpline[["raw.time"]][1], lagtime),
                                        "y" = if(gcFittedSpline$control$log.y.spline){
                                          exp(gcFittedSpline[["fit.data"]][1])*gcFittedSpline[["data.in"]][1]
                                        } else {
                                          gcFittedSpline[["fit.data"]][1]
                                        })

            # time values for tangent at mumax2
            time2_start.ndx <- which.min(abs(gcFittedSpline$fit.time-(coef$t.max2-0.15*growth.time)))
            time2_start <- gcFittedSpline$fit.time[time2_start.ndx]
            time2 <- seq(time2_start, max(gcFittedSpline$"fit.time"), length=200)
            # y values for tangent at mumax
            if(gcFittedSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu2*time2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (mu2*time2)
            }
            time2 <- time2[bla2 >= 0.6* gcFittedSpline$data.in[1]][bla2 <= 1.15 * max(gcFittedSpline$fit.data)]
            bla2 <- bla2[bla2 >= 0.6* gcFittedSpline$data.in[1]][bla2 <= 1.15 * max(gcFittedSpline$fit.data)]
            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)

            if(gcFittedSpline$control$log.y.spline){
              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla))], y = .data$y[which.min(abs(bla))],
                                        xend = .data$time[which.min(abs(.data$y - 2*p.yrange.end))],
                                        yend = .data$y[which.min(abs(.data$y - 2*p.yrange.end))]),
                                    data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = 0.9*lwd)

              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla2))], y = .data$y[which.min(abs(bla2))],
                                        xend = .data$time[which.min(abs(.data$y - 3.5*p.yrange.end))],
                                        yend = .data$y[which.min(abs(.data$y - 3.5*p.yrange.end))]),
                                    data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = 0.9*lwd)
            }
            else {
              p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
              p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = lwd)
            }

            if(!(lagtime <0)){
              p <- p + geom_segment(aes(x = .data$time[1], y = .data$y[1], xend = .data$time[2], yend = .data$y[2]), data = df.horizontal,
                                    linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
            }
          }
        } # if(gcFittedSpline$fitFlag2)
        else {
          # time values for tangent
          time <- seq(ifelse(lagtime<0, 0, lagtime), max(gcFittedSpline$"fit.time"), length=200)
          # y values for tangent
          if(gcFittedSpline$control$log.y.spline){
            bla <- (exp(coef["b.tangent"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu*time)
          } else {
            bla <- coef["b.tangent"][[1]] + (mu*time)
          }
          time <- time[bla <= 1.15 * max(gcFittedSpline$fit.data)]
          bla <- bla[bla <= 1.15 * max(gcFittedSpline$fit.data)]

          tangent.df <- data.frame("time" = time,
                                   "y" = bla)
          df.horizontal <- data.frame("time" = c(gcFittedSpline[["raw.time"]][1], lagtime),
                                      "y" = if(gcFittedSpline$control$log.y.spline){
                                        exp(gcFittedSpline[["fit.data"]][1])*gcFittedSpline[["data.in"]][1]
                                      } else {
                                        gcFittedSpline[["fit.data"]][1]
                                      })

          if(gcFittedSpline$control$log.y.spline){
            p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla))], y = .data$y[which.min(abs(bla))],
                                      xend = .data$time[which.min(abs(.data$y - 1.1*y_limit[2]))],
                                      yend = .data$y[which.min(abs(.data$y - 1.1*y_limit[2]))]),
                                  data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = 0.7*lwd)
          }
          else {
            p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
          }

          if(!(lagtime <0)){
            p <- p + geom_segment(aes(x = .data$time[1], y = .data$y[1], xend = .data$time[2], yend = .data$y[2]), data = df.horizontal,
                                  linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
          }
        } # else of if(gcFittedSpline$fitFlag2)
      } # if(slope == TRUE && log.y == TRUE)

      # /// add tangent at maximum slope
      if(slope == TRUE && log.y == FALSE){
        mu     <- as.numeric(coef$mu[1])
        if(gcFittedSpline$fitFlag2){
          lagtime2 <- coef$lambda2
          growth.time <- gcFittedSpline$fit.time[which.max(gcFittedSpline$fit.data)]
          mu2 <- coef$mu2
          if(lagtime2 < lagtime && lagtime2 > gcFittedSpline$raw.time[1]){
            # time values for tangent at mumax
            time_start.ndx <- which.min(abs(gcFittedSpline$fit.time-(coef$t.max-0.15*growth.time)))
            time_start <- gcFittedSpline$fit.time[time_start.ndx]
            time <- seq(time_start, max(gcFittedSpline$fit.time), length=200)
            # y values for tangent at mumax
            if(gcFittedSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (mu*time)
            }
            time <- time[bla >= 0.6* gcFittedSpline$data.in[1]][bla <= 1.15 * max(gcFittedSpline$fit.data)]
            bla <- bla[bla >= 0.6* gcFittedSpline$data.in[1]][bla <= 1.15 * max(gcFittedSpline$fit.data)]
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)
            # time values for tangent at mumax2
            time2 <- seq(ifelse(lagtime2<0, 0, lagtime2), max(gcFittedSpline$"fit.time"), length=200)
            # y values for tangent at mumax
            if(gcFittedSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu2*time2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (mu2*time2)
            }
            time2 <- time2[bla2 <= 1.15 * max(gcFittedSpline$fit.data)]
            bla2 <- bla2[bla2 <= 1.15 * max(gcFittedSpline$fit.data)]

            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)
            df.horizontal2 <- data.frame("time" = c(gcFittedSpline[["raw.time"]][1], lagtime2),
                                         "y" = if(gcFittedSpline$control$log.y.spline){
                                           exp(gcFittedSpline[["fit.data"]][1])*gcFittedSpline[["data.in"]][1]
                                         } else {
                                           gcFittedSpline[["fit.data"]][1]
                                         })

            if(gcFittedSpline$control$log.y.spline){
              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla))], y = .data$y[which.min(abs(bla))],
                                        xend = .data$time[which.min(abs(.data$y - 1.1*y_limit[2]))],
                                        yend = .data$y[which.min(abs(.data$y - 1.1*y_limit[2]))]),
                                    data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = 0.7*lwd)

              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla2))], y = .data$y[which.min(abs(bla2))],
                                        xend = .data$time[which.min(abs(.data$y - 1.1*y_limit[2]))],
                                        yend = .data$y[which.min(abs(.data$y - 1.1*y_limit[2]))]),
                                    data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = 0.7*lwd)
            }
            else {
              p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
              p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = lwd)
            }

            if(!(lagtime2 <0)){
              p <- p + geom_segment(aes(x = .data$time[1], y = .data$y[1], xend = .data$time[2], yend = .data$y[2]), data = df.horizontal2,
                                    linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = lwd)
            }
          } # if(lagtime2 < lagtime)
          else {
            # time values for tangent at mumax
            time <- seq(ifelse(lagtime<0, 0, lagtime), max(gcFittedSpline$"fit.time"), length=200)
            # y values for tangent at mumax
            if(gcFittedSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (mu*time)
            }
            time <- time[bla <= 1.15 * max(gcFittedSpline$fit.data)]
            bla <- bla[bla <= 1.15 * max(gcFittedSpline$fit.data)]
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)
            df.horizontal <- data.frame("time" = c(gcFittedSpline[["raw.time"]][1], lagtime),
                                        "y" = if(gcFittedSpline$control$log.y.spline){
                                          exp(gcFittedSpline[["fit.data"]][1])*gcFittedSpline[["data.in"]][1]
                                        } else {
                                          gcFittedSpline[["fit.data"]][1]
                                        })
            # time values for tangent at mumax2
            time2_start.ndx <- which.min(abs(gcFittedSpline$fit.time-(coef$t.max2-0.15*growth.time)))
            time2_start <- gcFittedSpline$fit.time[time2_start.ndx]
            time2 <- seq(time2_start, max(gcFittedSpline$"fit.time"), length=200)
            # y values for tangent at mumax
            if(gcFittedSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu2*time2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (mu2*time2)
            }
            time2 <- time2[bla2 >= 0.6* gcFittedSpline$data.in[1]][bla2 <= 1.15 * max(gcFittedSpline$fit.data)]
            bla2 <- bla2[bla2 >= 0.6* gcFittedSpline$data.in[1]][bla2 <= 1.15 * max(gcFittedSpline$fit.data)]
            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)

            if(!gcFittedSpline$control$log.y.spline){
              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla))], y = .data$y[which.min(abs(bla))],
                                        xend = .data$time[which.min(abs(.data$y - 1.1*p.yrange.end))],
                                        yend = .data$y[which.min(abs(.data$y - 1.1*p.yrange.end))]),
                                    data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = 0.7*lwd)

              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla2))], y = .data$y[which.min(abs(bla2))],
                                        xend = .data$time[which.min(abs(.data$y - 1.1*p.yrange.end))],
                                        yend = .data$y[which.min(abs(.data$y - 1.1*p.yrange.end))]),
                                    data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = 0.7*lwd)
            }
            else {
              p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
              p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.7), linewidth = lwd)
            }

            if(!(lagtime <0)){
              p <- p + geom_segment(aes(x = .data$time[1], y = .data$y[1], xend = .data$time[2], yend = .data$y[2]), data = df.horizontal,
                                    linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
            }
          }
        } # if(gcFittedSpline$fitFlag2)
        else {
          # time values for tangent
          time <- seq(ifelse(lagtime<0, 0, lagtime), max(gcFittedSpline$"fit.time"), length=200)
          # y values for tangent
          if(gcFittedSpline$control$log.y.spline){
            bla <- (exp(coef["b.tangent"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu*time)
          } else {
            bla <- coef["b.tangent"][[1]] + (mu*time)
          }
          time <- time[bla <= 1.15 * max(df$fit.data)]
          bla <- bla[bla <= 1.15 * max(df$fit.data)]
          tangent.df <- data.frame("time" = time,
                                   "y" = bla)
          df.horizontal <- data.frame("time" = c(gcFittedSpline[["raw.time"]][1], lagtime),
                                      "y" = if(gcFittedSpline$control$log.y.spline){
                                        exp(gcFittedSpline[["fit.data"]][1])*gcFittedSpline[["data.in"]][1]
                                      } else {
                                        gcFittedSpline[["fit.data"]][1]
                                      })
          if(!gcFittedSpline$control$log.y.spline){
            p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla))], y = .data$y[which.min(abs(bla))],
                                      xend = .data$time[which.min(abs(.data$y - 1.1*p.yrange.end))],
                                      yend = .data$y[which.min(abs(.data$y - 1.1*p.yrange.end))]),
                                  data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = 0.7*lwd)
          }
          else {
            p <- p + geom_line(aes(x = .data$time, y = .data$y), data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
          }
          if(!(lagtime <0)){
            p <- p + geom_segment(aes(x = .data$time[1], y = .data$y[1], xend = .data$time[2], yend = .data$y[2]), data = df.horizontal,
                                  linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
          }
        } # else of if(gcFittedSpline$fitFlag2)
      } # if(slope == TRUE && log.y == TRUE)

      # /// add panel with growth rate over time
      if(deriv == TRUE){
        df.mu <- data.frame(spline(gcFittedSpline$spline.deriv1$x, gcFittedSpline$spline.deriv1$y))
        #add missing time values due to min.growth and t0
        df.mu <-
          dplyr::bind_rows(data.frame("x" = df$time[is.na(df$fit.data)], "y" = rep(NA, length(df$time[is.na(df$fit.data)]))),
                           df.mu)

        p.mu <- ggplot(df.mu, aes(x=.data$x, y=.data$y)) +
          geom_line(color = colSpline, linewidth = lwd) +
          theme_classic(base_size = basesize) +
          xlab(ifelse(is.null(x.title), "Time", x.title)) +
          ylab(ifelse(is.null(y.title.deriv), "Growth rate", y.title.deriv)) +
          coord_cartesian(xlim = c(0, x_limit[2]*0.95), clip = "off") +
          theme(
            panel.background = ggplot2::element_rect(fill='transparent'),
            plot.background = ggplot2::element_rect(fill='transparent', color=NA)
          )

        if(!is.null(y.lim.deriv)){
          p.mu <- p.mu + scale_y_continuous(limits = y.lim.deriv, breaks = scales::pretty_breaks(n = 6))
        } else {
          p.mu <- p.mu + scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

        }
        if(!is.null(x.lim)){
          p.mu <- p.mu + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
        } else {
          p.mu <- p.mu + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
        }

        p <- suppressWarnings(
          ggpubr::ggarrange(p, p.mu, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1))
        )
      }
    } #if(gcFittedSpline$fitFlag == TRUE)
    else {
      if(gcFittedSpline$control$log.y.spline == TRUE){
        df <- data.frame("time" = gcFittedSpline[["raw.time"]],
                         "data" = exp(gcFittedSpline[["raw.data"]])*gcFittedSpline[["data.in"]][1]
        )
      } else{
        df <- data.frame("time" = gcFittedSpline[["raw.time"]],
                         "data" = gcFittedSpline[["raw.data"]]
        )
      }


      p <- ggplot(df, aes(x=.data$time, y=.data$data)) +
        geom_point(shape=pch, size = cex.point,alpha = 0.6, stroke=0.15*cex.point, color = colData) +
        xlab(ifelse(is.null(x.title), "Time", x.title)) +
        ylab(ifelse(is.null(y.title), "Growth [y(t)]", y.title)) +
        theme_classic(base_size = basesize) +
        ggtitle(gsub(" \\| NA", "", paste(gcFittedSpline$gcID, collapse=" | "))) +
        theme(legend.key = element_blank(),
              legend.background=element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.90, 0.08),
              plot.title = element_text(size = basesize*1.1, face = "bold", vjust = 3),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = unit(c(1,2,1,1), "lines")) +
        scale_color_manual(name='Growth fit',
                           breaks = "Spline fit",
                           values=c("spline" = ggplot2::alpha(colSpline, 0.85), "Spline fit" = ggplot2::alpha(colSpline, 0.85)))


      p.yrange.end <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]

      if(!is.null(x.lim)){
        p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
      } else {
        p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      }

      x_limit <- ggplot_build(p)$layout$panel_params[[1]]$x.range
      y_limit <- ggplot_build(p)$layout$panel_params[[1]]$y.range
      y_limit[2] <- y_limit[2] * 1.25

      p <- p +
        annotate(
          "text",
          label = paste("t0:", gcFittedSpline$control$t0, "  min.growth:", gcFittedSpline$control$min.growth, "  smoothing:", gcFittedSpline$control$smooth.gc),
          x = 0.5*x_limit[2],
          y = 1.3 * y_limit[2],
          angle = 0, parse = FALSE, size = basesize*3.2/12) +
        coord_cartesian(xlim = c(0, x_limit[2]*0.95), ylim = c(y_limit[1], y_limit[2]), clip = "off")


      if(log.y == TRUE){
        if(!is.null(y.lim)){
          p <- p + scale_y_continuous(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10), trans = 'log')
        } else {
          p <- p + scale_y_continuous(breaks = scales::log_breaks(n = n.ybreaks, base = 10), trans = 'log')
        }
      } else {
        if(!is.null(y.lim)){
          p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks))
        } else {
          p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks))
        }
      }
    }

    if(export == FALSE && plot == FALSE){
      invisible(p)
    }
    if (export == TRUE){
      w <- width
      h <- height
      out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
      dir.create(out.dir, showWarnings = FALSE)
      grDevices::png(paste0(out.dir, "/", paste(gcFittedSpline$gcID, collapse = "_"), "_SplineFit.png"),
                     width = w, height = h, units = 'in', res = 300)
      suppressWarnings(print(p))
      grDevices::dev.off()
      if (requireNamespace("Cairo", quietly = TRUE)) {
        Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(gcFittedSpline$gcID, collapse = "_"), "_SplineFit.pdf"))
      } else {
        message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
        grDevices::pdf(idth = w, height = h, file = paste0(out.dir, "/", paste(gcFittedSpline$gcID, collapse = "_"), "_SplineFit.pdf"))
      }
      suppressWarnings(print(p))
      grDevices::dev.off()
    }
    if (plot == TRUE){
      suppressWarnings(print(p))
    } else{
      invisible(p)
    }
  } # else of if (add == TRUE)
}

#' Generic plot function for \code{grofit} objects. Combine different groups of samples into a single plot
#'
#' \code{plot.grofit} extracts the spline fits of a subset of samples in a \code{grofit} object calculates averages and standard deviations of conditions with replicates and combines them into a single plot.
#'
#'
#' @param x A \code{grofit} object created with \code{\link{growth.workflow}} containing spline fits.
#' @param ... (_optional_) Additional \code{grofit} objects created in separate workflows for joint plotting in a single graph.
#' @param data.type (Character) Plot either raw data (\code{data.type = "raw"}) or the spline fit results
#' @param IDs (String or vector of strings) Define samples or groups (if \code{mean = TRUE}) to combine into a single plot based on exact matches with entries in the \code{label} or \code{condition} columns of \code{grofit$expdesign}.
#' @param names (String or vector of strings) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered. Note: Ensure to use unique concentration values to extract groups of interest. If the concentration value of one condition is included in its entirety within the name of other conditions (e.g., the dataset contains '1', '10', and '100', \code{code = 10} will select both '10 and '100'), it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define groups to exclude from the plot. Partial matches with sample/group names are accepted.
#' @param exclude.conc (Numeric or numeric vector) Define concentrations to exclude from the plot.
#' @param mean (Logical) Display the mean and standard deviation of groups with replicates (\code{TRUE}) or plot each sample individually (\code{FALSE})?
#' @param log.y (Logical) Log-transform the y-axis of the plot (\code{TRUE}) or not (\code{FALSE})?
#' @param deriv (Logical) Show derivatives over time in a separate panel below the plot (\code{TRUE}) or not (\code{FALSE})?
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param colors (vector of strings) Define a color palette used to draw the plots. If \code{NULL}, default palettes are chosen based on the number of groups/samples within the plot. Note: The number of provided colors should at least match the number of groups/samples.
#' @param color_groups (Logical) Shall samples within the same group but with different concentrations be shown in different shades of the same color?
#' @param group_pals (String vector) Define the colors used to display sample groups with identical concentrations. The number of selected color palettes must be at least the number of displayed groups. The order of the chosen palettes corresponds to the oder of conditions in the legend. Available options: "Green", "Oranges", "Purple", "Cyan", "Grey", "Red", "Blue", and "Magenta".
#' @param basesize (Numeric) Base font size.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the growth curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the x-axis of both growth curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim.deriv (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis of the derivative plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.title (Character) Optional: Provide a title for the y-axis of the growth curve plot.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both growth curve and derivative plots.
#' @param y.title.deriv (Character) Optional: Provide a title for the y-axis of the derivative plot.
#' @param lwd (Numeric) Line width of the individual plots.
#' @param legend.position (Character) Position of the legend. One of "bottom", "top", "left", "right".
#' @param legend.ncol (Numeric) Number of columns in the legend.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#'
#' @export plot.grofit
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggtitle labs guides
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab xlim ylim
#'
#' @return A plot with all growth curves (raw measurements or nonparametric fits) in a dataset, with replicates combined by the group averages (if \code{mean = TRUE}) or not (\code{mean = FALSE}).
#'
#' @examples
#' # Create random growth data set
#' rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#' rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = "Test2")
#'
#' rnd.data <- list()
#' rnd.data[["time"]] <- rbind(rnd.data1$time, rnd.data2$time)
#' rnd.data[["data"]] <- rbind(rnd.data1$data, rnd.data2$data)
#'
#' # Run growth curve analysis workflow
#' res <- growth.workflow(time = rnd.data$time,
#'                        data = rnd.data$data,
#'                        fit.opt = "s",
#'                        ec50 = FALSE,
#'                        export.res = FALSE,
#'                        suppress.messages = TRUE,
#'                        parallelize = FALSE)
#'
#'
#' plot(res, names = "Test1", legend.ncol = 4) # Show only samples for condition "Test1"
#'
plot.grofit <- function(x, ...,
                        data.type = c("spline", "raw"),
                        IDs = NULL,
                        names = NULL,
                        conc = NULL,
                        exclude.nm = NULL,
                        exclude.conc = NULL,
                        mean = TRUE,
                        log.y = TRUE,
                        deriv = TRUE,
                        n.ybreaks = 6,
                        colors = NULL,
                        color_groups = TRUE,
                        group_pals = c('Green', 'Orange', 'Purple', 'Magenta', 'Grey', 'Blue', 'Grey', 'Red', 'Cyan', 'Brown', 'Mint'),
                        basesize = 20,
                        y.lim = NULL,
                        x.lim = NULL,
                        y.title = NULL,
                        x.title = NULL,
                        y.lim.deriv = NULL,
                        y.title.deriv = NULL,
                        lwd = 1.1,
                        legend.position = "bottom",
                        legend.ncol = 2,
                        plot = TRUE,
                        export = FALSE,
                        height = NULL,
                        width = NULL,
                        out.dir = NULL,
                        out.nm = NULL
)
{
  grofit <- x
  if(!is.null(colors))
    colors <- toupper(colors)
  # Convert range  and selecting arguments
  names <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", names)), pattern = ";"))
  conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", conc)), pattern = "[;,]"))
  exclude.nm <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.nm)), pattern = ";"))
  exclude.conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.conc)), pattern = ";"))
  suppressWarnings(assign("x.lim" ,as.numeric(x.lim)))
  if(all(is.na(x.lim))) x.lim <- NULL
  suppressWarnings(assign("y.lim" ,as.numeric(y.lim)))
  if(all(is.na(y.lim))) y.lim <- NULL
  suppressWarnings(assign("y.lim.deriv" ,as.numeric(y.lim.deriv)))
  if(all(is.na(y.lim.deriv))) y.lim.deriv <- NULL
  # Change data.type for 'grodata' object
  if(is(x) %in% "grodata")
    data.type <- "raw"

  call <- match.call()
  # remove all function arguments from call to leave only multiple grofit objects
  call$export <- call$plot <- call$out.nm <- call$out.dir <- call$width <- call$height <- call$lwd <- call$y.title.deriv <- call$IDs <- call$legend.ncol <- call$color_groups <-
    call$y.lim.deriv <- call$x.title <- call$y.title <- call$x.lim <- call$y.lim <- call$basesize <- call$colors <- call$n.ybreaks <- call$deriv <- call$group_pals <-
    call$log.y <- call$mean  <- call$conc  <- call$names  <- call$data.type <- call$exclude.conc <- call$exclude.nm <- call$legend.position <- NULL

  arglist <- lapply(call[-1], function(x) x)
  var.names <- vapply(arglist, deparse, character(1))
  arglist <- lapply(arglist, eval.parent, n = 2)
  names(arglist) <- var.names
  if(length(arglist) > 1){
    # combine several grofit objects for joint plotting
    lapply(arglist, function(x) {
      if(methods::is(x) != "grofit" &&  methods::is(x) != "grodata") stop("Input objects need to be of class 'grofit' created with growth.workflow().")
    })
    merged <- grofit
    for(i in 2:length(arglist)){
      merged$time <- as.matrix(rbind.fill(as.data.frame(merged$time), as.data.frame(arglist[[i]]$time)))
      merged$data <- rbind.fill(merged$data, arglist[[i]]$data)
      merged$gcFit$gcFittedSplines <- c(merged$gcFit$gcFittedSplines, arglist[[i]]$gcFit$gcFittedSplines)
    }
    grofit <- merged
  }
  data.type <- match.arg(data.type)
  if(data.type == "raw" && deriv ==TRUE){
    warning("Derivatives cannot be calculated for 'raw' data. Only the growth values will be shown.")
    deriv = FALSE
  }

  # grofit an object of class grofit
  if(methods::is(grofit) != "grofit" && methods::is(grofit) != "grodata") stop("Input objects needs to be an object created with growth.workflow().")
  # /// check input parameters

  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(lwd)==FALSE)   stop("Need numeric value for: lwd")
  if(data.type == "spline"){
    if (!("s" %in% grofit$control$fit.opt | "a" %in% grofit$control$fit.opt)) stop("To plot spline fit results, please run growth.workflow() with 'a' or 's' in fit.opt.")
  }

  conc <- as.numeric(conc)
  exclude.conc <- as.numeric(exclude.conc)

  # Get name of conditions with multiple replicates; apply selecting arguments
  sample.nm <- nm <- if(methods::is(grofit) == "grofit"){
    as.character(names(grofit$gcFit$gcFittedSplines))} else {
      as.character(grofit$expdesign$label)
    }
  if(!is.null(IDs)){
    # Check if IDs refer to samples or conditions
    if(any(grep(" \\| ", IDs))){
      nm <- nm[
        grepl(x = nm,
              pattern = paste0("^", paste(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", IDs), collapse="$|^"), "$"))
        ]
    } else {
      nm <- nm[
        grepl(x = gsub(" \\| .+", "", nm),
              pattern = paste0("^", paste(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", IDs), collapse="$|^"), "$"))
      ]
    }
  }
  else {
    if(!is.null(names)  && length(names) > 0){
      if(!is.na(names) && names != ""){
        names <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", names)
        nm <- nm[grep(paste(names, collapse="|"), nm)]
      }
    }
    if(!is.null(exclude.nm)  && length(exclude.nm) > 0){
      if(!is.na(exclude.nm) && exclude.nm != ""){
        names.excl <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", exclude.nm)
        nm <- nm[!grepl(paste(names.excl, collapse="|"), gsub(" \\|.+", "", nm))]
      }
    }
  }
  if(!is.null(conc) && length(conc) > 0){
    if(!all(is.na(conc))) nm <- nm[which(str_extract(nm, "[:graph:]+$") %in% conc)]
  }
  if(!is.null(exclude.conc)  && length(exclude.conc) > 0){
    if(!all(is.na(exclude.conc))) nm <- nm[-which(str_extract(nm, "[:graph:]+$") %in% exclude.conc)]
  }
  if(length(nm)==0){
    stop("Please run plot.grofit() with valid 'names' or 'conc' argument.")
  }
  # remove conditions with fitFlag = FALSE in all replicates
  # Store each condition with its replicate indices in list filter.ls
  ndx.filt.rep <- unique(lapply(1:length(sample.nm), function(i) which(gsub(" \\| .+ \\| ", "_", sample.nm) %in% (paste0(unlist(str_split(sample.nm[i], " \\| "))[1], "_", unlist(str_split(sample.nm[i], " \\| "))[3])))))
  if(mean==TRUE){
    #keep only replicate indices if condition defined in nm
    # get indices of samples with selected names
    ndx.keep <- grep(paste0(
      gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm), collapse = "|"), sample.nm)
    ndx.filt.rep <- ndx.filt.rep[unlist(lapply(1:length(ndx.filt.rep), function(i) all(ndx.filt.rep[[i]] %in% ndx.keep)))]
  }
  filter.ls <- list()
  for(j in 1:length(ndx.filt.rep)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.rep[[j]]), function(i) ndx.filt.rep[[j]][grep(paste0("^",
                                                                                                           gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[1]),
                                                                                                                ".+[[:space:]]",
                                                                                                                unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                                "$"), sample.nm[ndx.filt.rep[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = FALSE)
  ndx.filt <- ndx.filt[lapply(ndx.filt,length)>0]
  # Check FitFlag for each replicate, work per condition
  if(data.type == "spline"){
    for(i in 1:length(ndx.filt)){
      if(!all(unlist(lapply(1:length(ndx.filt[[i]]), function(j) (grofit[["gcFit"]][["gcFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]]))))){
        fitflags <- unlist(lapply(1:length(ndx.filt[[i]]), function(j) (grofit[["gcFit"]][["gcFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]])))
        nm <- nm[!(nm %in% sample.nm[(ndx.filt[[i]][!fitflags])])]
      }
    }
  }

  # get indices of samples with selected names
  ndx.keep <- grep(paste0("^",
                          gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm), "$", collapse = "|"), sample.nm)

  if(data.type == "spline"){
    # correct for log transformation
    if(grofit$control$log.y.spline == TRUE){
      for(i in 1:length(ndx.keep)){
        grofit$gcFit$gcFittedSplines[[ndx.keep[i]]][["fit.data"]] <-
          exp(grofit$gcFit$gcFittedSplines[[ndx.keep[i]]][["fit.data"]]) * grofit$gcFit$gcFittedSplines[[ndx.keep[i]]]$data.in[1]
      }
    }
  }

  if(mean == TRUE){
    # Combine replicates via their mean and standard deviation
    conditions <- str_replace_all(nm, "\\| .+ \\| ", "| ")
    conditions_unique <- unique(conditions)

    # Create lists for each selected condition, with growth values and derivatives, respectively. Each list item represents one condition with their average and SD
    plotdata.ls <- list()
    deriv.ls <- list()
    for(n in 1:length(conditions_unique)){
      # find indexes of replicates
      ndx <- intersect(ndx.keep, grep(paste0("^",
                                             gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", unlist(str_split(conditions_unique[n], " \\| "))[1]),
                                             " \\|.+[[:space:]]",
                                             unlist(str_split(conditions_unique[n], " \\| "))[2],
                                             "$"), sample.nm))

      name <- conditions_unique[n]
      # Create lists for growth and time values for each sample
      if(data.type == "spline"){
        time <- as.list(lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$fit.time)))
        data <- as.list(lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$fit.data)))
      } else {
        time <- as.list(lapply(1:length(ndx), function(i) cbind(grofit$time[ndx[[i]], ])))
        if(methods::is(grofit) == "grofit"){
          data <- grofit$data[ndx, 4:ncol(grofit$data)]
        } else {
          data <- grofit$growth[ndx, 4:ncol(grofit$growth)]
        }
        data <- split(as.matrix(data), 1:nrow(as.matrix(data)))
        data <- lapply(1:length(data), function(i) as.numeric(data[[i]]))
      }

      # Create lists for derivatives and time values for each sample
      if(deriv){
        time.deriv <- as.list(lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$spline.deriv1$x)))
        data.deriv <- as.list(lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$spline.deriv1$y)))
      }
      # correct for unequal lengths of data series
      time.all <- Reduce(union, time)
      for(i in 1:length(time)){
        assign(paste0("time.missing_", i), setdiff(time.all, time[[i]]) )
        if(length(get(paste0("time.missing_", i))) > 0){
          for(j in 1:length(get(paste0("time.missing_", i)))){
            # extract growth values into a separate list
            data[[i]] <- append(data[[i]],
                                values = NA,
                                after = match(get(paste0("time.missing_", i))[j],
                                              time.all) - 1)
            # extract time values into a separate list
            time[[i]] <-
              append(time[[i]],
                     values = get(paste0("time.missing_", i))[j],
                     after = match(get(paste0("time.missing_", i))[j], time.all) - 1)
          }
        }
      }
      if(deriv){
        # correct for unequal lengths of derivative series and harmonize the time values.
        time.all <- Reduce(union, time.deriv)
        for(i in 1:length(time.deriv)){
          assign(paste0("time.missing_", i), setdiff(time.all, time.deriv[[i]]) )
          if(length(get(paste0("time.missing_", i))) > 0){
            for(j in 1:length(get(paste0("time.missing_", i)))){
              data.deriv[[i]] <- append(data.deriv[[i]],
                                        values = NA,
                                        after = match(get(paste0("time.missing_", i))[j],
                                                      time.all) - 1)
              time.deriv[[i]] <-
                append(time.deriv[[i]],
                       values = get(paste0("time.missing_", i))[j],
                       after = match(get(paste0("time.missing_", i))[j], time.all) - 1)
            }
          }
        }
      }
      time <- time[[1]]
      data <- do.call("cbind", data)
      avg <- rowMeans(data, na.rm = FALSE)
      sd <- apply(data, 1, sd, na.rm = FALSE)
      plotdata.ls[[n]] <- data.frame("name" = name, "time" = time, "mean" = avg, "upper" = avg+sd, "lower" = avg-sd)
      if(deriv){
        time.deriv <- time.deriv[[1]]
        data.deriv <- do.call("cbind", data.deriv)
        avg.deriv <- rowMeans(data.deriv, na.rm = FALSE)
        sd.deriv <- apply(data.deriv, 1, sd, na.rm = FALSE)
        deriv.ls[[n]] <- data.frame("name" = name, "time" = time.deriv, "mean" = avg.deriv, "upper" = avg.deriv+sd.deriv, "lower" = avg.deriv-sd.deriv)
      }
    }
    names(plotdata.ls) <- gsub(" \\| NA", "", conditions_unique)
    if(deriv){
      names(deriv.ls) <- gsub(" \\| NA", "", conditions_unique)
      deriv.ls <- deriv.ls[!is.na(deriv.ls)]
      df.deriv <- do.call(rbind.data.frame, deriv.ls)
      df.deriv$name <- gsub(" \\| NA", "", df.deriv$name)

      df.deriv$concentration <- as.numeric(gsub(".+ \\| ", "", df.deriv$name))
      df.deriv$group <- gsub(" \\| .+", "", df.deriv$name)

      # sort names
      df.deriv <- df.deriv[order(df.deriv$group, df.deriv$concentration), ]

      df.deriv$name <- factor(df.deriv$name, levels = unique(factor(df.deriv$name)))
    }

    plotdata.ls <- plotdata.ls[!is.na(plotdata.ls)]
    df <- do.call(rbind.data.frame, plotdata.ls)
    df$name <- gsub(" \\| NA", "", df$name)
    df$concentration <- as.numeric(gsub(".+ \\| ", "", df$name))
    df$group <- gsub(" \\| .+", "", df$name)

    # replace negative lower ribbon boundaries with 0 for log10 transformation
    if(log.y==TRUE){
      df$lower[df$lower<0] <- 0
    }

    # sort names
    df <- df[order(df$group, df$concentration), ]

    df$name <- factor(df$name, levels = unique(factor(df$name)))

    p <- ggplot(df, aes(x=.data$time, y=.data$mean, col = .data$name)) +
      geom_line(linewidth=lwd) +
      geom_ribbon(aes(ymin=.data$lower, ymax=.data$upper, fill=.data$name), alpha = 0.3, colour = NA) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title), "Time", x.title)) +
      ylab(ifelse(is.null(y.title), "Growth [y(t)]", y.title)) +
      theme(legend.position=legend.position,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol=legend.ncol)) +
      ggplot2::guides(colour=ggplot2::guide_legend(ncol=legend.ncol))


    if(log.y == TRUE){
      if(!is.null(y.lim)){
        p <- p + scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10))
      } else {
        p <- p + scale_y_log10(breaks = scales::log_breaks(n = n.ybreaks, base = 10))
      }
    } else {
      if(!is.null(y.lim)){
        p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    }

    if(!is.null(x.lim)){
      p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
    } else {
      p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }

    conditions <- str_replace_all(df$name, "\\| .+ \\| ", "| ")
    conditions_unique <- unique(conditions)

    groups <- str_replace_all(conditions_unique, " \\| .+", "")
    groups_unique <- unique(groups)
    if(is.null(colors) && color_groups == TRUE && length(group_pals) < length(groups_unique)){
      message("Fewer colors in 'group_pals' provided than the number of visualized groups. Grouped coloring was disabled (color_groups set to FALSE).")
      try(showModal(modalDialog("Fewer colors in 'group_pals' provided than the number of visualized groups. Grouped coloring was disabled.", easyClose = TRUE, footer=NULL)), silent = TRUE)
      color_groups <- FALSE
    }
    # apply color schemes
    if(is.null(colors)){
      if(color_groups && length(unique(df$concentration)) > 2){
        colors <- c()

        for(i in 1:length(groups_unique)){
          colors <- c(colors, grDevices::colorRampPalette(single_hue_palettes[group_pals][[i]])(length(which(groups %in% groups_unique[i]))))
        }

        p <- p + scale_fill_manual(name = "Condition",
                                   values = colors) +
          scale_color_manual(name = "Condition",
                            values = colors)
      } else {
        if (length(plotdata.ls) <= 8) {
          p <- p + scale_fill_brewer(name = "Condition", palette = "Dark2") + scale_color_brewer(name = "Condition", palette = "Dark2")
        } else if (length(plotdata.ls) <= 12) {
          p <- p + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
        } else if (length(plotdata.ls) <= 50){
          p <- p + scale_fill_manual(name = "Condition",
                                     values = big_palette
          ) + scale_color_manual(name = "Condition",
                                 values = big_palette
          )
        }
      }
    } else if (length(colors) < length(unique(df$name))){
      if (length(plotdata.ls) <= 8) {
        p <- p + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))])) +
          scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))]))
      } else if (length(plotdata.ls) <= 12) {
        p <- p + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))])) +
          scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))]))
      } else if (length(plotdata.ls) <= 50){
        p <- p + scale_fill_manual(name = "Condition",
                                   values = c(colors, big_palette[-(1:length(colors))])) +
          scale_color_manual(name = "Condition",
                             values = c(colors, big_palette[-(1:length(colors))])
          )
      }
    } else {
      p <- p + scale_fill_manual(name = "Condition",
                                 values = colors) +
        scale_color_manual(name = "Condition",
                           values = colors)
    }
    if(deriv){
      # /// add panel with growth rate over time
      p.deriv <- ggplot(df.deriv, aes(x=.data$time, y=.data$mean, col = .data$name)) +
        geom_line(linewidth=lwd) +
        geom_ribbon(aes(ymin=.data$lower,ymax=.data$upper, fill=.data$name), alpha = 0.3, colour = NA) +
        theme_classic(base_size = basesize) +
        xlab(ifelse(is.null(x.title), "Time", x.title)) +
        theme(legend.position=legend.position,
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      if(is.null(y.title.deriv)){
        p.deriv <- p.deriv + ylab(label = "Growth rate")
      } else {
        p.deriv <- p.deriv + ylab(label = y.title.deriv)
      }


      if(!is.null(y.lim.deriv)){
        p.deriv <- p.deriv + scale_y_continuous(limits = y.lim.deriv, breaks = scales::pretty_breaks(n = 6, bounds = FALSE))
      } else {
        p.deriv <- p.deriv + scale_y_continuous(breaks = scales::pretty_breaks(n = 6, bounds = FALSE))
      }

      if(!is.null(x.lim)){
        p.deriv <- p.deriv + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
      } else {
        p.deriv <- p.deriv + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      }

      if(is.null(colors)){
        if(color_groups &&  length(unique(df$concentration)) > 2){
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                     values = colors) +
            scale_color_manual(name = "Condition",
                              values = colors)
        }
        else{
          if (length(plotdata.ls) <= 8) {
            p.deriv <- p.deriv + scale_fill_brewer(name = "Condition", palette = "Dark2") + scale_color_brewer(name = "Condition", palette = "Dark2")
          } else if (length(plotdata.ls) <= 12) {
            p.deriv <- p.deriv + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
          } else if (length(plotdata.ls) <= 50){
            p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                                   values = big_palette
            ) + scale_color_manual(name = "Condition",
                                   values = big_palette
            )
          }
        }
      } else if (length(colors) < length(unique(df$name))){
        if (length(plotdata.ls) <= 8) {
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))])) +
            scale_color_manual(name = "Condition", values = c(colors, values = RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))]))
        } else if (length(plotdata.ls) <= 12) {
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))])) +
            scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))]))
        } else if (length(plotdata.ls) <= 50){
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                     values = c(colors, big_palette[-(1:length(colors))])) +
            scale_color_manual(name = "Condition",
                               values = c(colors, big_palette[-(1:length(colors))])
            )
        }
      } else {
        p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                               values = colors) +
          scale_color_manual(name = "Condition",
                             values = colors)
      }
      p <- suppressWarnings(
        ggpubr::ggarrange(p, p.deriv, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1), common.legend = TRUE, legend = legend.position, legend.grob = ggpubr::get_legend(p, position = "right"))
      )
    }
  } # if(mean == TRUE)
  else {
    df <- data.frame()
    for(i in 1:length(ndx.keep)){
      if(data.type == "spline"){
        df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                              "time" = grofit$gcFit$gcFittedSplines[[ndx.keep[i]]][["fit.time"]],
                                              "y" = grofit$gcFit$gcFittedSplines[[ndx.keep[i]]][["fit.data"]]))
      } else {
        df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                              "time" = as.vector(grofit$time[ndx.keep[i], ]),
                                              "y" = unlist(unname(utils::type.convert(grofit$data[ndx.keep[i], 4:ncol(grofit$data)], as.is=T)))))
      }

    }
    p <- ggplot(df, aes(x=.data$time, y=.data$y, col = .data$name)) +
      geom_line(linewidth=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title), "Time", x.title)) +
      ylab(ifelse(is.null(y.title), "Growth [y(t)]", y.title)) +
      theme(legend.position=legend.position,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol=legend.ncol))+
      ggplot2::guides(colour=ggplot2::guide_legend(ncol=legend.ncol))


    if(log.y == TRUE){
      if(!is.null(y.lim)){
        p <- p + scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10))
      } else {
        p <- p + scale_y_log10(breaks = scales::log_breaks(n = n.ybreaks, base = 10))
      }
    } else {
      if(!is.null(y.lim)){
        p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    }

    if(!is.null(x.lim)){
      p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
    } else {
      p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }

    if(is.null(colors)){
      if (length(ndx.keep) <= 8) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Dark2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else if (length(ndx.keep) <= 12) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
      } else if (length(ndx.keep) <= 50){
        p <- p + scale_fill_manual(name = "Condition",
                                   values = big_palette) +
          scale_color_manual(name = "Condition",
                               values = big_palette
        )
      }
    } else if (length(colors) < length(unique(df$name))){
      if (length(ndx.keep) <= 8) {
        p <- p + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))])) +
          scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))]))
      } else if (length(ndx.keep) <= 12) {
        p <- p + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))])) +
          scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))]))
      } else if (length(ndx.keep) <= 50){
        p <- p + scale_fill_manual(name = "Condition",
                                   values = c(colors, big_palette[-(1:length(colors))])) +
          scale_color_manual(name = "Condition",
                             values = c(colors, big_palette[-(1:length(colors))])
          )
      }
    } else {
      p <- p + scale_fill_manual(name = "Condition",
                                             values = colors) +
        scale_color_manual(name = "Condition",
                           values = colors)
    }
    if(deriv){
      df.deriv <- data.frame()
      for(i in 1:length(ndx.keep)){
        df.deriv <- plyr::rbind.fill(df.deriv, data.frame("name" = sample.nm[ndx.keep[i]],
                                              "time" = grofit$gcFit$gcFittedSplines[[ndx.keep[[i]]]]$spline.deriv1$x,
                                              "y" = grofit$gcFit$gcFittedSplines[[ndx.keep[[i]]]]$spline.deriv1$y))
      }
      p.deriv <- ggplot(df.deriv, aes(x=.data$time, y=.data$y, col = .data$name)) +
        geom_line(linewidth=lwd) +
        theme_classic(base_size = basesize) +
        xlab(ifelse(is.null(x.title), "Time", x.title)) +
        theme(legend.position=legend.position,
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      if(is.null(y.title.deriv)){
        p.deriv <- p.deriv + ylab(label = "Growth rate")
      } else {
        p.deriv <- p.deriv + ylab(label = y.title.deriv)
      }

      if(!is.null(y.lim)){
        p.deriv <- p.deriv + scale_y_continuous(limits = y.lim.deriv, breaks = scales::pretty_breaks(n = 10, bounds = FALSE))
      } else {
        p.deriv <- p.deriv + scale_y_continuous(breaks = scales::pretty_breaks(n = 10, bounds = FALSE))
      }

      if(!is.null(x.lim)){
        p.deriv <- p.deriv + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
      } else {
        p.deriv <- p.deriv + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      }

      if(is.null(colors)){
        if (length(ndx.keep) <= 8) {
          p.deriv <- p.deriv + scale_fill_brewer(name = "Condition", palette = "Dark2") + scale_color_brewer(name = "Condition", palette = "Dark2")
        } else if (length(ndx.keep) <= 12) {
          p.deriv <- p.deriv + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
        } else if (length(ndx.keep) <= 50){
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                     values = big_palette
          ) + scale_color_manual(name = "Condition",
                                 values = big_palette
          )
        }
      } else if (length(colors) < length(unique(df$name))){
        if (length(ndx.keep) <= 8) {
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))])) +
            scale_color_manual(name = "Condition", values = c(colors, values = RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))]))
        } else if (length(ndx.keep) <= 12) {
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))])) +
            scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))]))
        } else if (length(ndx.keep) <= 50){
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                                 values = c(colors, big_palette[-(1:length(colors))])) +
            scale_color_manual(name = "Condition",
                               values = c(colors, big_palette[-(1:length(colors))])
            )
        }
      } else {
        p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                               values = colors) +
          scale_color_manual(name = "Condition",
                             values = colors)
      }
      p <- suppressWarnings(
        ggpubr::ggarrange(p, p.deriv, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1), common.legend = TRUE, legend = legend.position, legend.grob = ggpubr::get_legend(p))
      )
    } # if(deriv)

  }
  if(export == FALSE && plot == FALSE){
    return(p)
  }
  out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
  if (export == TRUE){
    if(is.null(out.nm)) out.nm <- paste0("GroupPlot")
    if(is.null(width)){
      w <- 10 + 3*ifelse(mean==TRUE,length(conditions_unique), length(nm))/15
    } else {
      w <- width
    }
    if(is.null(height)){
      h <- ifelse(deriv==T, 9, 6)
    } else {
      h <- height
    }
    dir.create(out.dir, showWarnings = FALSE)
    grDevices::png(paste0(out.dir, "/", out.nm, ".png"),
                   width = w, height = h, units = 'in', res = 300)
    suppressWarnings(print(p))
    grDevices::dev.off()
    if (requireNamespace("Cairo", quietly = TRUE)) {
      Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
    } else {
      message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
      grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
    }
    suppressWarnings(print(p))
    grDevices::dev.off()
  }
  if (plot == TRUE){
    suppressWarnings(print(p))
  } else {
    return(p)
  }
}

base_breaks <- function(n = 10){
  function(x) {
    grDevices::axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

#' Generic plot function for \code{grodata} objects. Plots raw growth, fluorescence, or normalized fluorescence data of multiple samples or conditions.
#'
#' \code{plot.grodata} calls \code{\link{plot.grofit}} or \code{\link{plot.flFitRes}} based on the chosen \code{data.type}, respectively.
#'
#'
#' @param x A \code{grodata} object created with \code{\link{read_data}} or \code{\link{parse_data}}.
#' @param data.type (Character) Plot either raw growth (\code{data.type = "growth"}), raw fluorescence (\code{data.type = "fl"}), or fluorescence normalized to growth (\code{data.type = "norm.fl"}).
#' @param IDs (String or vector of strings) Define samples or groups (if \code{mean = TRUE}) to combine into a single plot based on exact matches with entries in the \code{label} or \code{condition} columns of \code{grofit$expdesign}.
#' @param names (String or vector of strings) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered. Note: Ensure to use unique concentration values to extract groups of interest. If the concentration value of one condition is included in its entirety within the name of other conditions (e.g., the dataset contains '1', '10', and '100', \code{code = 10} will select both '10 and '100'), it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define groups to exclude from the plot. Partial matches with sample/group names are accepted.
#' @param exclude.conc (Numeric or numeric vector) Define concentrations to exclude from the plot.
#' @param mean (Logical) Display the mean and standard deviation of groups with replicates (\code{TRUE}) or plot each sample individually (\code{FALSE})?
#' @param log.y (Logical) Log-transform the y-axis of the plot (\code{TRUE}) or not (\code{FALSE})?
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param colors (vector of strings) Define a color palette used to draw the plots. If \code{NULL}, default palettes are chosen based on the number of groups/samples within the plot. Note: The number of provided colors should at least match the number of groups/samples.
#' @param color_groups (Logical) Shall samples within the same group but with different concentrations be shown in different shades of the same color?
#' @param group_pals (String vector) Define the colors used to display sample groups with identical concentrations. The number of selected color palettes must be at least the number of displayed groups. The order of the chosen palettes corresponds to the oder of conditions in the legend. Available options: "Green", "Oranges", "Purple", "Cyan", "Grey", "Red", "Blue", and "Magenta".
#' @param basesize (Numeric) Base font size.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the growth curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the x-axis of both growth curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.title (Character) Optional: Provide a title for the y-axis of the growth curve plot.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both growth curve and derivative plots.
#' @param lwd (Numeric) Line width of the individual plots.
#' @param legend.position (Character) Position of the legend. One of "bottom", "top", "left", "right".
#' @param legend.ncol (Numeric) Number of columns in the legend.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @export plot.grodata
#' @export
#'
#' @return A plot with all growth curves (raw measurements) in a dataset, with replicates combined by the group averages (if \code{mean = TRUE}) or not (\code{mean = FALSE}).
#'
#' @examples
#' # Create random growth data sets
#' rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#' rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = "Test2")
#'
#' # Create dataframe with both data sets and a single time vector
#' time <- as.data.frame(matrix(t(c("Time",NA,NA, rnd.data1$time[1,])),nrow=1),
#'              stringsAsFactors=FALSE)
#' colnames(time) <- colnames(rnd.data1$data)
#' data <- rbind(time, rnd.data1$data, rnd.data2$data)
#'
#'
#' # Create a grodata object
#' grodata <- read_data(data.growth = data, data.format = "row")
#'
#' plot(grodata, exclude.nm = "Test1", legend.ncol = 4)
#'
plot.grodata <- function(x,
                         data.type = c("growth", "fl", "norm.fl"),
                         IDs = NULL,
                         names = NULL,
                         conc = NULL,
                         mean = TRUE,
                         exclude.nm = NULL,
                         exclude.conc = NULL,
                         log.y = FALSE,
                         n.ybreaks = 6,
                         colors = NULL,
                         color_groups = TRUE,
                         group_pals = c('Green', 'Orange', 'Purple', 'Magenta', 'Grey', 'Blue', 'Grey', 'Red', 'Cyan', 'Brown', 'Mint'),
                         basesize = 20,
                         y.lim = NULL,
                         x.lim = NULL,
                         y.title = NULL,
                         x.title = NULL,
                         lwd = 1.1,
                         legend.position = "bottom",
                         legend.ncol = 2,
                         plot = TRUE,
                         export = FALSE,
                         height = NULL,
                         width = NULL,
                         out.dir = NULL,
                         out.nm = NULL,
                         ...)
{
  data.type <- match.arg(data.type)
  if(!(is(x) %in% "grodata")) stop("x needs to be a grodata object created with read_data() or parse_data().")
  if(!(any(data.type %in% c("growth", "fl", "norm.fl")))) stop("data.type needs to be either a 'dens' (growth), 'fl' (fluorescence), or 'norm.fl' (fluorescence normalized by growth).")
  if(data.type == "fl" && length(x$fluorescence)<2) stop("x does not contain fluorescence data.")
  if(data.type == "norm.fl" && length(x$fluorescence)<2) stop("x does not contain norm. fluorescence data. Did you read both growth and fluorescence data when calling read_data() or parse_data()?")

  if(data.type == "fl" || data.type == "norm.fl")
    plot.flFitRes(x,
                  data.type = if(data.type == "fl"){
                    "raw"
                  } else {
                    "norm.fl"
                  },
                  IDs = IDs,
                  names = names,
                  conc = conc,
                  mean = mean,
                  exclude.nm = exclude.nm,
                  exclude.conc = exclude.conc,
                  log.y = log.y,
                  deriv = FALSE,
                  n.ybreaks = n.ybreaks,
                  colors = colors,
                  color_groups = color_groups,
                  basesize = basesize,
                  y.lim = y.lim,
                  x.lim = x.lim,
                  y.title = y.title,
                  x.title = x.title,
                  y.lim.deriv = NULL,
                  y.title.deriv = NULL,
                  lwd = lwd,
                  legend.position = legend.position,
                  legend.ncol = legend.ncol,
                  plot = plot,
                  export = export,
                  height = height,
                  width = width,
                  out.dir = out.dir,
                  out.nm = out.nm
    )
  else
    plot.grofit(x,
                data.type = "raw",
                IDs = IDs,
                names = names,
                conc = conc,
                mean = mean,
                exclude.nm = exclude.nm,
                exclude.conc = exclude.conc,
                log.y = log.y,
                deriv = FALSE,
                n.ybreaks = n.ybreaks,
                colors = colors,
                color_groups = color_groups,
                basesize = basesize,
                y.lim = y.lim,
                x.lim = x.lim,
                y.title = y.title,
                x.title = x.title,
                y.lim.deriv = NULL,
                y.title.deriv = NULL,
                lwd = lwd,
                legend.position = legend.position,
                legend.ncol = legend.ncol,
                plot = plot,
                export = export,
                height = height,
                width = width,
                out.dir = out.dir,
                out.nm = out.nm
    )
}

#' Compare growth parameters between samples or conditions
#'
#' \code{plot.parameter} gathers physiological parameters from the results of a growth fit analysis and compares a chosen parameter between each sample or condition in a column plot. Error bars represent the 95% confidence interval (only shown for > 2 replicates).
#'
#' @param x A \code{grofit}, \code{gcFit}, or \code{gcTable} object obtained with \code{\link{growth.workflow}} or \code{\link{growth.gcFit}}.
#' @param param (Character) The parameter used to compare different sample groups. Any name of a column containing numeric values in \code{gcTable} (which is stored within \code{grofit} or \code{gcFit} objects) can be used as input. Useful options are:
#' 'mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit',
#' 'mu.model', 'lambda.model', 'A.model',
#' 'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline',
#' 'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt'
#' @param IDs (String or vector of strings) Define samples or groups (if \code{mean = TRUE}) to combine into a single plot based on exact matches with entries in the \code{label} or \code{condition} columns of \code{grofit$expdesign}.
#' @param names (String or vector of strings) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered. Note: Ensure to use unique concentration values to extract groups of interest. If the concentration value of one condition is included in its entirety within the name of other conditions (e.g., the dataset contains '1', '10', and '100', \code{code = 10} will select both '10 and '100'), it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define groups to exclude from the plot. Partial matches with sample/group names are accepted.
#' @param exclude.conc (Numeric or numeric vector) Define concentrations to exclude from the plot.
#' @param reference.nm (Character) Name of the reference condition, to which parameter values are normalized. Partially matching strings are tolerated as long as they can uniquely identify the condition.
#' @param reference.conc (Numeric) Concentration of the reference condition, to which parameter values are normalized.
#' @param order_by_conc (Logical) Shall the columns be sorted in order of ascending concentrations (\code{TRUE}) or by sample groups \code{FALSE}?
#' @param colors (vector of strings) Define a color palette used to draw the columns. If \code{NULL}, default palettes are chosen. Note: The number of provided colors should at least match the number of groups.
#' @param basesize (Numeric) Base font size.
#' @param label.size (Numeric) Font size for sample labels below x-axis.
#' @param shape.size (Numeric) The size of the symbols indicating replicate values. Default: 2.5
#' @param legend.position (Character) Position of the legend. One of "bottom", "top", "left", "right".
#' @param legend.ncol (Numeric) Number of columns in the legend.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @export plot.parameter
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggtitle labs guides
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab geom_hline geom_col
#'
#' @return A column plot comparing a selected growth parameter between tested conditions.
#'
#' @examples
#' # Create random growth data set
#' rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#' rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = "Test2")
#'
#' rnd.data <- list()
#' rnd.data[["time"]] <- rbind(rnd.data1$time, rnd.data2$time)
#' rnd.data[["data"]] <- rbind(rnd.data1$data, rnd.data2$data)
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
#'
#' plot.parameter(res,
#'                param = "mu.spline",
#'                legend.ncol = 4,
#'                legend.position = "bottom",
#'                basesize = 15,
#'                label.size = 11)
#'
plot.parameter <- function(x, param = c('mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit', 'mu2.linfit', 'lambda2.linfit',
                                             'mu.model', 'lambda.model', 'A.model', "A.orig.model", "dY.model", "dY.orig.model", "tD.linfit", "tD2.linfit", "tD.spline", "tD2.spline",
                                             'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu2.spline', 'lambda2.spline',
                                             'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt',
                                             'max_slope.linfit', 'max_slope.spline'),
                           IDs = NULL,
                           names = NULL,
                           conc = NULL,
                           exclude.nm = NULL,
                           exclude.conc = NULL,
                           reference.nm = NULL,
                           reference.conc = NULL,
                           order_by_conc = FALSE,
                           colors = NULL,
                           basesize = 12,
                           label.size = NULL,
                           shape.size = 2.5,
                           legend.position = "right",
                           legend.ncol = 1,
                           plot = TRUE,
                           export = FALSE,
                           height = 7,
                           width = NULL,
                           out.dir = NULL,
                           out.nm = NULL,
                           ...)
  {
  object <- x
  if(!is.null(colors))
    colSpline <- toupper(colors)
  # Convert range  and selecting arguments
  names <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", names)), pattern = ";"))
  conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", conc)), pattern = "[;,]"))
  exclude.nm <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.nm)), pattern = ";"))
  exclude.conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.conc)), pattern = ";"))


  param <- match.arg(param)
  # check class of object
  if(!(any(methods::is(object) %in% c("gcTable", "grofit", "gcFit", "flTable", "flFitRes", "flFit")))) stop("object needs to be either a 'grofit', 'gcTable', 'gcFit', 'flTable', 'flFit', or 'flFitRes' object created with growth.workflow(), growth.gcFit(), fl.workflow(), or flFit().")
  if(!is.character(param) || !(param %in% c('mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit', 'mu2.linfit', 'lambda2.linfit',
                                            'mu.model', 'lambda.model', 'A.model', "A.orig.model", "dY.model", "dY.orig.model", "tD.linfit", "tD2.linfit", "tD.spline", "tD2.spline",
                                            'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu2.spline', 'lambda2.spline',
                                            'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt',
                                            'max_slope.linfit', 'max_slope.spline')))
                                            stop("param needs to be a character string and one of:\n 'mu.linfit', 'lambda.linfit', 'mu2.linfit', 'lambda2.linfit', 'dY.linfit', 'A.linfit', 'mu.model', 'lambda.model', 'A.model', 'A.orig.model', 'dY.model', 'dY.orig.model',  'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt', 'max_slope.linfit', 'max_slope.spline'.")

  #extract gcTable
  if(any(methods::is(object) %in% "gcTable")){
    gcTable <- object
  } else if (methods::is(object)=="gcFit"){
    gcTable <- object$gcTable
  } else if (methods::is(object)=="grofit"){
    gcTable <- object$gcFit$gcTable
  } else if (methods::is(object)=="flFitRes"){
    gcTable <- object$flFit$flTable
  } else if (any(methods::is(object) %in% "gcTable")){
    gcTable <- object
  } else if (methods::is(object)=="flFit"){
    gcTable <- object$flTable
  }
  #check if param exists in gcTable and has a valid value
  if(all(is.na(gcTable[[param]])) || all(gcTable[[param]] == 0)){
    if(gsub(".+\\.", "", param)=="linfit") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow() with 'fit.opt' containing 'l' or 'a', or growth.gcFit() with a control object with 'fit.opt' containing 'l' or 'a'."))
    if(gsub(".+\\.", "", param)=="model") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow() with 'fit.opt' containing 'm' or 'a', or growth.gcFit() with a control object with 'fit.opt' containing 'm' or 'a'."))
    if(gsub(".+\\.", "", param)=="spline") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow() with 'fit.opt' containing 's' or 'a', or growth.gcFit() with a control object with 'fit.opt' containing 's' or 'a'."))
  }

  conc <- as.numeric(conc)
  exclude.conc <- as.numeric(exclude.conc)

  # Get name of conditions with multiple replicates
  sample.nm <- nm <- as.character(paste(gcTable[,1], gcTable[,2], gcTable[,3], sep = " | "))

  if(!is.null(IDs)){
    # Check if IDs refer to samples or conditions
    if(any(grep(" \\| ", IDs))){
      nm <- nm[
        grepl(x = nm,
              pattern = paste0("^", paste(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", IDs), collapse="$|^"), "$"))
      ]
    } else {
      nm <- nm[
        grepl(x = gsub(" \\| .+", "", nm),
              pattern = paste0("^", paste(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", IDs), collapse="$|^"), "$"))
      ]
    }
  }
  else {
    if(!is.null(names)  && length(names) > 0){
      if(!is.na(names) && names != ""){
        names <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", names)
        nm <- nm[grep(paste(names, collapse="|"), nm)]
      }
    }
    if(!is.null(exclude.nm)  && length(exclude.nm) > 0){
      if(!is.na(exclude.nm) && exclude.nm != ""){
        names.excl <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", exclude.nm)
        nm <- nm[!grepl(paste(names.excl, collapse="|"), gsub(" \\|.+", "", nm))]
      }
    }
  }

  if(!is.null(conc) && length(conc) > 0){
    if(!all(is.na(conc))) nm <- nm[which(str_extract(nm, "[:graph:]+$") %in% conc)]
  }

  if(!is.null(exclude.conc)  && length(exclude.conc) > 0){
    if(!all(is.na(exclude.conc))) nm <- nm[-which(str_extract(nm, "[:graph:]+$") %in% exclude.conc)]
  }
  if(length(nm)==0){
    stop("Please run plot.parameters() with valid 'names' or 'conc' argument.")
  }
  gcTable <- gcTable[match(nm, paste(gcTable[,1], gcTable[,2], gcTable[,3], sep = " | ")), ]
  # get indices of replicates
  # remove conditions with fitFlag = FALSE in all replicates
    # Store each condition with its replicate indices in list filter.ls
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
  if(gsub(".+\\.", "", param)=="linfit") fit.type <- "linfit"
  if(gsub(".+\\.", "", param)=="model") fit.type <- "model"
  if(gsub(".+\\.", "", param)=="spline") fit.type <- "spline"

  # Check FitFlag for each replicate, work per condition
  for(i in 1:length(ndx.filt)){
    if(!all(unlist(lapply(1:length(ndx.filt[[i]]), function(j) (as.logical(gcTable[j, ifelse(fit.type=="linfit", "reliable_fit.linfit", ifelse(fit.type=="model", "reliable_fit.model", "reliable_fit.spline"))])))))){
      fitflags <- unlist(lapply(1:length(ndx.filt[[i]]), function(j) (as.logical(gcTable[ndx.filt[[i]][j], ifelse(fit.type=="linfit", "reliable_fit.linfit", ifelse(fit.type=="model", "reliable_fit.model", "reliable_fit.spline"))]))))
      for(j in 1:length(fitflags)){
        if(!is.na(gcTable[ndx.filt[[i]][j], param])){
          fitflags[j] <- TRUE
        }
      }
      nm <- nm[!(nm %in% sample.nm[(ndx.filt[[i]][!fitflags])])]
      ndx.filt[[i]] <- ndx.filt[[i]][fitflags]
    }
  }

  # calculate average param values
  mean <- unlist(lapply(1:length(ndx.filt), function (x) mean(as.numeric(gcTable[ndx.filt[[x]], param]), na.rm = TRUE)) ) # mean
  sd <- unlist(lapply(1:length(ndx.filt), function (x) sd(as.numeric(gcTable[ndx.filt[[x]], param]), na.rm = TRUE)) ) #standard deviation
  n <- unlist(lapply(1:length(ndx.filt), function (x) length(ndx.filt[[x]])) ) # number of replicates per condition
  labels <- gsub(" \\| NA", "", gsub(" \\| [[:digit:]]+ \\| ", " | ", names(ndx.filt))) # condition names

  # apply normalization to reference condition
  if(!is.null(reference.nm) && reference.nm != ""){
    ref.ndx <- grep( gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", reference.nm), labels)
    if (length(ref.ndx) > 1){
      if(!is.null(reference.conc)){
        refconc.ndx <- which(reference.conc == as.numeric(str_extract(labels, "[:graph:]+$")))
        ref.ndx <- intersect(refconc.ndx, ref.ndx)
      } else {
        ref.ndx <- ref.ndx[1]
      }
    }
    if(length(ref.ndx) > 1){
      message("The provided combination of reference.nm = '", reference.nm, "' and reference.conc = ", reference.conc, " did not allow for the unique identification of a reference condition. The first match will be returned.")
      ref.ndx <- ref.ndx[1]
    }
    mean.ref <- mean[ref.ndx]
    mean <- mean/mean.ref
    sd <- sd/mean.ref
  }
  sd[n < 3] <- NA
  error <- stats::qnorm(0.975) * sd / sqrt(n) # standard error
  CI.L <- mean - error #left confidence interval
  CI.R <- mean + error #right confidence interval

  df <- data.frame("name" = labels, "mean" = mean, "CI.L" = CI.L, "CI.R" = CI.R)
  df$name <- factor(df$name, levels = df$name)
  df$group <- gsub(" \\|.+", "", df$name)
  df$concentration <- as.numeric(gsub(".+\\| ", "", df$name))
  if (order_by_conc){
    df <- df[order(df$concentration, df$group), ]
  } else {
    df <- df[order(df$group, df$concentration), ]
  }

  df$name <- factor(df$name, levels = unique(factor(df$name)))

  df$mean[is.na(df$mean)] <- 0
  df$CI.L[is.na(df$CI.L)] <- 0
  df$CI.R[is.na(df$CI.R)] <- 0

  if(is.null(label.size) || label.size == "") label.size <-  12-length(unique(df$name))^(1/3)

  p <- ggplot(df, aes(x=.data$name, y=.data$mean, fill = .data$group)) +
    geom_bar(stat="identity", color = "black") +
    geom_errorbar(aes(ymin = .data$CI.L, ymax = .data$CI.R), width = 0.2) +
    ggplot2::labs(x = "Condition", y = paste0(param, " (\u00B1 95% CI)")) +
    theme_minimal(base_size = basesize) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = label.size),
          legend.position=legend.position,
          plot.margin = unit(c(1, 1, 1, nchar(as.character(df$name)[1])/6), "lines"),
          # remove the vertical grid lines
          panel.grid.major.x = element_blank() ,
          # explicitly set the horizontal lines (or they will disappear too)
          ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
    ggplot2::guides(fill=ggplot2::guide_legend(ncol=legend.ncol))

  if(!is.null(colors)){
    colors <- toupper(colors)
    if(length(unique(df$group)) > length(colors)){
      p <- p + scale_fill_manual(name = "Condition",
                                 values = c(colors, scales::hue_pal()(length(unique(df$group)))[-(1:length(colors))])
      )
    }
    else {
      p <- p + scale_fill_manual(name = "Condition",
                                 values = colors)
    }
  }

  replicates <- unlist(
    lapply(
      1:length(ndx.filt), function(x) if(length(ndx.filt[[x]])>0){ seq(1, length(ndx.filt[[x]]))}
    )
  )
  df_reps <-
    tibble::rownames_to_column(
      data.frame("condition" = unlist(lapply(1:length(ndx.filt), function(x) rep(labels[x], length(ndx.filt[[x]])))),
                 "replicate" = replicates,
                 "value" = unlist(lapply(1:length(ndx.filt), function (x) as.numeric(gcTable[ndx.filt[[x]], param])))
      )
    )

  df_reps$condition <- as.factor(df_reps$condition)
  df_reps$replicate <- as.factor(df_reps$replicate)

  df_reps$group <- gsub(" \\|.+", "", df_reps$condition)
  df_reps$concentration <- as.numeric(gsub(".+\\| ", "", df_reps$condition))
  if (order_by_conc){
    df_reps <- df_reps[order(df_reps$concentration, df_reps$group), ]
  } else {
    df_reps <- df_reps[order(df_reps$group, df_reps$concentration), ]
  }

  df_reps$condition <- factor(df_reps$condition, levels = unique(factor(df_reps$condition)))
  # apply normalization to reference condition
  if(!is.null(reference.nm)){
    df_reps$value <- df_reps$value/mean.ref
  }

  if(as.numeric(max(replicates))<=8){
    pal <- RColorBrewer::brewer.pal(n=as.numeric(max(replicates)), name="Greys")
  } else{
    pal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n=8, name="Greys"))(as.numeric(max(replicates)))
  }
   p <- p +
     ggnewscale::new_scale_fill() +
     geom_point(
       data = df_reps,
       aes(x = .data$condition, y = .data$value, fill = .data$replicate),
       shape = 23,
       size = shape.size,
       color = "black",
       position = position_dodge(width = 0.3),
       inherit.aes = FALSE
     ) +
     scale_fill_manual(values = pal)




  if(export == FALSE && plot == FALSE){
    return(p)
  }
  if (export == TRUE){
    if(is.null(out.nm)) out.nm <- paste0("ParameterPlot_", param)
    w <- ifelse(is.null(width), 7 + (3*length(nm))/20, width)
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = FALSE)
    grDevices::png(paste0(out.dir, "/", out.nm, ".png"),
                   width = w, height = h, units = 'in', res = 300)
    print(p)
    grDevices::dev.off()
    if (requireNamespace("Cairo", quietly = TRUE)) {
      Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
    } else {
      message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
      grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
    }
    print(p)
    grDevices::dev.off()
  }
  if (plot == TRUE){
    print(p)
  } else {
    return(p)
  }
}

#' Compare calculated dose-response parameters between conditions.
#'
#' \code{plot.dr_parameter} gathers parameters from the results of a dose-response analysis and compares a chosen parameter between each condition in a column plot. Error bars represent the 95% confidence interval (only shown for > 2 replicates).
#'
#' @param x A \code{grofit}, \code{drFit}, \code{drTable}, or \code{flFitRes} object obtained with \code{\link{growth.workflow}}, \code{\link{growth.drFit}}, \code{\link{fl.drFit}}, or \code{\link{fl.workflow}}.
#' @param param (Character) The parameter used to compare different sample groups. Any name of a column containing numeric values in \code{gcTable} (which is stored within \code{grofit} or \code{gcFit} objects) can be used as input. Useful options are:
#' 'y.max', 'y.min', 'fc', 'K', or 'n' for fluorescence dose-response analyses with \code{dr.type = 'model'} in the \code{control} argument,
#' or 'EC50', 'yEC50', 'drboot.meanEC50', 'drboot.meanEC50y'.
#' @param names (String or vector of strings) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define groups to exclude from the plot. Partial matches with sample/group names are accepted.
#' @param basesize (Numeric) Base font size.
#' @param reference.nm (Character) Name of the reference condition, to which parameter values are normalized. Partially matching strings are tolerated as long as they can uniquely identify the condition.
#' @param label.size (Numeric) Font size for sample labels below x-axis.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @export plot.dr_parameter
#' @export
#'
#' @return A column plot comparing a selected parameter of a dose-response analysis between tested conditions.
#'
#' @examples
#' \donttest{
#' # Create random growth data set
#' rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#' rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = "Test2")
#'
#' rnd.data <- list()
#' rnd.data[["time"]] <- rbind(rnd.data1$time, rnd.data2$time)
#' rnd.data[["data"]] <- rbind(rnd.data1$data, rnd.data2$data)
#'
#' # Run growth curve analysis workflow
#' gcFit <- growth.gcFit(time = rnd.data$time,
#'                        data = rnd.data$data,
#'                        parallelize = FALSE,
#'                        control = growth.control(fit.opt = "s",
#'                                                 suppress.messages = TRUE))
#'
#' # Perform dose-response analysis
#' drFit <- growth.drFit(gcTable = gcFit$gcTable,
#'             control = growth.control(dr.parameter = "mu.spline"))
#'
#' plot.dr_parameter(drFit, param = 'EC50')
#' }
#'
plot.dr_parameter <- function(x, param = c('EC50', 'EC50.Estimate', 'y.max', 'y.min', 'fc', 'K', 'n', 'yEC50', 'drboot.meanEC50', 'drboot.meanEC50y', 'EC50.orig', 'yEC50.orig'),
                              names = NULL, exclude.nm = NULL, basesize = 12, reference.nm = NULL, label.size = NULL,
                              plot = TRUE, export = FALSE, height = 7, width = NULL, out.dir = NULL, out.nm = NULL, ...)
{
  object <- x
  if(x$control$dr.method != "model.MM")
    param <- match.arg(param)
  names <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", names)), pattern = ";"))
  exclude.nm <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.nm)), pattern = ";"))
  # check class of object
  if(!(any(methods::is(object) %in% c("drTable", "grofit", "drFit", "flFitRes")))) stop("object needs to be either a 'grofit', 'drTable', 'drFit', or 'flFitRes' object created with growth.workflow(), growth.drFit(), fl.workflow(), or growth.drFit().")
  if(!is.character(param) || !(param %in% c('y.max', 'EC50.Estimate', 'y.min', 'fc', 'K', 'n', 'EC50', 'yEC50', 'drboot.meanEC50', 'drboot.meanEC50y', 'Km', 'Vmax', 'EC50.Estimate', 'EC50.orig', 'yEC50.orig')))
    stop("param needs to be a character string and one of:\n 'y.max', 'y.min', 'fc', 'K', 'n', 'yEC50', 'EC50.orig', or 'yEC50.orig' (for fluorescence fits), or \n 'yEC50', 'EC50', 'EC50.Estimate', 'drboot.meanEC50', 'drboot.meanEC50y', 'EC50.orig', or 'yEC50.orig' (for growth fits).")
  #extract drTable
  if(any(methods::is(object) %in% "drTable")){
    drTable <- as.data.frame(object)
  } else if (methods::is(object)=="drFit"){
    drTable <- as.data.frame(object$drTable)
  } else if (methods::is(object)=="grofit"){
    drTable <- as.data.frame(object$drFit$drTable)
  } else if (methods::is(object)=="flFitRes"){
    drTable <- as.data.frame(object$drFit$drTable)
  } else if (any(methods::is(object) %in% "drTable")){
    drTable <- as.data.frame(object)
  }
  if(param == "EC50" && "EC50.Estimate" %in% colnames(drTable))
    param <- "EC50.Estimate"

  if(!(param %in% colnames(drTable))) stop(paste0(param, " is not a suitable dr parameter for the performed dose-response analysis. Options 'y.max', 'y.min', 'fc', 'K', or 'n' are only available for drFitfl or flFitRes objects created with dr.method = 'model' in the fl.control object."))
  #check if param exists in drTable and has a valid value
  if(all(is.na(drTable[[param]]))){
    if(gsub(".+\\.", "", param)=="linfit") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow()/fl.workflow() with 'fit.opt' containing 'l' or 'a', or growth.drFit()/fl.drFit() with a control object with 'fit.opt' containing 'l' or 'a'."))
    if(gsub(".+\\.", "", param)=="model") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow()/fl.workflow() with 'fit.opt' containing 'm' or 'a', or growth.drFit()/fl.drFit() with a control object with 'fit.opt' containing 'm' or 'a'."))
    if(gsub(".+\\.", "", param)=="spline") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow()/fl.workflow() with 'fit.opt' containing 's' or 'a', or growth.drFit()/fl.drFit() with a control object with 'fit.opt' containing 's' or 'a'."))
  }
  # Get name of conditions with multiple replicates
  sample.nm <- nm <- as.character(drTable[,1])
  if(!is.null(names)){
    names <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", names)
    nm <- nm[grep(paste(names, collapse="|"), nm)]
  }
  if(!is.null(exclude.nm)  && length(exclude.nm) > 0){
    if(!is.na(exclude.nm) && exclude.nm != ""){
      names.excl <- gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", exclude.nm))
      nm <- nm[!grepl(paste(names.excl, collapse="|"), gsub(" \\|.+", "", nm))]
    }
  }
  if(length(nm)==0){
    stop("Please run plot.parameters() with valid 'names' or 'conc' argument.")
  }
  drTable <- drTable[match(nm, drTable[,1]), ]
  values <- drTable[, param]
  if(param == "EC50.Estimate"){
    upper <- drTable[, "EC50.Upper"]
    lower <- drTable[, "EC50.Lower"]
  }
  # apply normalization to reference condition
  if(!is.null(reference.nm)){
    ref.ndx <- match( gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", reference.nm), nm)
    if(length(ref.ndx) > 1){
      message("The provided combination of reference.nm = '", reference.nm, " did not allow for the unique identification of a reference condition. The first match will be returned.")
      ref.ndx <- ref.ndx[1]
    }
    value.ref <- values[ref.ndx]
    values <- values/value.ref
    if(param == "EC50.Estimate"){
      upper <- upper/value.ref
      lower <- lower/value.ref
    }
  }
  if(param == "EC50.Estimate"){
    df <- data.frame("name" = nm, "values" = values,
                     "upper" = upper,
                     "lower" = lower)
  } else {
    df <- data.frame("name" = nm, "values" = values)
  }

  df$name <- factor(df$name, levels = df$name)
  df$values[is.na(df$values)] <- 0

  if(is.null(label.size) || label.size == "") label.size <-  12-length(unique(df$name))^(1/3)

  p <- ggplot(df, aes(x=.data$name, y=.data$values)) +
    geom_bar(stat="identity", color = "black") +
    ggplot2::labs(x = "Condition", y = param) +
    theme_minimal(base_size = basesize) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = label.size),
          plot.margin = unit(c(1, 1, 1, nchar(as.character(df$name)[1])/6), "lines"),
          panel.grid.major.x = element_blank() , # explicitly remove the horizontal lines
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

  if(param == "EC50.Estimate"){
    p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
      ggplot2::labs(x = "Condition", y = "EC50 (\u00B1 95% CI)")
  }
  if(export == FALSE && plot == FALSE){
    return(p)
  }
  if (export == TRUE){
      if(is.null(out.nm)) out.nm <- paste0("drParameterPlot_", param)
      w <- ifelse(is.null(width), 7 + (3*length(nm))/20, width)
      h <- height
      out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
      dir.create(out.dir, showWarnings = FALSE)
      grDevices::png(paste0(out.dir, "/", out.nm, ".png"),
                     width = w, height = h, units = 'in', res = 300)
      print(p)
      grDevices::dev.off()
      if (requireNamespace("Cairo", quietly = TRUE)) {
        Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
      } else {
        message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
        grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
      }
      print(p)
      grDevices::dev.off()
  }
  if (plot == TRUE){
    print(p)
  } else {
    return(p)
  }
}

#' Plot a matrix of growth curve panels
#'
#' \code{plot.grid} takes a \code{grofit} or \code{flFitRes} object and returns a facet grid of individual growth and fluorescence plots
#'
#' @param x A \code{grofit} or \code{flFitRes} object created with \code{\link{growth.workflow}} or code{\link{fl.workflow}} containing spline fits.
#' @param data.type (Character) Plot either raw data (\code{data.type = "raw"}) or the spline fit results
#' @param param (Character) The parameter used to compare different sample groups. Any name of a column containing numeric values in \code{gcTable} (which is stored within \code{grofit} or \code{gcFit} objects) can be used as input. Useful options are:
#' 'mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit',
#' 'mu.model', 'lambda.model', 'A.model',
#' 'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline',
#' 'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt'
#' @param pal (Character string) Choose one of 'Green',   'Orange',  'Purple',  'Magenta', 'Grey', 'Blue', 'Grey', 'Red', 'Cyan', 'Brown', or 'Mint' to visualize the value of the parameter chosen as \code{param} for each sample or condition.
#' @param invert.pal (Logical) Shall the colors in the chosen \code{pal} be inverted (\code{TRUE}) or not \code{FALSE}?
#' @param IDs (String or vector of strings) Define samples or groups (if \code{mean = TRUE}) to combine into a single plot based on exact matches with entries in the \code{label} or \code{condition} columns of \code{grofit$expdesign}. The order of strings within the vector defines the order of samples within the grid.
#' @param sort_by_ID (Logical) Shall samples/conditions be ordered as entered in \code{IDs} (\code{TRUE}) or alphabetically (\code{FALSE})?
#' @param names (String or vector of strings) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered. Note: Ensure to use unique concentration values to extract groups of interest. If the concentration value of one condition is included in its entirety within the name of other conditions (e.g., the dataset contains '1', '10', and '100', \code{code = 10} will select both '10 and '100'), it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define groups to exclude from the plot. Partial matches with sample/group names are accepted.
#' @param exclude.conc (Numeric or numeric vector) Define concentrations to exclude from the plot.
#' @param mean (Logical) Display the mean and standard deviation of groups with replicates (\code{TRUE}) or plot each sample individually (\code{FALSE})?
#' @param log.y (Logical) Log-transform the y-axis of the plot (\code{TRUE}) or not (\code{FALSE})?#'
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param sort_by_conc (Logical) Shall the samples/conditions be sorted with concentrations in rows and groups in columns?
#' @param nrow (Numeric) Defines the number of rows in the grid if \code{sort_by_conc} is \code{FALSE}.
#' @param basesize (Numeric) Base font size.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the growth curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the x-axis of both growth curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param legend.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the color scale applied to \code{param} as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.title (Character) Optional: Provide a title for the y-axis of the growth curve plot.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both growth curve and derivative plots.
#' @param lwd (Numeric) Line width of the individual plots.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @export plot.grid
#' @export
#'
#' @return A plot matrix with all growth curves (raw measurements or nonparametric fits) in a dataset, with replicates combined by the group averages (if \code{mean = TRUE}) or not (\code{mean = FALSE}).
#'
#' @examples
#' # Create random growth data set
#' rnd.data1 <- rdm.data(d = 35, mu = 0.8, A = 5, label = "Test1")
#' rnd.data2 <- rdm.data(d = 35, mu = 0.6, A = 4.5, label = "Test2")
#'
#' rnd.data <- list()
#' rnd.data[["time"]] <- rbind(rnd.data1$time, rnd.data2$time)
#' rnd.data[["data"]] <- rbind(rnd.data1$data, rnd.data2$data)
#'
#' # Run growth curve analysis workflow
#' res <- growth.workflow(time = rnd.data$time,
#'                        data = rnd.data$data,
#'                        fit.opt = "s",
#'                        ec50 = FALSE,
#'                        export.res = FALSE,
#'                        suppress.messages = TRUE,
#'                        parallelize = FALSE)
#'
#'
#' plot.grid(res, param = "mu.spline")
#'
plot.grid <- function(x,
                      data.type = c("spline", "raw", "norm.fl"),
                      param = c('mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit', 'mu2.linfit', 'lambda2.linfit',
                                'mu.model', 'lambda.model', 'A.model', "A.orig.model", "dY.model", "dY.orig.model", "tD.linfit", "tD2.linfit", "tD.spline", "tD2.spline",
                                'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu2.spline', 'lambda2.spline',
                                'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt',
                                'max_slope.linfit', 'max_slope.spline'),
                      pal = c("Green",   "Orange",  "Purple",  "Magenta", "Grey", "Blue", "Grey", "Red", "Cyan", "Brown", "Mint"),
                      invert.pal = FALSE,
                      IDs = NULL,
                      sort_by_ID = FALSE,
                      names = NULL,
                      conc = NULL,
                      exclude.nm = NULL,
                      exclude.conc = NULL,
                      mean = TRUE,
                      log.y = TRUE,
                      n.ybreaks = 6,
                      sort_by_conc = TRUE,
                      nrow = NULL,
                      basesize = 20,
                      y.lim = NULL,
                      x.lim = NULL,
                      legend.lim = NULL,
                      y.title = NULL,
                      x.title = NULL,
                      lwd = 1.1,
                      plot = TRUE,
                      export = FALSE,
                      height = NULL,
                      width = NULL,
                      out.dir = NULL,
                      out.nm = NULL,
                      ...
)
{
  object <- x
  pal <- match.arg(pal)
  # Convert range  and selecting arguments
  names <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", names)), pattern = ";"))
  conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", conc)), pattern = "[;,]"))
  exclude.nm <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.nm)), pattern = ";"))
  exclude.conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.conc)), pattern = ";"))
  param <- match.arg(param)
  data.type <- match.arg(data.type)
  suppressWarnings(assign("x.lim" ,as.numeric(x.lim)))
  if(all(is.na(x.lim))) x.lim <- NULL
  suppressWarnings(assign("y.lim" ,as.numeric(y.lim)))
  if(all(is.na(y.lim))) y.lim <- NULL
  suppressWarnings(assign("legend.lim" ,as.numeric(legend.lim)))
  if(all(is.na(legend.lim))) legend.lim <- NULL

  if(data.type == "norm.fl" && !any("flFitRes" %in% methods::is(object)))
    stop("data.type 'norm.fl' is only compatible with an object created with fl.workflow().")

  if(!is.character(param) || !(param %in% c('mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit', 'mu2.linfit', 'lambda2.linfit',
                                            'mu.model', 'lambda.model', 'A.model', "A.orig.model", "dY.model", "dY.orig.model", "tD.linfit", "tD2.linfit", "tD.spline", "tD2.spline",
                                            'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu2.spline', 'lambda2.spline',
                                            'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt',
                                            'max_slope.linfit', 'max_slope.spline')))
    stop("param needs to be a character string and one of:\n 'mu.linfit', 'lambda.linfit', 'mu2.linfit', 'lambda2.linfit', 'dY.linfit', 'A.linfit', 'mu.model', 'lambda.model', 'A.model', 'A.orig.model', 'dY.model', 'dY.orig.model',  'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt', 'max_slope.linfit', 'max_slope.spline'.")

  #extract gcTable
  if (methods::is(x)=="grofit"){
    gcTable <- x$gcFit$gcTable
  } else if (methods::is(x)=="flFitRes"){
    gcTable <- x$flFit$flTable
  }

  #check if param exists in gcTable and has a valid value
  if(all(is.na(gcTable[[param]])) || all(gcTable[[param]] == 0)){
    if(gsub(".+\\.", "", param)=="linfit") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow() with 'fit.opt' containing 'l' or 'a', or growth.gcFit() with a control object with 'fit.opt' containing 'l' or 'a'."))
    if(gsub(".+\\.", "", param)=="model") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow() with 'fit.opt' containing 'm' or 'a', or growth.gcFit() with a control object with 'fit.opt' containing 'm' or 'a'."))
    if(gsub(".+\\.", "", param)=="spline") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow() with 'fit.opt' containing 's' or 'a', or growth.gcFit() with a control object with 'fit.opt' containing 's' or 'a'."))
  }

  # object an object of class object
  if(methods::is(object) != "grofit" && !any("flFitRes" %in% methods::is(object) )) stop("plot.grid: x needs to be an object created with growth.workflow() or fl.workflow().")
  # /// check input parameters

  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(lwd)==FALSE)   stop("Need numeric value for: lwd")
  if(data.type == "spline"){
    if (!("s" %in% object$control$fit.opt | "a" %in% object$control$fit.opt)) stop("plot.grid: To plot spline fit results, please run growth.workflow() or fl.workflow() with 'a' or 's' in fit.opt.")
  }

  conc <- as.numeric(conc)
  exclude.conc <- as.numeric(exclude.conc)

  # Get name of conditions with multiple replicates; apply selecting arguments
  sample.nm <- nm <- if(methods::is(object) == "grofit"){
      as.character(names(object$gcFit$gcFittedSplines))
    } else {
      as.character(names(object$flFit$flFittedSplines))
    }

  if(!is.null(IDs)){
    # Check if IDs refer to samples or conditions

    if(sort_by_ID){
      # preserve order of input IDs
      nm <- nm[unlist(lapply(1:length(IDs), function(x) which(gsub(" | .+", "", nm) %in% IDs[[x]] ) ) )]
    } else {
      # preserve order in expdesign
      if(any(grep(" \\| ", IDs))){
        nm <- nm[
          grepl(x = nm,
                pattern = paste0("^", paste(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", IDs), collapse="$|^"), "$"))
        ]
      } else {
        nm <- nm[
          grepl(x = gsub(" \\| .+", "", nm),
                pattern = paste0("^", paste(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", IDs), collapse="$|^"), "$"))
        ]
      }
    }
  }
  else {
    if(!is.null(names)  && length(names) > 0){
      if(!is.na(names) && names != ""){
        names <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", names)
        nm <- nm[grep(paste(names, collapse="|"), gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm))]
      }
    }
    if(!is.null(exclude.nm)  && length(exclude.nm) > 0){
      if(!is.na(exclude.nm) && exclude.nm != ""){
        names.excl <- gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", exclude.nm))
        nm <- nm[!grepl(paste(names.excl, collapse="|"), gsub(" \\|.+", "", nm))]
      }
    }
  }
  if(!is.null(conc) && length(conc) > 0){
    if(!all(is.na(conc))) nm <- nm[which(str_extract(nm, "[:graph:]+$") %in% conc)]
  }
  if(!is.null(exclude.conc)  && length(exclude.conc) > 0){
    if(!all(is.na(exclude.conc))) nm <- nm[-which(str_extract(nm, "[:graph:]+$") %in% exclude.conc)]
  }
  if(length(nm)==0){
    stop("Please run plot.grid() with valid 'IDs', names' or 'conc' argument.")
  }
  # remove conditions with fitFlag = FALSE in all replicates
  # Store each condition with its replicate indices in list filter.ls
  ndx.filt.rep <-
    unique(lapply(1:length(sample.nm), function(i)
      which(
        gsub(" \\| .+ \\| ", "_", sample.nm) %in% (paste0(
          unlist(str_split(sample.nm[i], " \\| "))[1], "_", unlist(str_split(sample.nm[i], " \\| "))[3]
        ))
      )))

  if(mean==TRUE){
    #keep only replicate indices if condition defined in nm
    # get indices of samples with selected names
    ndx.keep <- grep(paste0(
      gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm), collapse = "|"), sample.nm)
    ndx.filt.rep <- ndx.filt.rep[unlist(lapply(1:length(ndx.filt.rep), function(i) all(ndx.filt.rep[[i]] %in% ndx.keep)))]
  }
  filter.ls <- list()
  for(j in 1:length(ndx.filt.rep)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.rep[[j]]), function(i) ndx.filt.rep[[j]][grep(paste0("^",
                                                                                                           gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[1]),
                                                                                                           ".+[[:space:]]",
                                                                                                           unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                           "$"), sample.nm[ndx.filt.rep[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = FALSE)
  ndx.filt <- ndx.filt[lapply(ndx.filt,length)>0]
  # Check FitFlag for each replicate, work per condition
  if(data.type == "spline") {
    for (i in 1:length(ndx.filt)) {
      if(methods::is(object) == "grofit"){
        if (!all(unlist(lapply(1:length(ndx.filt[[i]]), function(j)
          (object[["gcFit"]][["gcFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]])))))
        {
          fitflags <-
            unlist(lapply(1:length(ndx.filt[[i]]), function(j)
              (object[["gcFit"]][["gcFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]])))
          nm <- nm[!(nm %in% sample.nm[(ndx.filt[[i]][!fitflags])])]
        }
      } else {
        if (!all(unlist(lapply(1:length(ndx.filt[[i]]), function(j)
          (object[["flFit"]][["flFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]])))))
        {
          fitflags <-
            unlist(lapply(1:length(ndx.filt[[i]]), function(j)
              (object[["flFit"]][["flFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]])))
          nm <- nm[!(nm %in% sample.nm[(ndx.filt[[i]][!fitflags])])]
        }
      }
    }
  }

  # get indices of samples with selected names
  if(length(IDs)>1 && sort_by_ID){
    ndx.keep <- unlist(lapply(1:length(nm), function(x) which(sample.nm %in% nm[x])))
  } else {
  ndx.keep <- grep(paste0("^",
                          gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm), "$", collapse = "|"), sample.nm)
  }

  if(data.type == "spline"){
    # correct for log transformation
    if(object$control$log.y.spline == TRUE){
      for(i in 1:length(ndx.keep)){
        if(methods::is(object) == "grofit"){
        object$gcFit$gcFittedSplines[[ndx.keep[i]]][["fit.data"]] <-
          exp(object$gcFit$gcFittedSplines[[ndx.keep[i]]][["fit.data"]]) * object$gcFit$gcFittedSplines[[ndx.keep[i]]]$data.in[1]
        } else {
          object$flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]] <-
            exp(object$flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]]) * object$flFit$flFittedSplines[[ndx.keep[i]]]$fl.in[1]
        }
      }
    }
  }

  xlab.title <- if (data.type == "spline" && methods::is(object) == "flFitRes" && object$control$x_type == "growth")
  {
    "Growth"
  } else {
    "Time"
  }

  ylab.title <- if(methods::is(object) == "flFitRes" && data.type == "norm.fl"){
    "Normalized fluorescence"
  } else if(methods::is(object) == "flFitRes" && data.type == "raw"){
    "Fluorescence"
  } else if(methods::is(object) == "flFitRes" && data.type == "spline" && object$control$x_type == "time" && object$control$norm_fl){
    "Normalized fluorescence"
  } else if(methods::is(object) == "flFitRes" && data.type == "spline" && object$control$x_type == "time" && !flFit$control$norm_fl){
    "Fluorescence"
  } else if(methods::is(object) == "flFitRes" && data.type == "spline" && object$control$x_type == "growth"){
    "Fluorescence"
  } else {
    "Growth [y(t)]"
  }

  if(mean == TRUE){
    # Combine replicates via their mean and standard deviation
    conditions <- str_replace_all(nm, "\\| .+ \\| ", "| ")
    conditions_unique <- unique(conditions)

    # Create lists for each selected condition, with growth values and derivatives, respectively. Each list item represents one condition with their average and SD
    plotdata.ls <- list()
    deriv.ls <- list()
    for(n in 1:length(conditions_unique)){
      # find indexes of replicates
      ndx <- intersect(ndx.keep, grep(paste0("^",
                                             gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", unlist(str_split(conditions_unique[n], " \\| "))[1]),
                                             " \\|.+[[:space:]]",
                                             unlist(str_split(conditions_unique[n], " \\| "))[2],
                                             "$"), sample.nm))

      name <- conditions_unique[n]
      # Create lists for growth, time, and param values for each sample
      if(data.type == "spline"){
        if(methods::is(object) == "grofit"){
          time <- as.list(lapply(1:length(ndx), function(i) cbind(object$gcFit$gcFittedSplines[[ndx[[i]]]]$fit.time)))
          data <- as.list(lapply(1:length(ndx), function(i) cbind(object$gcFit$gcFittedSplines[[ndx[[i]]]]$fit.data)))
          parameter <- as.list(lapply(1:length(ndx), function(i) cbind(as.numeric(rep(gcTable[ndx[[i]], param], length(data[[i]]))))))
        } else {
          time <- as.list(lapply(1:length(ndx), function(i) cbind(object$flFit$flFittedSplines[[ndx[[i]]]]$fit.x)))
          data <- as.list(lapply(1:length(ndx), function(i) cbind(object$flFit$flFittedSplines[[ndx[[i]]]]$fit.fl)))
          parameter <- as.list(lapply(1:length(ndx), function(i) cbind(as.numeric(rep(gcTable[ndx[[i]], param], length(data[[i]]))))))
        }
      } else {
        time <- as.list(lapply(1:length(ndx), function(i) cbind(object$time[ndx[[i]], ])))
        if(methods::is(object) == "grofit"){
          data <- object$data[ndx, 4:ncol(object$data)]
        } else {
          if(data.type == "norm.fl"){
            data <- object$data$norm.fluorescence[ndx, 4:ncol(object$data$norm.fluorescence)]
          } else {
            data <- object$data$fluorescence[ndx, 4:ncol(object$data$fluorescence)]
          }
        }
        data <- split(as.matrix(data), 1:nrow(as.matrix(data)))
        data <- lapply(1:length(data), function(i) as.numeric(data[[i]]))


        parameter <- as.list(lapply(1:length(ndx), function(i) as.numeric(cbind(rep(gcTable[ndx[[i]], param], length(data[[i]]))))))
      }

      # correct for unequal lengths of data series
      time.all <- Reduce(union, time)
      for(i in 1:length(time)){
        assign(paste0("time.missing_", i), setdiff(time.all, time[[i]]) )
        if(length(get(paste0("time.missing_", i))) > 0){
          for(j in 1:length(get(paste0("time.missing_", i)))){
            # extract growth values into a separate list
            data[[i]] <- append(data[[i]],
                                values = NA,
                                after = match(get(paste0("time.missing_", i))[j],
                                              time.all) - 1)
            # extract parameter values into a separate list
            parameter[[i]] <- append(parameter[[i]],
                                values = NA,
                                after = match(get(paste0("time.missing_", i))[j],
                                              time.all) - 1)
            # extract time values into a separate list
            time[[i]] <-
              append(time[[i]],
                     values = get(paste0("time.missing_", i))[j],
                     after = match(get(paste0("time.missing_", i))[j], time.all) - 1)
          }
        }
      }
      time <- time[[1]]
      data <- do.call("cbind", data)
      avg <- rowMeans(data, na.rm = FALSE)
      sd <- apply(data, 1, sd, na.rm = FALSE)
      parameter <- do.call("cbind", parameter)
      avg.param <- rowMeans(parameter, na.rm = FALSE)
      plotdata.ls[[n]] <- data.frame("name" = name, "time" = time, "mean" = avg, "upper" = avg+sd, "lower" = avg-sd, "mean.param" = avg.param)
    }
    names(plotdata.ls) <- gsub(" \\| NA", "", conditions_unique)

    plotdata.ls <- plotdata.ls[!is.na(plotdata.ls)]
    df <- do.call(rbind.data.frame, plotdata.ls)
    # add chosen parameter
    # df$param <- gcTable[df$]

    df$name <- gsub(" \\| NA", "", df$name)

    df$concentration <- suppressWarnings(as.numeric(gsub(".+ \\| ", "", df$name)))
    df$group <- gsub(" \\| .+", "", df$name)


    # replace negative lower ribbon boundaries with 0 for log10 transformation
    if(log.y==TRUE){
      df$lower[df$lower<0] <- 0
    }

    if(is.null(IDs) || !sort_by_ID){
      # sort names
      df <- df[order(df$group, df$concentration), ]
    }

    df$name <- factor(df$name, levels = unique(factor(df$name)))

    p <- ggplot(df, aes(x=.data$time, y=.data$mean, group = .data$name), col = "black")
    if(log.y == TRUE){
      p <- p + ggplot2::geom_rect(aes(fill = .data$mean.param, xmin = -Inf, xmax = Inf,
                             ymin = 10^-9, ymax = Inf), alpha = 1, inherit.aes = FALSE, data = df)
    } else {
      p <- p + ggplot2::geom_rect(aes(fill = .data$mean.param), xmin = -Inf, xmax = Inf,
                         ymin = -Inf, ymax = Inf, alpha = 1, inherit.aes = FALSE, data = df)
    }
    p <- p +
      geom_line(linewidth = lwd) +
      geom_ribbon(aes(ymin = .data$lower, ymax=.data$upper), fill = "black", alpha = 0.3, colour = NA) +
      ggplot2::theme_bw(base_size = basesize) +
      xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title), ylab.title, y.title)) +
      theme(strip.text.x = element_text(size = 0.8*basesize),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            legend.justification = c(1,0),
            panel.grid.minor = element_blank(),
            panel.background = ggplot2::element_rect(fill = "white", color = "black",
                                                     linewidth = basesize/22, linetype = "solid"),
            strip.background = ggplot2::element_rect(fill = "white", color = "black",
                                                     linewidth = basesize/22, linetype = "solid"))


    if(!is.null(legend.lim)){
      if(!invert.pal){
        p <- p + ggplot2::scale_fill_continuous(low = rev(single_hue_palettes[[pal]])[1],
                                                high = rev(single_hue_palettes[[pal]])[length(rev(single_hue_palettes[[pal]]))],
                                                limits = legend.lim)
      } else {
        p <- p + ggplot2::scale_fill_continuous(low = rev(single_hue_palettes[[pal]])[length(rev(single_hue_palettes[[pal]]))],
                                                high = rev(single_hue_palettes[[pal]])[1],
                                                limits = legend.lim)
      }

      p <- p +
        ggplot2::guides(fill = ggplot2::guide_colourbar(frame.colour = "black",
                                                        frame.linewidth = basesize/22,
                                                        title.hjust = 0.5,
                                                        title.vjust = 0.8,
                                                        label.theme = element_text(angle = 90, size = 0.8 * basesize),
                                                        label.vjust = 0.7,
                                                        title.position = "bottom",
                                                        title = param,
                                                        nrow = 1,
                                                        barwidth = basesize,
                                                        barheight = basesize/10))
    } else {
      if(!invert.pal){
        p <- p + ggplot2::scale_fill_continuous(low = rev(single_hue_palettes[[pal]])[1],
                                                high = rev(single_hue_palettes[[pal]])[length(rev(single_hue_palettes[[pal]]))])
      } else {
        p <- p + ggplot2::scale_fill_continuous(low = rev(single_hue_palettes[[pal]])[length(rev(single_hue_palettes[[pal]]))],
                                                high = rev(single_hue_palettes[[pal]])[1])
      }

      p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(frame.colour = "black",
                                                        frame.linewidth = basesize/22,
                                                        title.hjust = 0.5,
                                                        title.vjust = 0.8,
                                                        label.theme = element_text(angle = 90, size = 0.8 * basesize),
                                                        label.vjust = 0.7,
                                                        title.position = "bottom",
                                                        title = param,
                                                        nrow = 1,
                                                        barwidth = basesize,
                                                        barheight = basesize/10))
    }
    if(length(unique(df$concentration))<2)
      sort_by_conc <- FALSE

    if(sort_by_conc){
      p <- p + ggh4x::facet_nested(concentration ~ group, scales = "free") +
        ggh4x::facetted_pos_scales(
          x = list(
            group == unique(df$group)[1] ~
              if(!is.null(x.lim)){
                scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 6))
              } else {
                scale_x_continuous(limits = range(df$time), breaks = scales::pretty_breaks(n = 6))
              },
            group != unique(df$group)[1] ~
              if(!is.null(x.lim)){
                scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 6), labels = NULL)
              } else {
                scale_x_continuous(limits = range(df$time), breaks = scales::pretty_breaks(n = 6), labels = NULL)
              }
          ),
          y = list(
            concentration == unique(df$concentration)[length(unique(df$concentration))] ~
              if(log.y == TRUE){
                if(!is.null(y.lim)){
                  scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10))
                } else {
                  scale_y_log10(limits = range(df$mean), breaks = scales::log_breaks(n = n.ybreaks, base = 10))
                }
              } else {
                if(!is.null(y.lim)){
                  scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks))
                } else {
                  scale_y_continuous(limits = range(df$mean), breaks = scales::pretty_breaks(n = n.ybreaks))
                }
              }
            ,
            concentration != unique(df$concentration)[length(unique(df$concentration))] ~
              if(log.y == TRUE){
                if(!is.null(y.lim)){
                  scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10), labels = NULL)
                } else {
                  scale_y_log10(limits = range(df$mean), breaks = scales::log_breaks(n = n.ybreaks, base = 10), labels = NULL)
                }
              } else {
                if(!is.null(y.lim)){
                  scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks), labels = NULL)
                } else {
                  scale_y_continuous(limits = range(df$mean), breaks = scales::pretty_breaks(n = n.ybreaks), labels = NULL)
                }
              }
          )
        )
    } else {
      if(!is.null(nrow)){
        p <- p + ggh4x::facet_wrap2(~name, scales = "free", nrow = nrow)
      } else {
        rows <- ceiling(length(unique(df$name))/10)
        p <- p + ggh4x::facet_wrap2(~name, scales = "free", nrow = rows)
      }
      # get number of facet in the bottom-left
      g <- ggplot2::ggplotGrob(p)
      number <- which(g$layout[grep("panel", g$layout$name), ]["t"] ==
                        unique(g$layout[grep("panel", g$layout$name), ]["t"])[
                          nrow(unique(g$layout[grep("panel", g$layout$name), ]["t"])),
                        ])[1]
      # get number of facets in bottom row except the left
      number_bottom <- which(g$layout[grep("panel", g$layout$name), ]["t"] ==
                        unique(g$layout[grep("panel", g$layout$name), ]["t"])[
                          nrow(unique(g$layout[grep("panel", g$layout$name), ]["t"])),
                        ])[-1]

      # get number of facets on the left except the bottom one
      number_left <- match(unique(g$layout[grep("panel", g$layout$name), ]["t"])[
                                 -nrow(unique(g$layout[grep("panel", g$layout$name), ]["t"])),
                               ], g$layout[grep("panel", g$layout$name), ][,"t"])

      p <- p +  ggh4x::facetted_pos_scales(
        x = list(
          name == as.character(unique(df$name)[number]) ~
            if(!is.null(x.lim)){
              scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 6))
            } else {
              scale_x_continuous(limits = range(df$time), breaks = scales::pretty_breaks(n = 6))
            },
          name %in% as.character(unique(df$name)[number_bottom]) ~
            if(!is.null(x.lim)){
              scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 6), labels = NULL)
            } else {
              scale_x_continuous(limits = range(df$time), breaks = scales::pretty_breaks(n = 6), labels = NULL)
            },
          name %in% as.character(unique(df$name)[-c(number, number_bottom)]) ~
            if(!is.null(x.lim)){
              scale_x_continuous(limits = x.lim, breaks = NULL, labels = NULL)
            } else {
              scale_x_continuous(limits = range(df$time), breaks = NULL, labels = NULL)
            }
        ),
        y = list(
          name == as.character(unique(df$name)[number]) ~
            if(log.y == TRUE){
              if(!is.null(y.lim)){
                scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10))
              } else {
                scale_y_log10(limits = range(df$mean), breaks = scales::log_breaks(n = n.ybreaks, base = 10))
              }
            } else {
              if(!is.null(y.lim)){
                scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
              } else {
                scale_y_continuous(limits = range(df$mean), breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
              }
            }
          ,
          name %in% as.character(unique(df$name)[number_left]) ~
            if(log.y == TRUE){
              if(!is.null(y.lim)){
                scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10), labels = NULL)
              } else {
                scale_y_log10(limits = range(df$mean), breaks = scales::log_breaks(n = n.ybreaks, base = 10), labels = NULL)
              }
            } else {
              if(!is.null(y.lim)){
                scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE), labels = NULL)
              } else {
                scale_y_continuous(limits = range(df$mean), breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE), labels = NULL)
              }
            }
          ,
          name %in% as.character(unique(df$name)[-c(number, number_left)]) ~
            if(log.y == TRUE){
              if(!is.null(y.lim)){
                scale_y_log10(limits = y.lim, breaks = NULL, labels = NULL)
              } else {
                scale_y_log10(limits = range(df$mean), breaks = NULL, labels = NULL)
              }
            } else {
              if(!is.null(y.lim)){
                scale_y_continuous(limits = y.lim, breaks = NULL, labels = NULL)
              } else {
                scale_y_continuous(limits = range(df$mean), breaks = NULL, labels = NULL)
              }
            }
        )
      )
    }
  } # if(mean == TRUE)
  else {
    df <- data.frame()
    for(i in 1:length(ndx.keep)){
      if(any(is(object) %in% "flFitRes")){
        if(data.type == "spline"){
          df <- plyr::rbind.fill(df,
                                 data.frame("name" = sample.nm[ndx.keep[i]],
                                            "time" = object$flFit$flFittedSplines[[ndx.keep[i]]][["fit.x"]],
                                            "y" = object$flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]],
                                            "param" = as.numeric(rep(gcTable[ndx.keep[i], param],
                                                                     length(object$flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]]))
                                            )
                                 )
          )
        } else {
          if(data.type == "norm.fl"){
            df <- plyr::rbind.fill(df,
                                   data.frame("name" = sample.nm[ndx.keep[i]],
                                              "time" = as.vector(object$time[ndx.keep[i], ]),
                                              "y" = unlist(unname(utils::type.convert(object$data$norm.fluorescence[ndx.keep[i], 4:ncol(object$data$norm.fluorescence)], as.is=T))),
                                              "param" = as.numeric(rep(gcTable[ndx.keep[i], param],
                                                                       length(as.vector(object$time[ndx.keep[i], ])))
                                              )
                                   )
            )
          } else {
            df <- plyr::rbind.fill(df,
                                   data.frame("name" = sample.nm[ndx.keep[i]],
                                              "time" = as.vector(object$time[ndx.keep[i], ]),
                                              "y" = unlist(unname(utils::type.convert(object$data$fluorescence[ndx.keep[i], 4:ncol(object$data$fluorescence)], as.is=T))),
                                              "param" = as.numeric(rep(gcTable[ndx.keep[i], param],
                                                                       length(as.vector(object$time[ndx.keep[i], ])))
                                              )
                                   )
            )
          }
        }
      } else {
        if(data.type == "spline"){
          df <- plyr::rbind.fill(df,
                                 data.frame("name" = sample.nm[ndx.keep[i]],
                                            "time" = object$gcFit$gcFittedSplines[[ndx.keep[i]]][["fit.time"]],
                                            "y" = object$gcFit$gcFittedSplines[[ndx.keep[i]]][["fit.data"]],
                                            "param" = as.numeric(rep(gcTable[ndx.keep[i], param],
                                                                     length(object$gcFit$gcFittedSplines[[ndx.keep[i]]][["fit.data"]]))
                                            )
                                 )
          )
        } else {
          df <- plyr::rbind.fill(df,
                                 data.frame("name" = sample.nm[ndx.keep[i]],
                                            "time" = as.vector(object$time[ndx.keep[i], ]),
                                            "y" = unlist(unname(utils::type.convert(object$data[ndx.keep[i], 4:ncol(object$data)], as.is=T))),
                                            "param" = as.numeric(rep(gcTable[ndx.keep[i], param],
                                                                     length(as.vector(object$time[ndx.keep[i], ])))
                                            )
                                 )
          )
        }
      }
    }
    # add concentration column
    df$concentration <- as.numeric(gsub(".+ \\| ", "", df$name))
    # add group column
    df$group <- gsub(" \\| .+", "", df$name)
    if(is.null(IDs) || !sort_by_ID){
      # sort names
      df <- df[order(df$group, df$concentration), ]
    }

    # remove "NA" from names
    df$name <- gsub(" \\| NA", "", df$name)

    df$name <- factor(df$name, levels = unique(factor(df$name)))

    p <- ggplot(df, aes(x=.data$time, y=.data$y, group = .data$name), col = "black")
    if(log.y == TRUE){
      p <- p + ggplot2::geom_rect(aes(fill = .data$param, xmin = -Inf, xmax = Inf,
                             ymin = 10^-9, ymax = Inf), alpha = 1, inherit.aes = FALSE, data = df)
    } else {
      p <- p + ggplot2::geom_rect(aes(fill = .data$param), xmin = -Inf, xmax = Inf,
                         ymin = -Inf, ymax = Inf, alpha = 1, inherit.aes = FALSE, data = df)
    }
      p <- p +
      geom_line(linewidth = lwd) +
        ggplot2::theme_bw(base_size = basesize) +
      xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title), ylab.title, y.title)) +
      theme(strip.text.x = element_text(size = 0.8*basesize),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = ggplot2::element_rect(fill = "white", color = "black",
                                            linewidth = basesize/22, linetype = "solid"),
            strip.background = ggplot2::element_rect(fill = "white", color = "black",
                                            linewidth = basesize/22, linetype = "solid"))

    if(!is.null(legend.lim)){
      if(!invert.pal){
        p <- p + ggplot2::scale_fill_continuous(low = rev(single_hue_palettes[[pal]])[1],
                                                high = rev(single_hue_palettes[[pal]])[length(rev(single_hue_palettes[[pal]]))],
                                                limits = legend.lim)
      } else {
        p <- p + ggplot2::scale_fill_continuous(low = rev(single_hue_palettes[[pal]])[length(rev(single_hue_palettes[[pal]]))],
                                                high = rev(single_hue_palettes[[pal]])[1],
                                                limits = legend.lim)
      }

      p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(frame.colour = "black",
                                                        frame.linewidth = basesize/22,
                                                        title.hjust = 0.5,
                                                        title.vjust = 0.8,
                                                        title.position = "bottom",
                                                        label.theme = element_text(angle = 90, size = 0.8 * basesize),
                                                        label.vjust = 0.7,
                                                        title = param,
                                                        nrow = 1,
                                                        barwidth = basesize,
                                                        barheight = basesize/10))
    } else {
      if(!invert.pal){
        p <- p + ggplot2::scale_fill_continuous(low = rev(single_hue_palettes[[pal]])[1],
                                                high = rev(single_hue_palettes[[pal]])[length(rev(single_hue_palettes[[pal]]))])
      } else {
        p <- p + ggplot2::scale_fill_continuous(low = rev(single_hue_palettes[[pal]])[length(rev(single_hue_palettes[[pal]]))],
                                                high = rev(single_hue_palettes[[pal]])[1])
      }

      p <- p + ggplot2::guides(fill = ggplot2::guide_colourbar(frame.colour = "black",
                                                        frame.linewidth = basesize/22,
                                                        title.hjust = 0.5,
                                                        title.vjust = 0.8,
                                                        title.position = "bottom",
                                                        title = param,
                                                        label.theme = element_text(angle = 90, size = 0.8 * basesize),
                                                        label.vjust = 0.7,
                                                        nrow = 1,
                                                        barwidth = basesize,
                                                        barheight = basesize/10))
    }

    if(sort_by_conc){
      p <- p + ggh4x::facet_nested(concentration ~ group, scales = "free") +
        ggh4x::facetted_pos_scales(
          x = list(
            group == unique(df$group)[1] ~
              if(!is.null(x.lim)){
                scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
              } else {
                scale_x_continuous(limits = range(df$time), breaks = scales::pretty_breaks(n = 10))
              },
            group != unique(df$group)[1] ~
              if(!is.null(x.lim)){
                scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10), labels = NULL)
              } else {
                scale_x_continuous(limits = range(df$time), breaks = scales::pretty_breaks(n = 10), labels = NULL)
              }
          ),
          y = list(
            concentration == unique(df$concentration)[length(unique(df$concentration))] ~
              if(log.y == TRUE){
                if(!is.null(y.lim)){
                  scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10))
                } else {
                  scale_y_log10(limits = range(df$y), breaks = scales::log_breaks(n = n.ybreaks, base = 10))
                }
              } else {
                if(!is.null(y.lim)){
                  scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
                } else {
                  scale_y_continuous(limits = range(df$y), breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
                }
              }
            ,
            concentration != unique(df$concentration)[length(unique(df$concentration))] ~
              if(log.y == TRUE){
                if(!is.null(y.lim)){
                  scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10), labels = NULL)
                } else {
                  scale_y_log10(limits = range(df$y), breaks = scales::log_breaks(n = n.ybreaks, base = 10), labels = NULL)
                }
              } else {
                if(!is.null(y.lim)){
                  scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE), labels = NULL)
                } else {
                  scale_y_continuous(limits = range(df$y), breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE), labels = NULL)
                }
              }
          )
        )
    } else {
      if(!is.null(nrow)){
        p <- p + ggh4x::facet_wrap2(~name, scales = "free", nrow = nrow)
      } else {
        rows <- ceiling(length(unique(df$name))/10)
        p <- p + ggh4x::facet_wrap2(~name, scales = "free", nrow = rows)
      }
      # get number of facet in the bottom-left
      g <- ggplot2::ggplotGrob(p)
      number <- which(g$layout[grep("panel", g$layout$name), ]["t"] ==
                        unique(g$layout[grep("panel", g$layout$name), ]["t"])[
                          nrow(unique(g$layout[grep("panel", g$layout$name), ]["t"])),
                        ])[1]

      # get number of facets in bottom row except the left
      number_bottom <- which(g$layout[grep("panel", g$layout$name), ]["t"] ==
                               unique(g$layout[grep("panel", g$layout$name), ]["t"])[
                                 nrow(unique(g$layout[grep("panel", g$layout$name), ]["t"])),
                               ])[-1]

      # get number of facets on the left except the bottom one
      number_left <- match(unique(g$layout[grep("panel", g$layout$name), ]["t"])[
        -nrow(unique(g$layout[grep("panel", g$layout$name), ]["t"])),
      ], g$layout[grep("panel", g$layout$name), ][,"t"])

      p <- p +  ggh4x::facetted_pos_scales(
        x = list(
          name == unique(df$name)[number] ~
            if(!is.null(x.lim)){
              scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 6))
            } else {
              scale_x_continuous(limits = range(df$time), breaks = scales::pretty_breaks(n = 6))
            },
          name %in% unique(df$name)[number_bottom] ~
            if(!is.null(x.lim)){
              scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 6), labels = NULL)
            } else {
              scale_x_continuous(limits = range(df$time), breaks = scales::pretty_breaks(n = 6), labels = NULL)
            },
          name %in% unique(df$name)[-c(number, number_bottom)] ~
            if(!is.null(x.lim)){
              scale_x_continuous(limits = x.lim, breaks = NULL, labels = NULL)
            } else {
              scale_x_continuous(limits = range(df$time), breaks = NULL, labels = NULL)
            }
        ),
        y = list(
          name == unique(df$name)[number] ~
            if(log.y == TRUE){
              if(!is.null(y.lim)){
                scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10))
              } else {
                scale_y_log10(limits = range(df$y), breaks = scales::log_breaks(n = n.ybreaks, base = 10))
              }
            } else {
              if(!is.null(y.lim)){
                scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
              } else {
                scale_y_continuous(limits = range(df$y), breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
              }
            }
          ,
          name %in% unique(df$name)[number_left] ~
            if(log.y == TRUE){
              if(!is.null(y.lim)){
                scale_y_log10(limits = y.lim, breaks = scales::log_breaks(n = n.ybreaks, base = 10), labels = NULL)
              } else {
                scale_y_log10(limits = range(df$y), breaks = scales::log_breaks(n = n.ybreaks, base = 10), labels = NULL)
              }
            } else {
              if(!is.null(y.lim)){
                scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE), labels = NULL)
              } else {
                scale_y_continuous(limits = range(df$y), breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE), labels = NULL)
              }
            }
          ,
          name %in% unique(df$name)[-c(number, number_left)] ~
            if(log.y == TRUE){
              if(!is.null(y.lim)){
                scale_y_log10(limits = y.lim, breaks = NULL, labels = NULL)
              } else {
                scale_y_log10(limits = range(df$y), breaks = NULL, labels = NULL)
              }
            } else {
              if(!is.null(y.lim)){
                scale_y_continuous(limits = y.lim, breaks = NULL, labels = NULL)
              } else {
                scale_y_continuous(limits = range(df$y), breaks = NULL, labels = NULL)
              }
            }
        )
      )
    }

  }
  if(export == FALSE && plot == FALSE){
    return(p)
  }
  out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
  if (export == TRUE){
    if(is.null(out.nm)) out.nm <- paste0("GroupPlot")
    if(is.null(width)){
      w <- 10 + 3*ifelse(mean==TRUE,length(conditions_unique), length(nm))/15
    } else {
      w <- width
    }
    if(is.null(height)){
      h <- 6
    } else {
      h <- height
    }
    dir.create(out.dir, showWarnings = FALSE)
    grDevices::png(paste0(out.dir, "/", out.nm, ".png"),
                   width = w, height = h, units = 'in', res = 300)
    suppressWarnings(print(p))
    grDevices::dev.off()
    if (requireNamespace("Cairo", quietly = TRUE)) {
      Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
    } else {
      message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
      grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", out.nm, ".pdf"))
    }
    suppressWarnings( print(p))
    grDevices::dev.off()
  }
  if (plot == TRUE){
    suppressWarnings(print(p))
  } else {
    return(p)
  }
}
