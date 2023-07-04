#' Generic plot function for \code{flcFittedLinear} objects. Plot the results of a linear regression on ln-transformed data
#'
#' \code{plot.flFitLinear} shows the results of a linear regression and visualizes raw data, data points included in the fit, the tangent obtained by linear regression, and the lag time.
#'
#' @param x A \code{flFittedLinear} object created with \code{\link{flFitLinear}} or stored within a \code{flFitRes} or \code{flFit} object created with \code{\link{fl.workflow}} or \code{\link{flFit}}, respectively.
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
#' @export plot.flFitLinear
#' @export
#'
#' @return A plot with the linear fit.
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
#'                    data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
#'                    csvsep = "\t",
#'                    csvsep.fl = "\t")
#'
#' # Extract time and normalized fluorescence data for single sample
#' time <- input$time[4,]
#' data <- input$norm.fluorescence[4,-(1:3)] # Remove identifier columns
#'
#' # Perform linear fit
#' TestFit <- flFitLinear(time = time,
#'                        fl_data = data,
#'                        ID = "TestFit",
#'                        control = fl.control(fit.opt = "l", x_type = "time",
#'                        lin.R2 = 0.95, lin.RSD = 0.1,
#'                        lin.h = 20))
#'
#' plot(TestFit)
#
plot.flFitLinear <- function(x, log="", which=c("fit", "diagnostics", "fit_diagnostics"), pch = 21, cex.point = 1, cex.lab = 1.5,
                             cex.axis = 1.3, lwd = 2, color = "firebrick3", y.lim = NULL, x.lim = NULL,
                             plot = TRUE, export = FALSE, height = ifelse(which=="fit", 7, 5),
                             width = ifelse(which=="fit", 9, 9), out.dir = NULL, ...)
  {
  flFittedLinear <- x
  if(!is.null(color))
    colSpline <- toupper(color)

  if(methods::is(flFittedLinear) != "flFitLinear") stop("x needs to be an object created with flFitLinear().")
  which <- match.arg(which)
  control <- flFittedLinear$control
  if(control$x_type == "time"){
    if(control$norm_fl){
      if(control$log.y.lin){
        ylab = "norm. Ln[FL/FL(0)]"
      } else {
        ylab = "norm. FL"
      }
    } else {
      if(control$log.y.lin){
        ylab = "Ln[FL/FL(0)]"
      } else {
        ylab = "FL"
      }
    }
  } else {
    if(control$log.y.lin){
      ylab = "Ln[FL/FL(0)]"
    } else {
      ylab = "FL"
    }
  }
  if(control$x_type == "time"){
    if(control$log.x.lin){
      xlab <- "Ln(time + 1)"
    } else {
      xlab <- "Time"
    }
  } else {
    if(control$log.x.lin){
      xlab <- "Ln(growth + 1)"
    } else {
      xlab <- "Density"
    }
  }

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  p <- function(){
    switch(which,
           fit = {
             par(mar=c(5.1+cex.lab, 4.1+cex.lab+0.5*cex.axis, 4.1, 3.1), cex.lab = cex.lab, cex.axis = cex.axis)

             plot(flFittedLinear$"fl.in" ~ flFittedLinear$"x.in", xlab="", ylab = "", pch = pch,
                  log=log, las=1, yaxt="n", xaxt="n", type = "n", xlim = x.lim, ylim = y.lim, ...)
             points(flFittedLinear$"fl.in" ~ flFittedLinear$"x.in", cex = cex.point, pch=pch)

             title(ylab = ylab, line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = xlab, line = 1 + 0.7*cex.lab + 0.7*cex.axis, cex.lab = cex.lab)

             try(points(flFittedLinear$fl.in[flFittedLinear$ndx.in] ~ flFittedLinear$x.in[flFittedLinear$ndx.in], pch=pch, cex = cex.point*1.15, col="black", bg=color))
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)

             ## lag phase
             lag <- flFittedLinear$par["lag"]
             coef_ <- flFittedLinear$par

             if(flFittedLinear$fitFlag2){
               try(points(flFittedLinear$fl.in[flFittedLinear$ndx2.in] ~ flFittedLinear$x.in[flFittedLinear$ndx2.in], pch=pch, cex = cex.point*1.15, col="black", bg=ggplot2::alpha("magenta3", 1)))
               lag2 <- flFittedLinear$par["lag2"]
               if(lag2 < lag && lag2 > flFittedLinear$x.in[1]){
                 try(time2 <- seq(lag2, max(flFittedLinear$"x.in"), length=200), silent = TRUE)
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"x.in"), length=200), silent = TRUE)
                 try(lines(time2, grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = TRUE)
                 try(lines(c(min(flFittedLinear$"x.in"[1]), lag2), rep(flFittedLinear$"fl.in"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7)), silent = TRUE)
                 try(lines(time, grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               } else {
                 try(time2 <- seq(coef_["x.max2_start"]-0.25*(coef_["x.max2_end"]-coef_["x.max2_start"]), max(flFittedLinear$"x.in"), length=200), silent = TRUE)
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"x.in"), length=200), silent = TRUE)
                 try(lines(time, grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
                 try(lines(time2, grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = TRUE)

               }
             } else if(flFittedLinear$fitFlag){
               if(lag < flFittedLinear$x.in[flFittedLinear$ndx.in[1]]){
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"filt.x"), length=200), silent = TRUE)
                 try(lines(time, grow_linear(time, coef_)[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               } else {
                 try(time <- seq(flFittedLinear$filt.x[flFittedLinear$ndx.in[1]]/2, max(flFittedLinear$"filt.x"), length=200), silent = TRUE)
                 try(lines(time, grow_linear(time, coef_)[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               }
             }

             graphics::mtext(bquote(slope[max]: ~ .(round(flFittedLinear$par[["max_slope"]], digits = 3))~~~~
                                      lambda: ~ .(round(flFittedLinear$par[["lag"]], digits = 3))~~~~
                                      x[max]: ~ .(round(flFittedLinear$par[["x.max_start"]], digits = 2))-.(round(flFittedLinear$par[["x.max_end"]], digits = 2))~~~~
                                      R2:~ .(round(flFittedLinear$rsquared, digits = 3))),
                             side = 4 , adj = 0.45, line = -2.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab*0.7)

             graphics::mtext(paste("h:", ifelse(is.null(flFittedLinear$control$lin.h), "NULL", flFittedLinear$control$lin.h),
                                   "   R2-thresh.:",  flFittedLinear$control$lin.R2,
                                   "   RSD-thresh.:",  flFittedLinear$control$lin.RSD,
                                   "t0:", flFittedLinear$control$t0,
                                   "  min.growth:", flFittedLinear$control$min.growth,
                                   "   dY-thresh.:",  flFittedLinear$control$lin.dY),
                             cex = cex.lab*0.7, side = 3, line = -2.5, adj = 0.05, outer = TRUE)
           },
           diagnostics = {

             par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), cex.lab = cex.lab, cex.axis = cex.axis, mfrow=c(1,2))

             ## residuals vs. fitted
             obs <- flFittedLinear$log.data
             sim <- grow_linear(flFittedLinear$"x.in", flFittedLinear$par)
             plot(flFittedLinear$fit[["residuals"]] ~ fitted(flFittedLinear$fit), xlab="", ylab="", type = "n", pch = pch, xaxt="n", yaxt="n")
             points(flFittedLinear$fit[["residuals"]] ~ fitted(flFittedLinear$fit), cex = cex.point, pch=pch)
             abline(h=0, col="grey")
             title(ylab = "residuals", line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = "fitted", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)
             ## normal q-q-plot
             stats::qqnorm(flFittedLinear$fit[["residuals"]], cex = cex.point, xlab="", ylab="", xaxt="n", yaxt="n", main = "")
             stats::qqline(flFittedLinear$fit[["residuals"]])
             title("Normal Q-Q Plot", line = 1, cex.main = cex.lab)
             title(ylab = "Sample quantiles", line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = "Theoretical quantiles", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)
           },
           fit_diagnostics = {

             layout(matrix(c(1,1,2,3), nrow=2, byrow=TRUE))
             par(mar=c(5.1, 4.1+cex.lab, 4.1, 3.1), mai = c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.7 + 0.2*cex.lab + 0.2*cex.axis, 0.5, 0.5), cex.lab = cex.lab, cex.axis = cex.axis)

             plot(flFittedLinear$"fl.in" ~ flFittedLinear$"x.in", xlab="", ylab = "",
                  log=log, las=1, yaxt="n", xaxt="n", type = "n", xlim = x.lim, ylim = y.lim, pch = pch, ...)
             points(flFittedLinear$"fl.in" ~ flFittedLinear$"x.in", cex = cex.point, pch=pch)

             title(ylab = ylab, line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = xlab, line = 1 + 0.7*cex.lab + 0.7*cex.axis, cex.lab = cex.lab)

             try(points(flFittedLinear$fl.in[flFittedLinear$ndx.in] ~ flFittedLinear$x.in[flFittedLinear$ndx.in], pch=pch, cex = cex.point*1.15, col="black", bg=color))
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)

             ## lag phase
             lag <- flFittedLinear$par["lag"]
             coef_ <- flFittedLinear$par

             if(flFittedLinear$fitFlag2){
               try(points(flFittedLinear$fl.in[flFittedLinear$ndx2.in] ~ flFittedLinear$x.in[flFittedLinear$ndx2.in], pch=pch, cex = cex.point*1.15, col="black", bg=ggplot2::alpha("magenta3", 1)))
               lag2 <- flFittedLinear$par["lag2"]
               if(lag2 < lag && lag2 > flFittedLinear$x.in[1]){
                 try(time2 <- seq(lag2, max(flFittedLinear$"x.in"), length=200), silent = TRUE)
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"x.in"), length=200), silent = TRUE)
                 try(lines(time2, grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = TRUE)
                 try(lines(c(min(flFittedLinear$"x.in"[1]), lag2), rep(flFittedLinear$"fl.in"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7)), silent = TRUE)
                 try(lines(time, grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               } else {
                 try(time2 <- seq(coef_["x.max2_start"]-0.25*(coef_["x.max2_end"]-coef_["x.max2_start"]), max(flFittedLinear$"x.in"), length=200), silent = TRUE)
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"x.in"), length=200), silent = TRUE)
                 try(lines(time, grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
                 try(lines(time2, grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = TRUE)

               }
             } else if(flFittedLinear$fitFlag){
               if(lag < flFittedLinear$x.in[flFittedLinear$ndx.in[1]]){
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"filt.x"), length=200), silent = TRUE)
                 try(lines(time, grow_linear(time, coef_)[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               } else {
                 try(time <- seq(flFittedLinear$filt.x[flFittedLinear$ndx.in[1]]/2, max(flFittedLinear$"filt.x"), length=200), silent = TRUE)
                 try(lines(time, grow_linear(time, coef_)[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha(color, 0.7), ...), silent = TRUE)
               }
             }
             graphics::mtext(bquote(slope[max]: ~ .(round(flFittedLinear$par[["max_slope"]], digits = 3))~~~~
                                      lambda: ~ .(round(flFittedLinear$par[["lag"]], digits = 3))~~~~
                                      x[max]: ~ .(round(flFittedLinear$par[["x.max_start"]], digits = 2))-.(round(flFittedLinear$par[["x.max_end"]], digits = 2))~~~~
                                      R2:~ .(round(flFittedLinear$rsquared, digits = 3))),
                             side = 4 , adj = 0.55, line = -2.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab*0.7)

             graphics::mtext(paste("h:", ifelse(is.null(flFittedLinear$control$lin.h), "NULL", flFittedLinear$control$lin.h),
                                   "   R2-thresh.:",  flFittedLinear$control$lin.R2,
                                   "   RSD-thresh.:",  flFittedLinear$control$lin.RSD,
                                   "t0:", flFittedLinear$control$t0,
                                   "  min.growth:", flFittedLinear$control$min.growth,
                                   "   dY-thresh.:",  flFittedLinear$control$lin.dY),
                             cex = cex.lab*0.7, side = 3, line = -2.5, adj = 0.05, outer = TRUE)

             ## residuals vs. fitted
             obs <- flFittedLinear$log.data
             sim <- grow_linear(flFittedLinear$"x.in", flFittedLinear$par)
             plot(flFittedLinear$fit[["residuals"]] ~ fitted(flFittedLinear$fit), xlab="", ylab="", type = "n", pch = pch, xaxt="n", yaxt="n")
             points(flFittedLinear$fit[["residuals"]] ~ fitted(flFittedLinear$fit), cex = cex.point, pch=pch)
             abline(h=0, col="grey")
             title(ylab = "residuals", line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = "fitted", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)
             ## normal q-q-plot
             stats::qqnorm(flFittedLinear$fit[["residuals"]], cex = cex.point, xlab="", ylab="", xaxt="n", yaxt="n", main = "")
             stats::qqline(flFittedLinear$fit[["residuals"]])
             title("Normal Q-Q Plot", line = 1, cex.main = cex.lab)
             title(ylab = "Sample quantiles", line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = "Theoretical quantiles", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)
           }
    )
  }
  if (export == TRUE){
    w <- width
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = FALSE)
    grDevices::png(paste0(out.dir, "/", paste(flFittedLinear$gcID, collapse = "_"), "_LinFitPlot.png"),
                   width = w, height = h, units = 'in', res = 300)
    p()
    grDevices::dev.off()
    if (requireNamespace("Cairo", quietly = TRUE)) {
      Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(flFittedLinear$gcID, collapse = "_"), "_LinFitPlot.pdf"))
    } else {
      message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
      grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", paste(flFittedLinear$gcID, collapse = "_"), "_LinFitPlot.pdf"))
    }
    p()
    grDevices::dev.off()
  }
  if (plot == TRUE){
    p()
  }
}

#' Generic plot function for \code{flFitSpline} objects.
#'
#' \code{plot.flFitSpline} generates the spline fit plot for a single sample.
#'
#' @param x Object of class \code{flFitSpline}, created with \code{\link{flFitSpline}}.
#' @param add (Logical) Shall the fitted spline be added to an existing plot? \code{TRUE} is used internally by \code{\link{plot.flBootSpline}}.
#' @param raw (Logical) Display raw growth as circles (\code{TRUE}) or not (\code{FALSE}).
#' @param slope (Logical) Show the slope at the maximum slope (\code{TRUE}) or not (\code{FALSE}).
#' @param deriv (Logical) Show the derivative (i.e., slope) over time in a secondary plot (\code{TRUE}) or not (\code{FALSE}).
#' @param spline (Logical) Only for \code{add = TRUE}: add the current spline to the existing plot (\code{FALSE}).
#' @param log.y (Logical) Log-transform the y-axis (\code{TRUE}) or not (\code{FALSE}).
#' @param pch (Numeric) Symbol used to plot data points.
#' @param colData (Numeric or character) Contour color of the raw data circles.
#' @param colSpline (Numeric or character) Spline line colour.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param basesize (Numeric) Base font size.
#' @param lwd (Numeric) Spline line width.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on y-axis of the fluorescence curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis of both fluorescence curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim.deriv (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis of the derivative plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{axisTicks()}. Thus, the final number of breaks can deviate from the user input.
#' @param y.title (Character) Optional: Provide a title for the y-axis of the growth curve plot.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both growth curve and derivative plots.
#' @param y.title.deriv (Character) Optional: Provide a title for the y-axis of the derivative plot.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @export plot.flFitSpline
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
#'
#' @return A plot with the nonparametric fit.
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
#'                    data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
#'                    csvsep = "\t",
#'                    csvsep.fl = "\t")
#'
#' # Extract time and normalized fluorescence data for single sample
#' time <- input$time[4,]
#' data <- input$norm.fluorescence[4,-(1:3)] # Remove identifier columns
#'
#' # Perform linear fit
#' TestFit <- flFitSpline(time = time,
#'                        fl_data = data,
#'                        ID = "TestFit",
#'                        control = fl.control(fit.opt = "s", x_type = "time"))
#'
#' plot(TestFit)
#
plot.flFitSpline <- function(x, add=FALSE, raw = TRUE, slope=TRUE, deriv = TRUE, spline = TRUE, log.y = FALSE, basesize = 16,
                             pch=1, colData=1, colSpline="dodgerblue3", cex.point=2, lwd = 0.7,
                             y.lim = NULL, x.lim = NULL, y.lim.deriv = NULL,  n.ybreaks = 6,
                             y.title = NULL, x.title = NULL, y.title.deriv = NULL,
                             plot = TRUE, export = FALSE, width = 8, height = ifelse(deriv == TRUE, 8, 6),
                             out.dir = NULL, ...)
  {
  flFitSpline <- x
  if(!is.null(colSpline))
    colSpline <- toupper(colSpline)
  if(!is.null(colData))
    colData <- toupper(colData)

  n.ybreaks <- as.numeric(n.ybreaks)
  # x an object of class flFitSpline
  if(methods::is(flFitSpline) != "flFitSpline") stop("x needs to be an object created with flFitSpline().")
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
  if ((is.na(flFitSpline$fitFlag)==TRUE)|(flFitSpline$fitFlag==FALSE)){
    warning("plot.flFitSpline: no data fit available!")
  }

  if (add==TRUE){
    if(spline == TRUE){
      # /// try to plot data fit
      if ((flFitSpline$control$log.x.spline==FALSE) && (flFitSpline$control$log.y.spline==FALSE)){
        try( lines(flFitSpline$fit.x, flFitSpline$fit.fl, sub=flFitSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd) )
      }

      if ((flFitSpline$control$log.x.spline==FALSE) && (flFitSpline$control$log.y.spline==TRUE)){
        try( lines(flFitSpline$fit.x, flFitSpline$fit.fl, sub=flFitSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd) )
      }

      if ((flFitSpline$control$log.x.spline==TRUE)  && (flFitSpline$control$log.y.spline==FALSE)){
        try( lines(flFitSpline$fit.x, flFitSpline$fit.fl, sub=flFitSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd ) )
      }

      if ((flFitSpline$control$log.x.spline==TRUE)  && (flFitSpline$control$log.y.spline==TRUE)){
        try( lines(flFitSpline$fit.x, flFitSpline$fit.fl, sub=flFitSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd) )
      }
      # /// add tangent at maximum slope
      if (slope==TRUE){
        max_slope     <- as.numeric(flFitSpline$parameters$max_slope)
        lambda <- as.numeric(flFitSpline$parameters$lambda)

        time <- seq(lambda, max(flFitSpline$"fit.x"), length=200)
        y_tangent <- flFitSpline$parameters["b.tangent"][[1]]+time*max_slope
        try(lines(time, y_tangent, lty=2, lwd= 2.8*lwd, col=ggplot2::alpha(colSpline, 0.85), ...))
        try(lines(c(min(flFitSpline$"raw.x"[1]), lambda), rep(flFitSpline$"raw.fl"[1], 2), lty=2, lwd=2.8*lwd, col=ggplot2::alpha(colSpline, 0.7)))
      }
    }
    if (deriv  == TRUE){
      if ((flFitSpline$control$log.x.spline == FALSE)){
        try( lines(flFitSpline$spline.deriv1$x, flFitSpline$spline.deriv1$y, lwd=2.8*lwd, xlab="", ylab="", col = colSpline) )
      }
      if ((flFitSpline$control$log.x.spline == TRUE)){
        try( lines(flFitSpline$spline.deriv1$x, flFitSpline$spline.deriv1$y, lwd=2.8*lwd, xlab="", ylab="", col = colSpline) )
      }
    }
  } # if (add == TRUE)
  else {
    if(flFitSpline$fitFlag == TRUE){
      coef <- flFitSpline[["parameters"]]
      lagtime <- coef["lambda"][[1]][1]
      # correct for log transformation
      if(flFitSpline$control$log.y.spline == TRUE){
        fit.fl <-
          c(rep(NA, length(flFitSpline[["raw.fl"]]) - length(flFitSpline[["fit.fl"]])), exp(flFitSpline[["fit.fl"]]) *
              flFitSpline[["fl.in"]][1])
      } else {
        fit.fl <- c(rep(NA, length(flFitSpline[["raw.fl"]]) - length(flFitSpline[["fit.fl"]])), flFitSpline[["fit.fl"]])
      }
      if(flFitSpline$control$log.y.spline == TRUE){
        df <- data.frame("time" = flFitSpline[["raw.x"]],
                         "data" = exp(flFitSpline[["raw.fl"]])*flFitSpline[["fl.in"]][1],
                         "fit.x" = c(rep(NA, length(flFitSpline[["raw.x"]])-length(flFitSpline[["fit.x"]])), flFitSpline[["fit.x"]]),
                         "fit.fl" = fit.fl)
      } else{
        df <- data.frame("time" = flFitSpline[["raw.x"]],
                         "data" = flFitSpline[["raw.fl"]],
                         "fit.x" = c(rep(NA, length(flFitSpline[["raw.x"]])-length(flFitSpline[["fit.x"]])), flFitSpline[["fit.x"]]),
                         "fit.fl" = fit.fl)
      }

      x.label = if(flFitSpline$control$x_type == "growth"){
        "Density"
      } else {
        "Time"
      }
      y.label <- if(flFitSpline$control$norm_fl == TRUE){
        "Norm. fluorescence"
      } else {
        "Fluorescence"
      }

      p <- ggplot(df, aes(x=.data$time, y=.data$data)) +
        geom_point(shape=pch, size = cex.point,alpha = 0.6, stroke=0.15*cex.point, color = colData) +
        geom_line(aes(x=.data$fit.x, y = .data$fit.fl, color = "spline"), linewidth = lwd) +
        xlab(ifelse(is.null(x.title), x.label, x.title)) +
        ylab(ifelse(is.null(y.title), y.label, y.title)) +
        theme_classic(base_size = basesize) +
        ggtitle(gsub(" \\| NA", "", paste(flFitSpline$gcID, collapse=" | "))) +
        theme(legend.key = element_blank(),
              legend.background=element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.90, 0.08),
              plot.title = element_text(size = basesize*1.1, face = "bold", vjust = 3),
              plot.subtitle = element_text(size = basesize*0.8),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = unit(c(1,2,1,1), "lines")) +
        scale_color_manual(name='Fluorescence fit',
                           breaks = "Spline fit",
                           values=c("spline" = ggplot2::alpha(colSpline, 0.7), "Spline fit" = ggplot2::alpha(colSpline, 0.7)))

      if(log.y == TRUE){
        if(!is.null(y.lim)){
          if(!is.na(y.lim[1]) && y.lim[1] <= 0){
            message("A lower y axis limit of <= 0 is not supported for log scaling. Lower limit set to 0.001")
            y.lim[1] <- 0.001
          }
          p <- p + scale_y_continuous(limits = y.lim, breaks = base_breaks(n = n.ybreaks), trans = 'log')
        } else {
          p <- p + scale_y_continuous(breaks = base_breaks(n = n.ybreaks), trans = 'log')
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
        #   label = paste("t0:", flFitSpline$control$t0, "  min.growth:", flFitSpline$control$min.growth, "  smoothing:", flFitSpline$control$smooth.fl),
        #   x = 0.5*x_limit[2],
        #   y = ifelse(!is.null(y.lim) && !is.na(y.lim[2]), 1.05 * y.lim[2], 1.3 * y_limit[2]),
        #   angle = 0, parse = FALSE, size = basesize*3.2/12) +
        labs(subtitle = paste("t0:", flFitSpline$control$t0,
                              " tmax:", flFitSpline$control$tmax,
                              "  min.growth:", flFitSpline$control$min.growth,
                              "  max.growth:", flFitSpline$control$max.growth,
                              "  smoothing:", flFitSpline$control$smooth.fl)
        ) +
        annotate(
          "text",
          label = list(bquote(slope[max]: ~ .(round(flFitSpline$parameters$max_slope, digits = 0))~~~~
                                lambda: ~ .(round(flFitSpline$parameters$lambda, digits = 2))~~~~
                                x[max]: ~ .(round(flFitSpline$parameters$x.max, digits = 2)))),
          x = 1.02*x_limit[2],
          y = ifelse(deriv==TRUE, ifelse(log.y == TRUE, 0.5, -0.3), 0.9) * ifelse(!is.null(y.lim) && !is.na(y.lim[1]), y.lim[1], y_limit[1]),
          hjust = 0,
          angle = 90, parse = TRUE, size = basesize*3/12)
      if(!is.null(y.lim) && !is.na(y.lim[2])){
        p <- p + coord_cartesian(xlim = c(0, x_limit[2]*0.96), ylim = c(y_limit[1], y.lim[2]), clip = "off")
      } else {
        p <- p + coord_cartesian(xlim = c(0, x_limit[2]*0.96), ylim = c(y_limit[1], y_limit[2]), clip = "off")
      }

      # annotate(
      #   "text",
      #   label = paste("t0:", flFitSpline$control$t0, "  min.growth:", flFitSpline$control$min.growth, "  smoothing:", flFitSpline$control$smooth.fl),
      #   x = 19,
      #   y = 0.1 * y_limit[2],
      #   angle = 90, parse = FALSE, size = basesize*3.2/12) +
      #   coord_cartesian(xlim = c(x_limit[1], x_limit[2]), clip = "off")

      # /// add tangent at maximum slope
      if(slope == TRUE && log.y == TRUE){
        max_slope     <- as.numeric(coef$max_slope[1])
        if(flFitSpline$fitFlag2){
          lagtime2 <- coef$lambda2
          growth.time <- flFitSpline$fit.x[which.max(flFitSpline$fit.fl)]
          max_slope2 <- coef$max_slope2
          if(lagtime2 < lagtime && lagtime2 > flFitSpline$raw.x[1]){
            # time values for tangent at mumax
            time_start.ndx <- which.min(abs(flFitSpline$fit.x-(coef$x.max-0.15*growth.time)))
            time_start <- flFitSpline$fit.x[time_start.ndx]
            time <- seq(time_start, max(flFitSpline$fit.x), length=200)
            # y values for tangent at mumax
            if(flFitSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (max_slope*time)
            }
            time <- time[bla >= 0.6* flFitSpline$fl.in[1]][bla <= 1.15 * max(df$fit.fl)]
            bla <- bla[bla >= 0.6* flFitSpline$fl.in[1]][bla <= 1.15 * max(df$fit.fl)]
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)
            # time values for tangent at mumax2
            time2.in <- seq(ifelse(lagtime2<0, 0, lagtime2), max(flFitSpline$"fit.x"), length=200)
            # y values for tangent at mumax
            if(flFitSpline$control$log.y.spline){
              bla2.in <- (exp(coef["b.tangent2"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope2*time2.in)
            } else {
              bla2.in <- coef["b.tangent2"][[1]] + (max_slope2*time2.in)
            }
            time2 <- time2.in[(bla2.in <= 1.15 * max(df$fit.fl)) & (time2.in <= (time2.in[coef$x.max2] + 0.15*max(flFitSpline$"fit.x")))]
            bla2 <- bla2.in[(bla2.in <= 1.15 * max(df$fit.fl)) & (time2.in <= (time2.in[coef$x.max2] + 0.15*max(flFitSpline$"fit.x")))]

            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)
            df.horizontal2 <- data.frame("time" = c(flFitSpline[["raw.x"]][1], lagtime2),
                                         "y" = if(flFitSpline$control$log.y.spline){
                                           exp(flFitSpline[["fit.fl"]][1])*flFitSpline[["fl.in"]][1]
                                         } else {
                                           flFitSpline[["fit.fl"]][1]
                                         })

            if(flFitSpline$control$log.y.spline){
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
            time <- seq(ifelse(lagtime<0, 0, lagtime), max(flFitSpline$"fit.x"), length=200)
            # y values for tangent at mumax
            if(flFitSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (max_slope*time)
            }
            time <- time[bla <= 1.15 * max(df$fit.fl)]
            bla <- bla[bla <= 1.15 * max(df$fit.fl)]
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)

            df.horizontal <- data.frame("time" = c(flFitSpline[["raw.x"]][1], lagtime),
                                        "y" = if(flFitSpline$control$log.y.spline){
                                          exp(flFitSpline[["fit.fl"]][1])*flFitSpline[["fl.in"]][1]
                                        } else {
                                          flFitSpline[["fit.fl"]][1]
                                        })

            # time values for tangent at mumax2
            time2_start.ndx <- which.min(abs(flFitSpline$fit.x-(coef$x.max2-0.15*growth.time)))
            time2_start <- flFitSpline$fit.x[time2_start.ndx]
            time2 <- seq(time2_start, max(flFitSpline$"fit.x"), length=200)
            # y values for tangent at mumax
            if(flFitSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope2*time2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (max_slope2*time2)
            }
            time2 <- time2[bla2 >= 0.6* flFitSpline$fl.in[1]][bla2 <= 1.15 * max(df$fit.fl)]
            bla2 <- bla2[bla2 >= 0.6* flFitSpline$fl.in[1]][bla2 <= 1.15 * max(df$fit.fl)]
            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)

            if(flFitSpline$control$log.y.spline){
              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla))], y = .data$y[which.min(abs(bla))],
                                        xend = .data$time[which.min(abs(.data$y - 2*y_limit[2]))],
                                        yend = .data$y[which.min(abs(.data$y - 2*y_limit[2]))]),
                                    data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = 0.9*lwd)

              p <- p + geom_segment(aes(x = .data$time[which.min(abs(bla2))], y = .data$y[which.min(abs(bla2))],
                                        xend = .data$time[which.min(abs(.data$y - 3.5*y_limit[2]))],
                                        yend = .data$y[which.min(abs(.data$y - 3.5*y_limit[2]))]),
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
        } # if(flFitSpline$fitFlag2)
        else {
          # time values for tangent
          time <- seq(ifelse(lagtime<0, 0, lagtime), max(flFitSpline$"fit.x"), length=200)
          # y values for tangent
          if(flFitSpline$control$log.y.spline){
            bla <- (exp(coef["b.tangent"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope*time)
          } else {
            bla <- coef["b.tangent"][[1]] + (max_slope*time)
          }
          time <- time[bla <= 1.15 * max(df$fit.fl[!is.na(df$fit.fl)])]
          bla <- bla[bla <= 1.15 * max(df$fit.fl[!is.na(df$fit.fl)])]

          tangent.df <- data.frame("time" = time,
                                   "y" = bla)
          df.horizontal <- data.frame("time" = c(flFitSpline[["raw.x"]][1], lagtime),
                                      "y" = if(flFitSpline$control$log.y.spline){
                                        exp(flFitSpline[["fit.fl"]][1])*flFitSpline[["fl.in"]][1]
                                      } else {
                                        flFitSpline[["fit.fl"]][1]
                                      })

          if(flFitSpline$control$log.y.spline){
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
        } # else of if(flFitSpline$fitFlag2)
      } # if(slope == TRUE && log.y == TRUE)

      # /// add tangent at maximum slope
      if(slope == TRUE && log.y == FALSE){
        max_slope     <- as.numeric(coef$max_slope[1])
        if(flFitSpline$fitFlag2){
          lagtime2 <- coef$lambda2
          growth.time <- flFitSpline$fit.x[which.max(flFitSpline$fit.fl)]
          max_slope2 <- coef$max_slope2
          if(lagtime2 < lagtime && lagtime2 > flFitSpline$raw.x[1]){
            # time values for tangent at mumax
            time_start.ndx <- which.min(abs(flFitSpline$fit.x-(coef$x.max-0.15*growth.time)))
            time_start <- flFitSpline$fit.x[time_start.ndx]
            time <- seq(time_start, max(flFitSpline$fit.x), length=200)
            # y values for tangent at mumax
            if(flFitSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (max_slope*time)
            }
            time <- time[bla >= 0.6* flFitSpline$fl.in[1]][bla <= 1.15 * max(df$fit.fl)]
            bla <- bla[bla >= 0.6* flFitSpline$fl.in[1]][bla <= 1.15 * max(df$fit.fl)]
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)
            # time values for tangent at mumax2
            time2.in <- seq(ifelse(lagtime2<0, 0, lagtime2), max(flFitSpline$"fit.x"), length=200)
            # y values for tangent at mumax
            if(flFitSpline$control$log.y.spline){
              bla2.in <- (exp(coef["b.tangent2"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope2*time2.in)
            } else {
              bla2.in <- coef["b.tangent2"][[1]] + (max_slope2*time2.in)
            }
            time2 <- time2.in[(bla2.in <= 1.15 * max(df$fit.fl)) & (time2.in <= (time2.in[coef$x.max2] + 0.15*max(flFitSpline$"fit.x")))]
            bla2 <- bla2.in[(bla2.in <= 1.15 * max(df$fit.fl)) & (time2.in <= (time2.in[coef$x.max2] + 0.15*max(flFitSpline$"fit.x")))]

            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)
            df.horizontal2 <- data.frame("time" = c(flFitSpline[["raw.x"]][1], lagtime2),
                                         "y" = if(flFitSpline$control$log.y.spline){
                                           exp(flFitSpline[["fit.fl"]][1])*flFitSpline[["fl.in"]][1]
                                         } else {
                                           flFitSpline[["fit.fl"]][1]
                                         })

            if(flFitSpline$control$log.y.spline){
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
            time <- seq(ifelse(lagtime<0, 0, lagtime), max(flFitSpline$"fit.x"), length=200)
            # y values for tangent at mumax
            if(flFitSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (max_slope*time)
            }
            time <- time[bla <= 1.15 * max(df$fit.fl)]
            bla <- bla[bla <= 1.15 * max(df$fit.fl)]
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)
            df.horizontal <- data.frame("time" = c(flFitSpline[["raw.x"]][1], lagtime),
                                        "y" = if(flFitSpline$control$log.y.spline){
                                          exp(flFitSpline[["fit.fl"]][1])*flFitSpline[["fl.in"]][1]
                                        } else {
                                          flFitSpline[["fit.fl"]][1]
                                        })
            # time values for tangent at mumax2
            time2_start.ndx <- which.min(abs(flFitSpline$fit.x-(coef$x.max2-0.15*growth.time)))
            time2_start <- flFitSpline$fit.x[time2_start.ndx]
            time2 <- seq(time2_start, max(flFitSpline$"fit.x"), length=200)
            # y values for tangent at mumax
            if(flFitSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope2*time2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (max_slope2*time2)
            }
            time2 <- time2[bla2 >= 0.6* flFitSpline$fl.in[1]][bla2 <= 1.15 * max(df$fit.fl)]
            bla2 <- bla2[bla2 >= 0.6* flFitSpline$fl.in[1]][bla2 <= 1.15 * max(df$fit.fl)]
            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)

            if(!flFitSpline$control$log.y.spline){
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

            if(!(lagtime <0)){
              p <- p + geom_segment(aes(x = .data$time[1], y = .data$y[1], xend = .data$time[2], yend = .data$y[2]), data = df.horizontal,
                                    linetype = "dashed", color = ggplot2::alpha(colSpline, 0.7), linewidth = lwd)
            }
          }
        } # if(flFitSpline$fitFlag2)
        else {
          # time values for tangent
          time <- seq(ifelse(lagtime<0, 0, lagtime), max(flFitSpline$"fit.x"), length=200)
          # y values for tangent
          if(flFitSpline$control$log.y.spline){
            bla <- (exp(coef["b.tangent"][[1]])*flFitSpline[["fl.in"]][1])*exp(max_slope*time)
          } else {
            bla <- coef["b.tangent"][[1]] + (max_slope*time)
          }
          time <- time[bla <= 1.15 * max(df$fit.fl[!is.na(df$fit.fl)])]
          bla <- bla[bla <= 1.15 * max(df$fit.fl[!is.na(df$fit.fl)])]
          tangent.df <- data.frame("time" = time,
                                   "y" = bla)
          df.horizontal <- data.frame("time" = c(flFitSpline[["raw.x"]][1], lagtime),
                                      "y" = if(flFitSpline$control$log.y.spline){
                                        exp(flFitSpline[["fit.fl"]][1])*flFitSpline[["fl.in"]][1]
                                      } else {
                                        flFitSpline[["fit.fl"]][1]
                                      })
          if(!flFitSpline$control$log.y.spline){
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
        } # else of if(flFitSpline$fitFlag2)
      } # if(slope == TRUE && log.y == TRUE)

      # /// add panel with growth rate over time
      if(deriv == TRUE){
        df.max_slope <- data.frame(spline(flFitSpline$spline.deriv1$x, flFitSpline$spline.deriv1$y))
        #add missing time values due to min.growth and t0
        df.max_slope <-
          dplyr::bind_rows(data.frame("x" = df$time[is.na(df$fit.fl)], "y" = rep(NA, length(df$time[is.na(df$fit.fl)]))),
                           df.max_slope)

        p.max_slope <- ggplot(df.max_slope, aes(x=.data$x, y=.data$y)) +
          geom_line(color = colSpline, linewidth = lwd) +
          theme_classic(base_size = basesize) +
          xlab(ifelse(is.null(x.title), x.label, x.title)) +
          ylab(ifelse(is.null(y.title.deriv), "Slope", y.title.deriv)) +
          coord_cartesian(xlim = c(0, x_limit[2]*0.95), clip = "off") +
          theme(
            panel.background = ggplot2::element_rect(fill='transparent'), #transparent panel bg
            plot.background = ggplot2::element_rect(fill='transparent', color=NA)
          )

        if(!is.null(y.lim.deriv)){
          p.max_slope <- p.max_slope + scale_y_continuous(limits = y.lim.deriv, breaks = scales::pretty_breaks(n = 6))
        } else {
          p.max_slope <- p.max_slope + scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

        }
        if(!is.null(x.lim)){
          p.max_slope <- p.max_slope + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
        } else {
          p.max_slope <- p.max_slope + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
        }

        p <- suppressWarnings(
          ggpubr::ggarrange(p, p.max_slope, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1))
        )
      }
    } #if(flFitSpline$fitFlag == TRUE)
    else {
      if(flFitSpline$control$log.y.spline == TRUE){
        df <- data.frame("time" = flFitSpline[["raw.x"]],
                         "data" = exp(flFitSpline[["raw.fl"]])*flFitSpline[["fl.in"]][1]
        )
      } else{
        df <- data.frame("time" = flFitSpline[["raw.x"]],
                         "data" = flFitSpline[["raw.fl"]]
        )
      }


      p <- ggplot(df, aes(x=.data$time, y=.data$data)) +
        geom_point(shape=pch, size = cex.point,alpha = 0.6, stroke=0.15*cex.point, color = colData) +
        xlab(ifelse(is.null(x.title), x.label, x.title)) +
        ylab(ifelse(is.null(y.title), y.label, y.title)) +
        theme_classic(base_size = basesize) +
        ggtitle(gsub(" \\| NA", "", paste(flFitSpline$gcID, collapse=" | "))) +
        theme(legend.key = element_blank(),
              legend.background=element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.90, 0.08),
              plot.title = element_text(size = basesize*1.1, face = "bold", vjust = 3),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = unit(c(1,2,1,1), "lines")) +
        scale_color_manual(name='Fluorescence fit',
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
          label = paste("t0:", flFitSpline$control$t0, "  min.growth:", flFitSpline$control$min.growth, "  smoothing:", flFitSpline$control$smooth.fl),
          x = 0.5*x_limit[2],
          y = 1.3 * y_limit[2],
          angle = 0, parse = FALSE, size = basesize*3.2/12) +
        coord_cartesian(xlim = c(0, x_limit[2]*0.95), ylim = c(y_limit[1], y_limit[2]), clip = "off")


      if(log.y == TRUE){
        if(!is.null(y.lim)){
          p <- p + scale_y_continuous(limits = y.lim, breaks = base_breaks(n = n.ybreaks), trans = 'log')
        } else {
          p <- p + scale_y_continuous(breaks = base_breaks(n = n.ybreaks), trans = 'log')
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
        grDevices::png(paste0(out.dir, "/", paste(flFitSpline$ID, collapse = "_"), "_SplineFit.png"),
                       width = w, height = h, units = 'in', res = 300)
        print(p)
        grDevices::dev.off()
        if (requireNamespace("Cairo", quietly = TRUE)) {
          Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(flFitSpline$ID, collapse = "_"), "_SplineFit.pdf"))
        } else {
          message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
          grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", paste(flFitSpline$ID, collapse = "_"), "_SplineFit.pdf"))
        }
        print(p)
        grDevices::dev.off()
      }
      if (plot == TRUE){
        print(p)
      } else {
        invisible(p)
      }
    } # else of if (add == TRUE)

}


#' Generic plot function for \code{flBootSpline} objects.
#'
#' @param x Object of class \code{flBootSpline}, created with \code{\link{flBootSpline}}.
#' @param pch (Numeric) Size of the raw data circles.
#' @param deriv (Logical) Show the derivatives (i.e., slope) over time in a secondary plot (\code{TRUE}) or not (\code{FALSE}).
#' @param colData (Numeric or Character) Color used to plot the raw data.
#' @param colSpline (Numeric or Character) Color used to plot the splines.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param cex.lab (Numeric) Font size of axis titles.
#' @param cex.axis (Numeric) Font size of axis annotations.
#' @param lwd (Numeric) Spline line width.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on y-axis of the fluorescence curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis of both fluorescence curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim.deriv (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis of the derivative plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param combine (Logical) Indicate whether both growth curves and parameter plots shall be shown within the same window.
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @export plot.flBootSpline
#' @export
#'
#' @return A single plot with the all spline fits from the bootstrapping operation and statistical distribution of parameters if \code{combine = TRUE} or separate plots for fits and parameter distributions (if \code{combine = FALSE}).
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
#'                    data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
#'                    csvsep = "\t",
#'                    csvsep.fl = "\t")
#'
#' # Extract time and normalized fluorescence data for single sample
#' time <- input$time[4,]
#' data <- input$norm.fluorescence[4,-(1:3)] # Remove identifier columns
#'
#' # Perform linear fit
#' TestFit <- flBootSpline(time = time,
#'                        fl_data = data,
#'                        ID = "TestFit",
#'                        control = fl.control(fit.opt = "s", x_type = "time",
#'                        nboot.fl = 50))
#'
#' plot(TestFit, combine = TRUE, lwd = 0.5)
#
plot.flBootSpline <- function(x, pch = 1, colData=1, deriv = TRUE,
                              colSpline = "dodgerblue3",
                              cex.point = 1, cex.lab = 1.5, cex.axis = 1.3,
                              lwd = 2, y.lim = NULL, x.lim = NULL, y.lim.deriv = NULL,
                              plot = TRUE, export = FALSE,
                              height = 7, width = 9, out.dir = NULL, combine = FALSE, ...)
{
  flBootSpline <- x
  if(!is.null(colSpline))
    colSpline <- toupper(colSpline)
  if(!is.null(colData))
    colData <- toupper(colData)

  colSpline <- ggplot2::alpha(colSpline, 0.2)
  # flBootSpline an object of class flBootSpline
  if(methods::is(flBootSpline) != "flBootSpline") stop("x needs to be an object created with flBootSpline().")
  # /// initialize "Empty Plot" function
  empty.plot <- function(text="Empty plot",main=""){
    plot(c(0,1,0,1,0),c(0,1,1,0,0), type="l", axes=FALSE, xlab="", ylab="", lwd=lwd, col="gray",main=main)
    lines(c(0,0),c(0,1), type="l", lwd=lwd, col="gray")
    lines(c(1,1),c(1,0), type="l", lwd=lwd, col="gray")
    text(0.5,0.1,text, col="gray")
  }

  # /// check input parameters
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (is.numeric(cex.point)==FALSE)   stop("Need numeric value for: cex.point")
  if (flBootSpline$bootFlag==FALSE){
    message("Could not find successful bootstrapping operations for the provided flBootSpline object. Did you define 'nboot.fl' in the control object when running computations?")
    empty.plot()
  }
  else{

    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))

    p1 <- function()
    {
      par(cex.lab = cex.lab, cex.axis = cex.axis)
      par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), mgp=c(3, 1, 0), las=0)

      fit.log.x     <- flBootSpline$control$log.x.spline
      fit.log.y     <- flBootSpline$control$log.y.spline

      global.minx <- min(min(flBootSpline$boot.x,na.rm=TRUE),na.rm=TRUE)
      global.maxx <- max(max(flBootSpline$boot.x,na.rm=TRUE),na.rm=TRUE)
      global.miny <- min(min(flBootSpline$boot.fl,na.rm=TRUE),na.rm=TRUE)
      global.maxy <- max(max(flBootSpline$boot.fl,na.rm=TRUE),na.rm=TRUE)

      # initialize plot
      if(deriv == TRUE){
        layout(mat = matrix(c(1, 2), nrow = 2, ncol = 1),
               heights = c(2, 1.3), # Heights of the two rows
               widths = c(1, 1)) # Widths of the two columns
        par(mai=c(0.35 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.5,0))
        plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim, xaxt="n")
      } else {
        par(mai=c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.5,0))
        plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim, xaxt="n")
      }

      # /// plot data
      points(flBootSpline$raw.x, flBootSpline$raw.fl, col=colData, pch=pch, cex=cex.point)

      # /// plot all flFitSpline objects
      for(i in 1:flBootSpline$control$nboot.fl){
        plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = TRUE, lwd = lwd,
                         deriv = FALSE, plot = FALSE, export = FALSE, pch=0, colSpline=colSpline, cex.point = cex.point)
      }
      # add plot title
      title(paste(flBootSpline$gcID, collapse = "_"), line = ifelse(deriv==T, 0.8, 1), cex.main = cex.lab)
      #add axis titles
      if (fit.log.y==FALSE){
        title(ylab = "Fluorescence", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      else if (fit.log.y==TRUE){
        title(ylab = "Fluorescence [Ln(y(t)/y0)]", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      # add second plot with slope over time
      if(deriv == TRUE){
        par(cex.axis = cex.axis)
        par(mai=c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.2,0))
        y.max <- ceiling(max(unlist(lapply(1:length(flBootSpline$boot.flSpline), function(x) max(flBootSpline$boot.flSpline[[x]]$spline.deriv1$y))))*10)/10
        y.min <- floor(min(unlist(lapply(1:length(flBootSpline$boot.flSpline), function(x) min(flBootSpline$boot.flSpline[[x]]$spline.deriv1$y))))*10)/10
        if(is.null(y.lim.deriv)){
          y.lim.deriv <- c(y.min, y.max)
        }
        if ((flBootSpline$control$log.x.spline==FALSE)){
          try( plot(flBootSpline$boot.flSpline[[1]]$spline.deriv1$x, flBootSpline$boot.flSpline[[1]]$spline.deriv1$y,
                    xlab="", ylab="", type = "l", lwd = lwd, col = colSpline, ylim = y.lim.deriv, xlim = x.lim, xaxt = "n") )
        }
        if ((flBootSpline$control$log.x.spline==TRUE)){
          try( lines(flBootSpline$boot.flSpline[[1]]$x, flBootSpline$boot.flSpline[[1]]$spline.deriv1$y, lwd = lwd, xlab="Ln(1+time)", ylab="First derivative", type = "l") )
        }
        for(i in 2:flBootSpline$control$nboot.fl){
          plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = FALSE, lwd = lwd, xlim = x.lim,
                           deriv = TRUE, plot = FALSE, export = FALSE, pch=0, colSpline=colSpline, cex.point = cex.point)
        }
        title(ylab = "First derivative", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      if(flBootSpline$control$x_type == "growth"){
        if (fit.log.x==TRUE){
          title(xlab = "Density [Ln(x(t)/x0)]", line = 1+0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        }
        else if(fit.log.x==FALSE){
          title(xlab = "Density", line = 1+0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        }
      } else {
        if (fit.log.x==TRUE){
          title(xlab = "Ln(1+time)", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        }
        else if(fit.log.x==FALSE){
          title(xlab = "Time", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        }
      }
      axis(1, mgp=c(3,1+0.5*cex.axis,0))

      par(mfrow=c(1,1))
    } # p1 <- function()
    p2 <- function()
    {
      lambda    <- flBootSpline$lambda
      max_slope <- flBootSpline$max_slope
      dY         <- flBootSpline$dY
      integral  <- flBootSpline$integral

      # /// plot histograms of growth parameters
      par(mfrow=c(2,2))
      if (sum(!is.na(lambda))>1){
        try(hist(lambda, col="gray",main=expression(bold(lambda)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "lambda", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else{ empty.plot("Empty plot!") }
      if (sum(!is.na(max_slope))>1){
        try(hist(max_slope , col="gray", main=expression(bold(max_slope)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "max_slope", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else { empty.plot("Empty plot!", main=expression(max_slope)) }
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
      graphics::mtext(paste(flBootSpline$gcID, collapse = "_"), side = 3, line = -1, outer = TRUE)
      par(mfrow=c(1,1))
      par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    } # p2 <- function()
    p3 <- function()
    {
      if(deriv) layout(matrix(c(1,2,3,5,1,2,4,6), nrow = 4, ncol = 2))
      else layout(matrix(c(1,2,4,1,3,5), nrow = 3, ncol = 2))

      par(cex.lab = cex.lab, cex.axis = cex.axis)
      par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), mai = c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.7 + 0.2*cex.lab + 0.2*cex.axis, 0.5, 0.3), mgp=c(3, 1, 0), las=0)

      fit.log.x     <- flBootSpline$control$log.x.spline
      fit.log.y     <- flBootSpline$control$log.y.spline

      global.minx <- min(min(flBootSpline$boot.x,na.rm=TRUE),na.rm=TRUE)
      global.maxx <- max(max(flBootSpline$boot.x,na.rm=TRUE),na.rm=TRUE)
      global.miny <- min(min(flBootSpline$boot.fl,na.rm=TRUE),na.rm=TRUE)
      global.maxy <- max(max(flBootSpline$boot.fl,na.rm=TRUE),na.rm=TRUE)

      # initialize plot
      if(deriv == TRUE){
        par(mai=c(0.35 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.5,0))
        plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim, xaxt="n")
      } else {
        par(mai=c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.5,0))
        plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim, xaxt="n")
      }

      # /// plot data
      points(flBootSpline$raw.x, flBootSpline$raw.fl, col=colData, pch=pch, cex=cex.point)

      # /// plot all flFitSpline objects
      for(i in 1:flBootSpline$control$nboot.fl){
        plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = TRUE, lwd = lwd,
                         deriv = FALSE, plot = FALSE, export = FALSE, pch=0, colSpline=colSpline, cex.point = cex.point)
      }
      # add plot title
      title(paste(flBootSpline$gcID, collapse = "_"), line = ifelse(deriv==T, 0.8, 1), cex.main = cex.lab)
      #add axis titles
      if (fit.log.y==FALSE){
        title(ylab = "Fluorescence", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      else if (fit.log.y==TRUE){
        title(ylab = "Fluorescence [Ln(y(t)/y0)]", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      # add second plot with slope over time
      if(deriv == TRUE){
        par(cex.axis = cex.axis)
        par(mai=c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.2,0))
        y.max <- ceiling(max(unlist(lapply(1:length(flBootSpline$boot.flSpline), function(x) max(flBootSpline$boot.flSpline[[x]]$spline.deriv1$y))))*10)/10
        y.min <- floor(min(unlist(lapply(1:length(flBootSpline$boot.flSpline), function(x) min(flBootSpline$boot.flSpline[[x]]$spline.deriv1$y))))*10)/10
        if(is.null(y.lim.deriv)){
          y.lim.deriv <- c(y.min, y.max)
        }
        if ((flBootSpline$control$log.x.spline==FALSE)){
          try( plot(flBootSpline$boot.flSpline[[1]]$spline.deriv1$x, flBootSpline$boot.flSpline[[1]]$spline.deriv1$y,
                    xlab="", ylab="", type = "l", lwd = lwd, col = colSpline, ylim = y.lim.deriv, xlim = x.lim, xaxt = "n" ) )
        }
        if ((flBootSpline$control$log.x.spline==TRUE)){
          try( lines(flBootSpline$boot.flSpline[[1]]$x, flBootSpline$boot.flSpline[[1]]$spline.deriv1$y, lwd = lwd, xlab="Ln(1+time)", ylab="First derivative", type = "l") )
        }
        for(i in 2:flBootSpline$control$nboot.fl){
          plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = FALSE, lwd = lwd, xlim = x.lim,
                           deriv = TRUE, plot = FALSE, export = FALSE, pch=0, colSpline=colSpline, cex.point = cex.point)
        }
        title(ylab = "First derivative", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      if(flBootSpline$control$x_type == "growth"){
        if (fit.log.x==TRUE){
          title(xlab = "Density [Ln(x(t)/x0)]", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        }
        else if(fit.log.x==FALSE){
          title(xlab = "Density", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        }
      } else {
        if (fit.log.x==TRUE){
          title(xlab = "Ln(1+time)", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        }
        else if(fit.log.x==FALSE){
          title(xlab = "Time", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        }
      }
      axis(1, mgp=c(3,1+0.5*cex.axis,0))

      lambda    <- flBootSpline$lambda
      max_slope <- flBootSpline$max_slope
      dY         <- flBootSpline$dY
      integral  <- flBootSpline$integral

      # /// plot histograms of growth parameters
      par(mai=c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.2*cex.lab + 0.2*cex.axis,0.2*cex.lab,0))

      if (sum(!is.na(lambda))>1){
        try(hist(lambda, col="gray",main=expression(bold(lambda)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "lambda", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else{ empty.plot("Empty plot!") }
      if (sum(!is.na(max_slope))>1){
        try(hist(max_slope , col="gray", main=expression(bold(max_slope)), cex.main = cex.lab, cex.axis = cex.axis, xaxt = "n",xlab="",ylab=""))
        title(xlab = "max_slope", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
        title(ylab = "Frequency", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
        axis(1, mgp=c(3,1+0.5*cex.axis,0))
      } else { empty.plot("Empty plot!", main=expression(max_slope)) }
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
      graphics::mtext(paste(flBootSpline$gcID, collapse = "_"), side = 3, line = -1, outer = TRUE)
      par(mfrow=c(1,1))
      par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    } # p3 <- function()
  }
  if (export == TRUE && flBootSpline$bootFlag==TRUE){
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = FALSE)
    if(!combine){
      w1 <- width
      h1 <- height

      grDevices::png(paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSpline.png"),
                     width = w1, height = h1, units = 'in', res = 300)
      p1()
      grDevices::dev.off()
      if (requireNamespace("Cairo", quietly = TRUE)) {
        Cairo::CairoPDF(width = w1, height = h1, file = paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSpline.pdf"))
      } else {
        message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
        grDevices::pdf(width = w1, height = h1, file = paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSpline.pdf"))
      }
      p1()
      grDevices::dev.off()

      w2 <- width
      h2 <- width
      grDevices::png(paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSplineParam.png"),
                     width = w2, height = h2, units = 'in', res = 300)
      p2()
      grDevices::dev.off()
      if (requireNamespace("Cairo", quietly = TRUE)) {
        Cairo::CairoPDF(width = w2, height = h2, file = paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSplineParam.pdf"))
      } else {
        message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
        grDevices::pdf(width = w2, height = h2, file = paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSplineParam.pdf"))
      }
      p2()
      grDevices::dev.off()
    } else {
      w <- width
      h <- height

      grDevices::png(paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSpline.png"),
                     width = w, height = h, units = 'in', res = 300)
      p1()
      grDevices::dev.off()
      if (requireNamespace("Cairo", quietly = TRUE)) {
        Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSpline.pdf"))
      } else {
        message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
        grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSpline.pdf"))
      }
      p1()
      grDevices::dev.off()
    }
  }

  if (plot == TRUE && flBootSpline$bootFlag==TRUE){
    if(!combine){
      p1()
      dev.new()
      p2()
    } else {
      p3()
    }
  }
}

#' Generic plot function for \code{drFitFLModel} objects.
#'
#' @param x Object of class \code{drFitFLModel}, created with \code{\link{fl.drFitModel}}.
#' @param ec50line (Logical) Show pointed horizontal and vertical lines at the EC50 value (\code{TRUE}) or not (\code{FALSE}).
#' @param broken (Logical) If TRUE the x axis is broken provided this axis is logarithmic (using functionality in the CRAN package 'plotrix').
#' @param bp (Numeric) Specifying the break point below which the dose is zero (the amount of stretching on the dose axis above zero in order to create the visual illusion of a logarithmic scale including 0). The default is the base-10 value corresponding to the rounded value of the minimum of the log10 values of all positive dose values. This argument is only working for logarithmic dose axes.
#' @param log (Character) String which contains '"x"' if the x axis is to be logarithmic, '"y"' if the y axis is to be logarithmic and '"xy"' or '"yx"' if both axes are to be logarithmic. The default is "x". The empty string "" yields the original axes.
#' @param pch (Numeric) Symbol used to plot data points.
#' @param n.xbreaks (Numeric) Number of breaks on the x-axis (if not log-transformed). The breaks are generated using \code{pretty}. Thus, the final number of breaks can deviate from the user input.
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis (if not log-transformed). The breaks are generated using \code{pretty}. Thus, the final number of breaks can deviate from the user input.#' @param pch (Numeric) Size of the raw data circles.
#' @param colData (Numeric or Character) Color used to plot the raw data.
#' @param colSpline (Numeric or Character) Color used to plot the splines.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param cex.lab (Numeric) Font size of axis titles.
#' @param cex.axis (Numeric) Font size of axis annotations.
#' @param lwd (Numeric) Line width.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on y-axis as a vector in the form \code{c(l, u)}.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis as a vector in the form \code{c(l, u)}.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Further arguments to refine the generated base R plot.
#'
#' @export plot.drFitFLModel
#' @export
#'
#' @return A plot with the biosensor dose-response model fit.
#'
#' @examples
#' # Create concentration values via a serial dilution
#' conc <- c(0, rev(unlist(lapply(1:18, function(x) 10*(2/3)^x))),10)
#'
#' # Simulate response values via biosensor equation
#' response <- biosensor.eq(conc, y.min = 110, y.max = 6000, K = 0.5, n = 2) +
#'             0.01*6000*rnorm(10)
#'
#' # Perform fit
#' TestRun <- fl.drFitModel(conc, response, drID = "test", control = fl.control())
#'
#' print(summary(TestRun))
#' plot(TestRun)
#'
plot.drFitFLModel <- function(x, ec50line = TRUE, broken = TRUE,
                              bp, n.xbreaks, n.ybreaks, log = c("xy"), pch = 1,
                              colSpline = 1, colData = 1, cex.point = 1, cex.lab = 1.5,
                              cex.axis = 1.3, y.lim = NULL, x.lim = NULL,
                              lwd = 2, plot = TRUE, export = FALSE,
                              height = 7, width = 9, out.dir = NULL,
                              ...)
{
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  drFittedFLModel <- x
  if(!is.null(colSpline))
    colSpline <- toupper(colSpline)
  if(!is.null(colData))
    colData <- toupper(colData)

  # drFittedFLModel an object of class drFittedFLModel
  if(methods::is(drFittedFLModel) != "drFitFLModel") stop("x needs to be an object created with fl.drFitModel().")
  # /// check input parameters
  if (is.logical(ec50line) == FALSE)
    stop("Need logical value for: ec50line")
  if (is.numeric(pch) == FALSE)
    stop("Need numeric value for: pch")
  if (is.numeric(cex.point) == FALSE)
    stop("Need numeric value for: cex.point")
  conc <- drFittedFLModel$raw.conc
  test <- drFittedFLModel$raw.test
  bp <- ifelse(missing(bp)||!exists("bp")||bp == "", rlang::missing_arg(), bp)
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
      if(xt[1] == bp){
        xt[1] <- 0.1*bp
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
  log10cly <- round(log10(0.5*min(drFittedFLModel$fit.test[drFittedFLModel$fit.test > 0]))) + 1
  bpy <- 10^(log10cly)
  if(missing(n.ybreaks)){
    yt <- NULL
    if(any(grep("y", log))){
      if(min(test) <= 0){
        yt <- xgx_breaks_log10(c(bpy, max(test)))
        yt.minor <- xgx_minor_breaks_log10(c(bpy, max(test)))[-1]
      }
      else{
        yt <- xgx_breaks_log10(c(min(test), max(test)))
        yt.minor <- xgx_minor_breaks_log10(c(min(test), max(test)))
      }
      if(yt[1] == bpy){
        yt[1] <- 0.1*bpy
      }
    }
  } else {
    if(any(grep("x", log))){
      if(min(test) <= 0){
        yt <- xgx_breaks_log10(c(bpy, max(test)))
        yt.minor <- xgx_minor_breaks_log10(c(bpy, max(test)))[-1]
      }
      else{
        yt <- xgx_breaks_log10(c(min(test), max(test)))
        yt.minor <- xgx_minor_breaks_log10(c(min(test), max(test)))
      }
    } else {
      if(is.null(n.ybreaks)){
        yt <- pretty(test)
      } else {
        yt <- pretty(test, n.ybreaks)
      }
    }
  }
  x.lim <- if(x.lim == "" || is.null(x.lim) || any(is.na(x.lim)) ){
    NULL
  } else {
    x.lim
  }
  y.lim <- if(y.lim == "" || is.null(y.lim) || any(is.na(x.lim)) ){
    NULL
  }else {
    y.lim
  }
  p <- function(){
    par(mar=c(5.1+cex.lab, 4.1+cex.lab+0.5*cex.axis, 4.1, 3.1), cex.lab = cex.lab, cex.axis = cex.axis)
    if ((drFittedFLModel$control$log.x.dr == TRUE) && (drFittedFLModel$control$log.y.dr == TRUE)) {
      plot(
        log(conc + 1),
        log(drFittedFLModel$raw.test + 1),
        log = log,
        col = colData,
        xlab = "ln(1+concentration)",
        ylab = "ln(1+response)",
        type = "n", xlim = x.lim, ylim = y.lim, ...
      )
      points(log(conc + 1), log(drFittedFLModel$raw.test + 1), cex = cex.point, pch = pch)
    }
    else
    {
      if ((drFittedFLModel$control$log.x.dr == FALSE) && (drFittedFLModel$control$log.y.dr == TRUE)) {
        plot(
          conc,
          log(drFittedFLModel$raw.test + 1),
          log = log,
          col = colData,
          xlab = "concentration",
          ylab = "ln(1+response)",
          type = "n", xlim = x.lim, ylim = y.lim, ...
        )
        points(conc, log(drFittedFLModel$raw.test + 1), cex = cex.point, pch = pch)
      }
      else
      {
        if ((drFittedFLModel$control$log.x.dr == TRUE) && (drFittedFLModel$control$log.y.dr == FALSE)) {
          plot(
            log(conc + 1),
            drFittedFLModel$raw.test,
            log = log,
            col = colData,
            xlab = "Ln(1+concentration)",
            ylab = paste0("Response", ifelse(!is.na(drFittedFLModel$parameters$test), paste0(" (", drFittedFLModel$parameters$test, ")"), "")),
            type = "n", xlim = x.lim, ylim = y.lim, ...
          )
          points(log(conc + 1), drFittedFLModel$raw.test, cex = cex.point, pch = pch)
        }
        else
        {
          if ((drFittedFLModel$control$log.x.dr == FALSE) && (drFittedFLModel$control$log.y.dr == FALSE)) {
            if(any(grep("x", log))){
              conc[conc==0] <- 0.5* bp
            }
            mean <- sapply(1:length(conc), function(x) mean(drFittedFLModel$raw.test[conc==unique(conc)[x]]))
            mean <- mean[!is.na(mean)]
            if(any(grep("y", log))){
              mean[mean==0] <- 0.5*drFittedFLModel$fit.test[2]
            }
            sd <- sapply(1:length(conc), function(x) sd(drFittedFLModel$raw.test[conc==unique(conc)[x]]))
            sd <- sd[!is.na(sd)]
            plot(conc,
                 mean,
                 log = log,
                 col = colData,
                 type = "n",
                 xaxt = "n",
                 yaxt = "n",
                 xlab = "",
                 ylab = "",
                 xlim = x.lim,
                 ylim = y.lim,
                 ...
            )
            points(unique(conc)[order(unique(conc))], mean, cex = cex.point, pch = pch)
            if(length(sd)>0){
              try(arrows(x0=unique(conc)[order(unique(conc))], y0=mean-sd,
                         x1=unique(conc)[order(unique(conc))], y1=mean+sd, code=3, angle=90, length=0.1), silent = TRUE)
            }
            if(broken && min(drFittedFLModel$raw.conc) <= 0)
              plotrix::axis.break(1, bp)
            if(broken && min(drFittedFLModel$raw.test) <= 0)
              plotrix::axis.break(2, bpy)

            if(min(drFittedFLModel$raw.conc) <= 0)
              axis(1, lwd = 0, lwd.ticks = 1, at = c(0.5*bp,xt), mgp=c(3,0.5+0.5*cex.axis,0), labels = c("0", as.character(as.numeric(xt))))
            else
              axis(1, lwd = 0, lwd.ticks = 1, at = xt, mgp=c(3,0.5+0.5*cex.axis,0), labels = as.character(as.numeric(xt)))

            if(min(drFittedFLModel$raw.test) <= 0)
              axis(2, lwd = 0, lwd.ticks = 1, at = yt, las=1, line = 0, labels = c("0", as.character(as.numeric(yt[-1]))))
            else
              axis(2, lwd = 0, lwd.ticks = 1, at = yt, las=1, line = 0, labels = as.character(as.numeric(yt)))

            title(ylab = paste0("Response", ifelse(!is.na(drFittedFLModel$parameters$test), paste0(" (", drFittedFLModel$parameters$test, ")"), "")),
                  line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
            title(xlab = "Concentration", line = 1 + 0.7*cex.lab + 0.7*cex.axis, cex.lab = cex.lab)
            if(any(grep("x", log))){
              axis(side=1, lwd = 0, lwd.ticks = 1, at=xt.minor, las=0, tck=-0.01, labels=FALSE, line = 0)
            }
            if(any(grep("y", log))){
              axis(side=2, lwd = 0, lwd.ticks = 1, at=yt.minor, las=0, tck=-0.01, labels=FALSE, line = 0)
            }

            points(unique(conc)[order(unique(conc))], mean, cex = cex.point, pch = pch)
            if(length(sd)>0){
              try(arrows(x0=unique(conc)[order(unique(conc))], y0=mean-sd,
                         x1=unique(conc)[order(unique(conc))], y1=mean+sd, code=3, angle=90, length=0.1), silent = TRUE)
            }
            graphics::mtext(bquote(K: ~ .(signif(x$parameters[["K.orig"]][[1]], digits = 3)) ~~~~
                                     fc: ~ .(round(x$parameters[["fc"]], digits = 2)) ~~~~
                                     n: ~ .(round(x$parameters[["n"]], digits = 3)) ~~~~
                                     y[min]: ~ .(round(x$parameters[["y.min"]], digits = 0)) ~~~~
                              y[max]: ~ .(round(x$parameters[["y.max"]], digits = 0))),
                            side = 4 , adj = 0.55, line = -2.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab*0.8)
          }
        }
      }
    }

    try(lines(
      x = if(min(drFittedFLModel$fit.conc) <=0){
        c(0.5* bp, drFittedFLModel$fit.conc[-1])
      }else{
        drFittedFLModel$fit.conc
      },
      y = if(min(drFittedFLModel$fit.test) <=0){
        c(0.5*drFittedFLModel$fit.test[2], drFittedFLModel$fit.test[-1])
      }else{
        drFittedFLModel$fit.test
      },
      lwd = lwd,
      col = colSpline
    ), silent = FALSE)

    if (ec50line == TRUE) {
      #vertical lines
      totmin = min(min(drFittedFLModel$fit.conc), min(drFittedFLModel$fit.test))
      lines(c(drFittedFLModel$parameters$K, drFittedFLModel$parameters$K),
            c(if(min(drFittedFLModel$fit.test) <=0){
              0.1*drFittedFLModel$fit.test[2]
            }else{
              1}, drFittedFLModel$parameters$yEC50),
            lty = 2, lwd = lwd)
      #horizontal
      lines(c(ifelse(any(grep("x", log)), conc[1], -1), drFittedFLModel$parameters$K),
            c(drFittedFLModel$parameters$yEC50, drFittedFLModel$parameters$yEC50),
            lty = 2, lwd = lwd)
    }
    title(main = drFittedFLModel$drID)
  } # p <- function()
  if (export == TRUE){
    w <- width
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = FALSE)
    grDevices::png(paste0(out.dir, "/", paste(drFittedFLModel$drID, collapse = "_"), "_drFitFLModel.png"),
                   width = w, height = h, units = 'in', res = 300)
    p()
    grDevices::dev.off()
    if (requireNamespace("Cairo", quietly = TRUE)) {
      Cairo::CairoPDF(width = w, height = h, file = paste0(out.dir, "/", paste(drFittedFLModel$drID, collapse = "_"), "_drFitFLModel.pdf"))
    } else {
      message("Package 'Cairo' must be installed to preserve special characters in the exported PDF image")
      grDevices::pdf(width = w, height = h, file = paste0(out.dir, "/", paste(drFittedFLModel$drID, collapse = "_"), "_drFitFLModel.pdf"))
    }
    p()
    grDevices::dev.off()
  }

  if (plot == TRUE){
    if(drFittedFLModel$fitFlag == TRUE) p()
    else stop(paste0("Model could not be fitted to the data for: ", drFittedFLModel$drID, "."))
  }
}

#' Combine different groups of samples into a single plot
#'
#' Visualize fluorescence, normalized fluorescence, or spline fits of multiple sample groups in a single plot.
#'
#' @param x A \code{flFitRes}, \code{flFit}, or \code{grodata} object created with \code{\link{fl.workflow}} containing fluorescence data.
#' @param data.type (Character) Indicate, which type of fluorescence data should be displayed.
#' @param IDs (String or vector of strings) Define samples or groups (if \code{mean = TRUE}) to combine into a single plot based on exact matches with entries in the \code{label} or \code{condition} columns of \code{grofit$expdesign}.
#' @param names (String or vector of strings) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered. Note: Ensure to use unique concentration values to extract groups of interest. If the concentration value of one condition is included in its entirety within the name of other conditions (e.g., the dataset contains '1', '10', and '100', \code{code = 10} will select both '10 and '100'), it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define groups to exclude from the plot. Partial matches with sample/group names are accepted.
#' @param exclude.conc (Numeric or numeric vector) Define concentrations to exclude from the plot.
#' @param mean (Logical) Display the mean and standard deviation of groups with replicates (\code{TRUE}) or plot each sample individually (\code{FALSE})?
#' @param log.y (Logical) Log-transform the y-axis of the plot (\code{TRUE}) or not (\code{FALSE})?
#' @param deriv (Logical) Show derivatives over time in a separate panel below the plot (\code{TRUE}) or not (\code{FALSE})?
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{axisTicks()}. Thus, the final number of breaks can deviate from the user input.
#' @param colors (vector of strings) Define a color palette used to draw the plots. If \code{NULL}, default palettes are chosen based on the number of groups/samples within the plot. Note: The number of provided colors should at least match the number of groups/samples.
#' @param color_groups (Logical) Shall samples within the same group but with different concentrations be shown in different shades of the same color?
#' @param group_pals (String vector) Define the colors used to display sample groups with identical concentrations. The number of selected color palettes must be at least the number of displayed groups. The order of the chosen palettes corresponds to the oder of conditions in the legend. Available options: "Green", "Oranges", "Purple", "Cyan", "Grey", "Red", "Blue", and "Magenta".
#' @param basesize (Numeric) Base font size.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the fluorescence curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the x-axis of both fluorescence curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.title (Character) Optional: Provide a title for the y-axis of the fluorescence curve plot.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both fluorescence curve and derivative plots.
#' @param y.lim.deriv (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis of the derivative plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
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
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @export plot.flFitRes
#' @export
#'
#' @return A plot with all curves (nonparametric fits, raw fluorescence measurements, or raw normalized fluorescence over time) in a \code{flFitRes} object created with \code{\link{fl.workflow}}, with replicates combined by the group averages (if \code{mean = TRUE}) or not (\code{mean = FALSE}).
#'
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggtitle labs guides
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab xlim ylim
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
#' res <- fl.workflow(grodata = input, ec50 = FALSE, fit.opt = "s",
#'                    x_type = "time", norm_fl = TRUE,
#'                    dr.parameter = "max_slope.spline",
#'                    suppress.messages = TRUE,
#'                    parallelize = FALSE)
#'
#' plot(res, legend.ncol = 3, basesize = 15)
#' }
#'
plot.flFitRes <-  function(x,
                        data.type = c("spline", "raw", "norm.fl"),
                        IDs = NULL,
                        names = NULL,
                        conc = NULL,
                        mean = TRUE,
                        exclude.nm = NULL,
                        exclude.conc = NULL,
                        log.y = FALSE,
                        deriv = FALSE,
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
                        out.nm = NULL,
                        ...
)
{
  object <- x
  if(!is.null(colors))
    colSpline <- toupper(colors)

  # Convert range  and selecting arguments
  names <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", names)), pattern = ";"))
  conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", conc)), pattern = "[;,]"))
  exclude.nm <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.nm)), pattern = ";"))
  exclude.conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.conc)), pattern = ";"))
  x.lim <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", x.lim)), pattern = ";"))
  y.lim <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", y.lim)), pattern = ";"))
  y.lim.deriv <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", y.lim.deriv)), pattern = ";"))
  suppressWarnings(assign("x.lim" ,as.numeric(x.lim)))
  if(all(is.na(x.lim))) x.lim <- NULL
  suppressWarnings(assign("y.lim" ,as.numeric(y.lim)))
  if(all(is.na(y.lim))) y.lim <- NULL
  suppressWarnings(assign("y.lim.deriv" ,as.numeric(y.lim.deriv)))
  if(all(is.na(y.lim.deriv))) y.lim.deriv <- NULL

  if(!any(methods::is(object) %in% c("flFit","flFitRes", "grodata"))) stop("'x' needs to be an object created with fl.workflow(), flFit(), parse_data(), or read_data().")
  if(methods::is(object) == "grodata" && !any(data.type %in% c("raw", "norm.fl"))) stop("Raw input data can only be used to visualize data.type 'raw', or 'norm.fl'.")

  data.type <- match.arg(data.type)
  if(data.type == "raw" || data.type == "norm.fl"  && deriv ==TRUE){ # || data.type == "norm.fl2"  || data.type == "raw2"
    warning("Derivatives cannot be calculated for 'raw' or 'norm.fl' data. Only the fluorescence values will be shown.")
    deriv = FALSE
  }
  if(methods::is(object) == "grodata"){
    raw_data <- object
  }
  if(methods::is(object) == "flFitRes"){
    if(data.type == "spline" || data.type == "raw" || data.type == "norm.fl") flFit <- object$flFit
    # if(data.type == "spline2" || data.type == "raw2" || data.type == "norm.fl2") flFit <- object$flFit2
    raw_data <- object$data
  } else {
    flFit <- object
  }

  # /// check input parameters
  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(lwd)==FALSE)   stop("Need numeric value for: lwd")
  if(data.type == "spline" ){ # || data.type == "spline2"
    if (!("s" %in% flFit$control$fit.opt | "a" %in% flFit$control$fit.opt)) stop("To plot spline fit results, please run fl.workflow() with 's' in fit.opt.")
  }

  conc <- as.numeric(conc)
  exclude.conc <- as.numeric(exclude.conc)

  # Get name of conditions with multiple replicates
  if(any(methods::is(object) %in% c("flFit","flFitRes"))){
    sample.nm <- nm <- as.character(names(flFit$flFittedSplines))
  } else {
    if(data.type == "norm.fl"){
      sample.nm <- nm <- paste(object$norm.fluorescence[,1], object$norm.fluorescence[,2], object$norm.fluorescence[,3], sep = " | ")
    }
    # if(data.type == "norm.fl2"){
    #   sample.nm <- nm <- paste(object$norm.fluorescence2[,1], object$norm.fluorescence2[,2], object$norm.fluorescence2[,3], sep = " | ")
    # }
    if(data.type == "raw"){
      sample.nm <- nm <- paste(object$fluorescence[,1], object$fluorescence[,2], object$fluorescence[,3], sep = " | ")
    }
    # if(data.type == "raw2"){
    #   sample.nm <- nm <- paste(object$fluorescence2[,1], object$fluorescence2[,2], object$fluorescence2[,3], sep = " | ")
    # }
  }
  if(data.type == "norm.fl") data.nm = "norm.fluorescence"
  # if(data.type == "norm.fl2") data.nm = "norm.fluorescence2"
  if(data.type == "raw") data.nm = "fluorescence"
  # if(data.type == "raw2") data.nm = "fluorescence2"

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
    stop("Please run plot.flFitRes() with valid 'names' or 'conc' argument.")
  }
  # remove conditions with fitFlag = FALSE in all replicates
  # Store each condition with its replicate indices in list filter.ls
  ndx.filt.rep <- unique(lapply(1:length(sample.nm), function(i)which(gsub(" \\| .+", "", sample.nm) %in% (paste0(unlist(str_split(sample.nm[i], " \\| "))[1])))))
  filter.ls <- list()
  for(j in 1:length(ndx.filt.rep)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.rep[[j]]), function(i) ndx.filt.rep[[j]][grep(paste0("^",
                                                                                                           gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[1]),
                                                                                                           ".+[[:space:]]",
                                                                                                           unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                           "$"), sample.nm[ndx.filt.rep[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = FALSE)
  remove <- c()
  for(i in 1:length(ndx.filt)){
    if(length(ndx.filt[[i]]) == 0) remove <- c(remove, i)
  }
  if(!is.null(remove)) ndx.filt <- ndx.filt[-remove]
  # Check FitFlag for each replicate, work per condition
  if(data.type == "spline" ){ #|| data.type == "spline2"
    for(i in 1:length(ndx.filt)){
      if(!all(unlist(lapply(1:length(ndx.filt[[i]]), function(j) (flFit[["flFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]]))))){
        fitflags <- unlist(lapply(1:length(ndx.filt[[i]]), function(j) (flFit[["flFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]])))
        nm <- nm[!(nm %in% sample.nm[(ndx.filt[[i]][!fitflags])])]
      }
    }
  }

  # get indices of samples with selected names
  ndx.keep <- grep(paste0("^",
                          gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm), "$", collapse = "|"), sample.nm)

  if(data.type == "spline"  ){ #|| data.type == "spline2"
    # correct for log transformation
    if(flFit$control$log.y.spline == TRUE){
      for(i in 1:length(ndx.keep)){
        flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]] <-
          exp(flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]]) * flFit$flFittedSplines[[ndx.keep[i]]]$data.in[1]
      }
    }
  }
  if((data.type == "spline") && flFit$control$x_type == "growth" && mean == TRUE){ # || data.type == "spline2"
    message("Grouping of replicates is not supported for spline fits with x_type = 'growth'. Argument changed to mean = FALSE.")
    try(showModal(modalDialog("Grouping of replicates is not supported for spline fits with x_type = 'growth'.", easyClose = TRUE, footer=NULL)), silent = TRUE)
    mean <- FALSE
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
      if(data.type == "spline" ){ # || data.type == "spline2"
        time <- as.list(lapply(1:length(ndx), function(i) cbind(flFit$flFittedSplines[[ndx[[i]]]]$fit.x)))
        data <- as.list(lapply(1:length(ndx), function(i) cbind(flFit$flFittedSplines[[ndx[[i]]]]$fit.fl)))
      } else {
        if(methods::is(object) %in% "flFitRes"){
            time <- lapply(1:length(ndx), function(i) raw_data$time[ndx[i], ])
            data <- raw_data[[data.nm]][ndx, 4:ncol(raw_data[[data.nm]])]
        } else {
          time <- as.list(lapply(1:length(ndx), function(i) cbind(raw_data$time[ndx[[i]], ])))
          data <- raw_data[[data.nm]][ndx, 4:ncol(raw_data[[data.nm]])]
        }
        data <- split(as.matrix(data), 1:nrow(as.matrix(data)))
        data <- lapply(1:length(data), function(i) as.numeric(data[[i]]))
      }

      # Create lists for derivatives and time values for each sample
      if(deriv){
        time.deriv <- as.list(lapply(1:length(ndx), function(i) cbind(flFit$flFittedSplines[[ndx[[i]]]]$spline.deriv1$x)))
        data.deriv <- as.list(lapply(1:length(ndx), function(i) cbind(flFit$flFittedSplines[[ndx[[i]]]]$spline.deriv1$y)))
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
      } # if(deriv)
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
    } # for(n in 1:length(conditions_unique))
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
    # sort names
    df <- df[order(df$group, df$concentration), ]

    df$name <- factor(df$name, levels = unique(factor(df$name)))

    df <- df[df[["mean"]]>0, ]
    df <- df[!apply(df, 1, function(x){all(is.na(x))}),]
    if(!is.null(x.lim)) df <- df[df[["time"]]>x.lim[1], ]

    # replace negative lower ribbon boundaries with 0 for log10 transformation
    if(log.y==TRUE){
      df$lower[df$lower<0] <- 0
    }
    xlab.title <- if(data.type == "norm.fl"  || data.type == "raw" ){ # || data.type == "raw2" || data.type == "norm.fl2"
      "Time"
    } else if (flFit$control$x_type == "growth"){
      "Density"
    } else {
      "Time"
    }
    ylab.title <- if(data.type == "norm.fl"){
      "Normalized fluorescence"
    } else if(data.type == "raw"){
      "Fluorescence"
    } else if(data.type == "spline" && flFit$control$norm_fl){
      "Normalized fluorescence"
    } else if(data.type == "spline" && !flFit$control$norm_fl){
      "Fluorescence"
    }
    # else if(data.type == "norm.fl2"){
    #   "Normalized fluorescence 2"
    # else if(data.type == "raw2" || data.type == "spline2"){
    #   "Fluorescence 2"
    # }
    # else if(data.type == "spline2" && flFit$control$norm_fl){
    #     "Normalized fluorescence 2"
    #   }
    # else if(data.type == "spline2" && !flFit$control$norm_fl){
    #     "Fluorescence 2"
    #   }
    p <- ggplot(df, aes(x=.data$time, y=.data$mean, col = .data$name)) +
      geom_line(linewidth=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title) || y.title == "", ylab.title, y.title)) +
      theme(legend.position=legend.position,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol=legend.ncol)) +
      ggplot2::guides(colour=ggplot2::guide_legend(ncol=legend.ncol))

    if(!all(is.na(df$upper)))
      p <- suppressWarnings(p + geom_ribbon(data = df, aes(ymin=.data$lower,ymax=.data$upper, fill=.data$name), alpha = 0.3, colour = NA)
      )


    if(log.y == TRUE){
      if(!is.null(y.lim)){
        p <- p + scale_y_log10(limits = y.lim, breaks = base_breaks(n = n.ybreaks))
      } else {
        p <- p + scale_y_log10(breaks = base_breaks(n = n.ybreaks))
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
      if(!is.null(x.lim)){
        df.deriv <- df.deriv[df.deriv[,"time"]>=x.lim[1]&df.deriv[,"time"]<=x.lim[2],]
      }
      p.deriv <- ggplot(df.deriv, aes(x=.data$time, y=.data$mean, col = .data$name)) +
        geom_line(linewidth=lwd) +
        theme_classic(base_size = basesize) +
        xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
        theme(legend.position=legend.position,
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      if(!all(is.na(df.deriv$upper)))
        p.deriv <- p.deriv + geom_ribbon(data = df.deriv, aes(ymin=.data$lower, ymax=.data$upper, fill=.data$name), alpha = 0.3, colour = NA)

      y.label.mu = if(object$control$log.y.spline == TRUE){
        paste0("Slope [d(Ln(F/F0))/d",xlab.title, "]")
      } else {
        paste0("Slope [dF/d", xlab.title,"]")
      }
      if(is.null(y.title.deriv) || y.title.deriv == ""){
        p.deriv <- p.deriv + ylab(label = y.label.mu)
      } else {
        p.deriv <- p.deriv + ylab(label = y.title.deriv)
      }

      if(!is.null(x.lim)){
        p.deriv <- p.deriv + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
      } else {
        p.deriv <- p.deriv + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      }

      if(!is.null(y.lim.deriv)){
        p.deriv <- p.deriv + scale_y_continuous(limits = y.lim.deriv, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p.deriv <- p.deriv + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }

      if(is.null(colors)){
        if (length(plotdata.ls) <= 8) {
          p.deriv <- p.deriv + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
        } else if (length(plotdata.ls) <= 50){
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                                 values = big_palette
          ) + scale_color_manual(name = "Condition",
                                 values = big_palette
          )
        }
      } else if (length(colors) < length(unique(df$name))){
        if (length(plotdata.ls) <= 8) {
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))])) +
            scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))]))
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
      p <- suppressWarnings( ggpubr::ggarrange(p, p.deriv, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1), common.legend = TRUE, legend = legend.position, legend.grob = ggpubr::get_legend(p) ))
    }
  } # if(mean == TRUE)
  else {
    df <- data.frame()
    for(i in 1:length(ndx.keep)){
      if(data.type == "spline"){ # || data.type == "spline2"
        df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                              "time" = flFit$flFittedSplines[[ndx.keep[i]]][["fit.x"]],
                                              "y" = flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]]))
      } else {
        if(any(methods::is(object) %in% c("flFit","flFitRes"))){
          df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                                "time" = as.vector(raw_data$time[ndx.keep[i], ]),
                                                "y" = unlist(unname(utils::type.convert(raw_data[[data.nm]][ndx.keep[i], 4:ncol(raw_data[[data.nm]])], as.is=T)))))
        } else {
          df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                                "time" = as.vector(object$time[ndx.keep[i], ]),
                                                "y" = unlist(unname(utils::type.convert(object[[data.nm]][ndx.keep[i], 4:ncol(object[[data.nm]])], as.is=T)))))
        }
      }
    } # if(data.type == "spline"  || data.type == "spline2")
    df <- df[df[["y"]]>0, ]
    if(!is.null(x.lim)) df <- df[df[["time"]]>x.lim[1], ]
    xlab.title <- if(data.type == "norm.fl" || data.type == "raw" ){ # || data.type == "raw2" || data.type == "norm.fl2"
      "Time"
    } else if (object$control$x_type == "growth"){
      "Density"
    } else {
      "Time"
    }
    ylab.title <- if(data.type == "norm.fl"){
      "Normalized fluorescence"
    } else if(data.type == "raw"){
      "fluorescence"
    } else if(data.type == "spline" && flFit$control$norm_fl){
      "Normalized fluorescence"
    } else if(data.type == "spline" && !flFit$control$norm_fl){
      "fluorescence"
    }
    # else if(data.type == "raw2" || data.type == "spline2"){
    #   "Fluorescence 2"
    # } else if(data.type == "spline2" && !flFit$control$norm_fl){
    #   "Fluorescence 2"
    # } else if(data.type == "spline2" && flFit$control$norm_fl){
    #   "Normalized fluorescence 2"
    # } else if(data.type == "norm.fl2"){
    #   "Normalized fluorescence 2"
    # }
    p <- ggplot(df, aes(x=.data$time, y=.data$y, col = .data$name)) +
      geom_line(linewidth=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title) || y.title == "", ylab.title, y.title)) +
      theme(legend.position=legend.position,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol=legend.ncol)) +
      ggplot2::guides(colour=ggplot2::guide_legend(ncol=legend.ncol))

    if(!is.null(x.lim)){
      p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
    } else {
      p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }

    if(log.y == TRUE){
      if(!is.null(y.lim)){
        p <- p + scale_y_log10(limits = y.lim, breaks = base_breaks(n = n.ybreaks))
      } else {
        p <- p + scale_y_log10(breaks = base_breaks(n = n.ybreaks))
      }
    } else {
      if(!is.null(y.lim)){
        p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    }

    if(is.null(colors)){
      if (length(ndx.keep) <= 8) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else if (length(ndx.keep) <= 12) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
      } else if (length(ndx.keep) <= 50){
        p <- p + scale_fill_manual(name = "Condition",
                                   values = big_palette
        ) + scale_color_manual(name = "Condition",
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
                                                          "time" = flFit$flFittedSplines[[ndx.keep[[i]]]]$spline.deriv1$x,
                                                          "y" = flFit$flFittedSplines[[ndx.keep[[i]]]]$spline.deriv1$y))
      }
      if(!is.null(x.lim)){
        df.deriv <- df.deriv[df.deriv[,"time"]>=x.lim[1]&df.deriv[,"time"]<=x.lim[2],]
      }
      p.deriv <- ggplot(df.deriv, aes(x=.data$time, y=.data$y, col = .data$name)) +
        geom_line(linewidth=lwd) +
        theme_classic(base_size = basesize) +
        xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
        theme(legend.position=legend.position,
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      y.label.mu = if(object$control$log.y.spline == TRUE){
        paste0("Slope [d(Ln(F/F0))/d",xlab.title, "]")
      } else {
        paste0("Slope [dF/d", xlab.title,"]")
      }

      if(is.null(y.title.deriv) || y.title.deriv == ""){
        p.deriv <- p.deriv + ylab(label = y.label.mu)
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
          p.deriv <- p.deriv + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
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
            scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))]))
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
  if(is.null(out.nm)) out.nm <- paste0("flGroupPlot")
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
  }
}

#' @rdname plot.flFitRes
#' @export plot.flFit
#' @export
#' @return A plot with all curves (raw fluorescence measurements or raw normalized fluorescence over time) in a \code{flFit} object with \code{\link{flFit}}, with replicates combined by the group averages (if \code{mean = TRUE}) or not (\code{mean = FALSE}).
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
#'                    data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
#'                    csvsep = "\t",
#'                    csvsep.fl = "\t")
#'
#' # Run curve fitting workflow
#' res <- flFit(fl_data = input$norm.fluorescence,
#'              time = input$time,
#'              parallelize = FALSE,
#'              control = fl.control(fit.opt = "s", suppress.messages = TRUE,
#'              x_type = "time", norm_fl = TRUE))
#'
#' plot(res, legend.ncol = 3, basesize = 15)
#'
#'
plot.flFit <- plot.flFitRes

#' Compare fluorescence and growth over time
#'
#' \code{plot.dual} creates a two-panel plot in which fluorescence or growth values are shown over time, allowing for the identification of, e.g., expression patterns in different growth stages.
#'
#' @param x A \code{flFit}, \code{flFitRes}, or \code{grodata} object created with \code{\link{flFit}}, \code{\link{fl.workflow}} or \code{\link{read_data}}
#' @param fluorescence (Character) Indicate, which type of fluorescence data should be displayed.
#' @param IDs (String or vector of strings) Define samples or groups (if \code{mean = TRUE}) to combine into a single plot based on exact matches with entries in the \code{label} or \code{condition} columns of \code{grofit$expdesign}.
#' @param names (String or vector of strings) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered. Note: Ensure to use unique concentration values to extract groups of interest. If the concentration value of one condition is included in its entirety within the name of other conditions (e.g., the dataset contains '1', '10', and '100', \code{code = 10} will select both '10 and '100'), it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define groups to exclude from the plot. Partial matches with sample/group names are accepted.
#' @param exclude.conc (Numeric or numeric vector) Define concentrations to exclude from the plot.
#' @param mean (Logical) Display the mean and standard deviation of groups with replicates (\code{TRUE}) or plot each sample individually (\code{FALSE})?
#' @param log.y.growth (Logical) Log-transform the y-axis of the growth plot (\code{TRUE}) or not (\code{FALSE})?
#' @param log.y.fl (Logical) Log-transform the y-axis of the fluorescence plot (\code{TRUE}) or not (\code{FALSE})?
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param colors (vector of strings) Define a color palette used to draw the plots. If \code{NULL}, default palettes are chosen based on the number of groups/samples within the plot. Note: The number of provided colors should at least match the number of groups/samples.
#' @param color_groups (Logical) Shall samples within the same group but with different concentrations be shown in different shades of the same color?
#' @param group_pals (String vector) Define the colors used to display sample groups with identical concentrations. The number of selected color palettes must be at least the number of displayed groups. The order of the chosen palettes corresponds to the oder of conditions in the legend. Available options: "Green", "Oranges", "Purple", "Cyan", "Grey", "Red", "Blue", and "Magenta".
#' @param basesize (Numeric) Base font size.
#' @param y.lim.growth (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the growth plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim.fl (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the fluorescence plot as a vector in the form \code{c(l, u)}.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the x-axis of both fluorescence and growth plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both growth curve and derivative plots.
#' @param y.title.growth (Character) Optional: Provide a title for the y-axis of the growth plot.
#' @param y.title.fl (Character) Optional: Provide a title for the y-axis of the fluorescence plot.
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
#' @export plot.dual
#' @export
#'
#' @return A two-panel plot, showing raw fluorescence (\code{fluorescence = "fl"}) or normalized fluorescence (\code{fluorescence = "norm.fl"}) over time in the top panel, and growth over time in the bottom panel.
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
#' plot.dual(res, legend.ncol = 3, basesize = 15)
#'
#'
plot.dual <-  function(x,
                       fluorescence = c("fl", "norm.fl"),
                       IDs = NULL,
                       names = NULL,
                       conc = NULL,
                       mean = TRUE,
                       exclude.nm = NULL,
                       exclude.conc = NULL,
                       log.y.growth = FALSE,
                       log.y.fl = FALSE,
                       n.ybreaks = 6,
                       colors = NULL,
                       color_groups = TRUE,
                       group_pals = c('Green', 'Orange', 'Purple', 'Magenta', 'Grey', 'Blue', 'Grey', 'Red', 'Cyan', 'Brown', 'Mint'),
                       basesize = 20,
                       y.lim.growth = NULL,
                       y.lim.fl = NULL,
                       x.lim = NULL,
                       x.title = NULL,
                       y.title.growth = NULL,
                       y.title.fl = NULL,
                       lwd = 1.1,
                       legend.position = "bottom",
                       legend.ncol = 2,
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
  if(!is.null(colors))
    colSpline <- toupper(colors)

  # Convert range  and selecting arguments
  x.lim <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", x.lim)), pattern = ";|,"))
  y.lim.fl <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", y.lim.fl)), pattern = ";|,"))
  y.lim.growth <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", y.lim.growth)), pattern = ";|,"))
  suppressWarnings(assign("x.lim" ,as.numeric(x.lim)))
  if(all(is.na(x.lim))) x.lim <- NULL
  suppressWarnings(assign("y.lim.fl" ,as.numeric(y.lim.fl)))
  if(all(is.na(y.lim.fl))) y.lim.fl <- NULL
  suppressWarnings(assign("y.lim.growth" ,as.numeric(y.lim.growth)))
  if(all(is.na(y.lim.growth))) y.lim.growth <- NULL

  if(!any(methods::is(object) %in% c("flFit","flFitRes", "grodata"))) stop("'x' needs to be an object created with fl.workflow(), flFit(), parse_data(), or read_data().")
  growth <- growth.nm <- "growth"
  fluorescence <- match.arg(fluorescence)

  if(methods::is(object) == "grodata"){
    raw_data <- object
  }
  if(methods::is(object) == "flFitRes"){
    flFit <- object$flFit
    # if(length(grep("2", fluorescence))>0) flFit <- object$flFit2
    raw_data <- object$data
  } else {
    flFit <- object
    raw_data <- object
  }

  # /// check input parameters
  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(lwd)==FALSE)   stop("Need numeric value for: lwd")

  conc <- as.numeric(conc)
  exclude.conc <- as.numeric(exclude.conc)

  # Get name of conditions with multiple replicates
  if(any(methods::is(object) %in% c("flFit","flFitRes"))){
    sample.nm <- nm <- as.character(names(flFit$flFittedSplines))
  } else {
    sample.nm <- nm <- paste(object$fluorescence[,1], object$fluorescence[,2], object$fluorescence[,3], sep = " | ")
  }
  if(fluorescence == "fl") fl.nm = "fluorescence"
  # if(fluorescence == "fl2") fl.nm = "fluorescence"
  if(fluorescence == "norm.fl") fl.nm = "norm.fluorescence"
  # if(fluorescence == "norm.fl2") fl.nm = "norm.fluorescence2"

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
    stop("Please run plot.dual() with valid 'names' or 'conc' argument.")
  }
  # remove conditions with fitFlag = FALSE in all replicates
  # Store each condition with its replicate indices in list filter.ls
  ndx.filt.rep <- unique(lapply(1:length(sample.nm), function(i)which(gsub(" \\| .+", "", sample.nm) %in% (paste0(unlist(str_split(sample.nm[i], " \\| "))[1])))))
  filter.ls <- list()
  for(j in 1:length(ndx.filt.rep)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.rep[[j]]), function(i) ndx.filt.rep[[j]][grep(paste0("^",
                                                                                                           gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[1]),
                                                                                                           ".+[[:space:]]",
                                                                                                           unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                           "$"), sample.nm[ndx.filt.rep[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = FALSE)

  # get indices of samples with selected names
  ndx.keep <- grep(paste0(
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm), collapse = "|"), sample.nm)

  if(mean == TRUE){
    # Combine replicates via their mean and standard deviation
    conditions <- str_replace_all(nm, "\\| .+ \\| ", "| ")
    conditions_unique <- unique(conditions)

    # Create lists for each selected condition, with growth values and (normalized) fluorescence, respectively. Each list item represents one condition with their average and SD
    plotdata.ls <- list()
    for(n in 1:length(conditions_unique)){
      # find indexes of replicates
      ndx <- intersect(ndx.keep, grep(paste0("^",
                                             gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", unlist(str_split(conditions_unique[n], " \\| "))[1]),
                                             " \\|.+[[:space:]]",
                                             unlist(str_split(conditions_unique[n], " \\| "))[2],
                                             "$"), sample.nm))
      name <- conditions_unique[n]
      # Create lists for growth and time values for each sample

      if(methods::is(object) %in% "flFitRes"){
        time <- lapply(1:length(ndx), function(i) raw_data$time[ndx[i], ])
        dens.data <- raw_data[[growth.nm]][ndx, 4:ncol(raw_data[[growth.nm]])]
        fl.data <- raw_data[[fl.nm]][ndx, 4:ncol(raw_data[[fl.nm]])]
      } else {
        time <- as.list(lapply(1:length(ndx), function(i) cbind(raw_data$time[ndx[[i]], ])))
        dens.data <- raw_data[[growth.nm]][ndx, 4:ncol(raw_data[[growth.nm]])]
        fl.data <- raw_data[[fl.nm]][ndx, 4:ncol(raw_data[[fl.nm]])]
      }
      dens.data <- split(as.matrix(dens.data), 1:nrow(as.matrix(dens.data)))
      dens.data <- lapply(1:length(dens.data), function(i) as.numeric(dens.data[[i]]))
      fl.data <- split(as.matrix(fl.data), 1:nrow(as.matrix(fl.data)))
      fl.data <- lapply(1:length(fl.data), function(i) as.numeric(fl.data[[i]]))

      # correct for unequal lengths of data series
      time.all <- Reduce(union, time)
      for(i in 1:length(time)){
        assign(paste0("time.missing_", i), setdiff(time.all, time[[i]]) )
        if(length(get(paste0("time.missing_", i))) > 0){
          for(j in 1:length(get(paste0("time.missing_", i)))){
            # extract growth values into a separate list
            dens.data[[i]] <- append(dens.data[[i]],
                                     values = NA,
                                     after = match(get(paste0("time.missing_", i))[j],
                                                   time.all) - 1)
            # extract fl values into a separate list
            fl.data[[i]] <- append(fl.data[[i]],
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
      dens.data <- do.call("cbind", dens.data)
      dens.avg <- rowMeans(dens.data, na.rm = FALSE)
      dens.sd <- apply(dens.data, 1, sd, na.rm = FALSE)
      fl.data <- do.call("cbind", fl.data)
      fl.avg <- rowMeans(fl.data, na.rm = FALSE)
      fl.sd <- apply(fl.data, 1, sd, na.rm = FALSE)
      plotdata.ls[[n]] <- data.frame("name" = name, "time" = time,
                                     "dens.mean" = dens.avg, "dens.upper" = dens.avg+dens.sd, "dens.lower" = dens.avg-dens.sd,
                                     "fl.mean" = fl.avg, "fl.upper" = fl.avg+fl.sd, "fl.lower" = fl.avg-fl.sd)
    } # for(n in 1:length(conditions_unique))
    names(plotdata.ls) <- gsub(" \\| NA", "", conditions_unique)

    plotdata.ls <- plotdata.ls[!is.na(plotdata.ls)]
    df <- do.call(rbind.data.frame, plotdata.ls)
    df$name <- gsub(" \\| NA", "", df$name)

    df$concentration <- as.numeric(gsub(".+ \\| ", "", df$name))
    df$group <- gsub(" \\| .+", "", df$name)

    # sort names
    df <- df[order(df$group, df$concentration), ]

    df$name <- factor(df$name, levels = unique(factor(df$name)))
    df <- df[df[["dens.mean"]]>0, ]
    df <- df[df[["fl.mean"]]>0, ]
    df <- df[!apply(df, 1, function(x){all(is.na(x))}),]
    if(!is.null(x.lim)) df <- df[df[["time"]]>x.lim[1], ]

    # replace negative lower ribbon boundaries with 0 for log10 transformation
    if(log.y.growth==TRUE){
      df$dens.lower[df$dens.lower<0] <- 0
    }
    if(log.y.fl==TRUE){
      df$fl.lower[df$fl.lower<0] <- 0
    }

    xlab.title <- "Time"
    ylab.title.dens <- "Density"

    p <- ggplot(df, aes(x=.data$time, y=.data$dens.mean, col = .data$name)) +
      geom_line(linewidth=lwd) +
      geom_ribbon(aes(ymin=.data$dens.lower,ymax=.data$dens.upper, fill=.data$name), alpha = 0.3, colour = NA) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title.growth) || y.title.growth == "", ylab.title.dens, y.title.growth)) +
      theme(legend.position=legend.position,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol=legend.ncol))

    if(log.y.growth == TRUE){
      if(!is.null(y.lim.growth)){
        p <- p + scale_y_log10(limits = y.lim.growth, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    } else {
      if(!is.null(y.lim.growth)){
        p <- p + scale_y_continuous(limits = y.lim.growth, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
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
          p <- p + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
        } else if (length(plotdata.ls) <= 12){
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
    # /// add panel with fluorescence over time
    p.fl <- ggplot(df, aes(x=.data$time, y=.data$fl.mean, col = .data$name)) +
      geom_line(linewidth=lwd) +
      geom_ribbon(aes(ymin=.data$fl.lower,ymax=.data$fl.upper, fill=.data$name), alpha = 0.3, colour = NA) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      theme(legend.position=legend.position,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    if(log.y.fl == TRUE){
      if(!is.null(y.lim.fl)){
        p.fl <- p.fl + scale_y_log10(limits = y.lim.fl, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p.fl <- p.fl + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    } else {
      if(!is.null(y.lim.fl)){
        p.fl <- p.fl + scale_y_continuous(limits = y.lim.fl, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p.fl <- p.fl + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    }

    ylab.title.fl = if(any(grep("norm", fluorescence))){
      paste0("Normalized fluorescence")
    } else {
      paste0("Fluorescence")
    }
    if(is.null(y.title.fl) || y.title.fl == ""){
      p.fl <- p.fl + ylab(label = ylab.title.fl)
    } else {
      p.fl <- p.fl + ylab(label = y.title.fl)
    }

    if(!is.null(x.lim)){
      p.fl <- p.fl + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
    } else {
      p.fl <- p.fl + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }

    if(is.null(colors)){
      if (length(plotdata.ls) <= 8) {
        p.fl <- p.fl + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else if (length(plotdata.ls) <= 12){
        p.fl <- p.fl + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
      } else if (length(plotdata.ls) <= 50){
        p.fl <- p.fl + scale_fill_manual(name = "Condition",
                                         values = big_palette
        ) + scale_color_manual(name = "Condition",
                               values = big_palette
        )
      }
    } else if (length(colors) < length(unique(df$name))){
      if (length(plotdata.ls) <= 8) {
        p.fl <- p.fl + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))])) +
          scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))]))
      } else if (length(plotdata.ls) <= 12) {
        p.fl <- p.fl + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))])) +
          scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))]))
      } else if (length(plotdata.ls) <= 50){
        p.fl <- p.fl + scale_fill_manual(name = "Condition",
                                   values = c(colors, big_palette[-(1:length(colors))])) +
          scale_color_manual(name = "Condition",
                             values = c(colors, big_palette[-(1:length(colors))])
          )
      }
    } else {
      p.fl <- p.fl + scale_fill_manual(name = "Condition",
                                 values = colors) +
        scale_color_manual(name = "Condition",
                           values = colors)
    }
    p <- suppressWarnings(
      ggpubr::ggarrange(p, p.fl, ncol = 1, nrow = 2, align = "v", heights = c(2,2), common.legend = TRUE, legend = legend.position, legend.grob = ggpubr::get_legend(p))
    )
  } # if(mean == TRUE)
  else {
    df <- data.frame()
    for(i in 1:length(ndx.keep)){
      df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                            "time" = as.vector(raw_data$time[ndx.keep[i], ]),
                                            "growth" = unlist(unname(utils::type.convert(raw_data[[growth.nm]][ndx.keep[i], 4:ncol(raw_data[[growth.nm]])], as.is=T))),
                                            "fl" = unlist(unname(utils::type.convert(raw_data[[fl.nm]][ndx.keep[i], 4:ncol(raw_data[[fl.nm]])], as.is=T)))))

    }
    df <- df[df[["growth"]]>0, ]
    df <- df[df[["fl"]]>0, ]

    if(!is.null(x.lim)) df <- df[df[["time"]]>x.lim[1], ]
    xlab.title <- "Time"

    ylab.title <- "Density"
    p <- ggplot(df, aes(x=.data$time, y=.data$growth, col = .data$name)) +
      geom_line(linewidth=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title.growth) || y.title.growth == "", ylab.title, y.title.growth)) +
      theme(legend.position=legend.position,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol=legend.ncol))

    if(!is.null(x.lim)){
      p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
    } else {
      p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }

    if(log.y.growth == TRUE){
      if(!is.null(y.lim.growth)){
        p <- p + scale_y_log10(limits = y.lim.growth, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    } else {
      if(!is.null(y.lim.growth)){
        p <- p + scale_y_continuous(limits = y.lim.growth, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    }

    if(is.null(colors)){
      if (length(ndx.keep) <= 8) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else if (length(ndx.keep) <= 12) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
      } else if (length(ndx.keep) <= 50){
        p <- p + scale_fill_manual(name = "Condition",
                                   values = big_palette
        ) + scale_color_manual(name = "Condition",
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
    p.fl <- ggplot(df, aes(x=.data$time, y=.data$fl, col = .data$name)) +
      geom_line(linewidth=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
      theme(legend.position=legend.position,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    ylab.title <- if(any(grep("norm", fluorescence))){
      "Normalized fluorescence"
    } else {
      "Fluorescence"
    }

    if(log.y.fl == TRUE){
      if(!is.null(y.lim.fl)){
        p.fl <- p.fl + scale_y_log10(limits = y.lim.fl, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p.fl <- p.fl + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    } else {
      if(!is.null(y.lim.fl)){
        p.fl <- p.fl + scale_y_continuous(limits = y.lim.fl, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p.fl <- p.fl + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    }

    if(is.null(y.title.fl) || y.title.fl == ""){
      p.fl <- p.fl + ylab(label = ylab.title)
    } else {
      p.fl <- p.fl + ylab(label = y.title.fl)
    }

    if(!is.null(x.lim)){
      p.fl <- p.fl + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
    } else {
      p.fl <- p.fl + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }

    if(is.null(colors)){
      if (length(ndx.keep) <= 8) {
        p.fl <- p.fl + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else if (length(ndx.keep) <= 12) {
        p.fl <- p.fl + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
      } else if (length(ndx.keep) <= 50){
        p.fl <- p.fl + scale_fill_manual(name = "Condition",
                                         values = big_palette
        ) + scale_color_manual(name = "Condition",
                               values = big_palette
        )
      }
    } else if (length(colors) < length(unique(df$name))){
      if (length(ndx.keep) <= 8) {
        p.fl <- p.fl + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))])) +
          scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(8, name = "Dark2")[-(1:length(colors))]))
      } else if (length(ndx.keep) <= 12) {
        p.fl <- p.fl + scale_fill_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))])) +
          scale_color_manual(name = "Condition", values = c(colors, RColorBrewer::brewer.pal(12, name = "Set3")[-(1:length(colors))]))
      } else if (length(ndx.keep) <= 50){
        p.fl <-p.fl + scale_fill_manual(name = "Condition",
                                   values = c(colors, big_palette[-(1:length(colors))])) +
          scale_color_manual(name = "Condition",
                             values = c(colors, big_palette[-(1:length(colors))])
          )
      }
      else {
        p <- p + scale_fill_manual(name = "Condition",
                                   values = colors) +
          scale_color_manual(name = "Condition",
                             values = colors)
      }
    }
    p <- suppressWarnings(
      ggpubr::ggarrange(p, p.fl, ncol = 1, nrow = 2, align = "v", heights = c(2,2), common.legend = TRUE, legend = legend.position, legend.grob = ggpubr::get_legend(p))
    )
  }
  if(export == FALSE && plot == FALSE){
    return(p)
  }
  out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
  if (export == TRUE){
    if(is.null(out.nm)) out.nm <- paste0("DualPlot_", fluorescence)
    if(is.null(width)){
      w <- 10 + 3*ifelse(mean==TRUE,length(conditions_unique), length(nm))/15
    } else {
      w <- width
    }
    if(is.null(height)){
      h <- 9
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
  }
  if (plot == TRUE){
    print(p)
  }
}

#' Generic plot function for \code{drFitFL} objects.
#'
#' code{drFitfl} calls \code{\link{plot.drFitFLModel}} for each group used in a dose-response analysis with \code{dr.method = "model"}
#'
#' @param x object of class \code{drFit}, created with \code{\link{growth.drFit}}.
#' @param pch (Numeric) Shape of the raw data symbols.
#' @param broken (Logical) If TRUE the x axis is broken provided this axis is logarithmic (using functionality in the CRAN package 'plotrix').
#' @param bp (Numeric) Specifying the break point below which the dose is zero (the amount of stretching on the dose axis above zero in order to create the visual illusion of a logarithmic scale including 0). The default is the base-10 value corresponding to the rounded value of the minimum of the log10 values of all positive dose values. This argument is only working for logarithmic dose axes.
#' @param log (Character) String which contains '"x"' if the x axis is to be logarithmic, '"y"' if the y axis is to be logarithmic and '"xy"' or '"yx"' if both axes are to be logarithmic. The default is "x". The empty string "" yields the original axes.
#' @param n.xbreaks (Numeric) Number of breaks on the x-axis (if not log-transformed). The breaks are generated using \code{pretty}. Thus, the final number of breaks can deviate from the user input.
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis (if not log-transformed). The breaks are generated using \code{pretty}. Thus, the final number of breaks can deviate from the user input.
#' @param colData (Numeric or character) Contour color of the raw data circles.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param cex.lab (Numeric) Font size of axis titles.
#' @param cex.axis (Numeric) Font size of axis annotations.
#' @param colSpline (Numeric or character) Spline line colour.
#' @param lwd (Numeric) Line width of the individual splines.
#' @param ec50line (Logical) Show pointed horizontal and vertical lines at the EC50 values (\code{TRUE}) or not (\code{FALSE}).
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Additional arguments. This has currently no effect and is only meant to fulfill the requirements of a generic function.
#'
#' @return One plot per condition tested in the dose-response analysis (\code{\link{fl.drFit}} with \code{control = fl.control(dr.method = "model")}).
#'
#' @export plot.drFitfl
#' @export
#'
#' @examples
#' # load example dataset
#' input <- read_data(data.growth = system.file("lac_promoters_growth.txt", package = "QurvE"),
#'                    data.fl = system.file("lac_promoters_fluorescence.txt", package = "QurvE"),
#'                    csvsep = "\t",
#'                    csvsep.fl = "\t")
#'
#' # Define fit controls
#' control <- fl.control(fit.opt = "s",
#'              x_type = "time", norm_fl = TRUE,
#'              dr.parameter = "max_slope.spline",
#'              dr.method = "model",
#'              suppress.messages = TRUE)
#'
#' # Run curve fitting workflow
#' res <- flFit(fl_data = input$norm.fluorescence,
#'              time = input$time,
#'              parallelize = FALSE,
#'              control = control)
#'
#' # Perform dose-response analysis with biosensor model
#' drFitfl <- fl.drFit(flTable = res$flTable, control = control)
#'
#' plot(drFitfl)
#'
plot.drFitfl <- function(x, ec50line = TRUE, log = c("xy"), pch = 1, broken = TRUE, bp, n.xbreaks, n.ybreaks,
                         colSpline = 1, colData = 1, cex.point = 1, cex.lab = 1.5,
                         cex.axis = 1.3, y.lim = NULL, x.lim = NULL,
                         lwd = 2, plot = TRUE, export = FALSE,
                         height = 7, width = 9, out.dir = NULL, ...)
{
  drFitfl <- x
  if(!is.null(colSpline))
    colSpline <- toupper(colSpline)
  if(!is.null(colData))
    colData <- toupper(colData)
  # x an object of class drFitfl
  if(methods::is(drFitfl) != "drFitfl") stop("x needs to be an object of class 'drFitfl', created with fl.drFit(control=fl.control(dr.method='model').")
  if(length(drFitfl) == 1) stop("drFitfl is NA. Please run fl.drFit() with dr.method = 'model' in the control object.")
  if(drFitfl$control$dr.method != "model") stop("x needs")
    n <- length(drFitfl$drFittedModels)
      # /// plot all plot.drFitFLModel objects
      for (i in 1:n) {
        try(plot(drFitfl$drFittedModels[[i]], ec50line = ec50line, log = log, pch = pch, broken = broken, bp, n.xbreaks, n.ybreaks,
                 colSpline = colSpline, colData = colData, cex.point = cex.point, cex.lab = cex.lab,
                 cex.axis = cex.axis, y.lim = y.lim, x.lim = x.lim,
                 lwd = lwd, plot = plot, export = export,
                 height = height, width = width, out.dir = out.dir))
      }
}
