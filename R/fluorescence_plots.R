#' Generic plot function for \code{flcFittedLinear} objects. Plot the results of a linear regression on ln-transformed data
#'
#' \code{plot.flFitLinear} shows the results of a linear regression and visualizes raw data, data points included in the fit, the tangent obtained by linear regression, and the lag time.
#'
#' @param flFittedLinear A \code{flFittedLinear} object created with \code{\link{flFitLinear}} or stored within a \code{flFitRes} or \code{flFit} object created with \code{\link{fl.workflow}} or \code{\link{flFit}}, respectively.
#' @param log ("x" or "y") Display the x- or y-axis on a logarithmic scale.
#' @param which ("fit" or "diagnostics") Display either the results of the linear fit on the raw data or statistical evaluation of the linear regression.
#' @param pch (Numeric) Shape of the raw data symbols.
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
#' @export plot.flFitLinear
#' @export
#'
plot.flFitLinear <- function(flFittedLinear, log="", which=c("fit", "diagnostics", "fit_diagnostics"), pch = 21, cex.point = 1, cex.lab = 1.5,
                             cex.axis = 1.3, lwd = 2, y.lim = NULL, x.lim = NULL,
                             plot = TRUE, export = FALSE, height = ifelse(which=="fit", 7, 5),
                             width = ifelse(which=="fit", 9, 9), out.dir = NULL, ...)
  {
  if(methods::is(flFittedLinear) != "flFitLinear") stop("flFittedLinear needs to be an object created with flFitLinear().")
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
      xlab <- "Ln(density + 1)"
    } else {
      xlab <- "Density"
    }
  }

  p <- function(){
    switch(which,
           fit = {
             par(mar=c(5.1+cex.lab, 4.1+cex.lab+0.5*cex.axis, 4.1, 3.1), cex.lab = cex.lab, cex.axis = cex.axis)

             plot(flFittedLinear$"fl.in" ~ flFittedLinear$"x.in", xlab="", ylab = "", pch = pch,
                  log=log, las=1, yaxt="n", xaxt="n", type = "n", xlim = x.lim, ylim = y.lim, ...)
             points(flFittedLinear$"fl.in" ~ flFittedLinear$"x.in", cex = cex.point, pch=pch)

             title(ylab = ylab, line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = xlab, line = 1 + 0.7*cex.lab + 0.7*cex.axis, cex.lab = cex.lab)

             try(points(flFittedLinear$fl.in[flFittedLinear$ndx.in] ~ flFittedLinear$x.in[flFittedLinear$ndx.in], pch=pch, cex = cex.point*1.15, col="black", bg="red"))
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)

             ## lag phase
             lag <- flFittedLinear$par["lag"]
             coef_ <- flFittedLinear$par

             if(flFittedLinear$fitFlag2){
               try(points(flFittedLinear$fl.in[flFittedLinear$ndx2.in] ~ flFittedLinear$x.in[flFittedLinear$ndx2.in], pch=pch, cex = cex.point*1.15, col="black", bg=ggplot2::alpha("magenta3", 1)))
               lag2 <- flFittedLinear$par["lag2"]
               if(lag2 < lag && lag2 > flFittedLinear$x.in[1]){
                 try(time2 <- seq(lag2, max(flFittedLinear$"x.in"), length=200), silent = T)
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"x.in"), length=200), silent = T)
                 try(lines(time2, QurvE:::grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)
                 try(lines(c(min(flFittedLinear$"x.in"[1]), lag2), rep(flFittedLinear$"fl.in"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7)), silent = T)
                 try(lines(time, QurvE:::grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               } else {
                 try(time2 <- seq(coef_["x.max2_start"]-0.25*(coef_["x.max2_end"]-coef_["x.max2_start"]), max(flFittedLinear$"x.in"), length=200), silent = T)
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"x.in"), length=200), silent = T)
                 try(lines(time, QurvE:::grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
                 try(lines(time2, QurvE:::grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)

               }
             } else if(flFittedLinear$fitFlag){
               if(lag < flFittedLinear$x.in[flFittedLinear$ndx.in[1]]){
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"filt.x"), length=200), silent = T)
                 try(lines(time, QurvE:::grow_linear(time, coef_)[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               } else {
                 try(time <- seq(flFittedLinear$filt.x[flFittedLinear$ndx.in[1]]/2, max(flFittedLinear$"filt.x"), length=200), silent = T)
                 try(lines(time, QurvE:::grow_linear(time, coef_)[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               }
             }
             graphics::mtext(paste("R2:", round(flFittedLinear$rsquared, digits = 3)), side = 4 , adj = 0.75, line = -2.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab * 0.7)
             graphics::mtext(paste("h:", ifelse(is.null(flFittedLinear$control$lin.h), "NULL", flFittedLinear$control$lin.h),
                         "   R2-thresh.:",  flFittedLinear$control$lin.R2,
                         "   RSD-thresh.:",  flFittedLinear$control$lin.RSD,
                         "t0:", flFittedLinear$control$t0,
                         "  min.density:", flFittedLinear$control$min.density,
                         "   dY-thresh.:",  flFittedLinear$control$lin.dY),
                         cex = cex.lab*0.7, side = 3, line = -2.5, adj = 0.05, outer = TRUE)
           },
           diagnostics = {
             opar <- par(no.readonly = TRUE)
             on.exit(par(opar))
             par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), cex.lab = cex.lab, cex.axis = cex.axis, mfrow=c(1,2))

             ## residuals vs. fitted
             obs <- flFittedLinear$log.data
             sim <- QurvE:::grow_linear(flFittedLinear$"x.in", flFittedLinear$par)
             plot(flFittedLinear$fit[["residuals"]] ~ fitted(flFittedLinear$fit), xlab="", ylab="", type = "n", pch = pch, xaxt="n", yaxt="n")
             points(flFittedLinear$fit[["residuals"]] ~ fitted(flFittedLinear$fit), cex = cex.point, pch=pch)
             abline(h=0, col="grey")
             title(ylab = "residuals", line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = "fitted", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)
             ## normal q-q-plot
             qqnorm(flFittedLinear$fit[["residuals"]], cex = cex.point, xlab="", ylab="", xaxt="n", yaxt="n", main = "")
             qqline(flFittedLinear$fit[["residuals"]])
             title("Normal Q-Q Plot", line = 1, cex.main = cex.lab)
             title(ylab = "Sample quantiles", line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = "Theoretical quantiles", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)
           },
           fit_diagnostics = {

             opar <- par(no.readonly = TRUE)
             on.exit(par(opar))
             layout(matrix(c(1,1,2,3), nrow=2, byrow=TRUE))
             par(mar=c(5.1, 4.1+cex.lab, 4.1, 3.1), mai = c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.7 + 0.2*cex.lab + 0.2*cex.axis, 0.5, 0.5), cex.lab = cex.lab, cex.axis = cex.axis)

             plot(flFittedLinear$"fl.in" ~ flFittedLinear$"x.in", xlab="", ylab = "",
                  log=log, las=1, yaxt="n", xaxt="n", type = "n", xlim = x.lim, ylim = y.lim, pch = pch, ...)
             points(flFittedLinear$"fl.in" ~ flFittedLinear$"x.in", cex = cex.point, pch=pch)

             title(ylab = ylab, line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = xlab, line = 1 + 0.7*cex.lab + 0.7*cex.axis, cex.lab = cex.lab)

             try(points(flFittedLinear$fl.in[flFittedLinear$ndx.in] ~ flFittedLinear$x.in[flFittedLinear$ndx.in], pch=pch, cex = cex.point*1.15, col="black", bg="red"))
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)

             ## lag phase
             lag <- flFittedLinear$par["lag"]
             coef_ <- flFittedLinear$par

             if(flFittedLinear$fitFlag2){
               try(points(flFittedLinear$fl.in[flFittedLinear$ndx2.in] ~ flFittedLinear$x.in[flFittedLinear$ndx2.in], pch=pch, cex = cex.point*1.15, col="black", bg=ggplot2::alpha("magenta3", 1)))
               lag2 <- flFittedLinear$par["lag2"]
               if(lag2 < lag && lag2 > flFittedLinear$x.in[1]){
                 try(time2 <- seq(lag2, max(flFittedLinear$"x.in"), length=200), silent = T)
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"x.in"), length=200), silent = T)
                 try(lines(time2, QurvE:::grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)
                 try(lines(c(min(flFittedLinear$"x.in"[1]), lag2), rep(flFittedLinear$"fl.in"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7)), silent = T)
                 try(lines(time, QurvE:::grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               } else {
                 try(time2 <- seq(coef_["x.max2_start"]-0.25*(coef_["x.max2_end"]-coef_["x.max2_start"]), max(flFittedLinear$"x.in"), length=200), silent = T)
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"x.in"), length=200), silent = T)
                 try(lines(time, QurvE:::grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
                 try(lines(time2, QurvE:::grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)

               }
             } else if(flFittedLinear$fitFlag){
               if(lag < flFittedLinear$x.in[flFittedLinear$ndx.in[1]]){
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"filt.x"), length=200), silent = T)
                 try(lines(time, QurvE:::grow_linear(time, coef_)[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               } else {
                 try(time <- seq(flFittedLinear$filt.x[flFittedLinear$ndx.in[1]]/2, max(flFittedLinear$"filt.x"), length=200), silent = T)
                 try(lines(time, QurvE:::grow_linear(time, coef_)[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               }
             }
             graphics::mtext(paste("R2:", round(flFittedLinear$rsquared, digits = 3)), side = 4 , adj = 0.75, line = -2.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab * 0.7)
             graphics::mtext(paste("h:", ifelse(is.null(flFittedLinear$control$lin.h), "NULL", flFittedLinear$control$lin.h),
                         "   R2-thresh.:",  flFittedLinear$control$lin.R2,
                         "   RSD-thresh.:",  flFittedLinear$control$lin.RSD,
                         "t0:", flFittedLinear$control$t0,
                         "  min.density:", flFittedLinear$control$min.density,
                         "   dY-thresh.:",  flFittedLinear$control$lin.dY),
                         cex = cex.lab*0.7, side = 3, line = -2.5, adj = 0.05, outer = TRUE)

             ## residuals vs. fitted
             obs <- flFittedLinear$log.data
             sim <- QurvE:::grow_linear(flFittedLinear$"x.in", flFittedLinear$par)
             plot(flFittedLinear$fit[["residuals"]] ~ fitted(flFittedLinear$fit), xlab="", ylab="", type = "n", pch = pch, xaxt="n", yaxt="n")
             points(flFittedLinear$fit[["residuals"]] ~ fitted(flFittedLinear$fit), cex = cex.point, pch=pch)
             abline(h=0, col="grey")
             title(ylab = "residuals", line = 2 + 0.5*cex.lab+0.9*cex.axis, cex.lab = cex.lab)
             title(xlab = "fitted", line = 1+0.7*cex.lab+0.7*cex.axis, cex.lab = cex.lab)
             axis(1, mgp=c(3,1+0.5*cex.axis,0))
             axis(2, las=1)
             ## normal q-q-plot
             qqnorm(flFittedLinear$fit[["residuals"]], cex = cex.point, xlab="", ylab="", xaxt="n", yaxt="n", main = "")
             qqline(flFittedLinear$fit[["residuals"]])
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
    dir.create(out.dir, showWarnings = F)
    grDevices::png(paste0(out.dir, "/", paste(flFittedLinear$gcID, collapse = "_"), "_LinFitPlot.png"),
                   width = w, height = h, units = 'in', res = 300)
    p()
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", paste(flFittedLinear$gcID, collapse = "_"), "_LinFitPlot.pdf"))
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
#' @param flFitSpline Object of class \code{flFitSpline}, created with \code{\link{flFitSpline}}.
#' @param add (Logical) Shall the fitted spline be added to an existing plot? \code{TRUE} is used internally by \code{\link{plot.flBootSpline}}.
#' @param raw (Logical) Display raw density as circles (\code{TRUE}) or not (\code{FALSE}).
#' @param slope (Logical) Show the slope at the maximum slope (\code{TRUE}) or not (\code{FALSE}).
#' @param deriv (Logical) Show the derivative (i.e., slope) over time in a secondary plot (\code{TRUE}) or not (\code{FALSE}).
#' @param spline (Logical) Only for \code{add = TRUE}: add the current spline to the existing plot (\code{FALSE}).
#' @param log.y (Logical) Log-transform the y-axis (\code{TRUE}) or not (\code{FALSE}).
#' @param pch (Numeric) Size of the raw data circles.
#' @param colData (Numeric or character) Contour color of the raw data circles.
#' @param colSpline (Numeric or character) Spline line colour.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param basesize (Numeric) Base font size.
#' @param lwd (Numeric) Spline line width.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on y-axis of the fluorescence curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the x-axis of both fluorescence curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim.deriv (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis of the derivative plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Further arguments to refine the generated base R plot.
#'
#' @export plot.flFitSpline
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
plot.flFitSpline <- function(flFitSpline, add=FALSE, raw = TRUE, slope=TRUE, deriv = T, spline = T, log.y = F, basesize = 16,
                             pch=1, colData=1, colSpline="dodgerblue3", cex.point=2, lwd = 0.7,
                             y.lim = NULL, x.lim = NULL, y.lim.deriv = NULL,  n.ybreaks = 6,
                             plot = TRUE, export = FALSE, width = 8, height = ifelse(deriv == TRUE, 8, 6),
                             out.dir = NULL, ...)
  {
  if(methods::is(flFitSpline) != "flFitSpline") stop("flFitSpline needs to be an object created with flFitSpline().")
  # /// check input parameters
  if (is.logical(add)==FALSE)   stop("Need logical value for: add")
  if (is.logical(slope)==FALSE) stop("Need logical value for: slope")
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (is.numeric(cex.point)==FALSE)   stop("Need numeric value for: cex.point")

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
  else{
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
          mu     <- as.numeric(flFitSpline$parameters$max_slope)
          lambda <- as.numeric(flFitSpline$parameters$lambda)

          x <- seq(lambda, max(flFitSpline$"fit.x"), length=200)
          y_tangent <- flFitSpline$parameters["b.tangent"][[1]]+x*mu
          try(lines(x, y_tangent, lty=2, lwd=2, col=ggplot2::alpha(colSpline, 0.85), ...))
          try(lines(c(min(flFitSpline$"raw.x"[1]), lambda), rep(flFitSpline$"raw.fl"[1], 2), lty=2, lwd=2.8*lwd, col=ggplot2::alpha(colSpline, 0.7)))
        }
      }
      if (deriv  == TRUE){
        if ((flFitSpline$control$log.x.spline==FALSE)){
          try( lines(flFitSpline$spline.deriv1$x, flFitSpline$spline.deriv1$y, xlab="", ylab="", col = colSpline, lwd=2.8*lwd) )
        }
        if ((flFitSpline$control$log.x.spline==TRUE)){
          try( lines(flFitSpline$spline.deriv1$x, flFitSpline$spline.deriv1$y, xlab="", ylab="", col = colSpline, lwd=2.8*lwd) )
        }
      }
    } # if (add == TRUE)
    else {
      coef <- flFitSpline[["parameters"]]
      lag <- coef["lambda"][[1]][1]
      # correct for log transformation
      if(flFitSpline$control$log.y.spline == TRUE){
        fit.fl <-
          c(rep(NA, length(flFitSpline[["raw.fl"]]) - length(flFitSpline[["fit.fl"]])), exp(flFitSpline[["fit.fl"]]) *
              flFitSpline[["raw.fl"]][1])
      } else {
        fit.fl <- c(rep(NA, length(flFitSpline[["raw.fl"]]) - length(flFitSpline[["fit.fl"]])), flFitSpline[["fit.fl"]])
      }

      df.raw <- data.frame("x" = flFitSpline[["x.in"]],
                         "data" = flFitSpline[["fl.in"]])
      df.fit <- data.frame("fit.x" = c(rep(NA, length(flFitSpline[["raw.x"]])-length(flFitSpline[["fit.x"]])), flFitSpline[["fit.x"]]),
                           "fit.fl" = fit.fl)

      x.label = if(flFitSpline$control$x_type == "density"){
        "Density"
      } else {
        "Time"
      }
      y.label <- if(flFitSpline$control$norm_fl == TRUE){
        "Norm. fluorescence"
      } else {
        "Fluorescence"
      }
      p <- ggplot(NULL, aes(x=x, y=data)) +
        geom_line(aes(x=fit.x, y = fit.fl, color = "spline"), data = df.fit, size = lwd) +
        xlab(x.label) +
        ylab(label = y.label) +
        theme_classic(base_size = basesize) +
        ggtitle(gsub(" \\| NA", "", paste(flFitSpline$ID, collapse=" | "))) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        theme(legend.key = element_blank(),
              legend.background=element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.90, 0.08),
              plot.title = element_text(size=15),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_color_manual(name='Fluorescence Fit',
                           breaks = "Spline fit",
                           values=c("spline" = ggplot2::alpha(colSpline, 0.85), "Spline fit" = ggplot2::alpha(colSpline, 0.85)))
      if(raw){
        p <- p + geom_point(shape=pch, data = df.raw, shape=1, size = cex.point, alpha = 0.6, stroke=0.15)
      }


      p.yrange.end <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]

      # p <- p +
      #   annotate(
      #     "text",
      #     label = paste("t0:", flFittedSpline$control$t0, "  min.density:", flFittedSpline$control$min.density, "  smoothing:", flFittedSpline$control$smooth.gc),
      #     x = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$x.range[2],
      #     y = 1.2 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
      #     angle = 0, parse = F, size = 3.2)

      if(log.y == TRUE){
        if(!is.null(y.lim)){
          p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks), trans = 'log')
        } else {
          p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks), trans = 'log')
        }
      } else {
        if(!is.null(y.lim)){
          p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks))
        } else {
          p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks))
        }
      }

      if(!is.null(x.lim)){
        p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
      } else {
        p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      }


      # /// add tangent at maximum slope
      if(slope == TRUE && (flFitSpline$control$log.y.spline==F && log.y == F) ||
         (flFitSpline$control$log.y.spline==T && log.y == T)){
        mu     <- as.numeric(coef$max_slope[1])
        if(flFitSpline$fitFlag2){
          lag2 <- coef$lambda2
          fl.x <- flFitSpline$fit.x[which.max(flFitSpline$fit.fl)]
          mu2 <- coef$max_slope2
          if(lag2 < lag){
            # x values for tangent at µmax
            x_start.ndx <- which.min(abs(flFitSpline$fit.x-(coef$x.max-0.15*fl.x)))
            x_start <- flFitSpline$fit.x[x_start.ndx]
            x <- seq(x_start, max(flFitSpline$fit.x), length=200)
            # y values for tangent at µmax
            if(flFitSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*flFitSpline[["raw.fl"]][1])*exp(mu*x)
            } else {
              bla <- coef["b.tangent"][[1]] + (mu*x)
            }
            tangent.df <- data.frame("x" = x,
                                     "y" = bla)
            # x values for tangent at µmax2
            x2 <- seq(ifelse(lag2<0, 0, lag2), max(flFitSpline$"fit.x"), length=200)
            # y values for tangent at µmax
            if(flFitSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*flFitSpline[["raw.fl"]][1])*exp(mu2*x2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (mu2*x2)
            }
            tangent.df2 <- data.frame("x" = x2,
                                      "y" = bla2)
            df.horizontal2 <- data.frame("x" = c(flFitSpline[["raw.x"]][1], lag2),
                                         "y" = flFitSpline[["raw.fl"]][1])

            p <- p + geom_segment(aes(x = x[which.min(abs(bla))], y = y[which.min(abs(bla))],
                                      xend = x[which.min(abs(y - 1.1*p.yrange.end))],
                                      yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                  data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
            p <- p + geom_segment(aes(x = x[which.min(abs(bla2))], y = y[which.min(abs(bla2))],
                                      xend = x[which.min(abs(y - 1.1*p.yrange.end))],
                                      yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                  data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.85), size = 0.5)

            if(!(lag2 <0)){
              p <- p + geom_segment(aes(x = x[1], y = y[1], xend = x[2], yend = y[2]), data = df.horizontal2,
                                    linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.85), size = 0.5)
            }
          } # if(lag2 < lag)
          else {
            # x values for tangent at µmax
            x <- seq(ifelse(lag<0, 0, lag), max(flFitSpline$"fit.x"), length=200)
            # y values for tangent at µmax
            if(flFitSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*flFitSpline[["raw.fl"]][1])*exp(mu*x)
            } else {
              bla <- coef["b.tangent"][[1]] + (mu*x)
            }
            tangent.df <- data.frame("x" = x,
                                     "y" = bla)
            df.horizontal <- data.frame("x" = c(flFitSpline[["raw.x"]][1], lag),
                                        "y" = flFitSpline[["raw.fl"]][1])
            # x values for tangent at µmax2
            x2_start.ndx <- which.min(abs(flFitSpline$fit.x-(coef$x.max2-0.15*fl.x)))
            x2_start <- flFitSpline$fit.x[x2_start.ndx]
            x2 <- seq(x2_start, max(flFitSpline$"fit.x"), length=200)
            # y values for tangent at µmax
            if(flFitSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*flFitSpline[["raw.fl"]][1])*exp(mu2*x2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (mu2*x2)
            }
            tangent.df2 <- data.frame("x" = x2,
                                      "y" = bla2)

            p <- p + geom_segment(aes(x = x[which.min(abs(bla))], y = y[which.min(abs(bla))],
                                      xend = x[which.min(abs(y - 1.1*p.yrange.end))],
                                      yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                  data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
            p <- p + geom_segment(aes(x = x[which.min(abs(bla2))], y = y[which.min(abs(bla2))],
                                      xend = x[which.min(abs(y - 1.1*p.yrange.end))],
                                      yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                  data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.85), size = 0.5)

            # if(!(lag <0)){
            #   p <- p + geom_segment(aes(x = x[1], y = y[1], xend = x[2], yend = y[2]), data = df.horizontal,
            #                         linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
            # }
          }
        } # if(flFitSpline$fitFlag2)
        else {
          # x values for tangent
          x <- seq(ifelse(lag<0, 0, lag), max(flFitSpline$"fit.x"), length=200)
          # y values for tangent
          if(flFitSpline$control$log.y.spline){
            bla <- (exp(coef["b.tangent"][[1]])*flFitSpline[["raw.fl"]][1])*exp(mu*x)
          } else {
            bla <- coef["b.tangent"][[1]] + (mu*x)
          }
          tangent.df <- data.frame("x" = x,
                                   "y" = bla)
          df.horizontal <- data.frame("x" = c(flFitSpline[["raw.x"]][1], lag),
                                      "y" = flFitSpline[["raw.fl"]][1])
          p <- p + geom_segment(aes(x = x[which.min(abs(bla))], y = y[which.min(abs(bla))],
                                    xend = x[which.min(abs(y - 1.1*p.yrange.end))],
                                    yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
          # if(!(lag <0)){
          #   p <- p + geom_segment(aes(x = x[1], y = y[1], xend = x[2], yend = y[2]), data = df.horizontal,
          #                         linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
          # }
        } # else of if(flFitSpline$fitFlag2)
      } # if(slope == TRUE && log.y == T)

      # /// add panel with growth rate over x
      if(deriv == TRUE){
        df.mu <- data.frame(spline(flFitSpline$spline.deriv1$x, flFitSpline$spline.deriv1$y))
        #add missing x values due to min.density and t0
        x.missing <- df.raw[df.raw$x < df.mu$x[1], ]$x
        df.mu <-
          dplyr::bind_rows(data.frame(x = x.missing, y = rep(NA, length(x.missing))),
                    df.mu)

        y.label.mu = if(flFitSpline$control$log.y.spline == TRUE){
          paste0("Slope [d(Ln(F/F0))/d",x.label, "]")
        } else {
          paste0("Slope [dF/d", x.label,"]")
        }
        p.mu <- ggplot(df.mu, aes(x=x, y=y)) +
          geom_line(color = colSpline, size = lwd) +
          theme_classic(base_size = 15) +
          xlab(x.label) +
          ylab(label = y.label.mu) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

        p <- ggpubr::ggarrange(p, p.mu, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1))

      }

      if(export == FALSE && plot == FALSE){
        return(p)
      }
      if (export == TRUE){
        w <- width
        h <- height
        out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
        dir.create(out.dir, showWarnings = F)
        grDevices::png(paste0(out.dir, "/", paste(flFitSpline$ID, collapse = "_"), "_SplineFit.png"),
                       width = w, height = h, units = 'in', res = 300)
        print(p)
        grDevices::dev.off()
        grDevices::pdf(paste0(out.dir, "/", paste(flFitSpline$ID, collapse = "_"), "_SplineFit.pdf"), width = w, height = h)
        print(p)
        grDevices::dev.off()
      }
      if (plot == TRUE){
        print(p)
      }
    } # else of if (add == TRUE)
  } # else of if ((is.na(flFitSpline$fitFlag)==TRUE)|(flFitSpline$fitFlag==FALSE))
}


#' Generic plot function for \code{flBootSpline} objects.
#'
#' @param flBootSpline Object of class \code{flBootSpline}, created with \code{\link{flBootSpline}}.
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
#' @param shiny (Logical) Indicate if plot is generated within the shiny app.
#' @param ... Further arguments to refine the generated base R plot.
#'
#' @export plot.flBootSpline
#' @export
#'
plot.flBootSpline <- function(flBootSpline, pch=1, colData=1, deriv = TRUE,
                              colSpline=ggplot2::alpha("dodgerblue3", 0.2),
                              cex.point = 1, cex.lab = 1.5, cex.axis = 1.3,
                              lwd = 2, y.lim = NULL, x.lim = NULL, y.lim.deriv = NULL,
                              plot = TRUE, export = FALSE,
                              height = 7, width = 9, out.dir = NULL, shiny = FALSE, ...)
{
  # flBootSpline an object of class flBootSpline
  if(methods::is(flBootSpline) != "flBootSpline") stop("flBootSpline needs to be an object created with flBootSpline().")
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
    empty.plot()
  }
  else{
    p1 <- function()
    {
      par(cex.lab = cex.lab, cex.axis = cex.axis)
      par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
      colSpline <- rep(colSpline, (flBootSpline$control$nboot.fl%/%length(colSpline))+1)

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
        plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = T, lwd = lwd,
                         deriv = FALSE, plot = F, export = F, pch=0, colSpline=colSpline[i], cex.point = cex.point)
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
          plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = F, lwd = lwd, xlim = x.lim,
                           deriv = T, plot = F, export = F, pch=0, colSpline=colSpline[i], cex.point = cex.point)
        }
        title(ylab = "First derivative", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      if(flBootSpline$control$x_type == "density"){
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
      colSpline <- rep(colSpline, (flBootSpline$control$nboot.fl%/%length(colSpline))+1)

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
        plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = T, lwd = lwd,
                         deriv = FALSE, plot = F, export = F, pch=0, colSpline=colSpline[i], cex.point = cex.point)
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
          plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = F, lwd = lwd, xlim = x.lim,
                           deriv = T, plot = F, export = F, pch=0, colSpline=colSpline[i], cex.point = cex.point)
        }
        title(ylab = "First derivative", line = 1 + 0.5*cex.lab+0.5*cex.axis, cex.lab = cex.lab)
      }
      if(flBootSpline$control$x_type == "density"){
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
  if (export == TRUE){
    w1 <- width
    h1 <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = F)
    grDevices::png(paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSpline.png"),
                   width = w1, height = h1, units = 'in', res = 300)
    p1()
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSpline.pdf"))
    p1()
    grDevices::dev.off()

    w2 <- width
    h2 <- width
    grDevices::png(paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSplineParam.png"),
                   width = w2, height = h2, units = 'in', res = 300)
    p2()
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", paste(flBootSpline$gcID, collapse = "_"), "_flBootSplineParam.pdf"))
    p2()
    grDevices::dev.off()
  }

  if (plot == TRUE){
    if(!shiny){
      p1()
      dev.new()
      p2()
    } else {
      p3()
    }
  }
  # restore standard plot parameters
  par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
  par(mfrow = c(1, 1))
}

#' Generic plot function for \code{drFitModel} objects.
#'
#' @param drFittedModel Object of class \code{drFitModel}, created with \code{\link{fl.drFitModel}}.
#' @param ec50line (Logical) Show pointed horizontal and vertical lines at the EC50 value (\code{TRUE}) or not (\code{FALSE}).
#' @param log ("x", "y", or "xy") Display the x- or y-axis on a logarithmic scale.
#' @param pch (Numeric) Size of the raw data circles.
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
#' @export plot.drFitModel
#' @export
#'
plot.drFitModel <- function(drFittedModel, ec50line = TRUE, log = c("xy"), pch = 1,
                            colSpline = 1, colData = 1, cex.point = 1, cex.lab = 1.5,
                            cex.axis = 1.3, y.lim = NULL, x.lim = NULL,
                            lwd = 2, plot = TRUE, export = FALSE,
                            height = 7, width = 9, out.dir = NULL,
                            ...)
{
  # drFittedModel an object of class drFittedModel
  if(methods::is(drFittedModel) != "drFitModel") stop("drFittedModel needs to be an object created with fl.drFitModel().")
  # /// check input parameters
  if (is.logical(ec50line) == FALSE)
    stop("Need logical value for: ec50line")
  if (is.numeric(pch) == FALSE)
    stop("Need numeric value for: pch")
  if (is.numeric(cex.point) == FALSE)
    stop("Need numeric value for: cex.point")
  conc <- drFittedModel$raw.conc
  p <- function(){
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))

    par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1))
    par(cex.lab = cex.lab, cex.axis = cex.axis)
    if ((drFittedModel$control$log.x.dr == TRUE) && (drFittedModel$control$log.y.dr == TRUE)) {
      plot(
        log(conc + 1),
        log(drFittedModel$raw.test + 1),
        log = log,
        col = colData,
        xlab = "ln(1+concentration)",
        ylab = "ln(1+response)",
        type = "n", xlim = x.lim, ylim = y.lim, ...
      )
      points(log(conc + 1), log(drFittedModel$raw.test + 1), cex = cex.point, pch = pch)
    }
    else
    {
      if ((drFittedModel$control$log.x.dr == FALSE) && (drFittedModel$control$log.y.dr == TRUE)) {
        plot(
          conc,
          log(drFittedModel$raw.test + 1),
          log = log,
          col = colData,
          xlab = "concentration",
          ylab = "ln(1+response)",
          type = "n", xlim = x.lim, ylim = y.lim, ...
        )
        points(conc, log(drFittedModel$raw.test + 1), cex = cex.point, pch = pch)
      }
      else
      {
        if ((drFittedModel$control$log.x.dr == TRUE) && (drFittedModel$control$log.y.dr == FALSE)) {
          plot(
            log(conc + 1),
            drFittedModel$raw.test,
            log = log,
            col = colData,
            xlab = "Ln(1+concentration)",
            ylab = paste0("Response", ifelse(!is.na(drFittedModel$parameters$test), paste0(" (", drFittedModel$parameters$test, ")"), "")),
            type = "n", xlim = x.lim, ylim = y.lim, ...
          )
          points(log(conc + 1), drFittedModel$raw.test, cex = cex.point, pch = pch)
        }
        else
        {
          if ((drFittedModel$control$log.x.dr == FALSE) && (drFittedModel$control$log.y.dr == FALSE)) {
            if(any(grep("x", log))){
              conc[conc==0] <- drFittedModel$fit.conc[2]
            }
            mean <- sapply(1:length(conc), function(x) mean(drFittedModel$raw.test[conc==unique(conc)[x]]))
            mean <- mean[!is.na(mean)]
            sd <- sapply(1:length(conc), function(x) sd(drFittedModel$raw.test[conc==unique(conc)[x]]))
            sd <- sd[!is.na(sd)]
            plot(
              unique(conc)[order(unique(conc))],
              mean,
              log = log,
              col = colData,
              xlab = "Concentration",
              ylab = paste0("Response", ifelse(!is.na(drFittedModel$parameters$test), paste0(" (", drFittedModel$parameters$test, ")"), "")),
              type = "n", xlim = x.lim, ylim = y.lim, ...
            )
            points(unique(conc)[order(unique(conc))], mean, cex = cex.point, pch = pch)
            if(length(sd)>0){
            try(arrows(x0=unique(conc)[order(unique(conc))], y0=mean-sd,
                   x1=unique(conc)[order(unique(conc))], y1=mean+sd, code=3, angle=90, length=0.1), silent = T)
            }
          }
        }
      }
    }

    try(lines(
      drFittedModel$fit.conc,
      drFittedModel$fit.test,
      lwd = lwd,
      col = colSpline
    ), silent = F)

    if (ec50line == TRUE) {
      #vertical lines
      totmin = min(min(drFittedModel$fit.conc), min(drFittedModel$fit.test))
      lines(c(drFittedModel$parameters$K, drFittedModel$parameters$K),
            c(1, drFittedModel$parameters$yEC50),
            lty = 2, lwd = lwd)
      #horizontal
      lines(c(ifelse(any(grep("x", log)), 0.001, -1), drFittedModel$parameters$K),
            c(drFittedModel$parameters$yEC50, drFittedModel$parameters$yEC50),
            lty = 2, lwd = lwd)
    }
    title(main = drFittedModel$drID)
  } # p <- function()
  if (export == TRUE){
    w <- width
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = F)
    grDevices::png(paste0(out.dir, "/", paste(drFittedModel$drID, collapse = "_"), "_drFitModel.png"),
                   width = w, height = h, units = 'in', res = 300)
    p()
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", paste(drFittedModel$drID, collapse = "_"), "_drFitModel.pdf"))
    p()
    grDevices::dev.off()
  }

  if (plot == TRUE){
    if(drFittedModel$fitFlag == TRUE) p()
    else stop(paste0("Model could not be fitted to the data for: ", drFittedModel$drID, "."))
  }
}

#' Combine different groups of samples into a single plot
#'
#' Visualize fluorescence, normalized fluorescence, or spline fits of multiple sample groups in a single plot.
#'
#' @param object A \code{flFitRes}, \code{flFit}, or \code{grodata} object created with \code{\link{fl.workflow}} containing fluorescence data.
#' @param data.type (Character) Indicate, which type of fluorescence data should be displayed.
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
#' @param basesize (Numeric) Base font size.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the fluorescence curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the x-axis of both fluorescence curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.title (Character) Optional: Provide a title for the y-axis of the fluorescence curve plot.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both fluorescence curve and derivative plots.
#' @param y.lim.deriv (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis of the derivative plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.title.deriv (Character) Optional: Provide a title for the y-axis of the derivative plot.
#' @param lwd (Numeric) Line width of the individual plots.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#' @param shiny (Logical) Indicate if plot is generated within the shiny app.
#'
#' @export plot.flFitRes
#' @export
#'
plot.flFitRes <-  function(object,
                        data.type = c("spline1", "spline2", "raw1", "raw2", "norm.fl1", "norm.fl2"),
                        IDs = NULL,
                        names = NULL,
                        conc = NULL,
                        mean = TRUE,
                        exclude.nm = NULL,
                        exclude.conc = NULL,
                        log.y = F,
                        deriv = F,
                        n.ybreaks = 6,
                        colors = NULL,
                        basesize = 20,
                        y.lim = NULL,
                        x.lim = NULL,
                        y.title = NULL,
                        x.title = NULL,
                        y.lim.deriv = NULL,
                        y.title.deriv = NULL,
                        lwd = 1.1,
                        plot = TRUE,
                        export = FALSE,
                        height = NULL,
                        width = NULL,
                        out.dir = NULL,
                        out.nm = NULL,
                        shiny = FALSE
)
{
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

  if(!any(methods::is(object) %in% c("flFit","flFitRes", "grodata"))) stop("'object' needs to be an object created with fl.workflow(), flFit(), parse_data(), or read_data().")
  if(methods::is(object) == "grodata" && !any(data.type %in% c("raw1", "raw2", "norm.fl1", "norm.fl2"))) stop("Raw input data can only be used to visualize data.type 'raw1', 'raw2', 'norm.fl1', or 'norm.fl2'.")

  data.type <- match.arg(data.type)
  if(data.type == "raw1" || data.type == "raw2" || data.type == "norm.fl1" || data.type == "norm.fl2" && deriv ==TRUE){
    warning("Derivatives cannot be calculated for 'raw' or 'norm.fl' data. Only the fluorescence values will be shown.")
    deriv = FALSE
  }
  if(methods::is(object) == "grodata"){
    raw_data <- object
  }
  if(methods::is(object) == "flFitRes"){
    if(data.type == "spline1" || data.type == "raw1" || data.type == "norm.fl1") flFit <- object$flFit1
    if(data.type == "spline2" || data.type == "raw2" || data.type == "norm.fl2") flFit <- object$flFit2
    raw_data <- object$data
  } else {
    flFit <- object
  }

  # /// check input parameters
  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(lwd)==FALSE)   stop("Need numeric value for: lwd")
  if(data.type == "spline1" || data.type == "spline2"){
    if (!("s" %in% flFit$control$fit.opt | "a" %in% flFit$control$fit.opt)) stop("To plot spline fit results, please run fl.workflow() with 's' in fit.opt.")
  }

  conc <- as.numeric(conc)
  exclude.conc <- as.numeric(exclude.conc)

  # Get name of conditions with multiple replicates
  if(any(methods::is(object) %in% c("flFit","flFitRes"))){
    sample.nm <- nm <- as.character(names(flFit$flFittedSplines))
  } else {
    if(data.type == "norm.fl1"){
      sample.nm <- nm <- paste(object$norm.fluorescence1[,1], object$norm.fluorescence1[,2], object$norm.fluorescence1[,3], sep = " | ")
    }
    if(data.type == "norm.fl2"){
      sample.nm <- nm <- paste(object$norm.fluorescence2[,1], object$norm.fluorescence2[,2], object$norm.fluorescence2[,3], sep = " | ")
    }
    if(data.type == "raw1"){
      sample.nm <- nm <- paste(object$fluorescence1[,1], object$fluorescence1[,2], object$fluorescence1[,3], sep = " | ")
    }
    if(data.type == "raw2"){
      sample.nm <- nm <- paste(object$fluorescence2[,1], object$fluorescence2[,2], object$fluorescence2[,3], sep = " | ")
    }
  }
  if(data.type == "norm.fl1") data.nm = "norm.fluorescence1"
  if(data.type == "norm.fl2") data.nm = "norm.fluorescence2"
  if(data.type == "raw1") data.nm = "fluorescence1"
  if(data.type == "raw2") data.nm = "fluorescence2"

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
        names <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1",  names)
        nm <- nm[grep(paste(names, collapse="|"), nm)]
      }
    }
    if(!is.null(exclude.nm)  && length(exclude.nm) > 0){
      if(!is.na(exclude.nm) && exclude.nm != ""){
        names.excl <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", exclude.nm)
        nm <- nm[!grepl(paste(names.excl, collapse="|"), gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm))]
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
  ndx.filt <- unlist(filter.ls, recursive = F)
  remove <- c()
  for(i in 1:length(ndx.filt)){
    if(length(ndx.filt[[i]]) == 0) remove <- c(remove, i)
  }
  if(!is.null(remove)) ndx.filt <- ndx.filt[-remove]
  # Check FitFlag for each replicate, work per condition
  if(data.type == "spline1" || data.type == "spline2"){
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

  if(data.type == "spline1"  || data.type == "spline2"){
    # correct for log transformation
    if(flFit$control$log.y.spline == TRUE){
      for(i in 1:length(ndx.keep)){
        flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]] <-
          exp(flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]]) * flFit$flFittedSplines[[ndx.keep[i]]]$data.in[1]
      }
    }
  }
if((data.type == "spline1" || data.type == "spline2") && flFit$control$x_type == "density" && mean == TRUE){
  message("Grouping of replicates is not supported for spline fits with x_type = 'density'. Argument changed to mean = FALSE.")
  mean <- FALSE
}
  if(mean == TRUE){
    # Combine replicates via their mean and standard deviation
    conditions <- str_replace_all(nm, "\\| .+ \\| ", "| ")
    conditions_unique <- unique(conditions)

    # Create lists for each selected condition, with density values and derivatives, respectively. Each list item represents one condition with their average and SD
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
      # Create lists for density and time values for each sample
      if(data.type == "spline1"  || data.type == "spline2"){
        time <- lapply(1:length(ndx), function(i) cbind(flFit$flFittedSplines[[ndx[[i]]]]$fit.x)) %>% as.list(.)
        data <- lapply(1:length(ndx), function(i) cbind(flFit$flFittedSplines[[ndx[[i]]]]$fit.fl)) %>% as.list(.)
      } else {
        if(methods::is(object) %in% "flFitRes"){
            time <- lapply(1:length(ndx), function(i) raw_data$time[ndx[i], ])
            data <- raw_data[[data.nm]][ndx, 4:ncol(raw_data[[data.nm]])]
        } else {
          time <- lapply(1:length(ndx), function(i) cbind(raw_data$time[ndx[[i]], ])) %>% as.list(.)
          data <- raw_data[[data.nm]][ndx, 4:ncol(raw_data[[data.nm]])]
        }
        data <- split(as.matrix(data), 1:nrow(as.matrix(data)))
        data <- lapply(1:length(data), function(i) as.numeric(data[[i]]))
      }

      # Create lists for derivatives and time values for each sample
      if(deriv){
        time.deriv <- lapply(1:length(ndx), function(i) cbind(flFit$flFittedSplines[[ndx[[i]]]]$spline.deriv1$x)) %>% as.list(.)
        data.deriv <- lapply(1:length(ndx), function(i) cbind(flFit$flFittedSplines[[ndx[[i]]]]$spline.deriv1$y)) %>% as.list(.)
      }
      # correct for unequal lengths of data series
      time.all <- Reduce(union, time)
      for(i in 1:length(time)){
        assign(paste0("time.missing_", i), setdiff(time.all, time[[i]]) )
        if(length(get(paste0("time.missing_", i))) > 0){
          for(j in 1:length(get(paste0("time.missing_", i)))){
            # extract density values into a separate list
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
      avg <- rowMeans(data, na.rm = F)
      sd <- apply(data, 1, sd, na.rm = F)
      plotdata.ls[[n]] <- data.frame(name = name, time = time, mean = avg, upper = avg+sd, lower = avg-sd)
      if(deriv){
        time.deriv <- time.deriv[[1]]
        data.deriv <- do.call("cbind", data.deriv)
        avg.deriv <- rowMeans(data.deriv, na.rm = F)
        sd.deriv <- apply(data.deriv, 1, sd, na.rm = F)
       deriv.ls[[n]] <- data.frame(name = name, time = time.deriv, mean = avg.deriv, upper = avg.deriv+sd.deriv, lower = avg.deriv-sd.deriv)
      }
    } # for(n in 1:length(conditions_unique))
    names(plotdata.ls) <- gsub(" \\| NA", "", conditions_unique)
    if(deriv){
      names(deriv.ls) <- gsub(" \\| NA", "", conditions_unique)
      deriv.ls <- deriv.ls[!is.na(deriv.ls)]
      df.deriv <- do.call(rbind.data.frame, deriv.ls)
      df.deriv$name <- gsub(" \\| NA", "", df.deriv$name)
      df.deriv$name <- factor(df.deriv$name, levels = unique(factor(df.deriv$name)))
    }

    plotdata.ls <- plotdata.ls[!is.na(plotdata.ls)]
    df <- do.call(rbind.data.frame, plotdata.ls)
    df$name <- gsub(" \\| NA", "", df$name)
    df$name <- factor(df$name, levels = unique(factor(df$name)))
    df <- df[df[["mean"]]>0, ]
    df <- df[!apply(df, 1, function(x){all(is.na(x))}),]
    if(!is.null(x.lim)) df <- df[df[["time"]]>x.lim[1], ]

    # replace negative lower ribbon boundaries with 0 for log10 transformation
    if(log.y==TRUE){
      df$lower[df$lower<0] <- 0
    }
    xlab.title <- if(data.type == "norm.fl1" || data.type == "norm.fl2" || data.type == "raw1" || data.type == "raw2"){
      "Time"
    } else if (flFit$control$x_type == "density"){
      "Density"
    } else {
      "Time"
    }
    ylab.title <- if(data.type == "norm.fl1"){
      "Normalized fluorescence 1"
    } else if(data.type == "norm.fl2"){
      "Normalized fluorescence 2"
    } else if(data.type == "raw1"){
      "Fluorescence 1"
    } else if(data.type == "raw2" || data.type == "spline2"){
      "Fluorescence 2"
    } else if(data.type == "spline1" && flFit$control$norm_fl){
      "Normalized fluorescence 1"
    } else if(data.type == "spline1" && !flFit$control$norm_fl){
      "Fluorescence 1"
    } else if(data.type == "spline2" && flFit$control$norm_fl){
      "Normalized fluorescence 2"
    } else if(data.type == "spline2" && !flFit$control$norm_fl){
      "Fluorescence 2"
    }
    p <- ggplot(df, aes(x=time, y=mean, col = name)) +
      geom_line(size=lwd) +
      geom_ribbon(aes(ymin=lower,ymax=upper, fill=name), alpha = 0.3, colour = NA) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title) || y.title == "", ylab.title, y.title)) +
      theme(legend.position="bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    if(shiny == TRUE) p <- p + ggplot2::guides(fill=guide_legend(ncol=4))
    else p <- p + ggplot2::guides(fill=guide_legend(ncol=2))

    if(log.y == TRUE){
      if(!is.null(y.lim)){
        p <- p + scale_y_log10(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
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
      if (length(plotdata.ls) <= 8) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else if (length(plotdata.ls) <=50){
        p <- p + scale_fill_manual(name = "Condition",
                                   values = c(
                                     "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                     "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                     "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                     "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                     "green1", "yellow4", "yellow3", "darkorange4", "brown", "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                     "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                     "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                     "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                     "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                   )
        ) + scale_color_manual(name = "Condition",
                               values = c(
                                 "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                 "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                 "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                 "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                 "green1", "yellow4", "yellow3", "darkorange4", "brown", "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                 "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                 "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                 "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                 "green1", "yellow4", "yellow3", "darkorange4", "brown"
                               )
        )
      }
    }
    if(deriv){
      # /// add panel with growth rate over time
      if(!is.null(x.lim)){
        df.deriv <- df.deriv[df.deriv[,"time"]>=x.lim[1]&df.deriv[,"time"]<=x.lim[2],]
      }
      p.deriv <- ggplot(df.deriv, aes(x=time, y=mean, col = name)) +
        geom_line(size=lwd) +
        geom_ribbon(aes(ymin=lower,ymax=upper, fill=name), alpha = 0.3, colour = NA) +
        theme_classic(base_size = basesize) +
        xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
        theme(legend.position="bottom",
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
        } else if (length(plotdata.ls) <=50){
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                                 values = c(
                                                   "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                                   "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                                   "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                                   "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                                   "green1", "yellow4", "yellow3", "darkorange4", "brown", "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                                   "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                                   "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                                   "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                                   "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                                 )
          ) + scale_color_manual(name = "Condition",
                                 values = c(
                                   "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                   "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                   "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                   "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                   "green1", "yellow4", "yellow3", "darkorange4", "brown", "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                   "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                   "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                   "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                   "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                 )
          )
        }
      }
      p <- ggpubr::ggarrange(p, p.deriv, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1), common.legend = T, legend = "bottom", legend.grob = ggpubr::get_legend(p, position = "right"))
    }
  } # if(mean == TRUE)
  else {
    df <- data.frame()
    for(i in 1:length(ndx.keep)){
      if(data.type == "spline1"  || data.type == "spline2"){
        df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                              "time" = flFit$flFittedSplines[[ndx.keep[i]]][["fit.x"]],
                                              "y" = flFit$flFittedSplines[[ndx.keep[i]]][["fit.fl"]]))
      } else {
        if(any(methods::is(object) %in% c("flFit","flFitRes"))){
          df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                                "time" = as.vector(raw_data$time[ndx.keep[i], ]),
                                                "y" = unlist(unname(type.convert(raw_data[[data.nm]][ndx.keep[i], 4:ncol(raw_data[[data.nm]])], as.is=T)))))
        } else {
          df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                                "time" = as.vector(object$time[ndx.keep[i], ]),
                                                "y" = unlist(unname(type.convert(object[[data.nm]][ndx.keep[i], 4:ncol(object[[data.nm]])], as.is=T)))))
        }
      }
    } # if(data.type == "spline1"  || data.type == "spline2")
    df <- df[df[["y"]]>0, ]
    if(!is.null(x.lim)) df <- df[df[["time"]]>x.lim[1], ]
    xlab.title <- if(data.type == "norm.fl1" || data.type == "norm.fl2" || data.type == "raw1" || data.type == "raw2"){
      "Time"
    } else if (object$control$x_type == "density"){
      "Density"
    } else {
      "Time"
    }
    ylab.title <- if(data.type == "norm.fl1"){
      "Normalized fluorescence 1"
    } else if(data.type == "norm.fl2"){
      "Normalized fluorescence 2"
    } else if(data.type == "raw1"){
      "Fluorescence 1"
    } else if(data.type == "raw2" || data.type == "spline2"){
      "Fluorescence 2"
    } else if(data.type == "spline1" && flFit$control$norm_fl){
      "Normalized fluorescence 1"
    } else if(data.type == "spline1" && !flFit$control$norm_fl){
      "Fluorescence 1"
    } else if(data.type == "spline2" && flFit$control$norm_fl){
      "Normalized fluorescence 2"
    } else if(data.type == "spline2" && !flFit$control$norm_fl){
      "Fluorescence 2"
    }
    p <- ggplot(df, aes(x=time, y=y, col = name)) +
      geom_line(size=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title) || y.title == "", ylab.title, y.title)) +
      theme(legend.position="bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    if(shiny == TRUE) p <- p + ggplot2::guides(fill=guide_legend(ncol=4))
    else p <- p + ggplot2::guides(fill=guide_legend(ncol=2))

    if(!is.null(x.lim)){
      p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
    } else {
      p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }

    if(log.y == TRUE){
      if(!is.null(y.lim)){
        p <- p + scale_y_log10(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
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
      } else if (length(ndx.keep) <=50){
        p <- p + scale_fill_manual(name = "Condition",
                                   values = c(
                                     "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                     "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                     "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                     "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                     "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                   )
        ) + scale_color_manual(name = "Condition",
                               values = c(
                                 "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                 "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                 "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                 "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                 "green1", "yellow4", "yellow3", "darkorange4", "brown"
                               )
        )
      }
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
      p.deriv <- ggplot(df.deriv, aes(x=time, y=y, col = name)) +
        geom_line(size=lwd) +
        theme_classic(base_size = basesize) +
        xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
        theme(legend.position="bottom",
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
        } else if (length(ndx.keep) <=50){
          p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                                 values = c(
                                                   "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                                   "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                                   "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                                   "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                                   "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                                 )
          ) + scale_color_manual(name = "Condition",
                                 values = c(
                                   "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                   "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                   "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                   "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                   "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                 )
          )
        }
      }
      p <- ggpubr::ggarrange(p, p.deriv, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1), common.legend = T, legend = "bottom", legend.grob = ggpubr::get_legend(p))
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
    dir.create(out.dir, showWarnings = F)
    grDevices::png(paste0(out.dir, "/", out.nm, ".png"),
                   width = w, height = h, units = 'in', res = 300)
    print(p)
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", out.nm, ".pdf"), width = w, height = h)
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
plot.flFit <- plot.flFitRes

#' @rdname plot.flFitRes
#' @export plot.grodata
#' @export
plot.grodata <- plot.flFitRes

#' Compare fluorescence and density over time
#'
#' \code{plot.dual} creates a two-panel plot in which fluorescence or density values are shown over time, allowing for the identification of, e.g., expression patterns in different growth stages.
#'
#' @param object A \code{flFit}, \code{flFitRes}, or \code{grodata} object created with \code{\link{flFit}}, \code{\link{fl.workflow}} or \code{\link{read_data}}
#' @param fluorescence (Character) Indicate, which type of fluorescence data should be displayed.
#' @param IDs (String or vector of strings) Define samples or groups (if \code{mean = TRUE}) to combine into a single plot based on exact matches with entries in the \code{label} or \code{condition} columns of \code{grofit$expdesign}.
#' @param names (String or vector of strings) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered. Note: Ensure to use unique concentration values to extract groups of interest. If the concentration value of one condition is included in its entirety within the name of other conditions (e.g., the dataset contains '1', '10', and '100', \code{code = 10} will select both '10 and '100'), it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define groups to exclude from the plot. Partial matches with sample/group names are accepted.
#' @param exclude.conc (Numeric or numeric vector) Define concentrations to exclude from the plot.
#' @param mean (Logical) Display the mean and standard deviation of groups with replicates (\code{TRUE}) or plot each sample individually (\code{FALSE})?
#' @param log.y.density (Logical) Log-transform the y-axis of the density plot (\code{TRUE}) or not (\code{FALSE})?
#' @param log.y.fl (Logical) Log-transform the y-axis of the fluorescence plot (\code{TRUE}) or not (\code{FALSE})?
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param colors (vector of strings) Define a color palette used to draw the plots. If \code{NULL}, default palettes are chosen based on the number of groups/samples within the plot. Note: The number of provided colors should at least match the number of groups/samples.
#' @param basesize (Numeric) Base font size.
#' @param y.lim.density (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the density plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim.fl (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the fluorescence plot as a vector in the form \code{c(l, u)}.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the x-axis of both fluorescence and density plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both growth curve and derivative plots.
#' @param y.title.density (Character) Optional: Provide a title for the y-axis of the density plot.
#' @param y.title.fl (Character) Optional: Provide a title for the y-axis of the fluorescence plot.
#' @param lwd (Numeric) Line width of the individual plots.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#' @param shiny (Logical) Indicate if plot is generated within the shiny app.
#'
#' @export plot.dual
#' @export
#'
plot.dual <-  function(object,
                       fluorescence = c("fl1", "fl2", "norm.fl1", "norm.fl2"),
                       IDs = NULL,
                       names = NULL,
                       conc = NULL,
                       mean = TRUE,
                       exclude.nm = NULL,
                       exclude.conc = NULL,
                       log.y.density = F,
                       log.y.fl = F,
                       n.ybreaks = 6,
                       colors = NULL,
                       basesize = 20,
                       y.lim.density = NULL,
                       y.lim.fl = NULL,
                       x.lim = NULL,
                       x.title = NULL,
                       y.title.density = NULL,
                       y.title.fl = NULL,
                       lwd = 1.1,
                       plot = TRUE,
                       export = FALSE,
                       height = NULL,
                       width = NULL,
                       out.dir = NULL,
                       out.nm = NULL,
                       shiny = FALSE
)
{
  # Convert range  and selecting arguments
  x.lim <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", x.lim)), pattern = ";|,"))
  y.lim.fl <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", y.lim.fl)), pattern = ";|,"))
  y.lim.density <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", y.lim.density)), pattern = ";|,"))
  suppressWarnings(assign("x.lim" ,as.numeric(x.lim)))
  if(all(is.na(x.lim))) x.lim <- NULL
  suppressWarnings(assign("y.lim.fl" ,as.numeric(y.lim.fl)))
  if(all(is.na(y.lim.fl))) y.lim.fl <- NULL
  suppressWarnings(assign("y.lim.density" ,as.numeric(y.lim.density)))
  if(all(is.na(y.lim.density))) y.lim.density <- NULL

  if(!any(methods::is(object) %in% c("flFit","flFitRes", "grodata"))) stop("'object' needs to be an object created with fl.workflow(), flFit(), parse_data(), or read_data().")
  density <- density.nm <- "density"
  fluorescence <- match.arg(fluorescence)

  if(methods::is(object) == "grodata"){
    raw_data <- object
  }
  if(methods::is(object) == "flFitRes"){
    if(length(grep("1", fluorescence))>0) flFit <- object$flFit1
    if(length(grep("2", fluorescence))>0) flFit <- object$flFit2
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
    sample.nm <- nm <- paste(object$fluorescence1[,1], object$fluorescence1[,2], object$fluorescence1[,3], sep = " | ")
  }
  if(fluorescence == "fl1") fl.nm = "fluorescence1"
  if(fluorescence == "fl2") fl.nm = "fluorescence1"
  if(fluorescence == "norm.fl1") fl.nm = "norm.fluorescence1"
  if(fluorescence == "norm.fl2") fl.nm = "norm.fluorescence2"

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
  ndx.filt <- unlist(filter.ls, recursive = F)

  # get indices of samples with selected names
  ndx.keep <- grep(paste0(
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", nm), collapse = "|"), sample.nm)

  if(mean == TRUE){
    # Combine replicates via their mean and standard deviation
    conditions <- str_replace_all(nm, "\\| .+ \\| ", "| ")
    conditions_unique <- unique(conditions)

    # Create lists for each selected condition, with density values and (normalized) fluorescence, respectively. Each list item represents one condition with their average and SD
    plotdata.ls <- list()
    for(n in 1:length(conditions_unique)){
      # find indexes of replicates
      ndx <- intersect(ndx.keep, grep(paste0("^",
                                             gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", unlist(str_split(conditions_unique[n], " \\| "))[1]),
                                             " \\|.+[[:space:]]",
                                             unlist(str_split(conditions_unique[n], " \\| "))[2],
                                             "$"), sample.nm))
      name <- conditions_unique[n]
      # Create lists for density and time values for each sample

      if(methods::is(object) %in% "flFitRes"){
        time <- lapply(1:length(ndx), function(i) raw_data$time[ndx[i], ])
        dens.data <- raw_data[[density.nm]][ndx, 4:ncol(raw_data[[density.nm]])]
        fl.data <- raw_data[[fl.nm]][ndx, 4:ncol(raw_data[[fl.nm]])]
      } else {
        time <- lapply(1:length(ndx), function(i) cbind(raw_data$time[ndx[[i]], ])) %>% as.list(.)
        dens.data <- raw_data[[density.nm]][ndx, 4:ncol(raw_data[[density.nm]])]
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
            # extract density values into a separate list
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
      dens.avg <- rowMeans(dens.data, na.rm = F)
      dens.sd <- apply(dens.data, 1, sd, na.rm = F)
      fl.data <- do.call("cbind", fl.data)
      fl.avg <- rowMeans(fl.data, na.rm = F)
      fl.sd <- apply(fl.data, 1, sd, na.rm = F)
      plotdata.ls[[n]] <- data.frame(name = name, time = time,
                                     dens.mean = dens.avg, dens.upper = dens.avg+dens.sd, dens.lower = dens.avg-dens.sd,
                                     fl.mean = fl.avg, fl.upper = fl.avg+fl.sd, fl.lower = fl.avg-fl.sd)
    } # for(n in 1:length(conditions_unique))
    names(plotdata.ls) <- gsub(" \\| NA", "", conditions_unique)

    plotdata.ls <- plotdata.ls[!is.na(plotdata.ls)]
    df <- do.call(rbind.data.frame, plotdata.ls)
    df$name <- gsub(" \\| NA", "", df$name)
    df$name <- factor(df$name, levels = unique(factor(df$name)))
    df <- df[df[["dens.mean"]]>0, ]
    df <- df[df[["fl.mean"]]>0, ]
    df <- df[!apply(df, 1, function(x){all(is.na(x))}),]
    if(!is.null(x.lim)) df <- df[df[["time"]]>x.lim[1], ]

    # replace negative lower ribbon boundaries with 0 for log10 transformation
    if(log.y.density==TRUE){
      df$dens.lower[df$dens.lower<0] <- 0
    }
    if(log.y.fl==TRUE){
      df$fl.lower[df$fl.lower<0] <- 0
    }

    xlab.title <- "Time"
    ylab.title.dens <- "Density"

    p <- ggplot(df, aes(x=time, y=dens.mean, col = name)) +
      geom_line(size=lwd) +
      geom_ribbon(aes(ymin=dens.lower,ymax=dens.upper, fill=name), alpha = 0.3, colour = NA) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title.density) || y.title.density == "", ylab.title.dens, y.title.density)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    if(shiny == TRUE) p <- p + ggplot2::guides(fill=guide_legend(ncol=4))
    else p <- p + ggplot2::guides(fill=guide_legend(ncol=2))

    if(log.y.density == TRUE){
      if(!is.null(y.lim.density)){
        p <- p + scale_y_log10(limits = y.lim.density, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    } else {
      if(!is.null(y.lim.density)){
        p <- p + scale_y_continuous(limits = y.lim.density, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
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
      if (length(plotdata.ls) <= 8) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else if (length(plotdata.ls) <=50){
        p <- p + scale_fill_manual(name = "Condition",
                                   values = c(
                                     "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                     "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                     "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                     "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                     "green1", "yellow4", "yellow3", "darkorange4", "brown", "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                     "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                     "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                     "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                     "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                   )
        ) + scale_color_manual(name = "Condition",
                               values = c(
                                 "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                 "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                 "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                 "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                 "green1", "yellow4", "yellow3", "darkorange4", "brown", "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                 "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                 "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                 "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                 "green1", "yellow4", "yellow3", "darkorange4", "brown"
                               )
        )
      }
    }
    # /// add panel with fluorescence over time
    p.fl <- ggplot(df, aes(x=time, y=fl.mean, col = name)) +
      geom_line(size=lwd) +
      geom_ribbon(aes(ymin=fl.lower,ymax=fl.upper, fill=name), alpha = 0.3, colour = NA) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      theme(panel.grid.major = element_blank(),
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
      } else if (length(plotdata.ls) <=50){
        p.fl <- p.fl + scale_fill_manual(name = "Condition",
                                         values = c(
                                           "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                           "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                           "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                           "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                           "green1", "yellow4", "yellow3", "darkorange4", "brown", "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                           "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                           "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                           "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                           "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                         )
        ) + scale_color_manual(name = "Condition",
                               values = c(
                                 "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                 "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                 "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                 "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                 "green1", "yellow4", "yellow3", "darkorange4", "brown", "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                 "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                 "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                 "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                 "green1", "yellow4", "yellow3", "darkorange4", "brown"
                               )
        )
      }
    }
    p <- ggpubr::ggarrange(p, p.fl, ncol = 1, nrow = 2, align = "v", heights = c(2,2), common.legend = T, legend = "bottom", legend.grob = ggpubr::get_legend(p))
  } # if(mean == TRUE)
  else {
    df <- data.frame()
    for(i in 1:length(ndx.keep)){
      df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                            "time" = as.vector(raw_data$time[ndx.keep[i], ]),
                                            "density" = unlist(unname(type.convert(raw_data[[density.nm]][ndx.keep[i], 4:ncol(raw_data[[density.nm]])], as.is=T))),
                                            "fl" = unlist(unname(type.convert(raw_data[[fl.nm]][ndx.keep[i], 4:ncol(raw_data[[fl.nm]])], as.is=T)))))

    }
    df <- df[df[["density"]]>0, ]
    df <- df[df[["fl"]]>0, ]

    if(!is.null(x.lim)) df <- df[df[["time"]]>x.lim[1], ]
    xlab.title <- "Time"

    ylab.title <- "Density"
    p <- ggplot(df, aes(x=time, y=density, col = name)) +
      geom_line(size=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title) || x.title == "", xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title.density) || y.title.density == "", ylab.title, y.title.density)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    if(shiny == TRUE) p <- p + ggplot2::guides(fill=guide_legend(ncol=4))
    else p <- p + ggplot2::guides(fill=guide_legend(ncol=2))

    if(!is.null(x.lim)){
      p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
    } else {
      p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
    }

    if(log.y.density == TRUE){
      if(!is.null(y.lim.density)){
        p <- p + scale_y_log10(limits = y.lim, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p <- p + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }
    } else {
      if(!is.null(y.lim.density)){
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
      } else if (length(ndx.keep) <=50){
        p <- p + scale_fill_manual(name = "Condition",
                                   values = c(
                                     "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                     "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                     "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                     "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                     "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                   )
        ) + scale_color_manual(name = "Condition",
                               values = c(
                                 "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                 "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                 "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                 "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                 "green1", "yellow4", "yellow3", "darkorange4", "brown"
                               )
        )
      }
    }
    p.fl <- ggplot(df, aes(x=time, y=fl, col = name)) +
      geom_line(size=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
      theme(panel.grid.major = element_blank(),
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
      } else if (length(ndx.keep) <=50){
        p.fl <- p.fl + scale_fill_manual(name = "Condition",
                                         values = c(
                                           "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                           "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                           "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                           "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                           "green1", "yellow4", "yellow3", "darkorange4", "brown"
                                         )
        ) + scale_color_manual(name = "Condition",
                               values = c(
                                 "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                                 "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
                                 "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon",
                                 "orchid1", "deeppink1", "blue1", "steelblue4", "darkturquoise",
                                 "green1", "yellow4", "yellow3", "darkorange4", "brown"
                               )
        )
      }
    }
    p <- ggpubr::ggarrange(p, p.fl, ncol = 1, nrow = 2, align = "v", heights = c(2,2), common.legend = T, legend = "bottom", legend.grob = ggpubr::get_legend(p))
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
    dir.create(out.dir, showWarnings = F)
    grDevices::png(paste0(out.dir, "/", out.nm, ".png"),
                   width = w, height = h, units = 'in', res = 300)
    print(p)
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", out.nm, ".pdf"), width = w, height = h)
    print(p)
    grDevices::dev.off()
  }
  if (plot == TRUE){
    print(p)
  }
}
