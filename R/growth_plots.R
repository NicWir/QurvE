#' Generic plot function for \code{gcFittedLinear} objects. Plot the results of a linear regression on ln-transformed data
#'
#' \code{plot.gcFitLinear} shows the results of a linear regression on log-transformed data and visualizes raw data, data points included in the fit, the tangent obtained by linear regression, and the lag time.
#'
#' @param gcFittedLinear A \code{gcFittedLinear} object created with \code{\link{growth.gcFitLinear}} or stored within a \code{grofit} or \code{gcFit} object created with \code{\link{growth.workflow}} or \code{\link{growth.gcFit}}, respectively.
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
#' @export plot.gcFitLinear
#' @export
#'
plot.gcFitLinear <- function(gcFittedLinear, log="y", which=c("fit", "diagnostics", "fit_diagnostics"), pch = 21, cex.point = 1, cex.lab = 1.5,
                             cex.axis = 1.3, lwd = 2, y.lim = NULL, x.lim = NULL,
                             plot = TRUE, export = FALSE, height = ifelse(which=="fit", 7, 5),
                             width = ifelse(which=="fit", 9, 9), out.dir = NULL, ...)
{
  if(is(gcFittedLinear) != "gcFitLinear") stop("gcFittedLinear needs to be an object created with growth.gcFitLinear().")
  which <- match.arg(which)

  p <- function(){
    switch(which,
           fit = {

             par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), cex.lab = cex.lab, cex.axis = cex.axis)

             plot(gcFittedLinear$"raw.data" ~ gcFittedLinear$"raw.time", xlab="", ylab = "", pch = pch,
                  log=log, las=1, yaxt="n", xaxt="n", type = "n", xlim = x.lim, ylim = y.lim, ...)
             title(ylab = "Density", line = 2+cex.lab)
             title(xlab = "Time", line = 1+cex.lab)
             points(gcFittedLinear$"raw.data" ~ gcFittedLinear$"raw.time", cex = cex.point, pch=pch)
             axis(1)
             axis(2, las=1)
             try(points(gcFittedLinear$raw.data[gcFittedLinear$ndx] ~ gcFittedLinear$raw.time[gcFittedLinear$ndx], pch=pch, cex = cex.point*1.15, col="black", bg="red"))

             ## lag phase
             lag <- gcFittedLinear$par["lag"]
             coef_ <- gcFittedLinear$par


             if(gcFittedLinear$fitFlag2){
               try(points(gcFittedLinear$raw.data[gcFittedLinear$ndx2] ~ gcFittedLinear$raw.time[gcFittedLinear$ndx2], pch=pch, cex = cex.point*1.15, col="black", bg=ggplot2::alpha("magenta3", 1)))
               lag2 <- gcFittedLinear$par["lag2"]
               if(lag2 < lag && lag2 > gcFittedLinear$raw.time[1]){
                 try(time2 <- seq(lag2, max(gcFittedLinear$"raw.time"), length=200), silent = T)
                 try(time <- seq(coef_["tmax_start"]-0.25*(coef_["tmax_end"]-coef_["tmax_start"]), max(gcFittedLinear$"raw.time"), length=200), silent = T)
                 try(lines(time2, gcFittedLinear$FUN(time2, unname(c(coef_["y0_lm2"], coef_["mumax2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)
                 try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag2), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7)), silent = T)
                 try(lines(time, gcFittedLinear$FUN(time, unname(c(coef_["y0_lm"], coef_["mumax"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               } else {
                 try(time2 <- seq(coef_["tmax2_start"]-0.25*(coef_["tmax2_start"]), max(gcFittedLinear$"raw.time"), length=200), silent = T)
                 try(time <- seq(lag, max(gcFittedLinear$"raw.time"), length=200), silent = T)
                 try(lines(time, gcFittedLinear$FUN(time, parms = coef_)[,"y"], lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
                 try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7)), silent = T)
                 try(lines(time2, gcFittedLinear$FUN(time2, parms = unname(c(coef_["y0_lm2"], coef_["mumax2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)
               }
             } else {
               try(time <- seq(lag, max(gcFittedLinear$"raw.time"), length=200), silent = T)
               try(lines(time, gcFittedLinear$FUN(time, unname(c(coef_["y0_lm"], coef_["mumax"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7)), silent = T)
             }
             mtext(paste("R2:", round(gcFittedLinear$rsquared, digits = 3)), side = 4 , line = -1.8+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab*0.7)
             mtext(paste("h:", ifelse(is.null(gcFittedLinear$control$lin.h), "NULL", gcFittedLinear$control$lin.h),
                         "   R2-thresh.:",  gcFittedLinear$control$lin.R2,
                         "   RSD-thresh.:",  gcFittedLinear$control$lin.RSD,
                         "t0:", gcFittedLinear$control$t0,
                         "  min.density:", gcFittedLinear$control$min.density,
                         "   dY-thresh.:",  gcFittedLinear$control$lin.dY),
                   cex = cex.lab*0.7, side = 3, line = -4, adj = 0.05, outer = TRUE)

           },
           diagnostics = {
             opar <- par(no.readonly = TRUE)
             on.exit(par(opar))
             par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), cex.lab = cex.lab, cex.axis = cex.axis, mfrow=c(1,2))

             ## residuals vs. fitted
             obs <- gcFittedLinear$log.data
             sim <- gcFittedLinear$FUN(gcFittedLinear$"raw.time", gcFittedLinear$par)
             plot(gcFittedLinear$fit[["residuals"]] ~ fitted(gcFittedLinear$fit), xlab="fitted", ylab="residuals", pch = pch)
             abline(h=0, col="grey")
             ## normal q-q-plot
             qqnorm(gcFittedLinear$fit[["residuals"]])
             qqline(gcFittedLinear$fit[["residuals"]])
           },
           fit_diagnostics = {
             opar <- par(no.readonly = TRUE)
             on.exit(par(opar))
             layout(matrix(c(1,1,2,3), nrow=2, byrow=TRUE))
             par(mar=c(5.1+cex.lab, 4.1 + cex.lab, 4.1, 2.1), mai = c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.7 + 0.2*cex.lab + 0.2*cex.axis, 0.5, 0.3), cex.lab = cex.lab, cex.axis = cex.axis)

             plot(gcFittedLinear$"raw.data" ~ gcFittedLinear$"raw.time", xlab="", ylab = "", pch = pch,
                  log=log, las=1, yaxt="n", xaxt="n", type = "n", xlim = x.lim, ylim = y.lim, ...)
             title(ylab = "Density", line = 2+cex.lab)
             title(xlab = "Time", line = 1+cex.lab)

             points(gcFittedLinear$"raw.data" ~ gcFittedLinear$"raw.time", cex = cex.point, pch=pch)
             axis(1)
             axis(2, las=1)
             try(points(gcFittedLinear$raw.data[gcFittedLinear$ndx] ~ gcFittedLinear$raw.time[gcFittedLinear$ndx], pch=pch, cex = cex.point*1.15, col="black", bg="red"))

             ## lag phase
             lag <- gcFittedLinear$par["lag"]
             coef_ <- gcFittedLinear$par


             if(gcFittedLinear$fitFlag2){
               try(points(gcFittedLinear$raw.data[gcFittedLinear$ndx2] ~ gcFittedLinear$raw.time[gcFittedLinear$ndx2], pch=pch, cex = cex.point*1.15, col="black", bg=ggplot2::alpha("magenta3", 1)))
               lag2 <- gcFittedLinear$par["lag2"]
               if(lag2 < lag && lag2 > gcFittedLinear$raw.time[1]){
                 try(time2 <- seq(lag2, max(gcFittedLinear$"raw.time"), length=200), silent = T)
                 try(time <- seq(coef_["tmax_start"]-0.25*(coef_["tmax_end"]-coef_["tmax_start"]), max(gcFittedLinear$"raw.time"), length=200), silent = T)
                 try(lines(time2, gcFittedLinear$FUN(time2, unname(c(coef_["y0_lm2"], coef_["mumax2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)
                 try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag2), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7)), silent = T)
                 try(lines(time, gcFittedLinear$FUN(time, unname(c(coef_["y0_lm"], coef_["mumax"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               } else {
                 try(time2 <- seq(coef_["tmax2_start"]-0.25*(coef_["tmax2_start"]), max(gcFittedLinear$"raw.time"), length=200), silent = T)
                 try(time <- seq(lag, max(gcFittedLinear$"raw.time"), length=200), silent = T)
                 try(lines(time, gcFittedLinear$FUN(time, parms = coef_)[,"y"], lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
                 try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7)), silent = T)
                 try(lines(time2, gcFittedLinear$FUN(time2, parms = unname(c(coef_["y0_lm2"], coef_["mumax2"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)

               }
             } else {
               try(time <- seq(lag, max(gcFittedLinear$"raw.time"), length=200), silent = T)
               try(lines(time, gcFittedLinear$FUN(time, unname(c(coef_["y0_lm"], coef_["mumax"])))[,"y"], lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=lwd, col=ggplot2::alpha("firebrick3", 0.7)), silent = T)
             }
             mtext(paste("R2:", round(gcFittedLinear$rsquared, digits = 3)), side = 4 , adj = 0.75, line = -1.2+log(cex.lab, base = 6), outer = TRUE, cex = cex.lab*0.7)
             mtext(paste("h:", ifelse(is.null(gcFittedLinear$control$lin.h), "NULL", gcFittedLinear$control$lin.h),
                         "   R2-thresh.:",  gcFittedLinear$control$lin.R2,
                         "   RSD-thresh.:",  gcFittedLinear$control$lin.RSD,
                         "t0:", gcFittedLinear$control$t0,
                         "  min.density:", gcFittedLinear$control$min.density,
                         "   dY-thresh.:",  gcFittedLinear$control$lin.dY),
                   cex = cex.lab*0.7, side = 3, line = -2.5, adj = 0.05, outer = TRUE)

             # mtext(paste("R2:", round(gcFittedLinear$rsquared, digits = 3)), side = 3 , adj = 0.95, line = -1, outer = TRUE)
             # mtext(paste("h:", gcFittedLinear$control4lin.h,
             #             "   R2-thresh.:",  gcFittedLinear$control$lin.R2,
             #             "   RSD-thresh.:",  gcFittedLinear$control$lin.RSD,
             #             "   dY-thresh.:",  gcFittedLinear$control$lin.dY),
             #       side = 4, line = -2, outer = TRUE)

             ## residuals vs. fitted
             obs <- gcFittedLinear$log.data
             sim <- gcFittedLinear$FUN(gcFittedLinear$"raw.time", gcFittedLinear$par)
             plot(gcFittedLinear$fit[["residuals"]] ~ fitted(gcFittedLinear$fit), xlab="fitted", ylab="residuals", type = "n", pch = pch)
             points(gcFittedLinear$fit[["residuals"]] ~ fitted(gcFittedLinear$fit), cex = cex.point, pch=pch)
             abline(h=0, col="grey")
             ## normal q-q-plot
             qqnorm(gcFittedLinear$fit[["residuals"]], cex = cex.point)
             qqline(gcFittedLinear$fit[["residuals"]])
           }
    )
  }
  if (export == TRUE){
    w <- width
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = F)
    grDevices::png(paste0(out.dir, "/", paste(gcFittedLinear$gcID, collapse = "_"), "_LinFitPlot.png"),
                   width = w, height = h, units = 'in', res = 300)
    p()
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", paste(gcFittedLinear$gcID, collapse = "_"), "_LinFitPlot.pdf"))
    p()
    grDevices::dev.off()
  }
  if (plot == TRUE){
    p()
  }
}

#' Generic plot function for \code{gcBootSpline} objects.
#'
#' Plot the results of a parametric model fit on density vs. time data
#'
#' @param gcFittedModel A \code{gcFittedModel} object created with \code{\link{growth.gcFitModel}} or stored within a \code{grofit} or \code{gcFit} object created with \code{\link{growth.workflow}} or \code{\link{growth.gcFit}}, respectively.
#' @param raw (Logical) Show the raw data within the plot (\code{TRUE}) or not (\code{FALSE}).
#' @param pch (Numeric) Symbol used to plot data points.
#' @param colData (Numeric or Character) Color used to plot the raw data.
#' @param equation (Logical) Show the equation of the fitted model within the plot (\code{TRUE}) or not (\code{FALSE}).
#' @param colModel (Numeric or Character) Color used to plot the fitted model.
#' @param basesize (Numeric) Base font size.
#' @param cex.point (Numeric) Size of the raw data points.
#' @param lwd (Numeric) Spline line width.
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Further arguments to refine the generated \code{ggplot2} plot.
#'
#' @export plot.gcFitModel
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
plot.gcFitModel <- function(gcFittedModel, raw = TRUE, pch=1, colData=1, equation = TRUE, eq.size = 1,
                            colModel=ggplot2::alpha("forestgreen", 0.85), basesize=16, cex.point = 2, lwd = 0.7,
                            n.ybreaks = NULL, plot = TRUE, export = FALSE, height = 8, width = 6, out.dir = NULL,...)
{
  # x an object of class gcFitModel

  # /// check input parameters
  if (is.logical(raw)==FALSE)   stop("Need logical value for: raw")
  # if (is.logical(slope)==FALSE) stop("Need logical value for: slope")
  if (is.logical(equation)==FALSE)   stop("Need logical value for: equation")
  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (!(is(gcFittedModel)=="gcFitModel"))   stop("gcFittedModel needs to be an object created with growth.gcFitModel().")


  # /// check if a data fit is available
  if (gcFittedModel$fitFlag==FALSE||is.na(gcFittedModel$fitFlag)){
    warning("plot.gcFitModel: no data fit available!")
  }
  else{
    coef <- gcFittedModel[["parameters"]]
    lagtime <- coef["lambda"][[1]][1]
    model <- as.character(gcFittedModel$model)
    # assign(paste(model.vec), gcFittedModel[["fit.data"]])
    df <- data.frame("time" = gcFittedModel[["raw.time"]],
                     "data" = gcFittedModel[["raw.data"]],
                     "fit.time" = gcFittedModel[["fit.time"]],
                     "fit.data" = gcFittedModel[["fit.data"]])

    p <-    ggplot(df, aes(x=time, y=data)) +
      geom_line(aes_(x=as.name(names(df)[3]), y = as.name(names(df)[4]), color = "model"), size = lwd) +
      xlab("Time") +
      ylab(label = ifelse(gcFittedModel$control$log.y.model == TRUE, "Growth [Ln(y(t)/y0)]", "Growth [y(t)]")) +
      theme_classic(base_size = basesize) +
      ggtitle(gsub(" \\| NA", "", paste(gcFittedModel$gcID, collapse=" | "))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks)) +
      theme(plot.title = element_text(size=15),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    if(raw == TRUE){
      p <- p + geom_point(shape=pch, size = cex.point,alpha = 0.6, stroke=0.15*cex.point, color = colData)
    }
    if(equation == TRUE){
      if(gcFittedModel$model == "logistic"){
        p <- p + annotate(
          "text",
          label = "y(t) == frac(A , 1+exp(frac(4 %.% mu, A) %.% (lambda - t) + 2))",
          x = 1.08 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.2*eq.size) +
          annotate("text",
                   label = bquote(A == .(round(gcFittedModel$parameters$A[1],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[1],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[1],3)) ),
                   x = (1 + 0.13+ log(eq.size)*0.1) * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = F, size = 2.5*eq.size) +
          scale_color_manual(name='Growth Model',
                             breaks = "logistic",
                             values=c("model" = colModel, "logistic" = colModel))
      }
      if(gcFittedModel$model == "richards"){
        p <- p + annotate(
          "text",
          label = "y(t) == A%.%(1.0+nu%.%italic(e)^{1+nu}%.%exp(frac(mu,A)%.%(1+nu)^(1+frac(1,nu))%.%( lambda - t )))^(-1/nu)",
          x = 1.17 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.2*eq.size) +
          annotate("text",
                   label = bquote(A == .(round(gcFittedModel$parameters$A[1],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[1],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[1],3)) ~~~~ nu == .(round(as.numeric(gcFittedModel$parameters$fitpar$nu[1],3)))),
                   x = (1 + 0.22 + log(eq.size)*0.1) * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = F, size = 2.5*eq.size) +
          scale_color_manual(name='Growth Model',
                             breaks = "richards",
                             values=c("model" = colModel, "richards" = colModel))
      }
      if(gcFittedModel$model == "gompertz"){
        p <- p + annotate(
          "text",
          label = "y(t) == A%.%exp(-exp(frac(mu%.%italic(e),A)%.%(lambda-t) +1))",
          x = 1.08 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.4*eq.size) +
          annotate("text",
                   label = bquote(A == .(round(gcFittedModel$parameters$A[1],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[1],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[1],3)) ),
                   x = (1 + 0.13 + log(eq.size)*0.1) * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = F, size = 2.5*eq.size) +
          scale_color_manual(name='Growth Model',
                             breaks = "gompertz",
                             values=c("model" = colModel, "gompertz" = colModel))
      }
      if(gcFittedModel$model == "gompertz.exp"){
        lagtime <- lagtime - gcFittedModel$parameters$A[1]*exp(gcFittedModel$parameters$fitpar$alpha[1]*(gcFittedModel$parameters$lambda[1]-gcFittedModel$parameters$fitpar$t_shift[1]))
        p <- p + annotate(
          "text",
          label = "y(t) == A%.%exp(-exp(frac(mu%.%italic(e),A)%.%(lambda-t) +1)) + A%.%exp(alpha%.%(t-t[shift]))",
          x = 1.16 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.2*eq.size) +
          annotate("text",
                   label = bquote(A == .(round(gcFittedModel$parameters$A[1],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[1],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[1],2)) ~~~~ alpha == .(round(gcFittedModel$parameters$fitpar$alpha[1],3))  ~~~~
                                    t[shift] == .(round(gcFittedModel$parameters$fitpar$t_shift[1],2)) ),
                   x = (1 + 0.21 + log(eq.size)*0.1) * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
                   y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                   angle = 90, parse = F, size = 2.5*eq.size) +
          scale_color_manual(name='Growth Model',
                             breaks = "gompertz.exp",
                             values=c("model" = colModel, "gompertz.exp" = colModel))
      }
      if(gcFittedModel$model == "huang"){
        p <- p + annotate(
          "text",
          label = "y(t) == y0 + A - log( exp(y0) + (exp(A) - exp(y0)) * exp(-mu%.%(t+0.25%.%log(frac(1+exp(-4%.%(t-lambda)),1+exp(4%.%lambda))))) )",
          x = 1.16 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
          y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
          angle = 90, parse = TRUE, size = 3.0*eq.size) +
           annotate("text",
                    label = bquote(A == .(round(gcFittedModel$parameters$A[1],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[1],3)) ~~~~
                                     lambda == .(round(gcFittedModel$parameters$lambda[1],2)) ~~~~ y0 == .(round(gcFittedModel$parameters$fitpar$y0[1,1],3)) ),
                    x = (1 + 0.21 + log(eq.size)*0.1) * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
                    y = 0.5 * (ggplot_build(p)$layout$panel_params[[1]]$y.range[2] + ggplot_build(p)$layout$panel_params[[1]]$y.range[1]),
                    angle = 90, parse = F, size = 2.3*eq.size) +
           scale_color_manual(name='Growth Model',
                              breaks = "huang",
                              values=c("model" = colModel, "huang" = colModel))
      }
    } # if(equation == TRUE)
    else{
      if(gcFittedModel$model == "logistic"){
        p <- p + scale_color_manual(name='Growth Model',
                                    breaks = "logistic",
                                    values=c("model" = colModel, "logistic" = colModel))
      }
      if(gcFittedModel$model == "richards"){
        p <- p + scale_color_manual(name='Growth Model',
                                    breaks = "richards",
                                    values=c("model" = colModel, "richards" = colModel))
      }
      if(gcFittedModel$model == "gompertz"){
        p <- p + scale_color_manual(name='Growth Model',
                                    breaks = "gompertz",
                                    values=c("model" = colModel, "gompertz" = colModel))
      }
      if(gcFittedModel$model == "gompertz.exp"){
        p <- p + scale_color_manual(name='Growth Model',
                                    breaks = "gompertz.exp",
                                    values=c("model" = colModel, "gompertz.exp" = colModel))
      }
    }
    p <- p + theme(legend.key = element_blank(),
                   legend.background=element_blank(),
                   legend.title = element_blank(),
                   legend.position = c(0.92, 0.06),
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
    # if(slope == T){
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
    #                         data = tangent.df, linetype = "dashed", color = colModel, size = 0.5) +
    #     geom_segment(aes(x = time[1], y = y[1], xend = time[2], yend = y[2]), data = df.horizontal,
    #                  linetype = "dashed", color = colModel, size = 0.5)
    # }
    if(export == FALSE && plot == FALSE){
      return(p)
    }
    if (export == TRUE){
      w <- width
      h <- height
      out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
      dir.create(out.dir, showWarnings = F)
      grDevices::png(paste0(out.dir, "/", paste(gcFittedModel$gcID, collapse = "_"), "_ModelFitPlot.png"),
                     width = w, height = h, units = 'in', res = 300)
      p()
      grDevices::dev.off()
      grDevices::pdf(paste0(out.dir, "/", paste(gcFittedModel$gcID, collapse = "_"), "_ModelFitPlot.pdf"))
      suppress_warnings( {print(p)}, "is.na" )
      grDevices::dev.off()
    }
    if (plot == TRUE){
      suppress_warnings( {print(p)}, "is.na" )
    } else {
      return(p)
    }
  }
}

#' Generic plot function for \code{gcBootSpline} objects.
#'
#' @param drBootSpline A \code{drBootSpline} object created with \code{\link{growth.drBootSpline}} or stored within a \code{grofit} or \code{drFit} object created with \code{\link{growth.workflow}} or \code{\link{growth.drFit}}, respectively.
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
#' @param shiny (Logical) Indicate if plot is generated within the shiny app.
#' @param ...
#'
#' @export plot.drBootSpline
#' @export
#'
plot.drBootSpline <- function (drBootSpline,
                               pch = 19,
                               colData = 1,
                               colSpline = scales::alpha("black", 0.15),
                               cex.point = 1, cex.lab = 1.5, cex.axis = 1.3,
                               lwd = 2, plot = TRUE, export = FALSE,
                               height = 7, width = 9, out.dir = NULL, shiny = FALSE,
                               ...)
{
  # drBootSpline an object of class drBootSpline
  if(is(drBootSpline) != "drBootSpline") stop("drBootSpline needs to be an object created with growth.drBootSpline.")
  # /// initialize "Empty Plot" function
  empty.plot  <- function(text = "Empty plot", main = "") {
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
      par(cex.lab = cex.lab, cex.axis = cex.axis)

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
          type = "n",
          xlab = "Ln(1+concentration)",
          ylab = paste0("Response (", drBootSpline$control$dr.parameter, ")")
        )
      }
      else{
        if ((drBootSpline$control$log.x.dr == FALSE) &&
            (drBootSpline$control$log.y.dr == FALSE)) {
          plot(
            c(global.minx, global.maxx),
            c(global.miny, global.maxy),
            type = "n",
            xlab = "Concentration",
            ylab = paste0("Response (", drBootSpline$control$dr.parameter, ")")
          )
        }
        else{
          if ((drBootSpline$control$log.x.dr == TRUE) && (drBootSpline$control$log.y.dr == TRUE)) {
            plot(
              c(global.minx, global.maxx),
              c(global.miny, global.maxy),
              type = "n",
              xlab = "Ln(1+Concentration)",
              ylab = "Ln(1+Response)"
            )
          }
          else{
            if ((drBootSpline$control$log.x.dr == FALSE) && (drBootSpline$control$log.y.dr == TRUE)) {
              plot(
                c(global.minx, global.maxx),
                c(global.miny, global.maxy),
                type = "n",
                xlab = "Concentration",
                ylab = "Ln(1+Response)"
              )
            }
          }
        }
      }

      # /// plot raw data
      points(
        drBootSpline$raw.conc,
        drBootSpline$raw.test,
        col = colData, bg = colData,
        pch = pch,
        cex = cex.point
      )
      title(drBootSpline$drID)

      # /// loop over all fitted splines and plot drFitSpline objects
      for (i in 1:drBootSpline$control$nboot.dr) {
        plot(
          drBootSpline$boot.drSpline[[i]],
          add = TRUE,
          ec50line = FALSE,
          pch = 0,
          colSpline = colSpline[i],
          colData = 0,
          cex.point = cex.point,
          lwd = lwd
        )
      }
    }
    p2 <- function(){
      if (sum(!is.na(drBootSpline$ec50.boot)) == length(drBootSpline$ec50.boot)) {
        hist(
          drBootSpline$ec50.boot,
          col = "gray",
          main = as.character(drBootSpline$drID),
          xlab = "EC50"
        )
      }
      else{
        empty.plot()
      }
    } # p2 <- function()
    p3 <- function(){
      layout(matrix(c(1,1,1,2,2, 1,1,1,3,3), nrow = 5, ncol = 2))

      par(cex.lab = cex.lab, cex.axis = cex.axis)

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
          type = "n",
          xlab = "Ln(1+concentration)",
          ylab = paste0("Response (", drBootSpline$control$dr.parameter, ")")
        )
      }
      else{
        if ((drBootSpline$control$log.x.dr == FALSE) &&
            (drBootSpline$control$log.y.dr == FALSE)) {
          plot(
            c(global.minx, global.maxx),
            c(global.miny, global.maxy),
            type = "n",
            xlab = "Concentration",
            ylab = paste0("Response (", drBootSpline$control$dr.parameter, ")")
          )
        }
        else{
          if ((drBootSpline$control$log.x.dr == TRUE) && (drBootSpline$control$log.y.dr == TRUE)) {
            plot(
              c(global.minx, global.maxx),
              c(global.miny, global.maxy),
              type = "n",
              xlab = "Ln(1+Concentration)",
              ylab = "Ln(1+Response)"
            )
          }
          else{
            if ((drBootSpline$control$log.x.dr == FALSE) && (drBootSpline$control$log.y.dr == TRUE)) {
              plot(
                c(global.minx, global.maxx),
                c(global.miny, global.maxy),
                type = "n",
                xlab = "Concentration",
                ylab = "Ln(1+Response)"
              )
            }
          }
        }
      }

      # /// plot raw data
      points(
        drBootSpline$raw.conc,
        drBootSpline$raw.test,
        col = colData, bg = colData,
        pch = pch,
        cex = cex.point
      )
      title(drBootSpline$drID)

      # /// loop over all fitted splines and plot drFitSpline objects
      for (i in 1:drBootSpline$control$nboot.dr) {
        plot(
          drBootSpline$boot.drSpline[[i]],
          add = TRUE,
          ec50line = FALSE,
          pch = 0,
          colSpline = colSpline[i],
          colData = 0,
          cex.point = cex.point,
          lwd = lwd
        )
      }

      if (sum(!is.na(drBootSpline$ec50.boot)) == length(drBootSpline$ec50.boot)) {
        hist(
          drBootSpline$ec50.boot,
          col = "gray",
          main = as.character(drBootSpline$drID),
          xlab = "EC50"
        )
      }
      else{
        empty.plot()
      }
      if (sum(!is.na(drBootSpline$ec50y.boot)) == length(drBootSpline$ec50y.boot)) {
        hist(
          drBootSpline$ec50y.boot,
          col = "gray",
          main = as.character(drBootSpline$drID),
          xlab = "yEC50"
        )
      }
      else{
        empty.plot()
      }
    }
    if (export == TRUE){
      w1 <- width
      h1 <- height
      out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
      dir.create(out.dir, showWarnings = F)
      grDevices::png(paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplines.png"),
                     width = w1, height = h1, units = 'in', res = 300)
      p1()
      grDevices::dev.off()
      grDevices::pdf(paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplines.pdf"))
      p1()
      grDevices::dev.off()

      w2 <- width
      h2 <- height
      dir.create(paste0(getwd(), "/Plots"), showWarnings = F)
      grDevices::png(paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplinesEC50.png"),
                     width = w2, height = h2, units = 'in', res = 300)
      p2()
      grDevices::dev.off()
      grDevices::pdf(paste0(out.dir, "/", paste(drBootSpline$drID, collapse = "_"), "_drBootSplinesEC50.pdf"))
      p2()
      grDevices::dev.off()
    }

    if (plot == TRUE){
      if(!shiny){
        p1()
        p2()
      } else {
        p3()
      }
    }
  } # /// else of if (drBootSpline$bootFlag==FALSE){

  # restore standard plot parameters
  par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
  par(mfrow = c(1, 1))
}

#' Generic plot function for \code{drFit} objects.
#'
#' code{plot.drFit} calls code{plot.drFitSpline} for each group used in a dose-response analysis
#'
#' @param drFit object of class \code{drFit}, created with \code{\link{growth.drFit}}.
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
#'
#' @export plot.drFit
#' @export
#' @importFrom ggplot2 aes element_text geom_errorbar geom_line
#'   geom_point geom_segment ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous theme theme_classic theme_minimal xlab ylab
plot.drFit <- function(drFit, combine = TRUE, names = NULL, exclude.nm = NULL, pch = 16, cex = 2, basesize = 15, colors = NULL, lwd = 0.7, ec50line = TRUE,
                       y.lim = NULL, x.lim = NULL, y.title = NULL, x.title = NULL, log.y = FALSE, log.x = FALSE,
                       plot = TRUE, export = FALSE, height = NULL, width = NULL, out.dir = NULL, out.nm = NULL)
{
  # x an object of class drFit
  if(is(drFit) != "drFit") stop("drFit needs to be an object of class 'drFit', created with growth.drFit() or fl.drFit(control=fl.control(dr.method='spline').")
  if(length(drFit) == 1) stop("drFit is NA. Please run growth.drFit() with valid data input or growth.workflow() with 'ec50 = T'.")
  n <- length(drFit$drFittedSplines)
  if(combine == FALSE || n < 2){
    # /// plot all drFitSpline objects
    for (i in 1:n) {
      try(plot(drFit$drFittedSplines[[i]], ec50line = ec50line, pch = pch,
               y.lim = y.lim, x.lim = x.lim, y.title = NULL, x.title = NULL,
               cex = cex, export = export,
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
        names <- gsub("\\[", "\\\\[", gsub("\\]", "\\\\]", gsub("\\)", "\\\\)",
                                                                gsub("\\(", "\\\\(", gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", names))))))
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
    raw.df <- lapply(1:length(raw.x), function(x) data.frame(x = raw.x[[x]], y = raw.y[[x]], Condition = rep(names(raw.x)[[x]], length(raw.x[[x]]))))
    # raw.df <- do.call("rbind", raw.df)

    n <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) length(raw.y[[i]][raw.x[[i]]==unique(raw.x[[i]])[x]])))
    conc <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) unique(raw.x[[i]])[x]))
    mean <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) mean(raw.y[[i]][raw.x[[i]]==unique(raw.x[[i]])[x]])))
    sd <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) sd(raw.y[[i]][raw.x[[i]]==unique(raw.x[[i]])[x]])))
    names <- sapply(1:length(raw.x), function(i) sapply(1:length(unique(raw.x[[i]])), function(x) rep(names(drFit$drFittedSplines)[match(names(raw.x)[[i]], names(drFit$drFittedSplines))], length_out=length(raw.y[[i]][raw.x[[i]]==unique(raw.x[[i]])[x]]))))
    error <- stats::qnorm(0.975) * sd / sqrt(n) # standard error
    CI.L <- mean - error #left confidence interval
    CI.R <- mean + error #right confidence interval

    raw.df <- data.frame(Condition = as.vector(names), conc = as.vector(conc), mean = as.vector(mean), CI.L = as.vector(CI.L), CI.R = as.vector(CI.R))
    if(log.x == TRUE) raw.df[raw.df[, "conc"] == 0, "conc"] <- 0.001
    # raw.df$Condition <- factor(raw.df$Condition, levels = raw.df$Condition)
    # raw.df$group <- gsub(" \\|.+", "", raw.df$name)
    # raw.df$mean[is.na(raw.df$mean)] <- 0
    # raw.df$CI.L[is.na(raw.df$CI.L)] <- 0
    # raw.df$CI.R[is.na(raw.df$CI.R)] <- 0


    res.df <- lapply(1:length(raw.x), function(x) data.frame(Condition = names(drFit$drFittedSplines)[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]],
                                                             ec50 = drFit$drFittedSplines[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]][["parameters"]][["EC50"]],
                                                             yEC50 = drFit$drFittedSplines[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]][["parameters"]][["yEC50"]]))
    res.df <- do.call("rbind", res.df)

    spline.df  <- lapply(1:length(raw.x), function(x) data.frame(x = drFit$drFittedSplines[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]][["fit.conc"]],
                                                                 y = drFit$drFittedSplines[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]][["fit.test"]],
                                                                 Condition = rep(names(drFit$drFittedSplines)[[match(names(raw.x)[[x]], names(drFit$drFittedSplines))]], length(drFit$drFittedSplines[[x]][["fit.conc"]]))))
    spline.df <- do.call("rbind", spline.df)

    if(log.x == TRUE) spline.df[spline.df[, "x"] == 0, "x"] <- 0.001

    nrow <- ceiling(length(drFit$drFittedSplines)/2)
    p <- ggplot(data = raw.df, aes(conc, mean, colour = Condition)) +
      geom_point(size=cex, position = ggplot2::position_dodge( 0.015*max(conc)), shape = pch) +
      geom_errorbar(aes(ymin = CI.L, ymax = CI.R), width = 0.05*max(conc), position = ggplot2::position_dodge( 0.015*max(conc))) +
      geom_line(data = spline.df, aes(x, y, colour = Condition), size = lwd) +
      theme_classic(base_size = basesize) +
      theme(legend.position="bottom") +
      ggplot2::guides(color=ggplot2::guide_legend(nrow=nrow, byrow=TRUE))

    if(log.y == TRUE){
      if(!is.null(y.lim)){
        p <- p + scale_y_continuous(limits = y.lim, breaks = scales::pretty_breaks(), trans = "log10")
      } else {
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(), trans = "log10")
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
      p <- p + geom_segment(data = res.df, aes(x = plot.xmin, xend = ec50, y = yEC50, yend = yEC50), alpha = 0.7, linetype = 3, size = lwd) +
        geom_segment(data = res.df, aes(x = ec50, xend = ec50, y = plot.ymin, yend = yEC50), alpha = 0.7, linetype = 3, size = lwd)

      if(log.y == TRUE){
        if(!is.null(y.lim)){
          p <- p + scale_y_continuous(limits = y.lim, expand = ggplot2::expansion(mult = c(0, 0.05)),breaks = scales::pretty_breaks(), trans = "log10")
        } else {
          p <- p + scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)), breaks = scales::pretty_breaks(), trans = "log10")
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
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else if (length(drFit$drFittedSplines) <=50){
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
      dir.create(out.dir, showWarnings = F)
      grDevices::png(paste0(out.dir, "/", out.nm, ".png"),
                     width = w, height = h, units = 'in', res = 300)
      print(p)
      grDevices::dev.off()
      #grDevices::pdf(paste0(out.dir, "/", out.nm, ".pdf"), width = w, height = h)
      #print(p)
      #grDevices::dev.off()
      cat(paste0("drFit plots exported to: ", out.dir, "/", out.nm))
    }
    if (plot == TRUE){
      print(p)
    } else {
      return(p)
    }
  }
}

#' Generic plot function for \code{drFitSpline} objects.
#'
#' code{plot.drFitSpline} generates the spline fit plot for response-parameter vs. concentration data
#'
#' @param drFitSpline object of class \code{drFitSpline}, created with \code{\link{growth.drFitSpline}}.
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
#' @param ...
#'
#' @export plot.drFitSpline
#' @export
#'
plot.drFitSpline <-
  function (drFitSpline,
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
    # drFitSpline an object of class drFitSpline
    if(is(drFitSpline) != "drFitSpline") stop("drFitSpline needs to be an object created with growth.drFitSpline.")
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
      if (add == FALSE) {
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))

        par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1))
        par(cex.lab = cex.lab, cex.axis = cex.axis)

        if ((drFitSpline$control$log.x.dr == TRUE) && (drFitSpline$control$log.y.dr == TRUE)) {
          xlab = ifelse(!is.null(x.title) && x.title != "", x.title, "ln(1+concentration)")
          ylab = ifelse(!is.null(y.title) && y.title != "", y.title, paste0("ln[1+", "Response", ifelse(!is.na(drFitSpline$parameters$test), paste0(" (", drFitSpline$parameters$test, ")"), ""), "]"))
          plot(
            log(drFitSpline$raw.conc + 1),
            log(drFitSpline$raw.test + 1),
            log = log,
            pch = pch, bg = colData,
            cex = cex.point,
            col = colData,
            xlab = xlab,
            ylab = ylab, xlim = x.lim, ylim = y.lim
          )
        }
        else
        {
          if ((drFitSpline$control$log.x.dr == FALSE) && (drFitSpline$control$log.y.dr == TRUE)) {
            xlab = ifelse(!is.null(x.title) && x.title != "", x.title, "concentration")
            ylab = ifelse(!is.null(y.title) && y.title != "", y.title, paste0("ln[1+", "Response", ifelse(!is.na(drFitSpline$parameters$test), paste0(" (", drFitSpline$parameters$test, ")"), ""), "]"))
            plot(
              drFitSpline$raw.conc,
              log(drFitSpline$raw.test + 1),
              log = log,
              pch = pch, bg = colData,
              cex = cex.point,
              col = colData,
              xlab = xlab,
              ylab = ylab, xlim = x.lim, ylim = y.lim
            )
          }
          else
          {
            if ((drFitSpline$control$log.x.dr == TRUE) && (drFitSpline$control$log.y.dr == FALSE)) {
              xlab = ifelse(!is.null(x.title) && x.title != "", x.title, "ln(1+concentration)")
              ylab = ifelse(!is.null(y.title) && y.title != "", y.title, paste0("Response", ifelse(!is.na(drFitSpline$parameters$test), paste0(" (", drFitSpline$parameters$test, ")"), "")))
              plot(
                log(drFitSpline$raw.conc + 1),
                drFitSpline$raw.test,
                log = log,
                pch = pch, bg = colData,
                cex = cex.point,
                col = colData,
                xlab = xlab,
                ylab = ylab, xlim = x.lim, ylim = y.lim
              )
            }
            else
            {
              if ((drFitSpline$control$log.x.dr == FALSE) && (drFitSpline$control$log.y.dr == FALSE)) {
                xlab = ifelse(!is.null(x.title) && x.title != "", x.title, "Concentration")
                ylab = ifelse(!is.null(y.title) && y.title != "", y.title, paste0("Response", ifelse(!is.na(drFitSpline$parameters$test), paste0(" (", drFitSpline$parameters$test, ")"), "")))
                plot(
                  drFitSpline$raw.conc,
                  drFitSpline$raw.test,
                  log = log,
                  pch = pch, bg = colData,
                  cex = cex.point,
                  col = colData,
                  xlab = xlab,
                  ylab = ylab, xlim = x.lim, ylim = y.lim
                )
              }
            }
          }
        }
      }
      else{
        if ((drFitSpline$control$log.x.dr == TRUE) && (drFitSpline$control$log.y.dr == TRUE)) {
          points(
            log(drFitSpline$raw.conc + 1),
            log(drFitSpline$raw.test + 1),
            pch = pch, bg = colData,
            cex = cex.point,
            col = colData
          )
        }
        else{
          if ((drFitSpline$control$log.x.dr == FALSE) && (drFitSpline$control$log.y.dr == TRUE)) {
            points(
              drFitSpline$raw.conc,
              log(drFitSpline$raw.test + 1),
              pch = pch, bg = colData,
              cex = cex.point,
              col = colData
            )
          }
          else
          {
            if ((drFitSpline$control$log.x.dr == TRUE) && (drFitSpline$control$log.y.dr == FALSE)) {
              points(
                log(drFitSpline$raw.conc + 1),
                drFitSpline$raw.test,
                pch = pch, bg = colData,
                cex = cex.point,
                col = colData
              )
            }
            else
            {
              if ((drFitSpline$control$log.x.dr == FALSE) && (drFitSpline$control$log.y.dr == FALSE)) {
                points(
                  drFitSpline$raw.conc,
                  drFitSpline$raw.test,
                  pch = pch, bg = colData,
                  cex = cex.point,
                  col = colData
                )
              }
            }
          }
        }
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
      title(main = drFitSpline$drID)
    } # p <- function()
    if (export == TRUE){
      w <- width
      h <- height
      out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
      dir.create(out.dir, showWarnings = F)
      grDevices::png(paste0(out.dir, "/", paste(drFitSpline$drID, collapse = "_"), "_drFitSpline.png"),
                     width = w, height = h, units = 'in', res = 300)
      p()
      grDevices::dev.off()
      grDevices::pdf(paste0(out.dir, "/", paste(drFitSpline$drID, collapse = "_"), "_drFitSpline.pdf"))
      p()
      grDevices::dev.off()
    }

    if (plot == TRUE){
      p()
    }
  }

#' Generic plot function for \code{gcBootSpline} objects.
#'
#' @param gcBootSpline object of class \code{gcBootSpline}, created with \code{\link{growth.gcBootSpline}}.
#' @param pch (Numeric) Size of the raw data circles.
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
#' @param shiny (Logical) Indicate if plot is generated within the shiny app.
#' @param ...
#'
#' @export plot.gcBootSpline
#' @export
#'
plot.gcBootSpline <- function(gcBootSpline, pch=1, colData=1, deriv = TRUE,
                              colSpline=ggplot2::alpha("dodgerblue3", 0.2),
                              cex.point = 1, cex.lab = 1.5, cex.axis = 1.3,
                              lwd = 2, y.lim = NULL, x.lim = NULL, y.lim.deriv = NULL,
                              plot = TRUE, export = FALSE,
                              height = 7, width = 9, out.dir = NULL, shiny = FALSE, ...)
{
  # gcBootSpline an object of class gcBootSpline
  if(is(gcBootSpline) != "gcBootSpline") stop("gcBootSpline needs to be an object created with growth.gcBootSpline.")
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
    empty.plot()
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
        if(!shiny){
          layout(mat = matrix(c(1, 2), nrow = 2, ncol = 1),
                 heights = c(2, 1.3), # Heights of the two rows
                 widths = c(1, 1)) # Widths of the two columns
        }
        par(mai=c(0.35,0.8,0.5,0))
      } else {
        par(mai=c(0.7,0.8,0.5,0))
      }
      plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim)

      # /// plot data
      points(gcBootSpline$raw.time, gcBootSpline$raw.data, col=colData, pch=pch, cex=cex.point)

      # /// plot all gcFittedSpline objects
      for(i in 1:gcBootSpline$control$nboot.gc){
       plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = T, lwd=lwd,
                        deriv = FALSE, plot = F, export = F, pch=0, colSpline=colSpline[i], cex=cex.point)
      }
      # add plot title
      title(paste(gcBootSpline$gcID, collapse = "_"), line = ifelse(deriv==T, 0.8, 1))
      #add axis titles
      if (fit.log.y==FALSE){
        title(ylab = "Growth y(t) ", line = 2.3, cex.lab = cex.lab)
      }
      else if (fit.log.y==TRUE){
        title(ylab = "Growth [Ln(y(t)/y0)]", line = 2.3, cex.lab = cex.lab)
      }
      # add second plot with slope over time
      if(deriv == TRUE){
        par(cex.axis = cex.axis)
        par(mai=c(0.7,0.8,0.2,0))
        y.max <- ceiling(max(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) max(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        y.min <- floor(min(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) min(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        if(is.null(y.lim.deriv)){
          y.lim.deriv <- c(y.min, y.max)
        }
        if ((gcBootSpline$control$log.x.gc==FALSE)){
          try( plot(gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y,
                    xlab="", ylab="", type = "l", lwd=lwd, col = colSpline, ylim = y.lim.deriv, xlim = x.lim ) )
        }
        if ((gcBootSpline$control$log.x.gc==TRUE)){
          try( lines(gcBootSpline$boot.gcSpline[[1]]$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y, lwd=lwd, xlab="Ln(1+time)", ylab="Growth rate", type = "l") )
        }
        for(i in 2:gcBootSpline$control$nboot.gc){
          plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = F, lwd=lwd, xlim = x.lim,
                           deriv = T, plot = F, export = F, pch=0, colSpline=colSpline[i], cex=cex.point)
        }
        title(ylab = "Growth rate", line = 2.3, cex.lab = cex.lab)
      }
      if (fit.log.x==TRUE){
        title(xlab = "Ln(1+time)", line = 2.3, cex.lab = cex.lab)
      }
      else if(fit.log.x==FALSE){
        title(xlab = "Time", line = 2.3, cex.lab = cex.lab)
      }
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
        try(hist(lambda, col="gray",xlab="lambda", main=expression(lambda), cex.lab = cex.lab, cex.axis = cex.axis))
      }
      else{
        empty.plot("Empty plot!")
      }

      if (sum(!is.na(mu))>1){ try(hist(mu , col="gray", xlab="mu", main=expression(mu), cex.lab = cex.lab, cex.axis = cex.axis)) } else { empty.plot("Empty plot!", main=expression(mu)) }
      if (sum(!is.na(dY))>1){ try(hist(dY, col="gray", xlab="dY", main=expression(dY), cex.lab = cex.lab, cex.axis = cex.axis)) } else { empty.plot("Empty plot!", main=expression(dY)) }
      if (sum(!is.na(integral))>1){ try(hist(integral, col="gray", xlab="integral", main=expression(Integral), cex.lab = cex.lab, cex.axis = cex.axis)) } else { empty.plot("Empty plot!", main=expression(Integral))}
      mtext(paste(gcBootSpline$gcID, collapse = "_"), side = 3, line = -1, outer = TRUE)
      par(mfrow=c(1,1))
      par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    } # p2 <- function()
    p3 <- function()
    {
      if(deriv) layout(matrix(c(1,2,3,5,1,2,4,6), nrow = 4, ncol = 2))
      else layout(matrix(c(1,2,4,1,3,5), nrow = 3, ncol = 2))

      par(cex.lab = cex.lab, cex.axis = cex.axis)
      par(mar=c(5.1+cex.lab, 4.1+cex.lab, 4.1, 2.1), mai = c(0.7 + 0.05*cex.lab + 0.05*cex.axis, 0.7 + 0.2*cex.lab + 0.2*cex.axis, 0.5, 0.3), mgp=c(3, 1, 0), las=0)
      colSpline <- rep(colSpline, (gcBootSpline$control$nboot.gc%/%length(colSpline))+1)

      fit.log.x     <- gcBootSpline$control$log.x.gc
      fit.log.y     <- gcBootSpline$control$log.y.spline

      global.minx <- min(min(gcBootSpline$boot.time,na.rm=TRUE),na.rm=TRUE)
      global.maxx <- max(max(gcBootSpline$boot.time,na.rm=TRUE),na.rm=TRUE)
      global.miny <- min(min(gcBootSpline$boot.data,na.rm=TRUE),na.rm=TRUE)
      global.maxy <- max(max(gcBootSpline$boot.data,na.rm=TRUE),na.rm=TRUE)

      # initialize plot
      if(deriv == TRUE){
        par(mai=c(0.35,0.8,0.5,0))
      } else {
        par(mai=c(0.7,0.8,0.5,0))
      }
      plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="", xlim = x.lim, ylim = y.lim)

      # /// plot data
      points(gcBootSpline$raw.time, gcBootSpline$raw.data, col=colData, pch=pch, cex=cex.point)

      # /// plot all gcFittedSpline objects
      for(i in 1:gcBootSpline$control$nboot.gc){
        plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = T, lwd=lwd,
                         deriv = FALSE, plot = F, export = F, pch=0, colSpline=colSpline[i], cex=cex.point)
      }
      # add plot title
      title(paste(gcBootSpline$gcID, collapse = "_"), line = ifelse(deriv==T, 0.8, 1))
      #add axis titles
      if (fit.log.y==FALSE){
        title(ylab = "Growth y(t) ", line = 2.3, cex.lab = cex.lab)
      }
      else if (fit.log.y==TRUE){
        title(ylab = "Growth [Ln(y(t)/y0)]", line = 2.3, cex.lab = cex.lab)
      }
      # add second plot with slope over time
      if(deriv == TRUE){
        par(cex.axis = cex.axis)
        par(mai=c(0.7,0.8,0.2,0))
        y.max <- ceiling(max(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) max(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        y.min <- floor(min(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) min(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        if(is.null(y.lim.deriv)){
          y.lim.deriv <- c(y.min, y.max)
        }
        if ((gcBootSpline$control$log.x.gc==FALSE)){
          try( plot(gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y,
                    xlab="", ylab="", type = "l", lwd=lwd, col = colSpline, ylim = y.lim.deriv, xlim = x.lim ) )
        }
        if ((gcBootSpline$control$log.x.gc==TRUE)){
          try( lines(gcBootSpline$boot.gcSpline[[1]]$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y, lwd=lwd, xlab="Ln(1+time)", ylab="Growth rate", type = "l") )
        }
        for(i in 2:gcBootSpline$control$nboot.gc){
          plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = F, lwd=lwd, xlim = x.lim,
                           deriv = T, plot = F, export = F, pch=0, colSpline=colSpline[i], cex=cex.point)
        }
        title(ylab = "Growth rate", line = 2.3, cex.lab = cex.lab)
      }
      if (fit.log.x==TRUE){
        title(xlab = "Ln(1+time)", line = 2.3, cex.lab = cex.lab)
      }
      else if(fit.log.x==FALSE){
        title(xlab = "Time", line = 2.3, cex.lab = cex.lab)
      }

      lambda    <- gcBootSpline$lambda
      mu        <- gcBootSpline$mu
      dY         <- gcBootSpline$dY
      integral  <- gcBootSpline$integral

      # /// plot histograms of growth parameters
      if (sum(!is.na(lambda))>1){
        try(hist(lambda, col="gray",xlab="lambda", main=expression(lambda), cex.lab = cex.lab, cex.axis = cex.axis))
      }
      else{
        empty.plot("Empty plot!")
      }

      if (sum(!is.na(mu))>1){ try(hist(mu , col="gray", xlab="mu", main=expression(mu), cex.lab = cex.lab, cex.axis = cex.axis)) } else { empty.plot("Empty plot!", main=expression(mu)) }
      if (sum(!is.na(dY))>1){ try(hist(dY, col="gray", xlab="dY", main=expression(dY), cex.lab = cex.lab, cex.axis = cex.axis)) } else { empty.plot("Empty plot!", main=expression(dY)) }
      if (sum(!is.na(integral))>1){ try(hist(integral, col="gray", xlab="integral", main=expression(Integral), cex.lab = cex.lab, cex.axis = cex.axis)) } else { empty.plot("Empty plot!", main=expression(Integral))}
      par(mfrow=c(1,1))
      par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    } # p3 <- function()
  }
  if (export == TRUE){
    w1 <- width
    h1 <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = F)
    grDevices::png(paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSpline.png"),
                   width = w1, height = h1, units = 'in', res = 300)
    p1()
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSpline.pdf"))
    p1()
    grDevices::dev.off()

    w2 <- width
    h2 <- width
    grDevices::png(paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSplineParam.png"),
                   width = w2, height = h2, units = 'in', res = 300)
    p2()
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", paste(gcBootSpline$gcID, collapse = "_"), "_gcBootSplineParam.pdf"))
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

#' Generic plot function for \code{gcFitSpline} objects.
#'
#' code{plot.gcFitSpline} generates the spline fit plot for a single sample.
#'
#' @param gcFitSpline object of class \code{gcFitSpline}, created with \code{\link{growth.gcFitSpline}}.
#' @param add (Logical) Shall the fitted spline be added to an existing plot? \code{TRUE} is used internally by \code{\link{plot.gcBootSpline}}.
#' @param raw (Logical) Display raw density as circles (\code{TRUE}) or not (\code{FALSE}).
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
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ...
#'
#' @export plot.gcFitSpline
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
plot.gcFitSpline <- function(gcFittedSpline, add=FALSE, raw = TRUE, slope=TRUE, deriv = T, spline = T, log.y = T,
                             pch=1, colData=1, colSpline="dodgerblue3", basesize=16, cex.point = 2, lwd = 0.7,
                             y.lim = NULL, x.lim = NULL, y.lim.deriv = NULL, n.ybreaks = 6,
                             plot = TRUE, export = FALSE, width = 8, height = ifelse(deriv == TRUE, 8, 6),
                             out.dir = NULL, ...)
{
  n.ybreaks <- as.numeric(n.ybreaks)
  # x an object of class gcFittedSpline
  if(is(gcFittedSpline) != "gcFitSpline") stop("gcFittedSpline needs to be an object created with growth.gcFitSpline().")
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
  else{
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
          try(lines(time, y_tangent, lty=2, lwd=2, col=ggplot2::alpha(colSpline, 0.85), ...))
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


      p <- ggplot(df, aes(x=time, y=data)) +
        geom_point(shape=pch, size = cex.point,alpha = 0.6, stroke=0.15*cex.point, color = colData) +
        geom_line(aes(x=fit.time, y = fit.data, color = "spline"), size = lwd) +
        xlab("Time") +
        ylab(label = "Growth [y(t)]") +
        theme_classic(base_size = basesize) +
        ggtitle(gsub(" \\| NA", "", paste(gcFittedSpline$gcID, collapse=" | "))) +
        theme(legend.key = element_blank(),
              legend.background=element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.90, 0.08),
              plot.title = element_text(size = basesize, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_color_manual(name='Growth fit',
                           breaks = "Spline fit",
                           values=c("spline" = ggplot2::alpha(colSpline, 0.85), "Spline fit" = ggplot2::alpha(colSpline, 0.85)))


      p.yrange.end <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]

      if(!is.null(x.lim)){
        p <- p + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
      } else {
        p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      }

      p <- p +
        annotate(
          "text",
          label = paste("t0:", gcFittedSpline$control$t0, "  min.density:", gcFittedSpline$control$min.density, "  smoothing:", gcFittedSpline$control$smooth.gc),
          x = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$x.range[2],
          y = 1.2 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
          angle = 0, parse = F, size = basesize*3.2/12)

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






      # /// add tangent at maximum slope
      if(slope == TRUE && log.y == T){
        mu     <- as.numeric(coef$mu[1])
        if(gcFittedSpline$fitFlag2){
          lagtime2 <- coef$lambda2
          growth.time <- gcFittedSpline$fit.time[which.max(gcFittedSpline$fit.data)]
          mu2 <- coef$mu2
          if(lagtime2 < lagtime && lagtime2 > gcFittedSpline$raw.time[1]){
            # time values for tangent at µmax
            time_start.ndx <- which.min(abs(gcFittedSpline$fit.time-(coef$t.max-0.15*growth.time)))
            time_start <- gcFittedSpline$fit.time[time_start.ndx]
            time <- seq(time_start, max(gcFittedSpline$fit.time), length=200)
            # y values for tangent at µmax
            if(gcFittedSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (mu*time)
            }
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)
            # time values for tangent at µmax2
            time2 <- seq(ifelse(lagtime2<0, 0, lagtime2), max(gcFittedSpline$"fit.time"), length=200)
            # y values for tangent at µmax
            if(gcFittedSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu2*time2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (mu2*time2)
            }

            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)
            df.horizontal2 <- data.frame("time" = c(gcFittedSpline[["raw.time"]][1], lagtime2),
                                         "y" = gcFittedSpline[["data.in"]][1])

            p <- p + geom_segment(aes(x = time[which.min(abs(bla))], y = y[which.min(abs(bla))],
                                      xend = time[which.min(abs(y - 1.1*p.yrange.end))],
                                      yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                  data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
            p <- p + geom_segment(aes(x = time[which.min(abs(bla2))], y = y[which.min(abs(bla2))],
                                      xend = time[which.min(abs(y - 1.1*p.yrange.end))],
                                      yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                  data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.85), size = 0.5)

            if(!(lagtime2 <0)){
              p <- p + geom_segment(aes(x = time[1], y = y[1], xend = time[2], yend = y[2]), data = df.horizontal2,
                                    linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.85), size = 0.5)
            }
          } # if(lagtime2 < lagtime)
          else {
            # time values for tangent at µmax
            time <- seq(ifelse(lagtime<0, 0, lagtime), max(gcFittedSpline$"fit.time"), length=200)
            # y values for tangent at µmax
            if(gcFittedSpline$control$log.y.spline){
              bla <- (exp(coef["b.tangent"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu*time)
            } else {
              bla <- coef["b.tangent"][[1]] + (mu*time)
            }
            tangent.df <- data.frame("time" = time,
                                     "y" = bla)
            df.horizontal <- data.frame("time" = c(gcFittedSpline[["raw.time"]][1], lagtime),
                                        "y" = gcFittedSpline[["data.in"]][1])
            # time values for tangent at µmax2
            time2_start.ndx <- which.min(abs(gcFittedSpline$fit.time-(coef$t.max2-0.15*growth.time)))
            time2_start <- gcFittedSpline$fit.time[time2_start.ndx]
            time2 <- seq(time2_start, max(gcFittedSpline$"fit.time"), length=200)
            # y values for tangent at µmax
            if(gcFittedSpline$control$log.y.spline){
              bla2 <- (exp(coef["b.tangent2"][[1]])*gcFittedSpline[["data.in"]][1])*exp(mu2*time2)
            } else {
              bla2 <- coef["b.tangent2"][[1]] + (mu2*time2)
            }
            tangent.df2 <- data.frame("time" = time2,
                                      "y" = bla2)

            p <- p + geom_segment(aes(x = time[which.min(abs(bla))], y = y[which.min(abs(bla))],
                                      xend = time[which.min(abs(y - 1.1*p.yrange.end))],
                                      yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                  data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
            p <- p + geom_segment(aes(x = time[which.min(abs(bla2))], y = y[which.min(abs(bla2))],
                                      xend = time[which.min(abs(y - 1.1*p.yrange.end))],
                                      yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                  data = tangent.df2, linetype = "dashed", color = ggplot2::alpha("darkviolet", 0.85), size = 0.5)

            if(!(lagtime <0)){
              p <- p + geom_segment(aes(x = time[1], y = y[1], xend = time[2], yend = y[2]), data = df.horizontal,
                                    linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
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
          tangent.df <- data.frame("time" = time,
                                   "y" = bla)
          df.horizontal <- data.frame("time" = c(gcFittedSpline[["raw.time"]][1], lagtime),
                                      "y" = gcFittedSpline[["data.in"]][1])
          p <- p + geom_segment(aes(x = time[which.min(abs(bla))], y = y[which.min(abs(bla))],
                                    xend = time[which.min(abs(y - 1.1*p.yrange.end))],
                                    yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                                data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
          if(!(lagtime <0)){
            p <- p + geom_segment(aes(x = time[1], y = y[1], xend = time[2], yend = y[2]), data = df.horizontal,
                                  linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
          }
        } # else of if(gcFittedSpline$fitFlag2)
      } # if(slope == TRUE && log.y == T)

      # /// add panel with growth rate over time
      if(deriv == TRUE){
        df.mu <- data.frame(spline(gcFittedSpline$spline.deriv1$x, gcFittedSpline$spline.deriv1$y))
        #add missing time values due to min.density and t0
        df.mu <-
          dplyr::bind_rows(data.frame(x = df$time[is.na(df$fit.data)], y = rep(NA, length(df$time[is.na(df$fit.data)]))),
                    df.mu)

        p.mu <- ggplot(df.mu, aes(x=x, y=y)) +
          geom_line(color = colSpline, size = lwd) +
          theme_classic(base_size = basesize) +
          xlab("Time") +
          ylab(label = "Growth rate")

        if(!is.null(y.lim.deriv)){
          p.mu <- p.mu + scale_y_continuous(limits = y.lim.deriv, breaks = scales::pretty_breaks(n = 10))
        } else {
          p.mu <- p.mu + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

        }
        if(!is.null(x.lim)){
          p.mu <- p.mu + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
        } else {
          p.mu <- p.mu + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
        }

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
        grDevices::png(paste0(out.dir, "/", paste(gcFittedSpline$gcID, collapse = "_"), "_SplineFit.png"),
                       width = w, height = h, units = 'in', res = 300)
        print(p)
        grDevices::dev.off()
        grDevices::pdf(paste0(out.dir, "/", paste(gcFittedSpline$gcID, collapse = "_"), "_SplineFit.pdf"), width = w, height = h)
        print(p)
        grDevices::dev.off()
      }
      if (plot == TRUE){
        print(p)
      } else{
        return(p)
      }
    } # else of if (add == TRUE)
  } # else of if ((is.na(gcFittedSpline$fitFlag)==TRUE)|(gcFittedSpline$fitFlag==FALSE))
}

#' Generic plot function for \code{grofit} objects. Combine different groups of samples into a single plot
#'
#' \code{plot.grofit} extracts the spline fits of a subset of samples in a \code{grofit} object calculates averages and standard deviations of conditions with replicates and combines them into a single plot.
#'
#'
#' @param grofit A \code{grofit} object created with \code{\link{growth.workflow}} containing spline fits.
#' @param ... (_optional_) Additional \code{grofit} objects created in separate workflows.
#' @param data.type (Character) Plot either raw data (\code{data.type = "raw"}) or the spline fit results
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
#' @param lwd (Numeric) Line width of the individual plots.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param y.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the y-axis of the growth curve plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param x.lim (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds of the x-axis of both growth curve and derivative plots as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.lim.deriv (Numeric vector with two elements) Optional: Provide the lower (\code{l}) and upper (\code{u}) bounds on the y-axis of the derivative plot as a vector in the form \code{c(l, u)}. If only the lower or upper bound should be fixed, provide \code{c(l, NA)} or \code{c(NA, u)}, respectively.
#' @param y.title (Character) Optional: Provide a title for the y-axis of the growth curve plot.
#' @param x.title (Character) Optional: Provide a title for the x-axis of both growth curve and derivative plots.
#' @param y.title.deriv (Character) Optional: Provide a title for the y-axis of the derivative plot.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#' @param shiny (Logical) Indicate if plot is generated within the shiny app.
#'
#' @export plot.grofit
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab xlim ylim
#'
plot.grofit <- function(grofit, ...,
                        data.type = c("spline", "raw"),
                        names = NULL,
                        conc = NULL,
                        exclude.nm = NULL,
                        exclude.conc = NULL,
                        mean = TRUE,
                        log.y = T,
                        deriv = T,
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
  suppressWarnings(assign("x.lim" ,as.numeric(x.lim)))
  if(all(is.na(x.lim))) x.lim <- NULL
  suppressWarnings(assign("y.lim" ,as.numeric(y.lim)))
  if(all(is.na(y.lim))) y.lim <- NULL
  suppressWarnings(assign("y.lim.deriv" ,as.numeric(y.lim.deriv)))
  if(all(is.na(y.lim.deriv))) y.lim.deriv <- NULL

  call <- match.call()
  # remove all function arguments from call to leave only multiple grofit objects
  call$export <- call$plot <- call$out.nm <- call$out.dir <- call$width <- call$height <- call$lwd <- call$y.title.deriv <-
    call$y.lim.deriv <- call$x.title <- call$y.title <- call$x.lim <- call$y.lim <- call$basesize <- call$colors <- call$n.ybreaks <- call$deriv <-
    call$log.y <- call$mean  <- call$conc  <- call$names  <- call$data.type <- call$exclude.conc <- call$exclude.nm <- call$shiny <- NULL

  arglist <- lapply(call[-1], function(x) x)
  var.names <- vapply(arglist, deparse, character(1))
  arglist <- lapply(arglist, eval.parent, n = 2)
  names(arglist) <- var.names
  if(length(arglist) > 1){
    # combine several grofit objects for joint plotting
    lapply(arglist, function(x) {
      if(is(x) != "grofit") stop("Input objects need to be of class 'grofit' created with growth.workflow().")
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
    warning("Derivatives cannot be calculated for 'raw' data. Only the density values will be shown.")
    deriv = FALSE
  }

  # grofit an object of class grofit
  if(is(grofit) != "grofit") stop("grofit needs to be an object created with growth.workflow().")
  # /// check input parameters

  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(lwd)==FALSE)   stop("Need numeric value for: lwd")
  if(data.type == "spline"){
    if (!("s" %in% grofit$control$fit.opt | "a" %in% grofit$control$fit.opt)) stop("To plot spline fit results, please run growth.workflow() with 'a' or 's' in fit.opt.")
  }

  conc <- as.numeric(conc)
  exclude.conc <- as.numeric(exclude.conc)

  # Get name of conditions with multiple replicates; apply selecting arguments
  sample.nm <- nm <- as.character(names(grofit$gcFit$gcFittedSplines))
  if(!is.null(names)  && length(names) > 0){
    if(!is.na(names) && names != ""){
      names <- gsub("\\[", "\\\\[", gsub("\\]", "\\\\]", gsub("\\)", "\\\\)",
                                                              gsub("\\(", "\\\\(", gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", names))))))
      nm <- nm[grep(paste(names, collapse="|"), gsub("\\[", "\\\\[", gsub("\\]", "\\\\]", gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", nm)))))))]
    }
  }
  if(!is.null(conc) && length(conc) > 0){
    if(!all(is.na(conc))) nm <- nm[which(str_extract(nm, "[:graph:]+$") %in% conc)]
  }
  if(!is.null(exclude.nm)  && length(exclude.nm) > 0){
    if(!is.na(exclude.nm) && exclude.nm != ""){
      names.excl <- gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", exclude.nm))
      nm <- nm[!grepl(paste(names.excl, collapse="|"), gsub(" \\|.+", "", nm))]
    }
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
  #keep only replicate indices if condition defined in nm
    # get indices of samples with selected names
    ndx.keep <- grep(paste0(
      str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(nm, "\\)", "\\\\)"), "\\(", "\\\\("), "\\+", "\\\\+"), "\\-", "\\\\-"), "\\?", "\\\\?"), "\\|", "\\\\|"), collapse = "|"), sample.nm)
    ndx.filt.rep <- ndx.filt.rep[unlist(lapply(1:length(ndx.filt.rep), function(i) all(ndx.filt.rep[[i]] %in% ndx.keep)))]

  filter.ls <- list()
  for(j in 1:length(ndx.filt.rep)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.rep[[j]]), function(i) ndx.filt.rep[[j]][grep(paste0("^",
                                                                                                           gsub("\\)", "\\\\)",
                                                                                                                gsub("\\(", "\\\\(",
                                                                                                                     gsub("\\?", "\\\\?", gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[1])))),
                                                                                                                ".+[[:space:]]",
                                                                                                                unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                                "$")), sample.nm[ndx.filt.rep[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = F)
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
                          str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(nm, "\\)", "\\\\)"), "\\(", "\\\\("), "\\+", "\\\\+"), "\\-", "\\\\-"), "\\?", "\\\\?"), "\\|", "\\\\|"), "\\.", "\\\\."), "$", collapse = "|"), sample.nm)

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

    # Create lists for each selected condition, with density values and derivatives, respectively. Each list item represents one condition with their average and SD
    plotdata.ls <- list()
    deriv.ls <- list()
    for(n in 1:length(conditions_unique)){
      # find indexes of replicates
      ndx <- intersect(ndx.keep, grep(paste0("^",
                                             gsub("\\)", "\\\\)",
                                                  gsub("\\(", "\\\\(",
                                                       gsub("\\?", "\\\\?", gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", unlist(str_split(conditions_unique[n], " \\| "))[1]))))),
                                             " \\|.+[[:space:]]",
                                             unlist(str_split(conditions_unique[n], " \\| "))[2],
                                             "$"), sample.nm))

      name <- conditions_unique[n]
      # Create lists for density and time values for each sample
      if(data.type == "spline"){
        time <- lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$fit.time)) %>% as.list(.)
        data <- lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$fit.data)) %>% as.list(.)
      } else {
        time <- lapply(1:length(ndx), function(i) cbind(grofit$time[ndx[[i]], ])) %>% as.list(.)
        data <- grofit$data[ndx, 4:ncol(grofit$data)]
        data <- split(as.matrix(data), 1:nrow(as.matrix(data)))
        data <- lapply(1:length(data), function(i) as.numeric(data[[i]]))
      }

      # Create lists for derivatives and time values for each sample
      if(deriv){
        time.deriv <- lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$spline.deriv1$x)) %>% as.list(.)
        data.deriv <- lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$spline.deriv1$y)) %>% as.list(.)
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
      }
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
    }
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

    # replace negative lower ribbon boundaries with 0 for log10 transformation
    if(log.y==TRUE){
      df$lower[df$lower<0] <- 0
    }

    p <- ggplot(df, aes(x=time, y=mean, col = name)) +
      geom_line(size=lwd) +
      geom_ribbon(aes(ymin=lower,ymax=upper, fill=name), alpha = 0.3, colour = NA) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title), "Time", x.title)) +
      ylab(ifelse(is.null(y.title), "Growth [y(t)]", y.title)) +
      theme(legend.position="bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    if(shiny == TRUE) p <- p + guides(fill=guide_legend(ncol=4))
    else p <- p + guides(fill=guide_legend(ncol=2))

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
    } else {
      p <- p + scale_fill_manual(name = "Condition",
                                 values = colors) +
        scale_color_manual(name = "Condition",
                           values = colors)
    }
    if(deriv){
      # /// add panel with growth rate over time
      p.deriv <- ggplot(df.deriv, aes(x=time, y=mean, col = name)) +
        geom_line(size=lwd) +
        geom_ribbon(aes(ymin=lower,ymax=upper, fill=name), alpha = 0.3, colour = NA) +
        theme_classic(base_size = basesize) +
        xlab(ifelse(is.null(x.title), "Time", x.title)) +
        theme(legend.position="bottom",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      if(is.null(y.title.deriv)){
        p.deriv <- p.deriv + ylab(label = "Growth rate")
      } else {
        p.deriv <- p.deriv + ylab(label = y.title.deriv)
      }


      if(!is.null(y.lim.deriv)){
        p.deriv <- p.deriv + scale_y_continuous(limits = y.lim.deriv, breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      } else {
        p.deriv <- p.deriv + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
      }

      if(!is.null(x.lim)){
        p.deriv <- p.deriv + scale_x_continuous(limits = x.lim, breaks = scales::pretty_breaks(n = 10))
      } else {
        p.deriv <- p.deriv + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
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
      } else {
        p.deriv <- p.deriv + scale_fill_manual(name = "Condition",
                                   values = colors) +
          scale_color_manual(name = "Condition",
                             values = colors)
      }
      p <- ggpubr::ggarrange(p, p.deriv, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1), common.legend = T, legend = "bottom", legend.grob = ggpubr::get_legend(p, position = "right"))
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
                                              "y" = unlist(unname(type.convert(grofit$data[ndx.keep[i], 4:ncol(grofit$data)], as.is=T)))))
      }

    }
    p <- ggplot(df, aes(x=time, y=y, col = name)) +
      geom_line(size=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title), "Time", x.title)) +
      ylab(ifelse(is.null(y.title), "Growth [y(t)]", y.title)) +
      theme(legend.position="bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    if(shiny == TRUE) p <- p + guides(fill=guide_legend(ncol=4))
    else p <- p + guides(fill=guide_legend(ncol=2))

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
                                              "time" = grofit$gcFit$gcFittedSplines[[ndx.keep[[i]]]]$spline.deriv1$x,
                                              "y" = grofit$gcFit$gcFittedSplines[[ndx.keep[[i]]]]$spline.deriv1$y))
      }
      p.deriv <- ggplot(df.deriv, aes(x=time, y=y, col = name)) +
        geom_line(size=lwd) +
        theme_classic(base_size = basesize) +
        xlab(ifelse(is.null(x.title), "Time", x.title)) +
        theme(legend.position="bottom",
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
  } else {
    return(p)
  }
}

base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

#' Compare growth parameters between samples or conditions
#'
#' \code{plot.parameter} gathers physiological parameters from the results of a growth fit analysis and compares a chosen parameter between each sample or condition in a column plot. Error bars represent the 95% confidence interval (only shown for > 2 replicates).
#'
#' @param object A \code{grofit}, \code{gcFit}, or \code{gcTable} object obtained with \code{\link{growth.workflow}} or \code{\link{growth.gcFit}}.
#' @param param (Character) The parameter used to compare different sample groups. Any name of a column containing numeric values in \code{gcTable} (which is stored within \code{grofit} or \code{gcFit} objects) can be used as input. Useful options are:
#' 'mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit',
#' 'mu.model', 'lambda.model', 'A.model',
#' 'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline',
#' 'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt'
#' @param names (String or vector of strings) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered. Note: Ensure to use unique substrings to extract groups of interest. If the name of one condition is included in its entirety within the name of other conditions, it cannot be extracted individually.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered. Note: Ensure to use unique concentration values to extract groups of interest. If the concentration value of one condition is included in its entirety within the name of other conditions (e.g., the dataset contains '1', '10', and '100', \code{code = 10} will select both '10 and '100'), it cannot be extracted individually.
#' @param exclude.nm (String or vector of strings) Define groups to exclude from the plot. Partial matches with sample/group names are accepted.
#' @param exclude.conc (Numeric or numeric vector) Define concentrations to exclude from the plot.
#' @param basesize (Numeric) Base font size.
#' @param label.size (Numeric) Font size for sample labels below x-axis.
#' @param reference.nm (Character) Name of the reference condition, to which parameter values are normalized. Partially matching strings are tolerated as long as they can uniquely identify the condition.
#' @param reference.conc (Numeric) Concentration of the reference condition, to which parameter values are normalized.
#' @param shape.size (Numeric) The size of the symbols indicating replicate values. Default: 2.5
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param out.nm (Character) The name of the PDF and PNG files if \code{export = TRUE}. If \code{NULL}, a name will be automatically generated including the chosen parameter.
#'
#' @export plot.parameter
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab geom_hline geom_col
plot.parameter <- function(object, param = c('mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit', 'mu2.linfit', 'lambda2.linfit',
                                             'mu.model', 'lambda.model', 'A.model', "tD.linfit", "tD2.linfit", "tD.spline", "tD2.spline",
                                             'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu2.spline', 'lambda2.spline',
                                             'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt',
                                             'max_slope.linfit', 'max_slope.spline'),
                           names = NULL,
                           conc = NULL,
                           basesize = 12,
                           label.size = NULL,
                           reference.nm = NULL,
                           reference.conc = NULL,
                           exclude.nm = NULL,
                           exclude.conc = NULL,
                           shape.size = 2.5,
                           plot = T,
                           export = F,
                           height = 7,
                           width = NULL,
                           out.dir = NULL,
                           out.nm = NULL)
  {
  # Convert range  and selecting arguments
  names <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", names)), pattern = ";"))
  conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", conc)), pattern = "[;,]"))
  exclude.nm <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.nm)), pattern = ";"))
  exclude.conc <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.conc)), pattern = ";"))


  param <- match.arg(param)
  # check class of object
  if(!(any(is(object) %in% c("gcTable", "grofit", "gcFit", "flTable", "flFitRes", "flFit")))) stop("object needs to be either a 'grofit', 'gcTable', 'gcFit', 'flTable', 'flFit', or 'flFitRes' object created with growth.workflow(), growth.gcFit(), fl.workflow(), or flFit().")
  if(!is.character(param) || !(param %in% c('mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit', 'mu2.linfit', 'lambda2.linfit',
                                            'mu.model', 'lambda.model', 'A.model', "tD.linfit", "tD2.linfit", "tD.spline", "tD2.spline",
                                            'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu2.spline', 'lambda2.spline',
                                            'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt', 'max_slope.linfit', 'max_slope.spline')))
                                            stop("param needs to be a character string and one of:\n 'mu.linfit', 'lambda.linfit', 'mu2.linfit', 'lambda2.linfit', 'dY.linfit', 'A.linfit', 'mu.model', 'lambda.model', 'A.model', 'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt', 'max_slope.linfit', 'max_slope.spline'.")

  #extract gcTable
  if(any(is(object) %in% "gcTable")){
    gcTable <- object
  } else if (is(object)=="gcFit"){
    gcTable <- object$gcTable
  } else if (is(object)=="grofit"){
    gcTable <- object$gcFit$gcTable
  } else if (is(object)=="flFitRes"){
    gcTable <- object$flFit1$flTable
  } else if (any(is(object) %in% "gcTable")){
    gcTable <- object
  } else if (is(object)=="flFit"){
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
  if(!is.null(names)  && length(names) > 0){
    if(!is.na(names) && names != ""){
      names <- gsub("\\[", "\\\\[", gsub("\\]", "\\\\]", gsub("\\)", "\\\\)",
                                                              gsub("\\(", "\\\\(", gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", names))))))
      nm <- nm[grep(paste(names, collapse="|"), nm)]
    }
  }
  if(!is.null(conc) && length(conc) > 0){
    if(!all(is.na(conc))) nm <- nm[which(str_extract(nm, "[:graph:]+$") %in% conc)]
  }
  if(!is.null(exclude.nm)  && length(exclude.nm) > 0){
    if(!is.na(exclude.nm) && exclude.nm != ""){
      names.excl <- gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", exclude.nm))
      nm <- nm[!grepl(paste(names.excl, collapse="|"), gsub(" \\|.+", "", nm))]
    }
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
                                                                                                             gsub("\\)", "\\\\)",
                                                                                                                  gsub("\\(", "\\\\(",
                                                                                                                       gsub("\\?", "\\\\?",
                                                                                                                            gsub("\\.", "\\\\.",
                                                                                                                                 gsub("\\+", "\\\\+",
                                                                                                                                      unlist(str_split(nm[ndx.filt.rep[[j]][i]], " \\| "))[1]))))),
                                                                                                             ".+[[:space:]]",
                                                                                                             unlist(str_split(nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                             "$"), nm[ndx.filt.rep[[j]]])]))
    }
    ndx.filt <- unlist(filter.ls, recursive = F)
  ndx.filt <- ndx.filt[lapply(ndx.filt, length)>0]

  names(ndx.filt) <- unlist(lapply(1:length(ndx.filt), function (x) nm[ndx.filt[[x]][1]]) )
  if(gsub(".+\\.", "", param)=="linfit") fit.type <- "linfit"
  if(gsub(".+\\.", "", param)=="model") fit.type <- "model"
  if(gsub(".+\\.", "", param)=="spline") fit.type <- "spline"

  # Check FitFlag for each replicate, work per condition
  for(i in 1:length(ndx.filt)){
    if(!all(unlist(lapply(1:length(ndx.filt[[i]]), function(j) (as.logical(gcTable[j, ifelse(fit.type=="linfit", "reliable_fit.linfit", ifelse(fit.type=="model", "reliable_fit.model", "reliable_fit.spline"))])))))){
      fitflags <- unlist(lapply(1:length(ndx.filt[[i]]), function(j) (as.logical(gcTable[j, ifelse(fit.type=="linfit", "reliable_fit.linfit", ifelse(fit.type=="model", "reliable_fit.model", "reliable_fit.spline"))]))))
      nm <- nm[!(nm %in% sample.nm[(ndx.filt[[i]][!fitflags])])]
      ndx.filt[[i]] <- ndx.filt[[i]][fitflags]
    }
  }

  # calculate average param values
  mean <- unlist(lapply(1:length(ndx.filt), function (x) mean(as.numeric(gcTable[ndx.filt[[x]], param]), na.rm = T)) ) # mean
  sd <- unlist(lapply(1:length(ndx.filt), function (x) sd(as.numeric(gcTable[ndx.filt[[x]], param]), na.rm = T)) ) #standard deviation
  n <- unlist(lapply(1:length(ndx.filt), function (x) length(ndx.filt[[x]])) ) # number of replicates per condition
  labels <- gsub(" \\| NA", "", gsub(" \\| [[:digit:]]+ \\| ", " | ", names(ndx.filt))) # condition names

  # apply normalization to reference condition
  if(!is.null(reference.nm) && reference.nm != ""){
    ref.ndx <- grep( gsub("\\)", "\\\\)",
                          gsub("\\(", "\\\\(", gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", gsub("\\?", "\\\\?", reference.nm))))), labels)
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

  df <- data.frame(name = labels, mean = mean, CI.L = CI.L, CI.R = CI.R)
  df$name <- factor(df$name, levels = df$name)
  df$group <- gsub(" \\|.+", "", df$name)
  df$mean[is.na(df$mean)] <- 0
  df$CI.L[is.na(df$CI.L)] <- 0
  df$CI.R[is.na(df$CI.R)] <- 0

  if(is.null(label.size) || label.size == "") label.size <-  12-length(unique(df$name))^(1/3)

  p <- ggplot(df, aes(x=name, y=mean, fill = group)) +
    geom_bar(stat="identity", color = "black") +
    geom_errorbar(aes(ymin = CI.L, ymax = CI.R), width = 0.2) +
    ggplot2::labs(x = "Condition", y = paste0(param, " (\u00B1 95% CI)")) +
    theme_minimal(base_size = basesize) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = label.size),
          plot.margin = unit(c(1, 1, 1, nchar(as.character(df$name)[1])/6), "lines"),
          # remove the vertical grid lines
          panel.grid.major.x = element_blank() ,
          # explicitly set the horizontal lines (or they will disappear too)
          ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))


  replicates <- unlist(lapply(1:length(ndx.filt), function(x) seq(1, length(ndx.filt[[x]]))))
  df_reps <-
    data.frame(condition = unlist(lapply(1:length(ndx.filt), function(x) rep(labels[x], length(ndx.filt[[x]])))),
               replicate = unlist(lapply(1:length(ndx.filt), function(x) seq(1, length(ndx.filt[[x]])))),
               value = unlist(lapply(1:length(ndx.filt), function (x) as.numeric(gcTable[ndx.filt[[x]], param])))
               ) %>%
    tibble::rownames_to_column()

  df_reps$condition <- as.factor(df_reps$condition)
  df_reps$replicate <- as.factor(df_reps$replicate)
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
       aes(x = condition, y = value, fill = replicate),
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
  } else {
    return(p)
  }
}

#' Compare calculated dose-response parameters between conditions.
#'
#' \code{plot.dr_parameter} gathers parameters from the results of a dose-response analysis and compares a chosen parameter between each condition in a column plot. Error bars represent the 95% confidence interval (only shown for > 2 replicates).
#'
#' @param object A \code{grofit}, \code{drFit}, \code{drTable}, or \code{flFitRes} object obtained with \code{\link{growth.workflow}}, \code{\link{growth.drFit}}, \code{\link{fl.drFit}}, or \code{\link{fl.workflow}}.
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
#'
#' @export
#'
plot.dr_parameter <- function(object, param = c('y.max', 'y.min', 'fc', 'K', 'n', 'EC50', 'yEC50', 'drboot.meanEC50', 'drboot.meanEC50y'),
                              names = NULL, exclude.nm = NULL, basesize = 12, reference.nm = NULL, label.size = NULL,
                              plot = T, export = F, height = 7, width = NULL, out.dir = NULL, out.nm = NULL)
{
  param <- match.arg(param)
  names <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", names)), pattern = ";"))
  exclude.nm <- unlist(str_split(gsub("[;,][[:space:]]+", ";", gsub("[[:space:]]+[;,]", ";", exclude.nm)), pattern = ";"))
  # check class of object
  if(!(any(is(object) %in% c("drTable", "grofit", "drFit", "flFitRes")))) stop("object needs to be either a 'grofit', 'drTable', 'drFit', or 'flFitRes' object created with growth.workflow(), growth.drFit(), fl.workflow(), or growth.drFit().")
  if(!is.character(param) || !(param %in% c('y.max', 'y.min', 'fc', 'K', 'n', 'EC50', 'yEC50', 'drboot.meanEC50', 'drboot.meanEC50y')))
    stop("param needs to be a character string and one of:\n 'y.max', 'y.min', 'fc', 'K', 'n', or 'yEC50' (for fluorescence fits), or \n 'yEC50', 'EC50', 'drboot.meanEC50', or 'drboot.meanEC50y' (for growth fits).")
  #extract drTable
  if(any(is(object) %in% "drTable")){
    drTable <- object
  } else if (is(object)=="drFit"){
    drTable <- object$drTable
  } else if (is(object)=="grofit"){
    drTable <- object$drFit$drTable
  } else if (is(object)=="flFitRes"){
    drTable <- object$drFit1$drTable
  } else if (any(is(object) %in% "drTable")){
    drTable <- object
  }
  drTable <- as.data.frame(drTable)
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
    names <- gsub("\\[", "\\\\[", gsub("\\]", "\\\\]", gsub("\\)", "\\\\)",
                                                            gsub("\\(", "\\\\(", gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", names))))))
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
  # apply normalization to reference condition
  if(!is.null(reference.nm)){
    ref.ndx <- grep(reference.nm, nm)
    if(length(ref.ndx) > 1){
      message("The provided combination of reference.nm = '", reference.nm, " did not allow for the unique identification of a reference condition. The first match will be returned.")
      ref.ndx <- ref.ndx[1]
    }
    value.ref <- values[ref.ndx]
    values <- values/value.ref
  }

  df <- data.frame(name = nm, values = values)
  df$name <- factor(df$name, levels = df$name)
  df$values[is.na(df$values)] <- 0

  if(is.null(label.size) || label.size == "") label.size <-  12-length(unique(df$name))^(1/3)

  p <- ggplot(df, aes(x=name, y=values)) +
    geom_bar(stat="identity", color = "black") +
    ggplot2::labs(x = "Condition", y = param) +
    theme_minimal(base_size = basesize) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = label.size),
          plot.margin = unit(c(1, 1, 1, nchar(as.character(df$name)[1])/6), "lines"),
          # remove the vertical grid lines
          panel.grid.major.x = element_blank() ,
          # explicitly set the horizontal lines (or they will disappear too)
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  if(export == FALSE && plot == FALSE){
    return(p)
  }
  if (export == TRUE){
      if(is.null(out.nm)) out.nm <- paste0("drParameterPlot_", param)
      w <- ifelse(is.null(width), 7 + (3*length(nm))/20, width)
      h <- height
      out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
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
  } else {
    return(p)
  }
}
