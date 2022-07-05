#' Plot the results of a linear regression on ln-transformed data
#'
#' \code{plot.gcFitLinear} shows the results of a linear regression on log-transformed data and visualizes raw data, data points included in the fit, the tangent obtained by linear regression, and the lag time.
#'
#' @param gcFittedLinear A \code{gcFittedLinear} object created with \code{growth.gcFitLinear()} or stored within a \code{grofit} or \code{gcFit} object created with \code{growth.workflow()} or \code{growth.gcFit()}, respectively.
#' @param log ("x" or "y") Display the x- or y-axis on a logarithmic scale.
#' @param which ("fit" or "diagnostics") Display either the results of the linear fit on the raw data or statistical evaluation of the linear regression.
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
plot.gcFitLinear <- function(gcFittedLinear, log="y", which=c("fit", "diagnostics"),
                             plot = TRUE, export = FALSE, height = ifelse(which=="fit", 7, 5),
                             width = ifelse(which=="fit", 9, 9), out.dir = NULL, ...)
{
  which <- match.arg(which)

  p <- function(){
    switch(which,
           fit = {

             par(cex.lab=1.5)
             plot(gcFittedLinear$"raw.data" ~ gcFittedLinear$"raw.time", xlab="Time", ylab=ifelse(log == "y", "Density", "Density"),
                  log=log, las=1, main = "Linear fit", yaxt="n", xaxt="n", ...)
             axis(1,cex.axis=1.3)
             axis(2,cex.axis=1.3, las=1)
             try(points(gcFittedLinear$raw.data[gcFittedLinear$ndx] ~ gcFittedLinear$raw.time[gcFittedLinear$ndx], pch=21, col="black", bg="red"))

             ## lag phase
             lag <- gcFittedLinear$par["lag"]

             try(time <- seq(lag, max(gcFittedLinear$"raw.time"), length=200), silent = T)
             coef_ <- gcFittedLinear$par
             try(lines(time, gcFittedLinear$FUN(time, c(y0=unname(coef_["y0_lm"]), mumax=unname(coef_["mumax"])))[,"y"], lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
             try(lines(c(min(gcFittedLinear$"raw.time"[1]), lag), rep(gcFittedLinear$"raw.data"[1], 2), lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7)), silent = T)
           },
           diagnostics = {
             opar <- par(no.readonly = TRUE)
             on.exit(par(opar))
             par(mfrow=c(1,2))

             ## residuals vs. fitted
             obs <- gcFittedLinear$log.data
             sim <- gcFittedLinear$FUN(gcFittedLinear$"raw.time", gcFittedLinear$par)
             plot(gcFittedLinear$fit[["residuals"]] ~ fitted(gcFittedLinear$fit), xlab="fitted", ylab="residuals")
             abline(h=0, col="grey")
             ## normal q-q-plot
             qqnorm(gcFittedLinear$fit[["residuals"]])
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

#'
#' @param gcFittedModel A \code{gcFittedModel} object created with \code{growth.gcFitModel()} or stored within a \code{grofit} or \code{gcFit} object created with \code{growth.workflow()} or \code{growth.gcFit()}, respectively.
#' @param raw (Logical) Show the raw data within the plot (\code{TRUE}) or not (\code{FALSE}).
#' @param slope (Logical) Show the fitted model within the plot (\code{TRUE}) or not (\code{FALSE}).
#' @param colData (Numeric or Character) Color used to plot the raw data.
#' @param equation (Logical) Show the equation of the fitted model within the plot (\code{TRUE}) or not (\code{FALSE}).
#' @param colModel (Numeric or Character) Color used to plot the fitted model.
#' @param base_size (Numeric) Base font size.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ... Further arguments to refine the generated \code{ggplot2} plot.
#'
#' @export plot.gcFitModel
#' @export
#' @importFrom ggplot2 aes annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
plot.gcFitModel <- function(gcFittedModel, raw = TRUE, slope = TRUE, colData=1, equation = TRUE,
                            colModel=ggplot2::alpha("forestgreen", 0.85), base_size=16,
                            plot = TRUE, export = FALSE, height = 8, width = 6, out.dir = NULL,...)
{
  # x an object of class gcFitModel

  # /// check input parameters
  if (is.logical(raw)==FALSE)   stop("Need logical value for: raw")
  if (is.logical(slope)==FALSE) stop("Need logical value for: slope")
  if (is.logical(equation)==FALSE)   stop("Need logical value for: equation")
  if (is.numeric(base_size)==FALSE)   stop("Need numeric value for: base_size")
  if (!(class(gcFittedModel)=="gcFitModel"))   stop("gcFittedModel needs to be an object created with growth.gcFitModel().")


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
      geom_line(aes_(x=as.name(names(df)[3]), y = as.name(names(df)[4]), color = "model"), size = 0.7) +
      xlab("Time") +
      ylab(label = ifelse(gcFittedModel$control$log.y.gc == TRUE, "Growth [Ln(y(t)/y0)]", "Growth [y(t)]")) +
      theme_classic(base_size = 16) +
      ggtitle(gsub(" \\| NA", "", paste(gcFittedModel$gcID, collapse=" | "))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      theme(plot.title = element_text(size=15),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    if(raw == TRUE){
      p <- p + geom_point(shape=1, size = 2,alpha = 0.5, stroke=0.15)
    }
    if(equation == TRUE){
      if(gcFittedModel$model == "logistic"){
        p <- p + annotate(
          "text",
          label = "y(t) == frac(A , 1+exp(frac(4 %.% mu, A) %.% (lambda - t) + 2))",
          x = 1.08 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
          y = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
          angle = 90, parse = TRUE, size = 3.2) +
          annotate("text",
                   label = bquote(A == .(round(gcFittedModel$parameters$A[1],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[1],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[1],3)) ),
                   x = 1.13 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
                   y = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
                   angle = 90, parse = F, size = 2.5) +
          scale_color_manual(name='Growth Model',
                             breaks = "logistic",
                             values=c("model" = colModel, "logistic" = colModel))
      }
      if(gcFittedModel$model == "richards"){
        p <- p + annotate(
          "text",
          label = "y(t) == A%.%(1.0+nu%.%italic(e)^{1+nu}%.%exp(frac(mu,A)%.%(1+nu)^(1+frac(1,nu))%.%( lambda - t )))^(-1/nu)",
          x = 1.17 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
          y = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
          angle = 90, parse = TRUE, size = 3.2) +
          annotate("text",
                   label = bquote(A == .(round(gcFittedModel$parameters$A[1],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[1],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[1],3)) ~~~~ nu == .(round(as.numeric(gcFittedModel$parameters$fitpar$nu[1],3)))),
                   x = 1.22 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
                   y = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
                   angle = 90, parse = F, size = 2.5) +
          scale_color_manual(name='Growth Model',
                             breaks = "richards",
                             values=c("model" = colModel, "richards" = colModel))
      }
      if(gcFittedModel$model == "gompertz"){
        p <- p + annotate(
          "text",
          label = "y(t) == A%.%exp(-exp(frac(mu%.%italic(e),A)%.%(lambda-t) +1))",
          x = 1.08 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
          y = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
          angle = 90, parse = TRUE, size = 3.4) +
          annotate("text",
                   label = bquote(A == .(round(gcFittedModel$parameters$A[1],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[1],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[1],3)) ),
                   x = 1.13 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
                   y = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
                   angle = 90, parse = F, size = 2.5) +
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
          y = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
          angle = 90, parse = TRUE, size = 3.2) +
          annotate("text",
                   label = bquote(A == .(round(gcFittedModel$parameters$A[1],3)) ~~~~ mu == .(round(gcFittedModel$parameters$mu[1],3)) ~~~~
                                    lambda == .(round(gcFittedModel$parameters$lambda[1],2)) ~~~~ alpha == .(round(gcFittedModel$parameters$fitpar$alpha[1],3))  ~~~~
                                    t[shift] == .(round(gcFittedModel$parameters$fitpar$t_shift[1],2)) ),
                   x = 1.21 * gcFittedModel$raw.time[length(gcFittedModel$raw.time)],
                   y = 0.5 * ggplot_build(p)$layout$panel_params[[1]]$y.range[2],
                   angle = 90, parse = F, size = 2.5) +
          scale_color_manual(name='Growth Model',
                             breaks = "gompertz.exp",
                             values=c("model" = colModel, "gompertz.exp" = colModel))
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
    if(slope == T){
      # /// add tangent at maximum slope
      mu     <- as.numeric(coef$mu[1])
      bla    <- (gcFittedModel$fit.time)*mu
      bla    <- bla+(-mu*lagtime)
      tangent.df <- data.frame("time" = gcFittedModel$fit.time,
                               "y" = bla)
      df.horizontal <- data.frame("time" = c(gcFittedModel$fit.time[1], lagtime),
                                  "y" = gcFittedModel[["fit.data"]][1])
      p.yrange.end <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]
      p <- p + geom_segment(aes(x = time[which.min(abs(bla))], y = y[which.min(abs(bla))],
                                xend = time[which.min(abs(y - p.yrange.end))],
                                yend = y[which.min(abs(y - p.yrange.end))]),
                            data = tangent.df, linetype = "dashed", color = colModel, size = 0.5) +
        geom_segment(aes(x = time[1], y = y[1], xend = time[2], yend = y[2]), data = df.horizontal,
                     linetype = "dashed", color = colModel, size = 0.5)
    }
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
    }
  }
}

#'
#' @param drBootSpline A \code{drBootSpline} object created with \code{growth.drBootSpline()} or stored within a \code{grofit} or \code{drFit} object created with \code{growth.workflow()} or \code{growth.drFit()}, respectively.
#' @param pch (Numeric) Size of the raw data points.
#' @param colData (Numeric or Character) Color used to plot the raw data.
#' @param colSpline (Numeric or Character) Color used to plot the splines.
#' @param cex (Numeric) Line width of the individual splines.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ...
#'
#' @export plot.drBootSpline
#' @export
#'
plot.drBootSpline <- function (drBootSpline,
                               pch = 1,
                               colData = 1,
                               colSpline = scales::alpha("black", 0.15),
                               cex = 0.5, plot = TRUE, export = FALSE,
                               height = 7, width = 9, out.dir = NULL,
                               ...)
{
  # drBootSpline an object of class drBootSpline
  if(class(drBootSpline) != "drBootSpline") stop("drBootSpline needs to be an object created with growth.drBootSpline.")
  # /// initialize "Empty Plot" function
  empty.plot  <- function(text = "Empty plot", main = "") {
    plot(c(0, 1, 0, 1, 0), c(0, 1, 1, 0, 0),
      type = "l", axes = FALSE, xlab = "", ylab = "", lwd = 1,
      col = "gray", main = main)
    lines(c(0, 0), c(0, 1),
          type = "l", lwd = 1, col = "gray")
    lines(c(1, 1), c(1, 0),
          type = "l", lwd = 1, col = "gray")
    text(0.5, 0.1, text, col = "gray")
  }

  # /// check input parameters
  if (FALSE %in% (colData %in% c(colors(), 0:8)))
    stop("colData needs to be numeric from 0:8 or a string from colors()")
  if (is.numeric(pch) == FALSE)
    stop("Need numeric value for: pch")
  if (is.numeric(cex) == FALSE)
    stop("Need numeric value for: cex")

  if (drBootSpline$bootFlag == FALSE) {
    empty.plot()
  }
  else{
    p1 <- function(){
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
          ylab = "Response"
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
            ylab = "Response"
          )
        }
        else{
          if ((drBootSpline$control$log.x.dr == TRUE) && (drBootSpline$control$log.y.dr == TRUE)) {
            plot(
              c(global.minx, global.maxx),
              c(global.miny, global.maxy),
              type = "n",
              xlab = "Ln(1+Concentration)",
              ylab = "Ln(1+response)"
            )
          }
          else{
            if ((drBootSpline$control$log.x.dr == FALSE) && (drBootSpline$control$log.y.dr == TRUE)) {
              plot(
                c(global.minx, global.maxx),
                c(global.miny, global.maxy),
                type = "n",
                xlab = "Concentration",
                ylab = "Ln(1+response)"
              )
            }
          }
        }
      }

      # /// plot raw data
      points(
        drBootSpline$raw.conc,
        drBootSpline$raw.test,
        col = colData,
        pch = pch,
        cex = cex
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
          cex = cex,
          lwd = 1
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
    } # p <- function()
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
      p1()
      p2()
    }
  } # /// else of if (drBootSpline$bootFlag==FALSE){
}

#'
#' @param drFit
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ...
#'
#' @export plot.drFit
#' @export
#'
plot.drFit <- function(drFit, plot = TRUE, export = FALSE, out.dir = NULL, ...)
{
  # x an object of class drFit

  n <- length(drFit$drFittedSplines)

  # /// plot all drFitSpline objects
  for (i in 1:n) {
    try(plot(drFit$drFittedSplines[[i]], export = export, plot = plot, height = 7, width = 9, out.dir = out.dir))
  }

}

#'
#' @param drFitSpline
#' @param add
#' @param ec50line
#' @param pch
#' @param colSpline
#' @param colData
#' @param cex
#' @param lwd
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
            pch = 1,
            colSpline = 1,
            colData = 1,
            cex = 1,
            lwd = 2, plot = TRUE, export = FALSE,
            height = 7, width = 9, out.dir = NULL,
            ...)
  {
    # drFitSpline an object of class drFitSpline
    if(class(drFitSpline) != "drFitSpline") stop("drFitSpline needs to be an object created with growth.drFitSpline.")
    # /// check input parameters
    if (is.logical(add) == FALSE)
      stop("Need logical value for: add")
    if (is.logical(ec50line) == FALSE)
      stop("Need logical value for: ec50line")
    if (is.numeric(pch) == FALSE)
      stop("Need numeric value for: pch")
    if (is.numeric(cex) == FALSE)
      stop("Need numeric value for: cex")
    p <- function(){
      if (add == FALSE) {
        if ((drFitSpline$control$log.x.dr == TRUE) && (drFitSpline$control$log.y.dr == TRUE)) {
          plot(
            log(drFitSpline$raw.conc + 1),
            log(drFitSpline$raw.test + 1),
            pch = pch,
            cex = cex,
            col = colData,
            xlab = "ln(1+concentration)",
            ylab = "ln(1+response)"
          )
        }
        else
        {
          if ((drFitSpline$control$log.x.dr == FALSE) && (drFitSpline$control$log.y.dr == TRUE)) {
            plot(
              drFitSpline$raw.conc,
              log(drFitSpline$raw.test + 1),
              pch = pch,
              cex = cex,
              col = colData,
              xlab = "concentration",
              ylab = "ln(1+response)"
            )
          }
          else
          {
            if ((drFitSpline$control$log.x.dr == TRUE) && (drFitSpline$control$log.y.dr == FALSE)) {
              plot(
                log(drFitSpline$raw.conc + 1),
                drFitSpline$raw.test,
                pch = pch,
                cex = cex,
                col = colData,
                xlab = "Ln(1+concentration)",
                ylab = paste0("Response", ifelse(!is.na(drFitSpline$parameters$test), paste0(" (", drFitSpline$parameters$test, ")"), ""))
              )
            }
            else
            {
              if ((drFitSpline$control$log.x.dr == FALSE) && (drFitSpline$control$log.y.dr == FALSE)) {
                plot(
                  drFitSpline$raw.conc,
                  drFitSpline$raw.test,
                  pch = pch,
                  cex = cex,
                  col = colData,
                  xlab = "Concentration",
                  ylab = paste0("Response", ifelse(!is.na(drFitSpline$parameters$test), paste0(" (", drFitSpline$parameters$test, ")"), ""))
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
            pch = pch,
            cex = cex,
            col = colData
          )
        }
        else{
          if ((drFitSpline$control$log.x.dr == FALSE) && (drFitSpline$control$log.y.dr == TRUE)) {
            points(
              drFitSpline$raw.conc,
              log(drFitSpline$raw.test + 1),
              pch = pch,
              cex = cex,
              col = colData
            )
          }
          else
          {
            if ((drFitSpline$control$log.x.dr == TRUE) && (drFitSpline$control$log.y.dr == FALSE)) {
              points(
                log(drFitSpline$raw.conc + 1),
                drFitSpline$raw.test,
                pch = pch,
                cex = cex,
                col = colData
              )
            }
            else
            {
              if ((drFitSpline$control$log.x.dr == FALSE) && (drFitSpline$control$log.y.dr == FALSE)) {
                points(
                  drFitSpline$raw.conc,
                  drFitSpline$raw.test,
                  pch = pch,
                  cex = cex,
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

#'
#' @param gcBootSpline
#' @param pch
#' @param colData
#' @param deriv
#' @param colSpline
#' @param cex
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}).
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ...
#'
#' @export plot.gcBootSpline
#' @export
#'
plot.gcBootSpline <- function(gcBootSpline, pch=1, colData=1, deriv = TRUE,
                              colSpline=ggplot2::alpha("dodgerblue3", 0.2),
                              cex=1, plot = TRUE, export = FALSE,
                              height = 7, width = 9, out.dir = NULL, ...)
{
  # gcBootSpline an object of class gcBootSpline
  if(class(gcBootSpline) != "gcBootSpline") stop("gcBootSpline needs to be an object created with growth.gcBootSpline.")
  # /// initialize "Empty Plot" function
  empty.plot <- function(text="Empty plot",main=""){
    plot(c(0,1,0,1,0),c(0,1,1,0,0), type="l", axes=FALSE, xlab="", ylab="", lwd=1, col="gray",main=main)
    lines(c(0,0),c(0,1), type="l", lwd=1, col="gray")
    lines(c(1,1),c(1,0), type="l", lwd=1, col="gray")
    text(0.5,0.1,text, col="gray")
  }

  # /// check input parameters
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (is.numeric(cex)==FALSE)   stop("Need numeric value for: cex")
  if (gcBootSpline$bootFlag==FALSE){
    empty.plot()
  }
  else{
    p1 <- function()
      {
      par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
      colSpline <- rep(colSpline, (gcBootSpline$control$nboot.gc%/%length(colSpline))+1)

      log.x     <- gcBootSpline$control$log.x.gc
      log.y     <- gcBootSpline$control$log.y.gc

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
      } else {
        par(mai=c(0.7,0.8,0.5,0))
      }
      plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="",ylab="")

      # /// plot data
      points(gcBootSpline$raw.time, gcBootSpline$raw.data, col=colData, pch=pch, cex=cex)

      # /// plot all gcFitSpline objects
      for(i in 1:gcBootSpline$control$nboot.gc){
       plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = T,
                        deriv = FALSE, plot = F, export = F, pch=0, colSpline=colSpline[i], cex=cex)
      }
      # add plot title
      title(paste(gcBootSpline$gcID, collapse = "_"), line = ifelse(deriv==T, 0.8, 1))
      #add axis titles
      if (log.y==FALSE){
        title(ylab = "Growth y(t) ", line = 2.3, cex.lab = 1.2)
      }
      else if (log.y==TRUE){
        title(ylab = "Growth [Ln(y(t)/y0)]", line = 2.3, cex.lab = 1.2)
      }
      # add second plot with slope over time
      if(deriv == TRUE){
        par(mai=c(0.7,0.8,0.2,0))
        y.max <- ceiling(max(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) max(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        y.min <- floor(min(unlist(lapply(1:length(gcBootSpline$boot.gcSpline), function(x) min(gcBootSpline$boot.gcSpline[[x]]$spline.deriv1$y))))*10)/10
        if ((gcBootSpline$control$log.x.gc==FALSE)){
          try( plot(gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y,
                    xlab="", ylab="", type = "l", col = colSpline, ylim = c(y.min, y.max) ) )
        }
        if ((gcBootSpline$control$log.x.gc==TRUE)){
          try( lines(gcBootSpline$boot.gcSpline[[1]]$x, gcBootSpline$boot.gcSpline[[1]]$spline.deriv1$y, xlab="Ln(1+time)", ylab="Growth rate", type = "l") )
        }
        for(i in 2:gcBootSpline$control$nboot.gc){
          plot.gcFitSpline(gcBootSpline$boot.gcSpline[[i]], add = TRUE, slope = FALSE, spline = F,
                           deriv = T, plot = F, export = F, pch=0, colSpline=colSpline[i], cex=cex)
        }
        title(ylab = bquote("Growth rate \U00B5 "~(h^-1)), line = 2.3, cex.lab = 1.2)
      }
      if (log.x==TRUE){
        title(xlab = "Ln(1+time)", line = 2.3, cex.lab = 1.2)
      }
      else if(log.x==FALSE){
        title(xlab = "Time", line = 2.3, cex.lab = 1.2)
      }
      par(mfrow=c(1,1))
    } # p1 <- function()
    p2 <- function()
      {
      lambda    <- gcBootSpline$lambda
      mu        <- gcBootSpline$mu
      A         <- gcBootSpline$A
      integral  <- gcBootSpline$integral

      # /// plot histograms of growth parameters
      par(mfrow=c(2,2))
      if (sum(!is.na(lambda))>1){
        try(hist(lambda, col="gray",xlab="lambda", main=expression(lambda)))
      }
      else{
        empty.plot("Empty plot!")
      }

      if (sum(!is.na(mu))>1){ try(hist(mu , col="gray", xlab="mu", main=expression(mu))) } else { empty.plot("Empty plot!", main=expression(mu)) }
      if (sum(!is.na(A))>1){ try(hist(A, col="gray", xlab="A", main=expression(A))) } else { empty.plot("Empty plot!", main=expression(A)) }
      if (sum(!is.na(integral))>1){ try(hist(integral, col="gray", xlab="integral", main=expression(Integral))) } else { empty.plot("Empty plot!", main=expression(Integral))}
      mtext(paste(gcBootSpline$gcID, collapse = "_"), side = 3, line = -1, outer = TRUE)
      par(mfrow=c(1,1))
      par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    } # p2 <- function()
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
    p1()
    dev.new()
    p2()
  }
  # restore standard plot parameters
  par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
  par(mfrow = c(1, 1))
}

#'
#' @param gcFitSpline
#' @param add
#' @param slope
#' @param deriv
#' @param spline
#' @param log.y
#' @param pch
#' @param colData
#' @param colSpline
#' @param cex
#' @param lwd
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#' @param ...
#'
#' @export plot.gcFitSpline
#' @export
#' @importFrom ggplot2 aes annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
plot.gcFitSpline <- function(gcFitSpline, add=FALSE, slope=TRUE, deriv = T, spline = T, log.y = T,
                             pch=1, colData=1, colSpline="dodgerblue3", cex=1, lwd = 0.7,
                             plot = TRUE, export = FALSE, width = 8, height = ifelse(deriv == TRUE, 8, 6),
                             out.dir = NULL, ...)
{

  # x an object of class gcFitSpline
  if(class(gcFitSpline) != "gcFitSpline") stop("gcFitSpline needs to be an object created with growth.gcFitSpline().")
  # /// check input parameters
  if (is.logical(add)==FALSE)   stop("Need logical value for: add")
  if (is.logical(slope)==FALSE) stop("Need logical value for: slope")
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (is.numeric(cex)==FALSE)   stop("Need numeric value for: cex")

  # /// check if a data fit is available
  if ((is.na(gcFitSpline$fitFlag)==TRUE)|(gcFitSpline$fitFlag==FALSE)){
    warning("plot.gcFitSpline: no data fit available!")
  }
  else{
    if (add==TRUE){
      if(spline == TRUE){
        # /// try to plot data fit
        if ((gcFitSpline$control$log.x.gc==FALSE) && (gcFitSpline$control$log.y.gc==FALSE)){
          try( lines(gcFitSpline$fit.time, gcFitSpline$fit.data, sub=gcFitSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd) )
        }

        if ((gcFitSpline$control$log.x.gc==FALSE) && (gcFitSpline$control$log.y.gc==TRUE)){
          try( lines(gcFitSpline$fit.time, gcFitSpline$fit.data, sub=gcFitSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd) )
        }

        if ((gcFitSpline$control$log.x.gc==TRUE)  && (gcFitSpline$control$log.y.gc==FALSE)){
          try( lines(gcFitSpline$fit.time, gcFitSpline$fit.data, sub=gcFitSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd ) )
        }

        if ((gcFitSpline$control$log.x.gc==TRUE)  && (gcFitSpline$control$log.y.gc==TRUE)){
          try( lines(gcFitSpline$fit.time, gcFitSpline$fit.data, sub=gcFitSpline$name.fit, col=colSpline, type="l", lwd=2.8*lwd) )
        }
        # /// add tangent at maximum slope
        if (slope==TRUE){
          mu     <- as.numeric(gcFitSpline$parameters$mu)
          lambda <- as.numeric(gcFitSpline$parameters$lambda)

          time <- seq(lambda, max(gcFitSpline$"fit.time"), length=200)
          y_tangent <- gcFitSpline$parameters["b.tangent"][[1]]+time*mu
          try(lines(time, y_tangent, lty=2, lwd=2, col=ggplot2::alpha(colSpline, 0.85), ...))
          try(lines(c(min(gcFitSpline$"raw.time"[1]), lambda), rep(gcFitSpline$"raw.data"[1], 2), lty=2, lwd=2, col=ggplot2::alpha(colSpline, 0.7)))
        }
      }
      if (deriv  == TRUE){
        if ((gcFitSpline$control$log.x.gc==FALSE)){
          try( lines(gcFitSpline$spline.deriv1$x, gcFitSpline$spline.deriv1$y, xlab="", ylab="", col = colSpline) )
        }
        if ((gcFitSpline$control$log.x.gc==TRUE)){
          try( lines(gcFitSpline$spline.deriv1$x, gcFitSpline$spline.deriv1$y, xlab="", ylab="", col = colSpline) )
        }
      }
    } # if (add == TRUE)
    else {
      coef <- gcFitSpline[["parameters"]]
      lagtime <- coef["lambda"][[1]][1]
      # correct for log transformation
      if(gcFitSpline$control$log.y.gc == TRUE){
        fit.data <-
          c(rep(NA, length(gcFitSpline[["raw.data"]]) - length(gcFitSpline[["fit.data"]])), exp(gcFitSpline[["fit.data"]]) *
              gcFitSpline[["data.in"]][1])
      } else {
        fit.data <- c(rep(NA, length(gcFitSpline[["raw.data"]]) - length(gcFitSpline[["fit.data"]])), gcFitSpline[["fit.data"]])
      }
      df <- data.frame("time" = gcFitSpline[["raw.time"]],
                       "data" = exp(gcFitSpline[["raw.data"]])*gcFitSpline[["data.in"]][1],
                       "fit.time" = c(rep(NA, length(gcFitSpline[["raw.time"]])-length(gcFitSpline[["fit.time"]])), gcFitSpline[["fit.time"]]),
                       "fit.data" = fit.data)

      p <- ggplot(df, aes(x=time, y=data)) +
        geom_point(shape=1, size = 2,alpha = 0.6, stroke=0.15) +
        geom_line(aes(x=fit.time, y = fit.data, color = "spline"), size = lwd) +
        xlab("Time") +
        ylab(label = "Growth [y(t)]") +
        theme_classic(base_size = 16) +
        ggtitle(gsub(" \\| NA", "", paste(gcFitSpline$gcID, collapse=" | "))) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        theme(legend.key = element_blank(),
              legend.background=element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.90, 0.08),
              plot.title = element_text(size=15),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_color_manual(name='Growth Model',
                           breaks = "Spline fit",
                           values=c("spline" = ggplot2::alpha(colSpline, 0.85), "Spline fit" = ggplot2::alpha(colSpline, 0.85)))


        p.yrange.end <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]

      if(log.y == TRUE){
      p <- p + scale_y_continuous(breaks = scales::pretty_breaks(), trans = 'log')
      } else {
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks())
      }


      # /// add tangent at maximum slope
      if(slope == TRUE && log.y == T){
        mu     <- as.numeric(coef$mu[1])
        # time values for tangent
        time <- seq(ifelse(lagtime<0, 0, lagtime), max(gcFitSpline$"fit.time"), length=200)
        # y values for tangent
        bla <- ifelse(gcFitSpline$control$log.y.gc == TRUE, exp(coef["b.tangent"][[1]])*gcFitSpline[["data.in"]][1], coef["b.tangent"][[1]])*exp(mu*time)
        tangent.df <- data.frame("time" = time,
                                 "y" = bla)
        df.horizontal <- data.frame("time" = c(gcFitSpline[["raw.time"]][1], lagtime),
                                    "y" = gcFitSpline[["data.in"]][1])
        p <- p + geom_segment(aes(x = time[which.min(abs(bla))], y = y[which.min(abs(bla))],
                                  xend = time[which.min(abs(y - 1.1*p.yrange.end))],
                                  yend = y[which.min(abs(y - 1.1*p.yrange.end))]),
                              data = tangent.df, linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
        if(!(lagtime <0)){
        p <- p + geom_segment(aes(x = time[1], y = y[1], xend = time[2], yend = y[2]), data = df.horizontal,
                       linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
        }
      }

      # /// add panel with growth rate over time
      if(deriv == TRUE){
        df.mu <- data.frame(spline(gcFitSpline$spline.deriv1$x, gcFitSpline$spline.deriv1$y))
        #add missing time values due to min.density and t0
        df.mu <-
          bind_rows(data.frame(x = df$time[is.na(df$fit.data)], y = rep(NA, length(df$time[is.na(df$fit.data)]))),
                    df.mu)

        p.mu <- ggplot(df.mu, aes(x=x, y=y)) +
          geom_line(color = colSpline, size = lwd) +
          theme_classic(base_size = 15) +
          xlab("Time") +
          ylab(label = bquote("Exp. growth rate \U00B5 "~(h^-1))) +
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
        grDevices::png(paste0(out.dir, "/", paste(gcFitSpline$gcID, collapse = "_"), "_SplineFit.png"),
                       width = w, height = h, units = 'in', res = 300)
        print(p)
        grDevices::dev.off()
        grDevices::pdf(paste0(out.dir, "/", paste(gcFitSpline$gcID, collapse = "_"), "_SplineFit.pdf"), width = w, height = h)
        print(p)
        grDevices::dev.off()
      }
      if (plot == TRUE){
        print(p)
      }
    } # else of if (add == TRUE)
  }
}

#' Combine different groups of samples into a single plot
#'
#' \code{plot.grofit} extracts the spline fits of a subset of samples in a \code{grofit} object calculates averages and standard deviations of conditions with replicates and combines them into a single plot.
#'
#'
#' @param grofit A \code{grofit} object created with \code{growth.workflow()} containing spline fits.
#' @param names (String or string vector) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered.
#' @param mean (Logical) Display the mean and standard deviation of groups with replicates (\code{TRUE}) or plot each sample individually (\code{FALSE})?
#' @param log.y (Logical) Log-transform the y-axis of the plot (\code{TRUE}) or not (\code{FALSE})?
#' @param deriv (Logical) Show derivatives over time in a separate panel below the plot (\code{TRUE}) or not (\code{FALSE})?
#' @param n.ybreaks (Numeric) Number of breaks on the y-axis. The breaks are generated using \code{scales::pretty_breaks}. Thus, the final number of breaks can deviate from the user input.
#' @param colors (String vector) Define a color palette used to draw the plots. If \code{NULL}, default palettes are chosen based on the number of groups/samples within the plot. Note: The number of provided colors should at least match the number of groups/samples.
#' @param basesize (Numeric) Base font size.
#' @param lwd (Numeric) Line width of the individual plots.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#'
#' @export plot.grofit
#' @export
#' @importFrom ggplot2 aes annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
plot.grofit <- function(grofit, names = NULL, conc = NULL, mean = TRUE, log.y = T, deriv = T, n.ybreaks = 6,
                        colors = NULL, basesize = 20, lwd = 1.1, plot = TRUE, export = FALSE,
                        height = ifelse(deriv==T,9, 6), width = 10 + 3*ifelse(mean==TRUE,length(conditions_unique), length(nm))/15,
                        out.dir = NULL)
{

  # grofit an object of class grofit
  if(class(grofit) != "grofit") stop("grofit needs to be an object created with growth.workflow().")
  # /// check input parameters

  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(lwd)==FALSE)   stop("Need numeric value for: lwd")


  # Get name of conditions with multiple replicates
  sample.nm <- nm <- as.character(names(grofit$gcFit$gcFittedSplines))
  if(!is.null(names)){
    names <- gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", names))
    nm <- nm[grep(paste(names, collapse="|"), nm)]
  }
  if(!is.null(conc)){
    nm <- nm[which(str_extract(nm, "[:alnum:]+$") %in% conc)]
  }
  if(length(nm)==0){
    stop("Please run plot.grofit() with valid 'names' or 'conc' argument.")
  }
  # remove conditions with fitFlag = FALSE in all replicates
  ndx.filt.nm <- unique(lapply(1:length(sample.nm), function(i)which(gsub(" \\| .+", "", sample.nm) %in% (paste0(unlist(str_split(sample.nm[i], " \\| "))[1])))))
  filter.ls <- list()
  for(j in 1:length(ndx.filt.nm)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.nm[[j]]), function(i) ndx.filt.nm[[j]][grep(paste0("^",
                                                                                                         gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", unlist(str_split(sample.nm[ndx.filt.nm[[j]][i]], " \\| "))[1])),
                                                                                                         ".+[[:space:]]",
                                                                                                         unlist(str_split(sample.nm[ndx.filt.nm[[j]][i]], " \\| "))[3],
                                                                                                         "$"), sample.nm[ndx.filt.nm[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = F)

  for(i in 1:length(ndx.filt)){
    if(!all(unlist(lapply(1:length(ndx.filt[[i]]), function(j) (grofit[["gcFit"]][["gcFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]]))))){
      nm <- intersect(nm, sample.nm[-ndx.filt[[i]]])
    }
  }

  #get indices of samples with selected names
  ndx <- grep(paste0(
    str_replace_all(nm, "\\|", "\\\\|"), collapse = "|"), sample.nm)

  # correct for log transformation
  if(grofit$control$log.y.gc == TRUE){
      for(i in 1:length(ndx)){
        grofit$gcFit$gcFittedSplines[[ndx[i]]][["fit.data"]] <-
          exp(grofit$gcFit$gcFittedSplines[[ndx[i]]][["fit.data"]]) * grofit$gcFit$gcFittedSplines[[ndx[i]]]$data.in[1]
      }
  }

  if(mean == TRUE){
    conditions <- str_replace_all(nm, "\\| . \\| ", "| ")
    conditions_unique <- unique(conditions)

    plotdata.ls <- list()
    deriv.ls <- list()
    for(n in 1:length(conditions_unique)){
      # find indexes of replicates
      ndx <- grep(paste0("^",
                         gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", unlist(str_split(conditions_unique[n], " \\| "))[1])),
                         ".+[[:space:]]",
                         unlist(str_split(conditions_unique[n], " \\| "))[2],
                         "$"), sample.nm)
      name <- conditions_unique[n]
      time <- lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$fit.time)) %>% as.list(.)
      data <- lapply(1:length(ndx), function(i) cbind(grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$fit.data)) %>% as.list(.)
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
            data[[i]] <- append(data[[i]],
                                values = NA,
                                after = match(get(paste0("time.missing_", i))[j],
                                              time.all) - 1)
            time[[i]] <-
              append(time[[i]],
                     values = get(paste0("time.missing_", i))[j],
                     after = match(get(paste0("time.missing_", i))[j], time.all) - 1)
          }
        }
      }
      if(deriv){
        # correct for unequal lengths of derivative series
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
      xlab("Time") +
      ylab("Growth [y(t)]") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    if(log.y == TRUE){
      p <- p + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
    } else {
      p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))
    }

    if(is.null(colors)){
      if (length(plotdata.ls) <= 8) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else {
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
      p.deriv <- ggplot(df.deriv, aes(x=time, y=mean, col = name)) +
        geom_line(size=lwd) +
        geom_ribbon(aes(ymin=lower,ymax=upper, fill=name), alpha = 0.3, colour = NA) +
        theme_classic(base_size = basesize) +
        xlab("Time") +
        ylab(label = bquote("Exp. growth rate \U00B5 "~(h^-1))) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks, bounds = FALSE))

      if(is.null(colors)){
        if (length(plotdata.ls) <= 8) {
          p.deriv <- p.deriv + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
        } else {
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
        p <- ggpubr::ggarrange(p, p.deriv, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1), common.legend = T, legend = "right")
    }
  } # if(mean == TRUE)
  else {
    df <- data.frame()
    for(i in 1:length(ndx)){
      df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx[i]],
                                            "time" = grofit$gcFit$gcFittedSplines[[ndx[i]]][["fit.time"]],
                                            "y" = grofit$gcFit$gcFittedSplines[[ndx[i]]][["fit.data"]]))
    }
    p <- ggplot(df, aes(x=time, y=y, col = name)) +
      geom_line(size=lwd) +
      theme_classic(base_size = basesize) +
      xlab("Time") +
      ylab("Growth [y(t)]") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    if(log.y == TRUE){
      p <- p + scale_y_log10(breaks = scales::pretty_breaks(n = n.ybreaks))
    } else {
      p <- p + scale_y_continuous(breaks = scales::pretty_breaks(n = n.ybreaks))
    }

    if(is.null(colors)){
      if (length(ndx) <= 8) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
      } else if (length(ndx) <= 12) {
        p <- p + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
      } else {
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
      for(i in 1:length(ndx)){
        df.deriv <- plyr::rbind.fill(df.deriv, data.frame("name" = sample.nm[ndx[i]],
                                              "time" = grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$spline.deriv1$x,
                                              "y" = grofit$gcFit$gcFittedSplines[[ndx[[i]]]]$spline.deriv1$y))
      }
      p.deriv <- ggplot(df.deriv, aes(x=time, y=y, col = name)) +
        geom_line(size=lwd) +
        theme_classic(base_size = basesize) +
        xlab("Time") +
        ylab(label = bquote("Exp. growth rate \U00B5 "~(h^-1))) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

      if(is.null(colors)){
        if (length(ndx) <= 8) {
          p.deriv <- p.deriv + scale_fill_brewer(name = "Condition", palette = "Set2") + scale_color_brewer(name = "Condition", palette = "Dark2")
        } else if (length(ndx) <= 12) {
          p.deriv <- p.deriv + scale_fill_brewer(name = "Condition", palette = "Set3") + scale_color_brewer(name = "Condition", palette = "Set3")
        } else {
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
      p <- ggpubr::ggarrange(p, p.deriv, ncol = 1, nrow = 2, align = "v", heights = c(2,1.1), common.legend = T, legend = "right")
    } # if(deriv)

  }
  if(export == FALSE && plot == FALSE){
    return(p)
  }
  out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
  if (export == TRUE){
    w <- width
    h <- height
    dir.create(out.dir, showWarnings = F)
    grDevices::png(paste0(out.dir, "/", "grpSplineFit.png"),
                   width = w, height = h, units = 'in', res = 300)
    print(p)
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", "grpSplineFit.pdf"), width = w, height = h)
    print(p)
    grDevices::dev.off()
  }
  if (plot == TRUE){
    print(p)
  }
}

base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

#' Compare growth parameters between samples or conditions
#'
#' \code{plot.parameter} gathers physiological parameters from the results of a growth fit analysis and compares a chosen parameter between each sample or condition in a column plot.
#'
#' @param object A \code{grofit}, \code{gcFit}, or \code{gcTable} object obtained with \code{growth.workflow()} or \code{growth.gcFit}.
#' @param param (Character) The parameter used to compare different sample groups. Any name of a column containing numeric values in \code{gcTable} (which is stored within \code{grofit} or \code{gcFit} objects) can be used as input. Useful options are:
#' 'mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit',
#' 'mu.model', 'lambda.model', 'A.model',
#' 'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline',
#' 'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt'
#' @param names (String or string vector) Define groups to combine into a single plot. Partial matches with sample/group names are accepted. If \code{NULL}, all samples are considered.
#' @param conc (Numeric or numeric vector) Define concentrations to combine into a single plot. If \code{NULL}, all concentrations are considered.
#' @param basesize (Numeric) Base font size.
#' @param plot (Logical) Show the generated plot in the \code{Plots} pane (\code{TRUE}) or not (\code{FALSE}). If \code{FALSE}, a ggplot object is returned.
#' @param export (Logical) Export the generated plot as PDF and PNG files (\code{TRUE}) or not (\code{FALSE}).
#' @param height (Numeric) Height of the exported image in inches.
#' @param width (Numeric) Width of the exported image in inches.
#' @param out.dir (Character) Name or path to a folder in which the exported files are stored. If \code{NULL}, a "Plots" folder is created in the current working directory to store the files in.
#'
#' @export plot.parameter
#' @export
#' @importFrom ggplot2 aes annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
plot.parameter <- function(object, param = c('mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit',
                                               'mu.model', 'lambda.model', 'A.model',
                                               'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline',
                                               'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt'),
                            names = NULL, conc = NULL, basesize = 12, plot = T, export = F, height = 7, width = NULL, out.dir = NULL){
  param <- match.arg(param)
  # check class of object
  if(!(any(class(object) %in% c("gcTable", "grofit", "gcFit")))) stop("object needs to be either a 'grofit', 'gcTable', or 'gcFit' object created with growth.workflow() or growth.gcFit().")
  if(!is.character(param) || !(param %in% c('mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit',
                                            'mu.model', 'lambda.model', 'A.model',
                                            'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline',
                                            'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt')))
                                            stop("param needs to be a character string and one of:\n 'mu.linfit', 'lambda.linfit', 'dY.linfit', 'A.linfit', 'mu.model', 'lambda.model', 'A.model', 'mu.spline', 'lambda.spline', 'A.spline', 'dY.spline', 'integral.spline', 'mu.bt', 'lambda.bt', 'A.bt', 'integral.bt'.")

  #extract gcTable
  if(any(class(object) %in% "gcTable")){
    gcTable <- object
  } else if (class(object)=="gcFit"){
    gcTable <- object$gcTable
  } else if (class(object)=="grofit"){
    gcTable <- object$gcFit$gcTable
  }
  #check if param exists in gcTable and has a valid value
  if(all(is.na(gcTable[[param]]))){
    if(gsub(".+\\.", "", param)=="linfit") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow() with 'fit.opt' containing 'l' or 'a', or growth.gcFit() with a control object with 'fit.opt' containing 'l' or 'a'."))
    if(gsub(".+\\.", "", param)=="model") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow() with 'fit.opt' containing 'm' or 'a', or growth.gcFit() with a control object with 'fit.opt' containing 'm' or 'a'."))
    if(gsub(".+\\.", "", param)=="spline") stop(paste0("All values for param = '", param, "' are NA. Please run growth.workflow() with 'fit.opt' containing 's' or 'a', or growth.gcFit() with a control object with 'fit.opt' containing 's' or 'a'."))
  }
  # Get name of conditions with multiple replicates
  sample.nm <- nm <- as.character(paste(gcTable[,1], gcTable[,2], gcTable[,3], sep = " | "))
  if(!is.null(names)){
    names <- gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", names))
    nm <- nm[grep(paste(names, collapse="|"), nm)]
  }
  if(!is.null(conc)){
    nm <- nm[which(conc == str_extract(nm, "[:alnum:]+$"))]
  }
  if(length(nm)==0){
    stop("Please run plot.parameters() with valid 'names' or 'conc' argument.")
  }
  # get indices of replicates
  ndx.filt.nm <- unique(lapply(1:length(nm), function(i)which(gsub(" \\| .+", "", nm) %in% (paste0(unlist(str_split(nm[i], " \\| "))[1])))))
  filter.ls <- list()
  for(j in 1:length(ndx.filt.nm)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.nm[[j]]), function(i) ndx.filt.nm[[j]][grep(paste0("^",
                                                                          gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", unlist(str_split(nm[ndx.filt.nm[[j]][i]], " \\| "))[1])),
                                                                          ".+[[:space:]]",
                                                                          unlist(str_split(nm[ndx.filt.nm[[j]][i]], " \\| "))[3],
                                                                          "$"), nm[ndx.filt.nm[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = F)
  names(ndx.filt) <- unlist(lapply(1:length(ndx.filt), function (x) nm[ndx.filt[[x]][1]]) )
  # calculate average param values
  mean <- unlist(lapply(1:length(ndx.filt), function (x) mean(as.numeric(gcTable[ndx.filt[[x]], param]), na.rm = T)) )
  sd <- unlist(lapply(1:length(ndx.filt), function (x) sd(as.numeric(gcTable[ndx.filt[[x]], param]), na.rm = T) ) )

  df <- data.frame(name = gsub(" \\| NA", "", gsub(" \\| [[:digit:]]+ \\| ", " | ", names(ndx.filt))), mean = mean, sd = sd)
  df$name <- factor(df$name, levels = df$name)
  df$group <- gsub(" \\|.+", "", df$name)
  df$mean[is.na(df$mean)] <- 0
  df$sd[is.na(df$sd)] <- 0
  p <- ggplot(df, aes(x=name, y=mean, fill = group)) +
    geom_bar(stat="identity", color = "black") +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, size = 1,
                  position=position_dodge(.9)) +
    ggplot2::labs(x = "Sample", y = param) +
    theme_minimal(base_size = basesize) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12-length(unique(df$name))^(1/3)),
          plot.margin = unit(c(1, 1, 1, nchar(as.character(df$name)[1])/6), "lines"))
  if(export == FALSE && plot == FALSE){
    return(p)
  }
  if (export == TRUE){
    w <- ifelse(is.null(width), 7 + (3*length(nm))/20, width)
    h <- height
    out.dir <- ifelse(is.null(out.dir), paste0(getwd(), "/Plots"), out.dir)
    dir.create(out.dir, showWarnings = F)
    grDevices::png(paste0(out.dir, "/", "ParameterPlot_", param, ".png"),
                   width = w, height = h, units = 'in', res = 300)
    print(p)
    grDevices::dev.off()
    grDevices::pdf(paste0(out.dir, "/", "ParameterPlot_", param, ".pdf"), width = w, height = h)
    print(p)
    grDevices::dev.off()
  }
  if (plot == TRUE){
    print(p)
  }
}


