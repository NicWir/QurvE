plot.flFitLinear <- function(flFittedLinear, log="", which=c("fit", "diagnostics"),
                             plot = TRUE, export = FALSE, height = ifelse(which=="fit", 7, 5),
                             width = ifelse(which=="fit", 9, 9), out.dir = NULL, ...)
  {
  if(is(flFittedLinear) != "flFitLinear") stop("flFitLinear needs to be an object created with flFitLinear().")
  which <- match.arg(which)

  p <- function(){
    switch(which,
           fit = {

             par(cex.lab=1.5)
             plot(flFittedLinear$"filt.fl" ~ flFittedLinear$"filt.x", xlab="Time", ylab=ifelse(log == "y", "Density", "Density"),
                  log=log, las=1, main = "Linear fit", yaxt="n", xaxt="n", ...)
             axis(1,cex.axis=1.3)
             axis(2,cex.axis=1.3, las=1)
             try(points(flFittedLinear$raw.fl[flFittedLinear$ndx] ~ flFittedLinear$raw.x[flFittedLinear$ndx], pch=21, col="black", bg="red"))

             ## lag phase
             lag <- flFittedLinear$par["lag"]
             coef_ <- flFittedLinear$par


             if(flFittedLinear$fitflag2){
               try(points(flFittedLinear$raw.fl[flFittedLinear$ndx2] ~ flFittedLinear$raw.x[flFittedLinear$ndx2], pch=21, col="black", bg=ggplot2::alpha("magenta3", 1)))
               lag2 <- flFittedLinear$par["lag2"]
               if(lag2 < lag){
                 try(time2 <- seq(lag2, max(flFittedLinear$"raw.x"), length=200), silent = T)
                 try(time <- seq(coef_["x.max_start"]-0.25*(coef_["x.max_end"]-coef_["x.max_start"]), max(flFittedLinear$"raw.x"), length=200), silent = T)
                 try(lines(time2, grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=2, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)
                 try(lines(c(min(flFittedLinear$"raw.x"[1]), lag2), rep(flFittedLinear$"raw.fl"[1], 2), lty=2, lwd=2, col=ggplot2::alpha("magenta3", 0.7)), silent = T)
                 try(lines(time, grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               } else {
                 try(time2 <- seq(coef_["x.max2_start"]-0.25*(coef_["x.max2_end"]-coef_["x.max2_start"]), max(flFittedLinear$"raw.x"), length=200), silent = T)
                 try(time <- seq(lag, max(flFittedLinear$"raw.x"), length=200), silent = T)
                 try(lines(time, grow_linear(time, c(y0=unname(coef_["y0_lm"]), max_slope=unname(coef_["max_slope"])))[,"y"], lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
                 try(lines(c(min(flFittedLinear$"raw.x"[1]), lag), rep(flFittedLinear$"raw.fl"[1], 2), lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7)), silent = T)
                 try(lines(time2, grow_linear(time2, c(y0=unname(coef_["y0_lm2"]), max_slope=unname(coef_["max_slope2"])))[,"y"], lty=2, lwd=2, col=ggplot2::alpha("magenta3", 0.7), ...), silent = T)

               }
             } else {
               try(time <- seq(lag, max(flFittedLinear$"filt.x"), length=200), silent = T)
               try(lines(time, grow_linear(time, coef_)[,"y"], lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
               try(lines(c(min(flFittedLinear$"raw.x"[1]), lag), rep(flFittedLinear$"raw.fl"[1], 2), lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7)), silent = T)
             }
           },
           diagnostics = {
             opar <- par(no.readonly = TRUE)
             on.exit(par(opar))
             par(mfrow=c(1,2))

             ## residuals vs. fitted
             obs <- flFittedLinear$log.data
             sim <- grow_linear(flFittedLinear$"raw.x", flFittedLinear$par)
             plot(flFittedLinear$fit[["residuals"]] ~ fitted(flFittedLinear$fit), xlab="fitted", ylab="residuals")
             abline(h=0, col="grey")
             ## normal q-q-plot
             qqnorm(flFittedLinear$fit[["residuals"]])
             qqline(flFittedLinear$fit[["residuals"]])
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

#'
#' @param flFitSpline
#' @param add
#' @param raw
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
#' @export plot.flFitSpline
#' @export
#' @importFrom ggplot2 aes aes_ annotate coord_cartesian element_blank unit element_text geom_bar geom_errorbar geom_line
#'   geom_point geom_ribbon geom_segment ggplot ggplot_build ggplot ggtitle labs
#'   position_dodge scale_color_manual scale_fill_brewer scale_color_brewer scale_fill_manual scale_x_continuous
#'   scale_y_continuous scale_y_log10 theme theme_classic theme_minimal xlab ylab
plot.flFitSpline <- function(flFitSpline, add=FALSE, raw = TRUE, slope=TRUE, deriv = T, spline = T, log.y = F,
                             pch=1, colData=1, colSpline="dodgerblue3", cex=1, lwd = 0.7,
                             plot = TRUE, export = FALSE, width = 8, height = ifelse(deriv == TRUE, 8, 6),
                             out.dir = NULL, ...)
  {
  if(is(flFitSpline) != "flFitSpline") stop("flFitSpline needs to be an object created with flFitSpline().")
  # /// check input parameters
  if (is.logical(add)==FALSE)   stop("Need logical value for: add")
  if (is.logical(slope)==FALSE) stop("Need logical value for: slope")
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (is.numeric(cex)==FALSE)   stop("Need numeric value for: cex")

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
          try(lines(c(min(flFitSpline$"raw.x"[1]), lambda), rep(flFitSpline$"raw.fl"[1], 2), lty=2, lwd=2, col=ggplot2::alpha(colSpline, 0.7)))
        }
      }
      if (deriv  == TRUE){
        if ((flFitSpline$control$log.x.spline==FALSE)){
          try( lines(flFitSpline$spline.deriv1$x, flFitSpline$spline.deriv1$y, xlab="", ylab="", col = colSpline) )
        }
        if ((flFitSpline$control$log.x.spline==TRUE)){
          try( lines(flFitSpline$spline.deriv1$x, flFitSpline$spline.deriv1$y, xlab="", ylab="", col = colSpline) )
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

      df.raw <- data.frame("x" = flFitSpline[["raw.x"]],
                         "data" = flFitSpline[["raw.fl"]])
      df.fit <- data.frame("fit.x" = c(rep(NA, length(flFitSpline[["raw.x"]])-length(flFitSpline[["fit.x"]])), flFitSpline[["fit.x"]]),
                           "fit.fl" = fit.fl)

      x.label = if(flFitSpline$control$x_type == "density"){
        "Density"
      } else {
        "Time"
      }

      p <- ggplot(NULL, aes(x=x, y=data)) +
        geom_line(aes(x=fit.x, y = fit.fl, color = "spline"), data = df.fit, size = lwd) +
        xlab(x.label) +
        ylab(label = "Fluorescence") +
        theme_classic(base_size = 16) +
        ggtitle(gsub(" \\| NA", "", paste(flFitSpline$ID, collapse=" | "))) +
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
      if(raw){
        p <- p + geom_point(data = df.raw, shape=1, size = 2,alpha = 0.6, stroke=0.15)
      }


      p.yrange.end <- ggplot_build(p)$layout$panel_params[[1]]$y.range[2]

      if(log.y == TRUE){
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks(), trans = 'log')
      } else {
        p <- p + scale_y_continuous(breaks = scales::pretty_breaks())
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

            if(!(lag <0)){
              p <- p + geom_segment(aes(x = x[1], y = y[1], xend = x[2], yend = y[2]), data = df.horizontal,
                                    linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
            }
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
          if(!(lag <0)){
            p <- p + geom_segment(aes(x = x[1], y = y[1], xend = x[2], yend = y[2]), data = df.horizontal,
                                  linetype = "dashed", color = ggplot2::alpha(colSpline, 0.85), size = 0.5)
          }
        } # else of if(flFitSpline$fitFlag2)
      } # if(slope == TRUE && log.y == T)

      # /// add panel with growth rate over x
      if(deriv == TRUE){
        df.mu <- data.frame(spline(flFitSpline$spline.deriv1$x, flFitSpline$spline.deriv1$y))
        #add missing x values due to min.density and t0
        df.mu <-
          bind_rows(data.frame(x = df.raw$x[is.na(df.fit$fit.fl)], y = rep(NA, length(df.raw$x[is.na(df.fit$fit.fl)]))),
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


#'
#' @param flBootSpline
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
#' @export plot.flBootSpline
#' @export
#'
plot.flBootSpline <- function(flBootSpline, pch=1, colData=1, deriv = TRUE,
                              colSpline=ggplot2::alpha("dodgerblue3", 0.2),
                              cex=1, plot = TRUE, export = FALSE,
                              height = 7, width = 9, out.dir = NULL, ...)
{
  # flBootSpline an object of class flBootSpline
  if(is(flBootSpline) != "flBootSpline") stop("flBootSpline needs to be an object created with flBootSpline().")
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
  if (flBootSpline$bootFlag==FALSE){
    empty.plot()
  }
  else{
    p1 <- function()
    {
      par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
      colSpline <- rep(colSpline, (flBootSpline$control$nboot.fl%/%length(colSpline))+1)

      log.x     <- flBootSpline$control$log.x.spline
      log.y     <- flBootSpline$control$log.y.spline

      global.minx <- min(min(flBootSpline$boot.x,na.rm=TRUE),na.rm=TRUE)
      global.maxx <- max(max(flBootSpline$boot.x,na.rm=TRUE),na.rm=TRUE)
      global.miny <- min(min(flBootSpline$boot.fl,na.rm=TRUE),na.rm=TRUE)
      global.maxy <- max(max(flBootSpline$boot.fl,na.rm=TRUE),na.rm=TRUE)

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
      points(flBootSpline$raw.x, flBootSpline$raw.fl, col=colData, pch=pch, cex=cex)

      # /// plot all flFitSpline objects
      for(i in 1:flBootSpline$control$nboot.fl){
        plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = T,
                         deriv = FALSE, plot = F, export = F, pch=0, colSpline=colSpline[i], cex=cex)
      }
      # add plot title
      title(paste(flBootSpline$gcID, collapse = "_"), line = ifelse(deriv==T, 0.8, 1))
      #add axis titles
      if (log.y==FALSE){
        title(ylab = "Fluorescence", line = 2.3, cex.lab = 1.2)
      }
      else if (log.y==TRUE){
        title(ylab = "Fluorescence [Ln(y(t)/y0)]", line = 2.3, cex.lab = 1.2)
      }
      # add second plot with slope over time
      if(deriv == TRUE){
        par(mai=c(0.7,0.8,0.2,0))
        y.max <- ceiling(max(unlist(lapply(1:length(flBootSpline$boot.flSpline), function(x) max(flBootSpline$boot.flSpline[[x]]$spline.deriv1$y))))*10)/10
        y.min <- floor(min(unlist(lapply(1:length(flBootSpline$boot.flSpline), function(x) min(flBootSpline$boot.flSpline[[x]]$spline.deriv1$y))))*10)/10
        if ((flBootSpline$control$log.x.spline==FALSE)){
          try( plot(flBootSpline$boot.flSpline[[1]]$spline.deriv1$x, flBootSpline$boot.flSpline[[1]]$spline.deriv1$y,
                    xlab="", ylab="", type = "l", col = colSpline, ylim = c(y.min, y.max) ) )
        }
        if ((flBootSpline$control$log.x.spline==TRUE)){
          try( lines(flBootSpline$boot.flSpline[[1]]$x, flBootSpline$boot.flSpline[[1]]$spline.deriv1$y, xlab="Ln(1+time)", ylab="First derivative", type = "l") )
        }
        for(i in 2:flBootSpline$control$nboot.fl){
          plot.flFitSpline(flBootSpline$boot.flSpline[[i]], add = TRUE, slope = FALSE, spline = F,
                           deriv = T, plot = F, export = F, pch=0, colSpline=colSpline[i], cex=cex)
        }
        title(ylab = "First derivative", line = 2.3, cex.lab = 1.2)
      }
      if(flBootSpline$control$x_type == "density"){
        if (log.x==TRUE){
          title(xlab = "Density [Ln(x(t)/x0)]", line = 2.3, cex.lab = 1.2)
        }
        else if(log.x==FALSE){
          title(xlab = "Density", line = 2.3, cex.lab = 1.2)
        }
      } else {
        if (log.x==TRUE){
          title(xlab = "Ln(1+time)", line = 2.3, cex.lab = 1.2)
        }
        else if(log.x==FALSE){
          title(xlab = "Time", line = 2.3, cex.lab = 1.2)
        }
      }

      par(mfrow=c(1,1))
    } # p1 <- function()
    p2 <- function()
    {
      lambda    <- flBootSpline$lambda
      max_slope <- flBootSpline$max_slope
      A         <- flBootSpline$A
      integral  <- flBootSpline$integral

      # /// plot histograms of growth parameters
      par(mfrow=c(2,2))
      if (sum(!is.na(lambda))>1){
        try(hist(lambda, col="gray",xlab="lambda", main=expression(lambda)))
      }
      else{
        empty.plot("Empty plot!")
      }

      if (sum(!is.na(max_slope))>1){ try(hist(max_slope , col="gray", xlab="max_slope", main=expression(max_slope))) } else { empty.plot("Empty plot!", main=expression(max_slope)) }
      if (sum(!is.na(A))>1){ try(hist(A, col="gray", xlab="A", main=expression(A))) } else { empty.plot("Empty plot!", main=expression(A)) }
      if (sum(!is.na(integral))>1){ try(hist(integral, col="gray", xlab="integral", main=expression(Integral))) } else { empty.plot("Empty plot!", main=expression(Integral))}
      mtext(paste(flBootSpline$gcID, collapse = "_"), side = 3, line = -1, outer = TRUE)
      par(mfrow=c(1,1))
      par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    } # p2 <- function()
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
    p1()
    dev.new()
    p2()
  }
  # restore standard plot parameters
  par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
  par(mfrow = c(1, 1))
}

plot.flFitRes <-  function(object,
                        data.type = c("spline1", "spline2", "raw1", "raw2", "norm.fl1", "norm.fl2"),
                        names = NULL,
                        conc = NULL,
                        mean = TRUE,
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
                        out.dir = NULL
)
{
  if(!any(is(object) %in% c("flFit","flFitRes", "grodata"))) stop("'object' needs to be an object created with fl.workflow(), flFit(), parse_data(), or read_data().")
  if(is(object) == "grodata" && !any(data.type %in% c("raw1", "raw2", "norm.fl1", "norm.fl2"))) stop("Raw input data can only be used to visualize data.type 'raw1', 'raw2', 'norm.fl1', or 'norm.fl2'.")

  data.type <- match.arg(data.type)
  if(data.type == "raw1" || data.type == "raw2" || data.type == "norm.fl1" || data.type == "norm.fl2" && deriv ==TRUE){
    warning("Derivatives cannot be calculated for 'raw' or 'norm.fl' data. Only the fluorescence values will be shown.")
    deriv = FALSE
  }
  if(is(object) == "flFitRes") object <- object$flFit

  # /// check input parameters
  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(lwd)==FALSE)   stop("Need numeric value for: lwd")
  if(data.type == "spline1" || data.type == "spline2"){
    if (!("s" %in% object$control$fit.opt | "a" %in% object$control$fit.opt)) stop("To plot spline fit results, please run fl.workflow() with 's' in fit.opt.")
  }


  # Get name of conditions with multiple replicates
  if(any(is(object) %in% c("flFit","flFitRes"))){
    sample.nm <- nm <- as.character(names(object$flFittedSplines))
  } else {
    if(data.type == "norm.fl1"){
      sample.nm <- nm <- paste(object$norm.fluorescence1[,1], object$norm.fluorescence1[,2], object$norm.fluorescence1[,3], sep = " | ")
      data.nm = "norm.fluorescence1"
    }
    if(data.type == "norm.fl2"){
      sample.nm <- nm <- paste(object$norm.fluorescence2[,1], object$norm.fluorescence2[,2], object$norm.fluorescence2[,3], sep = " | ")
      data.nm = "norm.fluorescence2"
    }
    if(data.type == "raw1"){
      sample.nm <- nm <- paste(object$fluorescence1[,1], object$fluorescence1[,2], object$fluorescence1[,3], sep = " | ")
      data.nm = "fluorescence1"
    }
    if(data.type == "raw2"){
      sample.nm <- nm <- paste(object$fluorescence2[,1], object$fluorescence2[,2], object$fluorescence2[,3], sep = " | ")
      data.nm = "fluorescence2"
    }
  }
  if(!is.null(names)){
    names <- gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", names))
    nm <- nm[grep(paste(names, collapse="|"), nm)]
  }
  if(!is.null(conc)){
    nm <- nm[which(str_extract(nm, "(?<![:punct:])[:alnum:]+$") %in% conc)]
  }
  if(length(nm)==0){
    stop("Please run plot.grofit() with valid 'names' or 'conc' argument.")
  }
  # remove conditions with fitFlag = FALSE in all replicates
  # Store each condition with its replicate indices in list filter.ls
  ndx.filt.rep <- unique(lapply(1:length(sample.nm), function(i)which(gsub(" \\| .+", "", sample.nm) %in% (paste0(unlist(str_split(sample.nm[i], " \\| "))[1])))))
  filter.ls <- list()
  for(j in 1:length(ndx.filt.rep)){
    filter.ls[[j]] <- unique(lapply(1:length(ndx.filt.rep[[j]]), function(i) ndx.filt.rep[[j]][grep(paste0("^",
                                                                                                           gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[1])),
                                                                                                           ".+[[:space:]]",
                                                                                                           unlist(str_split(sample.nm[ndx.filt.rep[[j]][i]], " \\| "))[3],
                                                                                                           "$"), sample.nm[ndx.filt.rep[[j]]])]))
  }
  ndx.filt <- unlist(filter.ls, recursive = F)
  # Check FitFlag for each replicate, work per condition
  if(data.type == "spline1" || data.type == "spline2"){
    for(i in 1:length(ndx.filt)){
      if(!all(unlist(lapply(1:length(ndx.filt[[i]]), function(j) (object[["flFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]]))))){
        fitflags <- unlist(lapply(1:length(ndx.filt[[i]]), function(j) (object[["flFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]])))
        nm <- nm[!(nm %in% sample.nm[(ndx.filt[[i]][!fitflags])])]
      }
    }
  }

  # get indices of samples with selected names
  ndx.keep <- grep(paste0(
    str_replace_all(nm, "\\|", "\\\\|"), collapse = "|"), sample.nm)

  if(data.type == "spline1"  || data.type == "spline2"){
    # correct for log transformation
    if(object$control$log.y.spline == TRUE){
      for(i in 1:length(ndx.keep)){
        object$flFittedSplines[[ndx.keep[i]]][["fit.fl"]] <-
          exp(object$flFittedSplines[[ndx.keep[i]]][["fit.fl"]]) * object$flFittedSplines[[ndx.keep[i]]]$data.in[1]
      }
    }
  }
if(data.type == "spline1" || data.type == "spline2" && object$control$x_type == "density" && mean == TRUE){
  message("Grouped of replicates is not supported for spline fits with x_type = 'density'. Argument changed to mean = FALSE.")
  mean <- FALSE
}
  if(mean == TRUE){
    # Combine replicates via their mean and standard deviation
    conditions <- str_replace_all(nm, "\\| . \\| ", "| ")
    conditions_unique <- unique(conditions)

    # Create lists for each selected condition, with density values and derivatives, respectively. Each list item represents one condition with their average and SD
    plotdata.ls <- list()
    deriv.ls <- list()
    for(n in 1:length(conditions_unique)){
      # find indexes of replicates
      ndx <- intersect(ndx.keep, grep(paste0("^",
                                             gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", unlist(str_split(conditions_unique[n], " \\| "))[1])),
                                             ".+[[:space:]]",
                                             unlist(str_split(conditions_unique[n], " \\| "))[2],
                                             "$"), sample.nm))
      name <- conditions_unique[n]
      # Create lists for density and time values for each sample
      if(data.type == "spline1"  || data.type == "spline2"){
        time <- lapply(1:length(ndx), function(i) cbind(object$flFittedSplines[[ndx[[i]]]]$fit.x)) %>% as.list(.)
        data <- lapply(1:length(ndx), function(i) cbind(object$flFittedSplines[[ndx[[i]]]]$fit.fl)) %>% as.list(.)
      } else {
        time <- lapply(1:length(ndx), function(i) cbind(object$time[ndx[[i]], ])) %>% as.list(.)
        data <- object[[data.nm]][ndx, 4:ncol(object[[data.nm]])]
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
    if(!is.null(x.lim)) df <- df[df[["time"]]>x.lim[1], ]

    # replace negative lower ribbon boundaries with 0 for log10 transformation
    if(log.y==TRUE){
      df$lower[df$lower<0] <- 0
    }
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
    } else if(data.type == "raw1" || data.type == "spline1"){
      "Fluorescence 1"
    } else if(data.type == "raw2" || data.type == "spline2"){
      "Fluorescence 2"
    }
    p <- ggplot(df, aes(x=time, y=mean, col = name)) +
      geom_line(size=lwd) +
      geom_ribbon(aes(ymin=lower,ymax=upper, fill=name), alpha = 0.3, colour = NA) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title), ylab.title, y.title)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

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
  } # if(mean == TRUE)
  else {
    df <- data.frame()
    for(i in 1:length(ndx.keep)){
      if(data.type == "spline1"  || data.type == "spline2"){
        df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                              "time" = object$flFittedSplines[[ndx.keep[i]]][["fit.x"]],
                                              "y" = object$flFittedSplines[[ndx.keep[i]]][["fit.fl"]]))
      } else {
        df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx.keep[i]],
                                              "time" = as.vector(object$time[ndx.keep[i], ]),
                                              "y" = unlist(unname(type.convert(object[[data.nm]][ndx.keep[i], 4:ncol(object[[data.nm]])], as.is=T)))))
      }

    }
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
    } else if(data.type == "raw1" || data.type == "spline1"){
      "Fluorescence 1"
    } else if(data.type == "raw2" || data.type == "spline2"){
      "Fluorescence 2"
    }
    p <- ggplot(df, aes(x=time, y=y, col = name)) +
      geom_line(size=lwd) +
      theme_classic(base_size = basesize) +
      xlab(ifelse(is.null(x.title), xlab.title, x.title)) +
      ylab(ifelse(is.null(y.title), ylab.title, y.title)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

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
      for(i in 1:length(ndx.keep)){
        df.deriv <- plyr::rbind.fill(df.deriv, data.frame("name" = sample.nm[ndx.keep[i]],
                                                          "time" = object$flFittedSplines[[ndx.keep[[i]]]]$spline.deriv1$x,
                                                          "y" = object$flFittedSplines[[ndx.keep[[i]]]]$spline.deriv1$y))
      }
      p.deriv <- ggplot(df.deriv, aes(x=time, y=y, col = name)) +
        geom_line(size=lwd) +
        theme_classic(base_size = basesize) +
        xlab(ifelse(is.null(x.title), "Time", x.title)) +
        theme(panel.grid.major = element_blank(),
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

plot.flFit <- plot.flFitRes
