plot.gcFitLinear <- function(x, y, log="", which=c("fit", "diagnostics"), ...)
            {
            which <- match.arg(which)

            switch(which,
                   fit = {

                     plot(x$"raw.data" ~ x$"raw.time", xlab="Time", ylab=ifelse(log == "y", "Density", "Density"),
                          log=log, las=1, main = "Linear fit", ...)
                     try(points(x$raw.data[x$ndx] ~ x$raw.time[x$ndx], pch=21, col="black", bg="red"))

                     ## lag phase
                     lag <- x$par["lag"]

                     try(time <- seq(lag, max(x$"raw.time"), length=200), silent = T)
                     coef_ <- x$par
                     try(lines(time, x$FUN(time, c(y0=unname(coef_["y0_lm"]), mumax=unname(coef_["mumax"])))[,"y"], lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7), ...), silent = T)
                     try(lines(c(min(x$"raw.time"[1]), lag), rep(x$"raw.data"[1], 2), lty=2, lwd=2, col=ggplot2::alpha("firebrick3", 0.7)), silent = T)
                   },
                   diagnostics = {
                     opar <- par(no.readonly = TRUE)
                     on.exit(par(opar))
                     par(mfrow=c(1,2))

                     ## residuals vs. fitted
                     obs <- obs(x)
                     sim <- x$FUN(x$"raw.time", x$par)
                     plot(fit.linear[["fit"]][["residuals"]] ~ fitted(fit.linear[["fit"]]), xlab="fitted", ylab="residuals")
                     abline(h=0, col="grey")
                     ## normal q-q-plot
                     qqnorm(fit.linear[["fit"]][["residuals"]])
                     qqline(fit.linear[["fit"]][["residuals"]])
                   }
            )
          }

plot.gcFitModel <- function(x, add=FALSE, raw=TRUE, slope=TRUE, pch=1, colData=1, colModel=1, colLag = 1, cex=1, ...)
  {
    # x an object of class gcFitModel

    # /// check input parameters
    if (is.logical(add)==FALSE)   stop("Need logical value for: add")
    if (is.logical(raw)==FALSE)   stop("Need logical value for: raw")
    if (is.logical(slope)==FALSE) stop("Need logical value for: slope")
    if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
    if (is.numeric(cex)==FALSE)   stop("Need numeric value for: cex")


    # /// check if a data fit is available
    if ((is.na(x$fitFlag)==TRUE)|(x$fitFlag==FALSE)){
      warning("plot.gcFitModel: no data fit available!")
    }
    else{
      if (raw==TRUE){
        if (add==TRUE){
          if ((x$control$log.x.gc==FALSE) && (x$control$log.y.model==FALSE)){
            try( points(x$raw.time, x$raw.data, sub=x$name.fit, col=colData, pch=pch,cex=cex) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colModel, type="l") )
          }
          if ((x$control$log.x.gc==FALSE) && (x$control$log.y.model==TRUE)){
            try( points(x$raw.time, x$raw.data, sub=x$name.fit, col=colData, pch=pch, cex=cex) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colModel, type="l") )
          }
          if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.model==FALSE)){
            try( points(x$raw.time, x$raw.data, sub=x$name.fit, col=colData, pch=pch, cex=cex) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colModel, type="l" ) )
          }
          if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.model==TRUE)){
            try( points(x$raw.time, x$raw.data, sub=x$name.fit, col=colData, pch=pch, cex=cex) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colModel, type="l") )
          }
        }
        else{ # of if (add==TRUE){
          par(mar=c(5.1, 4.1, 4.1, 5.1))
          if ((x$control$log.x.gc==FALSE) && (x$control$log.y.model==FALSE)){
            try( plot(x$raw.time, x$raw.data, sub=x$name.fit, xlab="Time", ylab="Growth y(t)", col=colData, pch=pch, cex=cex) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l") )
          }
          if ((x$control$log.x.gc==FALSE) && (x$control$log.y.model==TRUE)){
            try( plot(x$raw.time, x$raw.data, sub=x$name.fit, xlab="Time", ylab="Growth [Ln(y(t)/y0)]", col=colData, pch=pch, cex=cex) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l") )
          }
          if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.model==FALSE)){
            try( plot(x$raw.time, x$raw.data, sub=x$name.fit, xlab="Ln(1+time)", ylab="Growth y(t)", col=colData, pch=pch, cex=cex) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l" ) )
          }
          if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.model==TRUE)){
            try( plot(x$raw.time, x$raw.data, sub=x$name.fit, xlab="Ln(1+time)", ylab="Growth [Ln(y(t)/y0)]", col=colData, pch=pch, cex=cex) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l") )
          }
        }
      }
      else{ # of if (raw==TRUE)
        if (add==TRUE){
          # /// try to plot data fit
          if ((x$control$log.x.gc==FALSE) && (x$control$log.y.model==FALSE)){
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l") )
          }

          if ((x$control$log.x.gc==FALSE) && (x$control$log.y.model==TRUE)){
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l") )
          }

          if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.model==FALSE)){
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l") )
          }

          if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.model==TRUE)){
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l") )
          }
        }
        else{ # of if (add==TRUE)
          par(mar=c(5.1, 4.1, 4.1, 5.1))
          # /// try to plot data fit
          if ((x$control$log.x.gc==FALSE) && (x$control$log.y.model==FALSE)){
            try( plot(x$fit.time, x$fit.data, sub=x$name.fit, xlab="Time", ylab="Growth y(t)", col=0) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colModel, type="l") )
          }

          if ((x$control$log.x.gc==FALSE) && (x$control$log.y.model==TRUE)){
            try( plot(x$fit.time, x$fit.data, sub=x$name.fit, xlab="Time", ylab="Growth [Ln(y(t)/y0)]", col=0) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l") )
          }

          if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.model==FALSE)){
            try( plot(x$fit.time, x$fit.data, sub=x$name.fit, xlab="Ln(1+time)", ylab="Growth y(t)", col=0 ) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l" ) )
          }

          if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.model==TRUE)){
            try( plot(x$fit.time, x$fit.data, sub=x$name.fit, xlab="Ln(1+time)", ylab="Growth [Ln(y(t)/y0)]", col=0) )
            try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colModel, type="l") )
          }
        }
      }
        mu     <- as.numeric(x$parameters$mu[1])
        lambda <- as.numeric(x$parameters$lambda[1])

      # if(x$model == "logistic"){
      #   text(x=44.45, par("usr")[4],
      #        expression(y(t) == frac(A , 1+exp(frac(4 %.% mu, A) %.% (lambda - t) + 2))),
      #        srt = 90, xpd = TRUE, pos = 1, offset=20, cex=1.4)
      #
      #   text(x=48.3, par("usr")[4],
      #        bquote(A == .(round(x$parameters$A[1],3)) ~~~~ mu == .(round(x$parameters$mu[1],3)) ~~~~
      #                 lambda == .(round(x$parameters$lambda[1],3)) ),
      #        srt = 90, xpd = TRUE, pos = 1, offset=20, cex=1.2)
      #
      # }
      # if(x$model == "richards"){
      #   text(x=44.5, par("usr")[4],
      #        expression(y(t) == A%.%(1.0+nu%.%italic(e)^{1+nu}%.%exp(frac(mu,A)%.%(1+nu)^(1+frac(1,nu))%.%( lambda - t )))^(-1/nu)),
      #        srt = 90, xpd = TRUE, pos = 1, offset=20, cex=1.4)
      #
      #   text(x=47.5, par("usr")[4],
      #        bquote(A == .(round(x$parameters$A[1],3)) ~~~~ mu == .(round(x$parameters$mu[1],3)) ~~~~
      #                 lambda == .(round(x$parameters$lambda[1],3)) ~~~~ nu == .(round(as.numeric(x$parameters$fitpar$nu[1],3))) ),
      #        srt = 90, xpd = TRUE, pos = 1, offset=20, cex=1.2)
      # }
      # if(x$model == "gompertz"){
      #   text(x=44.5, par("usr")[4],
      #        expression(y(t) == A%.%exp(-exp(frac(mu%.%italic(e),A)%.%(lambda-t) +1))),
      #        srt = 90, xpd = TRUE, pos = 1, offset=20, cex=1.4)
      #
      #   text(x=48.3, par("usr")[4],
      #        bquote(A == .(round(x$parameters$A[1],3)) ~~~~ mu == .(round(x$parameters$mu[1],3)) ~~~~
      #                 lambda == .(round(x$parameters$lambda[1],3)) ),
      #        srt = 90, xpd = TRUE, pos = 1, offset=20, cex=1.2)
      # }
      # if(x$model == "gompertz.exp"){
      #   lambda <- lambda - x$parameters$A[1]*exp(x$parameters$fitpar$alpha[1]*(x$parameters$lambda[1]-x$parameters$fitpar$t_shift[1]))
      #   text(x=44.5, par("usr")[4],
      #        expression(y(t) == A%.%exp(-exp(frac(mu%.%italic(e),A)%.%(lambda-t) +1)) + A%.%exp(alpha%.%(t-t[shift]))),
      #        srt = 90, xpd = TRUE, pos = 1, offset=20, cex=1.4)
      #
      #   text(x=47.8, par("usr")[4],
      #        bquote(A == .(round(x$parameters$A[1],3)) ~~~~ mu == .(round(x$parameters$mu[1],3)) ~~~~
      #                 lambda == .(round(x$parameters$lambda[1],2)) ~~~~ alpha == .(round(x$parameters$fitpar$alpha[1],3))  ~~~~
      #                 t[shift] == .(round(x$parameters$fitpar$t_shift[1],2)) ),
      #        srt = 90, xpd = TRUE, pos = 1, offset=20, cex=1.2)
      # }
        # /// add tangent at maximum slope
        if (slope==TRUE){
          bla    <- (x$fit.time)*mu
          bla    <- bla+(-mu*lambda)
          try(lines(x$fit.time, bla, lw=2, lty=2, col=colModel))
        }

    }
  }

plot.drBootSpline <- function (x,
                               pch = 1,
                               colData = 1,
                               colSpline = scales::alpha("black", 0.15),
                               cex = 0.5,
                               ...)
{
  # x an object of class drBootSpline
  if(class(x) != "drBootSpline") stop("x needs to be an object created with growth.drBootSpline.")
  # /// initialize "Empty Plot" function
  empty.plot  <- function(text = "Empty plot", main = "") {
    plot(
      c(0, 1, 0, 1, 0),
      c(0, 1, 1, 0, 0),
      type = "l",
      axes = FALSE,
      xlab = "",
      ylab = "",
      lwd = 1,
      col = "gray",
      main = main
    )
    lines(c(0, 0),
          c(0, 1),
          type = "l",
          lwd = 1,
          col = "gray")
    lines(c(1, 1),
          c(1, 0),
          type = "l",
          lwd = 1,
          col = "gray")
    text(0.5, 0.1, text, col = "gray")
  }

  # /// check input parameters
  if (FALSE %in% (colData %in% c(colors(), 0:8)))
    stop("colData needs to be numeric from 0:8 or a string from colors()")
  if (is.numeric(pch) == FALSE)
    stop("Need numeric value for: pch")
  if (is.numeric(cex) == FALSE)
    stop("Need numeric value for: cex")

  if (x$bootFlag == FALSE) {
    empty.plot()
  }
  else{
    colSpline   <-
      rep(colSpline, (x$control$nboot.dr %/% length(colSpline)) + 1)
    conc.log    <- log(x$raw.conc + 1)
    test.log    <- log(x$raw.test + 1)
    conc        <- x$raw.conc
    test        <- x$raw.test

    global.minx <- min(min(x$boot.conc))
    global.maxx <- max(max(x$boot.conc))
    global.miny <- min(min(x$boot.test))
    global.maxy <- max(max(x$boot.test))

    plot.new()
    # initialize plot
    if ((x$control$log.x.dr == TRUE) &&
        (x$control$log.y.dr == FALSE)) {
      plot(
        c(global.minx, global.maxx),
        c(global.miny, global.maxy),
        type = "n",
        xlab = "ln(1+concentration)",
        ylab = "response"
      )
    }
    else{
      if ((x$control$log.x.dr == FALSE) &&
          (x$control$log.y.dr == FALSE)) {
        plot(
          c(global.minx, global.maxx),
          c(global.miny, global.maxy),
          type = "n",
          xlab = "concentration",
          ylab = "response"
        )
      }
      else{
        if ((x$control$log.x.dr == TRUE) && (x$control$log.y.dr == TRUE)) {
          plot(
            c(global.minx, global.maxx),
            c(global.miny, global.maxy),
            type = "n",
            xlab = "ln(1+concentration)",
            ylab = "ln(1+response)"
          )
        }
        else{
          if ((x$control$log.x.dr == FALSE) && (x$control$log.y.dr == TRUE)) {
            plot(
              c(global.minx, global.maxx),
              c(global.miny, global.maxy),
              type = "n",
              xlab = "concentration",
              ylab = "ln(1+response)"
            )
          }
        }
      }
    }

    # /// plot raw data
    points(
      x$raw.conc,
      x$raw.test,
      col = colData,
      pch = pch,
      cex = cex
    )

    # /// loop over all fitted splines and plot drFitSpline objects
    for (i in 1:x$control$nboot.dr) {
      plot(
        x$boot.drSpline[[i]],
        add = TRUE,
        ec50line = FALSE,
        pch = 0,
        colSpline = colSpline[i],
        colData = 0,
        cex = cex,
        lwd = 1
      )
    }

    plot.new()
    if (sum(!is.na(x$ec50.boot)) == length(x$ec50.boot)) {
      hist(
        x$ec50.boot,
        col = "gray",
        main = as.character(x$gcID),
        xlab = "EC50"
      )
    }
    else{
      empty.plot()
    }

  } # /// of if (x$bootFlag==FALSE){

}

plot.drFit <- function(x, ...)
{
  # x an object of class drFit

  n <- length(x$drFittedSplines)

  # /// plot all drFitSpline objects
  for (i in 1:n) {
    #x11()
    try(plot(x$drFittedSplines[[i]]))
    title(as.character(x$drFittedSplines[[i]]$drID))
  }

}

plot.drFitSpline <-
  function (x,
            add = FALSE,
            ec50line = TRUE,
            pch = 1,
            colSpline = 1,
            colData = 1,
            cex = 1,
            lwd = 2,
            ...)
  {
    # x an object of class drFitSpline
    if(class(x) != "drFitSpline") stop("x needs to be an object created with growth.drFitSpline.")
    # /// check input parameters
    if (FALSE %in% (colData %in% c(colors(), 0:8)))
      stop("colData needs to be numeric from 0:8 or a string from colors()")
    if (is.logical(add) == FALSE)
      stop("Need logical value for: add")
    if (is.logical(ec50line) == FALSE)
      stop("Need logical value for: ec50line")
    if (is.numeric(pch) == FALSE)
      stop("Need numeric value for: pch")
    if (is.numeric(cex) == FALSE)
      stop("Need numeric value for: cex")

    if (add == FALSE) {
      if ((x$control$log.x.dr == TRUE) && (x$control$log.y.dr == TRUE)) {
        plot(
          log(x$raw.conc + 1),
          log(x$raw.test + 1),
          pch = pch,
          cex = cex,
          col = colData,
          xlab = "ln(1+concentration)",
          ylab = "ln(1+response)"
        )
      }
      else
      {
        if ((x$control$log.x.dr == FALSE) && (x$control$log.y.dr == TRUE)) {
          plot(
            x$raw.conc,
            log(x$raw.test + 1),
            pch = pch,
            cex = cex,
            col = colData,
            xlab = "concentration",
            ylab = "ln(1+response)"
          )
        }
        else
        {
          if ((x$control$log.x.dr == TRUE) && (x$control$log.y.dr == FALSE)) {
            plot(
              log(x$raw.conc + 1),
              x$raw.test,
              pch = pch,
              cex = cex,
              col = colData,
              xlab = "Ln(1+concentration)",
              ylab = paste0("Response", ifelse(!is.na(x$parameters$test), paste0(" (", x$parameters$test, ")"), ""))
            )
          }
          else
          {
            if ((x$control$log.x.dr == FALSE) && (x$control$log.y.dr == FALSE)) {
              plot(
                x$raw.conc,
                x$raw.test,
                pch = pch,
                cex = cex,
                col = colData,
                xlab = "Concentration",
                ylab = paste0("Response", ifelse(!is.na(x$parameters$test), paste0(" (", x$parameters$test, ")"), ""))
              )
            }
          }
        }
      }
    }
    else{
      if ((x$control$log.x.dr == TRUE) && (x$control$log.y.dr == TRUE)) {
        points(
          log(x$raw.conc + 1),
          log(x$raw.test + 1),
          pch = pch,
          cex = cex,
          col = colData
        )
      }
      else
      {
        if ((x$control$log.x.dr == FALSE) && (x$control$log.y.dr == TRUE)) {
          points(
            x$raw.conc,
            log(x$raw.test + 1),
            pch = pch,
            cex = cex,
            col = colData
          )
        }
        else
        {
          if ((x$control$log.x.dr == TRUE) && (x$control$log.y.dr == FALSE)) {
            points(
              log(x$raw.conc + 1),
              x$raw.test,
              pch = pch,
              cex = cex,
              col = colData
            )
          }
          else
          {
            if ((x$control$log.x.dr == FALSE) && (x$control$log.y.dr == FALSE)) {
              points(
                x$raw.conc,
                x$raw.test,
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
      x$fit.conc,
      x$fit.test,
      type = "l",
      lwd = lwd,
      col = colSpline
    ))

    if (ec50line == TRUE) {
      #vertical lines
      totmin = min(min(x$fit.conc), min(x$fit.test))
      lines(c(x$parameters$EC50, x$parameters$EC50),
            c(totmin - 1, x$parameters$yEC50),
            lty = 2)
      #horizontal
      lines(c(-1, x$parameters$EC50),
            c(x$parameters$yEC50, x$parameters$yEC50),
            lty = 2)
    }
    title(x$drID)
  }

plot.gcBootSpline <- function(x, pch=1, colData=1, colSpline=ggplot2::alpha("black", 0.1), cex=1, ...)
{
  # x an object of class gcBootSpline
  if(class(x) != "gcBootSpline") stop("x needs to be an object created with growth.gcBootSpline.")
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


  if (x$bootFlag==FALSE){
    empty.plot()
  }
  else{
    colSpline <- rep(colSpline, (x$control$nboot.gc%/%length(colSpline))+1)

    lambda    <- x$lambda
    mu        <- x$mu
    A         <- x$A
    integral  <- x$integral

    log.x     <- x$control$log.x.gc
    log.y     <- x$control$log.y.gc

    global.minx <- min(min(x$boot.time,na.rm=TRUE),na.rm=TRUE)
    global.maxx <- max(max(x$boot.time,na.rm=TRUE),na.rm=TRUE)
    global.miny <- min(min(x$boot.data,na.rm=TRUE),na.rm=TRUE)
    global.maxy <- max(max(x$boot.data,na.rm=TRUE),na.rm=TRUE)

    # initialize plot
    if ((log.x==TRUE)&&(log.y==FALSE)){
      plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="ln(1+time)",ylab="Growth y(t)")
    }
    else{
      if ((log.x==FALSE)&&(log.y==FALSE)){
        plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="Time",ylab="Growth y(t)")
      }
      else{
        if ((log.x==TRUE)&&(log.y==TRUE)){
          plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="ln(1+time)",ylab="Growth [Ln(y(t)/y0)]")
        }
        else{
          if ((log.x==FALSE)&&(log.y==TRUE)){
            plot(c(global.minx, global.maxx), c(global.miny, global.maxy), pch="",xlab="Time",ylab="Growth [Ln(y(t)/y0)]")
          }
        }
      }
    }

    # /// plot data
    points(x$raw.time, x$raw.data, col=colData, pch=pch, cex=cex)

    # /// plot all gcFitSpline objects
    for(i in 1:x$control$nboot.gc){
      plot(x$boot.gcSpline[[i]],add=TRUE, raw=FALSE, slope=FALSE, pch=0, colSpline=colSpline[i], cex=cex)
    }

    # /// plot histograms of growth parameters
    dev.new()
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
  }
  par(mfrow=c(1,1))
}

### revise!!!
plot.gcFit <- function(x, opt="m",raw=TRUE, slope=FALSE, pch=1, colModel=1, colSpline=2, colData=1, cex=1, ...)
{
  # x an object of class gcFit

  # /// check input parameters
  if ( FALSE%in%(colData%in%c(colors(),0:8))) stop("colData needs to be numeric from 0:8 or a string from colors()")
  if ( FALSE%in%(colSpline%in%c(colors(),0:8))) stop("colSpline needs to be numeric from 0:8 or a string from colors()")
  if (is.logical(raw)==FALSE)       stop("Need logical value for: raw")
  if (is.logical(slope)==FALSE)     stop("Need logical value for: slope")
  if (is.numeric(pch)==FALSE)       stop("Need numeric value for: pch")
  if (is.numeric(cex)==FALSE)       stop("Need numeric value for: cex")
  if (!(opt%in%c("m","s")))     stop("Need 'm' or 's' for: opt")

  # /// recycle plot options
  n         <- dim(x$gcTable)[1]
  pch       <- rep(pch,       (n%/%length(pch))       +1)
  colModel  <- rep(colModel,  (n%/%length(colModel))  +1)
  colSpline <- rep(colSpline, (n%/%length(colSpline)) +1)
  colData   <- rep(colData,   (n%/%length(colData))   +1)

  # /// determine number of different tests
  distinct <- summary(x$gcTable[,1])
  k        <- length(distinct)

  # /// loop over all different tests
  for (j in 1:k){

    # /// initialize plot
    if(j>1){dev.new()}

    ind <- which(x$raw.data[,1]==names(distinct)[j])
    tspan <- x$raw.data

    data <- as.matrix((x$raw.data[ind,])[-1:-3])
    time <- as.matrix((x$raw.time[ind,]))

    if (x$control$log.x.gc==TRUE) time <- log(time + 1)
    if (x$control$log.y.gc==TRUE) data <- log(data + 1)

    tspan <- c(min(time, na.rm=TRUE), max(time, na.rm=TRUE))
    yspan <- c(min(data, na.rm=TRUE), max(data, na.rm=TRUE))

    scale  <- 1.025
    ts     <- ((scale-1)* diff(tspan))/2
    ys     <- ((scale-1)* diff(yspan))/2

    tspan0 <- tspan+c(-ts,ts)
    yspan0 <- yspan+c(-ys,ys)

    if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==FALSE)){
      plot(tspan0, yspan0, xlab="Time", ylab="Growth y(t)", type="n")
    }
    if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==TRUE)){
      plot(tspan0, yspan0, xlab="Time", ylab="Growth [Ln(y(t)/y0)]", type="n")
    }
    if ((x$control$log.x.gc==TRUE) && (x$control$log.y.gc==FALSE)){
      plot(tspan0, yspan0, xlab="Ln(1+time)", ylab="Growth y(t)", type="n")
    }
    if ((x$control$log.x.gc==TRUE) && (x$control$log.y.gc==TRUE)){
      plot(tspan0, yspan0, xlab="Ln(1+time)", ylab="Growth [Ln(y(t)/y0)]", type="n")
    }

    counter <- 0
    id      <- 0
    leg     <- rep("",distinct[j])

    # plot parametric fit
    if (opt=="m"){
      for (i in 1:n){
        if ((x$gcFittedModels[[i]]$gcID[1]==names(distinct)[j])&&( (x$gcFittedModels[[i]]$reliable==TRUE)||(is.null(x$gcFittedModels[[i]]$reliable)==TRUE) ) ){
          counter     <- counter + 1
          id[counter] <- i
          for (m in 1:length(x$gcFittedModels[[i]]$gcID)){
            leg[counter]=paste(leg[counter], as.character((x$gcFittedModels[[i]]$gcID[m])))
          }
          plot(x$gcFittedModels[[i]], add=TRUE, raw=raw, slope=slope, pch=pch[i], colData=colData[i], colModel=colModel[i], cex=cex)
        }
      }
      legend(x="topleft", pch=pch[id], leg[1:counter], col=colData[id], cex=cex, bty="n")
    }


    # plot spline fit
    if (opt=="s"){
      for (i in 1:n){
        if ((x$gcFittedSplines[[i]]$gcID[1]==names(distinct)[j])&&( (x$gcFittedSplines[[i]]$reliable==TRUE)||(is.null(x$gcFittedSplines[[i]]$reliable)==TRUE) ) ){
          counter     <- counter + 1
          id[counter] <- i
          for (m in 1:length(x$gcFittedSplines[[i]]$gcID)){
            leg[counter]=paste(leg[counter], as.character((x$gcFittedSplines[[i]]$gcID[m])))
          }
          plot(x$gcFittedSplines[[i]], add=TRUE, raw=raw, slope=slope, pch=pch[i], colData=colData[i], colSpline=colSpline[i], cex=cex)
        }
      }
      legend(x="topleft", pch=pch[id], leg[1:counter], col=colData[id], cex=cex, bty="n")
    }

    title(sub=names(distinct)[j])
  } # of for (j in 1:k){
}

plot.gcFitSpline <- function(x, add=FALSE, raw=TRUE, slope=TRUE, deriv = T, pch=1, colData=1, colSpline="firebrick3", cex=1, lwd = 2, ...)
 {

  # x an object of class gcFitSpline
  if(class(x) != "gcFitSpline") stop("x needs to be an object created with growth.gcFitSpline.")
  # /// check input parameters
  if (is.logical(add)==FALSE)   stop("Need logical value for: add")
  if (is.logical(raw)==FALSE)   stop("Need logical value for: raw")
  if (is.logical(slope)==FALSE) stop("Need logical value for: slope")
  if (is.numeric(pch)==FALSE)   stop("Need numeric value for: pch")
  if (is.numeric(cex)==FALSE)   stop("Need numeric value for: cex")

  # /// check if a data fit is available
  if ((is.na(x$fitFlag)==TRUE)|(x$fitFlag==FALSE)){
    warning("plot.gcFitModel: no data fit available!")
  }
  else{
    if (raw==TRUE){
      if (add==TRUE){
        # /// try to plot raw data and data fit
        if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==FALSE)){
          try( points(x$raw.time, x$raw.data, sub=x$name.fit, col=colData, pch=pch, cex=cex) )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colSpline, type="l", lwd=lwd) )
        }
        if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==TRUE)){
          try( points(x$raw.time, x$raw.data, sub=x$name.fit, col=colData, pch=pch, cex=cex) )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colSpline, type="l", lwd=lwd) )
        }
        if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.gc==FALSE)){
          try( points(x$raw.time, x$raw.data, sub=x$name.fit, col=colData, pch=pch, cex=cex) )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colSpline, type="l", lwd=lwd ) )
        }
        if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.gc==TRUE)){
          try( points(x$raw.time, x$raw.data, sub=x$name.fit, col=colData, pch=pch, cex=cex)  )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colSpline, type="l", lwd=lwd) )
        }
      }
      else{ # of if (add==TRUE)
        if(deriv == TRUE){
          layout(mat = matrix(c(1, 2), nrow = 2, ncol = 1),
                 heights = c(2, 1.3), # Heights of the two rows
                 widths = c(1, 1)) # Widths of the two columns
        }
        par(mar=c(2.1, 3.1, 3.1, 2.1), mgp=c(2, 1, 0))
        # /// try to plot raw data and data fit
        if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==FALSE)){
          try( plot(x$raw.time, x$raw.data, sub=x$name.fit, xlab="Time", ylab="Growth y(t)", col=colData, pch=pch, cex=cex) )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd) )
        }
        if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==TRUE)){
          try( plot(x$raw.time, x$raw.data, sub=x$name.fit, xlab="Time", ylab="Growth [Ln(y(t)/y0)]", col=colData, pch=pch, cex=cex) )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd) )
        }
        if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.gc==FALSE)){
          try( plot(x$raw.time, x$raw.data, sub=x$name.fit, xlab="Ln(1+time)", ylab="Growth y(t)", col=colData, pch=pch, cex=cex) )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd ) )
        }
        if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.gc==TRUE)){
          try( plot(x$raw.time, x$raw.data, sub=x$name.fit, xlab="Ln(1+time)", ylab="Growth [Ln(y(t)/y0)]", col=colData, pch=pch, cex=cex) )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd) )
        }
        title("Nonparametric fit")
        legend(x="bottomright", legend="spline fit", col=colSpline, lty=1)
        mtext(line = 0.1 ,side=3, outer = F, cex=0.9, paste(x[["gcID"]], collapse = " | "))
      }
    }
    else{ # of (raw==TRUE)
      if (add==TRUE){
        # /// try to plot data fit
        if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==FALSE)){
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd) )
        }

        if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==TRUE)){
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd) )
        }

        if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.gc==FALSE)){
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd ) )
        }

        if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.gc==TRUE)){
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd) )
        }
      }
      else{ # of if (add==TRUE)
        if(deriv == TRUE){
          layout(mat = matrix(c(1, 2), nrow = 2, ncol = 1),
                        heights = c(2, 1.3), # Heights of the two rows
                        widths = c(1, 1)) # Widths of the two columns
        }
        par(mar=c(2.1, 3.1, 3.1, 2.1), mgp=c(2, 1, 0))
        # /// try to plot data fit
        if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==FALSE)){
          try( plot(x$fit.time, x$fit.data, sub=x$name.fit, xlab="Time", ylab="Growth y(t)", type="n") )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colSpline, type="l", lwd=lwd) )
        }

        if ((x$control$log.x.gc==FALSE) && (x$control$log.y.gc==TRUE)){
          try( plot(x$fit.time, x$fit.data, sub=x$name.fit, xlab="Time", ylab="Growth [Ln(y(t)/y0)]", type="n") )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd) )
        }

        if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.gc==FALSE)){
          try( plot(x$fit.time, x$fit.data, sub=x$name.fit, xlab="Ln(1+time)", ylab="Growth y(t)", type="n" ) )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit, col=colSpline, type="l", lwd=lwd ) )
        }

        if ((x$control$log.x.gc==TRUE)  && (x$control$log.y.gc==TRUE)){
          try( plot(x$fit.time, x$fit.data, sub=x$name.fit, xlab="Ln(1+time)", ylab="Growth [Ln(y(t)/y0)]", type="n") )
          try( lines(x$fit.time, x$fit.data, sub=x$name.fit,  col=colSpline, type="l", lwd=lwd) )
        }
        title("Nonparametric fit")
        legend(x="bottomright", legend="spline fit", col=colSpline, lty=1)
        mtext(line = 0.1 ,side=3, outer = F, cex=0.9, paste(x[["gcID"]], collapse = " | "))
      }
    }

    # /// add tangent at maximum slope
    if (slope==TRUE){
      mu     <- as.numeric(x$parameters$mu)
      lambda <- as.numeric(x$parameters$lambda)

      time <- seq(lambda, max(x$"fit.time"), length=200)
      y_tangent <- x$parameters["b.tangent"][[1]]+time*mu
      try(lines(time, y_tangent, lty=2, lwd=2, col=ggplot2::alpha(colSpline, 0.7), ...))
      try(lines(c(min(x$"raw.time"[1]), lambda), rep(x$"raw.data"[1], 2), lty=2, lwd=2, col=ggplot2::alpha(colSpline, 0.7)))
    }
    if (deriv  == TRUE){
      par(mar=c(3.1, 3.1, 1.1, 2.1), mgp=c(2, 1, 0))
      if ((x$control$log.x.gc==FALSE)){
          try( plot(x$spline.deriv1$x, x$spline.deriv1$y, xlab="Time", ylab="Growth rate", type="l") )
      }
      if ((x$control$log.x.gc==TRUE)){
          try( plot(x$spline.deriv1$x, x$spline.deriv1$y, xlab="Ln(1+time)", ylab="Growth rate", type="l") )
      }
    }
  }
  par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
  par(mfrow = c(1, 1))
}

plot.grpFitSpline <- function(x, names = NULL, conc = NULL, mean = TRUE, deriv = T, colors = NULL, basesize = 20, lwd = 1.1)
{

  # x an object of class grofit
  if(class(x) != "grofit") stop("x needs to be an object created with growth.fit")
  # /// check input parameters

  if (is.numeric(basesize)==FALSE)   stop("Need numeric value for: basesize")
  if (is.numeric(lwd)==FALSE)   stop("Need numeric value for: lwd")


  # Get name of conditions with multiple replicates
  sample.nm <- nm <- as.character(names(x$gcFit$gcFittedSplines))
  if(!is.null(names)){
    names <- gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", names))
    nm <- nm[grep(paste(names, collapse="|"), nm)]
  }
  if(!is.null(conc)){
    nm <- nm[which(conc == str_extract(nm, "[:alnum:]+$"))]
  }
  # remove conditions with fitFlag = FALSE in all replicates
  ndx.filt <- unique(lapply(1:length(nm), function(i) grep(paste0("^",
                                                                  gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", unlist(str_split(nm[i], " \\| "))[1])),
                                                      ".+[[:space:]]",
                                                      unlist(str_split(nm[i], " \\| "))[3],
                                                      "$"), sample.nm)))
  for(i in 1:length(ndx.filt)){
    if(!all(unlist(lapply(1:length(ndx.filt[[i]]), function(j) (x[["gcFit"]][["gcFittedSplines"]][[ndx.filt[[i]][j]]][["fitFlag"]]))))){
      nm <- intersect(nm, sample.nm[-ndx.filt[[i]]])
    }
  }

  if(mean == TRUE){
      conditions <- str_replace_all(nm, "\\| . \\| ", "| ")
      conditions_unique <- unique(conditions)

      plotdata.ls <- list()

      for(i in 1:length(conditions_unique)){
        # find indexes of replicates
          ndx <- grep(paste0("^",
                             gsub("\\.", "\\\\.",gsub("\\+", "\\\\+", unlist(str_split(conditions_unique[i], " \\| "))[1])),
                             ".+[[:space:]]",
                             unlist(str_split(conditions_unique[i], " \\| "))[2],
                             "$"), sample.nm)
          name <- conditions_unique[i]
          time <- x$gcFit$gcFittedSplines[[ndx[[1]]]]$fit.time
          data <- sapply(1:length(ndx), function(i) cbind(x$gcFit$gcFittedSplines[[ndx[[i]]]]$fit.data))
          mean <- rowMeans(data)
          sd <- apply(data, 1, sd)
          plotdata.ls[[i]] <- data.frame(name = name, time = time, mean = mean, upper = mean+sd, lower = mean-sd)
      }
      names(plotdata.ls) <- gsub(" \\| NA", "", conditions_unique)

      plotdata.ls <- plotdata.ls[!is.na(plotdata.ls)]
      df <- do.call(rbind.data.frame, plotdata.ls)
      df$name <- gsub(" \\| NA", "", df$name)

      p <- ggplot(df, aes(x=time, y=mean, col = name)) +
        geom_line(size=lwd) +
        geom_ribbon(aes(ymin=lower,ymax=upper, fill=name), alpha = 0.3, colour = NA) +
        theme_classic(base_size = basesize) +
        xlab("Time") +
        ylab(if(x$control$log.y.gc==TRUE){
          "Growth [Ln(y(t)/y0)]"
        } else {
          "Growth [y(t)]"
        }) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

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

      }
  } # if(mean == TRUE)
  else {
    ndx <- grep(paste0(
      str_replace_all(nm, "\\|", "\\\\|"), collapse = "|"), sample.nm)
    df <- data.frame()
    for(i in 1:length(ndx)){
      df <- plyr::rbind.fill(df, data.frame("name" = sample.nm[ndx[i]],
                                      "time" = x$gcFit$gcFittedSplines[[ndx[i]]][["fit.time"]],
                                      "y" = x$gcFit$gcFittedSplines[[ndx[i]]][["fit.data"]]))
    }
    p <- ggplot(df, aes(x=time, y=y, col = name)) +
      geom_line(size=lwd) +
      theme_classic(base_size = basesize) +
      xlab("Time") +
      ylab(if(x$control$log.y.gc==TRUE){
        "Growth [Ln(y(t)/y0)]"
      } else {
        "Growth [y(t)]"
      }) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

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
  }
  p
}

base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}
