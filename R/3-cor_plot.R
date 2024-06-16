#' Correlation Plot
#' 
#' Function for making a correlation plot starting from a  formula and a data.frame
#'
#' @param x  formula
#' @param ... weitere Param an Lattice oder pairs cex.labels
#'
#' @return nix
#' @export
#'
#' @examples
#' 
#' require(stp25plot)
#' require(stp25tools)
#' require(tidyverse)
#' n <- 50
#' 
#' dat3 <- data.frame(a = rnorm(n)) |>
#'   mutate(
#'     b = a + rnorm(n),
#'     c = b + rnorm(n),
#'     d = c + rnorm(n),
#'     e = d + rnorm(n),
#'     f = e + rnorm(n)
#'   )
#' 
#' corr_plot( ~ a + e + f+ b + c + d, 
#'     dat3,
#'     resize = TRUE)
#' 
corr_plot <- function(x, ...){
  UseMethod("corr_plot")
}


#' @param data  a data matrix
#' @rdname corr_plot
#' @export
#' 
corr_plot.formula <- function(x, 
                              data, 
                              cex.labels=NULL,
                              ...){
  X <- stp25tools::prepare_data2(x, data)
  data <- data.frame(plyr::llply(X$data, as.numeric))

  if( is.null(X$group.vars) )  corr_pairs(data, ..., cex.labels=cex.labels )
  else {
    # sicherstellen dass dur eine measure.vars am anfang steht
 stop( "In Corr_plot sind keine Gruppen erlaubt!")
  }
  
}

#' @param x formula oder data.frame
#'
#' @param ... an prepare_data2
#' @param jitter rauschen
#' @param smooth,lines Anpassungslienien
#' @param pch symbol Streudiagramm
#' @param digits,method correlation 
#' @param stars,resize,cex.cor correlation formatierung
#' @param hist  histogram 
#' @param col.bar,col.bar.border,col.line,col.smooth  Farben
#' @rdname corr_plot
#' @export
corr_plot.data.frame <- function(x,   ...,
                                  jitter = FALSE,
                                  smooth = FALSE,
                                  lines = TRUE,
                                  pch = 20,
                                  digits = 2,
                                  cex.cor = NULL,
                                  cex.labels=NULL,
                                  method = "pearson",
                                  stars = FALSE,
                                  resize = FALSE,
                                  hist=TRUE,
                                  col.bar= "RoyalBlue",
                                  col.bar.border = "lightblue",
                                  col.line= "blue",
                                  col.smooth=col.line,
                                  main="", sub, xlab, ylab  
) { 
  
  X <- stp25tools::prepare_data2(x, ...)
  data <- data.frame(plyr::llply(X$data, as.numeric))
  if( is.null(X$group.vars) )  corr_pairs(data,        
                                               jitter = jitter,
                                               smooth = smooth,
                                               lines = lines,
                                               pch = pch,
                                               digits = digits,
                                               cex.cor = cex.cor,
                                               cex.labels=cex.labels,
                                               method = method,
                                               stars = stars,
                                               resize = resize,
                                               hist=hist,
                                               col.bar= col.bar,
                                               col.bar.border = col.bar.border,
                                               col.line= col.line,
                                               col.smooth=col.smooth,
                                               main=main)
  else {
    # sicherstellen dass dur eine measure.vars am anfang steht
    stop( "In Corr_plot sind keine Gruppen erlaubt!")
  }
  
}

#' @param jitter Rauschen
#' @param smooth  Gezeichnete Lineie
#' @param lines  Regressinsgerade
#' @param pch Symbole  pch=20
#' @param digits Nachkommastellen in plot
#' @param cex.cor,resize   Fixe groese mit cex.cor, resize abhaengig von r-Wert
#' @param method c("pearson", "kendall", "spearman")
#' @param stars Sternchen
#' @param hist Histogram TRUE/FLASE
#' 
#' @rdname corr_plot 
#' @export
#' 
corr_pairs <- function(data,
                                 jitter = FALSE,
                                 smooth = FALSE,
                                 lines = TRUE,
                                 pch = 20,
                                 digits = 2,
                                 cex.cor = NULL,
                                 cex.labels=NULL,
                                 method = "pearson",
                                 stars = FALSE,
                                 resize = FALSE,
                                 hist=TRUE,
                                 col.bar= "RoyalBlue",
                                 col.bar.border = "lightblue",
                                 col.line= "blue",
                                 col.smooth=col.line,   ...
                              ) { 
  panel.cor <-
    function(x,
             y,
             prefix = "",
             cex_resize = .75
             #  digits, cex.cor, method, stars,resize,
            ) {
      usr <- par("usr")
      on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      # box(   col ="white")
      test <- cor.test(x, y, na.action = na.omit, method = method)
      r <- test$estimate
      
      txt <- formatC(r, digits = digits, format = "f")
      txt <- paste(prefix, txt, sep = "")
      
      txt.cex <- formatC(abs(r), digits = digits, format = "f")
      txt.cex <- paste(prefix, txt.cex, sep = "")
      
      if (is.null(cex.cor))
        cex <- cex_resize / strwidth(txt.cex)
      else cex<-cex.cor
      # borrowed from printCoefmat
      Signif <- stats::symnum(
        test$p.value,
        corr = FALSE,
        na = FALSE,
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
        symbols = c("***", "**", "*", ".", " ")
      )
      
      if (resize)
        text(0.5, 0.5, txt, cex = round(cex * abs(r), 2))
      else
        text(0.5, 0.5, txt, cex = cex)
      if (stars)
        text(.8, .8, Signif, cex = cex / 2, col = 2)
    }
  
  
  panel.hist <- function(x){
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    
    y <- h$counts
    y <- y / max(y)
    
    if (nlevels(factor(x)) < 5) {
      print(breaks[-nB])
      print(y)
    }
    box(lty = 1, col = 'white')
    rect(breaks[-nB], 0, breaks[-1], y, col = col.bar, border = col.bar.border)
  }
  
  panel.lines2 <-
    function (x, y,
              col = par("col"),
              bg = NA,
              pch = par("pch"),
              cex = 1,
             # col.smooth = "blue",
              span = 2 / 3,
              iter = 3
             # lines smooth,
            )
    {
      if (nlevels(factor(x)) < 5) x <- jitter(x)
      if (nlevels(factor(y)) < 5) y <- jitter(y)
      
      points(x ,y,
             pch = pch, col = col,
             bg = bg, cex = cex
      )
      axis(2, labels = FALSE)
      axis(1, labels = FALSE)
      
      if (lines)
        abline(lm(y ~ x, 
                  data = na.omit(data.frame(x, y))), 
               col = col.line)
      if (smooth) {
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
          lines(stats::lowess(x[ok], y[ok], 
                              f = span, iter = iter),
                col = col.smooth)
      }
    }
  
  
  par(pch = pch, bty = 'n')


  graphics::pairs(
    data,
    lower.panel = panel.lines2,
    upper.panel = panel.cor,
    diag.panel =   if(hist) panel.hist else NULL,
    cex.labels=cex.labels,
    ...)
}
