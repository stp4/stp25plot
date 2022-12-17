
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
#' #' 
#' require(stp25plot)
#' require(stp25tools)
#' require(tidyverse)
#' 
#' set.seed(1)
#' 
#' n <- 50
#' 
#' dat3 <- data.frame(a = rnorm(n)) %>%
#'   
#'   mutate(
#'     b = a +  rnorm(n),
#'     c = b +  rnorm(n),
#'     d = c +  rnorm(n),
#'     e = d +  rnorm(n),
#'     f = e + rnorm(n)
#'     
#'   )
#' 
#' 
#' corr_plot( ~ a + e + f+ b + c + d , dat3,
#'             resize=TRUE
#' )
#' 
#' #corr_plot( dat3,resize=TRUE)
#' 
#' corr_plot( dat3, a, b, c,
#'             resize=TRUE
#' )
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

# corr_plot.lm <- function(x, data = x$model,
#                          type = c("p", "r"),
#                          scales = list(x = list(relation = "free")),
#                          ylab = names(data)[1],
#                          xlab = "",
#                          layout = c(ncol(data)-1,1),
#                          ...) {
#   data <- dapply2(data, function(x)
#     as.numeric(x))
#  # dat <- stp25tools::Melt2(data, id.vars = 1)
#   dat <- tidyr::gather(data, key = "variable",  value = "value",-1)
#   
#   names(dat)[1] <- "y"
#   lattice::xyplot(
#     y ~ value | variable,
#     dat,
#     ylab = ylab,
#     xlab = xlab,
#     type = type,
#     scales = scales,
#     layout =layout,
#     ...
#   )
# }


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
  
  cat("Method: ",method, "\n")
  
  
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
      
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste(prefix, txt, sep = "")
      
      txt.cex <- format(c(abs(r), 0.123456789), digits = digits)[1]
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







# graphics_pairs.default<-
# function (x,
#           labels,
#           panel = points,
#           ...,
#           horInd = 1:nc,
#           verInd = 1:nc,
#           lower.panel = panel,
#           upper.panel = panel,
#           diag.panel = NULL,
#           text.panel = textPanel,
#           label.pos = 0.5 + has.diag / 3,
#           line.main = 3,
#           cex.labels = NULL,
#           font.labels = 1,
#           row1attop = TRUE,
#           gap = 1,
#           log = "",
#           horOdd = !row1attop,
#           verOdd = !row1attop)
# {
#   if (doText <- missing(text.panel) || is.function(text.panel))
#     textPanel <- function(x = 0.5,y = 0.5,txt,cex, font){
#       text(x, y, txt, cex = cex, font = font)
#       }
#   
#   localAxis <- function(side, x, y, xpd, bg, col = NULL, main, oma, ...) {
#     xpd <- NA
#     if (side %% 2L == 1L && xl[j])
#       xpd <- FALSE
#     if (side %% 2L == 0L && yl[i])
#       xpd <- FALSE
#     if (side %% 2L == 1L)
#       Axis(x, side = side, xpd = xpd, ...)
#     else
#       Axis(y, side = side, xpd = xpd, ...)
#   }
#   
#   localPlot <-
#     function(..., main, oma, font.main, cex.main)
#       plot(...)
#   localLowerPanel <-
#     function(..., main, oma, font.main, cex.main)
#       lower.panel(...)
#   localUpperPanel <-
#     function(..., main, oma, font.main, cex.main)
#       upper.panel(...)
#   localDiagPanel <-
#     function(..., main, oma, font.main, cex.main)
#       diag.panel(...)
#   dots <- list(...)
#   nmdots <- names(dots)
#   if (!is.matrix(x)) {
#     x <- as.data.frame(x)
#     for (i in seq_along(names(x))) {
#       if (is.factor(x[[i]]) || is.logical(x[[i]]))
#         x[[i]] <- as.numeric(x[[i]])
#       if (!is.numeric(unclass(x[[i]])))
#         stop("non-numeric argument to 'pairs'")
#     }
#   }
#   else if (!is.numeric(x))
#     stop("non-numeric argument to 'pairs'")
#   panel <- match.fun(panel)
#   if ((has.lower <-
#        !is.null(lower.panel)) && !missing(lower.panel))
#     lower.panel <- match.fun(lower.panel)
#   if ((has.upper <-
#        !is.null(upper.panel)) && !missing(upper.panel))
#     upper.panel <- match.fun(upper.panel)
#   if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel))
#     diag.panel <- match.fun(diag.panel)
#   if (row1attop) {
#     tmp <- lower.panel
#     lower.panel <- upper.panel
#     upper.panel <- tmp
#     tmp <- has.lower
#     has.lower <- has.upper
#     has.upper <- tmp
#   }
#   nc <- ncol(x)
#   if (nc < 2L)
#     stop("only one column in the argument to 'pairs'")
#   if (!all(1L <= horInd & horInd <= nc))
#     stop("invalid argument 'horInd'")
#   if (!all(1L <= verInd & verInd <= nc))
#     stop("invalid argument 'verInd'")
#   if (doText) {
#     if (missing(labels)) {
#       labels <- colnames(x)
#       if (is.null(labels))
#         labels <- paste("var", 1L:nc)
#     }
#     else if (is.null(labels))
#       doText <- FALSE
#   }
#   oma <- if ("oma" %in% nmdots) dots$oma
#   main <- if ("main" %in% nmdots)  dots$main
#   if (is.null(oma))
#     oma <- c(4, 4, if (!is.null(main)) 6 else  4, 4)
#   opar <-
#     par(
#       mfcol = c(length(horInd), length(verInd)),
#       mar = rep.int(gap / 2, 4),
#       oma = oma
#     )
#   on.exit(par(opar))
#   dev.hold()
#   on.exit(dev.flush(), add = TRUE)
#   xl <- yl <- logical(nc)
#   if (is.numeric(log))
#     xl[log] <- yl[log] <- TRUE
#   else {
#     xl[] <- grepl("x", log)
#     yl[] <- grepl("y", log)
#   }
#   ni <- length(iSet <- if (row1attop) horInd else rev(horInd))
#   nj <- length(jSet <- verInd)
#   for (j in jSet)
#     for (i in iSet) {
#       l <- paste0(if (xl[j]) "x" else "", if (yl[i]) "y" else "")
#       
#       localPlot(x[, j],  x[, i],
#         xlab = "",  ylab = "",  axes = FALSE,
#         type = "n",  ...,  log = l)
#       
#       if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
#         box()
#         j.odd <- (match(j, jSet) + horOdd) %% 2L
#         i.odd <- (match(i, iSet) + verOdd) %% 2L
#         if (i == iSet[1L] && (!j.odd || !has.upper || !has.lower))
#           localAxis(3L, x[, j], x[, i], ...)
#         if (i == iSet[ni] && (j.odd || !has.upper || !has.lower))
#           localAxis(1L, x[, j], x[, i], ...)
#         if (j == jSet[1L] && (!i.odd || !has.upper || !has.lower))
#           localAxis(2L, x[, j], x[, i], ...)
#         if (j == jSet[nj] && (i.odd || !has.upper || !has.lower))
#           localAxis(4L, x[, j], x[, i], ...)
#         mfg <- par("mfg")
#         if (i == j) {
#           if (has.diag)
#             localDiagPanel(as.vector(x[, i]), ...)
#           if (doText) {
#             par(usr = c(0, 1, 0, 1))
#             if (is.null(cex.labels)) {
#               l.wid <- strwidth(labels, "user")
#               cex.labels <- max(0.8, min(2, 0.9 / max(l.wid)))
#             }
#             xlp <- if (xl[i])  10 ^ 0.5 else 0.5
#             ylp <- if (yl[j]) 10 ^ label.pos else label.pos
#             text.panel(xlp, ylp, labels[i], cex = cex.labels,
#                        font = font.labels)
#           }
#         }
#         else if (i < j)
#           localLowerPanel(as.vector(x[, j]), as.vector(x[,
#                                                          i]), ...)
#         else
#           localUpperPanel(as.vector(x[, j]), as.vector(x[,
#                                                          i]), ...)
#         if (any(par("mfg") != mfg))
#           stop("the 'panel' function made a new plot")
#       }
#       else
#         par(new = FALSE)
#     }
#   if (!is.null(main)) {
#     font.main <- if ("font.main" %in% nmdots)
#       dots$font.main
#     else
#       par("font.main")
#     cex.main <- if ("cex.main" %in% nmdots)
#       dots$cex.main
#     else
#       par("cex.main")
#     mtext( main,3,line.main,
#       outer = TRUE, at = 0.5,cex = cex.main, font = font.main
#     )
#   }
#   invisible(NULL)
# }











