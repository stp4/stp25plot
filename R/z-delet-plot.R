##  Uebereinstimmung und Praezision von Messwerten
##  
##  Tukey Mean Difference oder auch Bland Altman Metode. Oft iteressiert
##  die Zuverlässigkeit und Reproduzierbarkeit ein einer Diagnose.
##  Die Beurteikung kann dabei durch einen Bewerter (Messverfahren) in wiederholter Form erfolgen
##  und wird dan als Intra-Raterbezeichnet oder die Beurteilung eines Merkmals erfolgt durch mehere Bewerter (Messverfahren).
##  und hier spricht man von Inter-Rater.
# 
##  Die Methode der Burteilung der Uebereinstimmung haengt vom jeweiligen Datentype ab.
# 
##  @param x  bland_altman-Objekt , par=TRUE
##  @param main   default =  name.diff
##  @param main1  default regression
##  @param main2  default differences
##  @param main3  default precentage
##  @param ylab1  default met_B
##  @param ylab2  default Differences
##  @param ylab3  default Differences/Average
##  @param xlab1  default met_A
##  @param xlab2  default  name
##  @param xlab3  default x$name
##  @param type  default =2
##  @param pch  default = 19
##  @param col farbe
##  @param x.var   met_A
##  @param y.var   met_B
##  @param pos  topleft
##  @param legend  levels groups
##  @param fil weiss nicht
##  @param abline.col Farbe c("black","black","black"),
##  @param abline.lty  Lineie abline.lty=c(3,1,3),
##  @param par par
##  @param ...  nicht Benutzt
##  @param abline.lwd  Strischstaerke abline.lwd=c(1,1,1)
# 
##  @return Ein bland_altman-Objekt mit den Daten (data) und der Statistik (stat).
##  @export
##  @examples
##  
##  library(stp25plot)
##  library(stpvers)
##  #library(tidyr)
##  #graphics.off()
##  #setwd("C:/Users/wpete/Dropbox/1_Projekte/000 Temp")
##  #Projekt("html", "bland_altman")
##  #- Understanding Bland Altman analysis
##  #Davide Giavarina
##  #Biochemia medica 2015;25(2) 141-51
##  #http://dx.doi.org/10.11613/BM.2015.015
##  
##  set.seed(0815)
##  DF<- data.frame(
##    A=c(1, 5,10,20,50,40,50,60,70,80, 90,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900, 950,1000),
##    B=c(8,16,30,14,39,54,40,68,72,62,122, 80,181,259,275,380,320,434,479,587,626,648,738,766,793,851,871,957,1001, 980),
##    group= sample(gl(2, 15, labels = c("Control", "Treat")))
##  )
##  
##  MetComp(~A+B, DF, caption = "Giavarina")
##  
##  #Sachs Angewandte Statistik Seite 627
##  
##  DF2<- data.frame(A= factor(c(rep(1, 14), rep(1, 3),
##                               rep(0, 5),rep(0, 18)),1:0, c("+", "-")),
##                   B= factor(c(rep(1, 14), rep(0, 3),
##                               rep(1, 5),rep(0, 18)),1:0, c("+", "-")) )
##  
##  
##  APA2(xtabs(~A+B, DF2), test=T)
##  MetComp(~A+B, DF2)
##  
##  
##  DF <- transform(DF, C = round( A + rnorm(30,0,20)),
##                  D = round( A + rnorm(30,0,10) + A/10 ),
##                  E = round( A + rnorm(30,5,10) + (100-A/10) ))
##  
##  head(DF)
##  xt <-xtabs(~A+B, DF2)
##  Klassifikation2(xt)
##  
##  x<- MetComp_BAP(~A+E , DF)
##  
##  ICC2(~A+E , DF)
##  windows(8,3)
##  plot(x)
##  SaveData()
##  x<- MetComp_BAP(A+E+B~group, DF)
##  
##  windows(8,3.2)
##  plot(x)
##  
##  n<-1000
##  DF<- data.frame(
##    A=rnorm(n, 100,50),
##    B=rnorm(n, 100,50),
##    C=NA,  D=NA,  E=NA,
##    group= sample(gl(2, n/2, labels = c("Control", "Treat")))
##  )
##  DF <- transform(DF, C = round( A + rnorm(n,0,20)),
##                  D = round( A + rnorm(n,0,10) + A/10 ),
##                  E = round( A + rnorm(n,5,10) + (100-A/10) ))
##  
##  x<- MetComp_BAP(A+E~group, DF)
##  
##  windows(8,3.2)
##  plot(x)
##  
##  
##  #library(stp25)
##  #library(tidyr)
##  #graphics.off()
##  #setwd("C:/Users/wpete/Dropbox/1_Projekte/000 Temp")
##  #Projekt("html", "bland_altman")
##  #- Understanding Bland Altman analysis
##  #Davide Giavarina
##  #Biochemia medica 2015;25(2) 141-51
##  #http://dx.doi.org/10.11613/BM.2015.015
##  
##  set.seed(0815)
##  
##  n<-100
##  DF<- data.frame(
##    A=rnorm(n, 100,50),
##    B=rnorm(n, 100,50),
##    C=NA,  D=NA,  E=NA,
##    group= sample(gl(2, n/2, labels = c("Control", "Treat")))
##  )
##  
##  cutA<-mean(DF$A)
##  DF <- transform(DF, C = round( A + rnorm(n, -5, 20)),
##                  D = round( A + rnorm(n,0,10) + A/10 ),
##                  #E = round( A + rnorm(n,5,10) + (100-A/10) )
##                  E = A + ifelse(A<cutA, A/5, -A/5 )+ rnorm(n, 0, 10)
##  )
##  
##  
##  x<- MetComp_BAP(~A+C, DF)
##  windows(8,3.2)
##  plot(x)
##  SaveData(caption="A und C Messen das gleiche mit SD=20")
##  
##  x<- MetComp_BAP(~A+B, DF)
##  windows(8,3.2)
##  plot(x)
##  SaveData(caption="A und B Messen unterschiedliche Parameter")
##  
##  
##  x<- MetComp_BAP(~A+D, DF)
##  windows(8,3.2)
##  plot(x)
##  SaveData(caption="A und D Messen das unterschiedlich D hat im unteren
##           Wertevereich deutlich geringere Werte")
##  x<- MetComp_BAP(~A+E, DF)
##  windows(8,3.2)
##  plot(x)
##  SaveData(caption="A und E Messen das unterschiedlich es esistiert ein knik im Wertebereich 100")
##  
##  End()
# plot.bland_altman <- function(x,
#                               par = TRUE,
#                               main = x$name.diff,
#                               main1 = "regression",
#                               main2 = "differences",
#                               main3 = "precentages",
# 
#                               ylab1 = x$met_B,
#                               ylab2 = "Differences",
#                               ylab3 = "Differences/Average [%]",
# 
#                               xlab1 = x$met_A,
#                               xlab2 = paste0("Means (" , x$name, ")"),
#                               xlab3 = paste0("Means (" , x$name, ")"),
# 
#                               type = 2,
#                               pch = 19,
#                               col = if (is.null(x$groups))
#                                 1
#                               else
#                                 3 + as.numeric(x$groups[[1]]),
#                               x.var = x$met_A,
#                               y.var = x$met_B,
#                               pos = "topleft",
#                               legend = levels(x$groups[[1]]),
#                               fil = 3 + (1:nlevels(x$groups[[1]])),
#                               abline.col = c("black", "black", "black"),
#                               abline.lty = c(3, 1, 3),
#                               abline.lwd = c(1, 1, 1),
#                               ...) {
#   # print(x$lines) # print(x$CI.lines) # print(x$lines.percent) # print(x$CI.lines.percent)
# 
#   if (par)
#     par(mfrow = c(1, 3), oma = c(0.4, 1, 1.6, 1))
#   xy_plot_range<- range( c(x$data[, x.var],  x$data[, y.var]), na.rm=TRUE) 
#  
#    plot(
#     x$data[, x.var],
#     x$data[, y.var],
#     xlab = xlab1,
#     ylab = ylab1,
#     xlim=xy_plot_range, ylim=xy_plot_range,
#     main = main1,
#     pch = pch,
#     col = col,
#     bty = 'l'
#   )
#   abline(lm(x$data[, y.var] ~ x$data[, x.var]),
#          lty = 1,
#          lwd = 1,
#          col = abline.col[2])
#   if (!is.null(x$groups))
#     legend(
#       x = pos,
#       legend = legend,
#       fill = fil,
#       bty = "n"
#     )
#   title(main = main, outer = TRUE)
# 
#   # plot(x$data$means, x$data$diffs,
#   #      ylim= range(c(x$data$diffs, x$lines), finite = TRUE),
#   #      xlab=xlab2, ylab=ylab2, main=main2, pch=pch, col=col )
#   #     abline(h = x$lines, lty=abline.lty, col=abline.col, lwd=abline.lwd)
#   #     abline(h = x$CI.lines)
#   rgn<-range(x$data$means, finite = TRUE) * c(1, 1.15)
#   plot(
#     x$data$means,
#     x$data$diffs,
#     type = "n",
#     axes = FALSE,
#     xlim = rgn,
#     ylim = range(c(x$data$diffs, x$lines), finite = TRUE),
#     xlab = xlab2,
#     ylab = ylab2,
#     main = main2
#   )
#   lim <- par("usr")
#   rect(lim[1],
#        x$CI.lines[3],
#        lim[2],
#        x$CI.lines[4],
#        border = "gray80",
#        col = "gray95")
#   abline(
#     h = x$lines,
#     lty = abline.lty,
#     col = abline.col,
#     lwd = abline.lwd
#   )
#   points(x$data$means, x$data$diffs, pch = pch, col = col)
# 
#   text(x=c(rgn[2],rgn[2]),
#        y=x$lines ,
#        c("-1.96 SD", "Mean", "+1.96 SD"), adj=c(1,0), cex=.7)
#   text(x=c(rgn[2],rgn[2]),
#        y=x$lines,
#        signif(x$lines, 2)  , adj=c(1,1), cex=.7)
# 
#   axis(1) ## add axes back
#   axis(2)
#   box()
# 
# 
# 
#   if (!is.null(x$groups))
#     legend(
#       x = pos,
#       legend = legend,
#       fill = fil,
#       bty = "n"
#     )
#   #- Prozent ---------------------------------
#   ## Fehler wenn Inf
#   #range(x$data$means )
# 
#   plot(
#     x$data$means,
#     x$data$diffs.percent,
#     type = "n",
#     axes = FALSE,
#     xlim = rgn,
#     ylim = range(c(x$lines.percent,x$data$diffs.percent), finite = TRUE),
#     ##x$data$diffs.percent,
#     xlab = xlab3,
#     ylab = ylab3,
#     main = main3
#   )
# 
# 
#   lim <- par("usr")
#   rect(
#     lim[1],
#     x$CI.lines.percent[3],
#     lim[2],
#     x$CI.lines.percent[4],
#     border = "gray80",
#     col = "gray95"
#   )
#   abline(
#     h = x$lines.percent,
#     lty = abline.lty,
#     col = abline.col,
#     lwd = abline.lwd
#   )
#   points(x$data$means,
#          x$data$diffs.percent,
#          pch = pch,
#          col = col)
#   text(x=c(rgn[2],rgn[2]),
#        y=x$lines.percent ,
#        c("-1.96 SD", "Mean", "+1.96 SD"), adj=c(1,0), cex=.7)
#   text(x=c(rgn[2],rgn[2]),
#        y=x$lines.percent,
#        signif(x$lines.percent, 2)  , adj=c(1,1), cex=.7)
# 
#   axis(1) ## add axes back
#   axis(2)
#   box()
#   if (!is.null(x$groups))
#     legend(
#       x = pos,
#       legend = legend,
#       fill = fil,
#       bty = "n"
#     )
# }




#  
# corr_plot <- function(x, ...){
#   UseMethod("corr_plot")
# }
# 
#  
# corr_plot.formula <- function(x, data, ...){
#   dat <- stp25formula::prepare_data2(x, data)
#   if( is.null(dat$group.vars) )  corr_plot.data.frame(dat$data, ... )
#   else {
#     # sicherstellen dass dur eine measure.vars am anfang steht
#     corr_plot.lm(NULL, dat$data[,c(dat$measure.vars[1], dat$group.vars)], ...)
#     }
#   
# }
# 
# 
#  
# corr_plot.lm <- function(x,
#                          data = x$model,
#                          type = c("p", "r"),
#                          scales = list(x = list(relation = "free")),
#                          ylab = names(data)[1],
#                          xlab = "",
#                          layout = c(ncol(data)-1,1),
#                          ...) {
#   data <- dapply2(data, function(x)
#     as.numeric(x))
#   dat <- Melt2(data, id.vars = 1)
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
# 
#  
# corr_plot.data.frame <- function(data,
#                                  jitter = FALSE,
#                                  smooth = TRUE,
#                                  lines = TRUE,
#                                  pch = 20,
#                                  digits = 2,
#                                  cex.cor = NULL,
#                                  method = "pearson",
#                                  stars = FALSE,
#                                  resize = FALSE,
#                                  hist=TRUE,
#                                  ...) { 
#   cat(method, "\n")
#   par(pch = pch, bty = 'n')
#   data <- dapply2(data, function(x) as.numeric(x))
#     pairs(
#     data,
#     lower.panel = panel.lines2,
#     upper.panel = panel.cor,
#     diag.panel =   if(hist)  panel.hist else NULL,
#     # cex.labels=2,
#     method = method,
#     stars = stars,
#     resize = resize ,
#     cex.cor = cex.cor,
#     digits = digits ,
#     smooth = smooth,
#     lines = lines,
#     ...)
# }
# 
# 
# panel.cor <-
#   function(x,
#            y,
#            digits,
#            prefix = "",
#            cex.cor,
#            method,
#            stars,
#            resize,
#            ...,
#            cex_resize = .75)
#   {
#     usr <- par("usr")
#     on.exit(par(usr))
#     par(usr = c(0, 1, 0, 1))
#     # box(   col ="white")
#     test <- cor.test(x, y , na.action = na.omit, method = method)
#     
#    # print(test)
#     r <- test$estimate
#   
#     txt <- format(c(r, 0.123456789), digits = digits)[1]
#     txt <- paste(prefix, txt, sep = "")
#     
#     txt.cex <- format(c(abs(r), 0.123456789), digits = digits)[1]
#     txt.cex <- paste(prefix, txt.cex, sep = "")
#     
#     if (is.null(cex.cor))
#       cex <- cex_resize / strwidth(txt.cex)
#     else cex<-cex.cor
#     # borrowed from printCoefmat
#     Signif <- stats::symnum(
#       test$p.value,
#       corr = FALSE,
#       na = FALSE,
#       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
#       symbols = c("***", "**", "*", ".", " ")
#     )
#     
#     if (resize)
#       text(0.5, 0.5, txt, cex = round(cex * abs(r), 2))
#     else
#       text(0.5, 0.5, txt, cex = cex)
#     if (stars)
#       text(.8, .8, Signif, cex = cex / 2, col = 2)
# }
# 
# 
# panel.hist <- function(x, ...)
# {
# 
#   usr <- par("usr")
#   on.exit(par(usr))
#   par(usr = c(usr[1:2], 0, 1.5))
#   
#   h <- hist(x, plot = FALSE)
#   breaks <- h$breaks
#   nB <- length(breaks)
#   
#   
#   y <- h$counts
#   y <- y / max(y)
#   
#   if (nlevels(factor(x)) < 5) {
#     print(breaks[-nB])
#     print(y)
#   }
#   box(lty = 1, col = 'white')
#   rect(breaks[-nB], 0, breaks[-1], y, col = "RoyalBlue", border = "lightblue")
# }
# 
# 
# # Anpassungslieneie mit Itter
# panel.lines2 <-
#   function (x, y,
#             col = par("col"),
#             bg = NA,
#             pch = par("pch"),
#             cex = 1,
#             col.smooth = "blue",
#             span = 2 / 3,
#             iter = 3,
#             lines,
#             smooth,
#             ...)
#   {
#     if (nlevels(factor(x)) < 5)
#       x <- jitter(x)
#     if (nlevels(factor(y)) < 5)
#       y <- jitter(y)
#     
#     points(x,y,
#            pch = pch,
#            col = col,
#            bg = bg,
#            cex = cex
#     )
#     axis(2, labels = FALSE)
#     axis(1, labels = FALSE)
#     
#     if (lines)
#       abline(lm(y ~ x, 
#                 data = na.omit(data.frame(x, y))), 
#              col = col.smooth)
#     if (smooth) {
#       ok <- is.finite(x) & is.finite(y)
#       if (any(ok))
#         lines(stats::lowess(x[ok], y[ok], 
#                             f = span, iter = iter),
#               col = col.smooth)
#     }
#   }






## Create a profile plot.
##
## This code is based on the the profile plotting code by Detlev Reymann October 2011
##
## @param  x The matrix of data to plot. Each profile item is in a column with an appropriate column name.The column names will be used as the names for the profiles in the legend.
## @param text.left text.left=NULL The text to appear on the left hand side (same length as nrow(x))
## @param text.right text.right=NULL The text to appear on the right hand side (same length as nrow(x))
## @param highlight.col highlight.col=-1 The column index of the entry to highlight. (Default, none highlighted)
## @param text.width.adjust text.width.adjust=35 The adjustment factor for the spacing for the text on the left and right of the plot. If you  need more space, make this larger. If you need less, make it smaller.
## @param colors colors=NULL,	 The vector of colors to use for the lines. Default is rainbow()
## @param draw.segments draw.segments=TRUE Whether or not horizontal segements should be drawn
## @param draw.mid draw.mid=TRUE Whether or not a vertical line should be drawn down the middle of the profile plot.
## @param plot.legend plot.legend=TRUE Whether or not to include a legend at the bottom
## @param legend.n.col legend.n.col=4 The number of columns that should be used in the legend
## @param sep sep=" - ""Aufregend - Langweilig"
## @param ... weitere Objekte nicht benutzt
##
## @return  plot
## @export
## @examples
## ## Create some random data
## set.seed(0815)
## x = matrix(rnorm(50), ncol=5)
## colnames(x) = c("A", "B", "C", "D", "E")
##
## ## Some random labels for the left and right text. These labels
## ## need to be the same length as the number of rows in x
## text.left = paste(1:nrow(x), "Left Text")
## text.right = paste(1:nrow(x), "Right Text")
##
## ## Create the profile plot. See the details in profile_plot.R for
## ## the details on the parameters
## labels<-c(
##   "Aufregend - Langweilig",
##   "Gut gemacht - Schlecht gemacht",
##   "Glaubwürdig - Unglaubwürdig",
##   "Spannend - Eintönig",
##   "Verständlich - Unverständlich",
##   "Guter Action-Schauspieler - schlechter Action Schauspieler",
##   "sympathisch - unsympathisch",
##   "talentiert - untalentiert",
##   "modisch - unmodisch",
##   "positive persönliche Einstellung - negative Einstellung gegenüber"
## )
##
##
##
## rownames(x)<- labels
## profile_plot(
##   x,
##   highlight.col=2,
##   legend.n.col=5)
##
## data <- data.frame(variable=labels,
## Control=rnorm(10),
## Treat=rnorm(10)
## )
## profile_plot(data,
##               draw.mid=FALSE
## )
##


# profile_plot <- function(x,
#                          text.left = NULL,
#                          text.right = NULL,
#                          highlight.col = -1,
#                          text.width.adjust = 35,
#                          colors = NULL,
#                          draw.segments = TRUE,
#                          draw.mid = TRUE,
#                          plot.legend = TRUE,
#                          legend.n.col = 4,
#                          sep = " - ",
#                          ...) {
#   # if (!stp25stat::all_identical2(x)) {
#   #   labels <- as.character(x[, 1])
#   #   x <- as.matrix(x[, -1])
#   #   rownames(x) <- labels
#   # }
#   # print(x)
#   if (is.null(text.left) & is.null(text.right)) {
#     if (!is.null(rownames(x)))
#       labels <- strsplit(rownames(x), sep)
#     text.left <- sapply(labels, "[", 1)
#     text.right <- sapply(labels, "[", 2)
#     
#     #text.left<-c()
#     # text.right<-c()
#       }
#   
#   ## Find the min and max for the x-axis
#   min.x = floor(min(x))
#   max.x = ceiling(max(x))
#   mid.x = (max.x + min.x) / 2
#   
#   ##
#   ## Configure the graphics output
#   ##
#   
#   ## Keep the old par() setting around so that we can reset them at the end
#   par.default = par(no.readonly = TRUE)
#   
#   ## create new window for output
#   plot.new()
#   
#   ## Outer margin:
#   par(oma = c(0, 0, 0, 0))
#   
#   ## letter size
#   # par(ps=14)
#   
#   ## First discover text largest in size to set dimensions
#   ## for text and graph
#   left.text.width = max(strwidth(text.left))
#   right.text.width = max(strwidth(text.right))
#   
#   ## I (Detlev Reymann) found this multiplication factor just by trail
#   ## and error has to be adjusted for longer text!!
#   left.text.width = left.text.width * text.width.adjust
#   right.text.width = right.text.width * text.width.adjust
#   
#   ## Define area for drawing the graph (Text is not part of the graph)
#   ## par(mar=c(bottom, left, top, right))
#   par(mar = c(0.5, left.text.width, 0.5, right.text.width))
#   
#   ## Calculating the size for the caption
#   ## three names in one line
#   if (plot.legend) {
#     line.height = 0.66
#     caption.height = -((nrow(x) %% 3) + 1) * line.height
#     plot.window(xlim = c(min.x, max.x),
#                 ylim = c(caption.height, nrow(x)))
#   } else {
#     plot.window(xlim = c(min.x, max.x), ylim = c(0, nrow(x)))
#   }
#   
#   ##
#   ## Let's do some plotting
#   ##
#   
#   ## Configure Y-axis
#   y = 1:nrow(x)
#   
#   ## Draw a dotted line for each item
#   if (draw.segments) {
#     segments(min.x, y, max.x, y, lty = "dotted", col = "gray")
#   }
#   
#   ## Draw a line for the average/neutral value
#   if (draw.mid) {
#     print(draw.mid)
#     lines(c(mid.x, mid.x),
#           c(1, nrow(x)),
#           lwd = 3,
#           col = "gray")
#   }
#   
#   ## The set of colors to use (if none provided)
#   if (is.null(colors)) {
#     colors = rainbow(ncol(x))
#   }
#   
#   ## If there is some highlighting to be done, make the default line
#   ## dashed
#   if (highlight.col > 0) {
#     par(lwd = 1, lty = "dashed")
#   }
#   
#   ## Loop over all profiles (columns)
#   for (i in 1:ncol(x)) {
#     #	print(c)
#     ## In most cases we want one of the objects to be highlighted
#     ## in the graph, my own brand, the reference object etc.
#     if (highlight.col == i) {
#       par(lwd = 3, lty = 1)
#     } else {
#       par(lwd = 1, lty = "dashed")
#     }
#     
#     ## Draw lines and symbols for each object
#     for (j in nrow(x):1) {
#       points(x[j, i], j, col = colors[i], pch = 19)
#       
#       ## Connect with lines
#       if (j < nrow(x)) {
#         lines(c(x[(j + 1), i], x[j, i]), c((j + 1), j), col = colors[i])
#       }
#     }
#   }
#   
#   ## Display Text on the left
#   mtext(
#     rev(text.left),
#     at = y,
#     adj = 1,
#     side = 2,
#     las = 2
#   )
#   
#   ## Display Text on the right
#   mtext(
#     rev(text.right),
#     at = y,
#     adj = 0,
#     side = 4,
#     las = 2
#   )
#   
#   ##
#   ## Draw x-axis
#   ##
#   
#   ## Set ticks for x-axis
#   ticks = c(round(min.x, 3),
#             round(((min.x + mid.x) / 2), 3),
#             round(mid.x, 3),
#             round(((mid.x + max.x) / 2), 3),
#             round(max.x, 3))
#   
#   par(cex.axis = 0.5, mex = 0.5)
#   axis(1,
#        at = ticks,
#        labels = ticks,
#        pos = 0.5)
#   
#   ##
#   ## Add in the legend
#   ##
#   
#   if (plot.legend) {
#     legend(
#       "bottom",
#       legend = colnames(x),
#       pch = 19,
#       col = colors,
#       bg = NA,
#       box.col = NA,
#       ncol = legend.n.col
#     )
#   }
#   
#   ## Reset all of the par() settings to what they were before we started
#   par(par.default)
# }

## Balken sind nicht Sinnvoll da das mit lattice einfacher geht##
## bzw mit  panel.barchart.value 

# bar_plot <- function(formula, data,
#                     main="", xlab=NULL, ylab=NULL,
# 
#                     xlim=NULL, ylim=NULL,
#                     col=NA,
#                     percent=TRUE,
#                     digits=2, #pos= 3,
#                     suffix="",
#                     type=1, #--Saeulendiagramm
#                     groups=NULL,
#                     margin = NULL,
#                     value=TRUE,
#                     fun=function(x) mean(x, na.rm=TRUE),
#                     pos=NULL,
#                     pos_panel= if(is.null(pos)) {
#                         if(type==1 | type=="vertikal" ) 3
#                         else 4
#                     }else  pos ,
#                     var_names= Formula_Names(formula, data),
#                     panel=function(...)   {
#                         panel.barchart(...)
#                         if(value) panel.barchart.value(..., digits=digits, pos=pos_panel, suffix=suffix)
#                     },
#                     ...){
# require(lattice)
#     #  print(var_names)
#     #  print(is_all_identical2(data[ var_names$yname]))
#     if(!is_all_identical2(data[ var_names$yname])) {
#         cat("\nDas Skalenniveau ist gemischt! und daher hann nichts berechnet werden\n")
#         return(str(data[, var_names$yname]))
#     }else{
#         if(all(sapply(data[var_names$yname], is.factor))){
# 
#             #
#             #print("Factor")
#             # print(var_names$yname )
#             if(!is.null(var_names$xname)) {
#                 if(is.null(margin)) margin<-1
#                 formula <- formula(paste( "~",
#                                           paste(c(var_names$xname, var_names$yname) , collapse="+")
# 
#                 ))}
# 
#             xtb <- xtabs(formula, data)
#             xdata <- data.frame( xtb )
#             xnames <- names(xdata)[-ncol(xdata) ]
#             prcdata <-   data.frame(prop.table(xtb, margin = margin)*100)
#             n <- ncol(prcdata )
#             x_data <- cbind(prcdata[-n], x__n= data.frame(xtb)[,n], x__Freq=prcdata[,n])
#             #  print(x_data)
#             vars  <- names(x_data)[1:(n-1)]
#             xylim <- extendrange(c(0, x_data$x__Freq)) + c(0, 5)
#             orgn  <- 0
#             xylab <- "Prozent"
#             suffix <- "%"
# 
#         }else{
#             # print("gggg")
#             m_data <- Recast2(formula, data, fun=fun)
#             n_data <- Recast2(formula, data, fun=function(x) length(na.omit(x)))
#             n <- ncol(m_data)
#             names(m_data)[n-1] <- "x__variable"
#             x_data <- cbind( m_data[-n], x__n=n_data[,n], x__Freq= m_data[,n])
# 
#             x_data<- x_data[c("x__variable", names(x_data)[-which(names(x_data) %in% "x__variable" )])]
#             vars <- names(x_data)[1:(n-1)]
#             rng <- range(data[var_names$yname], na.rm=TRUE)
#             xylim <- extendrange(rng , f= 0.01) #    c(0,  x_data$x__Freq))
#             orgn <- rng[1]
#             xylab <- "Mittelwert"
#         }
#         if(length(vars)>1){
# 
#             groups_name <-  if(is.null(substitute(groups))) vars[2]
#             else substitute(groups)
#             # cat("\ngroups: ", groups_name, class(groups_name), "\n")
#             if(groups_name=="F") warning("Achtung bei Gruppe mit Namen  F besteht verwechslungsgefahr besser FALSE verwenden")
# 
#             if(!is.logical(groups_name)) {
#                 groups_name<- as.character(groups_name)
# 
#                 groups <- x_data[, groups_name]
#                 vars <- vars[-which(vars %in% groups_name)]
#             }
#             else {
#                 groups=NULL
# 
# 
#             }
#         }
# 
# 
#         if(type==1 | type=="vertikal"){
#             if(length(vars)==1) fm <- formula(paste( "x__Freq~", vars[1]))
#             else if(length(vars)==2 & is.null(groups)) fm <- formula(paste( "x__Freq~", vars[2] ))
#             else fm <- formula(paste("x__Freq~",vars[1], "|", paste(vars[-1], collapse="+")))
#             if(is.null(ylim)) ylim <-xylim
#             if(is.null(ylab))   ylab <- xylab
#             print(fm)
#             print(vars)
#             p1 <- barchart(fm, x_data, groups=groups,
#                            origin=orgn,
#                            ylim= ylim, ylab=ylab, main=main,
#                            panel=panel, ...)
# 
# 
#         } else {
#             if(length(vars)==1) fm <- formula(paste(vars[1], "~x__Freq"))
#             else if(length(vars)==2 & is.null(groups)) fm <- formula(paste(vars[2] , "~x__Freq"))
#             else fm <- formula(paste(vars[1],"~x__Freq", "|", paste(vars[-1], collapse="+")))
#             if(is.null(xlim)) xlim <-xylim
#             if(is.null(xlab))   xlab <- xylab
# 
#             p1 <- barchart(fm, x_data, groups=groups,
#                            origin=orgn,
#                            xlim= xlim, xlab=xlab, main=main,
#                            panel=panel, ...)
# 
# 
#         }
#         print(p1)
#         return(x_data)
#     }
# }
# 
# 
# 
# 
# balken <- function(x,
#                    data,
#                    main = "",
#                    ylab = "",
#                    xlab = "",
#                    cex.x = 1.5,
#                    cex.y = 1.3,
#                    cex.ltext = 1.1,
#                    settings = set_lattice_ggplot(col.bar = cbPalette['blue']),
#                    margin = 2,
#                    include.value = FALSE,
#                    auto.key = list(space = "bottom", columns = 2),
#                    orientation = NULL,
#                    wrap=NULL,
#                    ...) {
#     if(!is.null(wrap)) main <- wrap_sentence(main, wrap)
#     
#     n_vars <- length(all.vars(x))
#     lhs <- all.vars(x)[1L]
#     rhs <-  all.vars(x)[2L]
#     chs <-  all.vars(x)[3L]
#     
#     if (n_vars == 1) {
#         dat <- to_table(x, data)
#         if (is.null(orientation))
#             orientation <- 1
#         
#         
#         if (orientation == 1) {
#             fm <- formula(paste0(lhs, "~ Percent"))
#             lattice::barchart(
#                 fm,
#                 dat,
#                 main = main,
#                 origin = 0,
#                 xlab = list(label = xlab, cex = cex.x),
#                 scales = list(y = list(cex = cex.y)),
#                 par.settings = settings,
#                 panel = function(x, y, ...) {
#                     lattice::panel.barchart(x, y, ...)
#                     if (include.value) {
#                         prz <- stp25rndr::rndr_percent(dat$Percent, dat$Freq)
#                         
#                         ltext(
#                             x = .4,
#                             y = y,
#                             adj = c(0, NA),
#                             col = "white",
#                             labels = prz,
#                             cex = cex.ltext
#                         )
#                     }
#                 },
#                 ...
#             )
#         }
#         else{
#             fm <- formula(paste0("Percent ~ ", lhs))
#             lattice::barchart(
#                 fm,
#                 dat,
#                 main = main,
#                 origin = 0,
#                 ylab = list(label = ylab, cex = cex.y),
#                 scales = list(x = list(cex = cex.x)),
#                 par.settings = settings,
#                 panel = function(x, y, ...) {
#                     lattice::panel.barchart(x, y, ...)
#                     if (include.value) {
#                         prz <- stp25rndr::rndr_percent(dat$Percent, dat$Freq)
#                         ltext(
#                             x = x,
#                             y = y - 5,
#                             adj = c(NA, 0),
#                             col = "white",
#                             labels = prz,
#                             cex = cex.ltext
#                         )
#                     }
#                 },
#                 ...
#             )
#         }
#         
#     }
#     else if (n_vars == 2) {
#         dat <- to_table(x, data, margin = margin)
#         names(dat)[2] <- "groups"
#         
#         fm <- formula(paste("Percent ~ ", lhs))
#         lattice::barchart(
#             fm,
#             dat,
#             groups = groups,
#             origin = 0,
#             ylab = ylab,
#             main = main,
#             par.settings = settings,
#             auto.key = auto.key,
#             panel = function(x, y, ...) {
#                 lattice::panel.barchart(x, y, ...)
#                 if (include.value) {
#                     prz <- stp25rndr::rndr_percent(dat$Percent, dat$Freq)
#                     ltext(
#                         x = x,
#                         y =  y - 5,
#                         adj = c(NA, 0),
#                         col = "white",
#                         labels = prz,
#                         cex = cex.ltext
#                     )
#                 }
#                 
#             },
#             ...
#         )
#         
#     }
#     else if (n_vars == 3) {
#         list(n_vars, lhs, rhs, chs)
#         
#     }
#     else{
#         list(n_vars, NULL, NULL, NULL)
#     }
# }



## Modifiziert effect-Plot
##
## Soll nur im Notfall geladen werden geht nicht Mehr
## 
## @param x Model fit
## @param selection,rows,cols,ask,graphics alles nicht ändern
## @param xlab  xlabs Ändern
## @param ...
##
## @return Plot
##
## @examples
## library(effects)
## library(gridExtra)
## A = rnorm(100)
## B = rnorm(100)
## C = factor(rep(c("This", "That"), 50))
##
## #-- Modifiziert wegen xlab
##
## plot.efflist <- stp25:::plot.efflist
##
## ef <- allEffects(lm(A ~ B + C))
## plot(ef, xlab = c("Foo", "Bar"))

# plot.efflist <- function(x, selection,
#                                  rows, cols,
#                                  ask=FALSE, # nicht vom mir benutzt
#                                  graphics=TRUE,
#                                  lattice,
#                                  xlab,
#                                  ...){
#   # Next line added 8/23/17 along with lattice, also lattice arg above
#   lattice <- if(missing(lattice)) list() else lattice
#   if (!missing(selection)){
#     if (is.character(selection)) selection <- gsub(" ", "", selection)
#     return(plot(x[[selection]], ...))
#   }
#   effects <- gsub(":", "*", names(x))
#   # if (ask){
#   #   repeat {
#   #     selection <- menu(effects, graphics=graphics, title="Select Term to Plot")
#   #     if (selection == 0) break
#   #     else print(plot(x[[selection]], ...))
#   #   }
#   # }
#  #  else {
# 
#   #effects:::mfrow
#     neffects <- length(x)
#     mfrow <- effects:::mfrow(neffects)
#     if (missing(rows) || missing(cols)){
#       rows <- mfrow[1]
#       cols <- mfrow[2]
#     }
#     for (i in 1:rows) {
#       for (j in 1:cols){
#         if ((i-1)*cols + j > neffects) break
#         more <- !((i-1)*cols + j == neffects)
#         lattice[["array"]] <- list(row=i, col=j, nrow=rows, ncol=cols, more=more)
# 
#         if (missing(xlab)) {
#           print(plot(x[[(i - 1) * cols + j]], lattice = lattice, ...))
#         }  else {
#           k <- (i - 1) * cols + j
#           x_lab <-
#             if (is.null(names(xlab)))
#               xlab[k]
#           else
#             xlab[names(x[k])]
# 
#           print(plot(x[[k]], lattice = lattice, xlab = x_lab, ...))
# 
#         }
#       }
#     }
#   #}
# }




