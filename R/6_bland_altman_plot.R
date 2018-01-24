#' @rdname plot.bland_altman
#' @title Uebereinstimmung und Praezision von Messwerten
#' @name plot.bland_altman
#' @description Tukey Mean Difference oder auch Bland Altman Metode. Oft iteressiert
#' die Zuverlässigkeit und Reproduzierbarkeit ein einer Diagnose.
#' Die Beurteikung kann dabei durch einen Bewerter (Messverfahren) in wiederholter Form erfolgen
#' und wird dan als Intra-Raterbezeichnet oder die Beurteilung eines Merkmals erfolgt durch mehere Bewerter (Messverfahren).
#' und hier spricht man von Inter-Rater.
#'
#' Die Methode der Burteilung der Uebereinstimmung haengt vom jeweiligen Datentype ab.
#' @param x  bland_altman-Objekt , par=TRUE
#' @param main   default =  name.diff
#' @param main1  default regression
#' @param main2  default differences
#' @param main3  default precentage
#' @param ylab1  default met_B
#' @param ylab2  default Differences
#' @param ylab3  default Differences/Average
#' @param xlab1  default met_A
#' @param xlab2  default  name
#' @param xlab3  default x$name
#' @param type  default =2
#' @param pch  default = 19
#' @param col farbe
#' @param x.var   met_A
#' @param y.var   met_B
#' @param pos  topleft
#' @param legend  levels groups
#' @param fil weiss nicht
#' @param abline.col Farbe c("black","black","black"),
#' @param abline.lty  Lineie abline.lty=c(3,1,3),
#' @param abline.lwd  Strischstaerke abline.lwd=c(1,1,1)
#' @return Ein bland_altman-Objekt mit den Daten (data) und der Statistik (stat).
#' @export
#' @examples
#' #library(stp25)
#' #library(tidyr)
#' #graphics.off()
#' #setwd("C:/Users/wpete/Dropbox/1_Projekte/000 Temp")
#' #Projekt("html", "bland_altman")
#' #- Understanding Bland Altman analysis
#' #Davide Giavarina
#' #Biochemia medica 2015;25(2) 141-51
#' #http://dx.doi.org/10.11613/BM.2015.015
#'
#' set.seed(0815)
#' DF<- data.frame(
#'   A=c(1, 5,10,20,50,40,50,60,70,80, 90,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900, 950,1000),
#'   B=c(8,16,30,14,39,54,40,68,72,62,122, 80,181,259,275,380,320,434,479,587,626,648,738,766,793,851,871,957,1001, 980),
#'   group= sample(gl(2, 15, labels = c("Control", "Treat")))
#' )
#'
#' MetComp2(~A+B, DF, caption = "Giavarina")
#'
#' #Sachs Angewandte Statistik Seite 627
#'
#' DF2<- data.frame(A= factor(c(rep(1, 14), rep(1, 3),
#'                              rep(0, 5),rep(0, 18)),1:0, c("+", "-")),
#'                  B= factor(c(rep(1, 14), rep(0, 3),
#'                              rep(1, 5),rep(0, 18)),1:0, c("+", "-")) )
#'
#'
#' APA2(xtabs(~A+B, DF2), test=T)
#' MetComp2(~A+B, DF2)
#'
#'
#' DF <- transform(DF, C = round( A + rnorm(30,0,20)),
#'                 D = round( A + rnorm(30,0,10) + A/10 ),
#'                 E = round( A + rnorm(30,5,10) + (100-A/10) ))
#'
#' head(DF)
#'  xt <-xtabs(~A+B, DF2)
#'  Klassifikation2(xt)
#'
#' x<- BlandAltman(~A+E , DF)
#' x$groups
#' x %>% Output( )
#' ICC2(~A+E , DF)
#' windows(8,3)
#' plot(x)
#' SaveData()
#' x<- BlandAltman(A+E+B~group, DF)
#'
#' windows(8,3.2)
#' plot(x)
#'
#' n<-1000
#' DF<- data.frame(
#'   A=rnorm(n, 100,50),
#'   B=rnorm(n, 100,50),
#'   C=NA,  D=NA,  E=NA,
#'   group= sample(gl(2, n/2, labels = c("Control", "Treat")))
#' )
#' DF <- transform(DF, C = round( A + rnorm(n,0,20)),
#'                 D = round( A + rnorm(n,0,10) + A/10 ),
#'                 E = round( A + rnorm(n,5,10) + (100-A/10) ))
#'
#' x<- BlandAltman(A+E~group, DF)
#'
#' windows(8,3.2)
#' plot(x)
#'
#'
#' #library(stp25)
#' #library(tidyr)
#' #graphics.off()
#' #setwd("C:/Users/wpete/Dropbox/1_Projekte/000 Temp")
#' #Projekt("html", "bland_altman")
#' #- Understanding Bland Altman analysis
#' #Davide Giavarina
#' #Biochemia medica 2015;25(2) 141-51
#' #http://dx.doi.org/10.11613/BM.2015.015
#'
#' set.seed(0815)
#'
#' n<-100
#' DF<- data.frame(
#'   A=rnorm(n, 100,50),
#'   B=rnorm(n, 100,50),
#'   C=NA,  D=NA,  E=NA,
#'   group= sample(gl(2, n/2, labels = c("Control", "Treat")))
#' )
#'
#' cutA<-mean(DF$A)
#' DF <- transform(DF, C = round( A + rnorm(n, -5, 20)),
#'                 D = round( A + rnorm(n,0,10) + A/10 ),
#'                 #E = round( A + rnorm(n,5,10) + (100-A/10) )
#'                 E = A + ifelse(A<cutA, A/5, -A/5 )+ rnorm(n, 0, 10)
#' )
#'
#'
#' x<- BlandAltman(~A+C, DF)
#' windows(8,3.2)
#' plot(x)
#' SaveData(caption="A und C Messen das gleiche mit SD=20")
#'
#' x<- BlandAltman(~A+B, DF)
#' windows(8,3.2)
#' plot(x)
#' SaveData(caption="A und B Messen unterschiedliche Parameter")
#'
#'
#' x<- BlandAltman(~A+D, DF)
#' windows(8,3.2)
#' plot(x)
#' SaveData(caption="A und D Messen das unterschiedlich D hat im unteren
#'          Wertevereich deutlich geringere Werte")
#' x<- BlandAltman(~A+E, DF)
#' windows(8,3.2)
#' plot(x)
#' SaveData(caption="A und E Messen das unterschiedlich es esistiert ein knik im Wertebereich 100")
#'
#' End()
plot.bland_altman <- function(x,
                              par = TRUE,
                              main = x$name.diff,
                              main1 = "regression",
                              main2 = "differences",
                              main3 = "precentages",

                              ylab1 = x$met_B,
                              ylab2 = "Differences",
                              ylab3 = "Differences/Average [%]",

                              xlab1 = x$met_A,
                              xlab2 = paste0("Means (" , x$name, ")"),
                              xlab3 = paste0("Means (" , x$name, ")"),

                              type = 2,
                              pch = 19,
                              col = if (is.null(x$groups))
                                1
                              else
                                3 + as.numeric(x$groups[[1]]),
                              x.var = x$met_A,
                              y.var = x$met_B,
                              pos = "topleft",
                              legend = levels(x$groups[[1]]),
                              fil = 3 + (1:nlevels(x$groups[[1]])),
                              abline.col = c("black", "black", "black"),
                              abline.lty = c(3, 1, 3),
                              abline.lwd = c(1, 1, 1),
                              ...) {
  # print(x$lines) # print(x$CI.lines) # print(x$lines.percent) # print(x$CI.lines.percent)

  if (par)
    par(mfrow = c(1, 3), oma = c(0.4, 1, 1.6, 1))

  plot(
    x$data[, x.var],
    x$data[, y.var],
    xlab = xlab1,
    ylab = ylab1,
    main = main1,
    pch = pch,
    col = col,
    bty = 'l'
  )
  abline(lm(x$data[, y.var] ~ x$data[, x.var]),
         lty = 1,
         lwd = 1,
         col = abline.col[2])
  if (!is.null(x$groups))
    legend(
      x = pos,
      legend = legend,
      fill = fil,
      bty = "n"
    )
  title(main = main, outer = TRUE)

  # plot(x$data$means, x$data$diffs,
  #      ylim= range(c(x$data$diffs, x$lines), finite = TRUE),
  #      xlab=xlab2, ylab=ylab2, main=main2, pch=pch, col=col )
  #     abline(h = x$lines, lty=abline.lty, col=abline.col, lwd=abline.lwd)
  #     abline(h = x$CI.lines)
  rgn<-range(x$data$means, finite = TRUE) * c(1, 1.15)
  plot(
    x$data$means,
    x$data$diffs,
    type = "n",
    axes = FALSE,
    xlim = rgn,
    ylim = range(c(x$data$diffs, x$lines), finite = TRUE),
    xlab = xlab2,
    ylab = ylab2,
    main = main2
  )
  lim <- par("usr")
  rect(lim[1],
       x$CI.lines[3],
       lim[2],
       x$CI.lines[4],
       border = "gray80",
       col = "gray95")
  abline(
    h = x$lines,
    lty = abline.lty,
    col = abline.col,
    lwd = abline.lwd
  )
  points(x$data$means, x$data$diffs, pch = pch, col = col)

  text(x=c(rgn[2],rgn[2]),
       y=x$lines ,
       c("-1.96 SD", "Mean", "+1.96 SD"), adj=c(1,0), cex=.7)
  text(x=c(rgn[2],rgn[2]),
       y=x$lines,
       signif(x$lines, 2)  , adj=c(1,1), cex=.7)

  axis(1) ## add axes back
  axis(2)
  box()



  if (!is.null(x$groups))
    legend(
      x = pos,
      legend = legend,
      fill = fil,
      bty = "n"
    )
  #- Prozent ---------------------------------
  ## Fehler wenn Inf
  #range(x$data$means )

  plot(
    x$data$means,
    x$data$diffs.percent,
    type = "n",
    axes = FALSE,
    xlim = rgn,
    ylim = range(c(x$lines.percent,x$data$diffs.percent), finite = TRUE),
    ##x$data$diffs.percent,
    xlab = xlab3,
    ylab = ylab3,
    main = main3
  )


  lim <- par("usr")
  rect(
    lim[1],
    x$CI.lines.percent[3],
    lim[2],
    x$CI.lines.percent[4],
    border = "gray80",
    col = "gray95"
  )
  abline(
    h = x$lines.percent,
    lty = abline.lty,
    col = abline.col,
    lwd = abline.lwd
  )
  points(x$data$means,
         x$data$diffs.percent,
         pch = pch,
         col = col)
  text(x=c(rgn[2],rgn[2]),
       y=x$lines.percent ,
       c("-1.96 SD", "Mean", "+1.96 SD"), adj=c(1,0), cex=.7)
  text(x=c(rgn[2],rgn[2]),
       y=x$lines.percent,
       signif(x$lines.percent, 2)  , adj=c(1,1), cex=.7)

  axis(1) ## add axes back
  axis(2)
  box()
  if (!is.null(x$groups))
    legend(
      x = pos,
      legend = legend,
      fill = fil,
      bty = "n"
    )
}
