% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/6_bland_altman_plot.R
\name{plot.bland_altman}
\alias{plot.bland_altman}
\title{Uebereinstimmung und Praezision von Messwerten}
\usage{
\method{plot}{bland_altman}(
  x,
  par = TRUE,
  main = x$name.diff,
  main1 = "regression",
  main2 = "differences",
  main3 = "precentages",
  ylab1 = x$met_B,
  ylab2 = "Differences",
  ylab3 = "Differences/Average [\%]",
  xlab1 = x$met_A,
  xlab2 = paste0("Means (", x$name, ")"),
  xlab3 = paste0("Means (", x$name, ")"),
  type = 2,
  pch = 19,
  col = if (is.null(x$groups)) 1 else 3 + as.numeric(x$groups[[1]]),
  x.var = x$met_A,
  y.var = x$met_B,
  pos = "topleft",
  legend = levels(x$groups[[1]]),
  fil = 3 + (1:nlevels(x$groups[[1]])),
  abline.col = c("black", "black", "black"),
  abline.lty = c(3, 1, 3),
  abline.lwd = c(1, 1, 1),
  ...
)
}
\arguments{
\item{x}{bland_altman-Objekt , par=TRUE}

\item{par}{par}

\item{main}{default =  name.diff}

\item{main1}{default regression}

\item{main2}{default differences}

\item{main3}{default precentage}

\item{ylab1}{default met_B}

\item{ylab2}{default Differences}

\item{ylab3}{default Differences/Average}

\item{xlab1}{default met_A}

\item{xlab2}{default  name}

\item{xlab3}{default x$name}

\item{type}{default =2}

\item{pch}{default = 19}

\item{col}{farbe}

\item{x.var}{met_A}

\item{y.var}{met_B}

\item{pos}{topleft}

\item{legend}{levels groups}

\item{fil}{weiss nicht}

\item{abline.col}{Farbe c("black","black","black"),}

\item{abline.lty}{Lineie abline.lty=c(3,1,3),}

\item{abline.lwd}{Strischstaerke abline.lwd=c(1,1,1)}

\item{...}{nicht Benutzt}
}
\value{
Ein bland_altman-Objekt mit den Daten (data) und der Statistik (stat).
}
\description{
Tukey Mean Difference oder auch Bland Altman Metode. Oft iteressiert
die Zuverlässigkeit und Reproduzierbarkeit ein einer Diagnose.
Die Beurteikung kann dabei durch einen Bewerter (Messverfahren) in wiederholter Form erfolgen
und wird dan als Intra-Raterbezeichnet oder die Beurteilung eines Merkmals erfolgt durch mehere Bewerter (Messverfahren).
und hier spricht man von Inter-Rater.
}
\details{
Die Methode der Burteilung der Uebereinstimmung haengt vom jeweiligen Datentype ab.
}
\examples{

library(stp25plot)
library(stpvers)
#library(tidyr)
#graphics.off()
#setwd("C:/Users/wpete/Dropbox/1_Projekte/000 Temp")
#Projekt("html", "bland_altman")
#- Understanding Bland Altman analysis
#Davide Giavarina
#Biochemia medica 2015;25(2) 141-51
#http://dx.doi.org/10.11613/BM.2015.015

set.seed(0815)
DF<- data.frame(
  A=c(1, 5,10,20,50,40,50,60,70,80, 90,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900, 950,1000),
  B=c(8,16,30,14,39,54,40,68,72,62,122, 80,181,259,275,380,320,434,479,587,626,648,738,766,793,851,871,957,1001, 980),
  group= sample(gl(2, 15, labels = c("Control", "Treat")))
)

MetComp(~A+B, DF, caption = "Giavarina")

#Sachs Angewandte Statistik Seite 627

DF2<- data.frame(A= factor(c(rep(1, 14), rep(1, 3),
                             rep(0, 5),rep(0, 18)),1:0, c("+", "-")),
                 B= factor(c(rep(1, 14), rep(0, 3),
                             rep(1, 5),rep(0, 18)),1:0, c("+", "-")) )


APA2(xtabs(~A+B, DF2), test=T)
MetComp(~A+B, DF2)


DF <- transform(DF, C = round( A + rnorm(30,0,20)),
                D = round( A + rnorm(30,0,10) + A/10 ),
                E = round( A + rnorm(30,5,10) + (100-A/10) ))

head(DF)
xt <-xtabs(~A+B, DF2)
Klassifikation2(xt)

x<- MetComp_BAP(~A+E , DF)

ICC2(~A+E , DF)
windows(8,3)
plot(x)
SaveData()
x<- MetComp_BAP(A+E+B~group, DF)

windows(8,3.2)
plot(x)

n<-1000
DF<- data.frame(
  A=rnorm(n, 100,50),
  B=rnorm(n, 100,50),
  C=NA,  D=NA,  E=NA,
  group= sample(gl(2, n/2, labels = c("Control", "Treat")))
)
DF <- transform(DF, C = round( A + rnorm(n,0,20)),
                D = round( A + rnorm(n,0,10) + A/10 ),
                E = round( A + rnorm(n,5,10) + (100-A/10) ))

x<- MetComp_BAP(A+E~group, DF)

windows(8,3.2)
plot(x)


#library(stp25)
#library(tidyr)
#graphics.off()
#setwd("C:/Users/wpete/Dropbox/1_Projekte/000 Temp")
#Projekt("html", "bland_altman")
#- Understanding Bland Altman analysis
#Davide Giavarina
#Biochemia medica 2015;25(2) 141-51
#http://dx.doi.org/10.11613/BM.2015.015

set.seed(0815)

n<-100
DF<- data.frame(
  A=rnorm(n, 100,50),
  B=rnorm(n, 100,50),
  C=NA,  D=NA,  E=NA,
  group= sample(gl(2, n/2, labels = c("Control", "Treat")))
)

cutA<-mean(DF$A)
DF <- transform(DF, C = round( A + rnorm(n, -5, 20)),
                D = round( A + rnorm(n,0,10) + A/10 ),
                #E = round( A + rnorm(n,5,10) + (100-A/10) )
                E = A + ifelse(A<cutA, A/5, -A/5 )+ rnorm(n, 0, 10)
)


x<- MetComp_BAP(~A+C, DF)
windows(8,3.2)
plot(x)
SaveData(caption="A und C Messen das gleiche mit SD=20")

x<- MetComp_BAP(~A+B, DF)
windows(8,3.2)
plot(x)
SaveData(caption="A und B Messen unterschiedliche Parameter")


x<- MetComp_BAP(~A+D, DF)
windows(8,3.2)
plot(x)
SaveData(caption="A und D Messen das unterschiedlich D hat im unteren
         Wertevereich deutlich geringere Werte")
x<- MetComp_BAP(~A+E, DF)
windows(8,3.2)
plot(x)
SaveData(caption="A und E Messen das unterschiedlich es esistiert ein knik im Wertebereich 100")

End()
}
