#' @name kano_plot
#' @rdname kano_plot
#' @title Analyze Kano type items.
#' @description Transformiert Kano-Fragebogen zu Kano-Kodierung \link{http://www.eric-klopp.de/texte/angewandte-psychologie/18-die-kano-methode}
#'
#' M O A I R Q Hoeufigkeit
#'
#' max Category
#'
#' M>O>A>I  max Category mit Hirarchie M Wichtiger als O usw.
#' also wen der Unterschied zwischen den zwei am hoechsten gelisteten Atributen
#' zwei Kategorien gleich ist,  5% Schwelle, dann gilt die Regel M>O>A>I
#'
#' Total Strength als zweite Ma?zahl gibt an wie hoch der Anteil an bedeutenden Produktmerkmalen ist.
#'
#' Category Strength ist eine Ma?zahl die die angibt ob eine Anforderung nur in eine Kategorie gehoert
#'
#' CS plus	Indx Positiv  CS.plus=  (A+O)/(A+O+M+I)
#'
#' CS minus	Index Negativ CS.minus= (O+M)/(A+O+M+I)
#'
#' Chi-Test	Eigendlich unsinn Testet ob Verteilung von M, a, O und I gleich ist
#'
#' Fong-Test Vergleich der zwei Haeufigsten-Kategorien gegenueber der Gesammtzahl ergibt entweder sig oder nicht sig
#'
#'
#'  \subsection{Basis-Faktoren (Mussfaktoren)}{
#'  Basis-Merkmale (M...Mustbe) werden vom Kunden Vorausgesetzt schaffen unzufriedenheit wenn sie nicht vorhanden sind.
#'  }
#'   \subsection{Leistungs- Faktoren}{
#'  Leistungs-Merkmale (O...One???dimensional) werden vom Kunden verlangt
#'  }
#'  \subsection{Begeisterung-Faktoren}{
#'  Begeisterungs-Merkmale (A...Attractive) Kunde rechnet nicht damit hebt das produkt vom Konkurenten ab.
#'  }
#'  \subsection{Unerhebliche- Faktoren }{
#'  Unerhebliche-Merkmale (I...Indifferent) werden vom Kunden Ignoriert.
#'  }
#' @param X data.frame mit parweisen Kano Fragen
#' @param grouping Dataframe mit Gruppen
#' @param type Fragetype entwerer vollstaendig (5) oder gekuerzt (3)
#' @param umcodieren logical False
#' @param rm_Q Remove Q Kategorien Q enfernen Anzahl an erlaubten Qs
#' @param rm_I Remove I Kategorien I enfernen Anzahl an erlaubten Is
#' @param methode eie sind die Items geornet
#' @param vars_func Welche Items sind die Funktionalen
#' @param vars_dysfunc Welche Items sind die Dys-Funktionalen
#' @param ...
#' \itemize{
#'    \item \code{type=c(1,2)}  Streudiagram (1) oder Balken (2)
#'    \item \code{groups= NULL} wenn ein Dataframe uebergeben dan sind das die Gruppen
#'    \item \code{legend.position = list(x="right", y=NULL) } - Beim Streudiagram die Legende bei Barplot \code{scale=list()}
#'    \item \code{txt.bg=list(m="M",i= "I", o="O", a="A")} Beim Streudiagram der Hintergrundtext
#'    \item \code{jitter=TRUE} Beim Streudiagram Verrauschen der Items (wegen besserer lesbarkeit)
#'    \item \code{col.bg="gray95"}
#'    \item \code{col = NA}
#'    \item \code{cex=0.70}
#'    \item \code{cex.items=cex*1}
#'
#'    \item \code{cex.lab=cex*1.07}
#'    \item \code{cex.legend=cex*1.1}
#'    \item \code{ylab="Zufriedenheitsstiftung (CS+)"}
#'    \item \code{xlab="Unzufriedenheitsstiftung (CS-)"}
#'    \item \code{CS.minus = "CS minus"}
#'    \item \code{center.axis=FALSE}
#'    \item \code{use.labels=FALSE  Zahlen oder Text als Beschriftung bei den Streudiagrammen}
#'    \item \code{use.total.strength =TRUE  groese der Schrift als Indikator fuer Total Strenght}
#'    \item \code{use.categorie=TRUE  M, O, A, oder i mit in die Labels ausgeben }
#'
#' }
#' @return data.frame mit der Kano-Kodierung
#' @export
#' @examples
#' library(stp5)
#'  Start("")
#'  head(DF<-GetData("   Geschlecht f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18
#'  1           w  1  1  1  2  1  3  1  4  1   5   5   1   4   1   5   2   5   1
#'  2           w  2  1  2  2  2  3  2  4  2   5   2   5   1   3   2   5   2   5
#'  3           m  3  1  3  2  3  3  3  4  3   5   5   1   4   1   5   2   5   1
#'  4           m  4  1  4  2  4  3  4  4  4   5   5   1   4   1   5   2   5   1
#'  5           w  5  1  5  2  5  3  5  4  5   5   5   1   4   1   5   2   5   1
#'  6           w  NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#'  7           m  2  5  1  5  2  5  1  5  1   5   2   5   1   5   1   5   2   5
#'  8           w  2  4  2  5  1  3  1  3  2   5   3   3   1   3   1   4   1   3
#'  9           m  2  4  2  5  2  3  1  3  2   5   1   3   1   3   2   4   3   3
#'  10          m  2  5  1  5  1  4  1  5  2   5   1   4   1   5   2   5   1   3
#'  11          w  1  5  2  5  1  4  1  4  1   5   1   4   2   5   2   5   1   4
#'  12          m  2  5  2  5  2  5  2  4  1   5   1   3   1   4   2   5   3   3
#'  13          w  2  5  2  5  3  3  1  5  2   5   1   5   1   5   3   3   3   3
#'  14          m  2  5  1  5  1  5  2  5  2  NA   1   5   1   5   2   5   1   5
#'  15          w  1  4  2  5  1  3  2  5  1   5   1   3   1   4   2   5   1   3
#'  16          w  1  4  2  5  2  5  2  5  1   5   1   4   2   5   2   5   1   3
#'  17          w  1  5  2  5  1  5  1  5  2   5   1   4   1   5   2   5   1   4
#'  18          w  1  5  2  5  2  5  2  5  1   5   1   2   1   5   1   5   3   2
#'  19          m  1  5  2  5  2  5  2  5  2   5   1   4   2   5   2   5   1   3
#'  20          w  2  5  2  5  2  5  2  5  2   5   1   3   2   5   2   5   1   3
#'  21          w  2  5  2  5  1  5  2  5  2   5   1   5   2   5   2   5   1   3
#'  22          m  1  3  2  5  1  3  2  5  2   5   1   3   2   5   1   3   1   3
#'  23          w  1  5  2  5  3  3  2  5  2   5   1   4   2   5   2   5   1   4
#'  24          w  2  4  2  5  2  3  2  4  2   5   3   3   2   4   2   4   1   3
#'  25          m  2  4  1  5  1  4  2  4  1   5   1   3   3   3   2   4   1   3
#'  26          w  1  5  1  5  1  3  1  5  1   5   1   3   1   5   1   5   3   3
#'  27          w  1  5  2  5  3  3  1  4  2   4   1   3   1   3   3   3   5   1
#'  28          w  2  5  2  5  1  4  2  5  1   5   1   3   2   4   2   5   4   1
#'  29          w  2  5  2  5  2  4  2  4  2   5   1   4   2   4   1   5   1   4
#'  30          m  1  5  2  5  1  3  1  3  1   4   1   3   1   3   1   3   1   3
#'  31          m  1  3  2  5  1  4  1  4  3   3   5   2   2   4  NA  NA   1   3
#'  "))
#'
#'  DF<- upData(DF,  labels=c(f1="f1.motiviert"
#'                            ,f3="f2.anpasst"
#'                            ,f5="f3.programm"
#'                            ,f7="f4.info"
#'                            ,f9="f5.text"
#'                            ,f11="f6.reagieren"
#'                            ,f13="f7.text"
#'                            ,f15="f8.Arzt"
#'                            ,f17="f9.Telefonanruf"
#'  ))
#'
#'#  match(DF$f1, 1) & match(DF$f2, 5)
#'
#'
#'  kano_res <- KANO2(DF[, -1], grouping = DF[1],
#'        # type = 5,
#'        # umcodieren = FALSE,
#'        # rm_Q = 999,
#'        # rm_I = 999
#'  )
#'
#'
#'  kano_res <- KANO2( .~ Geschlecht, DF)
#'
#'
#'  names(kano_res)
#'  #"value"      "scors"      "data"       "molten"     "formula"  "removed"    "Attributes" "answers"
#'
#'  APA2(kano_res, caption = "Ueber Objekt")
#'  APA2(value ~ variable, kano_res, caption = "Ueber formula")
#'  APA2(value ~ variable + Geschlecht , kano_res, caption = "Ueber formula +Gechlecht")
#'
#' #Kano_Auswertung( kano_res$molten, kano_res$formula)
#'
#'
#'
#'  kano_plot(value ~ variable + Geschlecht, kano_res,
#'      main = "kano-analysis",
#'      ylab = "Prozent",
#'      type = 2, col = 2:7,
#'      prop.table = TRUE)
#'
#'  kano_plot(value ~ variable + Geschlecht, kano_res,
#'      main = "kano-analysis",
#'      center.axis = TRUE)
#'
#' #   windows(8, 8)
#  #kano_plot(value ~ variable, kano_res, main = main, ylab = "CS+", xlab = "CS-")  #   center.axis = F,
#   #SaveData(paste0("txt ",plot_name))
#   #windows(8, 8)
#   #kano_plot(value ~ variable, kano_res, main = main, use.labels = FALSE, ylab = "CS+", xlab = "CS-")
#   #SaveData(paste0("nr ",plot_name))
#'
#'  End()
#'
kano_plot <-  function(...){
  UseMethod("kano_plot")
}


#' @rdname kano_plot
#' @export
kano_plot.formula <- function(formula,
                              data,
                              type=1,
                              prop.table=TRUE, # nicht fuer Kano dondern fuer Barplot
                              ...){

  kano_plot_bar<- function(x,
                           groups,
                           auto.key=list(space="right"),
                           prop.table=TRUE,
                           ylab=  if(prop.table) "percent" else "count",
                           ...,
                           col=brewer.pal(6,"Dark2")[c(4,1,2,3,5,6)]){

    datatab<- if(prop.table)
      melt(cbind(x[1], dummy="", sapply(x[, Cs(M,O,A,I,R,Q)],
                                        function(a)  as.numeric(as.character(a))/as.numeric(as.character(x$Total))*100)))
    else  melt(cbind(x[1], dummy="", sapply(x[, Cs(M,O,A,I,R,Q)],
                                            function(a)  as.numeric(as.character(a)) )))

    cols<-list(superpose.polygon=list(col= col))      #
    #
    if(is.null(groups)){  formulabar<-formula(paste("value~dummy|", names(x)[1]))
    }else {
      formulabar<-formula(paste("value~dummy|",names(x)[2], "+", names(x)[1]))
      datatab<-cbind( datatab, x[groups])

    }
    # print(formulabar)
    # print(datatab)
    # print(x)
    p2<-barchart( formulabar,
                  data=datatab,
                  groups=variable, #  horizontal=FALSE, stack = TRUE,
                  origin = 0,
                  auto.key=auto.key,
                  par.settings = cols,
                  ylab=ylab,
                  ...
                  # layout=c(2,nrow(allx))
    )
    # print(p2)

    if(is.null(groups))print(p2)
    else  print(useOuterStrips(p2))



  }




  ans <- Kano_Auswertung(data$molten, formula, prop.table=FALSE)
  if(type==1 | type=="dot") kano_plot(ans,
                                      groups=if(ncol(ans) == 17) NULL else names(ans)[2],
                                      ...)
  else kano_plot_bar(ans,
                     groups=if(ncol(ans) == 17) NULL else names(ans)[2],
                     prop.table=prop.table,
                     ...)

}

#' @rdname kano_plot
#' @export
kano_plot.data.frame <- function(data, main = "",
                                 groups = NULL,
                                 xlim = c(0, 1),
                                 ylim = c(0, 1), mar = c(0, 1, 2, 1),
                                 legend.position = list(x = "right", y = NULL),
                                 my.lines = "circle",
                                 col.bg = "gray95", col = NA,
                                 txt.bg = list(m = "M", i = "I", o = "O", a = "A"),
                                 cex.bg = 12, jitter = TRUE, cex = 1,
                                 cex.items = cex * 1, cex.lab = cex * 1.07,
                                 cex.legend = cex * 1.1,
                                 ylab = "Zufriedenheitsstiftung (CS+)",
                                 xlab = "Unzufriedenheitsstiftung (CS-)",
                                 CS.plus = "CS plus", CS.minus = "CS minus", Total.Strength = "Total Strength", M.O.A.I = 11, variable = "Item",
                                 center.axis = FALSE,
                                 use.labels = TRUE,
                                 use.total.strength = TRUE,
                                 use.categorie = TRUE,
                                 use.points = FALSE,
                                 ...) {

  # circle_plot <- function() symbols(rep(0, 5), rep(0, 5), circles =
  # (c(4,6,8,10,12)/10), add = TRUE, inches = FALSE, lwd=2, fg = Cs(gray75, gray80,
  # gray85,gray90,gray95))
  circle_plot <- function() symbols(0, 0, circles = 0.4, add = TRUE, inches = FALSE,
                                    lwd = 2, fg = "gray85")
  par(mar = mar)
  if (ncol(data) == 0) {
    # Leeres Blatt
    plot(1, 1, pch = "", xlim = xlim, ylim = ylim, ann = FALSE, axes = FALSE,
         frame.plot = FALSE)

    mtext(main, cex = 1.5)
    # -- Hintergrung
    text(0.75, 0.25, txt.bg$m, cex = cex.bg, col = "gray95")
    text(0.25, 0.25, txt.bg$i, cex = cex.bg, col = "gray95")
    text(0.75, 0.75, txt.bg$o, cex = cex.bg, col = "gray95")
    text(0.25, 0.75, txt.bg$a, cex = cex.bg, col = "gray95")

    # grafische Hilfslienien
    if (!is.na(my.lines)) {
      if (my.lines == "circle") {
        circle_plot()
      } else {
        x1 <- 1:10000
        xx <- abs(sin(x1)/2)
        yy <- abs(cos(x1)/2)
        points(xx, yy, pch = ".", type = "p", col = "gray90", cex = 4)
        xx <- x1/10000 + 0.5
        yy <- sqrt(x1)/100

        points(xx, yy, pch = ".", type = "p", col = "gray90", cex = 4)
        points(yy, xx, pch = ".", type = "p", col = "gray90", cex = 4)
      }
    }

    if (center.axis) {
      arrows(0.5, 1, 0.5, 0.01)
      text(-0.01, 0.5, ylab, adj = c(NA, 0), pos = NULL, offset = 0.5, vfont = NULL,
           cex = cex.lab, srt = 90)
      arrows(0.01, 0.5, 1, 0.5)  #arrows(0,0.5,  1,0.5)
      text(0.5, -0.01, xlab, adj = c(NA, 1), pos = NULL, offset = 0.5, vfont = NULL,
           cex = cex.lab)
    } else {
      arrows(0, 0.01, 0, 1)
      text(-0.01, 0.5, ylab, adj = c(NA, 0), pos = NULL, offset = 0.5, vfont = NULL,
           cex = cex.lab, srt = 90)
      arrows(0.01, 0, 1, 0)  #arrows(0,0.5,  1,0.5)
      text(0.5, -0.01, xlab, adj = c(NA, 1), pos = NULL, offset = 0.5, vfont = NULL,
           cex = cex.lab)  #  text(0.5, 0.49, 'Unzufriedenheitsstiftung (CS-)',adj=c(NA,1),cex=.75 )
    }
  } else {
    sadisfaction <- as.numeric(as.character(data[, CS.plus]))
    dissadisfaction <- as.numeric(as.character(data[, CS.minus])) * -1
    if (jitter) {
      set.seed(815)
      dissadisfaction <- jitter(dissadisfaction)
      sadisfaction <- jitter(sadisfaction)
    }
    mylevels <- NULL
    Col <- if (is.null(groups)) {
      4
    } else {
      gr <- factor(data[, groups])
      n <- nlevels(gr)
      mylevels <- levels(gr)
      mycolors <- if (is.na(col[1]))
        brewer.pal(ifelse(n < 4, 3, n), "Dark2")[1:n] else col
      gr <- as.character(factor(gr, mylevels, mycolors[1:n]))
    }

    Labels <- data[, variable]

    plot(dissadisfaction, sadisfaction, pch = "", xlim = xlim, ylim = ylim, ann = FALSE,
         axes = FALSE, frame.plot = FALSE)

    mtext(main, cex = 1.5)

    # -- Hintergrung
    text(0.75, 0.25, txt.bg$m, cex = cex.bg, col = "gray95")
    text(0.25, 0.25, txt.bg$i, cex = cex.bg, col = "gray95")
    text(0.75, 0.75, txt.bg$o, cex = cex.bg, col = "gray95")
    text(0.25, 0.75, txt.bg$a, cex = cex.bg, col = "gray95")
    # grafische Hilfslienien
    if (!is.na(my.lines)) {

      if (my.lines == "circle") {
        circle_plot()
      } else {
        x1 <- 1:10000
        xx <- abs(sin(x1)/2)
        yy <- abs(cos(x1)/2)
        points(xx, yy, pch = ".", type = "p", col = "gray90", cex = 4)
        xx <- x1/10000 + 0.5
        yy <- sqrt(x1)/100

        points(xx, yy, pch = ".", type = "p", col = "gray90", cex = 4)
        points(yy, xx, pch = ".", type = "p", col = "gray90", cex = 4)
      }
    }

    if (!is.null(mylevels))
      legend(x = legend.position$x, y = legend.position$y, mylevels, col = mycolors,
             pch = 16, box.lty = 3, box.col = "gray50", cex = cex.legend, title = groups)

    # x und y Achse mit Beschriftung
    if (center.axis) {
      arrows(0.5, 1, 0.5, 0.01)
      text(-0.01, 0.5, ylab, adj = c(NA, 0), pos = NULL, offset = 0.5, vfont = NULL,
           cex = cex.lab, srt = 90)

      arrows(0.01, 0.5, 1, 0.5)  #arrows(0,0.5,  1,0.5)
      text(0.5, -0.01, xlab, adj = c(NA, 1), pos = NULL, offset = 0.5, vfont = NULL,
           cex = cex.lab)
    } else {
      arrows(0, 0.01, 0, 1)
      text(-0.01, 0.5, ylab, adj = c(NA, 0), pos = NULL, offset = 0.5, vfont = NULL,
           cex = cex.lab, srt = 90)
      arrows(0.01, 0, 1, 0)  #arrows(0,0.5,  1,0.5)
      text(0.5, -0.01, xlab, adj = c(NA, 1), pos = NULL, offset = 0.5, vfont = NULL,
           cex = cex.lab)  #  text(0.5, 0.49, 'Unzufriedenheitsstiftung (CS-)',adj=c(NA,1),cex=.75 )
    }


    cexy <- as.numeric(gsub("%", "", as.character(data[, Total.Strength])))  # Total Strength Category Strength
    cexy <- cexy - min(cexy)
    if (use.total.strength)
      cex.items <- (cexy/max(cexy)/2 + 0.5) * cex.items  ## use.total.strength =TRUE,
    if (!use.labels) {
      saveLabels <- Labels

      Labels <- as.numeric(factor(Labels))
      legend(x = 1, y = 0, xjust = 1, yjust = 0, paste(Labels, factor(saveLabels)),
             pch = NULL, box.lty = 0, box.col = NULL, cex = cex.legend * 0.7,
             title = NULL)
    }
    if (use.points) {
      symbols(dissadisfaction, sadisfaction, circles = cex.items/30, add = TRUE,
              inches = FALSE, lwd = 1, fg = "gray60", bg = "gray90")
    }

    # color <- ifelse(data$Total.Strengt <60, 'gray40', 'black' )
    if (use.categorie)
      Labels <- paste0(Labels, " (", data[, M.O.A.I], ")")  #  =TRUE,
    text(dissadisfaction, sadisfaction, Labels, cex = cex.items, col = Col)
  }
}




