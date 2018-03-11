
kano_plot_del <-  function(...){
  UseMethod("kano_plot_del")
}



kano_plot_del.formula <- function(formula,
                              data,
                              type=1,
                              prop.table=TRUE, # nicht fuer Kano dondern fuer Barplot
                              ...){

  kano_plot_del_bar<- function(x,
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
  if(type==1 | type=="dot") kano_plot_del(ans,
                                      groups=if(ncol(ans) == 17) NULL else names(ans)[2],
                                      ...)
  else kano_plot_del_bar(ans,
                     groups=if(ncol(ans) == 17) NULL else names(ans)[2],
                     prop.table=prop.table,
                     ...)

}


kano_plot_del.data.frame <- function(data, main = "",
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




