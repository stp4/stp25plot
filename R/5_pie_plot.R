#' @name pie_plot
#' @rdname pie_plot
#' @title pie_plot
#' @description Standardgrafiken
#' @param formula Objekt
#' @param data Data.frame
#' @param main Ueberschrift
#' @return NULL
#' @examples
#' set.seed(2)
#' n<-20*3*2
#' DF<- upData(data.frame(n=runif(n, min = 1, max = 5),
#'                        e=runif(n, min = 1, max = 5),
#'                       o=runif(n, min = 1, max = 5),
#'                        g=runif(n, min = 1, max = 5),
#'                        a=runif(n, min = 1, max = 5),
#'                        treatment=gl(3, n/3, labels = c("UG1", "UG2", "KG"))[sample.int(n)],
#'                        sex=gl(2, n/2, labels = c("male", "female")
#'                        )
#' ), labels=c(  n="Neurotizismus",
#'               e="Extraversion",
#'               o="Offenheit fuer Erfahrungen",
#'               g= "Gewissenhaftigkeit",
#'               a="Vertraeglichkeit") )
#' DF[1,1:3] <-5;DF[1,4:5] <-1; DF[2:10,1] <-4.5
#' DF[n,4:5] <-5;DF[n,1:5] <-1
#' APA2(~treatment, DF)
#' windows(8,8)
#' pie_plot(~treatment, DF)
#' windows(8,8)
#' pie_plot(~treatment, DF)
#' windows(8,8)
#' Balken(~treatment, DF)
#' windows(8,8)
#' Balken(~treatment, DF, type=2)
#'  #
#' windows(8,8)
#' pie_plot(~treatment, DF, init.angle=45, main="pie_plot")
#'# windows(8,8)
#'# pie_plot(~treatment+sex, DF, init.angle=45)
#'#
#'# windows(8,8)
#'# pie_plot(~treatment|sex+BI, DF, init.angle=45)




#' @rdname pie_plot
#' @export
panel.piechart <-
    function(x, y, labels = as.character(y), #main="",
             edges = 200, radius = 0.8, clockwise = FALSE,
             init.angle = if(clockwise) 90 else 0,
             density = NULL, angle = 45,
             col = superpose.polygon$col,
             border = superpose.polygon$border, mar=c(2,2,2,2)-.2,  digits= 0,
             lty = superpose.polygon$lty, ...)
    {
      #  print(list(...))
        #     lbls <- as.character(xdata[, xnames[1]])
        pct <- round(x/sum(x)*100, digits)
        labels <- paste(labels, pct) # add percents to labels
        labels <- paste(labels,"%",sep="") # ad % to labels
        #     }
        stopifnot(require("gridBase"))
       # cat("\n x \n")
       # print(x)
       # cat("\n y \n")
       # print(y)
      #  print(labels)



        superpose.polygon <- trellis.par.get("superpose.polygon")
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
        if (panel.number() > 1) par(new = TRUE)
        par(fig = gridFIG(), omi = c(0, 0, 0, 0),
            mar=mar
           # mai = c(0, 0, 0, 0)
            )
        pie(as.numeric(x), labels = labels,
            edges = edges, radius = radius,
            clockwise = clockwise, init.angle = init.angle, angle = angle,
            density = density, col = col, border  = border, lty = lty)
    }


#' @rdname pie_plot
#' @export
piechart <- function(x, data = NULL, panel = "panel.piechart",  ...)
{
    ocall <- sys.call(sys.parent())
    ocall[[1]] <- quote(piechart)
    ccall <- match.call()
    ccall$data <- data
    # ccall$title <- "hallo"
    ccall$panel <- panel
    ccall$default.scales <- list(draw = FALSE)
    ccall[[1]] <- quote(lattice::barchart)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}


#' @rdname Grafik
#' @export
pie_plot<- function(fm,
                 data,
                 main="",
                 digits=0,
                 # col=NA,
                 percent=TRUE,
                 mar=  c(3,3,3,3) + 0.1,
                 drop.unused.levels = TRUE,
                 ...){
    library(gridBase)
    xtab<- xtabs(gsub("\\|", "+", fm), data,
                 drop.unused.levels =  drop.unused.levels)
    xdata<- data.frame( xtab)
    xnames<- names(xdata)[-ncol(xdata) ]
    slices <- xdata$Freq
#     lbls <- as.character(xdata[, xnames[1]])
#     if(percent) { pct <- round(slices/sum(slices)*100, digits)
#     lbls <- paste(lbls, pct) # add percents to labels
#     lbls <- paste(lbls,"%",sep="") # ad % to labels
#     }
  #  print(xtab)
    par(new = TRUE)
    plot(1)
    par(new = TRUE)
    piechart(xtab, groups = FALSE,
             main=main, xlab = "",
             digits=digits,
           #  labels=lbls,
             percent=percent, mar=mar, ...)



}



