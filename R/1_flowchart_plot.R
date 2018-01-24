#' flowchart_plot
#' @description Uebernommen aus library(diagram)
#' @param txt ist liste mit den Inhalten
#' @param txt_length  Zeilenumbruch
#' @param cex  schriftgroesse
#' @param arr.pos Pfeilspitze
#' @param box.size vektor der Box und zwar für links und rechts
#' @param ...  alles fuer reorder
#' @export
#' @examples
#' #library(stp25)
#' #'   windows(7,10)
#' txt<-"AAA" #LETTERS[sample.int(23,1)]
#' windows(8,8)
#' flowchart_plot(list(
#'   File=txt,
#'   Raw.x=c("B","C"),
#'   Clean.Up = c("D",paste("E",format(Sys.time(), "%Y-%m-%d"))                        ),
#'   Analyse=c( "F", "G")
#'
#' ),
#' box.size=c(.1,.12) )

flowchart_plot <- function(txt,
                     txt_length=30,
                     cex=.9,
                     #box_w, box_h,
                     arr.pos = 0.55,
                     box.size=c(.1,.15),
                                ... )
{
  nams <- NULL  # text in diagramm
  x <- NULL    	# position in x richtung
  y <- NULL		# position in y richtung
  b <- NULL		# breite der box links sind sie schmaeler
  p <- NULL		# pfeil
  l <- NULL		# laenge der box

  N <- sum(lengths(txt)) + length(txt) # anzahl an elemente
  M <- matrix(nrow = N, ncol = N, byrow = TRUE, data = 0)
  for(i in names(txt))
    nams<- c(nams, i, stringr::str_wrap(txt[[i]], width = txt_length))

  for(i in  lengths(txt)){
    p<-1:i + if(is.null(y)) 0 else p[length(p)] +1
    if(i==1) M[p+1, p] <- "" else diag(M[p+1, p])  <- ""
    x <- c(x, 0.2, rep(.7,i))
    b <- c(b, 0.2, rep(.3,i))       # Laenge/Breite Verhaeltnis
    l <- c(l, box.size[1] , rep(box.size[2],i))      # Box Laenge
    yi<-c(1, 1:i) + if(is.null(y))-1 else y[length(y)]
    y <-c(y, yi)
  }

  y<- round((max(y)-y)/max(y)*0.8 +.1, 2)  #oben und unten 0.1 platz

#print(l)
  par(mar = c(1, 1, 1, 1))
  diagram::plotmat(
   M,
   pos = cbind(x,y), curve = 0, name = nams, lwd = 1,
          box.size = l, arr.pos=arr.pos,
          box.lwd = 2, box.cex = cex,
          box.type = "square", box.prop = b,
          ...)
 # SaveData("FlowChart")


}


