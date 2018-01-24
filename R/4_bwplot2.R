#' bwplot2
#' @description Lattice bwplot mit groups. Ist eine erweiterung von lattice \link{bwplot}. Die Funktion arbeitet mit \link{panel.superpose}.
#' @param ... alles an lattice
#' @param box.width Box breite
#' @param space_between Box breite
#' @param nlevels anzahl an Gruppen 
#' @param panel eigene panel funktion zB mit  panel.abline(v=3)
#' @return NULL
#' @export
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
#' 
#' library(latticeExtra)
#' windows(6,4)
#' p1<-bwplot2( e~treatment, DF,  groups=sex,  auto.key=list(columns=2)) 
#' addlabels <-
#'   function(x,y)
#'     panel.abline(v=40)
#' p1 + 
#'   layer(addlabels(x,y)) +
#'   layer(panel.key(c("span = 0.5", 
#'                     "span = 1.0"), corner = c(.95,.23)))
#' 
#' 
#' 
bwplot2 <-function(...,
                   xlab=NULL, ylab=NULL,
                   auto.key=list(), # pch=15, lwd=2.5, points=FALSE,  columns=3, rectangles =TRUE
                   box.width = 1/4,
                   space_between=1.2, nlevels=2 ,
                   panel=function(...){
                         panel.superpose(...)
                        # panel.abline(v=3)
                   }
                   
){
 
  bwplot( ..., #groups=groups,
          xlab=xlab, ylab=ylab,
          box.width = box.width,
          auto.key=auto.key,
          par.settings = list(superpose.symbol=list(fill=trellis.par.get()$superpose.symbol$col)),
          panel = panel,
          space_between=space_between, nlevels=nlevels,
          panel.groups = function(x, y, 
                                  ..., 
                                  group.number, 
                                  nlevels=nlevels, 
                                  space_between=space_between){
                            dots <- list(...)
                            nshift <- (nlevels-1)/2+1
                             box_wd<-dots$box.width*space_between
                            if(dots$horizontal)  panel.bwplot(x, y+(group.number-nshift)*box_wd, ...)
                            else  panel.bwplot(x+(group.number-nshift)*box_wd, y, ...)
          })
  
  
}




