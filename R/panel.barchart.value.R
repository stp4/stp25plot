#' panel.barchart.value
#' @description Zahlen in Barplots
#' @param ... alles von lattice 
#' @param digits   siehe auch \link{sprintf}  
#' @param cex die groesse
#' @param col  col=1 Farbe der Schrift
#' @param adj   adj= c(0.5, 0.5), 
#' @param pos = NULL  pos = 4 ist hozizoltal rechts vom Balken, pos = 3 ist horizontal oben vom Balken 
#' @param offset   default ist ofoset= 0.5,
#' @param labels  Sortierung der Variablen beachten
#' @param prefix 
#' @param suffix
#' @param  fun Funktion zum Formatiren
#' @return  nix
#' @export
#' @examples
#' #graphics.off()
#'  
#'   #--http://www.ats.ucla.edu/stat/r/faq/barplotplus.htm
#'   #-- 24.06.2015 08:27:01
#'
#'library(stp5)
#'library(plyr)
#'Start()
#'set.seed(2)
#' n<-20*3*2
#'DF<- upData(data.frame(n=runif(n, min = 1, max = 5),
#'e=runif(n, min = 1, max = 5),
#'o=runif(n, min = 1, max = 5),
#'g=runif(n, min = 1, max = 5),
#'a=runif(n, min = 1, max = 5),
#'treatment=gl(3, n/3, labels = c("UG1", "UG2", "KG"))[sample.int(n)],
#' sex=gl(2, n/2, labels = c("male", "female")
#')
#'), labels=c(  n="Neurotizismus", 
#'              e="Extraversion",
#'              o="Offenheit fuer Erfahrungen",
#'              g= "Gewissenhaftigkeit",
#'              a="Vertraeglichkeit") )
#'DF[1,1:3] <-5;DF[1,4:5] <-1; DF[2:10,1] <-4.5
#'DF[n,4:5] <-5;DF[n,1:5] <-1
#' APA2(~., DF)
#'
#'   # ( x <-Recast2(.~sex, DF, fun=mean,formula = sex ~variable ))
#'   #  xMatrix<-as.matrix(x[,-1])
#'   #windows(6,4)
#'   #par(las=2, mar=c(5,15,5,5))
#'   #
#'   #bp <- barplot2(xMatrix,
#'   #beside = TRUE, horiz = TRUE,
#'   #names.arg = colnames(x)[-1],
#'   ## plot.ci = TRUE, ci.u = upper, ci.l = lower,
#'   #col = c("lightblue", "mistyrose" ),
#'   #xlim = c(0, 5),
#'   #legend = x$sex
#'   #)
#'   #text(xMatrix,bp,round(xMatrix, 1),cex=1,pos=4) #' label on the bars themselves
#'   #title(main = "Mean math scores by SES and gender", font.main = 4)
#'
#'
#'( ag.df <-Recast2(n+e+o+g+a~sex, DF, fun=mean  ))
#'
#'   #-------------------------------------------------------------------------------
#'   #g2<-ggplot(ag.df, aes(x = reorder(variable, value), y = value, fill=sex)) +
#'   #    geom_bar(stat = "identity", position=position_dodge()) +  #',width= .91
#'   #   scale_fill_manual(values=c("lightblue", "mistyrose" )) +
#'   #    #'scale_fill_discrete( name ="",
#'   #    #'                        labels=levels(ag.df$sex)) +
#'   #
#'   #    #' scale_fill_brewer(palette="Set1")+
#'   #    geom_text(aes(y=value, ymax=value-1, label=   sprintf("%.02f", value) #'   # round(value,2)
#'   #                 ),
#'   #               position= position_dodge(width=0.9),
#'   #               vjust= 0,hjust=1, color="black" ,
#'   #               cex=3) +
#'   #    scale_y_continuous("" , limits=c(0, 4), breaks=0:4,
#'   #              labels=c("anfang", "2","3","4", "ende"))  +
#'   #    scale_x_discrete("" ) +
#'   #    theme(axis.text.y=element_text(colour="black"))+
#'   #    coord_flip()
#'   #windows(6,4)
#'   #g2
#'
#'   #-------------------------------------------------------------------------------
#'barchart(value~reorder(variable, value),  ag.df,  groups=sex, box.ratio=2,
#'                           ylim=c(-0.9,4.1), origin=1,
#'              #' par.settings=colorset,
#'               panel=function(...)   {
#'                             panel.barchart(...)
#'                             panel.barchart.value(..., digist=2)
#'               }
#'                ,
#'               auto.key=list(columns=2, space="top",
#'                             cex=0.8, size=1.4, adj=1,
#'                             between=0.2, between.colums=0.1)
#'                             )
#'
#'
#'
#' ( ag.df <-Recast2(n+e+o+g+a~sex+treatment, DF, fun=mean  ))
#'
#'
#'windows(12,4)
#' barchart(reorder(variable, value)~value| treatment ,  ag.df,  groups=sex,  box.ratio=5,
#'                           xlim=c(0.9,4.1), origin=1,
#'              #' par.settings=colorset,
#'               panel= function(...) {
#'                 panel.barchart(...)
#'                 panel.barchart.value(..., digits=4)
#'
#'              },
#'               auto.key=list(columns=2, space="top",
#'                             cex=0.8, size=1.4, adj=1,
#'                             between=0.2, between.colums=0.1)
#'                             )
#' 
#' # #'windows(4,4)
#' # x <- xtabs( ~Unfall.Gruppe, DF )
#' # x <-   data.frame((x))
#' # 
#' # barchart( Freq~Unfall.Gruppe, x,
#' #           origin=0,
#' #           panel=function(...)   {
#' #               panel.barchart(...)
#' #               panel.barchart.value(..., digits=2, pos= 3, suffix="%", 
#' #                                    fun=function(x){
#' #                                        prz<- round(x/ sum(x)*100)
#' #                                        paste0(prz, "% (",x, ")" )
#' #                                    }
#' #               )
#' #           }
#' #           
#' #           
#' # )
# #'
# #'
# #'
panel.barchart.value<- function(...,
                                cex=0.7, col=1,
                                adj = c(0.5, 0.5), pos = NULL, offset = 0.5,
                                digits="%.02f",
                                labels=NULL,  #sortirung der Variablen beachten
                                prefix=NULL,
                                suffix=NULL,
                                fun=NULL
) {
    #-- Formatierung ueber
    require(plyr)
   # require(dplyr)
    if (is.numeric(digits)) digits <- paste0("%.0", digits, "f")
    
    #-- Position der mitte des (letzten) Balken
    box_ratio <- function(n, box.ratio) {
        box.ratio <- box.ratio/2/(1 + box.ratio) * (1 - 1/n)
        seq(from = -box.ratio, to = box.ratio, length.out = n)
    }
    
    #-- Argumente von Barchart
    tmp <- list(...)
    
    # print(str(tmp))
    mpo <- tmp$box.ratio  #-- Box Ratio  speichern
    
    #-- Horizontale Balken
    if(is.factor(tmp$y)){
        tmp <- data.frame(x = tmp$x, y = tmp$y)
        # calculate positions of text labels
        df <- ddply(tmp, .(y),
                    function(x) {
                        data.frame(x,
                                   pos.x = x$x,
                                   pos.y = as.numeric(x$y) + box_ratio(nrow(x), mpo))
                    })
        lbl <- if (is.null(labels)) {sprintf(digits, df$x)} else {labels}
        x <- df$pos.x
        y <- df$pos.y
        if (is.null(pos)) pos <- 2
    }else{
        tmp <- data.frame(y = tmp$x, x = tmp$y)
        
        df <- ddply(tmp, .(y),
                    function(x) {
                        data.frame(x,
                                   pos.x = x$x,
                                   pos.y = as.numeric(x$y) + box_ratio(nrow(x), mpo))
                    })
        
        lbl <- if (is.null(labels)) {sprintf(digits, df$x)} else {labels}
        x <- df$pos.y
        y <- df$pos.x
        if(is.null(pos)) pos <- 1
    }
    if(!is.null(prefix)) lbl <- paste0(prefix, lbl)
    if(!is.null(suffix)) lbl <- paste0(lbl, suffix)
    if(!is.null(fun))    lbl <-  fun(df$x)
    
    panel.text(x, y, label = lbl,
               col = col, cex = cex, adj = adj, pos = pos, offset = offset)
    
    
}
