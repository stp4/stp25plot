#' panel.mean
#' @description Erstellt Mittelwertdiagramm Mittelwerte in bwplots
#' ## http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions
#' @param Name
#' @export
#' @examples
#' library(stp5)
#' Start(col=7 )
#'
#' #
#' x<- rnorm(100)
#' data<-data.frame(x=x, y=x+rnorm(100) , z=cut(x+rnorm(100),2) )
#'
#'
#'
#'       #  less-than or equal to (U+2264) <= '\u2264'
#'    #  greater-than or equal to (U+2265) >=  "\u2265"
#'   #  plus-minus sign (U+00B1)\u00B1',
#' #levels(data$z) <-    c(">1986", "\u22641968")
#'
#'  #xyplot(y~x|z, data)
#'
#'
#'
#'  # xyplot(y~x|z, data,
#'  #       strip=strip.custom(factor.levels= c( expression(phantom(0)  < 1968 ),
#'  #                                             expression( phantom(0)>=1968  ) ) )
#'  #        )
#'
#'
#'
#' plot(1:10, col=cols, cex=5, pch=18)
#'
#' # #  Create your own list with
#' # -- http://www.magesblog.com/2012/12/changing-colours-and-legends-in-lattice.html
#' library(RColorBrewer)
#'
#' myColours <- brewer.pal(9,"Set1")[c(8,2)]     # --  brewer.pal(4,"Dark2")
#' # - get Default settings
#'         trellis.par.get()$plot.symbol
#'         names(trellis.par.get())
#' trellis.par.set(
#'        # -- auto.key und dotplot
#'                 superpose.symbol  = list(col=myColours, pch = 15:17),
#'        # -- barchart
#'                 superpose.polygon = list(col=myColours, border="transparent"),
#'                 plot.polygon =list(col="#' 377EB8"),
#'        # -- linien
#'                 superpose.line    = list(col=myColours, lty=1:3),
#'        # -- boxplotsund symbol fuer ausreiser und xyplot
#'                 box.dot = list(pch=19, cex=1.2),
#'                 # box.rectangle =list(),
#'                 # box.umbrella = list(),
#'                 plot.symbol=list(pch=1),
#'
#'                #  dot.symbol=list( )
#' )
#'
#' windows(8,8)
#'   show.settings()
#' End(FALSE)  # -- anderungen zuraecksetzen
#' # -------------------------------------------------------------------------------
#'  x1<-rnorm(100)
#'  x2<-gl(2, 50, labels = c("Control", "Treat"))
#'  y<-(1.5-as.numeric(x2))*x1+rnorm(100)
#' # -- Sonderzeichen -------------------------------
#' windows(7,4)
#' p1<- xyplot(y~x1|x2, xlab = expression(hat(mu)[0]), type=(c("p", "r"))
#'   ,ylab=list(label="Percent of Respondents", cex=2)  # - auch mit fontsize=20
#'         ,par.strip.text=list(lines=2.5, col=6)
#'         ,strip=strip.custom(factor.levels=expression(sqrt(G^{1}), sqrt(italic(R)^{1})) )
#'  )
#'  print(p1)
#' # -------------------------------------------------------------------------------
#' # -- Lange levels Umbrechen
#' #    stp5::wrap_sentence oder stringr::str_wrap unterschied ist der Return Wert
#' # -------------------------------------------------------------------------------
#' # -- Gruppierte Boxplots
#'
#' windows(6,4)
#' p2<-  bwplot(y ~ variable, df, groups=g3, xlab="",
#'              box.width = 1/4,
#'              auto.key=list( pch=15, lwd=2.5), #  rectangles =TRUE,  points=FALSE,, columns=3
#'              par.settings = list(superpose.symbol=list(fill=trellis.par.get()$superpose.symbol$col)),
#'              panel = panel.superpose,
#'              panel.groups = function(x, y, ..., group.number, nlevels=3, space_between=1.2){
#'                  dots <- list(...)
#'                  nshift <- (nlevels-1)/2+1
#'                  box_wd<-dots$box.width*space_between
#'                  if(dots$horizontal)
#'                      panel.bwplot(x, y+(group.number-nshift)*box_wd, ...)
#'                  else
#'                      panel.bwplot(x+(group.number-nshift)*box_wd, y, ...)
#'              }
#'
#' )
#'
#'
#'
#' print(p2)
#' # -------------------------------------------------------------------------------
#' # -- Mittelwert-Diagramm (Boxplot)
#' set.seed(1)
#' res = data.frame(coef=rnorm(99) + (-1):1,
#'                  habitat=sample(letters[1:4], 99, TRUE),
#'                  grp=c("W", "X", "Y"))
#'
#' windows(7,4)
#' p3<-bwplot(coef ~ habitat | grp, data = res,
#'         panel=function(...){
#'           #  panel.bwplot(..., pch='|')
#'           panel.mean(...)
#'         }
#'
#' )
#' print(p3)
#'
#' windows(7,4)
#'  bwplot(yield ~ site, barley, groups = year,
#'        panel = function(x, y, groups, subscripts, ...) {
#'           #  panel.grid(h = -1, v = 0)
#'            panel.stripplot(x, y, ..., jitter.data = TRUE,
#'                            groups = groups, subscripts = subscripts)
#'            panel.superpose(x, y, ..., panel.groups = panel.average,
#'                            groups = groups, subscripts = subscripts)
#'
#'             panel.points(x, y, ..., panel.groups = panel.average,
#'                            groups = groups, subscripts = subscripts)
#'           #  panel.mean(x, y, ... )
#'        },
#'        auto.key = list(points = FALSE, lines = TRUE, columns = 2))
#'
#' # -------------------------------------------------------------------------------
#' # -- Lattice plots zusammenfaegen
#' require(gridExtra)    # # install.packages("gridExtra")
#'
#' windows(12,12)
#'  grid.arrange(p1,p2,p3, ncol=2)
#'
#'  #  APA2(coef ~ habitat, res, fun=Mean2)
#' x<- Recast2(coef ~ habitat, res, fun=Mean2)
#' # grid.arrange(tableGrob(x), nrow=1, as.table=TRUE )
#' windows(5,5)
#' grid.table(x, show.rownames = FALSE, h.even.alpha=1, h.odd.alpha=1,  v.even.alpha=0.5, v.odd.alpha=1)
#' # -------------------------------------------------------------------------------
#' # -- Regression
#'  x1<- rnorm(10)
#'  x2<- rnorm(10)
#'  x3<-  rnorm(10)
#'  y1<- x1*2+x2 +rnorm(10)
#'  y2<- x1/2+x2 +rnorm(10)
#'
#'  m1<- lm(y1 ~x1+x2+x3)
#'  m2<- lm(y2 ~x1+x2)
#'
#'  #  Extrahieren der Lattice -Grafik
#'  p1 <- plot(allEffects(m1)[[1]])
#'  p2 <- plot(allEffects(m2)[[1]])
#'  p3 <- plot(allEffects(m2)[[2]])
#'  class(p1) <- class(p2) <- class(p3) <-"trellis"
#'  windows(8,8)
#'  grid.arrange(p1, p2,p3, ncol=3)
#'  # oder
#'  ef1 <-allEffects(m1)[[1]]#' str(ef1)
#'  ef2 <- allEffects(m2)[[1]]
#'  ef3 <- allEffects(m2)[[2]]
#'  elist <- list( ef1, ef2, ef3 )
#'  class(elist) <- "efflist"
#'  windows(8,8)
#'  plot(elist)
#'
#'  # ----------------------------------------------------------------------
#'  # aenderung der Position geht ueber die Reihenfolge im Modell also y~x*z vs y~z*x
#'  # mei multiline get auch z.var
#'  #  aenderungen von allEffects zb
#'  #    strip=strip.custom(strip.names=FALSE)
#'  #  geht nur ueber
#'  #    eff_cf <- effect("bs(winkel, 3)*treatment*rotation", fit0)
#'  #    p2 <-  plot(eff_cf, multiline=TRUE)
#'  #  und
#'  #    update(p2, xlim=c(0,90)
#'  #    scales=list(x=list(at=c(0,15,30,45,60,75,90), labels=c(0,15,30,45,60,75,90)    )  )
#'  #    strip=strip.custom(strip.names=FALSE)
#'
#'  #  aber nicht vergessen auch
#'  #    plot(eff_m2, factor.names=FALSE  ) macht das selbe
#'  #    Alternative ist  eff_df <- data.frame(eff_cf) und dann ggplot()
#'  #    multiline=TRUE,  key.args=list(x=0.2,y=0.9,corner=c(x=1, y=1))
#'  #    multiline=TRUE mit , z.var=2
#'
#'  #-- log-Skala
#'  #  plot(allEffects(fit2, transformation=list(link=log, inverse=exp)))
#'  #  APA2(allEffects(fit_lme2), transform=T, caption="k Ohm")
#'  #  APA2(allEffects(fit_lme2), caption="k Ohm")
#'  #  APA2(allEffects(fit_lme2, transformation=list(link=log, inverse=exp)), caption="k Ohm")
#'
#'
#'
#' End()
panel.mean <- function(x, y, ...) {
  if(any(class(x)=="factor"))  {
    tmp <- tapply(y, factor(x), FUN = mean, na.rm=TRUE)
    panel.points( tmp , pch = 16, ...)
  }
  else if(any(class(y)=="factor")) {
    tmp <- tapply(x, factor(y), FUN = mean, na.rm=TRUE)
    panel.points(x=tmp,y=1:nlevels(factor(y)), pch = 16, ...)
  }
  else {cat("Eine Variable muss ein Faktor sein!")}
}

