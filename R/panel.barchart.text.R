#' panel.barchart.text
#'
#'
#' Zahlen in Barplots
#'
#' @param x,y,box.ratio,horizontal,stack,groups kommt alles von lattice
#' @param digits,fmt  digits geht an fmt siehe  sprintf 
#' @param prefix,suffix Text dekoration
#' @param labels  Sortierung der Variablen beachten
#' @param cex,col an panel.text die Groesse und die Farbe der Schrift
#' @param offset Text inerhalb oder ausserhalb der Balken default ist ofoset= 0.5,
#' @param ... an panel.text  lineheight, font, fontfamily, fontface, adj
#' @return  lattice 
#' @export
#'
#' @examples
#' 
#' # graphics.off()
#' 
#' #--http://www.ats.ucla.edu/stat/r/faq/barplotplus.htm
#' #-- 24.06.2015 08:27:01
#' 
#' # library(stp25plot)
#' # require(lattice)
#' set.seed(2)
#' 
#' DF <-
#'   data.frame(
#'     value = runif(2 * 5, min = 20, max = 80),
#'     sex = factor(rep(c("male", "female"), times = 5)),
#'     variable = factor(rep(
#'       c(
#'         n = "Neurotizismus",
#'         e = "Extraversion",
#'         o = "Offenheit",
#'         g = "Gewissenhaftigkeit",
#'         a = "Vertraeglichkeit"
#'       ),
#'       times = 2
#'     ))
#'   )
#' 
#' 
#' 
#' 
#' my_key <- list(
#'   columns = 2,
#'   space = "top",
#'   cex = 0.8,
#'   size = 1.4,
#'   adj = 1,
#'   between = 0.2,
#'   between.colums = 0.1
#' )
#' 
#' 
#' 
#' #' windows(5, 6)
#' #' barchart(
#' #'   reorder(variable, value) ~ value,
#' #'   subset(DF,
#' #'          sex == "male"),
#' #'
#' #'   box.ratio = 2,
#' #'   xlim = c(-5, 100),
#' #'   origin = 0,
#' #'
#' #'   #' par.settings=colorset,
#' #'   panel = function(...)   {
#' #'     panel.barchart(...)
#' #'     panel.barchart.text(..., digits = 1, suffix = " %")
#' #'   }
#' #' )
#' windows(5, 6)
#' barchart(
#'   variable ~ value,
#'   DF,
#'   groups = sex,
#'   box.ratio = 2,
#'   xlim = c(-5, 100),
#'   origin = 0,
#'   auto.key = my_key,
#'   #' par.settings=colorset,
#'   panel = function(...)   {
#'     panel.barchart(...)
#'     panel.barchart.text(...,
#'                         digits = 1,
#'                         suffix = " %",
#'                         offset = -4)
#'   }
#' )
#' 
#' 
#' 
#' 
#' windows(5, 6)
#' barchart(
#'   value  ~ variable,
#'   DF,
#'   groups = sex,
#'   stack = TRUE,
#'   box.ratio = 2,
#'   ylim = c(-5, 100),
#'   origin = 0,
#'   auto.key = my_key,
#'   #' par.settings=colorset,
#'   panel = function(...)   {
#'     panel.barchart(...)
#'     panel.barchart.text(..., digits = 1, suffix = " %")
#'   }
#' )
#' 
#' 
#' 
#' 
#' 
#' dat<- stp25tools::get_data("
#' id                          value                          variable       Freq
#' 1             'weniger als heute'                   Telefonauskunft  68.333333
#' 2  'im gleichen Ausmaß wie heute'                   Telefonauskunft  25.000000
#' 3                'mehr als heute'                   Telefonauskunft   0.000000
#' 4                  'es gab keine'                   Telefonauskunft   6.666667
#' 5             'weniger als heute'                     Elektronische  28.333333
#' 6  'im gleichen Ausmaß wie heute'                     Elektronische  53.333333
#' 7                'mehr als heute'                     Elektronische   3.333333
#' 8                  'es gab keine'                     Elektronische  15.000000
#' 9             'weniger als heute'                  Videokonferenzen   0.000000
#' 10 'im gleichen Ausmaß wie heute'                  Videokonferenzen   1.000000
#' 11               'mehr als heute'                  Videokonferenzen   0.000000
#' 12                 'es gab keine'                  Videokonferenzen 100.000000
#' 13            'weniger als heute'                     Kurzmessenger   5.000000
#' 14 'im gleichen Ausmaß wie heute'                     Kurzmessenger   3.333333
#' 15               'mehr als heute'                     Kurzmessenger   0.000000
#' 16                 'es gab keine'                     Kurzmessenger  91.666667                     ")
#' 
#' barchart(value~Freq|variable, dat, 
#'          par.strip.text = list(lines = 2.5, cex=.75 ),
#'          xlab ="Prozent", origin =0,
#'          panel = function(...)   {
#'            panel.barchart(...)
#'            panel.barchart.text(..., digits = 0, suffix = " %", 
#'                                offset = c(-2.5, .4),
#'                                offset.tresh=20)
#'          }
#'          
#' )
#' 
panel.barchart.text <- function(x,
                                y,
                                box.ratio = 1,
                                horizontal = TRUE,
                                stack = FALSE,
                                groups = NULL,
                                
                                cex = 0.7,
                                col = 1,
                                digits = 2, 
                                fmt = paste0("%.", digits, "f"),
                                labels = NULL,
                                prefix = NULL,
                                suffix = NULL,
                          
                                offset.col = c("black", "white"),
                                offset.tresh = NULL,
                                offset = NULL,
                                ...) {
 # cat("\n neue Version\n")
  #-- Position der mitte des (letzten) Balken
  pos_width <- function(n, box.ratio, stack) {
    if (!stack) {
      box.ratio <- box.ratio / 2 / (1 + box.ratio) * (1 - 1 / n)
      seq(from = -box.ratio,
          to = box.ratio,
          length.out = n)
    }
    else
      0
  }
  
  pos_height <- function(w, stack) {
    if (!stack)
      w
    else
      cumsum(w)
  }
  
  if (horizontal) {
    # cat("\nHorizontale Balken\n")
    tmp2 <-  if (is.null(groups))
      data.frame(x = x,
                 y = y)
    else
      data.frame(x = x,
                 y = y,
                 groups = groups)[order(groups),]

    # calculate positions of text labels
    dat <-
      plyr::ddply(tmp2, "y",
                  function(x) {
                    data.frame(
                      x,
                      pos.x = pos_height(x$x, stack),
                      pos.y = as.numeric(x$y) +
                        pos_width(nrow(x), box.ratio, stack)
                    )
                  })
    x <- dat$pos.x
    y <- dat$pos.y
    pos <- 2
  }
  else  {
    #-- Vertikale Balken
    tmp2 <-  if (is.null(groups))
      data.frame(x = y, y = x)
    else
      data.frame(x = y,
                 y = x,
                 groups = groups)[order(groups),]
    dat <-
      plyr::ddply(tmp2, "y",
                  function(x) {
                    data.frame(
                      x,
                      pos.x = pos_height(x$x, stack),
                      pos.y = as.numeric(x$y) +
                        pos_width(nrow(x), box.ratio, stack)
                    )
                  })
    x <- dat$pos.y
    y <- dat$pos.x
    pos <- 1
  }
  
  if (is.null(labels)) {
    labels <- sprintf(fmt = fmt, dat$x)
    if (!is.null(prefix))
      labels <-  paste0(prefix, labels)
    if (!is.null(suffix))
      labels <-  paste0(labels, suffix)
  }
  
  
  if (!is.null(offset.tresh)) {
    if (is.null(offset))
      offset <-  c(-2.5, .5)
    offset <- ifelse(x < offset.tresh, offset[1], offset[2])
    col <-  ifelse(x < offset.tresh, offset.col[1], offset.col[2])
  }
  else if (is.null(offset))
    offset <- .5
  
  panel.text(
    x,
    y,
    label = labels,
    col = col,
    cex = cex,
    pos = pos,
    offset = offset,
    ...
  )
  
}

#panel.barchart.value <- panel.barchart.text

