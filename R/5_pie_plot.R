#' Tortendiagramm
#' @name torte
#'
#' @param x Formula
#' @param data daten
#' @param main,mar Grafik Parameter
#' @param drop.unused.levels an xtab
#' @param ... an panel.piechart zb init.angle=45
#'
#' @return lattice
#' @export
#'
#' @examples
#' # require(stpvers)
#' 
#' set.seed(2)
#' n<-20*3*2
#' require(stpvers)
#' require(lattice)
#' DF<- Label(data.frame(n=runif(n, min = 1, max = 5),
#'                       e=runif(n, min = 1, max = 5),
#'                       o=runif(n, min = 1, max = 5),
#'                       g=runif(n, min = 1, max = 5),
#'                       a=runif(n, min = 1, max = 5),
#'                       treatment=gl(3, n/3, labels = c("UG1", "UG2", "KG"))[sample.int(n)],
#'                       sex=gl(2, n/2, labels = c("male", "female")
#'                       )
#' ),  n="Neurotizismus",
#' e="Extraversion",
#' o="Offenheit fuer Erfahrungen",
#' g= "Gewissenhaftigkeit",
#' a="Vertraeglichkeit")
#' DF[1,1:3] <-5;DF[1,4:5] <-1; DF[2:10,1] <-4.5
#' DF[n,4:5] <-5;DF[n,1:5] <-1
#' 
#' 
#' 
#' # windows(8,8)
#' torte(~treatment+sex, DF, init.angle=45, main="lattise")
#' gtorte(~treatment+sex, DF, init.angle=45, main="ggplot")
#' #  to_table(~treatment|sex, DF)
#' 
#' 
#' 
#' tab <- as.data.frame(xtabs( ~ treatment + sex, DF))
#' 
#' par(new = TRUE)
#' barchart(
#'   ~ Freq | sex,
#'   tab,
#'   groups = treatment, scales=list(draw = FALSE), xlab="",
#'   auto.key = list(columns = 3),
#'   panel = panel.piechart
#' )
#' 
#' par(new = TRUE)
#' piechart(~Freq|sex, tab, groups= treatment, auto.key=list(columns=3))
#' tab <- as.data.frame(xtabs( ~ treatment + sex, DF))
#' 
#' par(new = TRUE)
#' barchart(
#'   ~ Freq | sex,
#'   tab,
#'   groups = treatment, scales=list(draw = FALSE), xlab="",
#'   auto.key = list(columns = 3),
#'   panel = panel.piechart
#' )
#' 
#' par(new = TRUE)
#' piechart(~Freq|sex, tab, groups= treatment, auto.key=list(columns=3))

torte <- function(x,
                  data,
                  main="",
                  digits=0,
                  percent=TRUE,
                  mar=  c(3,3,3,3) + 0.1,
                  drop.unused.levels = FALSE,
                  ...){
  require(gridBase)
  
  xtab<- xtabs(gsub("\\|", "+", x), data,
               drop.unused.levels =  drop.unused.levels)
  
  
  xdata<- data.frame( xtab )
  xnames<- names(xdata)[-ncol(xdata) ]
  slices <- xdata$Freq
  
  # par(new = TRUE)
  # plot(1)
  # par(new = TRUE)
  piechart(xtab, groups = FALSE,
           main=main, xlab = "",
           digits=digits,
           #  labels=lbls,
           percent=percent, mar=mar, ...)
}



# Quelle:
# https://r.789695.n4.nabble.com/Multi-panel-Pie-Charts-td1687026.html

#' @rdname torte
#' 
#' @export
panel.piechart <-
  function(x,
           y,
           labels = as.character(y),
           edges = 200,
           radius = 0.8,
           clockwise = FALSE,
           init.angle = if (clockwise) 90 else 0,
           density = NULL,
           angle = 45,
           col = superpose.polygon$col,
           border = superpose.polygon$border,
           mar = c(2, 2, 2, 2) - .2,
           digits = 0,
           lty = superpose.polygon$lty,
           ...)
  {
    pct <- round(x / sum(x) * 100, digits)
    if (!is.na(labels[1])) {
      labels <- paste(labels, pct) # add percents to labels
      labels <- paste(labels, "%", sep = "") # ad % to labels
    }  
    # stopifnot(require("gridBase"))
    
    superpose.polygon <-
      lattice::trellis.par.get("superpose.polygon")
    
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    
    if (lattice::panel.number() > 1)
      par(new = TRUE)
    
    par(fig = gridBase::gridFIG(),
        omi = c(0, 0, 0, 0),
        mar = mar)
    
    graphics::pie(
      as.numeric(x),
      labels = labels,
      edges = edges,
      radius = radius,
      clockwise = clockwise,
      init.angle = init.angle,
      angle = angle,
      density = density,
      col = col,
      border  = border,
      lty = lty
    )
  }


#' @rdname torte
#' @export
piechart <- function(x, 
                     data = NULL, 
                     panel = "panel.piechart", 
                     xlab="",
                     ...)
{
  ocall <- sys.call(sys.parent())
  ocall[[1]] <- quote(piechart)
  ccall <- match.call()
  ccall$data <- data

  ccall$panel <- panel
  ccall$default.scales <- list(draw = FALSE)
  ccall$xlab <- xlab
  ccall[[1]] <- quote(lattice::barchart)
  
  
  # plot.new() wegen Fehler: cannot pop the top-level viewport ('grid' and 'graphics' output mixed?
  plot.new()
  par(new = TRUE)
  
  ans <- eval.parent(ccall)
  ans$call <- ocall
  ans
}


 











blank_theme_torte<- function(){
  ggplot2::theme_minimal() +
    theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 14, 
                                         face = "bold", 
                                         hjust = 0.5)
    )
}


 
#' @rdname torte
#' @param col Farbe
#' @param cex.key Schriftgrösse
#' @param margin an to_table 
#' @param wrap wrap_sentence
#' @param ... 
#'
#' @return ggplot
#' @export

gtorte<- function(x,
                  data, 
                  main="", 
                  col= NULL,
                 # cex.x = 1.5,  cex.y = 1.3,
                  
                  cex.key=1,
                #  settings = NULL,#set_lattice_ggplot(col.bar = cbPalette['blue']),
                  margin = 2,
                 
                  cex.ltext = 1.1,
                #  auto.key = NULL,#list(space = "bottom", columns = 2),
                #  orientation = NULL,
                  wrap=NULL,
                  ...) {
    require(ggplot2)
  if(!is.null(wrap)) main <- wrap_sentence(main, wrap)
  n_vars <- length(all.vars(x))
  lhs <-  all.vars(x)[1L]
  rhs <-  all.vars(x)[2L]
  chs <-  all.vars(x)[3L] 
  n<- nlevels(data[,lhs])
  if(is.null(col)){
    if(lhs=="sex" & n==2) col <- as.character(c( cbPalette['blue'], cbPalette['purple'] ))
    else col<- as.character(cbPalette[1:n])
  }
    if(n_vars==1){
    dat<- to_table(x, data)  
    names(dat)[1]<- "Item"

    ggplot(data = dat,
           aes(x = "",  y = Percent, fill = (Item))) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      xlab('') + ylab('') +
      labs(fill = '', cex=2 ) +
      scale_fill_manual(values = col) +
      blank_theme_torte()  +
      # geom_text(aes(
      #  y = (cumsum(Percent)),
      #  label = paste0(round(Percent), "%")
      #), size = 4) +
      theme(legend.position = "bottom", 
            legend.box = "horizontal" ,
            #legend.background = element_rect(#fill="lightblue",
            #                                 size=0.5, linetype="solid", 
            #                                 colour ="darkblue"),
            legend.text = element_text(#colour="blue", 
              size=cex.key*10 #,  face="bold"
            )
            
      )  +
      ggtitle(main)
    
  }
  else{
    dat <- to_table(x, data, margin = margin)
    names(dat)[1:2] <- c("Item", "groups")
    ggplot(data = dat,
           aes(x = "",  y = Percent, fill = factor(Item))) +
      geom_bar(width = 1, stat = "identity") +
      facet_grid(facets = . ~ groups) +
      coord_polar(theta = "y") +
      xlab('') + ylab('') +
      labs(fill = '') +
      scale_fill_manual(values = col) +
      blank_theme_torte()  +
      # geom_text(aes(
      #   y = c(.7, .2, .7, .2, .7, .2),
      #   label = paste0(round(Freq * 100), "%")
      # ), size = 4) +
      theme(legend.position = "bottom", 
            legend.box = "horizontal" ,
            #legend.background = element_rect(#fill="lightblue",
            #                                 size=0.5, linetype="solid", 
            #                                 colour ="darkblue"),
            legend.text = element_text(#colour="blue", 
              size=cex.key*10 #,  face="bold"
            )
      ) +
      ggtitle(main)
  }
}




