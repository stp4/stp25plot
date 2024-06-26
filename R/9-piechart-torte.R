#' Tortendiagramm
#' 
#' Quelle: https://r.789695.n4.nabble.com/Multi-panel-Pie-Charts-td1687026.html
#' 
#' 
#' 
#' 
#' @param x Formula
#' @param data daten
#' @param drop.unused.levels an xtab
#' @param ... an panel.piechart zb init.angle=45
#'
#' @return lattice
#' @export
#'
#' @examples
#' require(stp25plot)
#' require(lattice)
#' set.seed(2)
#' n<-20*3*2
#' 
#' DF <- data.frame(
#'   n = runif(n, min = 1, max = 5),
#'   e = runif(n, min = 1, max = 5),
#'   o = runif(n, min = 1, max = 5),
#'   g = runif(n, min = 1, max = 5),
#'   a = runif(n, min = 1, max = 5),
#'   treatment = gl(3, n / 3, labels = c("UG1", "UG2", "KG"))[sample.int(n)],
#'   sex = gl(2, n / 2, labels = c("male", "female"))
#' ) |>
#'   stp25tools::Label(
#'     n = "Neurotizismus",
#'     e = "Extraversion",
#'     o = "Offenheit fuer Erfahrungen",
#'     g = "Gewissenhaftigkeit",
#'     a = "Vertraeglichkeit"
#'   )
#' 
#' DF[1, 1:3] <- 5
#' DF[1, 4:5] <- 1
#' DF[2:10, 1] <- 4.5
#' DF[n, 4:5] <- 5
#' DF[n, 1:5] <- 1
#' 
#' torte( ~ treatment, DF, init.angle = 45, main = "lattice")
#' torte( ~ treatment + sex, DF, init.angle = 45, main = "lattice")
#' 
#' 
#' 
#' require(ggplot2)
#' # Create test data.
#' data <- data.frame(
#'   category=c("Granulocytes", "CD3+", "CD56+",  "CD19+", "Monocytes"),
#'   count=c(80,10,5,3,2)
#' )
#' 
#' # Compute percentages
#' data$fraction <- data$count / sum(data$count)
#' # Compute the cumulative percentages (top of each rectangle)
#' data$ymax <- cumsum(data$fraction)
#' # Compute the bottom of each rectangle
#' data$ymin <- c(0, head(data$ymax, n=-1))
#' # Compute label position
#' data$labelPosition <- (data$ymax + data$ymin) / 2
#' # Compute a good label
#' #data$label <- paste0(data$category, "\n value: ", data$count)
#' data
#' # Make the plot
#' P1<- data |> ggplot(
#'   aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2,
#'       fill=category)) +
#'   geom_rect() +
#'   # geom_text( x=2,
#'   #            aes(y=labelPosition,
#'   #                label=label,
#'   #                color=1), size=6) + # x here controls label position (inner / outer)
#'   scale_fill_manual(
#'     values =
#'       c("#918E00","#00F801","#FF2600","#0433FF","#FE9300")) +
#'   coord_polar(theta="y") +
#'   xlim(c(-1, 4)) +
#'   theme_void() +
#'   theme(legend.position = "top") +
#'   labs(title = "Leukocyte composition 1h NMP") +
#'   theme(legend.title = element_blank(),# element_text(size=12, color = "salmon", face="bold"),
#'         legend.justification=c(0,1),
#'         legend.position=c(0.4, 0.7),
#'         legend.background = element_blank(),
#'         legend.key = element_blank()
#'   )
#' P1
#' 
#' ggplot(data, aes(x = 1, y = count , fill = category )) +
#'   # Make pie
#'   coord_polar(theta = "y") +
#'   # Add the *stacked* columns
#'   geom_col(position = position_stack(reverse = TRUE),
#'            color = "tan3", 
#'            size = 3, show.legend = FALSE) +
#'   # Add labels to the *stacked* position,
#'   # in the middle of the column (vjust = 0.5)
#'   geom_text(aes(label = category),
#'             position = position_stack(vjust = 0.5, reverse = TRUE)) +
#'   theme_void() +
#'   labs(title = "Relative time spent building piecharts with ggplot2")
torte <- function(x,
                  data,
                  drop.unused.levels = FALSE,
                  ...) {
  
     plot.new()
  if (length(all.vars(x)) == 1) {
    xdata <- data.frame(xtabs(x,
                              data,
                              drop.unused.levels =  drop.unused.levels))
    piechart(
      ~ Freq, 
      xdata,
      groups = xdata[[all.vars(x)]],
      ...)
    
  } else{
    x <-  formula(paste(gsub("\\|", "+", x), collapse = " "))
    xdata <- data.frame(xtabs(x,
                              data,
                              drop.unused.levels =  drop.unused.levels))
 
    piechart(
      formula(
        paste( "~Freq|", paste0(all.vars(x)[-1], collapse = "+"))), 
      xdata, 
      groups = xdata[[all.vars(x)[1]]],
      ...)
    
  }
  
}



 
#' @param x,y,groups kommt vom panel 
#' @param labels ist entweder die levels von groups oder ein character string
#' @param percent, digits Anzeigen der Prozent
#' @param edges,radius,,clockwise,init.angle,density,angle,col,border,mar,lty an   graphics::pie
#' @param ...  Fehler abfangen
#'
#' @rdname torte
#' 
#' @export
#' 
#' @examples 
#' 
#' tab <- as.data.frame(xtabs( ~ treatment + sex, DF))
#' 
#' # geht nur mit 
#' plot.new()
#' barchart(
#'   ~ Freq | sex,
#'   tab,
#'   groups = treatment, scales=list(draw = FALSE), xlab="",
#'   auto.key = list(columns = 3), 
#'   par.settings = bw_theme(farbe()),
#'   layout= c(2,1),
#'   panel = panel.piechart
#' )
#' 
panel.piechart<- function(
         x,
         y,
         groups,
         labels = levels(groups),
         percent= TRUE,
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
  if(percent){
    pct <- round(x / sum(x) * 100, digits)
    labels <- paste(labels,  " ",  pct, "%", sep = "") # ad % to labels
  }
  
  # stopifnot(require("gridBase"))
  superpose.polygon <-
    lattice::trellis.par.get("superpose.polygon")
  
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  
  # if (lattice::panel.number() > 1)
  # par(fig = gridBase::gridFIG(),
  #     omi = c(0, 0, 0, 0),
  #     mar = mar)
  
  par(fig = gridBase::gridFIG(),
      omi = c(0, 0, 0, 0),
      mar = mar,
      new = TRUE)

  
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
#' 
#' @description Das ist eine Kopie aus lattice Seite 253 
#' 
#' @export
#' 
#' @examples 
#' 
#' tab <- as.data.frame(xtabs( ~ treatment + sex, DF))
#' 
#' barchart(
#'   ~ Freq | sex,
#'   tab,
#'   groups = treatment, scales=list(draw = FALSE), xlab="",
#'   auto.key = list(columns = 3),
#'   panel = panel.piechart
#' )
#' 
#' 
#' piechart(~Freq|sex, tab, groups= treatment, auto.key=list(columns=3))
#' tab <- as.data.frame(xtabs( ~ treatment + sex, DF))
#' 
#' 
#' 
#' piechart(~Freq|sex, tab, groups= treatment, auto.key=list(columns=3))
#' 
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
  
  
  # # plot.new() wegen Fehler: cannot pop the top-level viewport ('grid' and 'graphics' output mixed?
    plot.new()
  # par(new = TRUE)
  
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
#' @param wrap   Titel  wrap_string -> main
#' @param ... 
#'
#' @return ggplot
#' @importFrom ggplot2 ggplot geom_bar aes coord_polar xlab labs scale_fill_manual theme ggtitle facet_grid 
#' @export
#' 
#' @examples 
#' 
#'  gtorte(~treatment+sex, DF, init.angle=45, main="ggplot")
#' 
#' 
gtorte<- function(x,
                  data, 
                  main="", 
                  col= NULL,
                  cex.key=1,
                  margin = 2,
                  cex.ltext = 1.1,
                  wrap=NULL,
                cbPalette= c(
                  orange     = "#E69F00",
                  skyblue    = "#56B4E9",
                  green      = "#009E73",
                  yellow     = "#F0E442",
                  blue       = "#0072B2",
                  vermillion = "#D55E00",
                  purple     = "#CC79A7"),
                  ...) {
   # require(ggplot2)
  if(!is.null(wrap)) main <- stp25tools:::wrap_string(main, wrap)
  
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
      facet_grid(rows = . ~ groups) +
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




