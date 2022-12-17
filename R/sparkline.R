#' @rdname sparkplot
#'
#' @param x,y from panel
#' @param z.name names of levels
#' @param digits to signif
#' @param cex.label,cex.numbers,cex.arrows,cex.points size
#' @param include.labels lables
#' @param include.arrows,lim.arrows reggression direction as arrow
#' @param include.max number
#' @export
#'
#' @examples 
#' 
#'  set.seed(1)
#' 
#' DF <- data.frame(
#'   Laborwert = gl(7, 8,
#'                  labels = c(
#'                    "Albumin", "Amylase", "Lipase",
#'                    "AST", "ALT","Bilirubin","C-Peptid")),
#'   Treat = gl(2, 4, labels = c("Control", "Treat")),
#'   Time = gl(4, 1, labels = c("t0", "t1", "t2", "t4")),
#'   x = rnorm(7 * 8)
#' )
#' DF <- transform(DF,
#'                 x = scale(x + as.numeric(Treat)*2 + as.numeric(Time) / 2))
#' DF1 <- stp25tools::Summarise(DF, x~ Laborwert+ Time, fun=mean )
#' 
#' names(DF1)[4]<- "x"
#' #         )
#' DF1[21,4]<-1
#' 
#' sparkplot(
#'   x ~ Time | Laborwert,
#'   DF1,
#'   between = 1.5,
#'   
#'   
#'   include.axis=TRUE,
#'   pch = 19,
#'   col="green",
#'   include.first = TRUE,
#'   #  as.table=FALSE,
#'   include.max = TRUE,
#'   include.labels=TRUE
#' )  
#' require(grid)
#' 
#' grid.text("A", .02, .95, gp=gpar(fontsize=20))
#' grid.text("B", .02, .45, gp=gpar(fontsize=20))
#' grid.lines(x = unit(c(.1, .95), "npc"),
#'            y = unit(c(.065, 0.065), "npc"),
#'            arrow =arrow(angle = 30, length = unit(0.1, "inches"),
#'                         ends = "last", type = "open"))
panel.sparkline <- function(x,
                            y,
                            ...,
                            z.name,
                            digits = 2,
                            cex.label=1,
                            cex.numbers=.7,
                            cex.arrows= NULL,
                            cex.points=.8,
                            lim.arrows=.25,
                            pch=NULL,
                            pch.default=16,
                            col.default="gray40",
                            col.max.min= c("blue", "red"), 
                            include.labels = TRUE,
                            include.arrows = TRUE, 
                            include.max=TRUE,
                            include.min=TRUE,
                            include.first=FALSE,
                            include.last=FALSE,
                            include.box =TRUE
                            ) {

dots<- list(...)
  
  if(!is.logical(include.box)){
    box.x1 <-min(x)
    box.x2<- max(x)
    box.y1 <- include.box[[1]][lattice::panel.number()]
    box.y2 <- include.box[[2]][lattice::panel.number()]
    include.box<- TRUE
  }
  else{
    
    box.x1 <-min(x)
    box.x2<- max(x)
    box.y1 <- quantile(y, 0.25)
    box.y2 <- quantile(y, 0.75)
  }
    
  if(include.box){
  lattice::panel.rect(
    box.x1,  box.y1, box.x2, box.y2,
    col = "grey90",  border = "grey90")
  }
  
  lattice::panel.xyplot(x, y, ...)

  if(!is.null(pch))
  lattice::panel.points(
    x, y, pch = pch, cex = cex.points, col=col.default)
  
  grid::pushViewport(
    grid::viewport(
      xscale = grid::current.viewport()$xscale,
      yscale = grid::current.viewport()$yscale,
      clip = "off"
    ))

  
  if (include.first | include.last) {
    y_range <- c(y[x == min(x)], y[x == max(x)])
    x_range <-c(x[x == min(x)], x[x == max(x)])
    xy_max <- which.max(y_range)
    
    y_range <-
      c(y_range[xy_max], min(y_range[which(x_range != x_range[xy_max])]))
    x_range <-
      c(x_range[xy_max], x_range[x_range != x_range[xy_max]][1])
    
    lattice::panel.points(
      x = x_range,
      y = y_range,
      pch = pch.default,
      cex = cex.points,
      col = col.default
    )
    
    lattice::panel.text(
      x_range[1],
      y_range[1],
      labels = signif(y_range[1], digits),
      cex = cex.numbers,
      fontfamily = "serif",
      adj = c(0.5,-.75)
    )
    
    lattice::panel.text(
      x_range[2],
      y_range[2],
      labels = signif(y_range[2], digits),
      cex = cex.numbers,
      fontfamily = "serif",
      adj = c(0.5, 1.5)
    )
  }
  
  if (include.max|include.min) {
    lattice::panel.points(
      x = c(x[which.max(y)], x[which.min(y)]),
      y = c(max(y), min(y)),
      pch = pch.default,
      cex = cex.points,
      col = col.max.min
    )
   
    if (! (include.first | include.last) ){
    lattice::panel.text(
      x = x[which.max(y)],
      y = max(y),
      labels = signif(max(y), digits),
      cex = cex.numbers,
      fontfamily = "serif",
      adj = c(0.5, -.75)
    )
  
    lattice::panel.text(
      x = x[which.min(y)],
      y = min(y),
      labels = signif(min(y), digits),
      cex = cex.numbers,
      fontfamily = "serif",
      adj = c(0.5, 1.5)
    )
    }
  }
  
  if (include.labels) {
    lattice::panel.text(
    x = x[1],
    y = mean(y),
    labels = z.name[lattice::panel.number()],
    fontfamily = "serif",
    pos = 2,
    cex=cex.label,
    offset = 1
    )
    }
  
  if (include.arrows) {
    draw_arrow(x, y, dots, cex.arrows, lim.arrows)
  }
  

  # grid.lines(x = unit(c(0, 1), "npc"),
  #            y = unit(c(-0.6, -0.6), "npc")#,
  #            # default.units = "npc",
  #            # arrow = NULL, name = NULL,
  #            # gp=gpar(), draw = TRUE, vp = NULL,
  #            # 
  #            
  #            )
  
  grid::popViewport()
  
  
}

#' @rdname sparkplot
#' @export
panel.sparkbar <- function(x,
                           y,
              
                           ...,
                           z.name,
                           digits = 2,
                           cex.label=1,
                           cex.numbers=.7,
                           cex.arrows= NULL,
                           cex.points=.8,
                           lim.arrows=.25,
                           include.labels = TRUE,
                           include.arrows = TRUE, 
                           include.max=TRUE,
                           include.min=TRUE,
                           include.first=FALSE,
                           include.last=FALSE
                          
                           ) {
  
  lattice::panel.barchart(x, y,  
                          # box.ratio = 1, #box.width = box.ratio/(1 + box.ratio), 
                          horizontal = FALSE, 
                          origin =   if (any(y<0)) 0 else NULL,
                          reference = TRUE, 
                          stack = FALSE,
                          ...)
  
  grid::pushViewport(
    grid::viewport(
      xscale = grid::current.viewport()$xscale,
      yscale = grid::current.viewport()$yscale,
      clip = "off"
    ))
  
  
  if (include.max){
    lattice::panel.text(
      x = x[which.max(y)],
      y = max(y),
      labels = signif(max(y), digits),
      cex = cex.numbers,
      fontfamily = "serif",
      adj = c(0.5,-.75)
    )
    lattice::panel.text(
      x = x[which.min(y)],
      y = min(y),
      labels = signif(min(y), digits),
      cex = cex.numbers,
      fontfamily = "serif",
      adj = c(0.5, 1.5)
    )
  }
  
  if (include.labels) {lattice::panel.text(
    x = 0.5,
    y = mean(y),
    labels = z.name[lattice::panel.number()],
    fontfamily = "serif",
    pos = 2,
    cex=cex.label
  )}
  
  if (include.arrows) {
    draw_arrow(x, y, list(...), cex.arrows, lim.arrows)
    
  }
  
  grid::popViewport()
}


draw_arrow<- function(x, y, 
                      dots, cex.arrows, 
                      lim.arrows,
                      char.arrows= c(down= '\u2193', updown='\u2195', up= '\u2191' )
                      
                      ){
  
  x <- as.numeric(x)
  if (is.null(dots$subscripts)) {
    beta <- as.vector(coef(lm(scale(y) ~ scale(x)))[2])
    pos_mean <- mean(y)
  }
  else{
    dat <- data.frame(x = x,
                      y = y,
                      g = dots$groups[dots$subscripts])
    dat <- split(dat, dat$g)
    pos_mean <- sapply(dat,
                       function(d) as.vector(mean(d$y))) 
    beta <- sapply(dat, 
                   function(d) as.vector(coef(lm(scale(y) ~ scale(x), d))[2]))
  }
  
  pos_x <- .95
  j <- 0
  superpose.line <- lattice::trellis.par.get("superpose.line")
  
  for (i in beta) {
    j <- j + 1
    pos_x <- pos_x + .06
    col <- superpose.line$col[j]
    if (i < -lim.arrows)  sm1 <- char.arrows[1]
    else if (i > lim.arrows) sm1 <- char.arrows[3]
    else  sm1 <-char.arrows[2]
    
    if(is.null(cex.arrows)) fontsize <- round( abs(beta[j]) *10) + 10
    else fontsize <- cex.arrows*12
    grid::grid.text(
      sm1,
      x = pos_x,
      y =  grid::unit(pos_mean[j], "native"), 
      gp =  grid::gpar(col = col, fontsize =  fontsize)
    )
    
  }
  
}


#' sparkplot
#' 
#' Stolen from http://www.motioninsocial.com/tufte/#sparklines
#'
#' @param x,data,... to xyplot() 
#' @param lwd,lty,col graphical parameters
#' @param pch graphical parameters used in auto.key
#' @param type "l"
#' @param scales,strip,layout not used
#' @param ylab,xlab character labels
#' @param digits  number to signif
#' @param right.padding,left.padding number links rechts abstand
#'
#' @return lattice
#' @export
#'
#' @examples
#' 
#'  set.seed(1)
#' 
#' DF <- data.frame(
#'   Laborwert = gl(7, 8,
#'                  labels = c(
#'                    "Albumin", "Amylase", "Lipase",
#'                    "AST", "ALT","Bilirubin","C-Peptid")),
#'   Treat = gl(2, 4, labels = c("Control", "Treat")),
#'   Time = gl(4, 1, labels = c("t0", "t1", "t2", "t4")),
#'   x = rnorm(7 * 8)
#' )
#' DF <- transform(DF,
#'                 x = scale(x + as.numeric(Treat)*2 + as.numeric(Time) / 2))
#' DF1 <- stp25tools::Summarise(DF, x~ Laborwert+ Time, fun=mean )
#' names(DF1)[4]<- "x"
#' 
#' #: "p", "l", "h", "b", "o", "s", "S", "r", "a", "g"
#' p1 <- sparkplot(x ~ Time | Laborwert, DF1, between=1.5)
#' col<- c("purple", "darkgreen")
#' p2<- sparkplot(
#'   x ~ Time | Laborwert,
#'   DF,
#'   groups = Treat,
#'   between=1.5,
#'   include.labels = FALSE, 
#'   left.padding=-5,  right.padding=3,
#'   col = col ,
#'   key = list(
#'     corner = c(1, 1.1),
#'     lines = list(col = col, lwd = 2),
#'     # title="CIT",
#'     cex = .75,
#'     columns = 2,
#'     text = list(levels(DF$Treat))
#'   )
#' )
#' 
#' p3 <- sparkplot(
#'   x ~ Time | Laborwert,
#'   DF,
#'   groups = Treat,
#'   type="barchart",
#'   between=1.5,
#'   include.labels = FALSE, 
#'   left.padding=-5,  right.padding=3,
#'   col =  col
#' )
#' #windows(8,4)
#' require(cowplot)
#' plot_grid(p1,  p2,  p3,
#'           nrow=1,
#'           rel_widths = c(1, .5, .5)
#' )
#' 
#
sparkplot <- function(x,
                      data,
                      lwd = 2,
                      lty = NULL,
                      col = NULL,
                      pch = NULL,
                      type = "l",
                      scales,
                      strip,
                      layout,
                      ylab = "",
                      xlab = "",
                      digits = 3,
                      between = 1,
                      
                      right.padding = 4,
                      left.padding =1.5,
                      include.labels = TRUE,
                      include.arrows = TRUE,
                      include.axis= FALSE,
                      include.max=TRUE,
                      include.min=include.max,
                      include.first=FALSE,
                      include.last=include.first,
                      include.box=TRUE,
                      ...) {
  if (type == "barchart"){
    return(
      sparkplot2(
        x,
        data,
        lwd = lwd,
        lty = lty,
        col = col,
        pch = pch,
        ylab = ylab,
        xlan = xlab,
        digits = digits,
        between = between,
        right.padding = right.padding,
        left.padding = left.padding,
        include.labels = include.labels,
        include.arrows = include.arrows,
        include.axis =   include.axis,
        include.max =   include.max,
        ...
      )
    )
  }
  
  # Daten vorgereiten
  # lhs <- x[[2L]]
  x.var <-   all.vars(x)[2]
  rhs <- all.vars(x)[3]
  data <- data[order(data[[x.var]]), ]
  
  if (is.null(col))  col <- 1
  if (is.null(lty))  lty <- 1
  
  
  if (!is.factor(data[[rhs]]))
    data[[rhs]] <- factor(data[[rhs]])
  
  z.name <- levels(data[[rhs]])
  
  if (is.factor(data[[x.var]])) {
    at <- 1:nlevels(data[[x.var]])
    label <- levels(data[[x.var]])
    data[[x.var]] <- as.numeric(data[[x.var]])
  }
  else{
    at <- label <-  pretty(data[[x.var]])
  }
  
  if(include.axis)
    scales <-  list(
      y = list(at = NULL, relation = "free"),
      x = list(
        fontfamily = "serif",  
        at = at,  
        label = label
        #,  col="red"
    
      )
    )
  else   scales <- list(
    y = list(at = NULL,  relation = "free"), 
    x = list(at=NULL)
  )
  
  
  #trellis.par.get("axis.line")
  #  "axis.text"         ""
  par.settings <- list(
    axis.line = list(col = "transparent"),
    axis.components =  list(bottom = list(
      tck = 1, pad1 = 2, pad2 = 0
    )),
    layout.widths = list(
      right.padding = right.padding,
      left.padding =   if (include.labels)
        left.padding + max(nchar(z.name) / 2)
      else
        left.padding
    ),
    superpose.symbol = list(pch = pch, fill = col, col = col) ,
    superpose.polygon = list(col = col, border = "transparent"),
    superpose.line = list(col = col, lty = lty),
    strip.shingle = list(col = col)
  )
  
  
 lattice::xyplot(
    x, data,
    xlab = xlab, ylab = ylab,
    strip = FALSE,
    lwd = lwd,
    type = type,
    layout = c(1, length(unique(data[[rhs]]))),
    between =  list(y = between),
    scales = scales,
   # as.table=TRUE,
    par.settings = par.settings,
    panel = function(x, y, ...) {
      panel.sparkline(
        x,  y,
        ...,
        #  data = data,
        pch=pch,
        z.name = z.name,
        digits = digits,
        include.labels = include.labels,
        include.arrows = include.arrows,
        include.max=include.max,
        include.min=include.min,
        include.first=include.first,
        include.last=include.last,
        include.box=include.box
      )
     #lims <- current.panel.limits()
      #print(lims)
      
 
    },
    ...
  )
 
}

#' @rdname sparkplot
#' @export
sparkplot2 <- function(x,
                       data,
                       lwd = 2,
                       lty = NULL,
                       col = NULL,
                       pch = NULL,
                       type = "barchart",
                       scales,
                       strip,
                       layout,
                       ylab = "",
                       xlab = "",
                       digits = 3,
                       between = 1,
                       right.padding = 4,
                       left.padding =1.5,
                       include.labels = TRUE,
                       include.arrows = TRUE,
                       include.axis= FALSE,
                       include.max=TRUE,
                       ...) {
  # lhs <- x[[2L]]
  x.var <-   all.vars(x)[2]
  rhs <- all.vars(x)[3]
  data <- data[order(data[[x.var]]), ]
  
  if (is.null(col))  col <- 1
  if (is.null(lty))  lty <- 1
  
  
  if (!is.factor(data[[rhs]]))
    data[[rhs]] <- factor(data[[rhs]])
  
  z.name <- levels(data[[rhs]])
  
  if (is.factor(data[[x.var]])) {
    
    at <- 1:nlevels(data[[x.var]])
    label <- levels(data[[x.var]])
    
  }
  else{
    at <- label <-  pretty(data[[x.var]])
    
    data[[x.var]] <- cut(data[[x.var]], at)
    
    
    
  }
  
  if(include.axis)
    scales <-  list(
      y = list(at = NULL, relation = "free"),
      x = list(
        fontfamily = "serif",
        at = at,
        label = label
      )
    )
  else   scales <-  list(
    y = list(at = NULL, relation = "free"),
    x= list(at=NULL)
  )
  
  par.settings <- list(
    axis.line = list(col = "transparent"),
    layout.widths = list(
      right.padding = right.padding,
      left.padding =   if( include.labels)   left.padding + max(nchar(z.name) / 2) else left.padding
    ),
    superpose.symbol = list(pch = pch, fill = col, col = col) ,
    superpose.polygon = list(col = col, border = "transparent"),
    superpose.line = list(col = col, lty = lty),
    strip.shingle = list(col = col)
  )
  
  
  lattice::xyplot(
    x,
    data,
    xlab = xlab,
    ylab = ylab,
    strip = FALSE,
    lwd = lwd,
    layout = c(1, length(unique(data[[rhs]]))),
    between =  list(y = between),
    scales = scales,
    
    par.settings = par.settings,
    panel = function(x, y, ...) {
      panel.sparkbar (
        x,
        y,
        ...,
        #  data = data,
        z.name = z.name,
        digits = digits,
        include.labels = include.labels,
        include.arrows = include.arrows,
        include.max=include.max
      )
    },
    ...
  )
  
  
}




# http://www.motioninsocial.com/tufte/#sparklines
# 
# trans$value[10]<-900
# col<- c("purple", "darkgreen")
# p3 <- sparkplot(
#   value ~ time | souce,  trans,
#   groups = variable,
#   right.padding=10,
#   lwd = lwd,
#   col = col,
#   key = list(
#     corner =pos,  lines = list(col = col, lwd = lwd),
#     between.columns=.4,  between=.4,
#     cex = .75,  columns = 2,
#     text = list(levels(trans$variable))
#   ),
#   include.first = TRUE,
#   include.max = FALSE
# )
# p3
