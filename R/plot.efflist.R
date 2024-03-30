#' Workaraund für plot
#'
#' @param ... alles an plot
#'
#' @return standard plot
#' @export
plot2 <- function(...){
  UseMethod("plot2")
}


#' @export
#' @rdname plot2
plot2.default <- function(...){ plot(...) }


#' @rdname plot2
#' 
#' @description
#'  method for class 'eff'  effects::allEffects
#' 
#' @param x efflist-object
#' @param main 	the title for the plot, 
#' @param factor.names  lattice
#' @param nrow,ncol   cowplot::plot_grid
#' @param lattice an optional list
#' @param bottom.padding,top.padding,left.padding,right.padding,axis.padding lattice::lattice.options
#' @param lty line type for the loess smooth; the default is the first line type, normally 1 (a solid line).
#' @param lty.factor line type for the loess smooth; 0.
#' @param plot plot or list cowplot::plot_grid(plotlist)
#' @param rug  rug
#' @param ... an effects::plot.eff
#'
#' @return list oder plot
#' @export
#'
#' @examples
#' 
#'  n <- 100
#' set.seed(2)
#' DF <- data.frame(
#'   y = rnorm(n),
#'   a = rnorm(n),
#'   b = rnorm(n),
#'   c = rnorm(n),
#'   d = rnorm(n),
#'   e = rnorm(n),
#'   f = rnorm(n),
#'   g = gl(2, n / 2, labels = c("Control", "Treat"))
#' ) |>
#'   transform(
#'     y = y + a + b + d + as.numeric(g),
#'     e = cut(e, 2, c("low", "hig")),
#'     b = factor(as.character(cut(b, 3, c(
#'       "a", "c", "b"
#'     ))))
#'   )
#' 
#' 
#' fit <- lm(y ~ a + b + c + g * d + e + g * b, DF)
#' #fit2 <- step(fit)
#' e1 <- effects::allEffects(fit)
#' 
#' p1 <-
#'   plot2(e1,
#'         symbols = list(pch = 15, cex = 1.2),
#'         axes = list(x = list(cex = .9)))
#' p1
#' #cowplot::plot_grid(plotlist =p1)
#' 
#' #' require(effects)
#' mtcars2 <- within(mtcars, {
#'   vs <- factor(vs, labels = c("V", "S"))
#'   am <- factor(am, labels = c("automatic", "manual"))
#'   cyl  <-  (cyl)
#'   cyl_ord  <-  ordered(cyl)
#'   gear <- ordered(gear)
#'   carb <- ordered(carb)
#' })
#' 
#' mtcars2$mpg[mtcars2$cyl_ord==6]  <- mtcars2$mpg[mtcars2$cyl_ord==6] *.5
#' 
#' mtcars2 <- mtcars2 |> stp25tools::Label(
#'   mpg	= "Miles/(US) gallon",
#'   cyl_ord	= "Number of cylinders",
#'   disp	= "Displacement (cu.in.)",
#'   hp	= "Gross horsepower",
#'   drat =	"Rear axle ratio",
#'   wt =	"Weight (1000 lbs)",
#'   qsec =	"1/4 mile time",
#'   vs =	"Engine (0 = V-shaped, 1 = straight)",
#'   am	= "Transmission",
#'   gear	= "Number of forward gears",
#'   carb =	"Number of carburetors"
#' )
#' 
#' #fit <- lm(mpg ~ hp * wt + am +cyl  , data = mtcars2)
#' fit2 <- lm(mpg ~ hp * wt + am + cyl_ord  , data = mtcars2)
#' 
#' 
#' 
#' ### Meine Version vs plot.efflist
#' 
#' 
#' #plot( effects::allEffects(fit2) )
#' e2<- effects::allEffects(fit2)
#' names(e2)
#' 
#' p2<-plot2(e2,
#'           labels = stp25tools::get_label(mtcars2),
#'           plot=FALSE)
#' 
#' cowplot::plot_grid(plotlist =p2, 
#'                    labels = c('A', 'B', 'C'),
#'                    # scale = c(1, .9, .8),
#'                    rel_heights = c(3,4))
plot2.efflist <-
  function (x,
            main = "",
            factor.names = FALSE,
            nrow = NULL,
            ncol = NULL,
            lattice = list(),
            labels = NULL,
            cex =.8,
            cex.x = cex, cex.y = cex,
            lty = 1,
            lty.factor = 0,
            axes = NULL,
            # axis.padding = lattice::lattice.getOption("axis.padding"),
            # layout.heights = lattice::lattice.getOption("layout.heights"),
            # layout.widths = lattice::lattice.getOption("layout.widths"),
            # bottom.padding = lattice::lattice.getOption("bottom.padding"),
            # top.padding = lattice::lattice.getOption("top.padding"),
            bottom.padding = 0,
            top.padding = 0,
            left.padding = .4,
            right.padding = 1,
            axis.padding = .4,
      
            
            plot = TRUE,
            rug = FALSE,
            multiline =NULL,
            ...)
  {
    plotlist <- list()
    effects <- gsub(":", "*", names(x))
    eff_names <-
      c(x[[1]]$response, unlist(strsplit(effects, "\\*")))
    
    if (!is.null(labels))
      labels <-  labels[eff_names]
    else{
      labels <- eff_names
      names(labels) <- eff_names
    }
    
    if(is.null(axes))
      axes <-  axis_list(effects = effects,
                         response = x[[1]]$response,
                         labels, cex.x, cex.y)
    
    lattice::lattice.options(
      layout.heights = list(
        bottom.padding = list(x = bottom.padding),
        top.padding = list(x = top.padding)
      ),
      layout.widths =
        list(
          left.padding = list(x = left.padding),
          right.padding = list(x = right.padding)
        ),
      axis.padding = list(numeric = axis.padding)
    )
    
    
    for (i in seq_along(effects)) {
      
      is_fctr <-
        unlist(purrr::map(x[[i]]$variables, \(x) x$is.factor))
      
      if (all(is_fctr)) lty2 <- lty.factor
      else lty2 <- lty
      
      if (is.null(multiline)) {
        if (any(is_fctr)) multiline2 <- FALSE
        else multiline2 <- TRUE
      }
      else multiline2 <- multiline
      #effects:::plot.eff
      plotlist[[effects[i]]] <-
        plot(
          x[[i]],
          factor.names = factor.names,
          lattice = lattice,
          main = main,
          rug = rug,
          lty = lty2,
          axes = axes,
          multiline = multiline2,
          ...
        )
    }
    
    if (plot)
      cowplot::plot_grid(plotlist = plotlist,
                         nrow = nrow,
                         ncol = ncol)
    else
      plotlist
  }

#' make a list for the axix
axis_list <- function(
                      effects,
                      response,
                      labels = NULL,
                      cex.x = 1,
                      cex.y = 1
                      ) {
  axs_list <- list(y = list(cex = cex.y))

  eff_names <-
    c(response, unlist(strsplit(effects, "\\*")))
  
  axs_list$y$lab <- as.vector(labels[response])
  axs_list$x  = list(cex = cex.x)
  
  for (i in names(labels)) {
    axs_list$x[[i]] <- list(lab = as.vector(labels[i]))
    
  }
  axs_list
  
  
}








