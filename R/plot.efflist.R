#' Workaround foreffects:::plot.eff
#'
#' @name plot.efflist
#' @param ... alles an plot
#'
#' @return standard plot
#' @export
plot_allEffects <- function(x, 
                            main = NULL,
                            factor.names = FALSE,
                            rel_widths = 1, rel_heights = 1,
                            ylab = NULL, xlab = NULL, labels = NULL,
                            cex = 1.1,
                            axis.padding = .4,
                            multiline =NULL,
                            x.var = 1,
                            space = "right",
                            columns = 1,
                            ...) {
  if (inherits(x, "efflist"))
    plot2(x, main = main,
          factor.names = factor.names,
          rel_widths = rel_widths, rel_heights = rel_widths,
          ylab = ylab, xlab = xlab, labels = labels,
          cex = cex,
          axis.padding = axis.padding,
          multiline = multiline,
          x.var = x.var,
          space = space,
          columns = columns,...)
  else
    plot2(effects::allEffects(x), 
          main = main,
          factor.names = factor.names,
          rel_widths = rel_widths, rel_heights = rel_widths,
          ylab = ylab, xlab = xlab, labels = labels,
          cex = cex,
          axis.padding = axis.padding,
          multiline = multiline,
          x.var = x.var,
          space = space,
          columns = columns,
          ...)
}


#' @rdname plot.efflist
#' @export
plot2 <- function(...){
  UseMethod("plot2")
}


#' @export
#' @rdname plot.efflist
plot2.default <- function(...){ plot(...) }


#' @rdname plot.efflist
#' 
#' @description
#'  method for class 'eff'  effects::allEffects
#' 
#' @param x efflist-object
#' @param main 	the title for the plot, 
#' @param factor.names  lattice factor.names = FALSE
#' @param multiline,x.var  multiline display a multiline plot in each panel
#'  x.var = 1,  bei interactionen immer was in der Formel an erster stelle steht description
#' @param nrow,ncol  an cowplot::plot_grid
#' @param rel_widths,rel_heights an cowplot::plot_grid
#' @param bottom.padding,top.padding,left.padding,right.padding,axis.padding lattice::lattice.options
#' @param lty line type for the loess smooth; the default is the first line type, normally 1 (a solid line).
#' @param lty.factor line type for the loess smooth; 0.
#' @param plot plot or list cowplot::plot_grid(plotlist)
#' @param rug  rug
#' @param ... an effects::plot.eff
#' @param ylab,xlab,labels x und y Beschriftung labels ist ein Vector mit Namen
#' @param cex,cex.x,cex.y größe der Schrift
#' @param axes liste wird automatisch aud den labels erstellt specifications for the x and y axes
#' @param key.args,space,columns a key, or legend, is added to the plot if multiline = TRUE
#' @param par.settings siehe https://stackoverflow.com/questions/13026196/how-to-nicely-rescale-lattice-figures
#' @return list oder plot
#' @export
#'
#' @examples
#' 
#' # require(stp25plot)
#' # require(stp25tools)
#' # require(stp25stat2)
#' 
#' 
#' mtcars2 <- mtcars |> 
#' dplyr::mutate(
#'   vs   = factor(vs, labels = c("V-shaped", "straight")),
#'   am   = factor(am, labels = c("automatic", "manual")),
#'   cyl  = ordered(cyl),
#'   gear = ordered(gear),
#'   carb = ordered(carb)
#' ) |> Label(
#'   mpg	 = "Miles/(US) gallon",
#'   cyl	 = "Number of cylinders",
#'   disp = "Displacement (cu.in.)",
#'   hp	 = "Gross horsepower",
#'   drat = "Rear axle ratio",
#'   wt   = "Weight (1000 lbs)",
#'   qsec = "1/4 mile time",
#'   vs   = "Engine",
#'   am   = "Transmission",
#'   gear = "Number of forward gears",
#'   carb = "Number of carburetors"
#' )
#' 
#' fit <- lm(mpg ~ hp * wt + vs +  am * cyl  , data = mtcars2)
#' 
#' # lattice::trellis.par.set(effectsTheme())
#' lattice::trellis.par.set(bw_theme(farbe(n=5), lwd = 1))
#' 
#' plot_allEffects(
#'   fit,
#'   labels = get_label(mtcars2),
#'   main = letters[1:3],  #as.roman(1:3),
#'   # cex= 1,
#'   space = "right",
#'   columns = 1,
#'   rel_widths = c(3, 4),
#'   rel_heights = c(5, 6)
#' ) 
plot2.efflist <-
  function (x,
            main = NULL,
            factor.names = FALSE,
            nrow = NULL, ncol = NULL,
            rel_widths = 1, rel_heights = 1,
            ylab = NULL, xlab = NULL, labels = NULL,
            cex = 1.1,
            cex.x = cex * .8, cex.y = cex * .8,
            cex.xlab = cex, cex.ylab = cex,
            cex.strip = cex * 1.2,
            cex.title = cex, cex.key = cex * 0.75, 
            lty = 1,
            lty.factor = 0,
            axes = NULL,
           
            bottom.padding = 0,
            top.padding = 0,
            left.padding = .4,
            right.padding = 1,
            axis.padding = .4,
      
            
            plot = TRUE,
            rug = FALSE,
            multiline =NULL,
            x.var = 1,
            space = "right",
            columns = 1,
            key.args = list(
              space = space,
              columns = columns,
              cex.title = cex.title,
              cex = cex.key
            ),
           par.strip.text = list(),
            #https://stackoverflow.com/questions/13026196/how-to-nicely-rescale-lattice-figures
           par.settings = list(
             add.text       = list(cex = cex.strip),
              par.zlab.text = list(cex = cex.xlab),
              par.ylab.text = list(cex = cex.ylab),
              par.xlab.text = list(cex = cex.xlab)), 
            ...)
  {
    plotlist <- list()
    param <- purrr::map(x, \(xfit) names(xfit$variables))
    x <- x[!duplicated(param)]
    
    effects <- gsub(":", "*", names(x))
    eff_names <-
      c(x[[1]]$response, unlist(strsplit(effects, "\\*")))
    
    if (!is.null(labels)){
      labels <- labels[eff_names]
      unlabl <- which(is.na(labels))
      labels[unlabl] <- eff_names[unlabl]
      names(labels)[unlabl] <- eff_names[unlabl]
    }
    else{
      # ylab und xlab aufdröseln
      labels <- eff_names
      names(labels) <- eff_names
      if (!is.null(ylab)) {
        labels[1] <- ylab
      }
      if (!is.null(xlab)) {
        for (i in seq_along(xlab))
          if (!is.na(xlab[i]))
            labels[i + 1] <- xlab[i]
      }
    }
 
    if(is.null(axes))
      axes <-  axis_list(effects = effects,
                         labels, cex.x, cex.y)
    
    
    # für Reset Lattice 
    layout.heights = lattice::lattice.getOption("layout.heights")
    layout.widths = lattice::lattice.getOption("layout.widths") 
    resetaxis.padding  <-  lattice::lattice.getOption("axis.padding")$numeric

    
    set_temp_lattice(bottom.padding,
                     top.padding,
                     left.padding,
                     right.padding,
                     axis.padding)


    
    for (i in seq_along(effects)) {
      is_fctr <-
        unlist(purrr::map(x[[i]]$variables, \(x) x$is.factor))
      
      if (all(is_fctr)) lty2 <- lty.factor
      else lty2 <- lty
      
      if (is.null(multiline)) {
        if (any(is_fctr)) multiline_i <- FALSE
        else multiline_i <- TRUE
      }
      else multiline_i <- multiline
      
      # das geht sicher einfacher 
      effects_i <- unlist(strsplit(effects[i], "\\*"))
      axes_i <- list( y = axes$y, x = list(cex= axes$x$cex))
       for (j in effects_i)
         axes_i$x[[j]]$lab <- axes$x[[j]]$lab

      if (length(effects_i) == 2 & multiline_i) {
      key.args$title <- axes$x[[effects_i[-x.var]]]$lab
      }
      
      plotlist[[effects[i]]]  <-
        update(
          effects:::plot.eff(
            x[[i]],
            factor.names = factor.names,
            main = "",
            rug = rug,
            lty = lty2,
            axes = axes_i,
            multiline = multiline_i,
            key.args = key.args,
            x.var = x.var,
            ...
          ),
          par.settings = par.settings,
          par.strip.text = par.strip.text
        )
     
    }
    
    # Reset Lattice
    set_temp_lattice(  layout.heights$bottom.padding$x,
                       layout.heights$top.padding$x,
                       layout.widths$left.padding$x,
                       layout.widths$right.padding$x,
                       resetaxis.padding)
    
    
    if (plot)
      cowplot::plot_grid(plotlist = plotlist,

                         nrow = nrow,
                         ncol = ncol,
                         rel_widths = rel_widths,
                         rel_heights  = rel_heights,
                         labels = main
                         
                         )
    else
      plotlist
  }


#' make a list for the axix
axis_list <- function(effects,
                      # response,
                      labels = NULL,
                      cex.x = 1,
                      cex.y = 1) {
  axs_list <- list(y = list(cex = cex.y))
  axs_list$y$lab <- as.vector(labels[1])
  axs_list$x <- list(cex = cex.x)
  
  for (i in names(labels[-1])) {
    axs_list$x[[i]] <- list(lab = as.vector(labels[i]))
    
  }
  axs_list
  
  
}

set_temp_lattice <- function( 
    bottom.padding,
                             top.padding,
                             left.padding,
                             right.padding,
                             axis.padding) {
  lattice::lattice.options(
    layout.heights =
      list(
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

}



# 
#  
# 
# plot_allEffects(fit,
#                 labels = get_label(mtcars2)
# )
# 
#  lattice::lattice.getOption("axis.padding")