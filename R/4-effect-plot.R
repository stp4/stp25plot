#' Workaround for effects::plot.eff
#'
#' @name plot.efflist
#' @param ... alles an plot
#' @param xlevels effects::effect the number of levels for any focal numeric predicto  xlevels=list(x1=c(2, 4.5, 7), x2=4)
#' @param predictor   ~ ., a predictor
#' @return standard plot
#' @export
plot_allEffects <- function(x,
                            predictor = NULL,
                            main = NULL,
                            factor.names = FALSE,
                            rel_widths = 1,
                            rel_heights = 1,
                            ylab = NULL,
                            xlab = NULL,
                            labels = NULL,
                            cex = 1.1,
                            axis.padding = .4,
                            multiline = NULL,
                            x.var = 1,
                            space = "right",
                            columns = 1,
                        
                            xlevels = NULL,
                            select = NULL, remove =NULL,
                            order = NULL,
                            ...) {

  if (inherits(x, "efflist"))
    plot2(
      x,
      main = main,
      factor.names = factor.names,
      rel_widths = rel_widths,
      rel_heights = rel_heights,
      ylab = ylab,
      xlab = xlab,
      labels = labels,
      cex = cex,
      axis.padding = axis.padding,
      multiline = multiline,
      x.var = x.var,
      space = space,
      columns = columns,
      select = select, remove = remove,
      order = order,
      ...
    )
  else if( is.null(predictor))
    plot2(
      effects::allEffects(x, xlevels = xlevels),
      main = main,
      factor.names = factor.names,
      rel_widths = rel_widths,
      rel_heights = rel_heights,
      ylab = ylab,
      xlab = xlab,
      labels = labels,
      cex = cex,
      axis.padding = axis.padding,
      multiline = multiline,
      x.var = x.var,
      space = space,
      columns = columns,
      select = select, remove = remove,
      order = order,
      ...
    )
  else if( is.formula(predictor) )
  plot2(
    effects::predictorEffects(x, predictor,  xlevels = xlevels),
    main = main,
    factor.names = factor.names,
    rel_widths = rel_widths,
    rel_heights = rel_heights,
    ylab = ylab,
    xlab = xlab,
    labels = labels,
    cex = cex,
    axis.padding = axis.padding,
    multiline = multiline,
    x.var = x.var,
    space = space,
    columns = columns,
    select = select, remove = remove,
    order = order,
    ...
  )
  else return(class(x))
  
  
  
}
# # lib effects
# 
# is.relative <-   function (term1, term2, factors) {
#   all(!(factors[, term1] & (!factors[, term2])))
# }
# term.names<- function (model, ...) {
#   term.names <- gsub(" ", "", labels(terms(model)))
#   if (has.intercept(model)) 
#     c("(Intercept)", term.names)
#   else term.names
# }
# 
# has.intercept <-  function (model, ...) any(names(coefficients(model)) == "(Intercept)")
# descendants <-  function (term, mod, ...)  {
#   names <- term.names(mod)
#   if (has.intercept(mod)) 
#     names <- names[-1]
#   if (length(names) == 1) 
#     return(NULL)
#   which.term <- which(term == names)
#   if (length(which.term) == 0) {
#     factors <- attr(terms(...), "factors")
#     rownames(factors) <- gsub(" ", "", rownames(factors))
#     colnames(factors) <- gsub(" ", "", colnames(factors))
#     (1:length(names))[sapply(names, function(term2) is.relative(term, 
#                                                                 term2, factors))]
#   }
#   else {
#     factors <- attr(terms(mod), "factors")
#     rownames(factors) <- gsub(" ", "", rownames(factors))
#     colnames(factors) <- gsub(" ", "", colnames(factors))
#     (1:length(names))[-which.term][sapply(names[-which.term], 
#                                           function(term2) is.relative(term, term2, factors))]
#   }
# }
# 
# allEffects_default <- 
# function (mod, ...) 
# {
#   high.order.terms <- function(mod) {
#     names <- term.names(mod)
#     if (has.intercept(mod)) 
#       names <- names[-1]
#     rel <- lapply(names, descendants, mod = mod)
#     (1:length(names))[sapply(rel, function(x) length(x) == 
#                                0)]
#   }
#   names <- term.names(mod)
#   if (has.intercept(mod)) 
#     names <- names[-1]
#   if (length(names) == 0) 
#     stop("the model contains no terms (beyond a constant)")
#   terms <- names[high.order.terms(mod)]
#   result <- lapply(terms, effect, mod = mod, xlevels=xlevels)
#   
#   print(terms)
#   
#   names(result) <- terms
#   class(result) <- "efflist"
#   result
# }

#' @rdname plot.efflist
#' @export
plot2 <- function(...) {
  UseMethod("plot2")
}


#' @export
#' @rdname plot.efflist
plot2.default <- function(...) {
  plot(...)
}


#' @rdname plot.efflist
#'
#' @description
#'  method for class 'eff'  effects::allEffects
#'
#' @param x 	an object of class "efflist"
#' @param main 	the title for the plot, gows to cowplot
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
#' @param  select,order Auswahl der Predictor
#' 
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
            nrow = NULL,
            ncol = NULL,
            rel_widths = 1,
            rel_heights = 1,
            ylab = NULL,
            xlab = NULL,
            xlim = NULL,
            ticks = NULL, y.ticks = if(is.list(ticks)) ticks else list(at=ticks),
            x.ticks = NULL,
            labels = NULL,
            cex = 1.1,
            cex.x = cex * .8,
            cex.y = cex * .8,
            cex.xlab = cex,
            cex.ylab = cex,
            cex.strip = cex * 1.2,
            cex.title = cex,
            cex.key = cex * 0.75,
            cex.points = cex,
            lty = 1,
            lty.factor = 0, 
            multiline = NULL,
            x.var = 1,
            space = "right",
            columns = 1,
            axes = NULL,
            axis.padding = .4,
            plot = TRUE,
            rug = FALSE,
            layout = NULL,
            key.args = list(
              space = space,
              columns = columns,
              cex.title = cex.title,
              cex = cex.key
            ),
            bottom.padding = 0,
            top.padding = 0,
            left.padding = .4,
            right.padding = 1,
            par.strip.text = list(),
            #https://stackoverflow.com/questions/13026196/how-to-nicely-rescale-lattice-figures
            
            #  par.settings = ggplot2like(),
            par.settings = list(
              add.text      = list(cex = cex.strip),
              par.zlab.text = list(cex = cex.xlab),
              par.ylab.text = list(cex = cex.ylab),
              par.xlab.text = list(cex = cex.xlab),
              layout.heights =
                list(bottom.padding = bottom.padding,
                     top.padding    = top.padding),
              layout.widths =
                list(left.padding =  left.padding,
                     right.padding =  right.padding)
              #   axis.line = list(col="gray"),
              #   axis.text = list(col= "red"),
              #   strip.background =list( col = 'grey80')
            ),
            select = NULL, remove =NULL,
            order = NULL,
            ...)
  {
    plotlist <- list()
    param <- purrr::map(x, \(xfit) names(xfit$variables))
    x <- x[!duplicated(param)]
    
    effects_all <- effects <- gsub(":", "*", names(x))
    
    if(!is.null(remove)){
      if(is.character(remove))
        effects <- setdiff(effects, remove)
      else stop( "Bei remove sind nur die Parameter als Character-Namen erlaubt!")
      cat("\nremove: ", remove, "\n")
      pos <- which(effects_all %in% effects)
      x <- x[ pos]
     
    }
    if(!is.null(select)){
      if(is.character(select))
        effects <- intersect(effects,select )
      else stop( "Bei select sind nur die Parameter als Character-Namen erlaubt!")
      
      cat("\nselect: ", effects, "\n")
      pos <- which(effects_all %in% effects)
      x <- x[ pos]
    }
    if(!is.null(order)){
      if(is.numeric(order))  effects <- effects[order]
      else stop( "Bei order sind nur die Parameter als Numeric-Position erlaubt!")
    }
 
    
    
    eff_names <-
      c(x[[1]]$response, unlist(strsplit(effects, "\\*")))
    
    if (!is.null(labels)) {
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
    
    reset_axis <-
      lattice::lattice.getOption("axis.padding")$numeric
    lattice::lattice.options(axis.padding = list(numeric = axis.padding))
    
    
 
    

    
    
    
    
    for (i in seq_along(effects)) {
      # was kommt -------------------------------------------
       is_fctr <- unlist(purrr::map(x[[i]]$variables, \(x) x$is.factor))
       effects_i <- unlist(strsplit(effects[i], "\\*"))
      # lty --------------------------------------------------
      if (all(is_fctr))
        lty2 <- lty.factor
      else
        lty2 <- lty
      # Multiline -------------------------------------------
      if (is.null(multiline)) {
        if (any(is_fctr))
          multiline_i <- FALSE
        else
          multiline_i <- TRUE
      }
      else
        multiline_i <- multiline
      
      # Axes -----------------------------------------------
     
      axes_i <- list(y = list(lab = labels[1],
                              cex = cex.y),
                     x = list(cex = cex.x))
      for (j in effects_i)
        axes_i$x[[j]]$lab <- labels[[j]]
      
      if (length(effects_i) == 2 & multiline_i) {
        key.args$title <- labels[[effects_i[-x.var]]]
      }
       if (!is.null(xlim)) {
         if (is.list(xlim))
           for (j in effects_i)
             axes_i$x[[j]]$lim <- xlim[[j]]
         else {
           
           print( xlim)
           for (j in effects_i)
             axes_i$x[[j]]$lim <- xlim 
           }
           
         #  warning("xlim: Hier sollte eine Liste mit Namen uebergeben werden. list(sex = c(1 ,2) } ")
           
           
           #stop ("xlim: hier muss eine Liste mit Namen uebergeben werden. list(x = c(1,2)  ")
       }
       if (!is.null(x.ticks)) {
         if (is.list(x.ticks))
           for (j in effects_i) axes_i$x[[j]]$ticks <- x.ticks[[j]]
         else stop ("xticks: hier muss eine Liste mit Namen uebergeben werden. list(x = c(1,2)  ")
       }
         # layout --------------------------------------------

       layout.i <- NULL
       if (!is.null(layout)) {
         is_lay <- effects_i %in% names(layout)
         if (!multiline_i & any(is_lay))
           layout.i <- layout[[
             effects_i[
               which(is_lay)][1L]]]
       }  
   
      #----------------------------------------------------
      plotlist[[effects[i]]]  <-
        update(
          effects:::plot.eff(
            x[[i]],
            factor.names = factor.names,
            main = "",
            rug = rug,
            lty = lty2,
            ticks =y.ticks,
            axes = axes_i,
            multiline = multiline_i,
            key.args = key.args,
            x.var = x.var,
            layout =  layout.i,
          
            ...
          ),
          par.settings = par.settings,
          par.strip.text = par.strip.text
        )
      
    }
    
    lattice::lattice.options(
      axis.padding = list(numeric = reset_axis))
    
    if (plot)
      cowplot::plot_grid(
        plotlist = plotlist,
        nrow = nrow,
        ncol = ncol,
        rel_widths = rel_widths,
        rel_heights  = rel_heights,
        labels = main,
        hjust = 0 
      )
    else
      plotlist
  }

#'
#' #' make a list for the axix
#' axis_list <- function(effects,
#'                       # response,
#'                       labels = NULL,
#'                       cex.x = 1,
#'                       cex.y = 1) {
#'   axs_list <- list(y = list(cex = cex.y))
#'   axs_list$y$lab <- as.vector(labels[1])
#'   axs_list$x <- list(cex = cex.x)
#'
#'   for (i in names(labels[-1])) {
#'     axs_list$x[[i]] <- list(lab = as.vector(labels[i]))
#'
#'   }
#'   axs_list
#'
#'
#' }



# 
# 
# #require(ggplot2)
# require(stp25tools)
# #require(stp25stat2)
# #require(stp25plot)
# 
# 
# mtcars2 <- mtcars |> dplyr::mutate(
#   vs   = factor(vs, labels = c("V-shaped", "straight")),
#   am   = factor(am, labels = c("automatic", "manual")),
#   cyl  = ordered(cyl),
#   gear = ordered(gear),
#   carb = ordered(carb)
# )  |> Label(
#   mpg	 = "Miles/(US) gallon",
#   cyl	 = "Number of cylinders",
#   disp = "Displacement (cu.in.)",
#   hp	 = "Gross horsepower",
#   drat = "Rear axle ratio",
#   wt   = "Weight (1000 lbs)",
#   qsec = "1/4 mile time",
#   vs   = "Engine",
#   am   = "Transmission",
#   gear = "Number of forward gears",
#   carb = "Number of carburetors"
# )
# 
# fit <- lm(mpg ~ hp * wt + vs +  am * cyl  , data = mtcars2)
# 
# # lattice::trellis.par.set(effectsTheme())
# #lattice::trellis.par.set(bw_theme(farbe(n=5), lwd =1,  cex.xlab = 2))
# plot_allEffects(
#   fit,
#   # labels = get_label(mtcars2),
#   labels = c(wt = "HHHHHHHH", vs = "VVVVVVVVV", mpg = "Out"),
#   cex.x = 1.5,
#   cex.y = .5,
#   main = letters[1:3],
#   #as.roman(1:3),
#   # cex= 1,
#   space = "right",
#   columns = 1,
#   rel_widths = c(3, 4),
#   rel_heights = c(5, 6)
# )
# plot_allEffects(
#   fit,
#   ylim = c(0, 30),
#   ticks =c(5,10,30),
#   x.ticks = list(hp = list(at = c(50, 200, 300) )
#                
#                 ),
#   xlevels=list(  wt =c(1,2,3,4,5)),
#   xlim = list(hp = c(0, 300))
# )

# plot_allEffects(
#   fit,
#   labels = get_label(mtcars2),
#   main = letters[1:3],  #as.roman(1:3),
#   # cex= 1,
#   space = "right",
#   columns = 1,
#   rel_widths = c(3, 4),
#   rel_heights = c(5, 6),
#   remove = "vs"
# )