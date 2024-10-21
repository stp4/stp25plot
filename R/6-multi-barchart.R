#' multi_barplot
#' 
#' Adaption von `lattice::barchart()` und Feineinstellung können 
#' mit `lattice::update.trellis` erstellt werden.
#'
#' @param ... an Summarise
#' @param reorder,last  an reorder2
#' @param main,ylab,origin,xlab an Lattice
#' @param include.percent summary
#' @return lattice plot
#' @export
#'
#' @examples
#'
#' n <-99
#' DF <- data.frame(
#'   Magazines = rbinom(n, 1,prob=.75),
#'   Comic.books =rbinom(n, 1,prob=.25),
#'   Fiction = rbinom(n, 1,prob=.5),
#'   Sonstiges = rbinom(n, 1,prob=.35)
#' ) |> transform(sex = cut(rnorm(n), 2, c("m", "f")))
#' 
#' 
#' multi_barplot(  DF, .~ sex, last="Sonstiges")
#' 
#' 
multi_barplot<-
  function (...,
            reorder = TRUE,
            last = NULL,
            main = NULL,
            ylab = "",
            include.percent = FALSE,

            origin = 0,
            xlab = if (include.percent) "Percent" else "Count",
            xlim = NULL,
            ylim = NULL,
            wrap = TRUE,
            use.level = 1)
  {
    dat_sum <- stp25stat2::Summarise(
      ...,
      fun = function(x) {
        n_tot <- length(x)
        x <- na.omit(x)
        if (is.factor(x))
          x <-  x == levels(x)[use.level]
        if (include.percent)
          mean(x) * 100
        else
          sum(x)
      }
    )
    dat_sum$variable <- rev(dat_sum$variable)

    if (reorder)
      dat_sum$variable <- stp25tools::reorder2(dat_sum$variable, 
                                           dat_sum$value,
                                           last = last)

    if (is.logical(wrap)) {
      if (wrap)
        dat_sum$variable <- stp25tools::wrap_factor(dat_sum$variable, 35)
    } else if (is.numeric(wrap)) {
      dat_sum$variable <- stp25tools::wrap_factor(dat_sum$variable, wrap)
    }
    
    
    fm <- "variable ~ value"
    if (ncol(dat_sum) > 2)
      fm <- paste(fm, "|", names(dat_sum)[1])
    
    if(is.null(xlim) & is.null(ylim)){
    p <- lattice::barchart(formula(fm),
                           dat_sum,
                           origin = origin,
                           xlab = xlab,
                           main = main)
    } else     if(!is.null(xlim) & is.null(ylim)){
      p <- lattice::barchart(formula(fm),
                             dat_sum,
                             origin = origin,
                             xlab = xlab,
                             
                              xlim = xlim,
                             # ylim,
                             main = main)
    } else{
      
      stop( "ylim noch nicht implementiert!!")
    }
    
    
    return(p)
  }

 