#' multi_barplot
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
#' #'set.seed(1)
#' n<-99
#' DF<- data.frame(
#'   Magazines = rbinom(n, 1,prob=.75),
#'   Comic.books =rbinom(n, 1,prob=.25),
#'   Fiction = rbinom(n, 1,prob=.5),
#'   Sonstiges = rbinom(n, 1,prob=.35)
#' )
#' 
#' 
#' DF <- transform(DF, sex = cut(rnorm(n), 2, c("m", "f")))
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
            wrap = TRUE,
            use.level = 1)
  {
    dat <- stp25stat2::Summarise(
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
    dat$variable <- rev(dat$variable)
    print(reorder)
    if (reorder)
      dat$variable <- stp25tools::reorder2(dat$variable, dat$value,
                                           last = last)
    if (is.logical(wrap)) {
      if (wrap)
        dat$variable <- stp25tools:::wrap_sentence(dat$variable,
                                                   35)
      else
        dat$variable <- stp25tools:::wrap_sentence(dat$variable,
                                                   wrap)
    }
    fm <- "variable ~ value"
    if (ncol(dat) > 2)
      fm <- paste(fm, "|", names(dat)[1])
    p <- lattice::barchart(formula(fm),
                           dat,
                           origin = origin,
                           xlab = xlab,
                           main = main)
    return(p)
  }

# 
# DF %>% 
#   multi_barplot(  Health.wellbeing,
#                   Sporting.performance,
#                   Food.scandals,
#                   Animal.welfare,
#                   Ecological,
#                   Social.aspects,
#                   Economic.aspects,
#                   Religion.spirituality,
#                   Custom.tradition,
#                   Taste.enjoyment,
#                   by =  ~ diet_flex,
#                   include.percent=TRUE)