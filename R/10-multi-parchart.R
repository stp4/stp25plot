


# setup -------------------------------------------------------------------


#+ setup, include=FALSE
# knitr::opts_chunk$set(echo = TRUE, warnings=FALSE)

#' Title
#'
#' @param ... an Summarise
#' @param reorder,last  an reorder2
#' @param main,ylab,origin,xlab an Lattice
#' @param percent summary
#' @return lattice plot
#' @export
#'
#' @examples
#'
#'  require(tidyverse)
#' require(stp25output2)
#' require(stp25stat2)
#' require(stp25tools)
#'
#'
#' setwd("C:/Users/wpete/Dropbox/1_Projekte/851_Kardelen_Cetin")
#'
#' load('Processed data/KardelenCetin.Rdata')
#'
#' complications$gender <- DF$gender
#' complications %>%
#'   multi_barplot(. ~ gender,
#'                 reorder=TRUE,
#'                 last= c("Others", "No Complications"))
#'
#' complications[1:5] %>%
#'   multi_barplot(  )
#'
multi_barplot <- function(...,
                          reorder = FALSE,
                          last = NULL,
                          main = "",
                          
                          ylab = "",
                          percent = FALSE,
                          origin = 0,
                          xlab = if (percent) "Percent" else "Count") {
  dat <-
    stp25stat2::Summarise(
      ...,
      fun = function(x) {
        x <- as.numeric(na.omit(x))
        if (percent)
          mean(x) * 100
        else
          sum(x)
      }
    )
  
  
  if (reorder)
    dat$variable <-
      stp25tools::reorder2(dat$variable, dat$value, last = last)
  
  fm <- "variable ~ value"
  if (ncol(dat) > 2)
    fm <- paste(fm ,  "|",  names(dat)[1])
  p <-  
    lattice::barchart(formula(fm),
                          dat,
                          origin = origin,
                          xlab = xlab)
  
  return(p)
}
# fun ---------------------------------------------------------------------

# lattice::barchart(
#   reorder2(variable, value, last = c("Others", "No Complications")) ~ value |
#     gender,
#   dat,
#   origin = 0,
#   xlab = "Percent"
# )
#
#
# table(dat$variable)
# table(reorder2(dat$variable,   last = c("Others", "No Complications")))
