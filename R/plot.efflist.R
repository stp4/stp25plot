#' Modifiziert effect-Plot
#'
#' Soll nur im Notfall geladen werden geht nicht Mehr
#' @param x Model fit
#' @param selection,rows,cols,ask,graphics alles nicht ändern
#' @param xlab  xlabs Ändern
#' @param ...
#'
#' @return Plot
#'
#' @examples
#' library(effects)
#' library(gridExtra)
#' A = rnorm(100)
#' B = rnorm(100)
#' C = factor(rep(c("This", "That"), 50))
#'
#' #-- Modifiziert wegen xlab
#'
#' plot.efflist <- stp25:::plot.efflist
#'
#' ef <- allEffects(lm(A ~ B + C))
#' plot(ef, xlab = c("Foo", "Bar"))
plot.efflist <- function(x, selection,
                                 rows, cols,
                                 ask=FALSE, # nicht vom mir benutzt
                                 graphics=TRUE,
                                 lattice,
                                 xlab,
                                 ...){
  # Next line added 8/23/17 along with lattice, also lattice arg above
  lattice <- if(missing(lattice)) list() else lattice
  if (!missing(selection)){
    if (is.character(selection)) selection <- gsub(" ", "", selection)
    return(plot(x[[selection]], ...))
  }
  effects <- gsub(":", "*", names(x))
  # if (ask){
  #   repeat {
  #     selection <- menu(effects, graphics=graphics, title="Select Term to Plot")
  #     if (selection == 0) break
  #     else print(plot(x[[selection]], ...))
  #   }
  # }
 #  else {

  #effects:::mfrow
    neffects <- length(x)
    mfrow <- effects:::mfrow(neffects)
    if (missing(rows) || missing(cols)){
      rows <- mfrow[1]
      cols <- mfrow[2]
    }
    for (i in 1:rows) {
      for (j in 1:cols){
        if ((i-1)*cols + j > neffects) break
        more <- !((i-1)*cols + j == neffects)
        lattice[["array"]] <- list(row=i, col=j, nrow=rows, ncol=cols, more=more)

        if (missing(xlab)) {
          print(plot(x[[(i - 1) * cols + j]], lattice = lattice, ...))
        }  else {
          k <- (i - 1) * cols + j
          x_lab <-
            if (is.null(names(xlab)))
              xlab[k]
          else
            xlab[names(x[k])]

          print(plot(x[[k]], lattice = lattice, xlab = x_lab, ...))

        }
      }
    }
  #}
}



