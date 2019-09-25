#' panel.mean
#' 
#' Erstellt Mittelwertdiagramm Mittelwerte in bwplots copie von panel.average()
#'
#'
#' @export
#' @examples
#' 
#'  set.seed(1)
#' res = data.frame(coef=rnorm(99) + (-1):1,
#'                  habitat=sample(letters[1:4], 99, TRUE),
#'                  grp=c("W", "X", "Y"))
#' 
#' 
#' 
#' 
#' 
#' bwplot(coef ~ habitat | grp, data = res, type="p",
#'        panel=function(...){
#'          #  panel.bwplot(..., pch='|')
#'        
#'          panel.mean(...)
#'        }
#' )
#' 
#' 
panel.mean <-
  function (x,
            y,
            fun = mean,
            horizontal = TRUE,
            lwd = reference.line$lwd,
            lty = reference.line$lty,
            col= trellis.par.get("superpose.symbol")$col,
            col.line = reference.line$col,
            type = "p",   # c("p", "l"),
            pch = trellis.par.get("superpose.symbol")$pch,
            ...,
            identifier = "linejoin")
  {
    x <- as.numeric(x)
    y <- as.numeric(y)
    reference.line = trellis.par.get("reference.line")
 
    if (horizontal) {
      vals <- unique(sort(y))
      yy <- seq_along(vals)
      xx <- numeric(length(yy))
      for (i in yy)
        xx[i] <- fun(x[y == vals[i]])
      
      if (any(type %in% "p"))
        panel.points(xx, vals[yy], pch = pch, ...)
      if (any(type %in% "l"))
        panel.lines(
          xx, vals[yy],
          col = col.line, lty = lty, lwd = lwd, ...,
          identifier = identifier
        )
    }
    else {
      vals <- unique(sort(x))
      xx <- seq_along(vals)
      yy <- numeric(length(xx))
      for (i in xx)
        yy[i] <- fun(y[x == vals[i]])
      
      if (any(type %in% "p"))
        panel.points(vals[xx], yy, pch = pch, ...)
      if (any(type %in% "l"))
        panel.lines(
          vals[xx], yy,
          col = col.line,lty = lty, lwd = lwd, ...,
          identifier = identifier
        )
    }
  }


# panel.mean
# function(x, y, ...) {
#   if(any(class(x)=="factor"))  {
#     tmp <- tapply(y, factor(x), FUN = mean, na.rm=TRUE)
#     panel.points( tmp , pch = 16, ...)
#   }
#   else if(any(class(y)=="factor")) {
#     tmp <- tapply(x, factor(y), FUN = mean, na.rm=TRUE)
#     panel.points(x=tmp,y=1:nlevels(factor(y)), pch = 16, ...)
#   }
#   else {cat("Eine Variable muss ein Faktor sein!")}
#}
