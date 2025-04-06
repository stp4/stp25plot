#' bwplot2
#' 
#' 
#' @description Lattice bwplot mit groups. Ist eine erweiterung von lattice \link{bwplot}. Die Funktion arbeitet mit \link{panel.superpose}.
#' @param ... alles an lattice
#' @param box.width Box breite
#' @param space_between Box breite
#' @param nlevels anzahl an Gruppen
#' @param panel eigene panel funktion zB mit  panel.abline(v=3)
#' @return NULL
#' @export
#' @examples
#' set.seed(2)
#'
#' res = data.frame(coef=rnorm(99) + (-1):1,
#'                  habitat=sample(letters[1:4], 99, TRUE),
#'                  grp=c("W", "X", "Y"))
#'
#' bwplot2(
#'   coef ~ habitat,
#'   data = res,  groups = grp,
#'   main = "bwplot",
#'   pch = 15,
#'   space_between = 1.10,
#'   box.width = .15
#' )
#'
#'
#'
#' # bwplot2(
#' #   coef ~grp ,
#' #   data = res,  groups =habitat ,
#' #   main = "bwplot",
#' #   pch = 15,
#' #   space_between = 1.10,
#' #  box.width = .15,
#' #   auto.key = list(columns=3)
#' # )
#'
#' bwplot2(
#'   habitat  ~ coef,
#'   data = res,
#'   groups = grp,
#'   main = "bwplot",
#'   pch = 15,
#'   auto.key = list(columns = 3),
#'   par.settings = stp25output::set_lattice_bw(col = grey.colors(4, start = 0.4, end = 0.9)),
#'   panel = function(...) {
#'     dots <- list(...)
#'     lattice::panel.abline(v = 0, lty = 1, col = "gray20")
#'     lattice::panel.superpose(...,
#'                              space_between = 1.1,
#'                              nlevels = nlevels(dots$groups))
#'
#'     lattice::panel.abline(v = 0, lty = 3, col = "gray80")
#'   }
#'   )
bwplot2 <- function (x, data, ...)
    UseMethod("bwplot2")

#' @rdname bwplot2
#' @method bwplot2 data.frame
#' @export
bwplot2.data.frame <- function (data, x , ...) {
    bwplot2.formula(x, data, ...)
  }


#' @rdname bwplot2
#' @method bwplot2 formula
#' @export
bwplot2.formula <- function(...,
                            xlab = NULL,
                            ylab = NULL,
                            auto.key = list(),
                            box.width = 1 / 4,
                            space_between = 1.2,
                            par.settings = list(
                              superpose.symbol = 
                                list(fill =lattice::trellis.par.get()$superpose.symbol$col)),
                            panel = function(...) {
                              dots <- list(...)
                              lattice::panel.superpose(...,
                                                       space_between = space_between,
                                                       nlevels = nlevels(dots$groups))
                            },
                            panel.groups = panel.bwplot.groups) {
  lattice::bwplot(
    ...,
    xlab = xlab,
    ylab = ylab,
    box.width = box.width,
    auto.key = auto.key,
    par.settings = par.settings,
    panel = panel,
    panel.groups = panel.groups
  )
  
}
 

#' @rdname bwplot2
#' @details Die Funktion kann nicht mit den 
#' funktionen von lattice kombiniert werden.
#'
panel.bwplot.groups <- function(x,
                                y,
                                ...,
                                group.number,
                                nlevels,
                                space_between) {
  dots <- list(...)
  nshift <- (nlevels - 1) / 2 + 1
  box_wd <- dots$box.width * space_between
  
  if (dots$horizontal) {
    xx <- x
    yy <- y + (group.number - nshift) * box_wd
  }
  else{
    xx <- x + (group.number - nshift) * box_wd
    yy <- y
  }
  lattice::panel.bwplot(xx, yy, ...)
}