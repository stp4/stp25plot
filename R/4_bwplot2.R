#' bwplot2
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
bwplot2 <- function(...,
                    xlab = NULL,
                    ylab = NULL,
                    auto.key = list(),
                    box.width = 1 / 4,
                    space_between = 1.2,
                    par.settings = list(superpose.symbol = list(fill = lattice::trellis.par.get()$superpose.symbol$col)),
                    panel = NULL) {
  panel_default <-  function(...) {
    dots <- list(...)
    # message("In panel")
    #  print(dots)
    lattice::panel.superpose(...,
                             space_between = space_between,
                             nlevels = nlevels(dots$groups))
  }
  
  if (is.null(panel))
    lattice::bwplot(
      ...,
      xlab = xlab,
      ylab = ylab,
      box.width = box.width,
      auto.key = auto.key,
      par.settings = par.settings,
      panel = panel_default,
      panel.groups = panel.groups.bwplot
    )
  else
    lattice::bwplot(
      ...,
      xlab = xlab,
      ylab = ylab,
      box.width = box.width,
      auto.key = auto.key,
      par.settings = par.settings,
      panel = panel
      
    )
  
}
panel.groups.bwplot <- function(x,
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



# bwplot2 <-function(...,
#                    xlab=NULL,
#                    ylab=NULL,
#                    auto.key=list(), # pch=15, lwd=2.5, points=FALSE,  columns=3, rectangles =TRUE
#                    box.width = 1/4,
#                    space_between=1.2,
#                    nlevels=2 ,
#                    par.settings = list(
#                        superpose.symbol=list(
#                          fill=lattice::trellis.par.get()$superpose.symbol$col)
#                        ),
#                    panel=function(...){
#                      lattice::panel.superpose(...)
#                         # panel.abline(v=3)
#                    }
#
# ){
#
#   lattice::bwplot( ...,
#                    xlab = xlab,
#                    ylab = ylab,
#                    box.width = box.width,
#                    auto.key = auto.key,
#                    par.settings = par.settings,
#                    panel = panel,
#                    space_between = space_between,
#                    nlevels = nlevels,
#                    panel.groups = function(x,
#                                            y,
#                                            ...,
#                                            group.number,
#                                            nlevels = nlevels,
#                                            space_between = space_between) {
#                      dots <- list(...)
#                      nshift <- (nlevels - 1) / 2 + 1
#                      box_wd <- dots$box.width * space_between
#                      if (dots$horizontal)
#                        lattice::panel.bwplot(x, y + (group.number - nshift) * box_wd, ...)
#                      else
#                        lattice::panel.bwplot(x + (group.number - nshift) * box_wd, y, ...)
#                    })
#
#
# }
