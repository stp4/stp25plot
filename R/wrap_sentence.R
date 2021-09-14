#' @name wrap_sentence
#' @title wrap_sentence
#' @description Kopie von \link{str_wrap} wobei die Labels mit
#' upData2 ergaenzt werden wenn ein Data.Frame-Objekt uebergeben wird
#' @param x data.frame oder String
#' @param width default width= 25
#' @param sep  default new line
#' @return Character String
#' @export
#' @examples
#'
#' wrap_sentence(
#' "R is free software and comes with ABSOLUTELY NO WARRANTY.
#' You are welcome to redistribute it under certain conditions.
#' "
#' )
#' wrap_sentence(
#'   "R is free software and comes with ABSOLUTELY NO WARRANTY.
#' You are welcome to redistribute it under certain conditions.", sep="<br>"
#' )

#Verschoben nach 
wrap_sentence <- function(x, 
                          ...) {
  if (is.data.frame(x))
    x <- stp25tools::get_label(x, include.units = TRUE)
  stp25tools::wrap_string(x,  ...)
}
 