#' @name wrap_sentence
#' @title wrap_sentence
#' @description Kopie von \link{str_wrap} wobei die Labels mit
#' upData2 ergaenzt werden wenn ein Data.Frame-Objekt uebergeben wird
#' @param x data.frame oder String
#' @param width default width= 25
#' @return Character String
#' @export
wrap_sentence<- function(x, width = 25)
{
  if(is.data.frame(x)) x<- GetLabelOrName(x)
  stringr::str_wrap(x, width = width)
}
