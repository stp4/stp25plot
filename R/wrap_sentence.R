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
wrap_sentence<- function(x, width = 25, sep =NULL)
{
  if(is.data.frame(x)) x<- GetLabelOrName(x)
  
  x<- gsub("\r?\n|\r", " ", x)
  if(is.null(sep)) stringr::str_wrap(x, width = width)
  else gsub("\n", sep, stringr::str_wrap(x, width = width))
  
 
}

