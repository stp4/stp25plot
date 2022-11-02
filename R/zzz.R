
#' @importFrom stp25settings farbe
#' @export
stp25settings::farbe

#' @importFrom stp25settings standard_theme
#' @export
stp25settings::standard_theme

#' @importFrom stp25settings ggplot_theme
#' @export
stp25settings::ggplot_theme

#' @importFrom stp25settings bw_theme
#' @export
stp25settings::bw_theme

#' @importFrom stp25settings reset_lattice
#' @export
stp25settings::reset_lattice

#' @importFrom stp25settings set_lattice
#' @export
stp25settings::set_lattice

 



cbPalette <- c(
  orange = "#E69F00",
  skyblue = "#56B4E9",
  green = "#009E73",
  yellow = "#F0E442",
  blue = "#0072B2",
  vermillion = "#D55E00",
  purple = "#CC79A7"
  # "#66C2A5",
  # "#FC8D62",
  # "#8DA0CB",
  # "#E78AC3",
  # "#A6D854" ,
  # "#FFD92F",
  # "#E5C494",
  # "#B3B3B3",
  # "#1B9E77" ,
  # "#D95F02",
  # "#7570B3" ,
  # "#E7298A" ,
  # "#66A61E" ,
  # "#E6AB02" ,
  # "#A6761D",
  # "#666666"

)

useLabels<- function(x,
                     lbl=NULL,
                     data,
                     fit=NULL){
  if (is.null(lbl)) {
    if(is.null(fit)) lbl<- get_label(data)
    else lbl<- get_label(data[all.vars(fit$formula)])
    
  }
  
  if(is.data.frame(x))  x[,1] <- replaceLabel(x[,1], lbl)
  else  x<- replaceLabel(x, lbl)

  x
}

replaceLabel<- function(x, lbl){
  x<- gsub("\\[T.","\\[", x)
  x<- gsub("\\[TRUE\\]","", x)
  for( i in names(lbl))
    x <- stringr::str_replace(x, i , lbl[i])

  x
}



#' Hilfsfunktion Tabelle
#' 
#' in pieplot verwendet
#' 
#' @noRd
to_table <- function(x, data, drop.unused.levels = FALSE, ...) {
  n <- length(all.vars(x))
  if (any(grepl("\\|", x)))
    x <- formula(paste(gsub("\\|", "+", x)))
  
  df_freq <-
    data.frame(stats::xtabs(x, data, drop.unused.levels = drop.unused.levels))
  df_perc <-
    data.frame(prop.table(stats::xtabs(x, data), ...) * 100)
  names(df_perc)[n + 1] <- "Percent"
  merge(df_freq, df_perc, by = 1:n, sort = FALSE)
}


