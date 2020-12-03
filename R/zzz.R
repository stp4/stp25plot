


useLabels<- function(x,
                     lbl=NULL,
                     data,
                     fit=NULL
){
  if (is.null(lbl)) {
    if(is.null(fit)) lbl<- get_label(data)
    else lbl<- get_label(data[all.vars(fit$formula)])
    
  }
  
  if(is.data.frame(x))  x[,1] <- replaceLabel(x[,1], lbl)
  else  x<- replaceLabel(x, lbl)
  
  
  # print(class(data))
  x
}

replaceLabel<- function(x, lbl){
  x<- gsub("\\[T.","\\[", x)
  x<- gsub("\\[TRUE\\]","", x)
  for( i in names(lbl))
    x <- stringr::str_replace(x, i , lbl[i])
  
  print(lbl)
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


