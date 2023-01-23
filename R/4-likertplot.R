#' Barcharts for Likert
#'
#' Constructs and plots diverging stacked barcharts for Likert (copie from HH:::plot.likert.formula)
#'
#' @param x formula
#' @param data daten
#' @param main,ylab,sub,xlab Beschriftung
#' @param col HH::brewer.pal.likert
#' @param wrap Zeien Umbrechen
#' farbe("likert.blue.red", data$nlevels, middle = ReferenceZero)
#' @param rightAxis,as.percent,ReferenceZero,reference.line.col,col.strip.background
#' an HH:::plot.likert.formula
#' @param positive.order das nicht verwenden - wird ueber Tbll_likert gesteuert
#' @param auto.key,columns,space,between   columns = 2,
#' @param ... HH:::plot.likert.formula
#'
#' @return lattice Plot
#' @export
#'
#' @examples
#' 
#' #require(stp25plot)
#' require(stp25stat2)
#' set.seed(1)
#' n <- 100
#' lvs <- c("--", "-", "o", "+", "++")
#' DF2 <- data.frame(
#'   Magazines = gl(length(lvs), 1, n, lvs),
#'   Comic.books = gl(length(lvs), 2, n, lvs),
#'   Fiction = gl(length(lvs), 3, n, lvs),
#'   Newspapers = gl(length(lvs), 5, n, lvs)
#' )
#' 
#' 
#' 
#' DF2$Comic.books[sample.int(n / 2)] <- lvs[length(lvs)]
#' DF2$Newspapers[sample.int(n / 2)] <- lvs[1]
#' DF2$Magazines[sample.int(n / 2)] <- lvs[2]
#' 
#' DF2 <- transform(DF2, Geschlecht = cut(rnorm(n), 2, Hmisc::Cs(m, f)))
#' Res1 <- Tbll_likert( ~ ., DF2[, -5])
#' Res2 <- Tbll_likert(. ~ Geschlecht, DF2)
#' 
#' #require(HH)  # ?likertplot
#' class(Res2)
#' windows(7, 3)
#' #attr(Res2, "plot")$results 
#' likertplot(Item   ~ . | Geschlecht , data = Res2)
#' 
#' 
likertplot <- function(x = Item   ~ . ,
                       data,
                       main = '',
                       ylab = "",
                       sub = "",
                       xlab = "Prozent",
                       col = NULL,
                       rightAxis = FALSE,
                       positive.order = FALSE, 
                       as.percent = TRUE,
                       auto.key = list(space = space, 
                                       columns = columns,
                                       between = between),
                       
                       ReferenceZero = NULL,
                       reference.line.col = "gray65",
                       col.strip.background = "gray97",
                       wrap = TRUE,
                       columns = 2,
                       space ="top",
                       between = 1,
                       ...) {
  
  if (is.data.frame(x)) {
    data <- x
    x <- attr(x, "plot")$formula
  }
  
  
  if (!is.null(attr(data, "plot"))) {
    
    if (is.null(ReferenceZero)) {
      ReferenceZero <-  attr(data, "plot")$ReferenceZero
    }
    
    
    if (is.null(col)) {
      col <- if (is.null(ReferenceZero))
        likert_col(attr(data, "plot")$nlevels)
      else
        likert_col(attr(data, "plot")$nlevels,
                   middle = ReferenceZero)
    }
    
    
    if (is.logical(wrap)) {
      if (wrap) {
        attr(data, "plot")$results$Item <-
          stp25tools:::wrap_sentence(as.character(attr(data, "plot")$results$Item),
                        35)
        
      }
    } 
    else{
      attr(data, "plot")$results$Item <-
        stp25tools:::wrap_sentence(as.character(attr(data, "plot")$results$Item),
                      wrap)
      
    }
    
    HH:::plot.likert.formula(
      x,
      attr(data, "plot")$results,
      main = main,
      ylab = ylab,
      sub = sub,
      xlab = xlab,
      col = col,
      rightAxis = rightAxis,
      positive.order = positive.order,
      as.percent = as.percent,
      auto.key = auto.key,
      ReferenceZero =  ReferenceZero,
      reference.line.col = reference.line.col,
      col.strip.background = col.strip.background,
      
      ...
    )
  }
  else if (inherits(data, "likert")) {
    
    
    if(is.null(ReferenceZero)){ 
      ReferenceZero<-  data$ReferenceZero 
    } 
    
    
    if (is.null(col)){
      col <- if (is.null(ReferenceZero))
        likert_col(data$nlevels)
      else
        likert_col(data$nlevels,
                         middle = ReferenceZero)}
    
    
    HH:::plot.likert.formula(
      x,
      data$results,
      
      main = main,
      ylab = ylab,
      sub = sub,
      xlab = xlab,
      col = col,
      rightAxis = rightAxis,
      positive.order = positive.order,
      as.percent = as.percent,
      auto.key = auto.key,
      ReferenceZero = ReferenceZero,
      reference.line.col = reference.line.col,
      col.strip.background = col.strip.background,
      
      ...
    )
  }
  else{
    if (is.null(col))
      col <-  stp25plot::farbe("likert.blue.red", 5)
    HH:::plot.likert.formula(
      x,
      data,
      main = main,
      ylab = ylab,
      sub = sub,
      xlab = xlab,
      col = col,
      rightAxis = rightAxis,
      positive.order = positive.order,
      as.percent = as.percent,
      auto.key = auto.key,
      ReferenceZero = ReferenceZero,
      reference.line.col = reference.line.col,
      col.strip.background = col.strip.background,
      ...
    )
  }
  
  
}



#' likert_col
#'
#' Farben:
#'   Greens,  Blues,  Reds,  Greys, Oranges, Purples
#'
#' @param n Number of different colors in the palette
#' @param name A palette name from the lists below "RdBl"  ist   RdBl = c("Reds", "Blues")
#' @param middle,middle.color reference   "gray65"
#'
#' @return character
#' @export
#'
#' @examples
#'
#' par(mfrow=c(1,3))
#' barplot(cbind(1:5, rep(3,5)),  horiz = TRUE,  col=likert_col(5 , "RdBl"))
#' barplot(cbind(1:3, rep(3,3)),  horiz = TRUE,  col=likert_col(3 , "RdBl"))
#' barplot(cbind(1:8, rep(3,8)),  horiz = TRUE,  col=likert_col(8 , "RdBl"))
#'
#'
#'
likert_col <- function(n = 5,
                       name =  "RdBl" ,
                       # c("RdBl", "BlRd", "RdGr", "GrRd","GrBl", "BlGr","Bw"),
                       middle = mean(1:n),
                       middle.color =  "gray90") {
  stp25settings:::likert_col( n=n, name=name, middle=middle, middle.color=middle.color)
}




