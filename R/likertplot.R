#' Barcharts for Likert
#'
#' Constructs and plots diverging stacked barcharts for Likert (copie from HH:::plot.likert.formula)
#'
#' @param x formula
#' @param data daten
#' @param main,ylab,sub,xlab Beschriftung
#' @param col HH::brewer.pal.likert
#' farbe("likert.blue.red", data$nlevels, middle = ReferenceZero)
#' @param rightAxis,positive.order,as.percent,ReferenceZero,reference.line.col,col.strip.background
#' an HH:::plot.likert.formula
#' @param auto.key,columns    auto.key = list(space = "top", columns = columns)
#' @param ... HH:::plot.likert.formula
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
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
#' DF2$Comic.books[sample.int(n / 2)] <- lvs[length(lvs)]
#' DF2$Newspapers[sample.int(n / 2)] <- lvs[1]
#' DF2$Magazines[sample.int(n / 2)] <- lvs[2]
#'
#' DF2 <- transform(DF2, Geschlecht = cut(rnorm(n), 2, Hmisc::Cs(m, f)))
#' Res1 <- Likert( ~ ., DF2[, -5])
#' Res2 <- Likert(. ~ Geschlecht, DF2)
#'
#' #require(HH)  # ?likertplot
#' class(Res2)
#' windows(7, 3)
#' likertplot(Item   ~ . | Geschlecht , data = Res2)
likertplot <- function(x = Item   ~ . ,
                       data,
                       main = '',
                       ylab = "",
                       sub = "",
                       xlab = "Prozent",
                       col = NULL,
                       rightAxis = FALSE,
                       positive.order = TRUE,
                       as.percent = TRUE,
                       auto.key = list(space = "top", columns = columns),
                       ReferenceZero = NULL,
                       reference.line.col = "gray65",
                       col.strip.background = "gray97",
                       columns = 2,
                       ...) {
  if (inherits(data, "likert")) {
    if (is.null(col))
      col <-
        farbe("likert.blue.red", data$nlevels, middle = ReferenceZero)
    
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
      col <- farbe("likert.blue.red", 5)
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




#
#
#
# #require(stpvers)
#
#
#
# brewer_pal_likert2 <- function(n = 5,
#                                name = "RdBu",
#                                middle.color = "gray80",
#                                min_gray = 10,
#                                max_gray = 60,
#                                ReferenceZero = NULL) {
#   if (tolower(name) == "gray") {
#     min_gray <- 1 - min_gray / 100
#     max_gray <- 1 - max_gray / 100
#     if (n %% 2 == 0) {
#       n <- (n) / 2
#       mycols <- gray(seq(min_gray,  max_gray, length.out = n))
#       c(rev(mycols), mycols)
#     }
#     else{
#       n <- (n - 1) / 2 + 1
#       mycols <- gray(seq(min_gray,  max_gray, length.out = n))
#       c(rev(mycols), mycols[-1])
#     }
#   } else{
#     #   if(is.null(ReferenceZero))
#     HH::brewer.pal.likert(6, name, middle.color)
#     # else (RColorBrewer::brewer.pal(n = n*2,name = name))[c(ReferenceZero: n, (n+1):(n*2))]
#   }
# }
#
#
