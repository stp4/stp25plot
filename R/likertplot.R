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
#' @param rightAxis,positive.order,as.percent,ReferenceZero,reference.line.col,col.strip.background
#' an HH:::plot.likert.formula
#' @param auto.key,columns,space,between   columns = 2,
#' @param ... HH:::plot.likert.formula
#'
#' @return
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
                       positive.order = TRUE,
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
          wrap_sentence(as.character(attr(data, "plot")$results$Item),
                        35)
        
      }
    } 
    else{
      attr(data, "plot")$results$Item <-
        wrap_sentence(as.character(attr(data, "plot")$results$Item),
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


# likert_col <- function(n = 5,
#                        name =  "RdBl" ,
#                        # c("RdBl", "BlRd",
#                        #          "RdGr", "GrRd",
#                        #          "GrBl", "BlGr",
#                        #          "Bw"),
#                        middle = mean(1:n),
#                        middle.color =  "gray90") {
#   # cat("\n n = ", n, " middle = " ,middle, "\n")
#   if (length(name) == 1) {
#     name <-
#       switch(
#         name,
#         RdBl = c("Reds", "Blues"),
#         BlRd = c("Blues", "Reds"),
#         RdGr = c("Reds", "Greens"),
#         BlGr = c("Blues", "Greens"),
#         GrBl = c("Greens", "Blues"),
#         GrRd = c("Greens", "Reds"),
#         c("Greys", "Greys")
#       )
#   }
#   if (is.odd(middle)) {
#     c(rev(brewer_pal2(n = middle - 1, name = name[1])),
#       middle.color = middle.color,
#       brewer_pal2(n = n - middle, name = name[2]))
#   }
#   else{
#     c(rev(brewer_pal2(n = floor(middle), name = name[1])),
#       brewer_pal2(n = n - floor(middle), name = name[2]))
#   }
#   
# }
# 
# brewer_pal2<- function (n, name="Blues")
# {
#   my_color <-
#     switch(
#       name,
#       
#       
#       
#       Reds = switch(
#         n,
#         rgb(
#           c( 252),
#           c( 146),
#           c( 114),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 252),
#           c(224, 146),
#           c(210, 114),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 252, 222),
#           c(224, 146, 45),
#           c(210, 114, 38),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254,  252, 251, 203),
#           c(229, 174, 106, 24),
#           c(217, 145, 74, 29),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 252, 251,222, 165),
#           c(229, 174, 106, 45, 15),
#           c(217, 145, 74, 38, 21),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 252,  252, 251, 222, 165),
#           c(229, 187, 146, 106, 45, 15),
#           c(217, 161, 114, 74, 38, 21),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 252, 252, 251, 239, 203, 153),
#           c(229, 187, 146, 106, 59, 24, 0),
#           c(217, 161, 114, 74, 44, 29, 13),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(255, 254, 252, 252, 251, 239, 203, 153),
#           c(245, 224, 187, 146, 106, 59, 24, 0),
#           c(240, 210, 161, 114, 74, 44, 29, 13),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(255, 254, 252, 252, 251, 239, 203, 165, 103),
#           c(245, 224, 187, 146, 106,  59, 24, 15, 0),
#           c(240, 210, 161, 114,  74,  44, 29, 21, 13),
#           maxColorValue = 255
#         )
#       )
#       
#       
#       ,
#       
#       
#       
#       Blues = switch(
#         n ,
#         rgb(
#           c(158),
#           c(202),
#           c(225),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(222, 158),
#           c(235, 202),
#           c(247, 225),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(222, 158, 49),
#           c(235, 202, 130),
#           c(247, 225, 189),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(239, 189, 107, 33),
#           c(243, 215, 174, 113),
#           c(255, 231, 214, 181),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(239, 189, 107, 49, 8),
#           c(243, 215, 174, 130, 81),
#           c(255, 231, 214, 189, 156),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(239, 198, 158, 107, 49, 8),
#           c(243, 219, 202, 174,
#             130, 81),
#           c(255, 239, 225, 214, 189, 156),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(239, 198, 158, 107, 66, 33, 8),
#           c(243, 219, 202,
#             174, 146, 113, 69),
#           c(255, 239, 225, 214, 198, 181,
#             148),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(247, 222, 198,
#             158, 107, 66, 33, 8),
#           c(251, 235, 219, 202, 174,
#             146, 113, 69),
#           c(255, 247, 239, 225, 214, 198, 181,
#             148),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(247, 222, 198,
#             158, 107, 66, 33, 8, 8),
#           c(251, 235, 219, 202, 174,
#             146, 113, 81, 48),
#           c(255, 247, 239, 225, 214, 198,
#             181, 156, 107),
#           maxColorValue = 255
#         )
#       ),
#       
#       
#       
#       
#       
#       Greens = switch(
#         n,
#         rgb(
#           c(161),
#           c(217),
#           c(155),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(229, 161),
#           c(245, 217),
#           c(224, 155),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(229, 161, 49),
#           c(245, 217, 163),
#           c(224, 155, 84),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(237,
#             186, 116, 35),
#           c(248, 228, 196, 139),
#           c(233, 179,
#             118, 69),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(237, 186, 116,
#             49, 0),
#           c(248, 228, 196, 163, 109),
#           c(233, 179, 118,
#             84, 44),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(237, 199, 161,
#             116, 49, 0),
#           c(248, 233, 217, 196, 163, 109),
#           c(233,
#             192, 155, 118, 84, 44),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(237,
#             199, 161, 116, 65, 35, 0),
#           c(248, 233, 217, 196,
#             171, 139, 90),
#           c(233, 192, 155, 118, 93, 69, 50),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(247, 229, 199, 161, 116,
#             65, 35, 0),
#           c(252, 245, 233, 217, 196, 171, 139,
#             90),
#           c(245, 224, 192, 155, 118, 93, 69, 50),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(247, 229, 199, 161, 116, 65, 35, 0, 0),
#           c(252,
#             245, 233, 217, 196, 171, 139, 109, 68),
#           c(245,
#             224, 192, 155, 118, 93, 69, 44, 27),
#           maxColorValue = 255
#         )
#       ),
#       
#       
#       Greys = switch(
#         n,
#         rgb(c(189),
#             c(189),
#             c(189),
#             maxColorValue = 255),
#         rgb(c(240, 189),
#             c(240, 189),
#             c(240, 189),
#             maxColorValue = 255),
#         
#         rgb(c(240, 189, 99),
#             c(240, 189,  99),
#             c(240, 189, 99), maxColorValue = 255),
#         rgb(
#           c(247,
#             204, 150, 82),
#           c(247, 204, 150, 82),
#           c(247, 204,
#             150, 82),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(247, 204, 150,
#             99, 37),
#           c(247, 204, 150, 99, 37),
#           c(247, 204, 150,
#             99, 37),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(247, 217, 189,
#             150, 99, 37),
#           c(247, 217, 189, 150, 99, 37),
#           c(247,
#             217, 189, 150, 99, 37),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(247,
#             217, 189, 150, 115, 82, 37),
#           c(247, 217, 189, 150,
#             115, 82, 37),
#           c(247, 217, 189, 150, 115, 82, 37),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(255, 240, 217, 189, 150, 115, 82, 37),
#           c(255, 240, 217, 189, 150, 115, 82,  37),
#           c(255, 240, 217, 189, 150, 115, 82, 37),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(255, 240, 217, 189, 150, 115, 82, 37, 0),
#           c(255, 240, 217, 189, 150, 115, 82, 37, 0),
#           c(255, 240, 217, 189, 150, 115, 82, 37, 0),
#           maxColorValue = 255
#         )
#       ),
#       
#       
#       
#       Oranges = switch(
#         n ,
#         rgb(
#           c(253),
#           c(174),
#           c(107),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 253),
#           c(230, 174),
#           c(206, 107),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 253, 230),
#           c(230, 174, 85),
#           c(206, 107, 13),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 253, 253, 217),
#           c(237, 190, 141, 71),
#           c(222, 133, 60, 1),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 253, 253, 230, 166),
#           c(237, 190, 141, 85, 54),
#           c(222, 133, 60, 13, 3),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 253, 253, 253, 230, 166),
#           c(237, 208, 174, 141, 85, 54),
#           c(222, 162, 107, 60, 13, 3),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(254, 253, 253, 253, 241, 217, 140),
#           c(237, 208, 174, 141, 105, 72,   45),
#           c(222, 162, 107, 60, 19, 1, 4),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(255, 254, 253, 253, 253, 241, 217, 140),
#           c(245, 230, 208, 174, 141, 105, 72, 45),
#           c(235, 206, 162, 107, 60, 19, 1, 4),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(255, 254, 253, 253, 253, 241, 217, 166, 127),
#           c(245, 230, 208, 174, 141, 105, 72, 54, 39),
#           c(235, 206, 162, 107, 60, 19, 1, 3, 4),
#           maxColorValue = 255
#         )
#       ),
#       
#       
#       Purples = switch(
#         n,
#         rgb(
#           c( 188),
#           c( 189),
#           c( 220),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(239, 188),
#           c(237, 189),
#           c(245, 220),
#           maxColorValue = 255
#         ),
#         
#         rgb(
#           c(239, 188, 117),
#           c(237, 189, 107),
#           c(245, 220, 177),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(242, 203, 158, 106),
#           c(240, 201, 154, 81),
#           c(247, 226, 200, 163),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(242, 203, 158, 117, 84),
#           c(240, 201, 154, 107, 39),
#           c(247, 226, 200, 177, 143),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(242, 218, 188, 158, 117, 84),
#           c(240, 218, 189,  154, 107, 39),
#           c(247, 235, 220, 200, 177, 143),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(242, 218, 188, 158,     128, 106, 74),
#           c(240, 218, 189, 154, 125, 81,  20),
#           c(247, 235, 220, 200, 186, 163, 134),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(252, 239, 218, 188, 158, 128, 106, 74),
#           c(251,  237, 218, 189, 154, 125, 81, 20),
#           c(253, 245,      235, 220, 200, 186, 163, 134),
#           maxColorValue = 255
#         ),
#         rgb(
#           c(252, 239, 218, 188, 158, 128, 106, 84, 63),
#           c(251, 237, 218, 189, 154, 125, 81, 39, 0),
#           c(253,245, 235, 220, 200, 186, 163, 143, 125),
#           maxColorValue = 255
#         )
#       )
#     )
#   names(my_color ) <- paste0(name, 1:n)
#   my_color
# }





is.odd <- function(x)
  trunc(x) - x == 0










# likertplot <- function(x = Item   ~ . ,
#                        data,
#                        main = '',
#                        ylab = "",
#                        sub = "",
#                        xlab = "Prozent",
#                        col = NULL,
#                        rightAxis = FALSE,
#                        positive.order = TRUE,
#                        as.percent = TRUE,
#                        auto.key = list(space = "top", columns = columns),
#                        ReferenceZero = NULL,
#                        reference.line.col = "gray65",
#                        col.strip.background = "gray97",
#                        columns = 2,
#                        ...) {
#   if (inherits(data, "likert")) {
#     if (is.null(col))
#       col <-
#         farbe("likert.blue.red", data$nlevels, middle = ReferenceZero)
#     
#     HH:::plot.likert.formula(
#       x,
#       data$results,
#       
#       main = main,
#       ylab = ylab,
#       sub = sub,
#       xlab = xlab,
#       col = col,
#       rightAxis = rightAxis,
#       positive.order = positive.order,
#       as.percent = as.percent,
#       auto.key = auto.key,
#       ReferenceZero = ReferenceZero,
#       reference.line.col = reference.line.col,
#       col.strip.background = col.strip.background,
#       
#       ...
#     )
#   }
#   else{
#     if (is.null(col))
#       col <- farbe("likert.blue.red", 5)
#     HH:::plot.likert.formula(
#       x,
#       data,
#       main = main,
#       ylab = ylab,
#       sub = sub,
#       xlab = xlab,
#       col = col,
#       rightAxis = rightAxis,
#       positive.order = positive.order,
#       as.percent = as.percent,
#       auto.key = auto.key,
#       ReferenceZero = ReferenceZero,
#       reference.line.col = reference.line.col,
#       col.strip.background = col.strip.background,
#       ...
#     )
#   }
#   
#   
# }




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
