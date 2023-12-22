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
#' @param auto.key,columns,space  columns = 2,
#' @param ... HH:::plot.likert.formula  
#'    between=list(x=0))
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
#' # require(HH)  # ?likertplot
#' # class(Res2)
#' # windows(7, 3)
#' # attr(Res2, "plot")$results 
#' 
#' likertplot(Item ~ . | Geschlecht,
#'            data = Res2,    
#'             between=list(x=0))
#' 
#' # col = likert_col(attr(data, "plot")$nlevels, middle = ReferenceZero)
#'   
#' DF2 %>% likert_plot(Magazines, Comic.books, Fiction, Newspapers,
#'                     relevel = letters[1:5],
#'                     ReferenceZero = 1.5,
#'                     columns=5)
#' 
#' # DF2 %>% 
#' #   Likert(Magazines, Comic.books, Fiction, Newspapers) %>% 
#' #   likertplot()
#' 
likertplot <- function(x = Item   ~ . ,
                       data=NULL,
                       main = '',
                       ylab = "",
                       sub = "",
                       xlab = if(horizontal) {if(as.percent) "Prozent" else "Anzahl"} else "",
                       col = NULL,
                   
                       rightAxis = FALSE,
                       positive.order = FALSE, 
                       as.percent = TRUE,
                       auto.key = list(space = space, 
                                       columns = columns,
                                       between = 1
                                       ),
                       
                       ReferenceZero = NULL,
                       reference.line.col = "gray65",
                       col.strip.background = "gray97",
                       wrap = TRUE,
                       columns = 2,
                       space ="top",
                       horizontal = TRUE,
                       between = list(x = 1 + (horizontal), 
                                      y = 0.5 +2 * (!horizontal)),
                      ...) {
  name_item <- "Item"
  
  if(is.null(data)){
    if(is.data.frame(x) & ("plot" %in% names(attributes(x))) ){
      # Tbll_likert() 
      formula <-  attr(x, "plot")$formula
      nlevels <-  attr(x, "plot")$nlevels
      data <-  attr(x, "plot")$results
    }
    else if(inherits(x, "likert")){
      formula <- x$formula
      nlevels <- x$nlevels
      data <-  x$results
    }
    else{ stop("No data.frame !") }
  }
  else if (plyr::is.formula(x)) {
   if (is.data.frame(data) ) {
     if("plot" %in% names(attributes(data))){
       # Tbll_likert()
       formula <-  x
       nlevels <-  attr(data, "plot")$nlevels
       data <-  attr(data, "plot")$results
     }
     else{
       formula <-  x
       name_item<- all.vars(x)[1]
     }
   }
    else if( inherits(x, "likert") ){
       formula <- x
       nlevels <- data$nlevels
       data <-  data$results
      }
  }
  
  
  

  if (is.null(col)) {
    col <- if (is.null(ReferenceZero)) likert_col(nlevels)
           else likert_col(nlevels, middle = ReferenceZero)
  }
  

  
  if (is.logical(wrap)) {
    if (wrap) {
     # if (!is.character(data[[name_item]]))
     #   stop("Hier kann ein fehler vorliegen!! wrap_string")
      data[[name_item]] <-
        stp25tools::wrap_factor(data[[name_item]], 35)
    }
  }
  else if (is.numeric(wrap)) {
   # if (!is.character(data[[name_item]]))
   #   stop("Hier kann ein fehler vorliegen!! wrap_string")
    data[[name_item]] <-
      stp25tools::wrap_factor(data[[name_item]], wrap)
  }
  
  
  
  HH:::plot.likert.formula(
      x = formula,
      data = data,
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
      between=between,
      horizontal = horizontal,
      ...
  
    )
 }


#' @rdname likertplot
#'
#' @param ... an Tbll_likert
#' @param include.table,include.na,caption Tabelle ausgeben (output ist eine Altlast)
#' @param type  nur in likert_plot Bei Gruppen Items als Zeilen => 1, oder Gruppen als Zeilen => 2
#' @param include.total an stp25stat2:::Likert
#' @param include.order sortiere muss die länge der Items entsprechen
#' @param relevel  Uberschreibt die levels levels(x) <- relevel ist nur nur in likert_plot vorhanden
#' @param par.strip.text an  HH:::plot.likert.formula
#' @return HH likertplot (lattice-Plot)
#' @export
#'
#' @examples
#' 
#' DF2 %>% likert_plot(Magazines, Comic.books, Fiction, Newspapers)
#' 
likert_plot <- function(...,
                        include.total = FALSE,
                        relevel = NULL, 
                        include.table = FALSE,
                        include.order = NULL,
                        caption = "",
                        # an likertplot
                        main = '',
                        ylab = "",
                        sub = "",
                        xlab = if(as.percent) "Prozent" else "Anzahl",
                        type = 1, 
                        col = NULL,
                        rightAxis = FALSE,
                        positive.order =  if(is.logical(include.order)) include.order else FALSE, 
                        as.percent = TRUE,
                        auto.key = list(space = space, 
                                        columns = columns,
                                        between = 1
                        ),
                        ReferenceZero = NULL,
                        reference.line.col = "gray65",
                        col.strip.background = "gray97",
                        wrap = TRUE,
                        columns = 2,
                        space ="top",
                       
                        horizontal = TRUE,
                        between = list(x = 1 + (horizontal), 
                                       y = 0.5 +2 * (!horizontal)),
                        par.strip.text = list(lines = 1, cex = .8),
                       
                      #  output = include.table,
                        include.na = FALSE
                        ){

  if (is.null(relevel)){
    X <- stp25stat2:::Likert(..., include.total=include.total)
   }
  else{
    X_old <-  stp25tools::prepare_data2(...)
    
    X_old$data[X_old$measure.vars] <-
      stp25tools::dapply2(
        X_old$data[X_old$measure.vars],
        fun = function(x) {
          if (nlevels(x) == length(relevel))
            levels(x) <- relevel
          else
            stop("\nDie relevel stimmen in der laenge nicht überein!\n")
          x
        }
      )
    X <- stp25stat2:::Likert(X_old$formula,  X_old$data, include.total=include.total)
  }
  
  if(is.numeric(include.order)){
    positive.order <- FALSE
    ny_levels <- levels(X$results$Item)
    if( length(ny_levels) != length(include.order)) stop("include.order ist die Reihenfolge der Items - muss also exakt gleich lang sein wie die Items!")
    X$results$Item <- factor(X$results$Item, ny_levels[include.order] )
  }
  
  if(include.table)
    stp25output2::Output(
      stp25stat2::Tbll_likert(X,
                              include.na = include.na,
                              ReferenceZero = ReferenceZero),
      caption = caption
    )
  
  

if( type !=1 ){
  fm <- X$formula
  x_in <- all.names(fm)
  
  if (length(x_in) == 5) {
    X$formula <-  formula(paste(x_in[5], x_in[1], x_in[4], x_in[3], x_in[2]))
  } else if (length(x_in) == 7) {
    X$formula <-
      formula(paste(x_in[6],  x_in[1], x_in[4], x_in[3], x_in[2], x_in[5], x_in[7]))
  }
}


  likertplot(
    X,
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
    wrap = wrap,
    horizontal = horizontal,
    between = between,
    par.strip.text = par.strip.text
  )
 
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
  stp25settings:::likert_col(
    n = n,
    name = name,
    middle = middle,
    middle.color = middle.color
  )
}

 