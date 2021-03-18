#' panel.conf.int
#' 
#' 
#' @description Erstellt Mittelwertdiagramm mit Confidenzintervall
#' @param pch = 16,
#' @param conf.level = 0.95,
#' @param length = 0.05,
#' @param method ="mean"
#' @export
#' @examples
#' library(stp5)
#'
#'  n<-100
#'  set.seed(3)
#'  DF <- make.groups( P.F.ok=data.frame( value=rnorm(n,-.4),type=gl(2, n/2, labels = c("Control", "Treat")))
#'                     ,P.F.uk =data.frame( value=rnorm(n,.4),type=gl(2, n/2, labels = c("Control", "Treat")))
#'                     , P.M.ok=data.frame( value=rnorm(n),type=gl(2, n/2, labels = c("Control", "Treat")))
#'                     , P.M.uk=data.frame( value=rnorm(n,.2,.5),type=gl(2, n/2, labels = c("Control", "Treat"))))
#'  windows(8,3)
#'  bwplot(which~value|type  , DF, xlim=c(-1, 1), xlab= "Difference (Absolute Value) [mm]",
#'        panel=function(x,y, ...){
#'          # panel.bwplot(x, y, ...)
#'          panel.abline(v=0, lty=2)
#'           panel.conf.int(x,y,...)
#'
#'        })
#'
#'
#'
panel.conf.int <-
  function(x,
           y,
           ...,
           pch = 19, lwd= 2,
           conf.level = 0.95,
           length = 0.05,
           method = "mean") {
    if (any(class(x) == "factor"))  {
      y_new <- 1:nlevels(factor(x))
      m_x <- tapply(y, factor(x), FUN = mean, na.rm = TRUE)
      
      #-- panel.bwplot( x, y, ...)
      
      if (method == "mean" || method == 1) {
        m_x <- tapply(y, factor(x), FUN = mean, na.rm = TRUE)
        Cis <-
          sapply(split(y, x), function(a)
            t.test(a, conf.level = conf.level)$conf.int)
      } else{
        #Median
        m_x <- tapply(y, factor(x), FUN = median, na.rm = TRUE)
        Cis <- sapply(split(y, x),
                      function(a) {
                        #-- Fehler bei Wenigen Daten
                        res_ci <-
                          sort(a)[qbinom(c(.025, .975), length(a), 0.5)]
                        if (length(res_ci) == 1)
                          res_ci <- (range(a))
                        return(res_ci)
                      })
      }
     # panel.points(y_new, m_x, pch = pch, ...)
      panel.segments(y_new - .15, m_x,
                     y_new + .15, m_x, lwd=lwd)
      panel.arrows(
        y_new,
        Cis[1, ],
        y_new,
        Cis[2, ],
        lty = 1,
        lwd = lwd,
        angle = 90,
        code = 3,
        length = length
      )
      
    }
    else if (any(class(y) == "factor")) {
      y_new <- 1:nlevels(factor(y))
      
      if (method == "mean" || method == 1) {
        m_x <- tapply(x, factor(y), FUN = mean, na.rm = TRUE)
        Cis <-
          sapply(split(x, y), function(a)
            t.test(a, conf.level = conf.level)$conf.int)
      } else{
        #Median
        m_x <- tapply(x, factor(y), FUN = median, na.rm = TRUE)
        
        print(cbind(x, y))
        Cis <- sapply(split(x, y),
                      function(a) {
                        #-- Fehler bei Wenigen Daten
                        res_ci <-
                          sort(a)[qbinom(c(.025, .975), length(a), 0.5)]
                        if (length(res_ci) == 1)
                          res_ci <- (range(a))
                        return(res_ci)
                      })
      }
      panel.points(x = m_x, y = y_new, pch = pch, ...)
      panel.arrows(
        Cis[1, ],
        y_new,
        Cis[2, ],
        y_new,
        lty = 1,
        angle = 90,
        code = 3,
        length = length
      )
    }
    else {
      cat("Eine Variable muss ein Faktor sein!")
    }
  }


#' @rdname panel.conf.int
#' @export
panel.median <-
  function(x,
           y,
           ...,
           pch = 19,
           conf.level = 0.95,
           length = 0.05 ) {
    
    
    panel.conf.int(x,
                   y,
                   ...,
                   pch = pch,
                   conf.level = conf.level,
                   length =length,
                   method = "median")
    
  }
