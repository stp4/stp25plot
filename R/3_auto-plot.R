#' Lattice-Matrix-Plot
#'
#'
#' @param ... Variablen und Daten
#' @param ylab,xlab default ist ""
#' @param type c("histogram", "boxplot")
#' @param par.settings sefault ist  par.settings = set_lattice()
#' @param include.n noch nicht implemeniert
#'
#' @return nichts
#' @export
#'
#' @examples
#'
#' # require(stpvers)
#' # Projekt("html")
#' #set_my_options(prozent = list(
#' #  digits = c(0, 0),
#' #  style = 2,
#'  # null_percent_sign = "."
#' #))
#' scale2 <- function(x, mn = 1, mx = 5) {
#'   x <-  x - min(x, na.rm = TRUE)
#'   x <-  x / max(x, na.rm = TRUE)
#'   x * (mx - 1) + mn
#' }
#' set.seed(1234)
#'
#' require(lavaan)
#'
#'
#' population.model <- '
#'     Sex =~ sex
#'     Age =~ age
#'     Bildungszertifikat =~ edu
#'     Beruf =~ beruf
#'     Education  ~ 1.04*Bildungszertifikat + 2.76* Beruf
#'
#'     single~ 1*age + 2*edu
#'
#'     Openness =~ o1+o2+o3+o4+o5
#'     Conscientiousness =~ c1+c2+c3+c4+c5
#'     Extraversion =~ e1+e2+e3+e4+e5
#'     Neuroticism =~  n1+n2+n3+n4+n5
#'     Agreeableness =~ a1+a2+a3+a4+a5
#'
#'     Einkommen ~ 1.25*Sex +  (-0.31)*Age  + .12*Openness + .23*Conscientiousness + (-0.91)*Extraversion + 2.3*Neuroticism + (-0.541)*Agreeableness
#'
#'     '
#'
#' DF <- lavaan::simulateData(population.model, sample.nobs = 100)
#'
#'
#' DF <- transform(
#'   DF,
#'   sex = cut(sex, 2, c("male", "female")),
#'   age = scale2(age, 18, 55),
#'   edu = cut(Education, 3, c("low", "med", "hig")),
#'   beruf = cut(Education, 2, c("blue", "withe")),
#'   single = cut(single, 2, c("yes", "no")),
#'   runner = cut(rnorm(nrow(DF)), 4, c("10km", "HM", "M", "UM")),
#'   Einkommen = (4 - log(Einkommen - min(Einkommen) + 1)) * 1000,
#'   Openness = o1 + o2 + o3 + o4 + o5,
#'   Conscientiousness = c1 + c2 + c3 + c4 + c5,
#'   Extraversion = e1 + e2 + e3 + e4 + e5,
#'   Neuroticism =  n1 + n2 + n3 + n4 + n5,
#'   Agreeableness = a1 + a2 + a3 + a4 + a5
#' )
#'
#'
#' DF <-
#'   Label(
#'     DF,
#'     sex="Geschlecht",
#'     age = "Alter",
#'     beruf = "Beruf",
#'     single = "Partner",
#'     edu = "Bildung"
#'   )
#'
#'
#'
#' auto_plot(Agreeableness~ sex, DF)
#' #SaveData()
#'
#' DF %>% auto_plot(Agreeableness,
#'                  age,
#'                  single,
#'                  beruf,
#'                  sex,
#'                  runner)
#' #SaveData()
#'
#' DF %>% auto_plot(
#'   Agreeableness,
#'   age,
#'   single,
#'   beruf,
#'   runner,
#'   by =  ~ sex,
#'   # type = "hist",
#'   par.settings = set_lattice_bw(col = grey.colors(4, start = 0.4, end = 0.9))
#' )
#'
#' #SaveData()
#' 
#' 
#'  auto_plot(
#' Einkommen ~ sex  + age  + Openness + Conscientiousness +
#'   Extraversion + Neuroticism + Agreeableness,
#' DF
#' )
#' 
#' 
#' auto_plot(
#'   edu ~ sex  + age  + Openness + Conscientiousness +
#'     Extraversion + Neuroticism + Agreeableness,
#'   DF
#' )
#' 
#'   DF %>% auto_plot2(n[dot], 
#'                   treatment[bar],
#'                   g[box],
#'                   e[hist],
#'                   sex[pie],
#'                   par.settings =
#'                     set_lattice_bw(pch=19,
#'                                    col = grey.colors(4,
#'                                                      start = 0.4,
#'                                                      end = 0.9)))
#' 
#' 
auto_plot <- function(...,
                       ylab =  "",
                       xlab = "",
                       par.settings = set_lattice(),
                       include.n = TRUE) {
  
  X <- stp25formula::prepare_data2(...)
 
  
  if (is.null(X$group.vars) |
      (length(X$group.vars) == 1) |
      (length(X$measure.vars) > length(X$group.vars))) {
    res <- multi_av_plot(
      X$data,
      X$measure.vars,
      X$group.vars,
      X$row_name,
      X$col_name,
      X$group.class,
      X$measure,
      ylab,
      xlab,
      par.settings,
      include.n
    )
  }
  else{
    res <- multi_uv_plot(
      X$data,
      X$group.vars,
      X$measure.vars,
      X$col_name,
      X$row_name,
      X$measure,
      X$group.class,
      ylab,
      xlab,
      par.settings,
      include.n
    )
  }
  
  if(length(res)>0)
  gridExtra::grid.arrange(grobs = res,
                          ncol = ifelse(length(res) < 4, length(res),
                                        ifelse(length(res) < 10, 3, 4)))
  else plot(1)
}


multi_av_plot <- function(data,
                          measure.vars,
                          group.vars,
                          row_name,
                          col_name,
                          group.class,
                          measure,
                          ylab,
                          xlab,
                          par.settings,
                          include.n) {
  z <-  group.vars[1]
  res <- list()
  
  for (i in seq.int(length(measure.vars))) {
    y <- measure.vars[i]
    if (is.null(z)) {
      
      if (measure[i] == "numeric" | measure[i] == "hist" ) {
        res[[i]] <-
          lattice::histogram(
            formula(paste("~", y)),
            data,
            type = "count",
            ylab = ylab,
            xlab = xlab,
            main = row_name[i],
            par.settings = par.settings
          )
      }
      else if (measure[i] == "factor" | measure[i] == "bar") {
        
        tab <- as.data.frame(xtabs(formula(paste("~", y)), data))
        
        res[[i]] <-
          lattice::barchart(
            formula(paste("Freq~", y)),
            data = tab,
            ylab = ylab,
            xlab = xlab,
            main = row_name[i],
            stack = FALSE,
            origin = 0,
            horizontal = FALSE,
            par.settings = par.settings
          )
      }
      else if ( measure[i] =="box"){
        res[[i]] <-    
          lattice::bwplot(
            formula(paste("~", y)),
            data,
            type = "count",
            ylab = ylab,
            xlab = xlab,
            main = row_name[i],
            par.settings = par.settings
          )
      }
      else if ( measure[i] =="pie"){
        tab <- as.data.frame(xtabs(formula(paste("~", y)), data))
        
        res[[i]] <-
          piechart(
            ~Freq,
            data = tab,
            ylab = ylab,
            xlab = xlab,
            main = row_name[i],
            
            par.settings = par.settings
          )
      }
      else if ( measure[i] =="dot"){
        tab <- as.data.frame(xtabs(formula(paste("~", y)), data))
        
        res[[i]] <-
          lattice::dotplot(
            formula(paste("Freq~", y)),
            data = tab,
            ylab = ylab,
            xlab = xlab,
            main = row_name[i],
            stack = FALSE,
            origin = 0,
            horizontal = FALSE,
            par.settings = par.settings
          )
      }
      else{}
    }
    else{
      if (group.class[1] == "factor") {
        if (measure[i] == "numeric" | measure[i] == "box") {
          
          res[[i]] <-
            lattice::bwplot(
              formula(paste(y, "~", z)),
              data,
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              par.settings = par.settings
            )
        }
        else if ( measure[i] == "hist" ) {
          res[[i]] <-
            lattice::histogram(
              formula(paste("~", y, "|", z)),
              data,
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              par.settings = par.settings
            )
        }
        else if ( measure[i] == "factor" | measure[i] == "bar") {
          tab <-
            as.data.frame(xtabs(formula(paste(
              "~", y, "+", z
            )), data))
          res[[i]] <-
            lattice::barchart(
              formula(paste("Freq~", y, "|", z)),
              data = tab,
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              stack = FALSE,
              origin = 0,
              horizontal = FALSE,
              par.settings = par.settings
            )
        }
        else if ( measure[i] =="pie"){
          tab <-
            as.data.frame(xtabs(formula(paste(
              "~", y, "+", z
            )), data))
          res[[i]] <-
            piechart(
              formula(paste("~Freq|" , z)),
              data = tab,
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              
              par.settings = par.settings
            )
        }
        else if ( measure[i] =="dot"){
          
          res[[i]] <-
            lattice::stripplot(
              formula(paste(y, "~", z)),
              data,
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              par.settings = par.settings,
              panel = function(x, y, ...) {
                panel.stripplot(x, y, ..., jitter.data = TRUE)
              }
            )
          
        }
        else {}
      }
      else{
        if (measure[i] == "numeric" | measure[i] =="dot") {
          xlab <- col_name[1]
          res[[i]] <-
            lattice::xyplot(
              formula(paste(y, "~", z)),
              data,
              type = c("p", "r"),
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              par.settings = par.settings
            )
        }
        else if (measure[i] == "factor" | measure[i] == "box") {
          res[[i]] <-
            lattice::bwplot(
              formula(paste(y, "~", z)),
              data,
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              par.settings = par.settings
            )
        }
        else if( measure[i] =="hist") {
          res[[i]] <-
            lattice::histogram(
              formula(paste("~", y, "|", z)),
              data,
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              par.settings = par.settings
            )
        }
        else if ( measure[i] =="bar") {
          tab <-
            as.data.frame(xtabs(formula(paste(
              "~", y, "+", z
            )), data))
          res[[i]] <-
            lattice::barchart(
              formula(paste("Freq~", y, "|", z)),
              data = tab,
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              stack = FALSE,
              origin = 0,
              horizontal = FALSE,
              par.settings = par.settings
            )
        }
        else if ( measure[i] =="pie"){
          res[[i]] <- lattice::xyplot(y~x, 
                                      data.frame(x=1:10, y=1:10),
                                      main="pie")
        }
        else {}
        
      }
    }
  }
  res
}


#' @noRd
#' Im wesentlichen ist das eine Kopie von oben nur die Formeln sind vertauscht 
#' und die auswahl an verschiede Plots ist nicht möglich.
multi_uv_plot <- function(data,
                          measure.vars,
                          group.vars,
                          row_name,
                          col_name,
                          group.class,
                          measure,
                          ylab,
                          xlab,
                          
                          par.settings,
                          include.n) {
  z <-  group.vars[1]
  res <- list()
  
  for (i in seq.int(length(measure.vars))) {
    y <- measure.vars[i]
    ylab <- col_name[1]
    if (group.class[1] == "factor") {
      if (measure[i] == "numeric") {
       # if (type == "boxplot") {
          res[[i]] <-
            lattice::bwplot(
              formula(paste(z, "~", y)),
              data,
              ylab = ylab,
              xlab = xlab,
              main = row_name[i],
              par.settings = par.settings
            )
       # }
        # else{
        #   res[[i]] <-
        #     lattice::histogram(
        #       formula(paste("~", z, "|", y)),
        #       data,
        #       ylab = ylab,
        #       xlab = xlab,
        #       main = row_name[i],
        #       par.settings = par.settings
        #     )
        # }
      }
      else if (measure[i] == "factor") {
        tab <-
          as.data.frame(xtabs(formula(paste(
            "~", z, "+", y
          )), data))
        
        res[[i]] <-
          lattice::barchart(
            formula(paste("Freq~", z, "|", y)),
            data = tab,
            ylab = ylab,
            xlab = xlab,
            main = row_name[i],
            stack = FALSE,
            origin = 0,
            horizontal = FALSE,
            par.settings = par.settings
          )
      }
    }
    else{
      if (measure[i] == "numeric") {
        res[[i]] <-
          lattice::xyplot(
            formula(paste(z, "~", y)),
            data,
            type = c("p", "r"),
            ylab = ylab,
            xlab = xlab,
            main = row_name[i],
            par.settings = par.settings
          )
      }
      else if (measure[i] == "factor") {
        res[[i]] <-
          lattice::bwplot(
            formula(paste(z, "~", y)),
            data,
            ylab = ylab,
            xlab = xlab,
            main = row_name[i],
            par.settings = par.settings
          )
      }
    }
  }
  res
}




# #das geht nicht
# multi_uv_plot <- function(data,
#                           measure.vars,
#                           group.vars,
#                           row_name,
#                           col_name,
#                           group.class,
#                           measure,
#                           ylab,
#                           xlab,
#                           par.settings,
#                           include.n) {
#   z <-  group.vars[1]
#   res <- list()
#   
#   
#   
#  
#   for (i in seq.int(length(measure.vars))) {
#     y <- measure.vars[i]
#     ylab <- col_name[1]
#     if (group.class[1] == "factor") {
#       if ( measure[i] == "numeric" | measure[i] == "box") {
#         res[[i]] <-
#           lattice::bwplot(
#             formula(paste(z, "~", y)),
#             data,
#             ylab = ylab,
#             xlab = xlab,
#             main = row_name[i],
#             par.settings = par.settings
#           )
#       }
#       else if (measure[i] == "factor" | measure[i] =="hist"){
#         res[[i]] <-
#           lattice::histogram(
#             formula(paste("~", z, "|", y)),
#             data,
#             ylab = ylab,
#             xlab = xlab,
#             main = row_name[i],
#             par.settings = par.settings
#           )
#       }
#       else if ( measure[i] == "factor" | measure[i] =="bar" ) {
#         tab <-
#           as.data.frame(xtabs(formula(paste(
#             "~", z, "+", y
#           )), data))
#         
#         res[[i]] <-
#           lattice::barchart(
#             formula(paste("Freq~", z, "|", y)),
#             data = tab,
#             ylab = ylab,
#             xlab = xlab,
#             main = row_name[i],
#             stack = FALSE,
#             origin = 0,
#             horizontal = FALSE,
#             par.settings = par.settings
#           )
#       }
#       else if ( measure[i] =="pie"){
#         res[[i]] <- lattice::xyplot(y~x, 
#                                     data.frame(x=1:10, y=1:10),
#                                     main="pie")
#       }
#       else if ( measure[i] =="dot"){
#         res[[i]] <-
#           lattice::stripplot(
#             formula(paste(z, "~", y)),
#             data,
#             ylab = ylab,
#             xlab = xlab,
#             main = row_name[i],
#             par.settings = par.settings,
#             panel = function(x, y, ...) {
#               panel.stripplot(x, y, ..., jitter.data = TRUE)
#             }
#           )
#       }
#       else if ( measure[i] =="hist"){
#         res[[i]] <- lattice::xyplot(y~x, 
#                                     data.frame(x=1:10, y=1:10),
#                                     main="hist")
#       }
#       else {}
#     }
#     else{
#       if ( measure[i] == "numeric" |  measure[i] =="dot") {
#         res[[i]] <-
#           lattice::xyplot(
#             formula(paste(z, "~", y)),
#             data,
#             type = c("p", "r"),
#             ylab = ylab,
#             xlab = xlab,
#             main = row_name[i],
#             par.settings = par.settings
#           )
#       }
#       else if ( measure[i] == "factor" | measure[i] == "box") {
#         res[[i]] <-
#           lattice::bwplot(
#             formula(paste(z, "~", y)),
#             data,
#             ylab = ylab,
#             xlab = xlab,
#             main = row_name[i],
#             par.settings = par.settings
#           )
#       }
#       else if ( measure[i] =="pie"){
#         res[[i]] <- lattice::xyplot(y~x, 
#                                     data.frame(x=1:10, y=1:10),
#                                     main="pie")
#       }
#       else if ( measure[i] =="bar"){
#         res[[i]] <- lattice::xyplot(y~x, 
#                                     data.frame(x=1:10, y=1:10),
#                                     main="bar")
#       }
#       else if ( measure[i] =="hist"){
#         res[[i]] <- lattice::xyplot(y~x, 
#                                     data.frame(x=1:10, y=1:10),
#                                     main="hist")
#       }
#       else {}
#     }
#   }
#   res
# }













# # alt
# auto_plot <- function(...,
#                       ylab =  "",
#                       xlab = "",
#                       type = c("boxplot", "histogram"),
#                       par.settings = set_lattice(),
#                       include.n = TRUE) {
#   type <-  match.arg(type, several.ok = FALSE)[1]
#   X <- stp25formula::prepare_data2(...)
# 
#   if (is.null(X$group.vars) |
#       (length(X$group.vars) == 1) |
#       (length(X$measure.vars) > length(X$group.vars))) {
#     res <- multi_av_plot(
#       X$data,
#       X$measure.vars,
#       X$group.vars,
#       X$row_name,
#       X$col_name,
#       X$group.class,
#       X$measure,
#       ylab,
#       xlab,
#       type,
#       par.settings,
#       include.n
#     )
#   }
#   else{
#     res <- multi_uv_plot(
#       X$data,
#       X$group.vars,
#       X$measure.vars,
#       X$col_name,
#       X$row_name,
#       X$measure,
#       X$group.class,
#       ylab,
#       xlab,
#       type,
#       par.settings,
#       include.n
#     )
#   }
# 
#   gridExtra::grid.arrange(grobs = res,
#                           ncol = ifelse(length(res) < 4, length(res),
#                                         ifelse(length(res) < 10, 3, 4)))
# }


# multi_av_plot <- function(data,
#                           measure.vars,
#                           group.vars,
#                           row_name,
#                           col_name,
#                           group.class,
#                           measure,
#                           ylab,
#                           xlab,
#                           type,
#                           par.settings,
#                           include.n) {
#   z <-  group.vars[1]
#   res <- list()
# 
#   for (i in seq.int(length(measure.vars))) {
#     y <- measure.vars[i]
#     if (is.null(z)) {
#       fm <- formula(paste("~", y))
#       if (measure[i] == "numeric") {
#         res[[i]] <-
#           lattice::histogram(
#             fm,
#             data,
#             type = "count",
#             ylab = ylab,
#             xlab = xlab,
#             main = row_name[i],
#             par.settings = par.settings
#           )
#       }
#       else if (measure[i] == "factor") {
#         fm <-  formula(paste("Freq~", y))
#         tab <- as.data.frame(xtabs(formula(paste("~", y)), data))
# 
#         res[[i]] <-
#           lattice::barchart(
#             fm,
#             data = tab,
#             ylab = ylab,
#             xlab = xlab,
#             main = row_name[i],
#             stack = FALSE,
#             origin = 0,
#             horizontal = FALSE,
#             par.settings = par.settings
#           )
#       }
#     }
#     else{
#       if (group.class[1] == "factor") {
#         if (measure[i] == "numeric") {
#           if (type == "boxplot") {
#             res[[i]] <-
#               lattice::bwplot(
#                 formula(paste(y, "~", z)),
#                 data,
#                 ylab = ylab,
#                 xlab = xlab,
#                 main = row_name[i],
#                 par.settings = par.settings
#               )
#           }
#           else{
#             res[[i]] <-
#               lattice::histogram(
#                 formula(paste("~", y, "|", z)),
#                 data,
#                 ylab = ylab,
#                 xlab = xlab,
#                 main = row_name[i],
#                 par.settings = par.settings
#               )
#           }
#         }
#         else if (measure[i] == "factor") {
#           tab <-
#             as.data.frame(xtabs(formula(paste(
#               "~", y, "+", z
#             )), data))
#           res[[i]] <-
#             lattice::barchart(
#               formula(paste("Freq~", y, "|", z)),
#               data = tab,
#               ylab = ylab,
#               xlab = xlab,
#               main = row_name[i],
#               stack = FALSE,
#               origin = 0,
#               horizontal = FALSE,
#               par.settings = par.settings
#             )
#         }
#       }
#       else{
#         if (measure[i] == "numeric") {
#           xlab <- col_name[1]
#           res[[i]] <-
#             lattice::xyplot(
#               formula(paste(y, "~", z)),
#               data,
#               type = c("p", "r"),
#               ylab = ylab,
#               xlab = xlab,
#               main = row_name[i],
#               par.settings = par.settings
#             )
#         }
#         else if (measure[i] == "factor") {
#           res[[i]] <-
#             lattice::bwplot(
#               formula(paste(y, "~", z)),
#               data,
#               ylab = ylab,
#               xlab = xlab,
#               main = row_name[i],
#               par.settings = par.settings
#             )
#         }
#       }
#     }
#   }
#   res
# }






 # auto_plot(treatment ~ g + e + sex ],
 #                 DF,
 #                 par.settings =
 #                   set_lattice_bw(pch = 19,
 #                                  col = grey.colors(4,
 #                                                    start = 0.4,
 #                                                    end = 0.9)))

 # 
 # DF %>%auto_plot(treatment, by= ~ g + e + sex[dot] ,
 #      
 #           par.settings =
 #             set_lattice_bw(pch = 19,
 #                            col = grey.colors(4,
 #                                              start = 0.4,
 #                                              end = 0.9)))