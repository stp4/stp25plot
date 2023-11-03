


#' @rdname forest_plot 
#' @description prepare_forest Extrahiere Parameter
#'
#' @param ... model-fits
#' @param names name 
#' @param standardize  parameters::model_parameters:  The method used for standardizing the 
#' parameters. Can be NULL (default; no standardization), 
#' "refit" (for re-fitting the model on standardized data) 
#' or one of "basic", "posthoc", "smart", "pseudo".
#' @param include.indercept logical
#' @param include.label logical or character
#' @param group_name Variable name of trhe group
#'
#' @return data.frame
#' @export
#'
#' @examples
#' 
#' # require(magrittr)
#' # require(tidyverse)
#' # require(stp25plot)
#' # #devtools::install_github("NightingaleHealth/ggforestplot")
#' # require(stp25tools)
#' require(ggplot2)
#' 
#' #library("extrafont")
#' require(survival)
#' #data(package = "survival", colon)
#' colon<- Label(colon, sex="Geschlecht")
#' #colon$sex <- factor(as.numeric(colon$sex), 0:1, c("male", "female"))
#'  
#' fit1 <- lm(status ~ sex + rx + adhere, data = colon)
#' fit2 <- lm(status ~ sex + rx* adhere, data = colon)
#' 
#' 
#' A <- prepare_forest("Base Model" = fit1, 
#'                     fit2,
#'                     include.referenze=FALES
#' )
#' 
#'  
#' 
#' #A$estimate[ is.na(A$estimate)] <- 0
#' #A <- A[A$term !="(Intercept)",]
#' 
#' # Quelle: https://ianasilver.com/making-a-forest-plot-with-ggplot2/
#' ggplot(data=A, aes(x=term, y=estimate , ymin=conf.low , ymax=conf.high)) +
#'   geom_pointrange()+ # Makes range for ggplot values based on the data and AES specified in first line
#'   geom_hline(yintercept=0, lty=2, size =1) +  # add a dotted line at x=0 after flip
#'   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.5, cex=1)+ # Makes whiskers on the range (more aesthetically pleasing)
#'   facet_wrap(~group)+ # Makes DV header (Can handle multiple DVs)
#'   coord_flip() + # flip coordinates (puts labels on y axis)
#'   geom_point(shape = 15, size = 2) + # specifies the size and shape of the geompoint
#'   ggtitle("")+ # Blank Title for the Graph
#'   xlab("") + # Label on the Y axis (flipped specification do to coord_flip) Independent Variables
#'   ylab("b (95% CI)") + # Label on the X axis (flipped specification do to coord_flip)
#'   scale_y_continuous(limits = c(-.250,.52), breaks = c(-.50,-.25,0,.25,.50))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
#'   theme(line = element_line(colour = "black", size = 1), # My personal theme for GGplots
#'         strip.background = element_rect(fill="gray90"), 
#'         legend.position ="none", 
#'         axis.line.x = element_line(colour = "black"), 
#'         axis.line.y = element_blank(), 
#'         panel.border = element_blank(), 
#'         panel.grid.major = element_blank(),
#'         panel.grid.minor = element_blank(), 
#'         panel.background = element_blank(), 
#'         axis.ticks = element_blank(),
#'         axis.title.x = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0)),
#'         axis.title.y = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
#'         plot.title = element_text(family="Times New Roman", colour = "Black", margin = margin(t = 0, r = 0, b = 20, l = 0)),
#'         axis.text=element_text(family="Times New Roman", size=14, color = "Black"), 
#'         text=element_text(family="Times New Roman",size=15), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))
#' 
#' 
#' 
prepare_forest <- function(...,
                           names = NULL,
                           standardize = FALSE,
                           include.indercept = TRUE,
                           include.referenze = TRUE,
                           include.label = FALSE,
                           group_name = "group")
{
  fit <- list(...)
  if (is.null(names)) {
    fit_names <- names(as.list(sys.call())[seq_len(length(fit)) + 1])
    names <- abbreviate(
      gsub("[~??+\\:=]", "",
           as.character(as.list(sys.call()))[seq_len(length(fit)) + 1]),
                        minlength = 7)
    
     if( any(fit_names ==""))  
       names <- ifelse(fit_names=="", names, fit_names)
  
  }
 
  rslt <- NULL
  
  if (length(fit) == 1) {
    rslt <-  prepare_forest1(
      fit[[1]],
      main = "",
      include.indercept = include.indercept,
      include.referenze = include.referenze,
      include.label = include.label,
      standardize = standardize,
      scale.log = FALSE
    )
  }
  else {
    for (i in seq_along(fit)) {
      rst <-  prepare_forest1(
        fit[[i]],
        main = "",
        include.indercept = include.indercept,
        include.referenze = include.referenze,
        include.label = include.label,
        standardize = standardize,
        scale.log = FALSE
      )
      rst[group_name] = names[i]
   #   print(rst)
      if (i == 1) rslt <- rst
      else  rslt <- rbind(rslt, rst)
    }
  }
  
  rslt$term  <-
    rev(factor(rslt$term ,  unique(rslt$term )))
  
  rslt
}



#' @rdname forest_plot 
#' 
#' @param x  fit
#' @param data data
#' @param main  Beschriftung
#' @param scale.log fuer die Beschriftung
#'
#' @return data.frame
#' 
#' 
prepare_forest1 <- function(x,
                            data = insight::get_data(x),  
                            main = NULL,
                            include.indercept = TRUE,
                            include.referenze = include.referenze,
                            include.label=FALSE,
                            standardize = FALSE,  
                            scale.log = FALSE
) {
  stopifnot(!is.null(data))
  model <- stp25stat2::model_info(x)
  #' main effect
  terms <- attr(x$terms, "dataClasses")[-1]
  
  terms_all <- attr(x$terms, "term.labels")
  if (any(grepl("\\:", terms_all))) {
    interact_terms <- terms_all[grepl("\\:", terms_all)]
    interact_terms_class <-
      sapply(stringr::str_split(interact_terms, "\\:"),
             function(x)  {
               if ("factor" %in% terms[x]) "interaction"
               else "numeric"
             })
    
    names(interact_terms_class) <- interact_terms
    terms <- c(terms, interact_terms_class)
    # print(head(model.matrix(x)))
    #  print(head(data))
    #  stop("Interactionen sind noch nicht fertig!")
    # unten in Zeile  
    # as.data.frame(table(data[, var])) 
    # muessen die Interactionen noch ausgezahlt werden
  }
  
  if (!standardize) { # coef <-effectsize::standardize_parameters(x)
    coef <-  as.data.frame(broom::tidy(x))
    ci <- confint(x)
    colnames(ci) <- c("conf.low", "conf.high")
    coef <- cbind(coef, ci)
  }
  else {
    coef <- as.data.frame(
      parameters::model_parameters(x, standardize = "refit"))
    names(coef) <- c(
      "term", "estimate", "std.error","CI", "conf.low", "conf.high",
      "statistic", "DF", "p.value")
  }
  
  allTerms <-
    lapply(seq_along(terms),
           function(i) {
             var <- names(terms)[i]
             if (terms[i] %in% c("factor", "character")) {
               adf <- as.data.frame(table(data[, var]))
               
               rst <- cbind(var = var,
                            adf,
                            pos = 1:nrow(adf))
             }
             
             else if (terms[i] == "numeric") {
               rst <- data.frame(
                 var = var,
                 Var1 = "",
                 Freq = model$N,
                 pos = 1,
                 stringsAsFactors = FALSE
               )
             }
             else if (terms[i] %in% c("interaction")) {
               #' Es fehlen noch die N
               #' sowie die Referenz ist noch nicht vorhanden
               #'
               
               # datx <- model.matrix(x)
               ix <- colnames(model.matrix(x))
               ax <- stringr::str_split(var, "\\:")[[1]]
               
               #' am Ende $
               #' aber name kann auch bei factoren dazischen liegen
               #'
               # matches <-
               #   grep(paste0("^", ax[1], ".*\\:", ax[2], "$"), ix,
               #        ignore.case = TRUE)
               matches <-
                 grep(paste0("^", ax[1], ".*\\:", ax[2]), ix,
                      ignore.case = TRUE)
               
               vars <- ix[matches]
               
               rst <- data.frame(
                 var = vars,
                 Var1 = "",
                 Freq = NA,
                 pos = seq_along(var),
                 stringsAsFactors = FALSE
               )
             }
             else {
               vars = grep(paste0("^", var, "*."), coef$term, value = TRUE)
               rst <- data.frame(
                 var = vars,
                 Var1 = "",
                 Freq = model$N,
                 pos = seq_along(vars),
                 stringsAsFactors = FALSE
               )
             }
             names(rst) <- c("var", "level", "N", "pos")
             rst
           })
  
  allTermsDF <- do.call(rbind, allTerms)
  
  inds <- apply(allTermsDF[, 1:2], 1, paste0, collapse = "")
  rownames(coef) <- gsub("\\[T\\.", "",
                         gsub("[\\]\\[\\(\\)]", "",
                              gsub("`", "",  coef$term)))

  if (inherits(x, "lm")) {
    if (inherits(x, "glm") & is.null(main)) main <- "Odds Ratio"
    else{
      if (is.null(main)){
        main <- if(!standardize & !scale.log) "Estimate" 
        else if(!standardize & scale.log )   "Estimate (log)"    
        else if(standardize & !scale.log )  "Estimate (standardize)"    
        else "Estimate (standardize and log-transformed)"
      }
    }
    
    
    inds <- c("(Intercept)", inds)
    
    allTermsDF <- rbind(
      data.frame(
        var = "(Intercept)",
        level = "",
        N = NA,
        pos = 1,
        stringsAsFactors = FALSE
      ),  
      allTermsDF)
  } 
  else{
    if(standardize)
      stop("standardize = TRUE ist fuer die Classe ", class(x)[1], " nicht vorgesehen!")
    if (is.null(main))  main <- "Hazard Ratio"
  }
  
  gparam <- cbind(allTermsDF, coef[inds, ])
  
 
  if (!include.indercept)  gparam <- gparam[-1, ]
  
  if (is.logical(include.label))
    include.label <- stp25tools:::get_label2(data[model$x])
  else if (is.character(include.label))
    gparam$var <- dplyr::recode(gparam$var, !!!include.label)
  

  gparam$term <- ifelse(gparam$level == "",
                        gparam$var, 
                        paste0(gparam$var, ": ", gparam$level) )
  gparam <- gparam[c(
    "term",
    "var",
    "level",
    "N",
    "estimate",
    "std.error",
    "conf.low",
    "conf.high",
    "statistic",
    "p.value"
  )]
  
  
  attr(gparam, "caption") <-  main
  gparam
}


