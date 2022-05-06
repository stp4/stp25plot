#' forest_plot
#'
#' Gestohlen von survminer::ggforest()
#'
#' @param x Model
#' @param data Daten
#' @param main Uebersrift
#' @param ... alles an
#'
#' @return ggplot
#' @export
#'
#' @examples
#' set.seed(1)
#' n <- 10 * 2 * 3
#' dat <- data.frame(
#'   y = rnorm(n),
#'   sex = gl(2, n / 2, labels = c("male", "female")),
#'   rx = gl(3, n / 3, labels = c("Obs",  "Tev", "Tev+5FU")),
#'   age = 1:n,
#'   bmi = rnorm(n )
#' )
#' dat <- transform(dat,
#'                  y = y + 
#'                    as.numeric(sex) / 2 + 
#'                    as.numeric(rx) 
#' )
#' 
#' 

#' fit1 <- lm(y ~ sex + rx  ,  dat)
#' fit2 <- lm(y ~ sex + rx + age * bmi,  dat)
#' 
#' forest_plot(fit1, plot=T)
#' forest_plot(fit1, plot=F)
#' forest_plot(fit1, plot="ggplot_forest", title="Hallo")
#' prepare_forest(fit2)
#' ggplot_forest( prepare_forest(fit1,fit2) )+
#' ggforce::facet_col(
#'     facets = ~group,
#'     scales = "free_y",
#'     space = "free"
#'   )
#'   
#'  ggplot_table(prepare_forest(fit1))
#' 
#' fit1 <- lm(y ~ sex + rx + age + bmi,  dat)
#' forest_plot(fit1, standardize = TRUE)
#' 
#' # require(surv)
#' # require(survminer)
#'  require(survival)
#'  require(stpvers)
#'
#' forest_plot(glm(status ~ sex + rx + adhere,
#' data = colon, family = binomial()))
#' #forest_plot(coxph(Surv(time, status) ~ sex + rx + adhere,
#' #             data = colon), data=colon)
#'
#'
forest_plot <- function(x,
                        data = insight::get_data(x), # x$model,
                        main = NULL,
                        include.indercept = TRUE,
                        include.referenze = TRUE,
                        include.gof = TRUE,
                        include.label=FALSE,
                        standardize = FALSE, 
                        scale.log = FALSE,
                        plot = TRUE,
                        ...) {
  if (include.gof) gof <- stp25stat2::APA(x)
  else gof <- NULL
  
  gparam <- prepare_forest1(
    x,
    data = data,
    main = main,
    include.indercept = include.indercept,
    include.referenze = include.referenze,
    include.label = include.label,
    standardize = standardize,
    scale.log=scale.log
  )
  if (is.logical(plot)){
   if (!plot) gparam 
   else  ggplot_table(gparam,
               gof =  gof,
               main = attr(gparam, "caption"),
               scale.log = scale.log,
               ...) 
  }
  else if(plot == "ggplot_table" ){
    ggplot_table(gparam,
                 gof =  gof,
                 main = attr(gparam, "caption"),
                 scale.log = scale.log,
                 ...) 
    
  }
  else ggplot_forest(gparam, ...)

  
  

  
}

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
#' model1 <- lm(mpg ~ wt, data = mtcars)
#' model2 <- lm(mpg ~ wt + cyl, data = mtcars)
#' prepare_forest(model1, model12)
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
    names <- abbreviate(gsub("[~??+\\:=]", "",
                             as.character(as.list(sys.call(
                             )))[seq_len(length(fit)) + 1]),
                        minlength = 7)
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
      if (i == 1)
        rslt <- rst
      else
        rslt <- rbind(rslt, rst)
    }
  }
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
               if ("factor" %in% terms[x])
                 "interaction"
               else
                 "numeric"
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
      parameters::model_parameters(x, 
                                   standardize = "refit"))
    names(coef) <- c(
      "term",
      "estimate", "std.error",
      "CI", "conf.low", "conf.high",
      "statistic","DF", "p.value")
  }
  
  allTerms <-
    lapply(seq_along(terms),
           function(i) {
             #  print(i)
             var <- names(terms)[i]
             #  print(var)
             if (terms[i] %in% c("factor", "character")) {
               
               adf <- as.data.frame(table(data[, var]))
               # print(adf)
               cbind(var = var,
                     adf,
                     pos = 1:nrow(adf)) }
             
             else if (terms[i] == "numeric") {
               data.frame(
                 var = var,
                 Var1 = "",
                 Freq = model$N,
                 pos = 1,
                 stringsAsFactors = FALSE)}
             
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
               
               data.frame(var = vars,
                          Var1 = "",
                          Freq = NA,
                          pos = seq_along(var),
                          stringsAsFactors = FALSE)
             }
             else {
               vars = grep(paste0("^", var, "*."), coef$term, value = TRUE)
               
               
               data.frame(
                 var = vars,
                 Var1 = "",
                 Freq = model$N,
                 pos = seq_along(vars),
                 stringsAsFactors = FALSE)}
           })
  
  
  # print(terms)
  
  allTermsDF <- do.call(rbind, allTerms)
  
  colnames(allTermsDF) <- c("var", "level", "N", "pos")
  
  # print(allTermsDF)
  inds <- apply(allTermsDF[, 1:2], 1, paste0, collapse = "")
  
  rownames(coef) <- gsub("\\[T\\.", "",
                         gsub("[\\]\\[]", "",
                              gsub("`", "",   coef$term)))
  
  
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
  
  gparam$term<- paste0(gparam$var, ": ", gparam$level) 
 
  gparam <- gparam[c(
    "term","var","level","N",
    "estimate",
    "std.error", "conf.low","conf.high",
    "statistic","p.value"
  )] 
  
  
  attr(gparam, "caption") <-  main
  gparam
}







