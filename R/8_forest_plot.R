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
#' 
#' # require(surv)
#' # require(survminer)
#'  require(stpvers)
#' 
#' forest_plot(glm(status ~ sex + rx + adhere,
#' data = colon, family = binomial())) 
#' #forest_plot(coxph(Surv(time, status) ~ sex + rx + adhere,
#' #             data = colon), data=colon) 
#' 
#' 
forest_plot <- function(x, data = NULL, main=NULL, transform=TRUE, ...) {
  model <- stp25stat::model_info(x)
  terms <- attr(x$terms, "dataClasses")[-1]
  coef <-  as.data.frame(broom::tidy(x))
  
  # print(coef)
  
  gof<- APA(x)
  if (is.null(data) ) {  data <- x$model  }
  else{
    model$labels <- stp25aggregate::get_label(data[model$x])
    
  }
 # stopifnot(class(model) == "coxph")
  stopifnot(!is.null(data) )
# -- make_data_table ----------------------------------------
 
    
    allTerms <- lapply(seq_along(terms), function(i) {
      var <- names(terms)[i]
      if (terms[i] %in% c("factor", "character")) {
        adf <- as.data.frame(table(data[, var]))
        cbind(var = var, 
              adf, 
              pos = 1:nrow(adf))
      }
      else if (terms[i] == "numeric") {
        data.frame(
          var = var,
          Var1 = "",
          Freq = model$N,
          pos = 1, stringsAsFactors = FALSE
        )
      }
      else {
        vars = grep(paste0("^", var, "*."), coef$term, value = TRUE)
        data.frame(
          var = vars,
          Var1 = "",
          Freq = model$N,
          pos = seq_along(vars), stringsAsFactors = FALSE
        )
      }
    })
    
    
    allTermsDF <- do.call(rbind, allTerms)
    colnames(allTermsDF) <- c("var", "level", "N", "pos")

    inds <- apply(allTermsDF[, 1:2], 1, paste0, collapse = "")
    rownames(coef) <- gsub("\\[T\\.", "", 
                      gsub("[\\]\\[]", "",
                      gsub("`", "",   coef$term)))
     
    if (is.null(main) & !transform) main<- "Estimate"
    
    if (inherits(x, "lm")) {
      if (inherits(x, "glm")) {
        if (is.null(main))
          main <- "Odds Ratio"
      }
      else{
        if (is.null(main))
          main <- "Estimate"
        transform <- FALSE
      }
      ci <- confint(x)
      colnames(ci) <- c("conf.low", "conf.high")
      coef <- cbind(coef, ci)
      
      inds <- c("(Intercept)", inds)
      allTermsDF <-
        rbind(data.frame(
          var = "(Intercept)",  level = "",  N = NA,  pos = 1, stringsAsFactors = FALSE),
        allTermsDF)
    } else{
      
      ci <- confint(x)
      colnames(ci) <- c("conf.low", "conf.high")
      coef <- cbind(coef, ci)
      
      if (is.null(main))
        main <- "Hazard Ratio"
    }
 
    

    gparam <- cbind(allTermsDF,  coef[inds,])
    
    #print(gparam)
    ggplot_table(gparam, gof, model, main, transform = transform, ...)
    
    
 
}



# # 
# require(survival)
# require(stpvers)
# colon<- Label(colon, sex="Geschlecht")
# fit1<- lm(  status ~ sex + rx + adhere,
#              data = colon)
# fit2<- glm(  status ~ sex + rx + adhere,
#            data = colon, family = binomial())
# 
# fit3 <- coxph(Surv(time, status) ~ sex + rx + adhere,
#   data = colon) 
#  forest_plot( fit1 )
#  forest_plot( fit2 )
#  forest_plot( fit3, colon )
 