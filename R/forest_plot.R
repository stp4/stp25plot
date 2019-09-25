# x <- APA2(fit_step, "Logistic regression analysis for EAD risk factors",
#           include.se=FALSE,include.odds=TRUE, include.ci=TRUE, output=FALSE)



#  setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25plot/R")



# 
# APA.coxph
# function (x,...){
#   gmodel <- broom::glance(x)
#   paste0(
#     "# Events: ",
#     gmodel$nevent,
#     "; Global p-value (Log-Rank): ",
#     stp25rndr::rndr_P(gmodel$p.value.log),
#     " \nAIC: ",
#     round(gmodel$AIC, 0),
#     "; Concordance Index: ",
#     round(gmodel$concordance, 2)
#   )
#   
# }

 
# Print <- function(x) {
#   cat("\n\n")
#   print(x)
#   cat("\n\n")
# }


useLabels<- function(x,
                     data,
                     fit=NULL
){
  if(is.null(fit)) lbl<- get_label(data)
  else lbl<- get_label(data[all.vars(fit$formula)])
  
  if(is.data.frame(x))  x[,1] <- replaceLabel(x[,1], lbl)
  else  x<- replaceLabel(x, lbl)
  
  x
}

replaceLabel<- function(x, lbl){
  x<- gsub("\\[T.","\\[", x)
  x<- gsub("\\[TRUE\\]","", x)
  for( i in names(lbl))
    x <- stringr::str_replace(x, i , lbl[i])
  x
}



#' forest_plot
#' 
#' Gestohlen von survminer::ggforest()
#'
#' @param x Model 
#' @param data Daten 
#' @param main Uebersrift
#' @param ... alles an 
#'
#' @return
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
  gof<- APA(x)
  if (is.null(data) ) {  data <- x$model  }
  else{
    model$labels <- stp25aggregate::get_label(data[model$terms])
  }

# -- make_data_table ----------------------------------------
 
    
    allTerms <- lapply(seq_along(terms), function(i) {
      var <- names(terms)[i]
      if (terms[i] %in% c("factor", "character")) {
        adf <- as.data.frame(table(data[, var]))
        cbind(var = var, adf, pos = 1:nrow(adf))
      }
      else if (terms[i] == "numeric") {
        data.frame(
          var = var,
          Var1 = "",
          Freq = model$N,
          pos = 1
        )
      }
      else {
        vars = grep(paste0("^", var, "*."), coef$term, value = TRUE)
        data.frame(
          var = vars,
          Var1 = "",
          Freq = model$N,
          pos = seq_along(vars)
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
    
    if( inherits(x, "glm") ) {
      if (is.null(main)) main<- "Odds Ratio"
      ci<- confint(x)
      colnames(ci)<- c("conf.low", "conf.high")
      coef<- cbind(coef, ci)
      
      inds<- c("(Intercept)", inds)
      allTermsDF<- rbind(data.frame(var="(Intercept)", level="", N=NA, pos=1),
                         allTermsDF )
    } else{
      if (is.null(main)) main<- "Hazard Ratio"
    }
 
    gparam <- cbind(allTermsDF,  coef[inds, ])
    ggplot_table(gparam, gof, model, main, transform=transform, ...)
}


#' ggplot_table
#'
#' @param gparam Parameter Tabele mit var, estimate, p.value usw
#' @param gof Godnes of fit als Character
#' @param model model-Info
#' @param main Ueerschrift
#' @param refLabel Bezeichnung der Referenzklasse 
#' @param use_label Label aus stpvers
#' @param digits Digits
#' @param cpositions  Position
#' @param col.text,col.plot,fontsize an ggplot
#' @param transform  log-Transformation TRUE/FALSE
#'
#' @return
#' @export
#'
#' @examples
ggplot_table  <- function(gparam,
                        gof=NULL,
                        model=NULL,
                        transform = TRUE,
                        main = "",
                        refLabel = "reference", use_label=TRUE,
                         
                        digits=2,
                        cpositions = c(0.02, 0.22, 0.4),
                        fontsize = 0.7,
                        col.text = 1,
                        col.plot ="gray20"
                        
                        ) {
  require(ggplot2)
  
  rangeb <- range(gparam$conf.low, gparam$conf.high, na.rm = TRUE)
  breaks <- grDevices::axisTicks(rangeb / 2, log = TRUE, nint = 7)
  rangeplot <- rangeb
  rangeplot[1] <- rangeplot[1] - diff(rangeb)
  rangeplot[2] <- rangeplot[2] + 0.15 * diff(rangeb)
  
  width <- diff(rangeplot)
  y_variable <- rangeplot[1] + cpositions[1] * width
  y_nlevel <- rangeplot[1] + cpositions[2] * width
  y_cistring <- rangeplot[1] + cpositions[3] * width
  y_stars <- rangeb[2]
  x_annotate <- seq_len(nrow(gparam))
  annot_size_mm <- fontsize * 5
  #as.numeric(grid::convertX(unit(theme_get()$text$size, "pt"), "mm"))
    
  if(use_label){
    gparam$var<- useLabels(gparam$var, model$labels)
  }
  
  if (transform) {
    gparam <- transform(
      gparam,
      stars = ifelse(p.value < 0.001, "p<.001", stp25rndr::rndr_P(p.value)),
      est = ifelse(is.na(estimate), refLabel, format(exp(estimate), digits = digits)),
      ci = ifelse(
        is.na(estimate),
        "",
        stp25rndr::rndr_CI(exp(gparam[c("conf.low", "conf.high")]),
                           digits, " -", "(", ")")
      ),
      N = ifelse(is.na(N), "", paste0("(N=", N, ")"))
      
      
    )
    y_variable <-  exp(y_variable)
    y_nlevel <-   exp(y_nlevel)
    y_cistring <-   exp(y_cistring)
    y_stars <-   exp(y_stars)
    rangeplot <- exp(rangeplot)
    hline = 1
    gparam$estimate[is.na(gparam$estimate)] = 0
    gparam$var <- as.character(gparam$var)
    gparam$var[duplicated(gparam$var)] = ""
    gparam <- gparam[nrow(gparam):1,]
    gparam$estimate <-  exp(gparam$estimate)
    gparam$conf.high <-  exp(gparam$conf.high)
    gparam$conf.low <-  exp(gparam$conf.low)
  }
  else{
    breaks <- grDevices::axisTicks(rangeb , log = FALSE,   nint = 7)
    gparam <- transform(
      gparam,
      stars = ifelse(p.value < 0.001, "p<.001", stp25rndr::rndr_P(p.value)),
      est = ifelse(is.na(estimate), refLabel, format((estimate), digits = digits)),
      ci = ifelse(
        is.na(estimate),
        "",
        stp25rndr::rndr_CI((gparam[c("conf.low", "conf.high")]),
                           digits, " -", "(", ")")
      ),
      N = ifelse(is.na(N), "", paste0("(N=", N, ")"))
    )
    
    
    hline = 0
    gparam$estimate[is.na(gparam$estimate)] = 0
    gparam$var <- as.character(gparam$var)
    gparam$var[duplicated(gparam$var)] = ""
    gparam <- gparam[nrow(gparam):1,]
  }
  
  
  
 # return(gparam)
  #
  
  gg_scale <- function() {
    if (transform)
      scale_y_log10(
        name = "",
        labels = sprintf("%g", breaks),
        expand = c(0.02, 0.02),
        breaks = breaks
      )
    
    else
      scale_y_continuous(
        name = "",
        labels = sprintf("%g", breaks),
        expand = c(-0.02, 0.02),
        breaks = breaks
      )
    }

  p <- ggplot(gparam, 
              aes(seq_along(var), estimate)) +
    geom_rect(aes(
        xmin = seq_along(var) - 0.5,
        xmax = seq_along(var) + 0.5,
        ymin = rangeplot[1],
        ymax = rangeplot[2],
        fill = ordered(seq_along(var) %% 2 + 1)
      )) +
   scale_fill_manual(values = c("#FFFFFF33", "#00000033"), guide = "none") +
    geom_point(pch = 15, size = 4, colour = col.plot) +
    geom_errorbar(aes(ymin = conf.low, 
                      ymax = conf.high), 
                  width = 0.15, colour = col.plot) +
    geom_hline(yintercept = hline, linetype = 3) +
    coord_flip(ylim = rangeplot) +
    ggtitle(main) +
    gg_scale() +
    # scale_y_log10(
    #   name = "",
    #   labels = sprintf("%g", breaks),
    #   expand = c(0.02, 0.02),
    #   breaks = breaks
    # ) +
    theme_light() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
  #  xlab("") +
    annotate(
      geom = "text",
      x = x_annotate,
      y = y_variable,
      label = gparam$var,
      colour = col.text,
      fontface = "bold",
      hjust = 0,
      size = annot_size_mm
    ) +
    annotate(
      geom = "text",
      x = x_annotate,
      y = y_nlevel,
      hjust = 0,
      label = gparam$level,
      colour = col.text,
      vjust = -0.1,
      size = annot_size_mm
    ) +
    annotate(
      geom = "text",
      x = x_annotate,
      y = y_nlevel,
      label = gparam$N,
      colour = col.text,
      fontface = "italic",
      hjust = 0,
      vjust = ifelse(gparam$level == "", 0.5, 1.1),
      size = annot_size_mm
    ) +
    annotate(
      geom = "text",
      x = x_annotate,
      y = y_cistring,
      label = gparam$est,
      colour = col.text,
      size = annot_size_mm,
      vjust = ifelse(gparam$est == "reference", 0.5, -0.1)
    ) +
    annotate(
      geom = "text",
      x = x_annotate,
      y = y_cistring,
      label = gparam$ci,
      colour = col.text,
      size = annot_size_mm,
      vjust = 1.1,
      fontface = "italic"
    ) +
    annotate(
      geom = "text",
      x = x_annotate,
      y = y_stars,
      label = gparam$stars,
      colour = col.text,
      size = annot_size_mm,
      hjust = -0.2,
      fontface = "italic"
    ) +
    annotate(
      geom = "text",
      x = 0.5,
      y = y_variable,
      label = gof,
      size = annot_size_mm,
      colour = col.text,
      hjust = 0,
      vjust = 1.2,
      fontface = "italic"
    )
  
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  ggpubr::as_ggplot(gt)
}


