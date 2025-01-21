#' plotROC
#'
#' This function plots a ROC curve with ggplot2.
#' 
#' Stolen from ggroc.roc {pROC}
#' 
#' Calculate the empirical Receiver Operating Characteristic curve
#'
#' Given a binary outcome d and continuous measurement m, computes the empirical
#' ROC curve for assessing the classification accuracy of m
#'  
#' 
#' 
#' Some R Packages for ROC Curves: https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
#'
#' @param x 	a roc object from the roc function, or a list of roc objects.
#' @param col,colour character. Farbe colour ist für singel 
#' @param lwd,size numeric.  lwd
#' @param legend.position character. position 
#' @param legacy.axes,ann.segment logical.  x-achse 0 bis 1 oder 1 bis 0,  ann.segment = annotate("segment")
#' @param include.facet_wrap logical. facet
#' @param include.labels logical.mit Label()erstellte Beschriftungen TRUE/FALSE
#' @param ylab,xlab,main character.   false positive fraction = FPF
#' @param include.table,caption logical. Tabelle Ausgeben
#' @param include.order logical. sortieren nach auc
#' true positive fraction = TPF
#' @param ... description
#' @return ggplot
#' @export
#' @examples
#'
#'require(pROC)
#'
#'  data(aSAH)
#'  roc(outcome ~ s100b, data=aSAH) |>
#'  plotROC2()
#'  
#'  # mehr Beispiele unter ?Tbll_roc
plotROC2 <- function(x,..., 
                     ylab = "Sensitivity (TPR)",
                     xlab = if( legacy.axes) "1-Specificity (FPR)" else "Specificity (FPR)",
                     main = NULL,
                     lwd = 1.1,
                     size = lwd, 
                     col = NULL,
                     colour  = if (is.null(col)) "gray50" else col,
                     include.facet_wrap = FALSE,
                     legend.position = 'bottom',
                     include.labels = TRUE,
                     include.table = FALSE,
                     include.order = TRUE,
                    # include.plot = TRUE,
                     caption = "Optimal cut-point: Area Under the Curve (AUC), Youden Index (YI).",
                     legacy.axes = TRUE,
                     ann.segment = if(legacy.axes) c(0,1,0,1) else c(0,1, 1,0)
                    
                    ) {
  require(ggplot2)
  roc_plot <- NULL
  if(plyr::is.formula(x) | is.data.frame(x)){
    X <- stp25tools::prepare_data2(x,...)
    x <- pROC::roc(formula= as.formula(X$formula), data = X$data) 
  }
  
  if(include.table) stp25stat2::Tbll_roc(x) |> stp25output2::Output(caption)
 
  if (!inherits(x, "roc")) {
    if(include.order){ x <- reorder_auc(x) }
    roc_plot <-  
      pROC::ggroc(x, 
                  size = size,
                  legacy.axes = legacy.axes) 
  }
  else{
    roc_plot <-  
    pROC::ggroc(x, 
                size = size, colour = colour,
                legacy.axes = legacy.axes) 
    }

  if(!is.null(main)) 
    roc_plot <- 
      roc_plot + 
      ggtitle(main)
  
   if (!inherits(x, "roc")) {
     variable <- names(x)
     if(is.null(col)) col <- gg_color_hue(length(variable))
     
    if(include.labels){
      variable_lbl <- NULL
    for( i in names(x)) {
      variable_lbl <- append(variable_lbl,
                 if(!is.null(attr(x[[i]]$predictor, "label" ))) 
                   attr( x[[i]]$predictor, "label" ) 
                 else  x[[i]]$predictor.name)
    }
    roc_plot <- 
      roc_plot +
      scale_colour_manual(labels=variable_lbl, values = col)
    }
     else{
       roc_plot <- 
         roc_plot +
         scale_colour_manual(labels=variable, values = col)
     }
   } 
  

  roc_plot <-  
    roc_plot +
    ggplot2::ylab(ylab) + 
    ggplot2::xlab(xlab) +
    theme_roc(legend.position=legend.position)
  
  
  if( is.numeric(ann.segment))
    roc_plot <-  
    roc_plot + ggplot2::annotate("segment",
                      x = ann.segment[1], xend = ann.segment[2], 
                      y = ann.segment[3], yend = ann.segment[4], 
                      color="darkgrey", linetype="dashed")

  
  if( include.facet_wrap )
    roc_plot <- roc_plot + 
    facet_grid(. ~ name) + 
    theme(legend.position="none")
  

  roc_plot 
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

reorder_auc <- function(x) {
  auc_s <- sapply(x, "[[", "auc")
  my_order <- names(x[order(auc_s, decreasing = TRUE)])
  x[my_order]
}



#' @noRd
theme_roc <-
  function (base_size = 11,
            base_family = "",
            base_line_size = base_size / 22,
            base_rect_size = base_size / 22,
            legend.position = 'bottom'
  ) {
    
    theme_grey(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) %+replace%
      theme(
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        panel.grid = element_line(colour = "grey92"),
        panel.grid.minor = element_line(linewidth = rel(0.5)),
        strip.background = element_rect(fill = "grey85", colour = "grey20"),
        legend.position = legend.position,
        legend.title = element_blank(),
        complete = TRUE
      )
  }






# require(pROC)
# 
# require(stp25stat2)
# #
# n <- 200
# set.seed(n)
# D.ex <- rbinom(n, size = 1, prob = .5)
# 
# re_scale <- function(x, min=0, max=2){
#   x <- x - min(x)
#   x <- x/max(x)
#   x <- x*(max - min)
#   
#   round(x + min, 2)
# }
# DF <- data.frame(
#   Group = factor(c("Control", "Ill")[D.ex + 1]),
#   Cys = re_scale(rnorm(n, mean = D.ex, sd = .65), 18,84),
#   Glu = re_scale(exp(2 - (D.ex) - rnorm(n, mean = 0, sd = .5)), 23, 537),
#   Arg = re_scale(rnorm(n, mean = D.ex, sd = 2), 41,216),
#   Gln = re_scale(rnorm(n, mean = D.ex, sd = 1.5) + D.ex, 163,1129)
# ) |> stp25tools::Label(
#   
#   Arg  = "Arginine",
#   Orn = "Ornithine",
#   Cit = "Citrulline",
#   ADMA  = "ADMA",
#   SDMA  = "SDMA",
#   Cys = "Cysteine",
#   HCys  = "Homocysteine",
#   Met.Oxi = "Met/Met-SO",
#   #Met,Met.SO
#   Glu = "Glutamate",
#   Gln = "Glutamine",
#   His = "Histidine",
#   Kyn = "Kynurenine"
#   
# )
# 
# 
# 
# ## analysis ----------------------------------------------------------------
# require(stp25settings)
# 
# roc1 <- pROC::roc(Group ~ Glu, data = DF)
# Tbll_roc(roc1)
# # plotROC2(roc1)
# 
# roc.list <- pROC::roc(Group ~ Glu+Gln+Arg+Cys, data = DF) 
# Tbll_roc(roc.list)
# 
# #plotROC2(roc.list)  
# 
# 
# plotROC2(Group ~ Glu, data = DF, col = "red") 
# plotROC2(Group ~ Glu+Gln+Arg+Cys, data = DF) 
# 
# stop()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# m1 <- glm(Group ~  Arg , data = DF, family = binomial)
# myroc <- roc(DF$Group, predict(m1, DF, type = "response")) 
# plotROC2(myroc)
# 
# 
# stp25output2::Stop()
# 
# 
# 
# summary(DF[Cs(Cys,Glu,Arg,Gln)])
# 
# sample <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.7,0.3))
# train <- DF[sample, ]
# test <- DF[!sample, ]
# 
# 
# m1 <- glm(Group ~ Cys + Glu + Arg + Gln, data = DF, family = binomial)
# # library(pROC)
# # library(ggplot2)
#  
# myroc <- roc(DF$Group, predict(m1, DF, type = "response")) 
# 
# plotROC2(myroc)
# 
# 
# #fit logistic regression model to training set
# model <- glm(Group ~ Cys + Glu + Arg + Gln, data = train, family = binomial) 
# 
# #use model to make predictions on test set
# predicted <- predict(model, test, type="response")
# 
#  
# rocobj <- roc(test$Group, predicted)
#  
# 
# plot(rocobj)
# 
# 
# 
# 
# 
# #define object to plot and calculate AUC
# 
# auc <- round(auc(test$Group, predicted),2)
# 
# #create ROC plot
# ggroc(rocobj, colour = 'steelblue', size = 2) +
#   ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))
# 
# 
#  plotROC::geom_roc(rocobj)
#' ##
### plotROC
###
### Calculate the empirical Receiver Operating Characteristic curve
###
### Given a binary outcome d and continuous measurement m, computes the empirical
### ROC curve for assessing the classification accuracy of m
### Quelle :https://sachsmc.github.io/plotROC/
###
###
### Some R Packages for ROC Curves: https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
###
### @param ... an stp25tools::prepare_data2
### @param control level der Controllgruppe
### @param subset subset geht nicht
### @param include.facet_wrap logical. facet
### @param include.value,n.cuts,digits 	logical. Messwerte als Zahlen im Plot	 Nachkommastellen für die Messwerte
### @param include.labels logical.mit Label()erstellte Beschriftungen TRUE/FALSE
### @param ylab,xlab character.   false positive fraction = FPF
### true positive fraction = TPF
### @param include.output,digits.output logical.
### @param order logical. Sortierung der lebels
### @param make.positiv logical.  wenn eine Variable ins Negative geht automatich umstellen.
### @return ggplot
### @export
### @examples
###
### n <- 200
### set.seed(n)
### D.ex <- rbinom(n, size = 1, prob = .5)
###
### DF <- data.frame(
###   Group = factor(c("Control", "Ill")[D.ex + 1]),
###   Cys = round(rnorm(n, mean = D.ex, sd = .65), 2),
###   Glu = round(2 - (D.ex) - rnorm(n, mean = 0, sd = .5), 2),
###   Arg = round(rnorm(n, mean = D.ex, sd = 2), 2),
###   Gln = round(rnorm(n, mean = D.ex, sd = 1.5) + D.ex, 2)
### )
###
### DF |> plotROC2(Glu, Gln, Arg, Cys, by = ~ Group, digits =0)
###
###
### # library(ROCit)
### #
### # ROCit_obj <- rocit(score = DF$Glu, class = DF$Group)
### # summary(ROCit_obj)
### # plot(ROCit_obj)


# ###plotROC2 <- function(...,
#                     control = 1L,
#                     subset = NULL,
#                     digits = 2L,
#                     include.facet_wrap = FALSE,
#                     include.value = FALSE,
#                     include.labels = TRUE,
#                     n.cuts = if (include.value)
#                       5L
#                     else
#                       0,
#                     ylab = "Sensitivity (TPR)",
#                     xlab = "1-Specificity (FPR)",
#                     include.output = TRUE,
#                     digits.output = digits,
#                     order = TRUE,
#                     make.positiv = TRUE) {
#   if (!is.null(subset))
#     stop("\nsubset geht nicht!\n")
#   X <- stp25tools::prepare_data2(...)
# 
#   g_levels <- levels(X$data[[X$group.vars]])[control]
# 
#   if (length(X$group.vars) == 1L &
#       nlevels(factor(X$data[[X$group.vars]])) == 2L) {
#     X$data[[X$group.vars]] <-
#       ifelse(X$data[[X$group.vars]] == g_levels, 0L, 1L)
# 
# 
# 
#   } else{
#     stop("Die Gruppen duerfen nur zwei Faktoren (Control vs. Treat) beinhelten.\n")
#   }
# 
# 
# 
# 
#   auc <- NULL
#   rslt <- NULL
# 
# 
# 
# 
#   for (m in X$measure.vars) {
#     rslt1 <- plotROC::calculate_roc(X$data[[m]], X$data[[X$group.vars]])[-1, ]
#     auc1 <- comp_auc2(rslt1)
#     rslt[[m]] <- c(AUC = auc1, YIndex(rslt1$TPF, rslt1$FPF , rslt1$c), c = 1)
#     if (auc1 < 0.50) {
#       if (make.positiv) {
#         X$data[[m]] <-  -(X$data[[m]])
#         auc1 <- 1 - auc1
#         rslt[[m]] <- c(AUC = auc1, YIndex(rslt1$FPF, rslt1$TPF, rslt1$c), c= -1)
#       }
#     }
#     auc <- append(auc, auc1)
#   }
# 
#   dat_roc <- plotROC::melt_roc(
#     data = X$data,
#     d = X$group.vars,
#     m = X$measure.vars
#   )
# 
# 
#   dat_roc$name <- factor(dat_roc$name)
#   if (order) {
#     X$measure.vars <- X$measure.vars[order(auc, decreasing = TRUE)]
#     X$row_name <- X$row_name[order(auc, decreasing = TRUE)]
#   }
# 
# 
#   rslt <-
#     stp25stat2::fix_format(
#     t(as.data.frame(rslt)),
#     digits = c(NA, 2, 2, 2, digits.output, NA))
#     rslt$YI_Cutoff  <- ifelse(rslt$c == "1", paste(">",  rslt$YI_Cutoff),
#                             paste("<",  rslt$YI_Cutoff))
# 
# 
# 
#   if (include.labels) {
#     dat_roc$name <-
#       factor(dat_roc$name, X$measure.vars, make.unique(X$row_name))
#     rslt[[1]] <-   factor(rslt[[1]], X$measure.vars, make.unique(X$row_name))
#     }
#   else {
#     dat_roc$name <- factor(dat_roc$name, X$measure.vars)
#     }
# 
#   names(dat_roc)[1] <- "D.Group"
# 
# 
#   if (include.facet_wrap) {
#     ggroc <-  "facet_wrap"
# 
#     ggroc <-
#       ggplot2::ggplot(dat_roc, ggplot2::aes(d = D.Group, m = M)) +
#       plotROC::geom_roc(n.cuts = n.cuts,
#                         labels = include.value,
#                         labelround = digits) +
#       ggplot2::facet_wrap( ~ name) +
#       plotROC::style_roc()  +
#       ggplot2::theme(
#         legend.position = 'bottom',
#         legend.box = "vertical",
#         legend.title = ggplot2::element_blank(),
#         legend.key.size = ggplot2::unit(1, "lines") ,
#         legend.box.spacing =  ggplot2::unit(1, "lines"),
#         plot.margin = ggplot2::margin(
#           t = 1,
#           r = 0,
#           l = 0,
#           b = 0.2,
#           unit = "lines"
#         )
#       )
# 
# 
#   }
#   else {
#     ggroc <-
#       ggplot2::ggplot(dat_roc, ggplot2::aes(d = D.Group, m = M, color = name)) +
#       plotROC::geom_roc(n.cuts = n.cuts,
#                         labels = include.value,
#                         labelround = digits) +
#       plotROC::style_roc(xlab = xlab, ylab = ylab)  +
#       ggplot2::theme(
#         legend.position = 'bottom',
#         legend.box = "vertical",
#         legend.title = ggplot2::element_blank(),
#         legend.key.size = ggplot2::unit(1, "lines") ,
#         legend.box.spacing =  ggplot2::unit(1, "lines"),
#         plot.margin = ggplot2::margin(
#           t = 1,
#           r = 0,
#           l = 0,
#           b = 0.2,
#           unit = "lines"
#         )
#       )
#   }
# 
# 
#   if (include.output)
#     stp25output2::Output(
#       rslt[1:5],
#       "Optimal cut-point: Area Under the Curve (AUC), Youden Index (YI)."
#       #Reference:",  g_levels
# 
#     )
# 
# 
#   ggroc
# }
# 
# 
# 
# 
# 
# 
# 
# comp_auc2 <- function(df) {
#   auc <- 0
#   for (i in 1:length(df$FPF)) {
#     auc <- auc + 0.5 * (df$FPF[i] - df$FPF[i - 1]) * (df$TPF[i] + df$TPF[i - 1])
#   }
#   auc
# }
# 
# 
# ### Youden’s J statistic###### Optimal (Youden Index) point###### The Youden index uses the maximum vertical distance of the ROC curve from the point (X, Y)### on the diagonal (random line). In fact, the Youden index maximizes the difference### between the Se and FP rate, in other words, it maximizes the percentage of Net correct### classification:###### Therefore, the optimal cut-off point is calculated by maximizing Se+Sp at### different cut-off points###### Quelle: https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-024-02198-2###YIndex <- function(y1, x1, cutoff) {
#   diff_yx <- y1 - x1
#   maxIndex <- which.max(diff_yx)
# 
# 
#   c(YI_TPR = x1[maxIndex],
#     YI_FPR = y1[maxIndex],
#     YI_Cutoff = cutoff[maxIndex])
# }
# 
# ### Mehr oder weniger eine kopie von plotROC###comp_auc2 <- function(df) {
#   auc <- 0
#   for (i in 2:length(df$FPF)) {
#     auc <- auc + 0.5 * (df$FPF[i] - df$FPF[i - 1]) * (df$TPF[i] + df$TPF[i - 1])
#   }
#   auc
# }
# 

# #DF |> plotROC2(Glu, Gln, Arg, Cys, by = ~ Group, digits =0)


 
