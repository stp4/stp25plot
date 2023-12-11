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
#' forest_plot(fit1, plot="ggplot_forest", main="Hallo")
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
#' # Alternative
#' 
#' 
#' #' library('ggplot2') 
#' 
#' Outcome_order <-
#'   c('Outcome C', 'Outcome A', 'Outcome B', 'Outcome D')
#' 
#' #this is the first dataset you have
#' df1 <-
#'   data.frame(
#'     Outcome = c("Outcome A", "Outcome B", "Outcome C", "Outcome D"),
#'     estimate = c(1.50, 2.60, 1.70, 1.30),
#'     conf.low = c(1.00, 0.98, 0.60, 1.20),
#'     conf.high = c(2.00, 3.01, 1.80, 2.20)
#'   )
#' 
#' 
#' 
#' 
#' # add a group column
#' df1$group <- "X"
#' # create a second dataset, similar format to first
#' df2 <- df1
#' # different group
#' df2$group <- "Y"
#' # and we adjust the values a bit, so it will look different in the plot
#' df2[, c("estimate", "conf.low", "conf.high")] <-
#'   df2[, c("estimate", "conf.low", "conf.high")] + 0.5
#' 
#' # combine the two datasets
#' df = rbind(df1, df2)
#' # you can do the factoring here
#' df$Outcome = factor (df$Outcome, level = Outcome_order)
#' 
#' df
#' 
#' 
#' 
#' 
#' 
#' p <- ggplot(df,
#'             aes(
#'               x = Outcome,
#'               y = estimate,
#'               ymin = conf.low ,
#'               ymax = conf.high,
#'               col = group,
#'               fill = group
#'             )) +
#'   #specify position here
#'   
#'   geom_hline(yintercept = c(0, 20, 40, 60), lty = 2) +
#'   geom_vline(xintercept = seq(0, 15) + .5, lty = 2, col = "gray") +
#'   geom_linerange(linewidth = 3, position = position_dodge(width = .7)) +
#'   #specify position here too
#'   geom_point(
#'     size = 3,
#'     shape = 21,
#'     colour = "white",
#'     stroke = 0.5,
#'     position = position_dodge(width = .7)
#'   ) +
#'   # scale_fill_manual(values = barCOLS) +
#'   # scale_color_manual(values = dotCOLS) + 
#'   
#'   guides( 
#'     colour = guide_legend(reverse=TRUE),
#'     fill = guide_legend(reverse=TRUE)
#'   ) +
#'   
#'   scale_x_discrete(name = "") +
#'   scale_y_continuous(name = "Estimate 95% CI", limits = c(-0, 5)) +
#'   coord_flip() +
#'   # GGally::geom_stripped_cols()+
#'   #theme_void()
#'   theme_minimal() +
#'   theme( 
#'     
#'     panel.grid.major = element_blank(),
#'     panel.grid.minor = element_blank())
#' 
#' 
#' 
#' dotCOLS = c(  "#74C476","#BAE4B3","#f9b282")
#' barCOLS = c("#006D2C" , "#74C476","#A63603")
#' 
#' 
#' p  +  
#'   scale_fill_manual(values = barCOLS) +
#'   scale_color_manual(values = dotCOLS)
forest_plot <- function(x, ...) {
  UseMethod("forest_plot")
}

 
#' @rdname forest_plot
#' @export
#'
forest_plot.data.frame <- function(x,   
                                   main = NULL,
                                   include.gof = NULL,
                                   replace.lable.dots=TRUE,
                                   scale.log = FALSE,
                                   plot = TRUE,
                                   ...) {
  if(replace.lable.dots) {
    x$var<-  gsub("\\.", " ", x$var)
    
  }
  
  if (is.logical(plot)){
    if (!plot) x 
    else  ggplot_table(x,
                       gof =  include.gof,
                       main = main,
                       scale.log = scale.log,
                       ...) 
  }
  else if(plot == "ggplot_table" ){
    ggplot_table(x,
                 gof =  include.gof,
                 main = main,
                 scale.log = scale.log,
                 ...) 
    
  }
  else ggplot_forest(x, ...)
}


#' @rdname forest_plot
#' @export
#'
forest_plot.default <- function(x,
                        data = insight::get_data(x), # x$model,
                        main = NULL,
                        include.indercept = TRUE,
                        include.referenze = TRUE,
                        include.gof = TRUE,
                        include.label=FALSE,
                        replace.lable.dots=TRUE,
                        standardize = FALSE, 
                        scale.log = FALSE,
                        plot = TRUE,
                        ...) {
  
  if(is.character(include.gof)){ gof <- include.gof }
  else if (include.gof){ gof <- stp25stat2::APA(x)}
  else {gof <- NULL}
  
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
  
  if(replace.lable.dots) {
    gparam$var<-  gsub("\\.", " ", gparam$var)
    
  }
  
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


#' ggplot_table
#'
#'
#' In forest_plot( verwendet)
#' Gestohlen von survminer::ggforest()
#'
#' @param gparam Parameter Tabele mit var, estimate, p.value usw
#' @param gof Godnes of fit als Character
#' @param main Ueerschrift
#' @param refLabel Bezeichnung der Referenzklasse
#' @param digits Digits
#' @param cpositions  shift Position left side (Text: Item - levels - estimate)
#' @param right.hjust,left.hjust  hjust erster und letzter Text
#' @param space.right,space.left space Plot

#' @param point.size,point.size.n point.size.n=TRUE Variable groesse der Punkte
#' @param col.text,col.plot,col.fill an ggplot c("#FFFFFF33", "#00000033") 
#' HEX8 Code die letzten stellen 33 sind fuer die Farb-Intensitaet
#' @param scale.log  log-Transformation of x-scale TRUE/FALSE
#'
#' @return ggplot
#' @export
#' @examples 
#' 
#' ggplot_table(
#' data.frame(
#'   var = c("Intercept", "Sex", "Sex", "Alter"),
#'   level = c(NA, "male", "female", NA),
#'   N = c(NA, 25, 47, 25+47),
#'   # pos = c(1, 1, 2, 1),
#'   estimate = c(-.77, NA, .51 , .4),
#'   conf.low = c(-1.53, NA, -.17, .2),
#'   conf.high = c(-0.1, NA, 1.2, .6),
#'   p.value = c(0.046, NA, 0.1407, 0.0021)
#' )
#' )
#' 
ggplot_table <- function(gparam,
            main = "", 
            gof = NULL,
            scale.log = FALSE,
            main.hjust = 0.5,
            left.hjust = -0.2,
            right.hjust = 0,
            space.right = 0.15,
            space.left=0.02,
            space.shift.left = 0,
            space.shift.n =0.22,
            space.shift.est =0.4,
            
            #  cpositions = c(space.left, space.shift.n, 0.4),  
            refLabel = "reference",
            rangeb = range(gparam$conf.low,
                           gparam$conf.high, na.rm = TRUE),
            digits = 2,
            cex= .7,
            pch=15,
            fontsize = cex,
            point.size = 4,
            point.size.n = FALSE,
            col.text = 1,
            col.plot = "gray20",
            col.fill = NULL , #c("#FFFFFF33", "#00000033")
            lty.hline= 1
) {
  
  require(ggplot2)
  gg_scale <- function() {
    if (scale.log)
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
  
  if(!all(
    c("var","level","N","estimate","conf.low","conf.high","p.value") %in% 
    names(gparam)
  )) stop("data.frame ist nicht vollstaendig!")
  
  
  
  
  if( point.size.n) {
    
    point.size.n <- gparam$N / max(gparam$N , na.rm=TRUE) 
    point.size.n[1] <- 1
    point.size <-  round(rev(point.size.n)*point.size/2 + point.size/2,1)
    
  }
  
  breaks <- grDevices::axisTicks(rangeb / 2, log = TRUE, nint = 7)
  rangeplot <- rangeb
  rangeplot[1] <- rangeplot[1] - diff(rangeb) + space.shift.left * diff(rangeb)
  rangeplot[2] <- rangeplot[2] + space.right * diff(rangeb)
  
  width <- diff(rangeplot)
  
  pos_item <- rangeplot[1] + space.left * width   # position links
  pos_levels <- rangeplot[1] + space.shift.n * width
  pos_estimate <- rangeplot[1] + space.shift.est * width
  pos_pvalue <- rangeb[2]   # position rechts
  
  
  
  
  pos_zeile <- seq_len(nrow(gparam))
  annot_size_mm <- fontsize * 4.5
  
  if (is.null(col.fill)) 
    col.fill <- c("#FFFFFF33", "#FFFFFF33")
  
  #as.numeric(grid::convertX(unit(theme_get()$text$size, "pt"), "mm"))
  
  
  if (scale.log) {
    
    gparam <- transform(
      gparam,
      stars = ifelse(p.value < 0.001, "p<.001", stp25stat2:::rndr_P(p.value)),
      est = ifelse(is.na(estimate), refLabel, 
                   stp25stat2::render_f(exp(estimate), digits = digits)),
      ci = ifelse(
        is.na(estimate),
        "",
        stp25stat2:::rndr_CI(exp(gparam[c("conf.low", "conf.high")]),
                           digits, " -", "(", ")")
      ),
      N = ifelse(is.na(N), "", paste0("(N=", N, ")"))
      
      
    )
    
    pos_item <-  exp(pos_item)
    
    pos_levels <-   exp(pos_levels)
    pos_estimate <-   exp(pos_estimate)
    pos_pvalue <-   exp(pos_pvalue)
    rangeplot <- exp(rangeplot)
    hline = 1
    gparam$estimate[is.na(gparam$estimate)] = 0
    gparam$var <- as.character(gparam$var)
    gparam$var[duplicated(gparam$var)] = ""
    gparam <- gparam[nrow(gparam):1, ]
    gparam$estimate <-  exp(gparam$estimate)
    gparam$conf.high <-  exp(gparam$conf.high)
    gparam$conf.low <-  exp(gparam$conf.low)
  }
  else{
    breaks <- grDevices::axisTicks(rangeb , log = FALSE,   nint = 7)
    
    gparam <- transform(
      gparam,
      stars = ifelse(p.value < 0.001, "p<.001", stp25stat2:::rndr_P(p.value)),
      est = ifelse(is.na(estimate), refLabel, 
                   stp25stat2::render_f(estimate, digits = digits)),
      ci = ifelse(
        is.na(estimate),
        "",
        stp25stat2:::rndr_CI((gparam[c("conf.low", "conf.high")]),
                           digits, " -", "(", ")")
      ),
      N = ifelse(is.na(N), "", paste0("(N=", N, ")"))
    )
    
    
    hline = 0
    gparam$estimate[is.na(gparam$estimate)] = 0
    gparam$var <- as.character(gparam$var)
    gparam$var[duplicated(gparam$var)] = ""
    gparam <- gparam[nrow(gparam):1, ]
  }
  
  # cat("\n ggplot_table -> transform\n")
  fill_rec <- if ( length(col.fill) == 2 ) 
    ordered(seq_along(gparam$var) %% 2 + 2) else 
      ordered(seq_along(gparam$var))
  
  # ggplot
  y_shift <- 0.5
  
  
 
  p <-
    ggplot(gparam, aes(seq_along(var), estimate)) + 
    geom_rect(
      aes(
        xmin = seq_along(var) - y_shift, xmax = seq_along(var) + y_shift,
        ymin = rangeplot[1], ymax = rangeplot[2],
        fill = fill_rec)) +
    scale_fill_manual(values = rev(col.fill), guide = "none") +
    geom_point(pch = pch,
               size = point.size,
               colour = col.plot) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15, colour = col.plot) +
    geom_hline(yintercept = hline, linetype = 3) +
    geom_vline(xintercept = c(y_shift, seq_along(gparam$var)+y_shift), 
               linetype = lty.hline) +
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
      axis.text.y =  element_blank(),
      axis.ticks.y = element_blank(),
      plot.title =   element_text(hjust = main.hjust)
    ) +
    # xlab("wo bin ich") +
    annotate(geom = "text",
             x = pos_zeile,y = pos_item,
             label = gparam$var,
             colour = col.text,
             fontface = "bold",
             hjust = right.hjust,
             size = annot_size_mm
    ) +
    annotate(geom = "text",
             x = pos_zeile,y = pos_levels,
             label = gparam$level,
             colour = col.text,
             hjust = 0,vjust = -0.1,
             size = annot_size_mm
    ) +
    annotate(geom = "text",
             x = pos_zeile,y = pos_levels,
             label = gparam$N,
             colour = col.text,
             fontface = "italic",
             hjust = 0,vjust = ifelse(gparam$level == "", y_shift, 1.1),
             size = annot_size_mm
    ) +
    annotate(geom = "text",
             x = pos_zeile,y = pos_estimate,
             label = gparam$est,
             colour = col.text, 
             vjust = ifelse(gparam$est == "reference", y_shift,-0.1),
             size = annot_size_mm
    ) +
    annotate(geom = "text",
             x = pos_zeile,y = pos_estimate,
             label = gparam$ci,
             colour = col.text,
             vjust = 1.1, size = annot_size_mm,
             fontface = "italic"
    ) +
    annotate(geom = "text",
             x = pos_zeile, y = pos_pvalue,
             label = gparam$stars,
             colour = col.text,
             size = annot_size_mm,
             hjust =  left.hjust,
             # -0.2
             fontface = "italic"
    )
  
  if (!is.null(gof))
    p <-  p +
    annotate(geom = "text",
             x = y_shift,y = pos_item,
             label = gof,
             size = annot_size_mm,
             colour = col.text,
             hjust = right.hjust, vjust = 1.2,
             fontface = "italic"
    )
  
  
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  ggpubr::as_ggplot(gt)
}



#' Draw a Forestplot of Measures of Effects
#' 
#'  Intern wird geom_pointrange verwendet
#'
#' Visualize multiple measures of effect with their confidence intervals in a
#' vertical layout.
#' 
#' stolen from https://github.com/NightingaleHealth/ggforestplot
#' 
#'  Anmerkung: 
#'   ggforestplot scheint nicht mehr betreut zu werden - manche 
#'    ggplot Funktionen sind nict mehr aktuell!
#'
#' @param df A data frame 
#' @param name,estimate,se,pvalue, the variable 
#' @param legend.title  Beschriftung  main, xlab 
#' 
#' @param groups,colour,shape the variable
#' @param logodds logical (defaults to FALSE) 
#' @param ...   main, xlab, graphical parameters
#' @param col.strip,col Farbe strips und Symbole col.strip =NULL keine strips
#' @param cex size
#' @param cex.pch, pch size symbol
#' @param dodge.width  abstand bei Gruppen zwischen den Balken
#' 
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom stats qnorm
#' @importFrom magrittr %>%
#' @importFrom rlang := !! enquo quo_is_null
#' @importFrom grDevices axisTicks
#' @importFrom scales trans_breaks
#' 
#' @examples 
#' 
#'  # ----------------------------------------
#'  # Draw a Forestplot of Measures of Effects
#'  # ----------------------------------------
#' 
#' require(magrittr)
#' require(ggplot2)
#' require(stp25plot)
#' 
#' load(
#'   "C:/Users/wpete/Dropbox/1_Projekte/002-NURMI/791_Martina_Gregori/dummy-forest-df.Rdata"
#' )
#' 
#' data$std.error[is.na(data$estimate)] <- 0
#' data$conf.low[is.na(data$estimate)] <- 0
#' data$conf.high[is.na(data$estimate)] <- 0
#' data$estimate[is.na(data$estimate)] <- 0
#' data <- data[-which(data$var == "age"), ]
#' data <- data[-which(data$var == "(Intercept)"), ]
#' 
#' data$level <- factor(data$level,
#'                      c("M/UM", "HM" , "10 km"),
#'                      c("M/UM", "HM", "10 km (Reference)"))
#' 
#' dotCOLS = c(  "#74C476","#9E9AC8","#f9b282")
#' barCOLS = c("#006D2C" , "#756BB1","#A63603")
#' 
#' 
#' ggplot_forest(
#'   data,
#'   name = group,
#'   groups = level ,
#'   title = "Associations to Running",
#'   xlab = "Estimate (95% CI)",
#'   legend.title = "legend title using guide",
#'   col = barCOLS,
#'   cex.pch = 3,
#'   cex = 1.1,
#'   # cex.pch =  1,
#'   pch = c(19, 15, 17)
#' )
#' 
#' #----------------------------
#' # Alternative geom_pointrange
#' #----------------------------
#' 
#' ggplot(data = data, aes(x = group  , y = estimate)) +
#'   geom_pointrange(
#'     mapping = aes(
#'       ymin = conf.low ,
#'       ymax = conf.high,
#'       color = level,
#'       shape = level
#'     ),
#'     position = ggplot2::position_dodge(width =  0.5)
#'   ) +
#'   GGally::geom_stripped_cols(odd = "#00000000", even = "#11111111") +
#'   ggplot2::guides(
#'     colour = guide_legend(
#'       reverse = TRUE,
#'       title = "legend.title",
#'       override.aes =  list(linewidth = 1)
#'     ),
#'     shape = guide_legend(reverse = TRUE, title = "legend.title")
#'   ) +
#'   scale_color_manual(values = dotCOLS) +
#'   stp25plot::theme_forest(base_size = 12) +
#'   coord_flip()
#' 
#' #-----------------------------
#' #' # Alternative geom_errorbar
#' #-----------------------------
#' 
#' ggplot(data = data, aes(x = group, y = estimate)) +
#'   geom_point(
#'     mapping = aes(
#'       color = level,
#'       shape = level
#'     ),
#'     position = ggplot2::position_dodge(width =  0.5)
#'   ) +
#'   geom_errorbar(
#'     mapping = aes(
#'       ymin = conf.low ,
#'       ymax = conf.high,
#'       color = level,
#'       width = 0.2
#'     ),
#'     position = ggplot2::position_dodge(width =  0.5)
#'   ) +
#'   GGally::geom_stripped_cols(odd = "#00000000", even = "#11111111") +
#'   coord_flip() +
#'   scale_color_manual(values = dotCOLS) +
#'   stp25plot::theme_forest(base_size = 12)
#' 
#' #---------------------------
#' # Alternative geom_linerange
#' #---------------------------
#' 
#' p <- ggplot(data,
#'             aes(
#'               x = group,
#'               y = estimate,
#'               ymin = conf.low ,
#'               ymax = conf.high,
#'               col = level,
#'               fill = level
#'             )) +
#'   #specify position here
#'   #geom_hline(yintercept = c(0, 20, 40, 60), lty = 2) +
#'   # geom_vline(xintercept = seq(0, 15) + .5, lty = 2, col = "gray") +
#'   geom_linerange(linewidth = 3, position = position_dodge(width = .7)) +
#'   #specify position here too
#'   geom_point(
#'     size = 3,
#'     shape = 21,
#'     colour = "white",
#'     stroke = 0.5,
#'     position = position_dodge(width = .7)
#'   ) +
#'   # scale_fill_manual(values = barCOLS) +
#'   # scale_color_manual(values = dotCOLS) +
#'   
#'   guides(
#'     colour = guide_legend(reverse=TRUE),
#'     fill = guide_legend(reverse=TRUE)
#'   ) +
#'   
#'   scale_x_discrete(name = "") +
#'   scale_y_continuous(name = "Estimate 95% CI", limits = c(-10, 5)) +
#'   coord_flip() +
#'   
#'   stp25plot::theme_forest(base_size = 12)
#' # theme_minimal() +
#' # theme(
#' #   panel.grid.major = element_blank(),
#' #   panel.grid.minor = element_blank())
#' 
#' p  +
#'   scale_fill_manual(values = barCOLS) +
#'   scale_color_manual(values = dotCOLS)
#' 

ggplot_forest <-
  function (data,
            name = term,
            estimate = estimate,
            conf.low = conf.low,
            conf.high = conf.high,
            se = NULL,
            #std.error,
            #  pvalue = NULL,
            groups = NULL,
            colour = NULL,
            shape = NULL,
            logodds = FALSE,
            # gesteuert ueber transparenz  "#FFFFFF33", "#00000033"
            col.strip = c(even = "#00000000", odd = "#33333333"  ),
            col = NULL,
            cex = 1,
            cex.pch = 1.2,
            cex.pch.legende = cex.pch ,
            
            dodge.width = .5,
            pch = c(21L, 22L, 23L, 24L, 25L),
            main = NULL,
           # title=NULL,
            xlab =NULL,
            xlim =NULL,
            xtickbreaks=NULL,
            
            legend.title =  ggplot2::waiver(),
            ...) {
    
    stopifnot(is.data.frame(data))
    stopifnot(is.logical(logodds))
    
    if(is.null(data$term))
      data$term <- 
      paste(data$var, ifelse(is.na(data$level), "",  data$level))
    
    name <-     rlang::enquo(name)
    estimate <- rlang::enquo(estimate)
    se <-       rlang::enquo(se)
    conf.low <- rlang::enquo(conf.low)
    conf.high <- rlang::enquo(conf.high)
    
    groups <-   rlang::enquo(groups)
    if (!rlang::quo_is_null(groups)) {
      colour <- shape <-  groups
    }
    else{
      colour <-   rlang::enquo(colour)
      shape <-    rlang::enquo(shape)
    }
    args <-     list(...)
    
    
    
    # calc 95%- CI if needed
    if (rlang::quo_is_null(se)) {
      data <- data %>%
        tidyr::drop_na(!!estimate) %>%
        dplyr::mutate(
          `:=`(
            !!name,
            factor(
              !!name,
              levels = !!name %>% unique() %>% rev(),
              ordered = TRUE
            )
          ),
          .xmin = !!conf.low,
          .xmax = !!conf.high,
          # .filled = TRUE,
          .label = sprintf("%.2f",!!estimate)
        )
      
    }
    else{
      const <- stats::qnorm(1 - (1 - .95) / 2)
      data <- data %>%
        tidyr::drop_na(!!estimate) %>%
        dplyr::mutate(
          `:=`(
            !!name,
            factor(
              !!name,
              levels = !!name %>% unique() %>% rev(),
              ordered = TRUE
            )
          ),
          .xmin = !!estimate - const * !!se,
          .xmax = !!estimate + const * !!se,
          # .filled = TRUE,
          .label = sprintf("%.2f",!!estimate)
        )
    }
    
    if (logodds) {
      data <-
        data %>%
        mutate(
          .xmin = exp(.data$.xmin),
          .xmax = exp(.data$.xmax),
          `:=`(!!estimate, exp(!!estimate))
        )
    }
    
    #' plot
    g <- ggplot2::ggplot(data,
                         ggplot2::aes(x = !!estimate, y = !!name))
    if (logodds) {
      g <- if ("xtickbreaks" %in% names(args))
        g + scale_x_continuous(trans = "log10", breaks = args$xtickbreaks)
      else
        g + scale_x_continuous(trans = "log10", breaks = scales::log_breaks(n = 7))
    }
    
    g <- g + theme_forest(base_size = cex * 13)
    
    if (!is.null(col.strip))
      g <- g + geom_stripes(odd = col.strip[1], even = col.strip[2])
    
    
    g <- g +
      ggplot2::geom_vline(
        xintercept = ifelse(test = logodds, yes = 1, no = 0),
        linetype = "solid",
      #  size = 0.4,
        linewidth= 0.4,
        colour = "black"
      )
    
    g<- g +
      geom_effect2(
        ggplot2::aes(
          xmin = .data$.xmin,
          xmax = .data$.xmax,
          colour = !!colour,
          shape = !!shape
        ),
        size = cex.pch*.5,
        stroke = cex.pch,
        dodge.width = dodge.width
      ) 
    
    
    # g1 <- g +
    #   geom_pointrange(
    #     ggplot2::aes(
    #       xmin = .data$.xmin,
    #       xmax = .data$.xmax,
    #       colour = !!colour,
    #       shape = !!shape
    #     ),
    #   #  size = cex.pch,
    #     stroke = cex.pch,
    #     position = ggplot2::position_dodge(width =  0.5)
    #   
    # 
    #   ) 
    
    #'   geom_point(
    #'     mapping = aes(
    #'       color = level,
    #'       shape = level
    #'     ),
    #'     position = ggplot2::position_dodge(width =  0.5)
    #'   ) +
    #'   geom_errorbar(
    #'     mapping = aes(
    #'       ymin = conf.low ,
    #'       ymax = conf.high,
    #'       color = level,
    #'       width = 0.2
    #'     ),
    
    
    
    g <- g +
      ggplot2::scale_shape_manual(values = pch) +
      ggplot2::guides(
        colour = guide_legend(
          reverse = TRUE,
          title = legend.title,
          override.aes =
            list(size = cex.pch.legende/2)
        ),
        shape = guide_legend(reverse = TRUE, 
                             title = legend.title),
        
        
        
      )
    
    if (!is.null(col))
      g <- g + scale_colour_manual(values = rev(col))
    
    g <- g + labs(y = "")
    
    if ( !is.null(main) ) g <- g + labs(title = main)
    if ( !is.null(xlab) )  g <- g + labs(x = xlab)
    if ( !is.null(xlim) ) g <- g + coord_cartesian(xlim = xlim)
    if ( !is.null(xtickbreaks)  & !logodds) g <- g + scale_x_continuous(breaks = xtickbreaks)
    
    
    g
  }


#' @rdname ggplot_forest
#' 
#' @description 
#' 
#' 
#' Alternating Background Colour
#'
#' Add alternating background color along the y-axis. The geom takes default
#' aesthetics \code{odd} and \code{even} that receive color codes. The codes
#' would preferably be in the 8-hex ARGB format to allow for transparency if
#' the geom is meant to be used as visual background.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @author Ilari Scheinin
geom_stripes <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStripes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

GeomStripes <- ggplot2::ggproto(
  "GeomStripes",
  ggplot2::Geom,
  required_aes = c("y"),
  
  default_aes = ggplot2::aes(
    xmin = -Inf,
    xmax = Inf,
    odd = "#22222222",
    even = "#00000000",
    # Change 'size' below from 0 to NA.
    # When not NA then when *printing in pdf device* borders are there despite
    # requested 0th size. Seems to be some ggplot2 bug caused by grid overriding
    # an lwd parameter somewhere, unless the size is set to NA. Found solution here
    # https://stackoverflow.com/questions/43417514/getting-rid-of-border-in-pdf-output-for-geom-label-for-ggplot2-in-r
    alpha = NA,
    colour = "black",
    linetype = "solid",
    size = NA
  ),
  
  # draw_key = ggplot2::draw_key_blank,
  draw_key = ggplot2::draw_key_rect,
  
  draw_panel = function(data, panel_params, coord) {
    ggplot2::GeomRect$draw_panel(
      data %>%
        dplyr::mutate(
          y = round(.data$y),
          ymin = .data$y - 0.5,
          ymax = .data$y + 0.5
        ) %>%
        dplyr::select(
          .data$xmin,
          .data$xmax,
          .data$ymin,
          .data$ymax,
          .data$odd,
          .data$even,
          .data$alpha,
          .data$colour,
          .data$linetype,
          .data$size
        ) %>%
        unique() %>%
        dplyr::arrange(.data$ymin) %>%
        dplyr::mutate(
          .n = dplyr::row_number(),
          fill = dplyr::if_else(
            .data$.n %% 2L == 1L,
            true = .data$odd,
            false = .data$even
          )
        ) %>%
        dplyr::select(-.data$.n, -.data$odd, -.data$even),
      panel_params,
      coord
    )
  }
)


#' @rdname ggplot_forest
#' 
#' @description 
#' 
#'  
#' Horizontal Study Effects with Confidence Intervals
#'
#' Builds a custom version of \code{\link[ggstance]{geom_pointrangeh}}.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggstance::geom_pointrangeh
#' @author Ilari Scheinin
geom_effect2 <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         dodge.width = 0.5,
                         position = ggplot2::position_dodge(width =  dodge.width),
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEffect2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(  na.rm = na.rm, ...)
  )
}


#' @noRd
GeomEffect2 <- ggplot2::ggproto(
  "GeomEffect",
  ggplot2::Geom,
  default_aes = ggplot2::aes(
    colour = "black",
    size = 0.6,
    linetype = 1,
    shape = 21,
    fill = "black",
    alpha = NA,
    stroke = 1,
    filled = TRUE
  ),
  
  # Implement draw_key
  draw_key = 
    function(data, params, size) {
      if (is.character(data$shape)) {
        data$shape <- translate_shape_string(data$shape)
      }
      grid::pointsGrob(
        0.5,
        0.5,
        pch = data$shape,
        gp = grid::gpar(
          col = scales::alpha(data$colour, data$alpha),
          # fill = scales::alpha(data$fill, data$alpha),
          fill = scales::alpha(data$colour, data$alpha),
          fontsize = data$size * .pt + data$stroke * .stroke / 2,
          lwd = data$stroke * .stroke / 2)
      )
    },
  
  required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),
  draw_panel = function(data,
                        panel_params,
                        coord) {
    ggplot2::GeomPointrange$draw_panel(
    
      dplyr::mutate(data,
                    fatten = 1
                    #  fatten	  A multiplicative factor used to increase 
                    # the size of the middle point in geom_pointrange().
                    # fill = dplyr::case_when(
                    #   is.na(.data$filled) ~ "#00000000",!.data$filled ~ "white",
                    #   TRUE ~ .data$colour
                    # )
      ),
      panel_params,
      coord,
      flipped_aes = TRUE
    )
  }
)



#' @rdname ggplot_forest
#' @export
theme_forest <-
  function (base_size = 13,
            base_line_size = base_size / 22,
            base_rect_size = base_size / 22) {
    ggplot2::theme_minimal(
      base_size = base_size,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) +
      ggplot2::theme(#rect = element_blank(),
        text = element_text(colour = "black")) %+replace%
      ggplot2::theme(
        axis.line.x = element_line(
          color = "black",
          linewidth = .5,
          lineend = "square"
        ),
        
        plot.title = element_text(face = "bold", hjust = 0),
        axis.text.y = element_text(colour = "black"),
        axis.text.y.right = element_text(hjust = 1),
        axis.text.x = element_text(colour = "black"),
        #  panel.border = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        #   panel.background = element_rect(colour = NA, fill = NA),
        panel.grid.major.x = element_line(
          colour = "gray50",
          linewidth = .25,
          linetype = 2
        ),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  }



