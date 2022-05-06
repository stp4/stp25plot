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
#' @param cpositions  Position left side
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
ggplot_table  <- function(gparam,
                          main = "", 
                          gof = NULL,
                          scale.log = FALSE,
                          main.hjust = 0.5,
                          left.hjust = -0.2,
                          right.hjust = 0,
                          
                          space.right = 0.15,
                          space.left = 0,
                          refLabel = "reference",
                          rangeb = range(gparam$conf.low,
                                         gparam$conf.high, na.rm = TRUE),
                          digits = 2,
                          cpositions = c(0.02, 0.22, 0.4),
                          fontsize = 0.7,
                          col.text = 1,
                          col.plot = "gray20",
                          col.fill = c("#FFFFFF33", "#00000033")) {
  
 
  if(!all(
    c(
      "var",
      "level",
      "N",
      "estimate",
      "conf.low",
      "conf.high",
      "p.value"
    ) %in% names(gparam)
  )) stop("data.frame ist nicht vollstaendig!")
   
  require(ggplot2)
  
  
  breaks <- grDevices::axisTicks(rangeb / 2, log = TRUE, nint = 7)
  rangeplot <- rangeb
  rangeplot[1] <-
    rangeplot[1] - diff(rangeb) + space.left * diff(rangeb)
  rangeplot[2] <- rangeplot[2] + space.right * diff(rangeb)
  
  width <- diff(rangeplot)
  
  pos_item <-
    rangeplot[1] + cpositions[1] * width   # position links
  pos_levels <-   rangeplot[1] + cpositions[2] * width
  pos_estimate <- rangeplot[1] + cpositions[3] * width
  pos_pvalue <-   rangeb[2]   # position rechts
  
  
  pos_zeile <- seq_len(nrow(gparam))
  annot_size_mm <- fontsize * 4.5
  
  if (is.null(col.fill)) 
    col.fill <- c("#FFFFFF33", "#FFFFFF33")
  
  #as.numeric(grid::convertX(unit(theme_get()$text$size, "pt"), "mm"))
  
  
  if (scale.log) {
 
    gparam <- transform(
      gparam,
      stars = ifelse(p.value < 0.001, "p<.001", stp25rndr::rndr_P(p.value)),
      est = ifelse(is.na(estimate), refLabel, 
                   stp25stat2::render_f(exp(estimate), digits = digits)),
      ci = ifelse(
        is.na(estimate),
        "",
        stp25rndr::rndr_CI(exp(gparam[c("conf.low", "conf.high")]),
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
      stars = ifelse(p.value < 0.001, "p<.001", stp25rndr::rndr_P(p.value)),
      est = ifelse(is.na(estimate), refLabel, 
                   stp25stat2::render_f(estimate, digits = digits)),
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
    gparam <- gparam[nrow(gparam):1, ]
  }
  
  # cat("\n ggplot_table -> transform\n")
  
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
  
  
  p <-
    ggplot(gparam,
           aes(seq_along(var), estimate)) +
    geom_rect(
      aes(
        xmin = seq_along(var) - 0.5,
        xmax = seq_along(var) + 0.5,
        ymin = rangeplot[1],
        ymax = rangeplot[2]
        , fill = ordered(seq_along(var) %% 2 + 1)
      )
    ) +
    scale_fill_manual(values = col.fill, guide = "none") +
    geom_point(pch = 15,
               size = 4,
               colour = col.plot) +
    geom_errorbar(aes(ymin = conf.low,
                      ymax = conf.high),
                  width = 0.15,
                  colour = col.plot) +
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
      axis.text.y =  element_blank(),
      axis.ticks.y = element_blank(),
      plot.title =   element_text(hjust = main.hjust)
    ) +
    # xlab("wo bin ich") +
    annotate(
      geom = "text",
      x = pos_zeile,
      y = pos_item,
      label = gparam$var,
      colour = col.text,
      fontface = "bold",
      hjust = right.hjust,
      size = annot_size_mm
    ) +
    annotate(
      geom = "text",
      x = pos_zeile,
      y = pos_levels,
      
      label = gparam$level,
      colour = col.text,
      hjust = 0,
      vjust = -0.1,
      size = annot_size_mm
    ) +
    annotate(
      geom = "text",
      x = pos_zeile,
      y = pos_levels,
      label = gparam$N,
      colour = col.text,
      fontface = "italic",
      hjust = 0,
      vjust = ifelse(gparam$level == "", 0.5, 1.1),
      size = annot_size_mm
    ) +
    annotate(
      geom = "text",
      x = pos_zeile,
      y = pos_estimate,
      label = gparam$est,
      colour = col.text,
      size = annot_size_mm,
      vjust = ifelse(gparam$est == "reference", 0.5,-0.1)
    ) +
    annotate(
      geom = "text",
      x = pos_zeile,
      y = pos_estimate,
      label = gparam$ci,
      colour = col.text,
      size = annot_size_mm,
      vjust = 1.1,
      fontface = "italic"
    ) +
    annotate(
      geom = "text",
      x = pos_zeile,
      y = pos_pvalue,
      label = gparam$stars,
      colour = col.text,
      size = annot_size_mm,
      hjust =  left.hjust,
      # -0.2
      fontface = "italic"
    )
  
  if (!is.null(gof))
    p <-  p +
    annotate(
      geom = "text",
      x = 0.5,
      y = pos_item,
      label = gof,
      size = annot_size_mm,
      colour = col.text,
      hjust = right.hjust,
      vjust = 1.2,
      fontface = "italic"
    )
  
  
  
  #  cat("\n ggplot_table -> nach ggplot \n")
  
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  ggpubr::as_ggplot(gt)
}


 


