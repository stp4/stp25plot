#' ggplot_table 
#'
#'
#' In forest_plot( verwendet)
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
#' @return ggplot
#' @export
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
  annot_size_mm <- fontsize * 4.5
  #as.numeric(grid::convertX(unit(theme_get()$text$size, "pt"), "mm"))
  
  if(use_label){
    
    gparam$var<- useLabels(gparam$var, model$labels)
    
    #print( gparam$var )
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
  
 # cat("\n ggplot_table -> transform\n")
  
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
  
#  cat("\n ggplot_table -> nach ggplot \n")
  
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  ggpubr::as_ggplot(gt)
}