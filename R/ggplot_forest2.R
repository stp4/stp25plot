#   Anmerkung: 
#   ggforestplot scheint nicht mehr betreut zu werden weil manche 
#   ggplot Funktionen sind nict mehr aktuell!


#' Draw a Forestplot of Measures of Effects
#'
#' Visualize multiple measures of effect with their confidence intervals in a
#' vertical layout.
#' 
#' stolen from https://github.com/NightingaleHealth/ggforestplot
#'
#' @param df A data frame 
#' @param name,estimate,se,pvalue, the variable 
#' @param legend.title  Beschriftung  main, xlab 
#' 
#' @param groups,colour,shape the variable
#' @param logodds logical (defaults to FALSE) 
#' @param ...   title, xlab, graphical parameters
#' @param col.strip,col Farbe strips und Symbole
#' @param cex size
#' @param cex.pchpch size symbol
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
#' #' 
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
#' 
#' 
#' data$level <- factor(data$level,
#'                      c("M/UM", "HM" , "10 km"),
#'                      c("M/UM", "HM", "10 km (Reference)"))
#' ggplot_forest(
#'   data,
#'   name = group,
#'   groups = level ,
#'   title = "Associations to Running",
#'   xlab = "Estimate (95% CI)",
#'   legend.title = "legend title using guide",
#'   cex = 1.1,
#'   # cex.pch =  1,
#'   pch = c(19, 15, 17)
#' )
#' 
#' 
#' # Alternative
#' 
#'  ggplot(data = dat, aes(x = group  , y = estimate)) +
#' geom_pointrange(
#'   mapping = aes(
#'     ymin = conf.low ,
#'     ymax = conf.high,
#'     color = level,
#'     shape = level
#'   ),
#'   position = ggplot2::position_dodge(width =  0.5)
#' ) +
#'   GGally::geom_stripped_cols(odd = "#00000000", even = "#11111111") +
#'   ggplot2::guides(
#'     colour = guide_legend(
#'       reverse = TRUE,
#'       title = "legend.title",
#'       override.aes =  list(size = 1)
#'     ),
#'     shape = guide_legend(reverse = TRUE, title = "legend.title")
#'   ) +
#'   theme_forest2(base_size = 12) +
#'   coord_flip() 
#' 
#' 
#' #' # Alternative
#' 
#' 
#' ggplot(data = data, aes(x = group  , y = estimate)) +
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
#'   coord_flip()
ggplot_forest <-
  function (df,
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
            pch = c(21L, 22L, 23L, 24L, 25L),
            title=NULL,
            xlab =NULL,
            xlim =NULL,
            xtickbreaks=NULL,
            
            legend.title =  ggplot2::waiver(),
            ...) {
    stopifnot(is.data.frame(df))
    stopifnot(is.logical(logodds))
    
    
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
    
    
    
    #' calc 95%- CI if needed
    if (rlang::quo_is_null(se)) {
      df <- df %>%
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
      df <- df %>%
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
      df <-
        df %>%
        mutate(
          .xmin = exp(.data$.xmin),
          .xmax = exp(.data$.xmax),
          `:=`(!!estimate, exp(!!estimate))
        )
    }
    
    #' plot
    g <- ggplot2::ggplot(df,
                         ggplot2::aes(x = !!estimate, y = !!name))
    if (logodds) {
      g <- if ("xtickbreaks" %in% names(args))
        g + scale_x_continuous(trans = "log10", breaks = args$xtickbreaks)
      else
        g + scale_x_continuous(trans = "log10", breaks = scales::log_breaks(n = 7))
    }
    
    g <- g + theme_forest2(base_size = cex * 13)
    
    if (!is.null(col.strip))
      g <- g + geom_stripes(odd = col.strip[1], even = col.strip[2])
    
    
    g <- g +
      ggplot2::geom_vline(
        xintercept = ifelse(test = logodds, yes = 1, no = 0),
        linetype = "solid",
        size = 0.4,
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
        stroke = cex.pch
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
    
    if ( !is.null(title) ) g <- g + labs(title = title)
    if ( !is.null(xlab) )  g <- g + labs(x = xlab)
    if ( !is.null(xlim) ) g <- g + coord_cartesian(xlim = xlim)
    if ( !is.null(xtickbreaks)  & !logodds) g <- g + scale_x_continuous(breaks = xtickbreaks)
    
    
    g
  }


#' @noRd
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
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' df <-
#'   # Use built-in demo dataset
#'   df_linear_associations %>%
#'     # Arrange by name in order to filter the first few biomarkers for more
#'     # than one studies
#'     dplyr::arrange(name) %>%
#'     # Estimate confidence intervals
#'     dplyr::mutate(
#'       xmin = beta - qnorm(1 - (1 - 0.95) / 2) * se,
#'       xmax = beta + qnorm(1 - (1 - 0.95) / 2) * se
#'     ) %>%
#'     # Select only first 30 rows (10 biomarkers)
#'     dplyr::filter(dplyr::row_number() <= 30) %>%
#'     # Add a logical variable for statistical significance
#'     dplyr::mutate(filled = pvalue < 0.001)
#'
#' g <-
#'   ggplot(data = df, aes(x = beta, y = name)) +
#'   # And point+errorbars
#'   geom_effect(
#'     ggplot2::aes(
#'       xmin = xmin,
#'       xmax = xmax,
#'       colour = trait,
#'       shape = trait,
#'       filled = filled
#'     ),
#'     position = ggstance::position_dodgev(height = 0.5)
#'   )
#' print(g)
#'
#' # Add custom theme, horizontal gray rectangles, vertical line to signify the
#' # NULL point, custom color palettes.
#' g <-
#'   g +
#'   # Add custom theme
#'   theme_forest() +
#'   # Add striped background
#'   geom_stripes(odd = "#33333333", even = "#00000000") +
#'   # Add vertical line at null point
#'   geom_vline(
#'     xintercept = 0,
#'     linetype = "solid",
#'     size = 0.4,
#'     colour = "black"
#'   )
#' print(g)
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



#' @noRd
#' Horizontal Study Effects with Confidence Intervals
#'
#' Builds a custom version of \code{\link[ggstance]{geom_pointrangeh}}.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggstance::geom_pointrangeh
#' @author Ilari Scheinin
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' df <-
#'   # Use built-in demo dataset
#'   df_linear_associations %>%
#'     # Arrange by name in order to filter the first few biomarkers for more
#'     # than one studies
#'     dplyr::arrange(name) %>%
#'     # Estimate confidence intervals
#'     dplyr::mutate(
#'       xmin = beta - qnorm(1 - (1 - 0.95) / 2) * se,
#'       xmax = beta + qnorm(1 - (1 - 0.95) / 2) * se
#'     ) %>%
#'     # Select only first 30 rows (10 biomarkers)
#'     dplyr::filter(dplyr::row_number() <= 30) %>%
#'     # Add a logical variable for statistical significance
#'     dplyr::mutate(filled = pvalue < 0.001)
#'
#' g <-
#'   ggplot(data = df, aes(x = beta, y = name)) +
#'   # And point+errorbars
#'   geom_effect(
#'     ggplot2::aes(
#'       xmin = xmin,
#'       xmax = xmax,
#'       colour = trait,
#'       shape = trait,
#'       filled = filled
#'     ),
#'     position = ggstance::position_dodgev(height = 0.5)
#'   )
#' print(g)
#'
#' # Add custom theme, horizontal gray rectangles, vertical line to signify the
#' # NULL point, custom color palettes.
#' g <-
#'   g +
#'   # Add custom theme
#'   theme_forest() +
#'   # Add striped background
#'   geom_stripes() +
#'   # Add vertical line at null point
#'   geom_vline(
#'     xintercept = 0,
#'     linetype = "solid",
#'     size = 0.4,
#'     colour = "black"
#'   )
#' print(g)
#' 
geom_effect2 <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = ggplot2::position_dodge(width =  0.5),
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
        lwd = data$stroke * .stroke / 2
      )
    )
  },
  required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),
  draw_panel = function(data,
                        panel_params,
                        coord) {
  ggplot2::GeomPointrange$draw_panel(
        dplyr::mutate(data,
                      fatten = 1
          #             ,
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

#' @noRd
theme_forest2 <-
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
          size = .5,
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
          size = .25,
          linetype = 2
        ),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  }










# 
# 
# ggplot_forest <-
#   function (df,
#             name = term,
#             estimate = estimate,
#             conf.low =conf.low,
#             conf.high=conf.high,
#             se = NULL,   #std.error,
#             #  pvalue = NULL,
#             groups =NULL,
#             colour = NULL,
#             shape = NULL,
#             logodds = FALSE,
#             
#             col.strip = c(even = "#00000000", odd = "#33333333"),
#             col =NULL,
#             cex = 1,
#             cex.pch =.6,
#             cex.pch.legende = cex.pch*2,
#             pch = c(21L, 22L, 23L, 24L, 25L),
#             legend.title =  ggplot2::waiver(),
#             ...){
#     
#     
#     stopifnot(is.data.frame(df))
#     stopifnot(is.logical(logodds))  
#     
#     
#     
#     #  psignif <- 0.05  
#     #   ci <- 0.95
#     name <-     rlang::enquo(name)
#     estimate <- rlang::enquo(estimate)
#     se <-       rlang::enquo(se)
#     conf.low <- rlang::enquo(conf.low)
#     conf.high <-rlang::enquo(conf.high)
#     #  pvalue <-   rlang::enquo(pvalue)
#     groups <-   rlang::enquo(groups)
#     if (!rlang::quo_is_null(groups)){
#       colour <- shape <-  groups
#       
#     }
#     else{
#       colour <-   rlang::enquo(colour)
#       shape <-    rlang::enquo(shape)
#     }
#     args <-     list(...)
#     
#     
#     
#     
#     if (rlang::quo_is_null(se)) {
#       df <- df %>%
#         tidyr::drop_na(!!estimate) %>%
#         dplyr::mutate(
#           `:=`(
#             !!name,
#             factor(
#               !!name,
#               levels = !!name %>% unique() %>% rev(),
#               ordered = TRUE
#             )
#           ),
#           .xmin = !!conf.low,
#           .xmax = !!conf.high,
#           # .filled = TRUE,
#           .label = sprintf("%.2f", !!estimate)
#         )
#       
#     }
#     else{
#       const <- stats::qnorm(1 - (1 - ci) / 2) 
#       df <- df %>% 
#         tidyr::drop_na(!!estimate) %>%
#         dplyr::mutate(
#           `:=`(
#             !!name,
#             factor(
#               !!name,
#               levels = !!name %>% unique() %>% rev(),
#               ordered = TRUE
#             )
#           ),
#           .xmin = !!estimate - const * !!se,
#           .xmax = !!estimate + const * !!se,
#           # .filled = TRUE,
#           .label = sprintf("%.2f", !!estimate)
#         )
#     }
#     
#     if (logodds) {
#       df <-
#         df %>% 
#         mutate(
#           .xmin = exp(.data$.xmin),
#           .xmax = exp(.data$.xmax),
#           `:=`(!!estimate, exp(!!estimate))
#         )
#     }
#     
#     #  if (!rlang::quo_is_null(pvalue)) {
#     #    df <- df %>% dplyr::mutate(.filled = !!pvalue < !!psignif)
#     #  }
#     
#     
#     
#     g <- ggplot2::ggplot(df, aes(x = !!estimate, y = !!name))
#     
#     if (logodds) {
#       if ("xtickbreaks" %in% names(args)) {
#         g <- g + scale_x_continuous(trans = "log10",
#                                     breaks = args$xtickbreaks)
#       }
#       else {
#         g <- g + scale_x_continuous(trans = "log10",
#                                     breaks = scales::log_breaks(n = 7))
#       }
#     }
#     
#     g <- g + 
#       ggforestplot::theme_forest(base_size = cex * 13) 
#     # + ggforestplot::scale_colour_ng_d(palette = "dark") +
#     #   ggforestplot::scale_fill_ng_d(palette = "dark")
#     
#     if (!is.null(col.strip))
#       g <- g +
#       ggforestplot::geom_stripes(odd = col.strip[1], even = col.strip[2])
#     
#     
#     g <- g +
#       ggplot2::geom_vline(
#         xintercept = ifelse(test = logodds, yes = 1, no = 0),
#         linetype = "solid",
#         size = 0.4, colour = "black"
#       )
#     
#     g <- g +
#       ggforestplot::geom_effect(
#         ggplot2::aes(
#           xmin = .data$.xmin,
#           xmax = .data$.xmax,
#           colour = !!colour,
#           shape = !!shape #,  filled = .data$.filled
#         ),
#         size = cex.pch, 
#         stroke =cex.pch, 
#         position = ggstance::position_dodgev(height = 0.5)
#         # linetype = 4, shape = 21, fill = "black", alpha = NA, 
#         # filled = TRUE, fatten = 6, colour = "black", 
#       ) +
#       ggplot2::scale_shape_manual(values = pch) +
#       ggplot2::guides(colour = guide_legend(reverse = TRUE, 
#                                             title=legend.title,
#                                             override.aes = 
#                                               list(size = cex.pch.legende)),
#                       shape = guide_legend(reverse = TRUE, title=legend.title)
#       )
#     
#     if(!is.null(col)) g <- g + scale_colour_manual(values = rev(col))
#     
#     g <- g + labs(y = "")
#     
#     if ("title" %in% names(args)) g <- g + labs(title = args$title)
#     if ("xlab" %in% names(args)) g <- g + labs(x = args$xlab)
#     if ("xlim" %in% names(args)) g <- g + coord_cartesian(xlim = args$xlim)
#     if ("xtickbreaks" %in% names(args) & !logodds) 
#       g <- g + scale_x_continuous(breaks = args$xtickbreaks)
#     
#     
#     # if ("subtitle" %in% names(args)) g <- g + labs(subtitle = args$subtitle)
#     #if ("caption" %in% names(args)) g <- g + labs(caption = args$caption)
#     # if (!"ylab" %in% names(args)) args$ylab <- ""
#     #  if ("ylim" %in% names(args)) g <- g + ylim(args$ylim)
#     
#     g
#   }





# load(
#   "C:/Users/wpete/Dropbox/1_Projekte/002-NURMI/791_Martina_Gregori/dummy-forest-df.Rdata"
# )
# 
# data$std.error[is.na(data$estimate)] <- 0
# data$conf.low[is.na(data$estimate)] <- 0
# data$conf.high[is.na(data$estimate)] <- 0
# data$estimate[is.na(data$estimate)] <- 0
# data <- data[-which(data$var == "age"), ]
# data <- data[-which(data$var == "(Intercept)"), ]
# 
# 
# 
# data$level <- factor(data$level,
#                      c("M/UM", "HM" , "10 km"),
#                      c("M/UM", "HM", "10 km (Reference)"))
# ggplot_forest(
#   data,
#   name = group,
# #  groups = NULL,
# #  colour = level,
# #  shape = NULL,
#   groups = level ,
#   title = "Associations to Running",
#   xlab = "Estimate (95% CI)",
#   legend.title = "legend title using guide",
#   cex = 1.1,
#   # cex.pch =  1,
#   pch = c(19, 15, 17)
# )
