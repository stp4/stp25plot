# theme_ggeffects2 <- function (base_size = 11,
#                               base_family = "")
# {
#   insight::check_if_installed("ggplot2")
#   (
#     ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
#       ggplot2::theme(
#         axis.line.x = ggplot2::element_line(colour = "grey80"),
#         axis.line.y = ggplot2::element_line(colour = "grey80"),
#         axis.text = ggplot2::element_text(colour = "grey50"),
#         axis.title = ggplot2::element_text(colour = "grey30"),
#         strip.background = ggplot2::element_rect(colour = "grey70",
#                                                  fill = "grey90"),
#         strip.text = ggplot2::element_text(colour = "grey30"),
#         legend.title = ggplot2::element_text(colour = "grey30"),
#         legend.text = ggplot2::element_text(colour = "grey30")
#       )
#   )
# }
# 
# 
# ggeffects_plot.ggeffects <-
#   function (x,
#             ci = TRUE,
#             ci.style = c("ribbon", "errorbar", "dash",
#                          "dot"),
#             facets,
#             add.data = FALSE,
#             limit.range = FALSE,
#             residuals = FALSE,
#             residuals.line = FALSE,
#             collapse.group = FALSE,
#             colors = "Set1",
#             alpha = 0.15,
#             dodge = 0.25,
#             use.theme = TRUE,
#             dot.alpha = 0.35,
#             jitter = 0.2,
#             log.y = FALSE,
#             case = NULL,
#             show.legend = TRUE,
#             show.title = TRUE,
#             show.x.title = TRUE,
#             show.y.title = TRUE,
#             dot.size = NULL,
#             line.size = NULL,
#             connect.lines = FALSE,
#             grid,
#             one.plot = TRUE,
#             rawdata,
#             ...)
#   {
#     insight::check_if_installed("ggplot2", reason = "to produce marginal effects plots")
#     if (missing(rawdata))
#       rawdata <- add.data
#     jitter.miss <- missing(jitter)
#     if (isTRUE(jitter))
#       jitter <- 0.2
#     else if (is.logical(jitter) &&
#              length(jitter) == 1L && !is.na(jitter) &&
#              !jitter)
#       jitter <- NULL
#     if (!is.null(jitter) &&
#         length(jitter) == 1 && is.numeric(jitter)) {
#       jitter <- c(jitter, jitter)
#     }
#     y.breaks <- NULL
#     y.limits <- NULL
#     xif <- attr(x, "x.is.factor", exact = TRUE)
#     x_is_factor <- !is.null(xif) && xif == "1"
#     if (is.null(dot.size))
#       dot.size <- 2
#     if (is.null(line.size))
#       line.size <- 0.7
#     if (!missing(grid))
#       facets <- grid
#     if (missing(ci.style) && x_is_factor)
#       ci.style <- "errorbar"
#     ci.style <- match.arg(ci.style)
#     add.args <-
#       lapply(match.call(expand.dots = FALSE)$..., function(x)
#         x)
#     if (!("breaks" %in% names(add.args)) && isTRUE(log.y)) {
#       y.breaks <- unique(round(log2(pretty(c(
#         min(x$conf.low),
#         max(x$conf.high)
#       )))))
#       y.breaks[is.nan(y.breaks)] <- NA
#       y.breaks[is.infinite(y.breaks)] <- NA
#       y.breaks <- 2 ^ y.breaks[!is.na(y.breaks)]
#       y.limits <- c(min(y.breaks), max(y.breaks))
#       while (y.limits[1] > min(x$conf.low) && !(y.limits[1] <=
#                                                 1e-05)) {
#         y.limits[1] <- y.limits[1] / 2
#       }
#       while (y.limits[2] < max(x$conf.high)) {
#         y.limits[2] <- y.limits[2] * 2
#       }
#     }
#     has_groups <-
#       .obj_has_name(x, "group") && length(unique(x$group)) >
#       1
#     has_facets <-
#       .obj_has_name(x, "facet") && length(unique(x$facet)) >
#       1
#     has_panel <-
#       .obj_has_name(x, "panel") && length(unique(x$panel)) >
#       1
#     if (isTRUE(limit.range)) {
#       raw_data <- attr(x, "rawdata", exact = TRUE)
#       if (!is.null(raw_data)) {
#         if (has_groups && has_facets) {
#           ranges <- lapply(split(raw_data, list(raw_data$group,
#                                                 raw_data$facet)), function(i)
#                                                   range(i$x, na.rm = TRUE))
#           for (i in unique(raw_data$group)) {
#             for (j in unique(raw_data$facet)) {
#               if (any(is.infinite(ranges[[paste0(i, ".",
#                                                  j)]]))) {
#                 remove <- x$group == i & x$facet == j
#                 x$x[remove] <- NA
#               }
#               else {
#                 remove <- x$group == i & x$facet == j &
#                   x$x < ranges[[paste0(i, ".", j)]][1]
#                 x$x[remove] <- NA
#                 remove <- x$group == i & x$facet == j &
#                   x$x > ranges[[paste0(i, ".", j)]][2]
#                 x$x[remove] <- NA
#               }
#             }
#           }
#         }
#         else if (has_groups) {
#           ranges <- lapply(split(raw_data, raw_data$group),
#                            function(i)
#                              range(i$x, na.rm = TRUE))
#           for (i in names(ranges)) {
#             remove <- x$group == i & x$x < ranges[[i]][1]
#             x$x[remove] <- NA
#             remove <- x$group == i & x$x > ranges[[i]][2]
#             x$x[remove] <- NA
#           }
#         }
#         else {
#           remove <- x$x < min(raw_data$x, na.rm = TRUE) |
#             x$x > max(raw_data$x, na.rm = TRUE)
#           x$x[remove] <- NA
#         }
#       }
#     }
#     if (residuals) {
#       model <- .get_model_object(x)
#       if (!is.null(model)) {
#         residual_data <- residualize_over_grid(grid = x,
#                                                model = model)
#         attr(x, "residual_data") <- residual_data
#         attr(x, "continuous.group") <- FALSE
#       }
#       else {
#         warning("Could not find model object to extract residuals.",
#                 call. = FALSE)
#         residuals <- FALSE
#       }
#     }
#     if (isTRUE(collapse.group) || (!is.null(collapse.group) &&
#                                    !isFALSE(collapse.group))) {
#       if (isTRUE(collapse.group)) {
#         collapse.group <- NULL
#       }
#       re_data <- collapse_by_group(
#         x,
#         model = .get_model_object(x),
#         collapse.by = collapse.group,
#         residuals = residuals
#       )
#       attr(x, "random_effects_data") <- re_data
#       attr(x, "continuous.group") <- FALSE
#       rawdata <- add.data <- residuals <- FALSE
#       attr(x, "residual_data") <- NULL
#     }
#     if (!is.numeric(x$x)) {
#       if (x_is_factor && !.is_numeric_factor(x$x)) {
#         levels(x$x) <- seq_len(nlevels(x$x))
#       }
#       x$x <- .factor_to_numeric(x$x)
#     }
#     facet_polr <- FALSE
#     if (.obj_has_name(x, "response.level") &&
#         length(unique(x$response.level)) >
#         1) {
#       has_facets <- TRUE
#       facet_polr <- TRUE
#     }
#     is_black_white <- colors[1] == "bw"
#     if (has_facets)
#       facets <- TRUE
#     else if (missing(facets) || is.null(facets))
#       facets <- has_facets
#     facets_grp <- facets && !has_facets
#     if ("conf.low" %in% names(which(colSums(is.na(x)) == nrow(x))) ||
#         !.obj_has_name(x, "conf.low"))
#       ci <- FALSE
#     if (facets) {
#       if (is.numeric(x$facet) || isTRUE(attr(x, "numeric.facet",
#                                              exact = TRUE))) {
#         x$facet <- sprintf("%s = %g",
#                            attr(x, "terms", exact = TRUE)[3],
#                            .factor_to_numeric(x$facet))
#       }
#     }
#     if (!has_panel)
#       one.plot <- FALSE
#     if (one.plot && !requireNamespace("see", quietly = TRUE)) {
#       warning(
#         "Package `see` needed to plot multiple panels in one integrated figure. Please install it by typing `install.packages(\"see\", dependencies = TRUE)` into the console.",
#         call. = FALSE
#       )
#       one.plot <- FALSE
#     }
#     if (has_panel) {
#       panels <- unique(x$panel)
#       p <- lapply(seq_along(panels), function(.i) {
#         .p <- panels[.i]
#         attr(x, "panel.title") <- sprintf("%s = %s", attr(x,
#                                                           "terms", exact = TRUE)[4], as.character(.p))
#         if (one.plot && .i < length(panels)) {
#           show_l <- FALSE
#         }
#         else {
#           show_l <- show.legend
#         }
#         pl <- plot_panel(
#           x = x[x$panel == .p, , drop = FALSE],
#           colors = colors,
#           has_groups = has_groups,
#           facets_grp = facets_grp,
#           facets = facets,
#           facet_polr = facet_polr,
#           is_black_white = is_black_white,
#           x_is_factor = x_is_factor,
#           alpha = alpha,
#           dot.alpha = dot.alpha,
#           dodge = dodge,
#           ci = ci,
#           ci.style = ci.style,
#           dot.size = dot.size,
#           line.size = line.size,
#           connect.lines = connect.lines,
#           case = case,
#           jitter = jitter,
#           jitter.miss = jitter.miss,
#           rawdata = rawdata,
#           residuals = residuals,
#           residuals.line = residuals.line,
#           show.title = show.title,
#           show.x.title = show.x.title,
#           show.y.title = show.y.title,
#           show.legend = show_l,
#           log.y = log.y,
#           y.breaks = y.breaks,
#           y.limits = y.limits,
#           use.theme = use.theme,
#           ...
#         )
#         if (one.plot) {
#           if (.i < length(panels)) {
#             pl <- pl + ggplot2::labs(x = NULL)
#           }
#           if (.i > 1) {
#             pl <- pl + ggplot2::labs(title = NULL)
#           }
#         }
#         pl
#       })
#     }
#     else {
#       p <- plot_panel(
#         x = x,
#         colors = colors,
#         has_groups = has_groups,
#         facets_grp = facets_grp,
#         facets = facets,
#         facet_polr = facet_polr,
#         is_black_white = is_black_white,
#         x_is_factor = x_is_factor,
#         alpha = alpha,
#         dot.alpha = dot.alpha,
#         dodge = dodge,
#         ci = ci,
#         ci.style = ci.style,
#         dot.size = dot.size,
#         line.size = line.size,
#         connect.lines = connect.lines,
#         case = case,
#         jitter = jitter,
#         jitter.miss = jitter.miss,
#         rawdata = rawdata,
#         residuals = residuals,
#         residuals.line = residuals.line,
#         show.title = show.title,
#         show.x.title = show.x.title,
#         show.y.title = show.y.title,
#         show.legend = show.legend,
#         log.y = log.y,
#         y.breaks = y.breaks,
#         y.limits = y.limits,
#         use.theme = use.theme,
#         ...
#       )
#     }
#     if (has_panel &&
#         one.plot && requireNamespace("see", quietly = TRUE)) {
#       do.call(see::plots, p)
#     }
#     else {
#       p
#     }
#   }
# 
# 
# 
# 
# 
# 
# ggeffects:::plot_panel <-
#   function (x,
#             colors,
#             has_groups,
#             facets_grp,
#             facets,
#             facet_polr,
#             is_black_white,
#             x_is_factor,
#             alpha,
#             dot.alpha,
#             dodge,
#             ci,
#             ci.style,
#             dot.size,
#             line.size,
#             connect.lines,
#             case,
#             jitter,
#             jitter.miss,
#             rawdata,
#             residuals,
#             residuals.line,
#             show.title,
#             show.x.title,
#             show.y.title,
#             show.legend,
#             log.y,
#             y.breaks,
#             y.limits,
#             use.theme,
#             ...)
#   {
#     if (.obj_has_name(x, "group") && is.character(x$group))
#       x$group <- factor(x$group, levels = unique(x$group))
#     if (.obj_has_name(x, "facet") && is.character(x$facet))
#       x$facet <- factor(x$facet, levels = unique(x$facet))
#     if (.obj_has_name(x, "response.level") &&
#         is.character(x$response.level))
#       x$response.level <-
#         ordered(x$response.level, levels = unique(x$response.level))
#     if (rawdata && isTRUE(attr(x, "continuous.group"))) {
#       x$group_col <- as.numeric(as.character(x$group))
#     }
#     else {
#       x$group_col <- x$group
#     }
#     plot_data <- x[!is.na(x$x),]
#     if (has_groups && !facets_grp && is_black_white && x_is_factor) {
#       p <- ggplot2::ggplot(
#         plot_data,
#         ggplot2::aes_string(
#           x = "x",
#           y = "predicted",
#           colour = "group_col",
#           fill = "group_col",
#           shape = "group"
#         )
#       )
#     }
#     else if (has_groups &&
#              !facets_grp && is_black_white && !x_is_factor) {
#       p <- ggplot2::ggplot(
#         plot_data,
#         ggplot2::aes_string(
#           x = "x",
#           y = "predicted",
#           colour = "group_col",
#           fill = "group_col",
#           linetype = "group"
#         )
#       )
#     }
#     else if (has_groups && !facets_grp && colors[1] == "gs" &&
#              x_is_factor) {
#       p <- ggplot2::ggplot(
#         plot_data,
#         ggplot2::aes_string(
#           x = "x",
#           y = "predicted",
#           colour = "group_col",
#           fill = "group_col",
#           shape = "group"
#         )
#       )
#     }
#     else if (has_groups && colors[1] != "bw") {
#       p <- ggplot2::ggplot(
#         plot_data,
#         ggplot2::aes_string(
#           x = "x",
#           y = "predicted",
#           colour = "group_col",
#           fill = "group_col"
#         )
#       )
#     }
#     else {
#       p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "x",
#                                                           y = "predicted"))
#     }
#     colors <-
#       .get_colors(colors, length(unique(stats::na.omit(x$group))),
#                   isTRUE(attr(x, "continuous.group")))
#     rawdat <- attr(x, "rawdata", exact = TRUE)
#     if (rawdata) {
#       p <- .add_raw_data_to_plot(p,
#                                  x,
#                                  rawdat,
#                                  ci.style,
#                                  dot.alpha,
#                                  dot.size,
#                                  dodge,
#                                  jitter,
#                                  jitter.miss,
#                                  colors)
#     }
#     residual_data <- attr(x, "residual_data", exact = TRUE)
#     if (isTRUE(residuals)) {
#       p <- .add_residuals_to_plot(
#         p,
#         x,
#         residual_data,
#         residuals.line,
#         ci.style,
#         line.size,
#         dot.alpha,
#         dot.size,
#         dodge,
#         jitter,
#         colors
#       )
#     }
#     random_effects_data <-
#       attr(x, "random_effects_data", exact = TRUE)
#     if (!is.null(random_effects_data)) {
#       p <- .add_re_data_to_plot(p,
#                                 x,
#                                 random_effects_data,
#                                 dot.alpha,
#                                 dot.size,
#                                 dodge,
#                                 jitter)
#     }
#     if (x_is_factor) {
#       p <-
#         p + ggplot2::geom_point(position = ggplot2::position_dodge(width = dodge),
#                                 size = dot.size)
#     }
#     else {
#       p <-
#         p + ggplot2::geom_line(linewidth = line.size, ggplot2::aes_string(group = "group"))
#     }
#     if (x_is_factor && connect.lines) {
#       p <-
#         p + ggplot2::geom_line(size = line.size,
#                                position = ggplot2::position_dodge(width = dodge))
#     }
#     if (ci) {
#       if (x_is_factor) {
#         if (ci.style == "errorbar") {
#           p <-
#             p + ggplot2::geom_errorbar(
#               ggplot2::aes_string(ymin = "conf.low",
#                                   ymax = "conf.high"),
#               position = ggplot2::position_dodge(width = dodge),
#               width = 0,
#               size = line.size
#             )
#         }
#         else {
#           lt <- switch(ci.style, dash = 2, dot = 3, 2)
#           p <-
#             p + ggplot2::geom_errorbar(
#               ggplot2::aes_string(
#                 ymin = "conf.low",
#                 ymax = "conf.high",
#                 linetype = NULL
#               ),
#               position = ggplot2::position_dodge(width = dodge),
#               width = 0,
#               linetype = lt,
#               size = line.size
#             )
#         }
#       }
#       else {
#         if (ci.style == "ribbon") {
#           p <-
#             p + ggplot2::geom_ribbon(
#               ggplot2::aes_string(
#                 ymin = "conf.low",
#                 ymax = "conf.high",
#                 colour = NULL,
#                 linetype = NULL,
#                 shape = NULL,
#                 group = "group"
#               ),
#               alpha = alpha
#             )
#         }
#         else if (ci.style == "errorbar") {
#           p <-
#             p + ggplot2::geom_point(position = ggplot2::position_dodge(width = dodge),
#                                     size = dot.size) + ggplot2::geom_errorbar(
#                                       ggplot2::aes_string(
#                                         ymin = "conf.low",
#                                         ymax = "conf.high",
#                                         shape = NULL
#                                       ),
#                                       position = ggplot2::position_dodge(width = dodge),
#                                       size = line.size,
#                                       width = 0
#                                     )
#         }
#         else {
#           lt <- switch(ci.style, dash = 2, dot = 3, 2)
#           p <-
#             p + ggplot2::geom_line(ggplot2::aes_string(y = "conf.low",
#                                                        linetype = NULL),
#                                    linetype = lt) + ggplot2::geom_line(ggplot2::aes_string(y = "conf.high",
#                                                                                            linetype = NULL),
#                                                                        linetype = lt)
#         }
#       }
#     }
#     x_lab <- get_x_labels(x, case)
#     if (!is.null(x_lab)) {
#       p <- p + ggplot2::scale_x_continuous(breaks = unique(plot_data$x),
#                                            labels = x_lab)
#     }
#     if (facets_grp) {
#       p <- p + ggplot2::facet_wrap( ~ group, scales = "free_x")
#       p <- p + ggplot2::guides(colour = "none",
#                                linetype = "none",
#                                shape = "none")
#     }
#     else if (facet_polr) {
#       p <- p + ggplot2::facet_wrap( ~ response.level, scales = "free_x")
#     }
#     else if (facets) {
#       p <- p + ggplot2::facet_wrap( ~ facet, scales = "free_x")
#     }
#     if (isTRUE(rawdata) && isTRUE(attr(x, "continuous.group"))) {
#       p <- p + ggplot2::scale_color_gradientn(
#         colors = colors,
#         aesthetics = c("colour", "fill"),
#         guide = "legend",
#         breaks = as.numeric(levels(x$group)),
#         limits = range(c(rawdat$group_col,
#                          x$group_col))
#       )
#     }
#     else {
#       p <- p + ggplot2::scale_color_manual(values = colors,
#                                            aesthetics = c("colour", "fill"))
#     }
#     if (!show.title)
#       attr(x, "title") <- NULL
#     if (!show.title)
#       attr(x, "n.trials") <- NULL
#     if (!show.x.title)
#       attr(x, "x.title") <- NULL
#     if (!show.y.title)
#       attr(x, "y.title") <- NULL
#     p <-
#       p + ggplot2::labs(
#         title = get_title(x, case),
#         x = get_x_title(x,
#                         case),
#         y = get_y_title(x, case),
#         fill = NULL,
#         subtitle = get_sub_title(x)
#       )
#     if (has_groups && show.legend)
#       p <- p + ggplot2::labs(
#         colour = get_legend_title(x, case),
#         linetype = get_legend_title(x, case),
#         shape = get_legend_title(x,
#                                  case)
#       )
#     p <- p + ggplot2::guides(fill = "none")
#     if (is_black_white) {
#       p <-
#         p + ggplot2::guides(colour = "none") + ggplot2::labs(colour = NULL)
#     }
#     if (!show.legend) {
#       p <- p + ggplot2::labs(colour = NULL,
#                              linetype = NULL,
#                              shape = NULL) + ggplot2::guides(colour = "none",
#                                                              linetype = "none",
#                                                              shape = "none")
#     }
#     if (attr(x, "logistic", exact = TRUE) == "1" &&
#         attr(x, "is.trial",
#              exact = TRUE) == "0") {
#       if (log.y) {
#         if (is.null(y.breaks))
#           p <- p + ggplot2::scale_y_log10(labels = .percents,
#                                           ...)
#         else
#           p <- p + ggplot2::scale_y_log10(labels = .percents,
#                                           breaks = y.breaks,
#                                           limits = y.limits,
#                                           ...)
#       }
#       else {
#         p <- p + ggplot2::scale_y_continuous(labels = .percents,
#                                              ...)
#       }
#     }
#     else if (log.y) {
#       if (is.null(y.breaks))
#         p <- p + ggplot2::scale_y_log10(...)
#       else
#         p <- p + ggplot2::scale_y_log10(breaks = y.breaks,
#                                         limits = y.limits, ...)
#     }
#     else {
#       p <- p + ggplot2::scale_y_continuous(...)
#     }
#     if (use.theme)
#       p <- p + theme_ggeffects()
#     p
#   }
