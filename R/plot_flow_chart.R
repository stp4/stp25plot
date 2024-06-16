
#' simple consort plot
#' 
#' 
#' @description   consort_plot
#'
#' @param x data.frame
#' @param ... an  stp25plot:::visr
#'
#' @return  plot
#' @export
#'
simple_consort_plot <- function(x, ...) {
 visr(attr(x, "filter"),  ...)
}



#' @title Plot a supported S3 object
#'
#' @description S3 method for creating plots directly from objects using `ggplot2`,
#'   similar to the base R `plot()` function.
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @param x Object of class `survfit`, `attritiontable`, or `tidycuminc`
#' @param x_label \code{character} Label for the x-axis. When not specified,
#'   the function will look for `"PARAM"` or `"PARAMCD"` information in the original data set (CDISC standards).
#'   If no `"PARAM"`/`"PARAMCD"` information is available, the default x-axis label is `"Time"`.
#' @param y_label \code{character} Label for the y-axis. When not specified,
#'   the default will do a proposal, depending on the `fun` argument.
#' @param x_units Unit to be added to the x_label (x_label (x_unit)).
#'   Default is NULL.
#' @param x_ticks Ticks for the x-axis. When not specified, the default will
#'   do a proposal.
#' @param y_ticks Ticks for the y-axis. When not specified,
#'   the default will do a proposal based on the `fun` argument.
#' @param fun Function that represents the scale of the estimate.
#'   The current options are:
#'   \itemize{
#'   \item{`surv`}{ is the survival probability. This is the default.}
#'   \item{`log`}{ is log of the survival probability}
#'   \item{`event`}{ is the failure probability}
#'   \item{`cloglog`}{ is log(-log(survival probability))}
#'   \item{`pct`}{ is survival as a percentage}
#'   \item{`logpct`}{ is log survival as a percentage}
#'   \item{`cumhaz`}{ is the cumulative hazard}
#'   }
#' @param legend_position Specifies the legend position in the plot.
#'   Character values allowed are "top" "left" "bottom" "right".
#'   Numeric coordinates are also allowed.
#'   Default is "right".
#' @param description_column_name \code{character} Name of the column containing
#'   the inclusion descriptions
#' @param value_column_name \code{character} Name of the column containing the
#'   remaining sample counts
#' @param complement_column_name \code{character} Optional: Name of the column
#'   containing the exclusion descriptions
#' @param box_width \code{character} The box width for each box in the flow
#'   chart
#' @param font_size \code{character} The fontsize in pt
#' @param fill The color (string or hexcode) to use to fill the boxes in the
#'   flowchart
#' @param border The color (string or hexcode) to use for the borders of the
#'   boxes in the flowchart
#' @param ... other arguments passed on to the method
#'
#' @return Object of class \code{ggplot} and \code{ggsurvplot} for `survfit` objects.
#'
#' @rdname visr
#'


visr <- function(x, ...) {
  UseMethod("visr", x)
}

#' @rdname visr
#' @method visr default


visr.default <- function(x, ...) {
  graphics::plot(x)
}



#' @description
#' `r lifecycle::badge("questioning")`
#' S3 function to draw a Consort flow diagram chart.
#'
#' @examples
#'  data("adtte", package = "visR")
#' ## Create attrition
#' attrition <- visR::get_attrition(adtte,
#'   criteria_descriptions = c(
#'     "1. Not in Placebo Group",
#'     "2. Be 75 years of age or older.",
#'     "3. White",
#'     "4. Female"
#'   ),
#'   criteria_conditions = c(
#'     "TRTP != 'Placebo'",
#'     "AGE >= 75",
#'     "RACE=='WHITE'",
#'     "SEX=='F'"
#'   ),
#'   subject_column_name = "USUBJID"
#' )
#'
#' ## Draw a CONSORT attrition chart without specifying extra text for the complement
#' attrition %>%
#'   visr("Criteria", "Remaining N")
#'
#' ## Add detailed complement descriptions to the "exclusion" part of the CONSORT diagram
#' # Step 1. Add new column to attrition dataframe
#' attrition$Complement <- c(
#'   "NA",
#'   "Placebo Group",
#'   "Younger than 75 years",
#'   "Non-White",
#'   "Male"
#' )
#'
#' # Step 2. Define the name of the column in the call to the plotting function
#' attrition %>%
#'   visr("Criteria", "Remaining N", "Complement")
#'
#' ## Styling the CONSORT flowchart
#' # Change the fill and outline of the boxes in the flowchart
#' attrition %>%
#'   visr("Criteria", "Remaining N", "Complement", fill = "lightblue", border = "grey")
#'
#' ## Adjust the font size in the boxes
#' attrition %>%
#'   visr("Criteria", "Remaining N", font_size = 10)
#'
#' @rdname visr
#' @method visr attrition
#' @export

visr.attrition <- function(x,
                           description_column_name = "Criteria",
                           value_column_name = "Remaining.N",
                           complement_column_name = "",
                           box_width = 50,
                           font_size = 12,
                           fill = "white",
                           border = "black",
                           ...) {
  if (!description_column_name %in% names(x)) {
    stop(paste0(
      "Column \"", description_column_name, "\" cannot be found in the input data. ",
      "Please provide the column name as string in the input ",
      "data containing the inclusion descriptions."
    ))
  }
  
  if (!value_column_name %in% names(x)) {
    stop(paste0(
      "Column \"", value_column_name, "\" cannot be found in the input data. ",
      "Please provide the column name as string in the input data containing",
      "the sample size after applying inclusion criteria."
    ))
  }
  
  if (complement_column_name != "" & !complement_column_name %in% names(x)) {
    stop(paste0(
      "Column \"", complement_column_name, "\" cannot be found in the input data. ",
      "Please provide a valid column name as string in the input data containing",
      "complement description or omit this argument for default labels."
    ))
  }
  
  if (!is.numeric(box_width)) {
    warning("An invalid input was given for `box_width`, must be `numeric` value. Setting it to 50.")
    box_width <- 50
  }
  
  if (!is.numeric(font_size)) {
    warning("An invalid input was given for `font_size`, must be `numeric` value. Setting it to 12.")
    font_size <- 12
  }
  
  if (!is.character(fill)) {
    warning("An invalid input was given for `fill`, must be `character` string. Setting it to \"white\".")
    fill <- "white"
  }
  
  if (!is.character(border)) {
    warning("An invalid input was given for `border`, must be `character` string. Setting it to \"black\".")
    border <- "black"
  }
  
  label <- complement_label <- NULL
  y <- down_ystart <- down_yend <- side_xstart <- side_xend <- side_y <- NULL
  cx <- cy <- NULL
  
  # split up space into evenly sized chunks
  field_height <- 100 / nrow(x)
  
  # allow for some spacing between boxes by reducing the size of the chunk
  box_height <- 0.75 * field_height
  
  # assign coordinates to each row in the attrition table
  plotting_data <- x %>%
    .get_labels(description_column_name, value_column_name, complement_column_name, wrap_width = box_width) %>%
    .get_labelsizes(label, complement_label) %>%
    .get_coordinates(box_width, box_height, field_height)
  
  # draw plot
  gg <- plotting_data %>%
    ggplot2::ggplot() +
    # boxes
    ggplot2::geom_tile(
      data = plotting_data, ggplot2::aes(
        x = x,
        y = y,
        width = box_width,
        height = box_height
      ),
      color = border, fill = fill
    ) +
    # text in boxes
    ggplot2::geom_text(
      data = plotting_data, ggplot2::aes(
        x = x,
        y = y,
        label = label
      ),
      size = font_size / ggplot2::.pt
    ) +
    # down arrow
    ggplot2::geom_segment(
      data = plotting_data, ggplot2::aes(
        x = x,
        xend = x,
        y = down_ystart,
        yend = down_yend
      ),
      arrow = ggplot2::arrow(length = 0.5 * ggplot2::unit(font_size, "pt")),
      size = .2,
      na.rm = TRUE
    ) +
    # side arrow
    ggplot2::geom_segment(
      data = plotting_data, ggplot2::aes(
        x = side_xstart,
        xend = side_xend,
        y = side_y,
        yend = side_y
      ),
      arrow = ggplot2::arrow(length = 0.5 * ggplot2::unit(font_size, "pt")),
      size = .2,
      na.rm = TRUE
    ) +
    # complement box
    ggplot2::geom_tile(
      data = plotting_data, ggplot2::aes(
        x = cx,
        y = cy,
        width = box_width,
        height = box_height
      ),
      color = border, fill = fill,
      na.rm = TRUE
    ) +
    # text in complement box
    ggplot2::geom_text(
      data = plotting_data, ggplot2::aes(
        x = cx,
        y = cy,
        label = complement_label
      ),
      size = font_size / ggplot2::.pt,
      na.rm = TRUE
    ) +
    # remove all plot elements
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
  
  return(gg)
}





#' @title Align multiple ggplot graphs, taking into account the legend
#'
#' @description This function aligns multiple \code{ggplot} graphs by making them the same width by taking into account the legend width.
#'
#' @param pltlist A list of plots
#'
#' @return List of \code{ggplot} with equal width.
#'
#' @references \url{https://stackoverflow.com/questions/26159495}
#'
#' @examples
#' \donttest{
#'  data("adtte", package = "visR")
#' ## create 2 graphs
#' p1 <- ggplot2::ggplot(adtte, ggplot2::aes(x = as.numeric(AGE), fill = "Age")) +
#'   ggplot2::geom_histogram(bins = 15)
#'
#' p2 <- ggplot2::ggplot(adtte, ggplot2::aes(x = as.numeric(AGE))) +
#'   ggplot2::geom_histogram(bins = 15)
#'
#' ## default alignment does not take into account legend size
#' cowplot::plot_grid(
#'   plotlist = list(p1, p2),
#'   align = "none",
#'   nrow = 2
#' )
#'
#' ## align_plots() takes into account legend width
#' cowplot::plot_grid(
#'   plotlist = visR::align_plots(pltlist = list(p1, p2)),
#'   align = "none",
#'   nrow = 2
#' )
#' }
#'

#'
align_plots <- function(pltlist) {
  if (missing(pltlist) | is.null(pltlist)) {
    base::stop("Please provide a list of valid `ggplot` objects.")
  }
  
  for (plt in pltlist) {
    if (!inherits(plt, "ggplot")) {
      base::stop("Not all elements of the provided list are `ggplot` objects.")
    }
  }
  
  ### turn plots into grobs and determine amount of columns
  plots_grobs <- lapply(pltlist, ggplot2::ggplotGrob)
  
  ncols <- lapply(plots_grobs, function(x) dim(x)[[2]])
  maxcols <- max(unlist(ncols))
  
  ### Function to add more columns to compensate for eg missing legend
  .addcols <- function(x) {
    diffcols <- maxcols - dim(x)[[2]]
    
    if (diffcols > 0) {
      for (i in seq(1:diffcols)) {
        x <- gtable::gtable_add_cols(x, widths = grid::unit(1, "null"), pos = 8)
      }
    }
    
    x
  }
  
  ### TableGrob 1 has 11 columns while the others have only 9 because lacking legend+spacer
  ## => add two columns and then resize
  plots_grobs_xcols <- lapply(plots_grobs, .addcols)
  
  ### assign max length to ensure alignment
  max_width <- do.call(grid::unit.pmax, lapply(plots_grobs_xcols, "[[", "widths"))
  for (i in seq(1, length(plots_grobs_xcols))) {
    plots_grobs_xcols[[i]]$widths <- max_width
  }
  
  # ## The grob of the graph shrinks, but the Y-label inside a different grob remains at same location => move
  # grid.draw(plots.grobs[[1]])
  # grid.draw(plots.grobs.xcols[[1]])
  #
  # # Grob with graph is shronken and label does not move with it because it is in another grob
  # # Investigate which is the relative distance the graphgrob moved so we can move the label equally
  # # Key = spacer that broadened
  # # TableGrob has 11 columns, each with a width
  # plots.grobs[[1]] # at row 7 we have 4 grobs before we see grid background
  # plots.grobs[[1]]$widths
  
  xcol_widths <- grid::convertWidth(
    plots_grobs_xcols[[1]]$widths,
    unitTo = "cm",
    valueOnly = FALSE
  )
  grob_widths <- grid::convertWidth(
    plots_grobs[[1]]$widths,
    unitTo = "cm",
    valueOnly = FALSE
  )
  x <- xcol_widths[[4]] - grob_widths[[4]]
  
  plots_grobs_xcols[[1]]$grobs[[13]]$children[[1]]$x <- grid::unit(x, "cm")
  
  # grid.draw(plots.grobs.xcols[[1]])
  
  
  return(plots_grobs_xcols)
  
  # ## layout without cowplot: rel. length is challenging to get right
  # layout <- cbind(seq(1:length(pltlist)))
  #
  # gridExtra::grid.arrange(grobs = plots.grobs.xcols, layout_matrix=layout)
  
  # ### old code
  # .LegendWidth <- function(x)
  #   x$grobs[[8]]$grobs[[1]]$widths[[4]]
  #
  # plots.grobs <- lapply(pltlist, ggplot2::ggplotGrob)
  # max.widths <-
  #   do.call(grid::unit.pmax, lapply(plots.grobs, "[[", "widths"))
  # legends.widths <- lapply(plots.grobs, .LegendWidth)
  #
  # max.legends.width <-
  #   base::suppressWarnings(do.call(max, legends.widths))
  #
  # plots.grobs.eq.widths <- lapply(plots.grobs, function(x) {
  #   x$widths <- max.widths
  #   x
  # })
  #
  # plots.grobs.eq.widths.aligned <-
  #   lapply(plots.grobs.eq.widths, function(x) {
  #     if (gtable::is.gtable(x$grobs[[8]])) {
  #       x$grobs[[8]] <-
  #         gtable::gtable_add_cols(x$grobs[[8]], unit(abs(diff(
  #           c(LegendWidth(x), max.legends.width)
  #         )), "mm"))
  #     }
  #     x
  #   })
  #
  # plots.grobs.eq.widths.aligned
}

#' @title Get strata level combinations
#'
#' @description Extracts the strata level combinations from a survival objects without the specified strata.
#'
#' @param strata The strata from a survival object
#'
#' @return The strata level combinations from a survival objects without the specified strata.
#'
#' @keywords internal

.get_strata <- function(strata) {
  x1 <- base::strsplit(as.character(strata), ",")
  x2 <- lapply(x1, FUN = function(x) base::sub(".*=", "", x))
  x3 <- base::lapply(x2, FUN = function(x) base::paste0(x, collapse = ", "))
  x4 <- base::unlist(trimws(x3, which = "both"))
  
  return(x4)
}


#' @title Translates options for legend into a list that can be passed to ggplot2
#'
#' @description This function takes the legend position and orientation, defined by the user and puts them into a list for ggplot2.
#'
#' @param legend_position  Default = "right".
#' @param legend_orientation Default = NULL.
#'
#' @return List of legend options for ggplot2.

legendopts <- function(legend_position = "right",
                       legend_orientation = NULL) {
  ## replace default eg "h" if user specified something else
  .ucoalesce <- function(x, default) {
    ifelse(is.null(x), default, x)
  }
  
  showlegend <- TRUE
  
  if (is.character(legend_position)) {
    if (legend_position == "bottom") {
      leg_opts <- list(
        xanchor = "center",
        x = 0.5,
        y = -0.2,
        orientation = .ucoalesce(legend_orientation, "h")
      )
    } else if (legend_position == "right") {
      leg_opts <- list(
        yanchor = "center",
        x = 1.2,
        y = 0.5,
        orientation = .ucoalesce(legend_orientation, "v")
      )
    } else if (legend_position == "top") {
      leg_opts <- list(
        xanchor = "center",
        x = 0.5,
        y = 1.2,
        orientation = .ucoalesce(legend_orientation, "h")
      )
    } else if (legend_position == "left") {
      leg_opts <- list(
        yanchor = "center",
        x = -1.0,
        y = 0.5,
        orientation = .ucoalesce(legend_orientation, "v")
      )
    } else if (legend_position == "none") {
      showlegend <- FALSE
      leg_opts <- NULL
    }
  } else {
    if (length(legend_position) == 2) {
      leg_opts <- list(
        x = legend_position[1],
        y = legend_position[2]
      )
    } else if (length(legend_position) > 2) {
      warning("The provided vector for the legend position contains more than 2 elements, only using the first two.")
      
      leg_opts <- list(
        x = legend_position[1],
        y = legend_position[2]
      )
    } else {
      stop("Invalid argument for 'legend_position'. Use 'bottom', 'right', 'top', 'left', 'none' or a vector indicating the desired absolute x/y position, as for examle c(1, 2).")
    }
  }
  
  return(list(leg_opts = leg_opts, showlegend = showlegend))
}


#' @title Create labels for flowchart
#'
#' @description This function creates lables with a maximal character length per line by combining content of two dataframe columns
#'
#' @param data A dataframe
#' @param description_column_name \code{character} The column name containing description part of the new label
#' @param value_column_name \code{character} The column name containing the sample size part of the new label
#' @param complement_column_name \code{character} The column name containing a complement description part (will result in a second label)
#' @param wrap_width \code{integer} for the maximal character count per line
#'
#' @return The input dataframe extended by two columns containing the label and complement label
#'
#' @keywords internal
#' @noRd
.get_labels <- function(data, description_column_name, value_column_name, complement_column_name = "", wrap_width = 50) {
  label <- complement_label <- NULL
  
  plotting_data <- data %>%
    dplyr::rowwise() %>%
    # below needs update to description_column_name instead of Criteria
    dplyr::mutate(label = paste(strwrap(get(description_column_name), width = wrap_width), collapse = "\n")) %>%
    # below needs update to value_column_name instead of `Remaining N`
    dplyr::mutate(label = sprintf("%s\nN = %d", label, get(value_column_name)))
  
  
  if (complement_column_name != "") {
    plotting_data <- plotting_data %>%
      # below needs update to complement_column_name instead of Complement
      dplyr::mutate(complement_label = paste(strwrap(get(complement_column_name), width = wrap_width), collapse = "\n")) %>%
      dplyr::ungroup() %>%
      # below needs update to value_column_name instead of `Remaining N`
      dplyr::mutate(complement_label = sprintf(
        "%s\nN = %d",
        complement_label,
        dplyr::lag(get(value_column_name)) - get(value_column_name)
      ))
  } else {
    plotting_data <- plotting_data %>%
      dplyr::ungroup() %>%
      dplyr::mutate(complement_label = sprintf(
        "%s N = %d", "Excluded",
        dplyr::lag(get(value_column_name)) - get(value_column_name)
      ))
  }
  
  return(plotting_data)
}

#' @title Calculate the size labels on the
#'
#' @description Calculate the text width and maximal text width of both the label and complement labels
#'
#' @param data Dataframe with label and complement label strings
#' @param label The column containing attrition labels
#' @param complement_label The column containing complement description labels
#'
#' @return The input dataframe extended by several columns containing the label and complement label height and width
#'
#' @keywords internal
#' @noRd
#'
.get_labelsizes <- function(data, label, complement_label) {
  labelheight <- labelwidth <- complementheight <- complementwidth <- maxwidth <- maxheight <- NULL
  
  plotting_data <- data %>%
    dplyr::mutate(
      labelwidth = graphics::strwidth({{ label }}, units = "inch"),
      complementwidth = graphics::strwidth({{ complement_label }}, units = "inch"),
      maxwidth = max(labelwidth, complementwidth),
      labelheight = graphics::strheight({{ label }}, units = "inch"),
      complementheight = graphics::strheight({{ complement_label }}, units = "inch"),
      maxheight = max(labelheight, complementheight)
    ) %>%
    dplyr::select(labelwidth, complementwidth, maxwidth, labelheight, complementheight, maxheight, dplyr::everything())
  return(plotting_data)
}

#' @title Create coordinates for each row in the attrition table
#'
#' @description This function creates lables with a maximal character length per line by combining content of two dataframe columns
#'
#' @param data A dataframe containing the attrition data
#' @param box_width \code{integer} The width of the boxes in the flow charts (in canvas coordinates)
#' @param box_height \code{integer} The height of the boxes in the flow charts (in canvas coordinates)
#' @param field_height \code{float} The width of the boxes in the flow charts (in canvas coordinates)
#'
#' @return The input dataframe extended by columns containing x and y coordinates for included and excluded counts
#'
#' @keywords internal
#' @noRd
.get_coordinates <- function(data, box_width, box_height, field_height) {
  y <- ymin <- ymax <- down_ystart <- down_yend <- x <- side_xend <- side_y <- NULL
  
  plotting_data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      x = 50,
      y = 100 - dplyr::row_number() * field_height + box_height / 2
    ) %>%
    # coordinates of text box
    dplyr::mutate(
      box_width = box_width,
      box_height = box_height,
      ymin = y - (box_height / 2),
      ymax = y + (box_height / 2)
    ) %>%
    # coordinates of down arrow
    dplyr::mutate(
      down_ystart = dplyr::lag(ymin),
      down_yend = ymax
    ) %>%
    # coordinates of side arrow
    dplyr::mutate(
      side_y = down_ystart - 0.5 * (down_ystart - down_yend),
      side_xstart = x,
      side_xend = x + (box_width / 2) + 10
    ) %>%
    # complement coordinates
    dplyr::mutate(
      cx = side_xend + (box_width / 2),
      cy = side_y
    )
  
  return(plotting_data)
}

#' @title Extract the numerical alpha representation of #RRGGBBAA colour
#'
#' @description RGB colours can be encoded as hexadecimal values, as for example internally used by `ggplot2`.
#'  For this, the numerical RGB values are mapped from their 0-255 value range to two-character hex-values.
#'  This yields a string in the form of '#RRGGBB'. Additionally, a fourth optional block can be present encoding
#'  the alpha transparency of the colour. This extends the string to '#RRGGBBAA'.
#'  This function takes such a string as input for `hex_colour`, extracts the 'AA' part and returns the
#'  numerical representation if it.
#'
#' @param hex_colour A string in the format '#RRGGBBAA'
#'
#' @return The numeric representation of the colors' alpha value, rounded to 2 digits.
#'
#' @keywords internal

.get_alpha_from_hex_colour <- function(hex_colour = NULL) {
  if (missing(hex_colour) | !is.character(hex_colour)) {
    stop("Please provide a colour in hex representation as a string for `hex_colour`.")
  }
  
  if (!nchar(hex_colour) == 9) {
    stop("Please provide a hex colour in the format #RRGGBBAA.")
  } else {
    colour_parts <- strsplit(hex_colour, "")[[1]]
    
    if (colour_parts[1] != "#") {
      stop("Please provide a hex colour in the format #RRGGBBAA.")
    } else {
      alpha <- grDevices::col2rgb(hex_colour, alpha = TRUE)["alpha", ][[1]]
      alpha <- round(alpha / 255, 2)
      
      return(alpha)
    }
  }
}

#' @title Converts an alpha value between its numeric and its hex-encoded form.
#'
#' @description The function accepts a numeric (or NULL/NA) or a two-character hex encoded alpha representation and returns a the respective other representation.
#'
#' @param numeric_alpha A numerical value between 0 and 1.
#' @param hex_alpha A two-letter character string.
#'
#' @return If `numeric_alpha` was specified, its two-letter representation is returned. If `hex_alpha` was specified, its numeric representation is returned.
#'
#' @keywords internal

.convert_alpha <- function(numeric_alpha = NULL, hex_alpha = NULL) {
  if (missing(numeric_alpha) & missing(hex_alpha)) {
    stop("Either `numeric_alpha` or `hex_alpha` has to be specified.")
  } else if (!missing(numeric_alpha) & missing(hex_alpha)) {
    
    # Two separate ifs so that is.na(NULL) doesn't cause an error
    if (is.null(numeric_alpha)) {
      return("00")
    }
    if (is.na(numeric_alpha)) {
      return("00")
    }
    
    if (is.numeric(numeric_alpha)) {
      if (numeric_alpha > 1 | numeric_alpha < 0) {
        stop("Please enter a numeric value between 0 and 1 for `numeric_alpha`.")
      } else {
        alpha_decimal <- base::round((numeric_alpha * 100) * (255 / 100))
        alpha_hex <- base::format(base::as.hexmode(alpha_decimal),
                                  width = 2,
                                  upper.case = TRUE
        )
        
        return(alpha_hex)
      }
    } else {
      stop("Please enter a numeric value between 0 and 1 for `numeric_alpha`.")
    }
  } else if (missing(numeric_alpha) & !missing(hex_alpha)) {
    if (is.character(hex_alpha) & nchar(hex_alpha) == 2) {
      alpha <- grDevices::col2rgb(paste0("#FFFFFF", hex_alpha), alpha = TRUE)["alpha", ][[1]]
      alpha <- round(alpha / 255, 2)
      
      return(alpha)
    } else {
      stop("Please specify a two-letter character string for `hex_alpha`.")
    }
  } else if (!missing(numeric_alpha) & !missing(hex_alpha)) {
    stop("Please choose either `numeric_alpha` or `hex_alpha`.")
  }
}

#' @title Replaces the AA part of a #RRGGBBAA hex-colour.
#'
#' @description RGB colours can be encoded as hexadecimal values, as for example internally used by `ggplot2`. For this, the numerical RGB values are mapped from their 0-255 value range to two-character hex-values. This yields a string in the form of '#RRGGBB'. Additionally, a fourth optional block can be present encoding the alpha transparency of the colour. This extends the string to '#RRGGBBAA'. This function takes an '#RRGGBBAA' string as input for `colour` and a two-character hex-representation of an alpha value as an input for `new_alpha`, replaces the 'AA' part of `colour` with the `new_alpha` and returns the new colour.
#'
#' @param colour A character string of the format #RRGGBBAA.
#' @param new_alpha A two-character string with the new alpha value.
#'
#' @return A hex-encoded RGBA colour.
#'
#' @keywords internal

.replace_hex_alpha <- function(colour, new_alpha) {
  if (missing(colour) | missing(new_alpha)) {
    stop("Please provide a `colour` and a `new_alpha` in hex representation as strings.")
  }
  
  if (!(is.character(new_alpha) & nchar(new_alpha) == 2)) {
    stop("Please provide a two-character string for the hex representation of the new alpha.")
  }
  
  if (!(is.character(colour))) {
    stop("Please provide a hex colour as a string.")
  } else {
    if (!nchar(colour) == 9) {
      stop("Please provide a hex colour in the format #RRGGBBAA.")
    } else {
      colour_parts <- strsplit(colour, "")[[1]]
      
      if (colour_parts[1] != "#") {
        stop("Please provide a hex colour in the format #RRGGBBAA.")
      } else {
        colour_red <- paste0(colour_parts[2:3], collapse = "")
        colour_green <- paste0(colour_parts[4:5], collapse = "")
        colour_blue <- paste0(colour_parts[6:7], collapse = "")
        new_alpha <- base::toupper(new_alpha)
        
        new_colour <- paste0("#", colour_red, colour_green, colour_blue, new_alpha)
        
        return(new_colour)
      }
    }
  }
}

#' Extract estimate object
#'
#' Function returns the estimate object from `estimate_KM()` or `estimate_cuminc()`
#' extracted from the plotted objected
#' @param gg a ggplot object created with `visr()`
#'
#' @return estimate object
#' @noRd
.extract_estimate_object <- function(gg) {
  if (inherits(gg, "ggsurvfit")) {
    call <- as.character(rlang::quo_squash(gg$data$call[[1]]))
    
    survfit_object <- rlang::eval_tidy(gg$data$call[[1]])
    
    # Since this call is using survival instead of visR, some characteristics are missing eg strata = "Overall" when no strata present
    main <- base::trimws(base::sub(".*~", "", call[[2]]), which = "both")
    
    if (is.null(survfit_object$strata) && main == "1") {
      survfit_object$strata <- as.vector(length(survfit_object$time))
      attr(survfit_object$strata, "names") <- "Overall"
    }
    return(survfit_object)
  } else if (inherits(gg, "ggtidycuminc")) {
    return(attr(gg, "tidycuminc"))
  }
}



#' Construct strata label for visr legend title
#'
#' @param x a survfit or tidycuminc object
#'
#' @return string
#' @noRd
.construct_strata_label <- function(x, sep = ", ") {
  tryCatch(
    {
      if (inherits(x, "survfit") && is.null(x$strata_lbl)) {
        strata_label <- ""
      } else if (inherits(x, "survfit")) {
        strata_label <- unlist(x$strata_lbl) %>% paste(collapse = ", ")
      } else if (inherits(x, "tidycuminc")) {
        strata <- .extract_strata_varlist(x)
        strata_label <-
          lapply(
            as.list(strata),
            function(variable) attr(x$data[[variable]], "label") %||% x
          ) %>%
          unlist() %>%
          paste(collapse = ", ")
      }
      
      strata_label
    },
    error = function(e) {
      return("")
    }
  )
}

#' Extract the strata variable names
#'
#' @param x a survfit or tidycuminc object
#'
#' @return vector of variable names
#' @noRd
.extract_strata_varlist <- function(x) {
  if (inherits(x, "survfit")) {
    return(names(x$strata_lbls))
  }
  if (inherits(x, "tidycuminc")) {
    return(.formula_to_strata_varlist(x$formula, x$data))
  }
}

#' Extract the strata variable names from model formula
#'
#' @param formula a formula
#' @param data a data frame
#'
#' @return vector of variable names
#' @noRd
.formula_to_strata_varlist <- function(formula, data) {
  tryCatch(
    {
      strata <- stats::model.frame(formula, data = data)[, -1, drop = FALSE] %>% names()
      if (rlang::is_empty(strata)) {
        strata <- NULL
      }
      strata
    },
    error = function(e) {
      return(NULL)
    }
  )
}