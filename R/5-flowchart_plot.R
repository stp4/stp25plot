#' flowchart_plot
#' 
#' 
#' @description Uebernommen aus library(diagram)
#' @param txt ist liste mit den Inhalten
#' @param txt_length  Zeilenumbruch
#' @param cex  schriftgroesse
#' @param arr.pos Pfeilspitze
#' @param box.size vektor der Box und zwar für links und rechts
#' @param  use_str_wrap  workaround fuer new-lines = TRUE,
#' @param ...  alles fuer reorder
#' @export
#' @examples
#' 
#' txt<-"AAA" #LETTERS[sample.int(23,1)]
#' 
#' flowchart_plot(list(
#'   File=txt,
#'   Raw.x=c("B","C"),
#'   Clean.Up = c("D",paste("E",format(Sys.time(), "%Y-%m-%d"))                        ),
#'   Analyse=c( "F", "G")
#'   
#' ),
#' box.size=c(.1,.12) )
flowchart_plot <- function(txt,
                           txt_length = 30,
                           cex = .9,
                           #box_w, box_h,
                           arr.pos = 0.55,
                           box.size = c(.1, .15),
                           use_str_wrap = TRUE,
                           ...)
{
  nams <- NULL  # text in diagramm
  x <- NULL    	# position in x richtung
  y <- NULL		# position in y richtung
  b <- NULL		# breite der box links sind sie schmaeler
  p <- NULL		# pfeil
  l <- NULL		# laenge der box
  
  N <- sum(lengths(txt)) + length(txt) # anzahl an elemente
  M <- matrix(
    nrow = N,
    ncol = N,
    byrow = TRUE,
    data = 0
  )
  
  
  for (i in names(txt)) {
    if (use_str_wrap)
      nams <-
        c(nams, i, stringr::str_wrap(txt[[i]], width = txt_length))
    else
      nams <- c(nams, i, txt[[i]])
  }
  for (i in  lengths(txt)) {
    p <- 1:i + if (is.null(y))
      0
    else
      p[length(p)] + 1
    if (i == 1)
      M[p + 1, p] <- ""
    else
      diag(M[p + 1, p])  <- ""
    x <- c(x, 0.2, rep(.7, i))
    b <- c(b, 0.2, rep(.3, i))       # Laenge/Breite Verhaeltnis
    l <- c(l, box.size[1] , rep(box.size[2], i))      # Box Laenge
    yi <- c(1, 1:i) + if (is.null(y))
      - 1
    else
      y[length(y)]
    y <- c(y, yi)
  }
  
  y <- round((max(y) - y) / max(y) * 0.8 + .1, 2)  #oben und unten 0.1 platz
  
  #print(nams)
  par(mar = c(1, 1, 1, 1))
  diagram::plotmat(
    M,
    pos = cbind(x, y),
    curve = 0,
    name = nams,
    lwd = 1,
    box.size = l,
    arr.pos = arr.pos,
    box.lwd = 2,
    box.cex = cex,
    box.type = "square",
    box.prop = b,
    ...
  )
  # SaveData("FlowChart")
  
  
}


#' @rdname flowchart_plot
#' @description simple_consort_plot()  simple consort plot with the help of Filter2().
#'
#' @param x data.frame
#' @param ... an visr_copy
#'
#' @return  plot
#' @export
#' 
#' @examples
#' 
#' 
#' data(DFdummy, package = "stp25data")
#' 
#' DF1 <- DFdummy |> stp25tools::filter2(study.agreement)
#' attr(DF1, "filter")
#' 
#' simple_consort_plot(DF1)
#' 
#'
#' DF2 <- DF1 |> stp25tools::filter2(
#' st.p.sars.cov2 == "nein",
#' !is.na(spike.igg.3.impfung),
#' !is.na(MPN)
#' 
#' )
#' 
#' DF3 <- DF2 |> stp25tools::filter2(
#'   study.agreement,
#'   sero.negativ.after.dose.2,
#'   !is.na(spike.igg.3.impfung),
#'   !is.na(spike.igg.4.impfung),
#'   spike.igg.3.impfung == "<7.1 BAU/ml"
#' )
#' dat <- prepare_consort(DF1, DF2, DF3)
#' 
#' consort::consort_plot(
#'   dat,
#'   orders = c(
#'     Trial.Nr   = "Population",
#'     Condition.1           = "Excluded",
#'     Trial.Nr     = "Allocated \nDeskriptive Analyse",
#'     Condition.2    =    "Fehlende Daten",
#'     Trial.Nr = "Regressionsanalyse",
#'     Condition.3    = "Not evaluable for the final analysis",
#'     Trial.Nr = "Final Analysis"
#'   ),
#'   side_box = c("Condition.1", "Condition.2", "Condition.3"),
#'   cex = 0.9
#' )
#'
simple_consort_plot <- function(x, ...) {
  visr_copy(attr(x, "filter"),  ...)
}


#' @rdname flowchart_plot
#'
visr_copy <- function(x, ...) {
  UseMethod("visr_copy", x)
}

#' @rdname flowchart_plot
#' @method visr_copy default
visr_copy.default <- function(x, ...) {
  graphics::plot(x)
}



#' @description #'
#' S3 function to draw a Consort flow diagram chart.
#' Stolen from https://cran.r-project.org/web//packages/visR/index.html
#' 
#' https://cran.r-project.org/web//packages/visR/vignettes/Consort_flow_diagram.html
#' 
#'
#' @rdname flowchart_plot
#' @method visr_copy attrition
visr_copy.attrition <- function(x,
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


