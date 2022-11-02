#' format-p-values-vcd
#' 
#' 
#' How to format p-values in an association plot made with "vcd"?
#'
#'
#' https://stackoverflow.com/questions/54674441/how-to-format-p-values-in-an-association-plot-made-with-vcd
#' @name format_vcd
#' @export
#' @examples 
#' 
#' require(vcd)
#' #' class(legend_resbased)
#' legend_resbased<-stp25plot::legend_resbased2
#' shading_hcl <-stp25plot::shading_hcl2
#' assignInNamespace("legend_resbased", legend_resbased, pos = "package:vcd")
#' assignInNamespace("shading_hcl", shading_hcl, pos = "package:vcd")
#' #' 
#' pst <- function(txt) {
#'   paste(txt, "n =", n)
#' }
#' 
#' ipol <- function(x)
#'   pmin(x / 10, 1)
#' 
#' 
#' # figure-2 --------------------------------------------------------
#' 
#' 
#' dat <- stp25tools::get_data(
#'   "
#' racing diet    sex Freq
#'     km10 omnv female  147
#'       HM omnv female  238
#'     M_UM omnv female  215
#'     km10 vgtr female  113
#'       HM vgtr female  143
#'     M_UM vgtr female  125
#'     km10 vegn female  208
#'       HM vegn female  271
#'     M_UM vegn female  168
#'    km10 omnv   male   76
#'      HM omnv   male  197
#'    M_UM omnv   male  399
#'    km10 vgtr   male   29
#'      HM vgtr   male   72
#'    M_UM vgtr   male  116
#'    km10 vegn   male   49
#'      HM vegn   male  111
#'    M_UM vegn   male  187"
#' )
#' 
#' xtabs(Freq ~ racing + diet + sex, dat)
#' 
#' mosaic(
#'   Freq ~ racing + diet | sex,
#'   dat,
#'   shade = TRUE,
#'   legend = TRUE ,
#'   varnames = F,
#'   labeling_args = list(abbreviate_labs = c(8, 5, 4)),
#'   gp = shading_hcl,
#'   gp_args = list(
#'     interpolate = ipol,
#'     c = 100,
#'     lty = 1
#'   ),
#'   gp_labels = gpar(fontsize = 11),
#'   labeling = labeling_residuals,
#'   gp_text  =  gpar(fontsize = 9)
#' )
#' 
#' 
#' cotabplot(
#'   Freq ~ racing + diet + sex,
#'   dat,
#'   panel = cotab_coindep,
#'   n = 5000,
#'   type = "assoc",
#'   test = "maxchisq",
#'   interpolate = 1:2,
#'   legend = TRUE
#' )
#' 
#' 
#' 
#' cotabplot(
#'   Freq ~ racing + diet + sex,
#'   dat,
#'   panel = cotab_coindep,
#'   n = 5000,
#'   #  type = "assoc",
#'   test = "maxchisq",
#'   interpolate = 1:2,
#'   legend = TRUE
#' )
legend_resbased2 <-
  structure(function(fontsize = 12,
                     fontfamily = "",
                     x = unit(1, "lines"),
                     y = unit(0.1, "npc"),
                     height = unit(0.8, "npc"),
                     width = unit(0.7, "lines"),
                     digits = 2,
                     check_overlap = TRUE,
                     text = NULL,
                     steps = 200,
                     ticks = 10,
                     pvalue = TRUE,
                     range = NULL) {
    if (!is.unit(x))
      x <- unit(x, "native")
    if (!is.unit(y))
      y <- unit(y, "npc")
    if (!is.unit(width))
      width <- unit(width, "lines")
    if (!is.unit(height))
      height <- unit(height, "npc")
    function(residuals, shading, autotext) {
      res <- as.vector(residuals)
      if (is.null(text))
        text <- autotext
      p.value <- attr(shading, "p.value")
      legend <- attr(shading, "legend")
      if (all(residuals == 0)) {
        pushViewport(
          viewport(
            x = x,
            y = y,
            just = c("left",
                     "bottom"),
            default.units = "native",
            height = height,
            width = width
          )
        )
        grid.lines(y = 0.5)
        grid.text(
          0,
          x = unit(1, "npc") + unit(0.8, "lines"),
          y = 0.5,
          gp = gpar(fontsize = fontsize, fontfamily = fontfamily)
        )
        warning("All residuals are zero.")
      }
      else {
        if (is.null(range))
          range <- range(res)
        if (length(range) != 2)
          stop("Range must have length two!")
        if (is.na(range[1]))
          range[1] <- min(res)
        if (is.na(range[2]))
          range[2] <- max(res)
        pushViewport(
          viewport(
            x = x,
            y = y,
            just = c("left",
                     "bottom"),
            yscale = range,
            default.units = "native",
            height = height,
            width = width
          )
        )
        if (is.null(legend$col.bins)) {
          col.bins <- seq(range[1], range[2], length = steps)
          at <- NULL
        }
        else {
          col.bins <- sort(unique(c(legend$col.bins, range)))
          col.bins <- col.bins[col.bins <= range[2] & col.bins >=
                                 range[1]]
          at <- col.bins
        }
        y.pos <- col.bins[-length(col.bins)]
        y.height <- diff(col.bins)
        grid.rect(
          x = unit(rep.int(0, length(y.pos)), "npc"),
          y = y.pos,
          height = y.height,
          default.units = "native",
          gp = gpar(
            fill = shading(y.pos + 0.5 * y.height)$fill,
            col = 0
          ),
          just = c("left", "bottom")
        )
        grid.rect(gp = gpar(fill = "transparent"))
        if (is.null(at))
          at <- seq(
            from = head(col.bins, 1),
            to = tail(col.bins,
                      1),
            length = ticks
          )
        lab <- format(round(at, digits = digits), nsmall = digits)
        tw <- lab[which.max(nchar(lab))]
        grid.text(
          format(signif(at, digits = digits)),
          x = unit(1,
                   "npc") + unit(0.8, "lines") + unit(1, "strwidth",
                                                      tw),
          y = at,
          default.units = "native",
          just = c("right",
                   "center"),
          gp = gpar(fontsize = fontsize, fontfamily = fontfamily),
          check.overlap = check_overlap
        )
        grid.segments(
          x0 = unit(1, "npc"),
          x1 = unit(1, "npc") +
            unit(0.5, "lines"),
          y0 = at,
          y1 = at,
          default.units = "native"
        )
      }
      popViewport(1)
      grid.text(
        text,
        x = x,
        y = unit(1, "npc") - y + unit(1,
                                      "lines"),
        gp = gpar(
          fontsize = fontsize,
          fontfamily = fontfamily,
          lineheight = 0.8
        ),
        just = c("left", "bottom")
      )
      if (!is.null(p.value) && pvalue) {
        grid.text(
          p.value,
          x = x,
          y = y - unit(1, "lines"),
          gp = gpar(
            fontsize = fontsize,
            fontfamily = fontfamily,
            lineheight = 0.8
          ),
          just = c("left",
                   "top")
        )
      }
    }
  }, 
  class = "grapcon_generator")

#' @rdname format_vcd
#' @export
 shading_hcl2 <-
  structure(function (observed,
                      residuals = NULL,
                      expected = NULL,
                      df = NULL,
                      h = NULL,
                      c = NULL,
                      l = NULL,
                      interpolate = c(2, 4),
                      lty = 1,
                      eps = NULL,
                      line_col = "black",
                      p.value = NULL,
                      level = 0.95,
                      ...)
  {
    if (is.null(h))
      h <- c(260, 0)
    if (is.null(c))
      c <- c(100, 20)
    if (is.null(l))
      l <- c(90, 50)
    my.h <- rep(h, length.out = 2)
    my.c <- rep(c, length.out = 2)
    my.l <- rep(l, length.out = 2)
    lty <- rep(lty, length.out = 2)
    if (is.null(expected) && !is.null(residuals))
      stop("residuals without expected values specified")
    if (!is.null(expected) && is.null(df) && is.null(p.value)) {
      warning("no default inference available without degrees of freedom")
      p.value <- NA
    }
    if (is.null(expected) && !is.null(observed)) {
      expected <- loglin(observed,
                         1:length(dim(observed)),
                         fit = TRUE,
                         print = FALSE)
      df <- expected$df
      expected <- expected$fit
    }
    if (is.null(residuals) && !is.null(observed))
      residuals <- (observed - expected) / sqrt(expected)
    if (is.null(p.value))
      p.value <-
      function(observed, residuals, expected, df)
        pchisq(sum(as.vector(residuals) ^ 2),
               df, lower.tail = FALSE)
    if (!is.function(p.value) && is.na(p.value)) {
      max.c <- my.c[1]
      p.value <- NULL
    }
    else {
      if (is.function(p.value))
        p.value <- p.value(observed, residuals, expected,
                           df)
      max.c <- ifelse(p.value < (1 - level), my.c[1], my.c[2])
    }
    if (!is.function(interpolate)) {
      col.bins <- sort(interpolate)
      interpolate <-
        stepfun(col.bins, seq(0, 1, length = length(col.bins) +
                                1))
      col.bins <- sort(unique(c(col.bins, 0, -col.bins)))
    }
    else {
      col.bins <- NULL
    }
    legend <- NULL
    if (!is.null(col.bins)) {
      res2 <- col.bins
      res2 <- c(head(res2, 1) - 1, res2[-1] - diff(res2) / 2,
                tail(res2, 1) + 1)
      legend.col <- hcl2hex(
        ifelse(res2 > 0, my.h[1], my.h[2]),
        max.c * pmax(pmin(interpolate(abs(
          res2
        )), 1), 0),
        my.l[1] + diff(my.l) * pmax(pmin(interpolate(abs(
          res2
        )),
        1), 0),
        ...
      )
      lty.bins <- 0
      legend.lty <- lty[2:1]
      legend <- list(
        col = legend.col,
        col.bins = col.bins,
        lty = legend.lty,
        lty.bins = lty.bins
      )
    }
    rval <- function(x) {
      res <- as.vector(x)
      fill <- hcl2hex(
        ifelse(res > 0, my.h[1], my.h[2]),
        max.c *
          pmax(pmin(interpolate(abs(
            res
          )), 1), 0),
        my.l[1] +
          diff(my.l) * pmax(pmin(interpolate(abs(
            res
          )), 1),
          0),
        ...
      )
      dim(fill) <- dim(x)
      col <- rep(line_col, length.out = length(res))
      if (!is.null(eps)) {
        eps <- abs(eps)
        col[res > eps] <- hcl2hex(my.h[1], max.c, my.l[2],
                                  ...)
        col[res < -eps] <- hcl2hex(my.h[2], max.c, my.l[2],
                                   ...)
      }
      dim(col) <- dim(x)
      ltytmp <- ifelse(x > 0, lty[1], lty[2])
      if (!is.null(eps))
        ltytmp[abs(x) < abs(eps)] <- lty[1]
      dim(ltytmp) <- dim(x)
      return(structure(list(
        col = col,
        fill = fill,
        lty = ltytmp
      ),
      class = "gpar"))
    }
    attr(rval, "legend") <- legend
    attr(rval, "p.value") <-
      ifelse(p.value < 0.01,
             "p-value\n < 0.01",
             paste("p-value =\n" , format.pval(p.value)))
    return(rval)
  }, 
  class = "grapcon_generator")
#Then substitute these functions to the corresponding vcd functions:



 

