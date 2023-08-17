


HH_plot.likert.formula <-
  function (x,
            data,
            ReferenceZero = NULL,
            value,
            levelsName = "",
            scales.in = NULL,
            between = list(x = 1 + (horizontal), y = 0.5 +
                             2 * (!horizontal)),
            auto.key.in = NULL,
            panel.in = NULL,
            horizontal = TRUE,
            par.settings.in = NULL,
            ...,
            as.percent = FALSE,
            ylab = if (horizontal) {
              if (length(x) == 3)
                deparse(x[[2]])
              else
                "Question"
            } else if (as.percent != FALSE)
              "Percent"
            else
              "Count",
            xlab = if (!horizontal) {
              if (length(x) == 3)
                deparse(x[[2]])
              else
                "Question"
            } else if (as.percent != FALSE)
              "Percent"
            else
              "Count",
            main = x.sys.call,
            rightAxisLabels = rowSums(data.list$Nums),
            rightAxis = !missing(rightAxisLabels),
            ylab.right = if (rightAxis)
              "Row Count Totals"
            else
              NULL,
            xlab.top = NULL,
            right.text.cex = if (horizontal) {
              if (!is.null(scales$y$cex))
                scales$y$cex
              else
                0.8
            } else {
              if (!is.null(scales$x$cex))
                scales$x$cex
              else
                0.8
            },
            xscale.components = xscale.components.top.HH,
            yscale.components = yscale.components.right.HH,
            xlimEqualLeftRight = FALSE,
            xTickLabelsPositive = TRUE,
            as.table = TRUE,
            positive.order = FALSE,
            data.order = FALSE,
            reverse = ifelse(horizontal,
                             as.table, FALSE),
            h.resizePanels = sapply(result$y.used.at,
                                    length),
            w.resizePanels = sapply(result$x.used.at, length),
            reference.line.col = "gray65",
            col.strip.background = "gray97",
            key.border.white = TRUE,
            col = likertColor(
              Nums.attr$nlevels,
              ReferenceZero = ReferenceZero,
              colorFunction = colorFunction,
              colorFunctionOption = colorFunctionOption
            ),
            colorFunction = "diverge_hcl",
            colorFunctionOption = "lighter")
  {
    rightAxisMissing <- missing(rightAxis)
    if (positive.order)
      data.order <- FALSE
    if (!missing(value)) {
      x.sys.call <- deparse(match.call()[1:4], width.cutoff = 500L)
      varNamesUsedLong <- c(getVarNames(x, data), list(Value = value))
      levelsName <- varNamesUsedLong$LevelNames[[1]]
      data.list.list <- getLikertDataLong(x, data, varNamesUsedLong)
      data.list <- data.list.list$data.list
      varNamesUsed <- data.list.list$varNamesUsed
      x <- data.list.list$x
    }
    else {
      x.sys.call <- deparse(match.call()[1:3], width.cutoff = 500L)
      varNamesUsed <- getVarNames(x, data)
      data.list <- getLikertData(data, varNamesUsed)
    }
    if (as.percent != FALSE) {
      Nums.pct <- data.list$Nums / rowSums(data.list$Nums) *
        100
      Nums.pct[data.list$Nums == 0] <- 0
      Nums.lik <- as.likert(Nums.pct, ReferenceZero = ReferenceZero)
      if (rightAxisMissing && as.percent != "noRightAxis") {
        rightAxis <- TRUE
        if (missing(ylab.right))
          ylab.right <- "Row Count Totals"
      }
    }
    else {
      Nums.lik <- as.likert(data.list$Nums, ReferenceZero = ReferenceZero)
    }
    par.settings <-
      list(strip.background = list(col = col.strip.background))
    par.settings[names(par.settings.in)] <- par.settings.in
    if (rightAxis) {
      par.settings$clip$panel <- "off"
      if (horizontal) {
        par.settings$layout.widths$ylab.right <-
          max(6, par.settings$layout.widths$ylab.right,
              na.rm = TRUE)
        par.settings$layout.widths$axis.key.padding <- max(2,
                                                           par.settings$layout.widths$axis.key.padding,
                                                           na.rm = TRUE)
      }
      else {
        par.settings$layout.heights$main.key.padding <- max(2,
                                                            par.settings$layout.heights$main.key.padding,
                                                            na.rm = TRUE)
        par.settings$layout.heights$key.axis.padding <- max(1.5,
                                                            par.settings$layout.heights$key.axis.padding,
                                                            na.rm = TRUE)
      }
    }
    Nums.attr <- attributes(Nums.lik)
    scales <-
      list(x = list(alternating = 1), y = list(alternating = 1))
    if (!missing(scales.in)) {
      scales.x <- scales$x
      scales.y <- scales$y
      if (is.null(scales.in$x) && is.character(scales.in$y))
        scales.in$y <- list(relation = scales.in$y)
      if (is.null(scales.in$y) && is.character(scales.in$x))
        scales.in$x <- list(relation = scales.in$x)
      if (is.character(scales.in))
        scales.in <- list(x = list(relation = scales.in$x),
                          y = list(relation = scales.in$y))
      scales.x[names(scales.in$x)] <- scales.in$x
      scales.y[names(scales.in$y)] <- scales.in$y
      scales[names(scales.in)] <- scales.in
      scales$x <- scales.x
      scales$y <- scales.y
    }
    lim <- NULL
    if (horizontal) {
      xlim <- list(...)$xlim
      if (!is.null(xlim) && is.null(scales.in$x$limits))
        lim <- xlim
      if (!is.null(scales.in$x$limits))
        lim <- scales.in$x$limits
    }
    else {
      ylim <- list(...)$ylim
      if (!is.null(ylim) && is.null(scales.in$y$limits))
        lim <- ylim
      if (!is.null(scales.in$y$limits))
        lim <- scales.in$y$limits
    }
    if (((
      horizontal && is.null(scales$x$at) && is.null(scales$x$labels)
    ) ||
    (
      !horizontal &&
      is.null(scales$y$at) && is.null(scales$y$labels)
    )) &&
    (xlimEqualLeftRight || xTickLabelsPositive)) {
      if (is.null(lim)) {
        tmp <- Nums.lik
        tmp[Nums.lik < 0] <- 0
        data.max <- max(rowSums(tmp))
        tmp <- Nums.lik
        tmp[Nums.lik > 0] <- 0
        data.min <- min(rowSums(tmp))
        lim <- c(data.min, data.max)
        lim <- lim + c(-0.04, 0.04) * diff(lim)
      }
      if (xlimEqualLeftRight)
        lim <- c(-1, 1) * max(abs(lim))
      at <- pretty(lim)
      if (horizontal && !is.null(scales.in$x$at))
        at <- scales.in$x$at
      if (!horizontal && !is.null(scales.in$y$at))
        at <- scales.in$y$at
      if (xTickLabelsPositive)
        labels <- abs(at)
      else
        labels <- at
      if (horizontal && !is.null(scales.in$x$labels))
        at <- scales.in$x$labels
      if (!horizontal && !is.null(scales.in$y$labels))
        at <- scales.in$y$labels
      if (horizontal) {
        scales$x$limits <- lim
        scales$x$at <- at
        scales$x$labels <- labels
      }
      else {
        scales$y$limits <- lim
        scales$y$at <- at
        scales$y$labels <- labels
      }
    }
    if (horizontal)
      FormulaString <- with(varNamesUsed,
                            paste(
                              "`",
                              QuestionName,
                              "` ~ .value",
                              if (is.null(CondNames))
                                NULL
                              else
                                paste(" |", paste(CondNames, collapse = " + ")),
                              sep = ""
                            ))
    else
      FormulaString <- with(varNamesUsed,
                            paste(
                              ".value ~ `",
                              QuestionName,
                              "`",
                              if (is.null(CondNames))
                                NULL
                              else
                                paste(" |", paste(CondNames, collapse = " + ")),
                              sep = ""
                            ))
    if (is.logical(auto.key.in) && length(auto.key.in) == 1 &&
        auto.key.in == FALSE)
      auto.key <- FALSE
    else {
      auto.key = list(
        title = levelsName,
        text = Nums.attr$original.levels,
        columns = ifelse(horizontal, Nums.attr$nlevels, 1),
        space = ifelse(horizontal, "bottom", "right"),
        reverse.rows = ifelse(horizontal,
                              FALSE, TRUE),
        size = 2,
        cex = 0.8,
        between = 0.6,
        points = FALSE,
        rectangles = FALSE,
        rect = list(col = col,
                    border = if (key.border.white)
                      "white"
                      else
                        col)
      )
      if (!missing(auto.key.in))
        auto.key[names(auto.key.in)] <- auto.key.in
    }
    data2 <-
      with(
        data.list,
        data.frame(
          rightAxisLabels = rightAxisLabels,
          Question,
          Conditions,
          Nums.lik,
          check.names = FALSE
        )
      )
    names(rightAxisLabels) <- data.list$Question[[1]]
    {
      if (positive.order || data.order) {
        if (positive.order) {
          if (reverse)
            data2 <- data2[Nums.attr$positive.order,]
          else
            data2 <- data2[rev(Nums.attr$positive.order),]
        }
        else {
          do <- 1:nrow(data2)
          if (reverse)
            data2 <- data2[do,]
          else
            data2 <- data2[rev(do),]
        }
        newQ <- factor(data2[[varNamesUsed$QuestionName]],
                       levels = unique(data2[[varNamesUsed$QuestionName]]))
        new.order <-
          do.call(order, data.frame(data2[, varNamesUsed$CondNames,
                                          drop = FALSE], newQ = newQ, check.names = FALSE))
        data2[[varNamesUsed$QuestionName]] <-
          factor(data2[[varNamesUsed$QuestionName]],
                 levels = unique(newQ[new.order]))
      }
      if (!positive.order && reverse && !data.order)
        data2[[varNamesUsed$QuestionName]] <-
        factor(data2[[varNamesUsed$QuestionName]],
               levels = rev(levels(data2[[varNamesUsed$QuestionName]])))
    }
    if (rightAxis)
      data2.melt <- reshape2::melt((data2[match(unique(names(data2)),
                                                names(data2))]),
                                   id.vars = c(unique(unlist(varNamesUsed[1:2])),
                                               "rightAxisLabels"),
                                   variable.name = ".variable"
      )
    else
      data2.melt <- reshape2::melt((data2[match(unique(names(data2)),
                                                names(data2))])[,-1],
                                   id.vars = unique(unlist(varNamesUsed[1:2])),
                                   variable.name = ".variable")
    names(data2.melt)[ncol(data2.melt)] <- ".value"
    panel <- function(x,
                      y,
                      subscripts,
                      ...,
                      horizontal = horizontal,
                      rightAxis = rightAxis,
                      rightAxisLabels = rightAxisLabels,
                      reference.line.col = reference.line.col,
                      right.text.cex = 0.8) {
      if (horizontal)
        panel.abline(v = 0, col = reference.line.col)
      else
        panel.abline(h = 0, col = reference.line.col)
      panel.barchart(x, y, subscripts = subscripts, ..., horizontal = horizontal)
      if (rightAxis) {
        if (horizontal) {
          at.which <- match(levels(y), y)
          labels <- (rightAxisLabels[subscripts])[at.which]
          panel.axis(
            "right",
            at = seq(along = levels(y)),
            labels = labels,
            outside = TRUE,
            half = FALSE,
            text.cex = right.text.cex
          )
        }
        else {
          at.which <- match(levels(x), x)
          labels <- (rightAxisLabels[subscripts])[at.which]
          panel.axis(
            "top",
            at = seq(along = levels(x)),
            labels = paste(labels, "\n", sep = ""),
            outside = TRUE,
            half = FALSE,
            rot = 0,
            text.cex = right.text.cex
          )
        }
      }
    }
    if (!is.null(panel.in))
      panel <- panel.in
    barchart.args <-
      list(
        as.formula(FormulaString),
        groups = data2.melt$.variable,
        data = data2.melt,
        as.table = as.table,
        xlab = xlab,
        ylab = ylab,
        ylab.right = ylab.right,
        xlab.top = xlab.top,
        main = main,
        horizontal = horizontal,
        stack = TRUE,
        reference = TRUE,
        col = col[Nums.attr$color.seq],
        panel = panel,
        scales = scales,
        right.text.cex = right.text.cex,
        between = between,
        auto.key = auto.key,
        par.settings = par.settings,
        reference.line.col = reference.line.col,
        ...,
        xscale.components = xscale.components,
        yscale.components = yscale.components,
        rightAxis = rightAxis,
        rightAxisLabels = data2.melt$rightAxisLabels,
        subscripts = TRUE
      )
    if (is.null(list(...)$border))
      barchart.args$border <- barchart.args$col
    result <- do.call("barchart", barchart.args)
    if (length(h.resizePanels) > 0) {
      if (is.character(h.resizePanels) && h.resizePanels ==
          "rowSums")
        h.resizePanels <- rowSums(data.list$Nums)
      result <- resizePanels(result, h = h.resizePanels)
    }
    if (length(w.resizePanels) > 0) {
      if (is.character(w.resizePanels) && w.resizePanels ==
          "rowSums")
        w.resizePanels <- rowSums(data.list$Nums)
      result <- resizePanels(result, w = w.resizePanels)
    }
    result
  }

