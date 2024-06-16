#' Plot prism-like Plots
#'
#' gestohlen von Dustin Fife
#'
#'
#' @param formula a formula object with the quantitative variable as the response variable (e.g., Var~group).
#' @param data a dataset containing the variables indicated in \code{formula}
#' @param fun what function should be used to indicate the center of the distribution. Defaults to median.
#' @param interquartile Should the interquartile range be plotted? Defaults to TRUE.
#' @param spreadfunc what function should be used to calculate the spread of the distribution? If interquartile=TRUE,
#' this argument will be ignored. The default (when not ignored) is to produce a 95\% confidence interval (1.96*sd(x)/sqrt(n)).
#' @param def.axis Logical. Should the default axes be used?
#' @param jitter Logical. Should the y values be jittered as well?
#' @param add Should the plot be added to an existing plot?
#' @param col What color should the dots be painted? Defaults to gray.
#' @param ... other arguments passed to plot
## @author Dustin Fife
#' @export
#' @examples
#' 
#' require(lattice)
#' require(latticeExtra)
#' require(effects)
#' 
#' prism.plots(count ~ spray, data = InsectSprays, fun = mean)
#' 
#' Insect <- lm(count ~ spray, data = InsectSprays)
#' stripplot(
#'   count ~ spray,
#'   data = InsectSprays,
#'   # ylim=c(-10,70),
#'   ylab = "Insect count",
#'   xlab = "The type of spray",
#'   jitter.data = TRUE,
#'   panel = function(x, y, ...) {
#'     # panel.conf.int(x, y, ...)
#'     panel.stripplot(x, y, pch = 19, col = "gray50", ...)
#'     # panel.points(x,y, pch=19,...)
#'     # panel.mean(x,y,  ...)
#'     panel.median(x, y, ...)
#'     panel.sig.bars(Insect, include.stars = FALSE, offset = .4)
#'   }
#' )
#' 
#' 
#' p <- stripplot(
#'   count ~ spray,
#'   data = InsectSprays,
#'   jitter.data = TRUE,
#'   pch = 20,
#'   col = "gray50"
#' )
#' p + layer(panel.sig.bars(Insect, include.stars = FALSE, offset = .4))
#' 
#' p2 <- plot(effect("spray", Insect), lty = 0 , main = "Effectiveness of Insect Sprays")
#' p2 +  layer(panel.sig.bars(Insect, include.stars = FALSE, offset = .4))
 
prism.plots = function(formula,
                       data,
                       fun = median,
                       interquartile = TRUE,
                       spreadfunc = function(x) { return(1.96 * sd(x) / sqrt(length(x)))},
                       def.axis = TRUE,
                       jitter = TRUE,
                       add = FALSE,
                       col = "gray",
                       pch = 16,
                       
                       ...) {
  dv = as.character(formula[[2]])
  iv = as.character(formula[[3]])
  set.seed(1)
  #### make a vector of colors if they didn't supply one
  col = rep(col, times = nrow(data))
  
  
  #### resort so variables line up
  ord = order(data[, iv])
  data = data[ord,]
  col = col[ord]
  types = unique(data[, iv])
  
  centers = aggregate(formula, data = data, FUN = fun)[, 2]
  spread = matrix(nrow = length(types), ncol = 2)
  if (interquartile) {
    vals = aggregate(formula,
                     data = data,
                     FUN = quantile,
                     probs = c(.25, .75))
    spread = vals[, 2]
  } else {
    ss = aggregate(formula, data = data, FUN = spreadfunc)[, 2]
    spread = cbind(centers - ss, centers + ss)
  }
  
  
  data = data[order(data[, iv]),]
  un.vals = unique(data[, iv])
  iv.vals = rep(NA, times = nrow(data))
  for (i in 1:length(iv.vals)) {
    rws = which(data[, iv] == un.vals[i])
    iv.vals[rws] = i
  }
  
  if (jitter) {
    iv.vals = jitter(iv.vals)
  }
  depvar = data[, dv]
  
  
  
  
  labels = list(
    xlab = "",
    ylab = dv,
    ylim = range(depvar, na.rm = T) + c(-.25 * sd(depvar, na.rm = T), .25 *
                                          sd(depvar, na.rm = T)),
    xlim = c(.5, (length(types) + .5)),
    xaxt = "n",
    x = NA,
    y = NA
  )
  args = modifyList(labels, list(x = NA, ...))
  
  
  
  
  ##### compute mean (or median)
  if (def.axis) {
    if (!add) {
      do.call("plot", args)
    }
    points(iv.vals, depvar,
           col = col, pch = pch, ...)
    axis(1,
         at = (1:length(types)),
         labels = unique(data[, iv]))
  } else {
    if (!add) {
      do.call("plot", args)
    }
    points(iv.vals, depvar, col =
             col, pch = pch, ...)
  }
  
  segments(1:length(unique(data[, iv])) - .2,
           centers,
           1:length(unique(data[, iv])) + .2,
           centers,
           lwd = 2,
           ...)
  segments(1:length(unique(data[, iv])),
           spread[, 1],
           1:length(unique(data[, iv])),
           spread[, 2],
           lwd = 2,
           ...)
  segments(1:length(unique(data[, iv])) - .05,
           spread[, 1],
           1:length(unique(data[, iv])) + .05,
           spread[, 1],
           lwd = 2,
           ...)
  segments(1:length(unique(data[, iv])) - .05,
           spread[, 2],
           1:length(unique(data[, iv])) + .05,
           spread[, 2],
           lwd = 2,
           ...)
}


#' Der Fliegen-Schiss-Plot
#'
#' @param x Objekt lm oder emmeans
#' @param stars Sternchen oder P-Weret
#' @param shift oben unden oder links rechts
#' @param cex.stars groesse
#' @param ... nicht benutzt
#'
#' @return  plot-text
#' @export
#'
plotSigBars <- function(x,
                        include.stars = TRUE,
                        shift = TRUE,
                        cex.stars = .9,
                        offset = 0.2,
                        ...) {
  cntr <- extract_pvalue(x)
  if ((length(cntr$p.value) == 0))
    return()
  
  cntr$p.value <-  cntr$p.value[order(cntr$p.value$p.value), ]
  limits <- par("usr")

  for (i in seq_len(nrow(cntr$p.value))) {
    crt <-
      cordinats(i, cntr$p.value, cntr$levels, limits,
                shift, include.stars, cex.stars, offset)

    segments(crt$blk$x0,  crt$blk$y0,  crt$blk$x1,  crt$blk$y1)
    segments(crt$ant1$x0, crt$ant1$y0, crt$ant1$x1, crt$ant1$y1)
    segments(crt$ant2$x0, crt$ant2$y0, crt$ant2$x1, crt$ant2$y1)
    
    text(
      x = crt$txt$x,
      y = crt$txt$y,
      labels = crt$txt$labels,
      cex = crt$txt$cex,
      pos = crt$txt$pos,
      offset = offset
    )
  }
 # cntr$p.value
}

#' @rdname plotSigBars
#' @export
panel.sig.bars <- function(x,
                           include.stars = TRUE,
                           shift = TRUE,
                           cex.stars = .9,
                           offset = 0.4,
                           ...) {
  
  cntr <- extract_pvalue(x)
  if ((length(cntr$p.value) == 0))
    return()
  
  cntr$p.value <-  cntr$p.value[order(cntr$p.value$p.value), ]
  limits <- current.panel.limits(unit = "native")
  
  limits <- c(limits$xlim,  limits$ylim)

  for (i in seq_len(nrow(cntr$p.value))) {
    crt <-
      cordinats(i, cntr$p.value, cntr$levels, limits,
                shift, include.stars, cex.stars, offset)
    
    panel.segments(crt$blk$x0,  crt$blk$y0,  crt$blk$x1,  crt$blk$y1, col=1)
    panel.segments(crt$ant1$x0, crt$ant1$y0, crt$ant1$x1, crt$ant1$y1, col=1)
    panel.segments(crt$ant2$x0, crt$ant2$y0, crt$ant2$x1, crt$ant2$y1, col=1)
    
    panel.text(
      x = crt$txt$x,
      y = crt$txt$y,
      labels = crt$txt$labels,
      cex = crt$txt$cex,
      pos = crt$txt$pos,
      adj = NULL,
      offset = offset
    )
  }
  
}



cordinats <-
  function(i,
           cntr,
           levs,
           limits,
           shift,
           stars,
           cex.stars,
           offset) {
    xcoords <-
      c(which(levs %in% cntr$lhs[i]) ,   which(levs %in% cntr$rhs[i]))
    
    miny <- limits[3:4]
    yheights <- (miny[2] - miny[1]) / 50
    
    
    if (shift) {
      shft <- i * diff(miny) * 0.04
      xlengths <- 0
    }
    else{
      shft <- 0
      xlengths <- limits
      xlengths <- (xlengths[2] - xlengths[1]) / 50
    }
    
    ### offset so they don't overlap
    if (i / 2 == round(i / 2)) {
      miny[1] = miny[2] - shft
      yheights = -1 * yheights
      adj <- c(NA, 0)
      pos <- if (stars)
        2
      else
        3
      
    }		else{
      miny[1] = miny[1] + shft + diff(miny) * 0.04
      yheights <- yheights
      adj <- c(NA, 1)
      pos <- 1
      
    }
    
    if (stars)  {
      cex.stars <- cex.stars * 1.1
      p.text <- cntr$stars[i]
      
    } else{
      p.text <- cntr$p[i]
      
    }
    
    y  <- miny[1] + yheights
    x1 <- xcoords[1] + xlengths
    x2 <- xcoords[2] - xlengths
    y2 <- miny[1] + 2 * yheights
    list(
      blk = list(
        x0 = x1,        y0 = y,        x1 = x2,        y1 = y
      ),
      ant1 = list(
        x0 = x1,        y0 = y,        x1 = x1,        y1 = y2
      ),
      ant2 = list(
        x0 = x2,        y0 = y,        x1 = x2,        y1 = y2
      ),
      txt = list(
        x = mean(c(xcoords[1], xcoords[2])),
        y = y,
        labels = p.text,
        cex = cex.stars,
        pos = pos
      )
    )
  }


#' @param x lm, lmer, emm_list
#'
#' @return data.frame

#' @noRd
extract_pvalue <- function(x) {
  if (inherits(x, "emm_list")) {
    rslt <- as.data.frame(x[[2]])
    csp <- strsplit(rslt[[1]], " - ")
    rslt <-  data.frame(
      contrast = rslt[[1]],
      lhs = sapply(csp, "[", 1),
      rhs = sapply(csp, "[", 2),
      p.value = rslt[[ncol(rslt)]]
    )
    levs <- x[[1]]@levels[[1]]
  }
  else if (inherits(x, "lmerModLmerTest")) {
    rhs <-  all.vars(formula(x))[2L]
    data <- x@frame
    levs <- levels(data[[rhs]])
    c1 <-  levs[1]
    rslt <- summary(x)$coefficients
    
    rslt <-  data.frame(
      contrast =
        paste(c1, "-", gsub(rhs, "", rownames(rslt))),
      lhs = c1,
      rhs = gsub(rhs, "", rownames(rslt)),
      p.value = rslt[, ncol(rslt)]
    )[-1, ]
  }
  else {
    y <-  all.names(formula(x)[3L])[1]
    data <- x$model
    levs <- levels(data[[y]])
    c1 <-  levs[1]
    rslt <- broom::tidy(x)
    
    rhs = gsub(y, "", rslt[[1]])
    
    if (all(getOption("contrasts") == c("contr.Treatment", "contr.poly"))) {
      rhs <- substr(rhs, 4, nchar(rhs) - 1)
    }
    
    rslt <- data.frame(
      contrast =
        paste(c1, "-", gsub(y, "", rslt[[1]])),
      lhs = c1,
      rhs = rhs,
      p.value = rslt[[ncol(rslt)]]
    )[-1, ]
    
  }

  rslt$p <- stp25stat2:::rndr_P(rslt$p.value)
  rslt$stars <- stp25stat2:::rndr_Stars(rslt$p.value)
  list(p.value = rslt[which(rslt$p.value <= .1), ], levels = levs)
}



 