#' @rdname addNrisk
#' @export
#'
nrisk <- function(x, 
                  times = pretty(x$time)) {
  stopifnot(class(x) == 'survfit')
  
  if ('strata' %in% names(x)) {
    ns <- length(x$strata)
    idx <- rep.int(1:ns, x$strata)
    str.n.risk <- split(x$n.risk, idx)
    str.times <- split(x$time, idx)
    m <- sapply(times, function(y) {
      sapply(1:ns, function(i) {
        w <- which(str.times[[i]] >= y)[1]
        ifelse(is.na(w), 0, str.n.risk[[i]][w])
      })
    })
    rownames(m) <- names(x$strata)
  } else {
    # no strata
    m1 <- sapply(times, function(y) {
      w <- which(x$time >= y)[1]
      ifelse(is.na(w), 0, x$n.risk[w])
    })
    m <- matrix(m1, nrow = 1)
  }
  colnames(m) <- times
  m
}




#' Add number-at-risk annotations to a plot
#'
#'    Add number-at-risk (NAR) annotations to an existing survival plot, underneath the X-axis.
#'     2009-02-20  Aron Charles Eklund http://www.cbs.dtu.dk
#' @param x A list as returned by survfit
#' @param at Time points at which the NAR values are calculated and placed.
#' @param line Number of lines into the margin to start displaying the NAR.
#' @param hadj izontal adjustment for the NAR values
#' @param title Optional title above the NAR.
#' @param title.adj Text adjustment for the title
#' @param labels Labels for each stratum.
#' @param hoff Horizontal offset for the labels
#' @param col Color for each stratum.
#' @param ... an axis gap.axis, cex.axis, col.axis and font.axis
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
#' require("survival")
#' 
#' s <- Surv(colon$time / 365, colon$status)
#' 
#' ## Need to increase margins a bit
#' par(mar = c(10, 6, 2, 1),mfrow = c(1,2))
#' 
#' ## no stratification
#' fit1 <- survfit(s ~ 1)
#' plot(fit1)
#' addNrisk(fit1)
#' 
#' ## with stratification
#' at <- c(0, 2, 4)
#' lty <- 1:3
#' xlim <- c(0, 6)
#' fit2 <- survfit(s ~ rx, data = colon)
#' plot(fit2,
#'      xlab = 'Time (years)',
#'      ylab = 'Survival',
#'      xaxt = "n",
#'      xlim=xlim,
#'      lty = lty)
#' 
#' addNrisk(fit2, at)
#' axis(1, at = at, gap.axis = 1 / 4)
#' legend(
#'   'bottomleft',
#'   legend = names(fit2$strata),
#'   lty = lty,
#'   bty = 'n'
#' )
#' Hmisc::minor.tick(nx = 4, tick.ratio = 1 / 2)
#' 
addNrisk <- function(x,
                     at = axTicks(1),
                     line = 4,
                     hadj = 0.5,
                     title = 'Number at risk',
                     title.adj = 0,
                     labels,
                     hoff = 5,
                     col = 1,
                     ...) {
  m <- nrisk(x, times = at)
  ns <- nrow(m)  # number of strata
  if (missing(labels)) {
    if (ns > 1) {
      labels <- sapply(strsplit((names(x$strata)), "="), "[", 2)
    } else {
      labels <- NA
    }
  }
  label.pad <- paste(rep(' ', hoff), collapse = '')
  labels2 <- paste(labels, label.pad, sep = '')
  labels2[is.na(labels)] <- NA
  col <- rep(col, length.out = ns)
  hasTitle <- (!is.null(title)) && (!is.na(title))
  if (hasTitle) {
    title(xlab = title,
          line = line,
          adj = title.adj)
  }
  for (i in 1:ns) {
    axis(
      1,
      at = at,
      labels = m[i, ],
      line = line + i + hasTitle - 2,
      tick = FALSE,
      col.axis = col[i],
      hadj = hadj,
      ...
    )
    axis(
      1,
      at = par('usr')[1],
      labels = labels2[i],
      line = line + i + hasTitle - 2,
      tick = FALSE,
      col.axis = col[i],
      hadj = 1,
      ...
    )
  }
  invisible(m)
}
