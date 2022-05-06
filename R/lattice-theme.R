# ## set_lattice
# ##
# ##
# ## @param ... nicht benutzt
# ##
# ## @return im Hintergrund oopt unt opar sowie  invisible(lattice::trellis.par.get())
# ## @export
# ##
# ## @examples
# ##    lattice::trellis.par.set(bw_theme)
# ## require(lattice)
# ## require(latticeExtra)
# ## 
# ## set.seed(2)
# ## n <- 20 * 3 * 2
# ## DF <- data.frame(
# ##   n = runif(n, min = 1, max = 5),
# ##   e = runif(n, min = 1, max = 5),
# ##   o = runif(n, min = 1, max = 5),
# ##   g = runif(n, min = 1, max = 5),
# ##   a = runif(n, min = 1, max = 5),
# ##   treatment = gl(3, n / 3, labels = c("UG1", "UG2", "KG"))[sample.int(n)],
# ##   sex = gl(2, n / 2, labels = c("female", "male"))
# ## )
# ## 
# 
# ## p_bar <- barchart(xtabs(~ sex + treatment, DF),
# ##                   auto.key = list(columns = 1))
# ## 
# ## p_box <- bwplot(e ~ treatment | sex,
# ##                 DF,
# ##                 auto.key = list(columns = 2))
# ## 
# ## p_box2 <- bwplot2(e ~ treatment,
# ##                   DF,
# ##                   groups = sex,
# ##                   auto.key = list(columns = 2))
# ## 
# ## update(p_bar,
# ##        par.settings = theEconomist.theme(), main = "theEconomist.theme")
# ## 
# ## update(p_bar,
# ##        par.settings = bw_theme(), main = "bw_theme")
# ## 
# ## update(p_bar,
# ##        par.settings = bw_theme(farbe()), main = "bw_theme farbe = 'pirat'")
# ## 
# ## 
# ## update(p_box,
# ##        par.settings = bw_theme(cex.add = 1.2), main = "bw_theme cex.add = 1.2")
# ## 
# ## 
# ## update(p_box2,
# ##        par.settings = ggplot_theme())
# ## 
# ## update(p_box2,
# ##        par.settings = bw_theme(farbe("pastel")))
# ## 
# ## 
# set_lattice <- function(theme = standard_theme(), ...) {
#   # workaraund
#   # weil beim standart theme die transpazenz fehlt.
#   lattice::trellis.par.set(theme)
#   invisible(theme)
# }
# 
# ## @rdname set_lattice
# ##
# ## @export
# ##
# set_lattice_ggplot <- function(...) {
#   set_lattice()
#   theme <- ggplot_theme(...)
#   lattice::trellis.par.set(theme)
#   
#   invisible(theme)
# }
# 
# ## @rdname set_lattice
# ## @description set_lattice_bw:  schwarz-weis lattice-Theme
# ## @export
# ##
# set_lattice_bw <- function(...) {
#   set_lattice()
#   theme <- bw_theme(...)
#   lattice::trellis.par.set(theme)
#   
#   invisible(theme)
# }
# 
# ## @rdname set_lattice
# ## @export
# reset_lattice <- function(...) {
#   set_lattice()
# }
# 
# ## @rdname set_lattice
# ## @description  Black and White lattice-Theme
# ## @export
# bw_theme <- function(
#   col = grey.colors(7, start = 0.3, end = 0.9),
#   col.bar =  "grey50",
#   pch = 15:18,
#   lty = 1:3,
#   
#   cex = 1,
#   cex.main = 1.2,
#   cex.axis = 0.8,
#   cex.xlab = 1,
#   cex.ylab = 1,
#   cex.add = 0.8,
#   strip.background.col = "transparent",
#   box=NULL,
#   ...
# ) {
#   theme <- lattice::standard.theme(color = FALSE)
#   theme$strip.background$col <- strip.background.col
#   theme$superpose.symbol$pch <- pch
#   theme$superpose.symbol$fill <- col
#   theme$superpose.symbol$col <- col
#   
#   theme$superpose.polygon$col <- col
#   theme$superpose.polygon$border <- "transparent"
#   
#   theme$plot.polygon$col <- col.bar
#   
#   theme$superpose.line$col <- col
#   theme$superpose.line$lty <- lty
#   
#   theme$box.dot$pch <- 19
#   theme$box.dot$cex <- cex
#   
#   theme$plot.symbol$pch <- 1
#   
#   theme$strip.shingle$col <- col
#   
#   
#   theme$par.xlab.text$cex <- cex.xlab
#   theme$par.ylab.text$cex <- cex.ylab
#   
#   theme$par.main.text$cex <- cex.main
#   # tp$par.title.text
#   
#   
#   theme$axis.text$cex <- cex.axis
#   # text in auto.key
#   theme$add.text$cex <- cex.add
#   
#   #box um die Grafik
#   if(!is.null(box)){
#     
#   theme$box.3d$col<- "transparent"
#   theme$strip.border$col<- "transparent"
#   theme$axis.line$col<- "transparent"
#   }
#   
#   
#   theme
# }
# 
# ## @rdname set_lattice
# ## @export
# standard_theme <- function() {
#   theme <- lattice::standard.theme()
#   theme$background$col <- "transparent"
#   theme
# }
# 
# ## @rdname set_lattice
# ## @description  ggplot lattice-Theme
# ##
# ## @export
# ggplot_theme <- function(col = c("#00BA38",
#                                  "#00BFC4",
#                                  "#619CFF",
#                                  "#F564E3",
#                                  "#F8766D",
#                                  "#B79F00"),
#                          col.bar = NULL ,
#                          pch = 15:18,
#                          lty = 1:3,
#                          cex = 1,
#                          cex.main = 1.2,
#                          cex.axis = 0.8,
#                          cex.xlab = 1,
#                          cex.ylab = 1,
#                          cex.add = 0.8,
#                          strip.background.col = c("grey80", "grey70", "grey60"),
#                          
#                          ...) {
#   theme <- latticeExtra::ggplot2like(n = 6, h.start = 120)
#   
#   if (is.null(col.bar))
#     col.bar <- theme$plot.polygon$col
#   
#   theme$strip.background$col = strip.background.col
#   
#   theme$axis.text$cex = 0.8
#   theme$axis.text$lineheight = 0.9
#   theme$axis.text$col = "#000000"
#   
#   theme$superpose.symbol$col = col
#   theme$superpose.symbol$pch = pch
#   
#   
#   theme$superpose.polygon$col = col
#   theme$superpose.polygon$border = "transparent"
#   
#   theme$plot.polygon$col = col.bar
#   
#   theme$superpose.line$col = col
#   theme$superpose.line$lty = lty
#   
#   theme$box.dot$pch = 19
#   theme$box.dot$cex = cex
#   theme$box.rectangle$col = col.bar
#   theme$superpose.symbol$fill = col
#   
#   theme$plot.symbol$pch = 1
#   
#   
#   theme$par.xlab.text$cex <- cex.xlab
#   theme$par.ylab.text$cex <- cex.ylab
#   theme$par.main.text$cex <- cex.main
#   theme$axis.text$cex <- cex.axis
#   # text in auto.key
#   theme$add.text$cex <- cex.add
#   
#   
#   theme
# }
# 
# 
# 
# 
# 
# 
