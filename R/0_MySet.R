#' MySet
#'
#' Initialisiert Lattice ladet die nötigen Pakete und Formatiert die Lattice Grafiken
#' Einstellen der Farben und Symbole ueber \link{trellis.par.set}
#' @param col Farben brewer.pal(9,"Set1")
#' @param pch  Symbole
#' @param lty Striche und Lienien
#' @param cex Punkt in Boxplots
#' @param col.bar Farbe von einzelnen Balken
#' @param n anzahl an Farben default ist 4
#' @param h.start  Farben = 120,
#' @param theme LaticeExtra ggplot2like(), #theEconomist.theme() custom.theme()
#' @param axis.grid  Referenzlinien = FALSE,
#' @param knit einstellung für knitter
#' @param ...  nicht verwendet
#' @return Gibt opar zureuck (Wird fue reset benötigt)
#' @export
#' @examples
#' #graphics.off()
#' #library(stp25)
#' #library(RColorBrewer)
#' # Set3 rosa-himmelblau
#' # brewer.pal(8,"Set3")[c(3,4)]
#'
#'
#'  #- display.brewer.pal(n = 4, name = 'Set2')
#'  #-
#'  #- brewer.pal.info
#'  #- Diverging palettes
#'  #-          maxcolors category colorblind
#'  #- BrBG            11      div       TRUE  braun-gruen
#'  #- PiYG            11      div       TRUE  violet-gruen
#'  #- PRGn            11      div       TRUE  violet-gruen
#'  #- PuOr            11      div       TRUE  braun-violett
#'  #- RdBu            11      div       TRUE  rot-blau
#'  #- RdGy            11      div      FALSE  rot-grau
#'  #- RdYlBu          11      div       TRUE  rot-blau
#'  #- RdYlGn          11      div      FALSE  rot-gruen
#'  #- Spectral        11      div      FALSE  rot-blau
#'  #- Qualitative palettes
#'  #-          maxcolors category colorblind
#'  #- Accent           8     qual      FALSE
#'  #- Dark2            8     qual       TRUE
#'  #- Paired          12     qual       TRUE
#'  #- Pastel1          9     qual      FALSE
#'  #- Pastel2          8     qual      FALSE
#'  #- Set1             9     qual      FALSE
#'  #- Set2             8     qual       TRUE
#'  #- Set3            12     qual      FALSE
#'  #- Sequential palettes
#'  #-          maxcolors category colorblind
#'  #- Blues            9      seq       TRUE
#'  #- BuGn             9      seq       TRUE
#'  #- BuPu             9      seq       TRUE
#'  #- GnBu             9      seq       TRUE
#'  #- Greens           9      seq       TRUE
#'  #- Greys            9      seq       TRUE
#'  #- Oranges          9      seq       TRUE
#'  #- OrRd             9      seq       TRUE
#'  #- PuBu             9      seq       TRUE
#'  #- PuBuGn           9      seq       TRUE
#'  #- PuRd             9      seq       TRUE
#'  #- Purples          9      seq       TRUE
#'  #- RdPu             9      seq       TRUE
#'  #- Reds             9      seq       TRUE
#'  #- YlGn             9      seq       TRUE
#'  #- YlGnBu           9      seq       TRUE
#'  #- YlOrBr           9      seq       TRUE
#'  #- YlOrRd           9      seq       TRUE
#'
#'  #Projekt( )
#'  #MySet(brewer.pal(9,"Set1")[c(8,2)], col.bar=3 )
#'  #windows(8,8)
#'  #  show.settings()
#'  #windows(7,4)
#'  #  bwplot(yield ~ site|year, barley )
#'  #  End()
MySet <- function(col = NULL,
                  # brewer.pal(9,"Set1")
                  pch = 15:18,
                  lty = 1:3,
                  cex = 1,
                  col.bar = NULL,
                  n = if (is.numeric(col))  col else 4,
                  h.start = 120,
                  theme = latticeExtra::ggplot2like(n = n, h.start = h.start),
                  axis.grid = FALSE,
                  knit=FALSE,
                  #theEconomist.theme() custom.theme()
                 # reset = FALSE,
                #  show.device = FALSE,

                  ...) {
  require(lattice)
  require(latticeExtra)
  require(RColorBrewer)
  require(effects)
  # require(ggplot2)

  # Speichert die Default einstellungen zum zurücksetzen der Optionen
  if( exists( "opar" )) {
    lattice::trellis.par.set(opar)
    if( exists( "oopt" )) lattice::lattice.options(oopt)#Lattice Options fuer Grid
  }
  else{
      opar <<- lattice::trellis.par.get()
      if(!knit) lattice::trellis.device() #-- Initialisiert Projekt
  }

  if (!is.null(theme)) {
    lattice::trellis.par.set(theme)

    if (is.null(col) | is.numeric(col))
      col <- lattice::trellis.par.get()$superpose.polygon$col
    else if( is.character(col) & col=="sex")
      col<-brewer.pal(8,"Set3")[c(3,4)] # Set3 rosa-himmelblau
    cat("\nFarbe: ", paste(col, collapse=", "))
    if (is.null(col.bar))
      col.bar <- lattice::trellis.par.get()$plot.polygon$col

    lattice::trellis.par.set(
      axis.text = list(cex = 0.8, lineheight = 0.9, col = "grey20"),

      superpose.symbol = list(col = col, pch = pch),
      superpose.polygon = list(col = col, border = "transparent"),
      plot.polygon = list(col = col.bar),
      superpose.line = list(col = col, lty = lty),
      box.dot = list(pch = 19, cex = cex),
      plot.symbol = list(pch = 1)
      # box.rectangle =list(),
      # box.umbrella = list(),
    )

    if(axis.grid) oopt <<- lattice.options(ggplot2like.opts())

     cat("\nTheme set\n")

  } else {
     cat("\nno Theme\n")
  }


}
