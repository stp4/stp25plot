% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparkline.R
\name{panel.sparkline}
\alias{panel.sparkline}
\alias{panel.sparkbar}
\alias{sparkplot}
\alias{sparkplot2}
\title{sparkplot}
\usage{
panel.sparkline(
  x,
  y,
  ...,
  z.name,
  digits = 2,
  cex.label = 1,
  cex.numbers = 0.7,
  cex.arrows = NULL,
  cex.points = 0.8,
  lim.arrows = 0.25,
  pch = NULL,
  pch.default = 16,
  col.default = "gray40",
  col.max.min = c("blue", "red"),
  include.labels = TRUE,
  include.arrows = TRUE,
  include.max = TRUE,
  include.min = TRUE,
  include.first = FALSE,
  include.last = FALSE,
  include.box = TRUE
)

panel.sparkbar(
  x,
  y,
  ...,
  z.name,
  digits = 2,
  cex.label = 1,
  cex.numbers = 0.7,
  cex.arrows = NULL,
  cex.points = 0.8,
  lim.arrows = 0.25,
  include.labels = TRUE,
  include.arrows = TRUE,
  include.max = TRUE,
  include.min = TRUE,
  include.first = FALSE,
  include.last = FALSE
)

sparkplot(
  x,
  data,
  lwd = 2,
  lty = NULL,
  col = NULL,
  pch = NULL,
  type = "l",
  scales,
  strip,
  layout,
  ylab = "",
  xlab = "",
  digits = 3,
  between = 1,
  right.padding = 4,
  left.padding = 1.5,
  include.labels = TRUE,
  include.arrows = TRUE,
  include.axis = FALSE,
  include.max = TRUE,
  include.min = include.max,
  include.first = FALSE,
  include.last = include.first,
  include.box = TRUE,
  ...
)

sparkplot2(
  x,
  data,
  lwd = 2,
  lty = NULL,
  col = NULL,
  pch = NULL,
  type = "barchart",
  scales,
  strip,
  layout,
  ylab = "",
  xlab = "",
  digits = 3,
  between = 1,
  right.padding = 4,
  left.padding = 1.5,
  include.labels = TRUE,
  include.arrows = TRUE,
  include.axis = FALSE,
  include.max = TRUE,
  ...
)
}
\arguments{
\item{x, y}{from panel}

\item{z.name}{names of levels}

\item{digits}{number to signif}

\item{cex.label, cex.numbers, cex.arrows, cex.points}{size}

\item{pch}{graphical parameters used in auto.key}

\item{include.labels}{lables}

\item{include.arrows, lim.arrows}{reggression direction as arrow}

\item{include.max}{number}

\item{lwd, lty, col}{graphical parameters}

\item{type}{"l"}

\item{scales, strip, layout}{not used}

\item{ylab, xlab}{character labels}

\item{right.padding, left.padding}{number links rechts abstand}

\item{x, data, ...}{to xyplot()}
}
\value{
lattice
}
\description{
Stolen from http://www.motioninsocial.com/tufte/#sparklines
}
\examples{

 set.seed(1)

DF <- data.frame(
  Laborwert = gl(7, 8,
                 labels = c(
                   "Albumin", "Amylase", "Lipase",
                   "AST", "ALT","Bilirubin","C-Peptid")),
  Treat = gl(2, 4, labels = c("Control", "Treat")),
  Time = gl(4, 1, labels = c("t0", "t1", "t2", "t4")),
  x = rnorm(7 * 8)
)
DF <- transform(DF,
                x = scale(x + as.numeric(Treat)*2 + as.numeric(Time) / 2))
DF1 <- stp25tools::Summarise(DF, x~ Laborwert+ Time, fun=mean )

names(DF1)[4]<- "x"
#         )
DF1[21,4]<-1

sparkplot(
  x ~ Time | Laborwert,
  DF1,
  between = 1.5,
  
  
  include.axis=TRUE,
  pch = 19,
  col="green",
  include.first = TRUE,
  #  as.table=FALSE,
  include.max = TRUE,
  include.labels=TRUE
)  
require(grid)

grid.text("A", .02, .95, gp=gpar(fontsize=20))
grid.text("B", .02, .45, gp=gpar(fontsize=20))
grid.lines(x = unit(c(.1, .95), "npc"),
           y = unit(c(.065, 0.065), "npc"),
           arrow =arrow(angle = 30, length = unit(0.1, "inches"),
                        ends = "last", type = "open"))

 set.seed(1)

DF <- data.frame(
  Laborwert = gl(7, 8,
                 labels = c(
                   "Albumin", "Amylase", "Lipase",
                   "AST", "ALT","Bilirubin","C-Peptid")),
  Treat = gl(2, 4, labels = c("Control", "Treat")),
  Time = gl(4, 1, labels = c("t0", "t1", "t2", "t4")),
  x = rnorm(7 * 8)
)
DF <- transform(DF,
                x = scale(x + as.numeric(Treat)*2 + as.numeric(Time) / 2))
DF1 <- stp25tools::Summarise(DF, x~ Laborwert+ Time, fun=mean )
names(DF1)[4]<- "x"

#: "p", "l", "h", "b", "o", "s", "S", "r", "a", "g"
p1 <- sparkplot(x ~ Time | Laborwert, DF1, between=1.5)
col<- c("purple", "darkgreen")
p2<- sparkplot(
  x ~ Time | Laborwert,
  DF,
  groups = Treat,
  between=1.5,
  include.labels = FALSE, 
  left.padding=-5,  right.padding=3,
  col = col ,
  key = list(
    corner = c(1, 1.1),
    lines = list(col = col, lwd = 2),
    # title="CIT",
    cex = .75,
    columns = 2,
    text = list(levels(DF$Treat))
  )
)

p3 <- sparkplot(
  x ~ Time | Laborwert,
  DF,
  groups = Treat,
  type="barchart",
  between=1.5,
  include.labels = FALSE, 
  left.padding=-5,  right.padding=3,
  col =  col
)
#windows(8,4)
require(cowplot)
plot_grid(p1,  p2,  p3,
          nrow=1,
          rel_widths = c(1, .5, .5)
)

}
