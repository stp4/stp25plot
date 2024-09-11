#' https://gt.rstudio.com/reference/cols_nanoplot.html

require(gt)



illness |>
  dplyr::slice_head(n = 10)


illness |>
  dplyr::slice_head(n = 10) |>
  gt(rowname_col = "test") |>
  tab_header("Partial summary of daily tests performed on YF patient") |>
  tab_stubhead(label = md("**Test**")) |>
  cols_hide(columns = starts_with("norm")) |>
  fmt_units(columns = units) |>
  cols_nanoplot(
    columns = starts_with("day"),
    new_col_name = "nanoplots",
    new_col_label = md("*Progression*")
  ) |>
  cols_align(align = "center", columns = nanoplots) |>
  cols_merge(columns = c(test, units), pattern = "{1} ({2})") |>
  tab_footnote(
    footnote = "Measurements from Day 3 through to Day 8.",
    locations = cells_column_labels(columns = nanoplots)
  )




# eigene version ----------------------------------------------------------



set.seed(1)
require(stp25plot)
require(stp25tools)
require(stp25stat2)
DF_sprk <- data.frame(
  Laborwert = gl(7, 8,
                 labels = c(
                   "Albumin", "Amylase", "Lipase",
                   "AST", "ALT","Bilirubin","C-Peptid")),
  Treat = gl(2, 4, labels = c("Control", "Treat")),
  Time = gl(4, 1, labels = c("t0", "t1", "t2", "t4")),
  x = rnorm(7 * 8)
)
DF_sprk <- transform(DF_sprk,
                     x = scale(x + as.numeric(Treat)*2 + as.numeric(Time) / 2))
DF_sprk <-  Summarise(DF_sprk, 
                                     x ~ Laborwert + Time + Treat, fun = mean, value ="x")
#names(DF_sprk)[4]<- "x"
#DF_sprk$Treat
#Wide( Laborwert+Treat ~ Time,  DF_sprk)




p1 <- sparkplot(x ~ Time | Laborwert, DF_sprk, between=1.5)

col<- c("purple", "darkgreen")

p2<- sparkplot(
  x ~ Time | Laborwert,
  DF_sprk,
  groups = Treat,
  between=1.5,
  include.labels = FALSE,
  left.padding=-5,  right.padding=3,
  col = col ,
  include.first=TRUE,
  key = list(
    corner = c(1, 1.1),
    lines = list(col = col, lwd = 2),
    cex = .75,
    columns = 2,
    text = list(levels(DF_sprk$Treat))
  )#,  char.arrows= c(down= '-', updown='', up= "+" )
)

p3 <- sparkplot(
  x ~ Time | Laborwert,
  DF_sprk,
  groups = Treat,
  type="barchart",
  between=1.5,
  include.labels = FALSE,
  left.padding=-5,  right.padding=3,
  col =  col#,  char.arrows= c(down= '-', updown='', up= "+" )
)
#windows(8,4)
require(cowplot)
plot_grid(p1,  p2,  p3,
          nrow=1,
          rel_widths = c(.7, .5, .5)
)
