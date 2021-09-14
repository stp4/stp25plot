#' Farben
#'
#' @param type,trans  Farbschema
#' @param trans A number in the interval [0, 1] indicating how transparent to make the colors. A value of 0 means no transparency and a value of 1 means completely transparency.
#' @param n,middle likert-plot 
#'
#' @return
#' @export
#'
#' @examples
#' 
#'   library("colorspace")
#' hcl_palettes(plot = TRUE)
#' diverging_hcl(10, palette = "Blue-Red")
#' hcl_palettes("qualitative", plot = TRUE)
#' hcl_palettes("sequential (single-hue)", n = 7, plot = TRUE)
#' hcl_palettes("sequential (multi-hue)", n = 7, plot = TRUE)
#' hcl_palettes("diverging", n = 7, plot = TRUE)
#' 
#' ## inspect a specific palette
#' ## (upper-case, spaces, etc. are ignored for matching)
#' hcl_palettes(palette = "Dark 2")
#' hcl_palettes(palette = "dark2")
#' 
#' 
#' barplot(GNP ~ Year, data = longley, col=farbe())
#' barplot(GNP ~ Year, data = longley, col=farbe("sex"))
#' barplot(GNP ~ Year, data = longley, col=farbe("ggplot"))
#' #barplot(GNP ~ Year, data = longley, col=farbe("x"))
#' barplot(GNP ~ Year, data = longley, col=farbe("likert.green.red", 7, middle=3))
#' barplot(GNP ~ Year, data = longley, col=farbe("likert", 5))
#' 
#' 
#' 
 
farbe <- function(type = c(
                 "pirat", "ggplot", 
                  
                  "dark", "dunkel",
                  "pastel", "hell",
                  "cb", "color.blinde",
                  "bw",
                  "sex", "sex.mf",
                  
                  "likert", 
                  "likert.red.green",  "likert.red.blue",
                  "likert.green.red", "likert.blue.red",
                  "likert.bw", 
                 "brewer"),
                  n = 5,
                  middle =   mean(1:n), #    if (is.odd(n))  NULL else (n %/% 2) + 1,
                  trans= 0) {
  if(trans < 0 | trans > 1) trans=1
  type <-  match.arg(type, several.ok = FALSE)
  
  col_pirat <- c(
          "blue1" = rgb(12, 91, 176, alpha = (1 - trans) * 255, maxColorValue = 255),
          "green" = rgb(21, 152, 61, alpha = (1 - trans) * 255, maxColorValue = 255),
          "pink" = rgb(236, 87, 154, alpha = (1 - trans) * 255, maxColorValue = 255),
          "orange" = rgb(250, 107, 9, alpha = (1 - trans) * 255, maxColorValue = 255),
          "green1" = rgb(102, 120, 64, alpha = (1 - trans) * 255, maxColorValue = 255),
          
          "red1" = rgb(185, 18, 38, alpha = (1 - trans) * 255, maxColorValue = 255),
          "blue2" = rgb(20, 155, 237, alpha = (1 - trans) * 255, maxColorValue = 255),
          "green2" = rgb(161, 199, 32, alpha = (1 - trans) * 255, maxColorValue = 255),
          "yellow" = rgb(254, 193, 11, alpha = (1 - trans) * 255, maxColorValue = 255),
          "turquoise" = rgb(22, 160, 140, alpha = (1 - trans) * 255, maxColorValue = 255),
          
          "poop" = rgb(154, 112, 62, alpha = (1 - trans) * 255, maxColorValue = 255),
          "purple2" = rgb(151, 44, 141, alpha = (1- trans) * 255, maxColorValue = 255),
          "orange2" = rgb(255, 100, 53, alpha = (1 - trans) * 255, maxColorValue = 255),
          "brown" = rgb(106, 29, 26, alpha = (1 - trans) * 255, maxColorValue = 255),
          "purple" = rgb(90, 88, 149, alpha = (1 - trans) * 255, maxColorValue = 255),
          
          "salmon" = rgb(216, 108, 79, alpha = (1 - trans) * 255, maxColorValue = 255),
          "darkgreen" = rgb(0, 106, 64, alpha = (1 - trans) * 255, maxColorValue = 255),
          "brown1" = rgb(136, 119, 95, alpha = (1 - trans) * 255, maxColorValue = 255),
          "red" = rgb(238, 0, 17, alpha = (1- trans) * 255, maxColorValue = 255),
          "green" = rgb(95, 178, 51, alpha = (1 - trans) * 255, maxColorValue = 255),
          
          "gray1" = rgb(50, 51, 55, alpha = (1 - trans) * 255, maxColorValue = 255),
          "gray2" = rgb(83, 76, 83, alpha = (1 - trans) * 255, maxColorValue = 255),
          "blue" = rgb(63, 81, 106, alpha = (1 - trans) * 255, maxColorValue = 255),
          "grayblue" = rgb(112, 140, 152, alpha = (1 - trans) * 255, maxColorValue = 255)
          )

  switch(
    type,    
    pirat = col_pirat[1:n],
    ggplot =  c( "#00BA38", "#00BFC4", "#619CFF", "#F564E3", "#F8766D", "#B79F00"),
    
    dark = RColorBrewer::brewer.pal(n, "Dark2"),
    pastel= RColorBrewer::brewer.pal(n, "Pastel1"),
    dunkel = RColorBrewer::brewer.pal(n, "Dark2"),
    hell= RColorBrewer::brewer.pal(n, "Pastel1"),
    
    cb = cbPalette[1:n],
    color.blinde = cbPalette[1:n],
    
    bw =   grey.colors(n, start = 0.4, end = 0.9),
    sex =  c( pink = "#EC579AFF", blue2 = "#149BEDFF"),
    sex.mf = c(blue2 = "#149BEDFF",  pink = "#EC579AFF"),
    
    likert = likert_col(n = n),
    likert.red.green =  likert_col(n = n, name = "RdYlGn", middle = middle),
    likert.red.blue = likert_col(n = n, name = "RdBu", middle = middle),
    likert.green.red = likert_col(n = n, name = "RdYlGn", middle = middle, rev = TRUE),
    likert.blue.red = likert_col(n = n, name = "RdBu", middle = middle, rev = TRUE),
    likert.bw = likert_col(n = n, name = "gray", middle = middle),
    
    RColorBrewer::brewer.pal(n, "Set1")
  )
  
}


# is.odd(2)
is.odd <- function(x)
  x %% 2 == 0




# copy from HH::brewer.pal.likert
my_brewer_palette <-   function(n, name = "RdBu")
  if (n <= 2) {
    bp <- RColorBrewer::brewer.pal(n = 3, name = name)
    if (n == 1)   bp[2]
    else    bp[-2]
  }  else {
    if (n <= 11)
      RColorBrewer::brewer.pal(n = n, name = name)
    else {
      if (is.odd(n))
        colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = name))(n)
      else
        colorRampPalette(RColorBrewer::brewer.pal(n = 10, name = name))(n)
    }
  }



# The palette in pieplot verwendet
#
cbPalette <- c(
  orange = "#E69F00",
  skyblue = "#56B4E9",
  green = "#009E73",
  yellow = "#F0E442",
  blue = "#0072B2",
  vermillion = "#D55E00",
  purple = "#CC79A7"
  # "#66C2A5",
  # "#FC8D62",
  # "#8DA0CB",
  # "#E78AC3",
  # "#A6D854" ,
  # "#FFD92F",
  # "#E5C494",
  # "#B3B3B3",
  # "#1B9E77" ,
  # "#D95F02",
  # "#7570B3" ,
  # "#E7298A" ,
  # "#66A61E" ,
  # "#E6AB02" ,
  # "#A6761D",
  # "#666666"
  
)

Grays   <- gray(seq(1, .6, length.out=9))
Greens  <- c("#F7FCF5", "#EEF8EA", "#E5F5E0", "#D6EFD0", "#C7E9C0", "#B4E1AD", "#A1D99B", "#8ACE88", "#74C476")
Blues   <- c("#F7FBFF", "#EAF3FB", "#DEEBF7", "#D2E3F3", "#C6DBEF", "#B2D2E8", "#9ECAE1", "#84BCDB", "#6BAED6")
Browns  <- hsv(h=0.06, s=seq(from=0, to=.6, length.out=9), v=seq(from=1, to=.85, length.out=9))
Oranges <- c("#FFF5EB", "#FEEDDC", "#FEE6CE", "#FDDBB8", "#FDD0A2", "#FDBF86", "#FDAE6B", "#FD9D53", "#FD8D3C")
Reds    <- c("#FFF5F0", "#FEEAE1", "#FEE0D2", "#FDCDB9", "#FCBBA1", "#FCA689", "#FC9272", "#FB7E5E", "#FB6A4A")
Purples <- c("#FFFBFF", "#FFF4FF", "#FFEDFF", "#FFE3FF", "#FFDAFF", "#FFCBFF", "#FFBDFF", "#FFABFF", "#FF9AFF")


likert_col <- function(n = 5,
                       name = "RdBu",
                       middle = mean(1:n),   #if (is.odd(n))  NULL else (n %/% 2) + 1,
                       middle.color =   "gray65",# "gray80",
                       min_gray = 10,
                       max_gray = 60,
                       rev = FALSE) {
  
   #   cat("\n n = ", n, "\n middle = " ,middle, " middle.color = " , middle.color, "\n")
   
  if (tolower(name) == "gray") {
    min_gray <- 1 - min_gray / 100
    max_gray <- 1 - max_gray / 100
    
    if (n %% 2 == 0) {
      n <- (n) / 2
      palette <- gray(seq(min_gray,  max_gray, length.out = n))
      palette <- c(rev(palette), palette)
    }
    else{
      n <- (n - 1) / 2 + 1
      palette <- gray(seq(min_gray,  max_gray, length.out = n))
      palette <- c(rev(palette), palette[-1])
    }
  } else {
    palette <- my_brewer_palette(n, name)
    if (rev)      palette <- rev(palette)
    if (!is.odd(n) |  is.odd(middle)){
  
      palette[middle] <- middle.color
      
      }
    
    
  }
  
  
  palette
}

# 
# farbe("likert.red.blue", 4)
# graphics.off()
# windows(7, 3)
# likertplot(Item  ~ . | Geschlecht,
#            data = Res2,
#            col = farbe("likert.red.blue", 5, 2))
# 
# 
