## Create a profile plot.
##
## This code is based on the the profile plotting code by Detlev Reymann October 2011
##
## @param  x The matrix of data to plot. Each profile item is in a column with an appropriate column name.The column names will be used as the names for the profiles in the legend.
## @param text.left text.left=NULL The text to appear on the left hand side (same length as nrow(x))
## @param text.right text.right=NULL The text to appear on the right hand side (same length as nrow(x))
## @param highlight.col highlight.col=-1 The column index of the entry to highlight. (Default, none highlighted)
## @param text.width.adjust text.width.adjust=35 The adjustment factor for the spacing for the text on the left and right of the plot. If you  need more space, make this larger. If you need less, make it smaller.
## @param colors colors=NULL,	 The vector of colors to use for the lines. Default is rainbow()
## @param draw.segments draw.segments=TRUE Whether or not horizontal segements should be drawn
## @param draw.mid draw.mid=TRUE Whether or not a vertical line should be drawn down the middle of the profile plot.
## @param plot.legend plot.legend=TRUE Whether or not to include a legend at the bottom
## @param legend.n.col legend.n.col=4 The number of columns that should be used in the legend
## @param sep sep=" - ""Aufregend - Langweilig"
## @param ... weitere Objekte nicht benutzt
##
## @return  plot
## @export
## @examples
## ## Create some random data
## set.seed(0815)
## x = matrix(rnorm(50), ncol=5)
## colnames(x) = c("A", "B", "C", "D", "E")
##
## ## Some random labels for the left and right text. These labels
## ## need to be the same length as the number of rows in x
## text.left = paste(1:nrow(x), "Left Text")
## text.right = paste(1:nrow(x), "Right Text")
##
## ## Create the profile plot. See the details in profile_plot.R for
## ## the details on the parameters
## labels<-c(
##   "Aufregend - Langweilig",
##   "Gut gemacht - Schlecht gemacht",
##   "Glaubwürdig - Unglaubwürdig",
##   "Spannend - Eintönig",
##   "Verständlich - Unverständlich",
##   "Guter Action-Schauspieler - schlechter Action Schauspieler",
##   "sympathisch - unsympathisch",
##   "talentiert - untalentiert",
##   "modisch - unmodisch",
##   "positive persönliche Einstellung - negative Einstellung gegenüber"
## )
##
##
##
## rownames(x)<- labels
## profile_plot(
##   x,
##   highlight.col=2,
##   legend.n.col=5)
##
## data <- data.frame(variable=labels,
## Control=rnorm(10),
## Treat=rnorm(10)
## )
## profile_plot(data,
##               draw.mid=FALSE
## )
##


# profile_plot <- function(x,
#                          text.left = NULL,
#                          text.right = NULL,
#                          highlight.col = -1,
#                          text.width.adjust = 35,
#                          colors = NULL,
#                          draw.segments = TRUE,
#                          draw.mid = TRUE,
#                          plot.legend = TRUE,
#                          legend.n.col = 4,
#                          sep = " - ",
#                          ...) {
#   # if (!stp25stat::all_identical2(x)) {
#   #   labels <- as.character(x[, 1])
#   #   x <- as.matrix(x[, -1])
#   #   rownames(x) <- labels
#   # }
#   # print(x)
#   if (is.null(text.left) & is.null(text.right)) {
#     if (!is.null(rownames(x)))
#       labels <- strsplit(rownames(x), sep)
#     text.left <- sapply(labels, "[", 1)
#     text.right <- sapply(labels, "[", 2)
#     
#     #text.left<-c()
#     # text.right<-c()
#       }
#   
#   ## Find the min and max for the x-axis
#   min.x = floor(min(x))
#   max.x = ceiling(max(x))
#   mid.x = (max.x + min.x) / 2
#   
#   ##
#   ## Configure the graphics output
#   ##
#   
#   ## Keep the old par() setting around so that we can reset them at the end
#   par.default = par(no.readonly = TRUE)
#   
#   ## create new window for output
#   plot.new()
#   
#   ## Outer margin:
#   par(oma = c(0, 0, 0, 0))
#   
#   ## letter size
#   # par(ps=14)
#   
#   ## First discover text largest in size to set dimensions
#   ## for text and graph
#   left.text.width = max(strwidth(text.left))
#   right.text.width = max(strwidth(text.right))
#   
#   ## I (Detlev Reymann) found this multiplication factor just by trail
#   ## and error has to be adjusted for longer text!!
#   left.text.width = left.text.width * text.width.adjust
#   right.text.width = right.text.width * text.width.adjust
#   
#   ## Define area for drawing the graph (Text is not part of the graph)
#   ## par(mar=c(bottom, left, top, right))
#   par(mar = c(0.5, left.text.width, 0.5, right.text.width))
#   
#   ## Calculating the size for the caption
#   ## three names in one line
#   if (plot.legend) {
#     line.height = 0.66
#     caption.height = -((nrow(x) %% 3) + 1) * line.height
#     plot.window(xlim = c(min.x, max.x),
#                 ylim = c(caption.height, nrow(x)))
#   } else {
#     plot.window(xlim = c(min.x, max.x), ylim = c(0, nrow(x)))
#   }
#   
#   ##
#   ## Let's do some plotting
#   ##
#   
#   ## Configure Y-axis
#   y = 1:nrow(x)
#   
#   ## Draw a dotted line for each item
#   if (draw.segments) {
#     segments(min.x, y, max.x, y, lty = "dotted", col = "gray")
#   }
#   
#   ## Draw a line for the average/neutral value
#   if (draw.mid) {
#     print(draw.mid)
#     lines(c(mid.x, mid.x),
#           c(1, nrow(x)),
#           lwd = 3,
#           col = "gray")
#   }
#   
#   ## The set of colors to use (if none provided)
#   if (is.null(colors)) {
#     colors = rainbow(ncol(x))
#   }
#   
#   ## If there is some highlighting to be done, make the default line
#   ## dashed
#   if (highlight.col > 0) {
#     par(lwd = 1, lty = "dashed")
#   }
#   
#   ## Loop over all profiles (columns)
#   for (i in 1:ncol(x)) {
#     #	print(c)
#     ## In most cases we want one of the objects to be highlighted
#     ## in the graph, my own brand, the reference object etc.
#     if (highlight.col == i) {
#       par(lwd = 3, lty = 1)
#     } else {
#       par(lwd = 1, lty = "dashed")
#     }
#     
#     ## Draw lines and symbols for each object
#     for (j in nrow(x):1) {
#       points(x[j, i], j, col = colors[i], pch = 19)
#       
#       ## Connect with lines
#       if (j < nrow(x)) {
#         lines(c(x[(j + 1), i], x[j, i]), c((j + 1), j), col = colors[i])
#       }
#     }
#   }
#   
#   ## Display Text on the left
#   mtext(
#     rev(text.left),
#     at = y,
#     adj = 1,
#     side = 2,
#     las = 2
#   )
#   
#   ## Display Text on the right
#   mtext(
#     rev(text.right),
#     at = y,
#     adj = 0,
#     side = 4,
#     las = 2
#   )
#   
#   ##
#   ## Draw x-axis
#   ##
#   
#   ## Set ticks for x-axis
#   ticks = c(round(min.x, 3),
#             round(((min.x + mid.x) / 2), 3),
#             round(mid.x, 3),
#             round(((mid.x + max.x) / 2), 3),
#             round(max.x, 3))
#   
#   par(cex.axis = 0.5, mex = 0.5)
#   axis(1,
#        at = ticks,
#        labels = ticks,
#        pos = 0.5)
#   
#   ##
#   ## Add in the legend
#   ##
#   
#   if (plot.legend) {
#     legend(
#       "bottom",
#       legend = colnames(x),
#       pch = 19,
#       col = colors,
#       bg = NA,
#       box.col = NA,
#       ncol = legend.n.col
#     )
#   }
#   
#   ## Reset all of the par() settings to what they were before we started
#   par(par.default)
# }

## Balken sind nicht Sinnvoll da das mit lattice einfacher geht##
## bzw mit  panel.barchart.value 

# bar_plot <- function(formula, data,
#                     main="", xlab=NULL, ylab=NULL,
# 
#                     xlim=NULL, ylim=NULL,
#                     col=NA,
#                     percent=TRUE,
#                     digits=2, #pos= 3,
#                     suffix="",
#                     type=1, #--Saeulendiagramm
#                     groups=NULL,
#                     margin = NULL,
#                     value=TRUE,
#                     fun=function(x) mean(x, na.rm=TRUE),
#                     pos=NULL,
#                     pos_panel= if(is.null(pos)) {
#                         if(type==1 | type=="vertikal" ) 3
#                         else 4
#                     }else  pos ,
#                     var_names= Formula_Names(formula, data),
#                     panel=function(...)   {
#                         panel.barchart(...)
#                         if(value) panel.barchart.value(..., digits=digits, pos=pos_panel, suffix=suffix)
#                     },
#                     ...){
# require(lattice)
#     #  print(var_names)
#     #  print(is_all_identical2(data[ var_names$yname]))
#     if(!is_all_identical2(data[ var_names$yname])) {
#         cat("\nDas Skalenniveau ist gemischt! und daher hann nichts berechnet werden\n")
#         return(str(data[, var_names$yname]))
#     }else{
#         if(all(sapply(data[var_names$yname], is.factor))){
# 
#             #
#             #print("Factor")
#             # print(var_names$yname )
#             if(!is.null(var_names$xname)) {
#                 if(is.null(margin)) margin<-1
#                 formula <- formula(paste( "~",
#                                           paste(c(var_names$xname, var_names$yname) , collapse="+")
# 
#                 ))}
# 
#             xtb <- xtabs(formula, data)
#             xdata <- data.frame( xtb )
#             xnames <- names(xdata)[-ncol(xdata) ]
#             prcdata <-   data.frame(prop.table(xtb, margin = margin)*100)
#             n <- ncol(prcdata )
#             x_data <- cbind(prcdata[-n], x__n= data.frame(xtb)[,n], x__Freq=prcdata[,n])
#             #  print(x_data)
#             vars  <- names(x_data)[1:(n-1)]
#             xylim <- extendrange(c(0, x_data$x__Freq)) + c(0, 5)
#             orgn  <- 0
#             xylab <- "Prozent"
#             suffix <- "%"
# 
#         }else{
#             # print("gggg")
#             m_data <- Recast2(formula, data, fun=fun)
#             n_data <- Recast2(formula, data, fun=function(x) length(na.omit(x)))
#             n <- ncol(m_data)
#             names(m_data)[n-1] <- "x__variable"
#             x_data <- cbind( m_data[-n], x__n=n_data[,n], x__Freq= m_data[,n])
# 
#             x_data<- x_data[c("x__variable", names(x_data)[-which(names(x_data) %in% "x__variable" )])]
#             vars <- names(x_data)[1:(n-1)]
#             rng <- range(data[var_names$yname], na.rm=TRUE)
#             xylim <- extendrange(rng , f= 0.01) #    c(0,  x_data$x__Freq))
#             orgn <- rng[1]
#             xylab <- "Mittelwert"
#         }
#         if(length(vars)>1){
# 
#             groups_name <-  if(is.null(substitute(groups))) vars[2]
#             else substitute(groups)
#             # cat("\ngroups: ", groups_name, class(groups_name), "\n")
#             if(groups_name=="F") warning("Achtung bei Gruppe mit Namen  F besteht verwechslungsgefahr besser FALSE verwenden")
# 
#             if(!is.logical(groups_name)) {
#                 groups_name<- as.character(groups_name)
# 
#                 groups <- x_data[, groups_name]
#                 vars <- vars[-which(vars %in% groups_name)]
#             }
#             else {
#                 groups=NULL
# 
# 
#             }
#         }
# 
# 
#         if(type==1 | type=="vertikal"){
#             if(length(vars)==1) fm <- formula(paste( "x__Freq~", vars[1]))
#             else if(length(vars)==2 & is.null(groups)) fm <- formula(paste( "x__Freq~", vars[2] ))
#             else fm <- formula(paste("x__Freq~",vars[1], "|", paste(vars[-1], collapse="+")))
#             if(is.null(ylim)) ylim <-xylim
#             if(is.null(ylab))   ylab <- xylab
#             print(fm)
#             print(vars)
#             p1 <- barchart(fm, x_data, groups=groups,
#                            origin=orgn,
#                            ylim= ylim, ylab=ylab, main=main,
#                            panel=panel, ...)
# 
# 
#         } else {
#             if(length(vars)==1) fm <- formula(paste(vars[1], "~x__Freq"))
#             else if(length(vars)==2 & is.null(groups)) fm <- formula(paste(vars[2] , "~x__Freq"))
#             else fm <- formula(paste(vars[1],"~x__Freq", "|", paste(vars[-1], collapse="+")))
#             if(is.null(xlim)) xlim <-xylim
#             if(is.null(xlab))   xlab <- xylab
# 
#             p1 <- barchart(fm, x_data, groups=groups,
#                            origin=orgn,
#                            xlim= xlim, xlab=xlab, main=main,
#                            panel=panel, ...)
# 
# 
#         }
#         print(p1)
#         return(x_data)
#     }
# }
# 
# 
# 
# 
# balken <- function(x,
#                    data,
#                    main = "",
#                    ylab = "",
#                    xlab = "",
#                    cex.x = 1.5,
#                    cex.y = 1.3,
#                    cex.ltext = 1.1,
#                    settings = set_lattice_ggplot(col.bar = cbPalette['blue']),
#                    margin = 2,
#                    include.value = FALSE,
#                    auto.key = list(space = "bottom", columns = 2),
#                    orientation = NULL,
#                    wrap=NULL,
#                    ...) {
#     if(!is.null(wrap)) main <- wrap_sentence(main, wrap)
#     
#     n_vars <- length(all.vars(x))
#     lhs <- all.vars(x)[1L]
#     rhs <-  all.vars(x)[2L]
#     chs <-  all.vars(x)[3L]
#     
#     if (n_vars == 1) {
#         dat <- to_table(x, data)
#         if (is.null(orientation))
#             orientation <- 1
#         
#         
#         if (orientation == 1) {
#             fm <- formula(paste0(lhs, "~ Percent"))
#             lattice::barchart(
#                 fm,
#                 dat,
#                 main = main,
#                 origin = 0,
#                 xlab = list(label = xlab, cex = cex.x),
#                 scales = list(y = list(cex = cex.y)),
#                 par.settings = settings,
#                 panel = function(x, y, ...) {
#                     lattice::panel.barchart(x, y, ...)
#                     if (include.value) {
#                         prz <- stp25rndr::rndr_percent(dat$Percent, dat$Freq)
#                         
#                         ltext(
#                             x = .4,
#                             y = y,
#                             adj = c(0, NA),
#                             col = "white",
#                             labels = prz,
#                             cex = cex.ltext
#                         )
#                     }
#                 },
#                 ...
#             )
#         }
#         else{
#             fm <- formula(paste0("Percent ~ ", lhs))
#             lattice::barchart(
#                 fm,
#                 dat,
#                 main = main,
#                 origin = 0,
#                 ylab = list(label = ylab, cex = cex.y),
#                 scales = list(x = list(cex = cex.x)),
#                 par.settings = settings,
#                 panel = function(x, y, ...) {
#                     lattice::panel.barchart(x, y, ...)
#                     if (include.value) {
#                         prz <- stp25rndr::rndr_percent(dat$Percent, dat$Freq)
#                         ltext(
#                             x = x,
#                             y = y - 5,
#                             adj = c(NA, 0),
#                             col = "white",
#                             labels = prz,
#                             cex = cex.ltext
#                         )
#                     }
#                 },
#                 ...
#             )
#         }
#         
#     }
#     else if (n_vars == 2) {
#         dat <- to_table(x, data, margin = margin)
#         names(dat)[2] <- "groups"
#         
#         fm <- formula(paste("Percent ~ ", lhs))
#         lattice::barchart(
#             fm,
#             dat,
#             groups = groups,
#             origin = 0,
#             ylab = ylab,
#             main = main,
#             par.settings = settings,
#             auto.key = auto.key,
#             panel = function(x, y, ...) {
#                 lattice::panel.barchart(x, y, ...)
#                 if (include.value) {
#                     prz <- stp25rndr::rndr_percent(dat$Percent, dat$Freq)
#                     ltext(
#                         x = x,
#                         y =  y - 5,
#                         adj = c(NA, 0),
#                         col = "white",
#                         labels = prz,
#                         cex = cex.ltext
#                     )
#                 }
#                 
#             },
#             ...
#         )
#         
#     }
#     else if (n_vars == 3) {
#         list(n_vars, lhs, rhs, chs)
#         
#     }
#     else{
#         list(n_vars, NULL, NULL, NULL)
#     }
# }



## Modifiziert effect-Plot
##
## Soll nur im Notfall geladen werden geht nicht Mehr
## 
## @param x Model fit
## @param selection,rows,cols,ask,graphics alles nicht ändern
## @param xlab  xlabs Ändern
## @param ...
##
## @return Plot
##
## @examples
## library(effects)
## library(gridExtra)
## A = rnorm(100)
## B = rnorm(100)
## C = factor(rep(c("This", "That"), 50))
##
## #-- Modifiziert wegen xlab
##
## plot.efflist <- stp25:::plot.efflist
##
## ef <- allEffects(lm(A ~ B + C))
## plot(ef, xlab = c("Foo", "Bar"))

# plot.efflist <- function(x, selection,
#                                  rows, cols,
#                                  ask=FALSE, # nicht vom mir benutzt
#                                  graphics=TRUE,
#                                  lattice,
#                                  xlab,
#                                  ...){
#   # Next line added 8/23/17 along with lattice, also lattice arg above
#   lattice <- if(missing(lattice)) list() else lattice
#   if (!missing(selection)){
#     if (is.character(selection)) selection <- gsub(" ", "", selection)
#     return(plot(x[[selection]], ...))
#   }
#   effects <- gsub(":", "*", names(x))
#   # if (ask){
#   #   repeat {
#   #     selection <- menu(effects, graphics=graphics, title="Select Term to Plot")
#   #     if (selection == 0) break
#   #     else print(plot(x[[selection]], ...))
#   #   }
#   # }
#  #  else {
# 
#   #effects:::mfrow
#     neffects <- length(x)
#     mfrow <- effects:::mfrow(neffects)
#     if (missing(rows) || missing(cols)){
#       rows <- mfrow[1]
#       cols <- mfrow[2]
#     }
#     for (i in 1:rows) {
#       for (j in 1:cols){
#         if ((i-1)*cols + j > neffects) break
#         more <- !((i-1)*cols + j == neffects)
#         lattice[["array"]] <- list(row=i, col=j, nrow=rows, ncol=cols, more=more)
# 
#         if (missing(xlab)) {
#           print(plot(x[[(i - 1) * cols + j]], lattice = lattice, ...))
#         }  else {
#           k <- (i - 1) * cols + j
#           x_lab <-
#             if (is.null(names(xlab)))
#               xlab[k]
#           else
#             xlab[names(x[k])]
# 
#           print(plot(x[[k]], lattice = lattice, xlab = x_lab, ...))
# 
#         }
#       }
#     }
#   #}
# }




