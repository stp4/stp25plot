#' Lattice-Matrix-Plot
#'
#'
#' @param ... Variablen und Daten
#' @param ylab,xlab default ist ""
#' @param type c("histogram", "boxplot")
#' @param par.settings sefault ist  par.settings = bw_theme((farbe()))
#' @param include.n noch nicht implemeniert
#' @param cex.main,cex.scales Ueberschrift und Scales
#' @param ncol an grid.arrange
#' @param grid.arrange  logical Arrange multiple grobs on a page
#'
#' @return lattice Plot
#' @export
#'
#' @examples
#' 
#' enviro <- lattice::environmental
#' 
#' enviro <- transform(
#'   enviro,
#'   smell = cut(
#'     enviro$ozone,
#'     breaks = c(0, 30, 50, Inf),
#'     labels = c("ok", "hmmm", "yuck"),
#'     ordered = TRUE
#'   ),
#'   is.windy = factor(
#'     enviro$wind > 10,
#'     levels = c(TRUE, FALSE),
#'     labels = c("windy", "calm")
#'   ))
#' head(enviro   )
#' 
#' 
#' 
#' 
#' 
#' # marginal.plot(enviro[,1:5], data = enviro, groups = is.windy,
#' #               auto.key = list(lines = TRUE))
#' marginal_plot(enviro, ozone, radiation, is.windy, wind, smell, by=~temperature)
#' 
#' auto_plot(enviro, ozone, radiation, is.windy, wind, smell, by=~temperature)
#' 
#' auto_plot(ozone ~ radiation + is.windy+ wind+smell, enviro)
#' 
#' auto_plot(enviro, ozone[box], radiation[hist], is.windy[pie], wind, smell, temperature )
#' 
#' auto_plot(enviro, ozone, radiation, is.windy, wind, by=~smell )
#'
#' p1 <-
#' auto_plot(enviro,
#'           ozone,
#'           radiation,
#'           is.windy,
#'           wind,
#'           smell,
#'           temperature,
#'           grid.arrange = FALSE)
#' 
#' 
#' for (i in seq_along(p1)) {
#'   cat("\n",  names(p1[i]))
#'   print(p1[[i]])
#'   #  SavePlot(  names(p1[i]) , w=3.6, h=2.9)
#' }
#'
auto_plot<- function(...){
  UseMethod("auto_plot")
}


#' @rdname auto_plot
#' @export
#' 
#' 
auto_plot.lm<- function(x, ...){
 
  auto_plot(formula(terms(x)), x$model, ...)
  
}

#' @rdname auto_plot
#' @export
#' 
#'
auto_plot.default <- function(...,
                      origin = 0,
                      xlab = NULL,
                      ylab = NULL,
                      type = c("p", "r"),
                      cex.main = 1,
                      cex.scales = 0.75,
                      ncol = NULL,
                      default.scales = list(abbreviate = TRUE,
                                            minlength = 5,
                                            cex = cex.scales),
                      # relation = "free",
                      # rot = 30,tick.number = 3, 
                      # y = list(draw = FALSE)
                      layout = NULL,
                      lattice.options = list(layout.heights = list(
                        axis.xlab.padding = list(x = 0),
                        xlab.key.padding = list(x = 0)
                      )),
                      
                      col = farbe(),
                      col.bar = NULL,
                      
                      par.settings =  bw_theme(col=col, col.bar =col.bar),
                      
                      include.n = TRUE,
                      par.strip.text = NULL,
                      wrap.main=NULL,
                      bar.percent = FALSE,
                      grid.arrange = TRUE, # Arrange multiple grobs on a page
                      levels.logical = c(TRUE, FALSE),
                      labels.logical = levels.logical
                      
                      
                      ) {
  X <- stp25tools::prepare_data2(...)
  
  if (!is.null(wrap.main))
    X$row_name <- stp25tools::wrap_string(X$row_name, wrap.main)
  if (length(xlab) == 1)
    xlab <- rep(xlab, length(X$measure.vars))
  if (length(ylab) == 1)
    ylab <- rep(ylab, length(X$measure.vars))
  
  
  if (is.null(X$group.vars) |
      (length(X$group.vars) == 1) |
      (length(X$measure.vars) > length(X$group.vars))) {
    
    
    cat("\n in multi_av_plot\n")
    print(xlab)
    
    res <- multi_av_plot(
      X$data,
      X$measure.vars,
      X$group.vars,
      X$row_name,
      X$col_name,
      X$group.class,
      X$measure,
      #reorder ,plot.points, ref, cut,
      origin,
      xlab,
      ylab,
      type,
      #subset,as.table,subscripts,
      default.scales,
      lattice.options,
      par.settings,
      include.n,
      cex.main,
      layout,
      par.strip.text,
      bar.percent,
      levels.logical,
      labels.logical
    )
  }
  else{
    
    if (is.null(xlab))
      xlab <-  rep(X$col_name[1], length(X$measure.vars))
    
    cat("\n in multi_uv_plot\n")
    print(xlab )
    res <- multi_uv_plot(
      X$data,
      X$group.vars,
      X$measure.vars,
      X$col_name,
      X$row_name,
      X$measure,
      X$group.class,
      
      origin,
      xlab,
      ylab,
      type,
      
      default.scales,
      lattice.options,
      par.settings,
      include.n,
      cex.main,
      layout,par.strip.text,
      bar.percent,
      levels.logical,
      labels.logical
    )
  }
  
  
  if (grid.arrange) {
    if (length(res) > 0) {
      if (is.null(ncol))
        ncol <- ifelse(length(res) < 4, length(res),
                       ifelse(length(res) < 10, 3, 4))
      gridExtra::grid.arrange(grobs = res, ncol = ncol)
    }
    else {
      plot(1)
    }
  }
  else {
    names(res) <- X$measure.vars
    res
  }
  
}






multi_av_plot <- function(data,
                          measure.vars,
                          group.vars,
                          row_name,
                          col_name,
                          group.class,
                          measure,
                          origin,
                          xlab,
                          ylab,
                          type,
                          default.scales,
                          lattice.options,
                          
                          par.settings,
                          include.n, cex.main,layout,
                          par.strip.text,
                          bar.percent,
                          levels.logical,
                          labels.logical,
                          ...) {
  z <-  group.vars[1]
  res <- list()
  
  

   
  for (i in seq.int(length(measure.vars))) {
    
   
      
    y <- measure.vars[i]
    if (is.null(z)) {
      
      if (measure[i] == "numeric" | measure[i] == "hist" ) {
        res[[i]] <-
          lattice::histogram(
            formula(paste("~", y)),
            data,
            type = "count",
            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab[i],
            ylab = ylab[i]
          )
      }
      else if (measure[i] == "factor" | measure[i] == "bar" | measure[i] == "logical") {
        
        if( measure[i] == "logical" ) data[[y]] <- factor(data[[y]], levels.logical, labels.logical )
        tab <-  xtabs(formula(paste("~", y)), data)
        
        if (bar.percent) {
          tab <- as.data.frame(prop.table(tab,2)*100)
          if(is.null(ylab)) ylab<- "percent"
          
          }
        else tab <- as.data.frame(tab)
        
        res[[i]] <-
          lattice::barchart(
            formula(paste("Freq~", y)),
            data = tab,
            
            main = list(label=row_name[i], cex=cex.main),
            stack = FALSE,
            origin = origin,
            horizontal = FALSE,
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            layout=layout, 
            par.strip.text=par.strip.text,
            xlab = xlab[i],
            ylab = ylab[i]
          )
      }
      else if ( measure[i] =="box"){
        res[[i]] <-    
          lattice::bwplot(
            formula(paste("~", y)),
            data,
            
            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab[i],
            ylab = ylab[i]
          )
      }
      else if ( measure[i] =="pie"){
        tab <- as.data.frame(xtabs(formula(paste("~", y)), data))
        
        res[[i]] <-
          piechart(
            ~Freq,
            data = tab,
            
            main = list(label=row_name[i], cex=cex.main),
            
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab[i],
            ylab = ylab[i]
          )
      }
      else if ( measure[i] =="dot"){
        tab <- as.data.frame(xtabs(formula(paste("~", y)), data))
        
        res[[i]] <-
          lattice::dotplot(
            formula(paste("Freq~", y)),
            data = tab,
            
            main = list(label=row_name[i], cex=cex.main),
            stack = FALSE,
            origin = origin,
            horizontal = FALSE,
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab[i],
            ylab = ylab[i]
          )
      }
      else{}
    }
    else{
      if (group.class[1] == "factor") {
        
        if (measure[i] == "numeric" | measure[i] == "box") {
          
          res[[i]] <-
            lattice::bwplot(
              formula(paste(y, "~", z)),
              data,
              
              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if ( measure[i] == "hist" ) {
          res[[i]] <-
            lattice::histogram(
              formula(paste("~", y, "|", z)),
              data,
              ylab = ylab[i],
              xlab = xlab[i],
              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings
            )
        }
        else if ( measure[i]=="factor"  | measure[i] == "bar" | measure[i] == "logical" ) {
          if( measure[i] == "logical" ) data[[y]] <- factor(data[[y]], levels.logical, labels.logical )
          tab <- xtabs(formula(paste("~", y, "+", z)), data)
          if (bar.percent) {
            tab <- as.data.frame(prop.table(tab,2)*100)
            if(is.null(ylab)) ylab <- "percent"
          }
          else tab <- as.data.frame(tab)
          
        #  print(tab)
          res[[i]] <-
            lattice::barchart(
              formula(paste("Freq~", y, "|", z)),
              data = tab,
              
              main = list(label=row_name[i], cex=cex.main),
              stack = FALSE,
              origin = origin,
              horizontal = FALSE,
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              layout=layout,   par.strip.text=par.strip.text,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if ( measure[i] =="pie"){
          tab <-
            as.data.frame(xtabs(formula(paste(
              "~", y, "+", z
            )), data))
          res[[i]] <-
            piechart(
              formula(paste("~Freq|" , z)),
              data = tab,
              
              main = list(label=row_name[i], cex=cex.main),
              
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if ( measure[i] =="dot"){
          
          res[[i]] <-
            lattice::stripplot(
              formula(paste(y, "~", z)),
              data,
              
              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              panel = function(x, y, ...) {
                panel.stripplot(x, y, ..., jitter.data = TRUE)
              },
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
          
        }
        else if ( measure[i] =="dens" ){
          # noch nicht implementiert
          res[[i]] <-  densityplot(
            formula(paste("~", y)),
            data,
            outer = TRUE,
            subscripts = TRUE,
            groups = data[[ z ]],
           
            plot.points = FALSE,
            ref = TRUE,
            cut = 0,
            as.table = TRUE,
            
            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab[i],
            ylab = ylab[i], 
            ...
          )
          
          
        }
        else {}
      }
      else{
        if (measure[i] == "numeric" | measure[i] =="dot") {
        res[[i]] <-
            lattice::xyplot(
              formula(paste(y, "~", z)),
              data,
              type = type,
              
              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if (measure[i] == "factor" | measure[i] == "box") {
          res[[i]] <-
            lattice::bwplot(
              formula(paste(y, "~", z)),
              data,
              
              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if( measure[i] =="hist") {
          res[[i]] <-
            lattice::histogram(
              formula(paste("~", y, "|", z)),
              data,
              
              main = list(label=row_name[i], cex=cex.main),
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if ( measure[i] =="bar") {
          tab <-
            as.data.frame(xtabs(formula(paste(
              "~", y, "+", z
            )), data))
          res[[i]] <-
            lattice::barchart(
              formula(paste("Freq~", y, "|", z)),
              data = tab,
              
              main = list(label=row_name[i], cex=cex.main),
              stack = FALSE,
              origin = origin,
              horizontal = FALSE,
              par.settings = par.settings,
              default.scales = default.scales,
              lattice.options = lattice.options,
              layout=layout,
              xlab = xlab[i],
              ylab = ylab[i]
            )
        }
        else if ( measure[i] =="pie"){
          res[[i]] <- lattice::xyplot(y~x, 
                                      data.frame(x=1:10, y=1:10),
                                      main=list(label="pie", cex=cex.main),
                                      )
        }
        else {}
        
      }
    }
  }
  res
}


#' Hilfsfunktion
#' 
#' Im wesentlichen ist das eine Kopie von oben nur die Formeln sind vertauscht 
#' und die auswahl an verschiede Plots ist nicht möglich.
#'  @noRd
#' 
multi_uv_plot <- function(data,
                          measure.vars,
                          group.vars,
                          row_name,
                          col_name,
                          group.class,
                          measure,
                          origin,
                          xlab,
                          ylab,
                          type,
                          default.scales,
                          lattice.options,
      
                          par.settings,
                          include.n, 
                          cex.main,layout,
                          par.strip.text,
                          bar.percent,
                          levels.logical,
                          labels.logical
                          ) {
  z <-  group.vars[1]
  res <- list()
  
  
  for (i in seq.int(length(measure.vars))) {
    y <- measure.vars[i]
    ylab <- col_name[1]
    if (group.class[1] == "factor") {
      if (measure[i] == "numeric") {
        
        res[[i]] <-
          lattice::bwplot(
            formula(paste(z, "~", y)),
            data,
            
            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab,
            ylab = ylab
          )
        
      }
      else if (measure[i] == "factor" | measure[i] == "bar"| measure[i] == "logical") {
        
        if( measure[i] == "logical" ) data[[y]] <- factor(data[[y]], levels.logical, labels.logical )
        
        tab <-
          as.data.frame(xtabs(formula(paste(
            "~", z, "+", y
          )), data))
        
        
         
          if (bar.percent) tab <- as.data.frame(prop.table(tab,2)*100)
          else tab <- as.data.frame(tab)
        
        res[[i]] <-
          lattice::barchart(
            formula(paste("Freq~", z, "|", y)),
            data = tab,
            
            main = list(label=row_name[i], cex=cex.main),
            stack = FALSE,
            origin = origin,
            horizontal = FALSE,
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab,
            ylab = ylab
          )
      }
    }
    else{
      if (measure[i] == "numeric") {
        res[[i]] <-
          lattice::xyplot(
            formula(paste(z, "~", y)),
            data,
            type = type,
            
            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab,
            ylab = ylab
          )
      }
      else if (measure[i] == "factor"| measure[i] == "bar"| measure[i] == "logical") {
        if( measure[i] == "logical" ) data[[y]] <- factor(data[[y]], levels.logical, labels.logical )
        
        res[[i]] <-
          lattice::bwplot(
            formula(paste(z, "~", y)),
            data,
            
            main = list(label=row_name[i], cex=cex.main),
            par.settings = par.settings,
            default.scales = default.scales,
            lattice.options = lattice.options,
            xlab = xlab,
            ylab = ylab
          )
      }
    }
  }
  res
}



#' lattice marginal plot
#'
#' @param ... an prepare_data2
#' @param par.settings,auto.key  an lattice sefault ist  
#' par.settings = stp25output::set_lattice()
#'
#' @return lattice Plot
#' @export
#'
#' @examples
#' 
#' enviro <- environmental
#' ## make an ordered factor (so it will not be reordered)
#' enviro$smell <- cut(enviro$ozone, breaks = c(0, 30, 50, Inf),
#'                     labels = c("ok", "hmmm", "yuck"), ordered = TRUE)
#' enviro$is.windy <- factor(enviro$wind > 10,
#'                           levels = c(TRUE, FALSE), labels = c("windy", "calm"))
#' head(enviro)
#' 
#' # marginal.plot(enviro[,1:5], data = enviro, groups = is.windy,
#' #               auto.key = list(lines = TRUE))
#' marginal_plot(enviro, ozone, radiation, is.windy, wind, smell, by=~temperature)
marginal_plot <- function(...,
                          par.settings = bw_theme(farbe()),
                          auto.key = list(lines = TRUE)) {
  X <- stp25tools::prepare_data2(...)
  groups = X$data[[X$group.vars]]
  
  if (!is.factor(groups))
    groups <- cut(groups, 3)
  
  
  latticeExtra::marginal.plot(
    X$data[X$measure.vars],
    data = X$data,
    groups = groups,
    auto.key = auto.key,
    par.settings = par.settings
  )
  
}




