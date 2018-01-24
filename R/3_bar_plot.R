#' @rdname bar_plot
#' @export
#'
#'
bar_plot <- function(formula, data,
                    main="", xlab=NULL, ylab=NULL,

                    xlim=NULL, ylim=NULL,
                    col=NA,
                    percent=TRUE,
                    digits=2, #pos= 3,
                    suffix="",
                    type=1, #--Saeulendiagramm
                    groups=NULL,
                    margin = NULL,
                    value=TRUE,
                    fun=function(x) mean(x, na.rm=TRUE),
                    pos=NULL,
                    pos_panel= if(is.null(pos)) {
                        if(type==1 | type=="vertikal" ) 3
                        else 4
                    }else  pos ,
                    var_names= Formula_Names(formula, data),
                    panel=function(...)   {
                        panel.barchart(...)
                        if(value) panel.barchart.value(..., digits=digits, pos=pos_panel, suffix=suffix)
                    },
                    ...){
require(lattice)
    #  print(var_names)
    #  print(is_all_identical2(data[ var_names$yname]))
    if(!is_all_identical2(data[ var_names$yname])) {
        cat("\nDas Skalenniveau ist gemischt! und daher hann nichts berechnet werden\n")
        return(str(data[, var_names$yname]))
    }else{
        if(all(sapply(data[var_names$yname], is.factor))){

            #
            #print("Factor")
            # print(var_names$yname )
            if(!is.null(var_names$xname)) {
                if(is.null(margin)) margin<-1
                formula <- formula(paste( "~",
                                          paste(c(var_names$xname, var_names$yname) , collapse="+")

                ))}

            xtb <- xtabs(formula, data)
            xdata <- data.frame( xtb )
            xnames <- names(xdata)[-ncol(xdata) ]
            prcdata <-   data.frame(prop.table(xtb, margin = margin)*100)
            n <- ncol(prcdata )
            x_data <- cbind(prcdata[-n], x__n= data.frame(xtb)[,n], x__Freq=prcdata[,n])
            #  print(x_data)
            vars  <- names(x_data)[1:(n-1)]
            xylim <- extendrange(c(0, x_data$x__Freq)) + c(0, 5)
            orgn  <- 0
            xylab <- "Prozent"
            suffix <- "%"

        }else{
            # print("gggg")
            m_data <- Recast2(formula, data, fun=fun)
            n_data <- Recast2(formula, data, fun=function(x) length(na.omit(x)))
            n <- ncol(m_data)
            names(m_data)[n-1] <- "x__variable"
            x_data <- cbind( m_data[-n], x__n=n_data[,n], x__Freq= m_data[,n])

            x_data<- x_data[c("x__variable", names(x_data)[-which(names(x_data) %in% "x__variable" )])]
            vars <- names(x_data)[1:(n-1)]
            rng <- range(data[var_names$yname], na.rm=TRUE)
            xylim <- extendrange(rng , f= 0.01) #    c(0,  x_data$x__Freq))
            orgn <- rng[1]
            xylab <- "Mittelwert"
        }
        if(length(vars)>1){

            groups_name <-  if(is.null(substitute(groups))) vars[2]
            else substitute(groups)
            # cat("\ngroups: ", groups_name, class(groups_name), "\n")
            if(groups_name=="F") warning("Achtung bei Gruppe mit Namen  F besteht verwechslungsgefahr besser FALSE verwenden")

            if(!is.logical(groups_name)) {
                groups_name<- as.character(groups_name)

                groups <- x_data[, groups_name]
                vars <- vars[-which(vars %in% groups_name)]
            }
            else {
                groups=NULL


            }
        }


        if(type==1 | type=="vertikal"){
            if(length(vars)==1) fm <- formula(paste( "x__Freq~", vars[1]))
            else if(length(vars)==2 & is.null(groups)) fm <- formula(paste( "x__Freq~", vars[2] ))
            else fm <- formula(paste("x__Freq~",vars[1], "|", paste(vars[-1], collapse="+")))
            if(is.null(ylim)) ylim <-xylim
            if(is.null(ylab))   ylab <- xylab
            print(fm)
            print(vars)
            p1 <- barchart(fm, x_data, groups=groups,
                           origin=orgn,
                           ylim= ylim, ylab=ylab, main=main,
                           panel=panel, ...)


        } else {
            if(length(vars)==1) fm <- formula(paste(vars[1], "~x__Freq"))
            else if(length(vars)==2 & is.null(groups)) fm <- formula(paste(vars[2] , "~x__Freq"))
            else fm <- formula(paste(vars[1],"~x__Freq", "|", paste(vars[-1], collapse="+")))
            if(is.null(xlim)) xlim <-xylim
            if(is.null(xlab))   xlab <- xylab

            p1 <- barchart(fm, x_data, groups=groups,
                           origin=orgn,
                           xlim= xlim, xlab=xlab, main=main,
                           panel=panel, ...)


        }
        print(p1)
        return(x_data)
    }
}





