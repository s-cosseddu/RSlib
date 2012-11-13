###################################################################################
#: Title       : qplot.ts.R
#: Author      : Salvatore Cosseddu - S.M.Cosseddu@warwick.ac.uk                  #
#: Institution : University of Warwick - Centre for Scientific Computing and	  #
#                                        School of Engineering			  #
#: Description : Alternative version of plot.ts using ggplot2
#: Written on Tuesday, 13 November 2012.
#: Version     : 0.01								  #
# 										  #
#										  #
#  COPYRIGHT					       				  #
#  Copyright © 2012.Salvatore Cosseddu		       				  #
#  Centre for Scientific Computing, University of Warwick.		       	  #
#  License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>. #
#  This is free software: you are free to change and redistribute it.         	  #
#  There is NO WARRANTY, to the extent permitted by law.          		  #
#										  #
###################################################################################


qplot.ts <- function(x, window.length=NULL,var.name="variable", val.name="value",
                     smoothonly=FALSE, facet=T, facetscale="free", title=NULL,
                     xlab="x", ylab="y", xlim=NULL, ylim=NULL, ref=NULL,
                     legend=F, add=NULL, ts.names=NULL, ...) {

  require(ggplot2)
  require(reshape2)
  # x is considered a ts 
  x <- as.ts(x)
  
  ## getting x properties
  singleseries <- FALSE
  if (!is.null(colnames(x))) {
    if (is.null(ts.names)) {
      ##       #multiple series
      ts.names <- colnames(x)
    }  else {
      colnames(x) <- ts.names
    }
    n.series <- ncol(x)
    print(paste("working with", ts.names))
  } else {
    ##single ts, I'm not sure how to deal with it.
    singleseries <- TRUE
    n.series <- 1
  }
  
  dt <- deltat(x)
  first <- start(x)

  ## checking wheter smoothing trajectory is required
  ## it depends if window.length is declared
  if (is.null(window.length)) {
    print.smooth <- FALSE
  } else {
    print.smooth <- TRUE
  }

  if (!print.smooth && smoothonly) {
    stop("qplot.ts error: if smoothonly is T, window.length must be passed")
  }
  
  ## creating smooth trajectory
  if (print.smooth) {
    ## TODO add function for this
    ## filtering the trajectory with moving averages method
    k <- rep(1/window.length, window.length) #filter
    x.ma <- as.matrix(na.omit(filter(x,k,sides=2)))  #filtered traj
    ## using a matrix and modifing the name to fix the bug in the case of 
    ## x being a simple vector (ver 0.5)
    colnames(x.ma) <- ts.names
    
    ## creating a new ts with the filtered traj
    x.ts.ma <- ts(x.ma, deltat=dt ,start=first+(window.length/2)*dt)
  }
    
  ## ####################
  ## ggplot2
  ## reshaping for ggplot2, that needs well shaper data.frames
  x.ts.df <- melt(data.frame(time=as.numeric(time(x)), x), id.vars="time", variable.name=var.name, value.name=val.name)
  if (print.smooth) {
    x.ts.ma.df <- melt(data.frame(time=as.numeric(time(x.ts.ma)), x.ts.ma), id.vars="time", variable.name=var.name, value.name=val.name)
  }
  
  ## in the case of a single ts, rename the variable according to ts.names
  if (!is.null(ts.names) && singleseries ) {
    x.ts.df[,var.name] <- ts.names

    if (print.smooth) {
      x.ts.ma.df[,var.name] <- ts.names
    }
  }
  
    ## value name is <value.name>.ma
#  x.ts.ma.df <- melt(data.frame(time=as.numeric(time(x.ts.ma)), x.ts.ma), id.vars="time", variable.name=variable.name, value.name=paste(value.name,"ma",sep="."))

  ## plotting
  ## adding first ggplot layer
  if (is.null(add)) {
    x.ts.plt <- ggplot(x.ts.df)
  } else {
    x.ts.plt <- add
  }


  ###################################################################
  ##  plot ts
  ## if smooth line is present, the coloured part is the smoothed line
  if (!smoothonly && print.smooth) {
    x.ts.plt <- x.ts.plt + geom_line(data=x.ts.df, aes_string(x="time", y=val.name, group = var.name))  #all points
  }

  if (print.smooth) {
    x.ts.plt <- x.ts.plt + geom_line(data=x.ts.ma.df, aes_string(x="time", y=val.name, colour=var.name),...)
  } else {
    x.ts.plt <- x.ts.plt + geom_line(data=x.ts.df, aes_string(x="time", y=val.name, colour=var.name),...)
  }

  #########################################################################

  
    ## creating subplot if required (facet=TRUE)
  facetformula <- paste(var.name,".", sep=" ~ ")
  if (facet) { x.ts.plt <-  x.ts.plt + facet_grid(as.formula(facetformula), scales=facetscale) }
  
  ## if a reference exists
  if (!is.null(ref)) {
    if ( !is.numeric(ref) ) {
      if (ref == "mean") {
        ref <- mean(as.vector(x))
        print(paste("the mean is", ref))
      } else {
        stop ("Error in mean definition, a number or mean are allowed")
      }
    }
    
    ref.l <- length (ref)
    if ( ref.l == 1) {
      ## if only one data is provided a data.frame the same size of x is built
      ref <- as.data.frame(matrix (rep(ref, n.series), ncol = n.series))
    } else if (ref.l == n.series) {
      ## ref is the same length as x
      ref <- as.data.frame (t(ref))
    } else {
      stop("ref has to be length 1 or ncol(x)")
    }

    names(ref) <- paste(sub(" ",".",ts.names))
    print (ref)
    mean.set <- melt(ref, value.name="ref", variable.name=var.name)
    print (mean.set)
    print("using reference values:")
    print(mean.set)
  
    x.ts.plt <- x.ts.plt + geom_hline(data=mean.set, aes_string(yintercept="ref", colour=var.name), size=2, linetype="dashed", alpha=1) #, col="green"

  }

  ## labelling the plot
  x.ts.plt <- x.ts.plt + labs(list(title = title, x = xlab, y = ylab))

  ## applying limits
  if (!is.null(xlim)) {
    x.ts.plt <- x.ts.plt + xlim(xlim)
  }

  if (!is.null(ylim)) {
    x.ts.plt <- x.ts.plt + ylim(ylim)
  }

  if (!legend) {
    x.ts.plt <- x.ts.plt + theme(legend.position="none")
  }
  
  return(x.ts.plt)

  
}


## qplot.ts                 package:clib                  

## Plotting Time-Series Objects using ggplot2

## Description:

##      plot.ts alternative that uses gglot2

## Usage:

##     qplot.ts (x, var.name="variable", val.name="value", window.length=NULL,
##                  smoothonly=FALSE, facet=FALSE, facetscale="free", title=NULL,
##                  xlab="x", ylab="y", xlim=NULL, ylim=NULL, ref=NULL,
##                  legend=TRUE, add=NULL, ts.names=NULL, ...) {
##
     
## Arguments:

## x: time series objects or data.frame.

## var.name: a definition for the data sets, it will return in the legend.

## val.name: a definition for the values in the data sets.

## window.length: integer, if present the ts will be plotted in black, with a 
##                colored smoothed line. The smoothing procedure is moving averages.

## smoothonly=logical, indicating if only the smooth lines should be drawn.

## facet: logical, for multivariate time series, should the series by plotted
##        separately (with a common time axis) or on a single plot?

## facetscale: it is the scale value for the facet y axes, directly passed to gglot2;
##             from ggplot2 manual:
##             Are scales shared across all facets ("fixed"),
##             or do they vary across rows ("free_x"), columns ("free_y"),
##             or both rows and columns ("free")

## title: plot title

## xlab, ylab: axis labels.

## xlim, ylim: axis ranges. 

## ref: vector, contains reference values that will be plotted as dashed lines.

## legend: logical, plot legend,

## add: if a ggplot object is provided (for instance a preview output from qplot.ts)
##      the new layers will be added to it. 

## ts.names: vector, contains the names of each data sets. If not provided, the names from
##           colnames(x) will be used. It will return in the legend 
## ... : additional arguments to be passed to ggplot (see ggplot manual)

## See Also:

##      ‘ts’ for basic time series construction and access functionality.

## Examples:

 

## working on a ts on different facets
## gggg <- ts (
##   cbind (rnorm(2000, 10, 0.2),
##          rnorm(2000, 9, 0.2)), deltat=0.01
##   )

## qplot.ts( x=gggg, window.length=20,var.name="Friends", facet=TRUE, facetscale="fixed", title="test", xlab="r", ylab="gr", xlim=NULL, ylim=NULL, legend=TRUE, add=NULL, ts.names=c("mice","cats"))


## qplot.ts( x=gggg, window.length=21,var.name="Friends", facet=FALSE, facetscale="fixed", title="test", xlab="r", ylab="gr", xlim=NULL, ylim=NULL, legend=FALSE, add=NULL, ts.names=c("mice","cats"))

## colnames(gggg) <- c("Mike", "Joe")
## qplot.ts( x=gggg, window.length=101, var.name="Friends", facet=T, facetscale="free", title="test", xlab="x", ylab="y", ylim=range(5,12), legend=T, ref=c(9.5,6.5))

## ## playing with several options
## gggg2 <- rnorm(2000, 8, 0.2)
## data1 <- qplot.ts( x=gggg2, window.length=101, smoothonly=T, var.name="Friends", facet=F, title="test", xlab="x", ylab="y", ts.names="Mike", ref=7, size=2)

## gggg3 <- rnorm(2000, 5, 0.2)
## data1 <- qplot.ts( x=gggg3, window.length=101, var.name="Friends", facet=F, title="test", xlab="x", ylab="y", add=data1, ts.names="Joe",legend=T,ref=4.5)

## gggg4 <- rnorm(2000, 3, 0.2)
## qplot.ts( x=gggg4, var.name="Friends", facet=F, title="test", xlab="x", ylab="y", add=data1, ts.names="Frank",legend=T)





