###################################################################################
#: Title       : qplot.density.R
#: Author      : Salvatore Cosseddu - S.M.Cosseddu@warwick.ac.uk                  #
#: Institution : University of Warwick - Centre for Scientific Computing and	  #
#                                        School of Engineering			  #
#: Description : Plot several distributions using ggplot2
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


qplot.density <- function(x, var.name="variable", val.name="value", facet=FALSE,
                          facetscale="free", title=NULL,
                          xlab="x", ylab="y", xlim=NULL, ylim=NULL,
                          ref=NULL, legend=TRUE, binwidth=0.025, add=NULL, ...) {

  require(ggplot2)
  require(reshape2)
  ## ####################
  ## ggplot2
  ## reshaping for ggplot2, that needs well shaper data.frames
  x.df <- melt(as.data.frame(x), variable.name=var.name, value.name=val.name)
   
  ## plotting
  ## adding first ggplot layer
  ## adding first ggplot layer
  if (is.null(add)) {
    x.plt <- ggplot(x.df)
  } else {
    x.plt <- add
  }
  x.plt <- ggplot(x.df)

    ## creating subplot if required (facet=TRUE)
  facetformula <- paste(var.name,".", sep=" ~ ")
  if (facet) {
    x.plt <-  x.plt +
                 facet_grid(as.formula(facetformula), scales=facetscale) +
                   geom_histogram(aes_string(x=val.name, y="..density.."),colour = "black", fill = "white", binwidth = binwidth )
  }

  ## plotting density 
  x.plt <- x.plt +
    geom_density(data=x.df, aes_string(x=val.name, fill=var.name), alpha=.5) 

  
  ## if a reference exists
  if (!is.null(ref)) {
    if ( is.numeric(ref) ) {
      ref <- data.frame (t(ref))
      names(ref) <- paste(sub(" ",".",colnames(x)))
      mean.set <- melt(ref, value.name="ref", variable.name=var.name)
    } else if (ref == "mean") {
      mean.set <- data.frame ( ref = mean(as.vector(x)))
    } else {
      return ("Error in mean")
    }
    print("using reference values:")
    print(mean.set)
      
    x.plt <- x.plt + geom_vline(data=mean.set, aes_string(xintercept="ref", colour=var.name), size=2, linetype="dashed", alpha=1) #, col="green"

  }

  ## labelling the plot
  x.plt <- x.plt + labs(list(title = title, x = xlab, y = ylab))

  ## applying limits
  if (!is.null(xlim)) {
    x.plt <- x.plt + xlim(xlim)
  }

  if (!is.null(ylim)) {
    x.plt <- x.plt + ylim(ylim)
  }

  if (!legend) {
    x.plt <- x.plt + theme(legend.position="none")
  }
  
  return(x.plt)

  
}

## qplot.density                 package:clib                  

## Plotting Time-Series Objects using ggplot2

## Description:

##      Plot several distributions using ggplot2

## Usage:

## qplot.density (x, var.name="variable", val.name="value", facet=FALSE,
##                   facetscale="free", title=NULL,
##                   xlab="x", ylab="y", xlim=NULL, ylim=NULL,
##                   ref=NULL, legend=TRUE, binwidth=0.025, ...) 
     
## Arguments:

## x: an object containing the data set (i.g. data.frame, matrix, ts)

## var.name: a definition for the data sets, it will return in the legend.

## val.name: a definition for the values in the data sets.

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

## ref: vector, contains reference values that will be plotted as vertical dashed lines.

## legend: logical, plot legend,

## add: if a ggplot object is provided (for instance a preview output from qplot.density)
##      the new layers will be added to it. 

## binwidth: if facet is true a histogram will be brawn; Binwidth is the width of the bins

## known bugs. qplot.density is under development. "ref" do not work if a single data set
## is provided (the solution is known, the time is needed), and add feauture does not work.
## Sorry. 

## Examples:

## working on a ts on different facets
## gggg <-  cbind (rnorm(2000, 10, 0.2),
##          rnorm(2000, 9, 0.2))

## colnames(gggg) <- c("mice","cats")

## qplot.density( x=gggg, var.name="Friends", facet=TRUE, facetscale="fixed", title="test", xlab="x", ylab="y", xlim=NULL, ylim=NULL, legend=TRUE, add=NULL)


## colnames(gggg) <- c("Mike", "Joe")
## data1 <- qplot.density( x=gggg, var.name="Friends", facet=F, facetscale="free", title="test", xlab="x", ylab="y", legend=T, ref=c(9.5,6.5))

## gggg2 <- rnorm(2000, 6, 0.2)
## data1 <- qplot.density( x=gggg2, var.name="Friends", facet=F, title="test", xlab="x", ylab="y", xlim=range(4,8))
