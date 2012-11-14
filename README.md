RSlib
=====
Version     : 0.01	


Description : 
-----------

The present repository contains R functions written by Salvatore Cosseddu for his PhD project. 
They were not developed to be neither complete nor user friendly, but to fulfil his needs. 
But anyway you might find something useful. 

Available functions:
------

1. _qplot.ts_ : (quick plot time series) a version of plot.ts that uses ggplot2 packages, adding some nice features to plot.ts:  
  *   the obtained object can be easily modified using ggplot2 grammar, adding or changhing properties;
  *   a smooth coloured line can be added (moving average is the only implemented at the moment, just because that is what I needed);
  *   optional legend is automatically added in the ggplot2 style;
  *   add option available;
  *   facetting optionally available (default TRUE);
  *   reference horizontal lines can be easily added;
  *   more than 10 ts can be plotted.

  The function is an implementation of a simple method to create a data.frame from the time series using the package reshape2, 
  commonly used to plot ts with ggplot2

2. _qplot.density_ : (quick density plot) uses ggplot2 packages to plot several distributions:
  *   the object obtained can be easily modified using ggplot2 grammar, adding or changhing properties;
  *   optional legend is automatically added in the ggplot2 style;
  *   facetting optionally available;
  *   if the distribution are drawn on a single plot, only density are shown, otherwise alse an histogram will be added;
  *   more than 10 ts can be plotted.

  The function is not fully stable, but the main features work fine


------
										  
  COPYRIGHT					       				  
  Copyright © 2012.  
  Salvatore Cosseddu		       				  
  Centre for Scientific Computing and School of Engineering, University of Warwick, Coventry, UK.		       	  
  License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.  
  This is free software: you are free to change and redistribute it.         	  
  There is NO WARRANTY, to the extent permitted by law.          		  


