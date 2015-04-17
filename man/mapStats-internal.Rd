\name{mapStats-internal}
\alias{plotStats}
\alias{print.plotStats}
\alias{calcQuantiles}
\alias{nobsEach}
\title{
Internal functions for mapStats package
}
\description{
\code{plotStats} creates trellis plot objects that are then printed by \code{print.plotStats}.  They are called
internally from within \code{\link[mapStats]{mapStats}}.  \code{nobsEach} is used by \code{jiggleClass} to 
calculate the number of observations in each division.
}
\usage{
plotStats(statmats, map.file, d.geo.var, map.geo.var, ngroups, separate, paletteName,
          colorVec, map.label, map.label.names, cex.label, col.label, titles, cex.title,
          wt.ind = FALSE, wt.label, var.pretty, geo.pretty, by.pretty,
          sp_layout.pars, plotbyvar, num.col, ...)

calcQuantiles(...)

nobsEach(divs)

\method{print}{plotStats}(x, horizontal.fill = TRUE, num.row = 1, num.col = 1, ...)
}
\arguments{
  \item{statmats}{
a list object produced by \code{calcStats} or \code{calcQuantiles} containing the matrices of statistics to plot.
}
 \item{map.file}{
an object of class \code{\link[sp]{SpatialPolygonsDataFrame}} on which the statistics will be plotted.
}
  \item{d.geo.var}{
a character string of the name of the variable in the data frame \code{d} that is the geographic identifier.
}
  \item{map.geo.var}{
a character string of the name of the geographic identifier in the data portion of \code{map.file}. This is the counterpart of
\code{d.geo.var}.  The default is for this to be the same name as \code{d.geo.var}.
The values of \code{d.geo.var} and \code{map.geo.var} must be coded the same way for merging.
}
  \item{ngroups}{
a numeric vector of the number of levels for color plotting of variable statistics.  If more than one number is specified, \code{ngroups}
will be different in each plot.
}
 \item{separate}{
logical.  Default is TRUE, meaning that class divisions will be calculated separately for each statistic's values.  Setting
it to FALSE causes the function to calculate a color key by pooling the values from all the statistics across the by variables.
Generally if you plot multiple statistics on a page with the same color palette, setting \code{separate} to TRUE may cause confusion
because colors will represent different values for each panel.
}
 \item{paletteName}{
a character vector containing names of color palettes for the \code{RColorBrewer} function \code{\link[RColorBrewer]{brewer.pal}}. See details
below for valid names.  The default is to use these palettes for coloring, in which case \code{ngroups} will be restricted to between
3 and 9 levels, since there are at most 9 levels in \code{RColorBrewer} palettes.  This is a good simple option.
User-provided palettes can be used instead by specifying the argument \code{colorVec} to override this option.  See details below.
}
\item{colorVec}{
a list where each element is vector of ordered colors; they should be ordered from light to dark for a sequential palette.  These will override
the use of \code{RColorBrewer} through the \code{paletteName} argument.  See the demo for an example of using HCL sequential palettes from the
\code{colorspace} package.  Use of the \code{colorVec} argument will override a value provided for \code{ngroups}.
}
  \item{map.label}{
logical.  Default is TRUE; if FALSE, names of the geographic regions will not be labeled on the map outputs.
}
  \item{map.label.names}{
a character string naming the vector from the \code{map.file@data} data.frame to use to label the map. The default is to
use \code{map.geo.var}.
}
  \item{cex.label}{
numeric. Character expansion for the labels to be printed.
}
  \item{col.label}{
color of the label text to be printed.  Default is black.
}
  \item{titles}{
a character string of length equal to the number of statistics to be plotted, in order. Replaces the default plot titles.
}
  \item{cex.title}{
numeric. Character expansion for the plot titles.
}
 \item{wt.ind}{
logical.  Default is FALSE. If TRUE, weighted statistics were calculated. If \code{map.label} is TRUE, default plot titles will indicate whether weighted
or unweighted statistics were calculated.
}
  \item{wt.label}{
logical.  Default is TRUE, in which case automatic titles will be followed by the string '(wtd.)' or '(unwtd.)' as appropriate,
depending on whether weighted statistics were calculated. If FALSE no label will be added.
}
  \item{var.pretty}{
a character string used to name the analysis variable in the default plot titles. The default is to use \code{var} as the name in titles.
}
  \item{geo.pretty}{
a character string used to name the geographic class variable in the default plot titles. The default is to use \code{d.geo.var} as the name in titles.
}
  \item{by.pretty}{
a vector of character strings used to name the by variables (other than the geographic one) in the default panel strip labels. The default is to use the original variable names in \code{by.var}.
}
  \item{sp_layout.pars}{
a list.  This contains additional parameters to be plotted on each panel.  See details section below and explanation of \code{sp.layout}
in \code{\link[sp]{spplot}}. An example is provided in the demo file.
}
\item{plotbyvar}{
logical.  If TRUE plots will be grouped by variable, otherwise by statistic.
}
\item{num.col}{
numeric. To print multiple statistics on one page, indicate the number of columns for panel arrangement.  Under the default, one statistic is
printed per page.
}
  \item{x}{
list. A list produced by \code{plotStats} containing \code{trellis} plot objects.
}
\item{horizontal.fill}{
logical.  Default is TRUE, meaning that given the plot arrangement specified with \code{num.row} and \code{num.col},
plots will be plotted in order left to right then down.  FALSE means they will be plotted going down first and then left to right.
The user may need to use the optional \code{lattice} \code{layout} argument to control the layout of panels within a
single plot to make sure the plots print with enough space.  Examples are shown in the demo file.
}
  \item{num.row}{
numeric. To print multiple statistics on one page, indicate the number of rows for panel arrangement.  Under the default, one statistic is
printed per page.
}
\item{divs}{
An object of class \code{classIntervals}.
}
  \item{...}{
Further arguments, usually lattice plot arguments.
}
}
\author{
Samuel Ackerman
}
\keyword{ color }
\keyword{ dplot }
\keyword{ print }
\keyword{ internal }