\name{mapStats}
\alias{mapStats}
\alias{calcStats}
\title{
Calculate and plot survey statistics
}
\description{
\code{mapStats} computes statistics and quantiles of survey variable(s), with optional class variables, and displays them on a color-coded map.
It calls function \code{calcStats} which is also usable outside of \code{mapStats}.
}
\usage{
mapStats(d = NULL, main.var, stat = c("mean", "quantile"), quantiles = c(0.5, 0.75), 
         wt.var = NULL, wt.label = TRUE, d.geo.var, by.var = NULL, 
          map.file = NULL, map.geo.var = d.geo.var, makeplot = TRUE, ngroups = 4, 
         separate = 1 , cell.min = 0, paletteName = "Reds", colorVec = NULL, 
         map.label = TRUE, map.label.names = map.geo.var, cex.label = 0.8,
           col.label = "black", titles = NULL, cex.title = 1, var.pretty = main.var,
         geo.pretty = map.geo.var, by.pretty = by.var, as.table = TRUE, 
         sp_layout.pars = list(), between = list(y = 1), horizontal.fill = TRUE,
         plotbyvar = ifelse(separate == 1 & length(main.var) > 1, FALSE, TRUE), 
         num.row = 1, num.col = 1, ...) 

calcStats(d, main.var, d.geo.var, stat = c("mean", "quantile"), quantiles = c(0.5, 0.75), 
          by.var = NULL, wt.var = NULL, cell.min = 0) 
 
}
\arguments{
  \item{d}{
a data frame containing the variables to be analyzed. 
}
  \item{main.var}{
a vector of character strings of the name of the variables that statistics will be calculated for.  Multiple variables are allowed.
}
  \item{stat}{
a character vector of names of statistics to calculate. Valid names are "mean", "total", "quantile", "sd" (standard deviation), and "var" (variance).  "Quantile" must be included
for the quantiles specified to be calculated. Statistics are printed in the order given. For instance if \code{stat = c("total","quantile","mean")},
then the order will be total, then quantiles in order specified in argument \code{quantiles}, then the mean.
}
  \item{quantiles}{
a numeric vector of quantiles to be calculated for each variable in variable \code{main.var}. The quantiles must be specified as decimals between 0 and 1.
In order to be calculated, "quantile" must be specified as a statistic in the argument \code{stat}.  
}
  \item{wt.var}{
a character string of the name of the variable to be used as sample weights in calculating statistics.  The default is NULL,
meaning unweighted statistics will be calculated.
}
  \item{wt.label}{
logical.  Default is TRUE, in which case automatic titles will be followed by the string '(wtd.)' or '(unwtd.)' as appropriate,
depending on whether weighted statistics were calculated. If FALSE no label will be added.
}
  \item{d.geo.var}{
a character string specifying the variable name in the dataset that is the geographic unit to calculate statistics by.
When using \code{calCstats} outside of \code{mapStats} without a mapping application, \code{d.geo.var} would be the first class variable,
and additional ones can be specified in \code{by.var}.
}
  \item{by.var}{
a vector of character strings specifying variable names in the dataset \code{d} to use as class variables.  The order in which variables are specified will affect the order in which they are combined for calculations.
The default is NULL, in which case statistics are calculated by each geographic unit (\code{d.geo.var}) only.
The function will omit from analysis any class variables that have only one level over the entire dataset.  However it is still possible that a given class variable will have only one value 
for one of the analysis variables if, say, multiple analysis variables are used. 
}
  \item{map.file}{
an object of class \code{\link[sp]{SpatialPolygonsDataFrame}} on which the statistics will be plotted.
}
  \item{map.geo.var}{
a character string of the name of the geographic identifier in the data portion of \code{map.file}. The default is for it to be \code{d.geo.var}.The values of the geographic variables in the map and original dataset must be coded the same way for merging (i.e. the factor levels must be the same).
}
  \item{makeplot}{
logical. Default is TRUE; if FALSE, plots will not be drawn.  This option can be used to calculate statistics without an available shapefile.
}
  \item{ngroups}{
a numeric vector of the number of levels for color plotting of variable statistics.  If more than one number is specified, \code{ngroups} 
will be different in each plot.  See details.
}
 \item{separate}{
numeric (or logical TRUE or FALSE for legacy cases).  Accepted values are 0,1,2,3.  This parameter controls how plot color breaks are calculated.  If \code{separate}=0, all variables and statistic combinations have the same color breaks.  If \code{separate}=3, each variable and statistic combination plot has a potentially different color break.  If \code{separate}=1 (the default), then each statistic has a different color break, but this break is the same for the statistics acoss different variables.  If \code{separate}=2, then each variable has a different color break, but this break is the same for all statistics of that variable.  In the legacy case of this parameter, TRUE results in 1 and FALSE results in 0.
}
 \item{cell.min}{
numeric. Indicates the minimum number of observations in a cell combination of class variables of \code{d.geo.var} and  \code{by.var}.  If there are fewer than that, the statistic will be NA in that combination, and if plotted, the geographic unit will be white and not used in calculating the color key.
}
  \item{paletteName}{
a character vector containing names of color palettes for the \code{RColorBrewer} function \code{\link[RColorBrewer]{brewer.pal}}. See details
below for valid names.  The default is to use these palettes for coloring, in which case \code{ngroups} will be restricted to between 
3 and 9 levels, since there are at most 9 levels in \code{RColorBrewer} palettes.  This is a good simple option. 
User-provided palettes can be used instead by specifying the argument \code{colorVec} to override this option.  See details below.
}
\item{colorVec}{
a list where each element is vector of ordered colors; they should be ordered from light to dark for a sequential palette.  These override
the use of \code{RColorBrewer} through the \code{paletteName} argument.  See the demo for an example of using HCL sequential palettes from the 
\code{colorspace} package.  Use of the \code{colorVec} argument will override a value provided for \code{ngroups}.  See details below.
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
a character string of length equal to the number of statistics to be plotted, in order. Replaces the default plot titles.  The default is NULL, in which case the plot titles are automatically generated.  See details below.
}
  \item{cex.title}{
numeric. Character expansion for the plot titles.
}
  \item{var.pretty}{
a character string used to name the analysis variables \code{main.var} in the default plot titles. The default is to use \code{main.var} as the name in titles.
}
  \item{geo.pretty}{
a character string used to name the geographic variable in the default panel strip labels. The default is to use \code{map.geo.var} as the name labels.
}
  \item{by.pretty}{
a character string used to name the by-variables (optional class variables) in the default panel strip labels. The default is to use \code{by.var} as the name labels.
}
  \item{as.table}{
logical.  Default is TRUE, meaning the panels will be displayed in ascending order of \code{by.var} (top to bottom).
}
  \item{sp_layout.pars}{
a list.  This contains additional parameters to be plotted on each panel.  See details section below and explanation of \code{sp.layout} 
in \code{\link[sp]{spplot}}. An example is provided in the demo file. 
}
 \item{between}{
list.  A \code{lattice} argument for parameters for spacing between panels.
}
 \item{horizontal.fill}{
logical.  Default is TRUE, meaning that given the plot arrangement specified with \code{num.row} and \code{num.col}, 
plots will be plotted in order left to right then down.  FALSE means they will be plotted going down first and then left to right.
The user may need to use the optional \code{lattice} \code{layout} argument to control the layout of panels within a 
single plot to make sure the plots print with enough space, and \code{par.strip.text} to control the size of panel strip fonts.  Examples are shown in the demo file.
}
\item{plotbyvar}{
logical.  If TRUE plots will be grouped by variable, otherwise by statistic.
}
 \item{num.row}{
numeric. To print multiple statistics on one page, indicate the number of rows for panel arrangement.  Under the default, one statistic is
printed per page.
}
 \item{num.col}{
numeric. To print multiple statistics on one page, indicate the number of columns for panel arrangement.  Under the default, one statistic is
printed per page.
}
  \item{...}{
Further arguments, usually lattice plot arguments.  
}
}
\details{
\code{paletteName} should contain one or more names of a sequential color palette in R from the \code{\link[RColorBrewer]{RColorBrewer}} package.  These are: 
Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd.  The argument \code{ngroups} for this option should contain
values between 3 and 9 since sequential color palettes have at most nine levels.  The \code{style} argument from \code{\link[classInt]{classIntervals}} can be included 
to change the method for calculating breaks (the default is by quantiles).

The default titles for the plots will be "(stat) of (variable) by (geography)", followed by either "(unwtd.)" or "(wtd.)", as appropriate.  Using the \code{wt.label} 
argument controls the appearance of the weight label in the titles.  Providing a value for the \code{titles} argument will override the default titles. 
This can be used, for instance, as shown below, to display percentages for a binary variable by calculating the mean of an
indicator variable and specifying titles that indicate the percent is displayed.

If \code{quantiles} are 0 (minimum), 0.50 (median), or 1 (maximum), the statistics in the titles will be named "Minimum", "Median", and "Maximum" instead of "Q0", "Q50" or "Q100".

Parameter \code{separate} affects given values of \code{colorVec}, \code{paletteName}, and \code{ngroups}.  Say there are 2 variables and 3 statistics (mean, median, and variance) to calculate for each.  If any of the 3 parameters above are of length 1, (e.g. \code{paletteName}="Reds"), these will be used for all plots. If multiple values are given for any of the 3 parameters, these should be done with \code{separate} in mind.  For instance, if \code{separate}=0 (same color breaks for all plots), then the first element of each of \code{colorVec}, \code{paletteName}, and \code{ngroups} will be used.  If \code{separate}=1 (e.g. all of the means) have the same breaks, then the user may want to specify 3 different color palettes, for example.  In this case, if \code{ngroups}=c(3,4,5,6), for instance, only the first 3 values will be used since there are only 3 statistics.  If \code{separate}=2, then only up to 2 palettes, for instance, are used.  If \code{separate}=3, then up to 6 values for each parameter will be used.  See the demo file for examples.

The \code{lattice} \code{layout} argument can be used to control the placement of panels within a graph, especially if multiple plots are done on a page.

\code{sp_layout.pars} must itself be a list, even if its contents are lists also.  This allows overplotting of more than one object.  For instance, say you had a shapefile
\code{areas} to be colored blue, and a vector of strings \code{labels1} that had x-coordinates \code{xplaces} and y-coordinates \code{yplaces} to overlay on
the plot.  Create objects \code{areas_overlay}=
\code{list("sp.polygons", areas, fill="blue")}, and \code{labels_overlay}= 
\code{list("panel.text", labels1, xplaces, yplaces)}.  
Then set argument \code{sp_layout.pars}= 
\code{list(areas_overlay, labels_overlay)}.  Even if you only wanted to
overlay with \code{areas}, you would still need to enclose it in another list, for example \code{sp_layout.pars}= \code{list(areas_overlay)}.  An example is shown in the demo file.

}
\value{
\code{mapStats} and \code{calcStats} return an object of class "list"
\item{summary.stats}{a list containing the calculated statistics matrices, as well as a frequency count matrix}
with attribute
\item{variable}{    the name of the variable}
}

\author{
Samuel Ackerman
}
\note{
Please see the included demo file \code{map_examples} for examples on controlling formatting,
coloring, and other customizable options, as well as more examples 
}


\seealso{
The \code{survey} package function \code{\link[survey]{svyby}} is used to calculate variable statistics, and \code{\link{spplot}} plots the map. 
See the supplied demo \code{map_examples} for a fuller set of examples illustrating the range of parameter options.
}
\examples{
#More complex examples with formatting are shown in the map_examples 
#demo for the package

#create synthetic survey dataset from internal function

surveydata <- synthetic_US_dataset()


#load map shapefile
usMap <- sf::as_Spatial(sf::st_read(system.file("shapes/usMap.shp", package="mapStats")[1]))


#Calculate weighted mean of variable income by state and year.  Display using red 
#sequential color palette with 4 groups.  In the titles, rename 'income'
#by 'household income'.     

# save result in an object to suppress output

res <- mapStats(d=surveydata, main.var="income", d.geo.var="state", by.var="year",
				wt.var="obs_weight", map.file=usMap, stat="mean", ngroups=4,
				paletteName="Reds", var.pretty="household income", map.label=TRUE)


#Calculate the weighted mean and 40th and 50th quantiles of the variable income
#by state, survey year, and education. Use three color palettes

\donttest{
res <- mapStats(d=surveydata, main.var="income", d.geo.var="state", 
				by.var=c("year","educ"), wt.var="obs_weight", map.file=usMap,
				stat=c("mean","quantile"), quantiles=c(.4, .5), ngroups=6,
				paletteName=c("Reds","Greens","Blues"), 
				var.pretty="household income", map.label=TRUE, num.col=1, 
				num.row=2, layout=c(2,1), cex.label=.5)
}

#To calculate percentages of class variables, create an indicator variable, calculate
#its mean, and override the default titles to say you are calculating the percentage.
#Here we illustrate by calculating the percent of respondents by state that have income
#above $20,000.
\donttest{
res <- mapStats(d=surveydata, main.var="income_ge30k", wt.var="obs_weight", 
				map.file=usMap, d.geo.var="state", map.geo.var="STATE_ABBR", 
				paletteName="Reds", stat=c("mean"), 
				titles="Percent of respondents with income at least $30,000")
}
#calculating statistics using the functions outside of mapStats
\donttest{
res <- calcStats(d=surveydata, main.var="income", stat="mean",
				 d.geo.var="state", by.var=c("year", "educ"), 
				 wt.var="obs_weight")
}

}

\keyword{ color }
\keyword{ dplot }
\keyword{ print }