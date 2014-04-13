\name{mapStats}
\alias{mapStats}
\alias{calcStats}
\alias{calcQuantiles}
\title{
Calculate and plot survey statistics
}
\description{
\code{mapStats} computes statistics and quantiles of a survey variable and displays them on a color-coded map.
It calls functions \code{calcStats} and \code{calcQuantiles}, which are also usable outside of \code{mapStats}.
}
\usage{
mapStats(d, var, stat = c("mean", "quantile"), quantiles = c(0.5, 0.75), 
         wt.var = NULL, wt.label = TRUE, d.geo.var, by.var = NULL, 
         map.file, map.geo.var = d.geo.var, makeplot = TRUE, ngroups = 4, 
         separate = TRUE, cell.min = 0, palette = "Reds", col = NULL, 
         map.label = TRUE, map.label.names = map.geo.var, cex.label = 0.8,
         col.label = "black", titles = NULL, cex.title = 1, var.pretty = var,
         geo.pretty = d.geo.var, by.pretty = by.var, as.table = TRUE, 
         sp_layout.pars = list(), between = list(y = 1), horizontal.fill = TRUE, 
         num.row = 1, num.col = 1, ...) 

calcStats(d, var, stat = c("mean", "total"), d.geo.var, 
          by.var = NULL, wt.var = NULL, cell.min = 0) 

calcQuantiles(d, var, quantiles = c(0.50, .75), d.geo.var,
              by.var = NULL, wt.var = NULL, cell.min = 2)
                          
}
\arguments{
  \item{d}{
a data frame containing the variables to be analyzed. 
}
  \item{var}{
a character string of the name of the variable that statistics will be calculated for.  Only one variable can be used at a time.
}
  \item{stat}{
a character vector of names of statistics to calculate. Valid names are "mean", "total", and "quantile".  "Quantile" must be included
for the quantiles specified to be calculated. Statistics are printed in the order given. For instance if \code{stat = c("total","quantile","mean")},
then the order will be total, then quantiles in order, and then the mean.
}
  \item{quantiles}{
a numeric vector of quantiles to be calculated for the variable var. The quantiles must be specified as decimals between 0 and 1.
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
a character string of the name of the variable in the data frame \code{d} that is the geographic identifier. 
}
  \item{by.var}{
a character string specifying an optional class variable to calculate statistics by.  If specified, statistics will be calculated at
all level combinations of \code{d.geo.var} and \code{by.var}; otherwise, just overall statistics for \code{d.geo.var} are calculated.
Useful for combining multiple survey years and seeing how statistics change over time.  
}
  \item{map.file}{
an object of class \code{\link[sp]{SpatialPolygonsDataFrame}} on which the statistics will be plotted.
}
  \item{map.geo.var}{
a character string of the name of the geographic identifier in the data portion of \code{map.file}. This is the counterpart of 
\code{d.geo.var}.  The default is for this to be the same name as \code{d.geo.var}.
The values of \code{d.geo.var} and \code{map.geo.var} must be coded the same way for merging.
}
  \item{makeplot}{
logical. Default is TRUE; if FALSE, plots will not be drawn.  This option can be used to calculate statistics without an available shapefile.
}
  \item{ngroups}{
a numeric vector of the number of levels for color plotting of variable statistics.  If more than one number is specified, \code{ngroups} 
will be different in each plot.
}
 \item{separate}{
logical.  Default is TRUE, meaning that class divisions will be calculated separately for each statistic's values.  Setting
it to FALSE causes the function to calculate a color key by pooling the values from all the statistics across the by variables.  
Generally if multiple statistics are plotted on a page with the same color palette, setting \code{separate} to TRUE may cause confusion
because colors will represent different values for each panel.
}
 \item{cell.min}{
numeric. Indicates the minimum number of observations in a cell combination of \code{d.geo.var} and \code{by.var} (if specified).  
If there are fewer than that, the statistic will be NA in that cell.  For \code{calcQuantiles}, \code{cell.min} must be at least 2
to allow for interpolation.
}
  \item{palette}{
a character vector containing names of color palettes for the \code{RColorBrewer} function \code{\link[RColorBrewer]{brewer.pal}}. See details
below for valid names.  The default is to use these palettes for coloring, in which case \code{ngroups} will be restricted to between 
3 and 9 levels, since there are at most 9 levels in \code{RColorBrewer} palettes.  This is a good simple option. 
User-provided palettes can be used instead by specifying the argument \code{col} to override this option.  See details below.
}
\item{col}{
a list where each element is vector of ordered colors; they should be ordered from light to dark for a sequential palette.  These override
the use of \code{RColorBrewer} through the \code{palette} argument.  See the demo for an example of using HCL sequential palettes from the 
\code{colorspace} package.  Use of the \code{col} argument will override a value provided for \code{ngroups}.
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
  \item{var.pretty}{
a character string used to name the analysis variable in the default plot titles. The default is to use \code{var} as the name in titles.
}
  \item{geo.pretty}{
a character string used to name the geographic class variable in the default plot titles. The default is to use \code{d.geo.var} as the name in titles.
}
  \item{by.pretty}{
a character string used to name the by variable in the default panel strip labels. The default is to use \code{by.var} as the name labels.
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
single plot to make sure the plots print with enough space.  Examples are shown in the demo file.
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
Quantiles for combinations \code{d.geo.var} and \code{by.var} (if specified) will be missing if the combination has 
fewer than two non-missing observations.  A warning message may be returned by the function \code{\link[quantreg]{rq}} that the 
output may be singular if there are small cell sizes.  

\code{palette} should contain one or more names of a sequential color palette in R from the \code{\link[RColorBrewer]{RColorBrewer}} package.  These are: 
Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd.  The argument \code{ngroups} for this option should contain
values between 3 and 9 since sequential color palettes have at most nine levels.  The \code{style} argument from \code{\link[classInt]{classIntervals}} can be included 
to change the method for calculating breaks (the default is by quantiles).

The default titles for the plots will be "(stat) of (variable) by (geography)", followed by either "(unwtd.)" or "(wtd.)", as appropriate.  Using the \code{wt.label} 
argument controls the appearance of the weight label in the titles.  Providing a value for the \code{titles} argument will override the default titles. 
This can be used, for instance, as shown below, to display percentages for a binary variable by calculating the mean of an
indicator variable and specifying titles that indicate the percent is displayed.

If \code{quantiles} are 0 (minimum), 0.50 (median), or 1 (maximum), the statistics in the titles will be named "Minimum", "Median", and "Maximum" instead of "Q0", "Q50" or "Q100".

The \code{lattice} \code{layout} argument can be used to control the placement of panels within a graph, especially if multiple plots are done on a page.

\code{sp_layout.pars} must itself be a list, even if its contents are lists also.  This allows overplotting of more than one object.  For instance, say you had a shapefile
\code{areas} to be colored blue, and a vector of strings \code{labels1} that had x-coordinates \code{xplaces} and y-coordinates \code{yplaces} to overlay on
the plot.  Create objects \code{areas_overlay}=
\code{list("sp.polygons", areas, fill="blue")}, and \code{labels_overlay}= 
\code{list("panel.text", labels1, xplaces, yplaces)}.  
Then set argument \code{sp_layout.pars}= 
\code{list(areas_overlay, labels_overlay)}.  Even if you only wanted to
overlay with \code{areas}, you would still need to enclose it in another list, for example \code{sp_layout.pars}= \code{list(areas_overlay)}.

}
\value{
\code{mapStats}, \code{calcStats}, and \code{calcQuantiles} return an object of class "list"
\item{summary.stats}{    a list containing the calculated statistics matrices}
with attribute
\item{variable}{    the name of the variable}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Samuel Ackerman
}
\note{
Please see the included demo file \code{map_examples} for examples on controlling formatting,
coloring, and other customizable options, as well as more examples 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
The \code{survey} package function \code{\link[survey]{svyby}} is used to calculate mean 
and totals, and \code{\link[quantreg]{rq}} calculates quantiles. \code{\link{spplot}} plots the map. 
}
\examples{
#More complex examples with formatting are shown in the map_examples demo for the package

#create synthetic survey dataset


state_codes <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL',
                 'IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE',
                 'NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD',
                 'TN','TX','UT','VT','VA','WA','WV','WI','WY')

surveydata <- data.frame(state=factor(rep(rep(state_codes, 
                         times=3), times=2)))
surveydata$year <- rep(c(2009, 2010), each=nrow(surveydata)/2)
surveydata$obs_weight <- runif(n=nrow(surveydata), min=0.8, max=1.5)

#two income distributions
surveydata$income <- 100000*rbeta(n=nrow(surveydata), 
                                  shape1=ifelse(surveydata$year==2009, 2, 1.5),
                                  shape2=ifelse(surveydata$year==2010, 10, 11))
surveydata$income_ge20k <- ifelse(surveydata$income >=20000, 100, 0)

#these state and year combinations will not be shaded if they are missing entirely
surveydata[ surveydata$state == "NV" & surveydata$year == 2009, c("income","income_ge20k")] <- NA
surveydata[ surveydata$state == "OH" & surveydata$year == 2010, c("income","income_ge20k")] <- NA


#load map shapefile
usMap <- readShapePoly(system.file("shapes/usMap.shp", package="mapStats")[1])


#Calculate weighted mean of variable income by state.  Display using red 
#sequential color palette with 4 groups.  In the titles, rename 'income'
#by 'household income'.     

mapStats(d=surveydata, var="income", wt.var="obs_weight", 
         map.file=usMap, d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean"), ngroups=4, palette="Reds", 
         var.pretty="household income", geo.pretty="state",
         map.label=TRUE)


#Calculate the weighted mean and 40th and 50th quantiles of the variable income
#by state and survey year. Display 2 statistics on the first page and 1 on the
#last, and use three color palettes

\dontrun{  
mapStats(d=surveydata, var="income", by.var="year",
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean","quantile"), quantiles=c(.4, .5),
         ngroups=6, palette=c("Reds","Greens","Blues"), 
         var.pretty="household income", geo.pretty="state", 
         by.pretty="Year", map.label=TRUE, num.col=1, num.row=2, 
         layout=c(2,1))
}

#To calculate percentages of class variables, create an indicator variable, calculate
#its mean, and override the default titles to say you are calculating the percentage.
#Here we illustrate by calculating the percent of respondents by state that have income
#above $20,000.

\dontrun{
mapStats(d=surveydata, var="income_ge20k", wt.var="obs_weight", 
         map.file=usMap, d.geo.var="state", map.geo.var="STATE_ABBR", 
         col.pal="Reds", stat=c("mean"), 
         titles="Percent of respondents with income at least $20,000")
}


#calculating statistics using the functions outside of mapStats
#unweighted quantiles

\dontrun{
calcQuantiles(d=surveydata, var="income", d.geo.var="state", 
              by.var="year", quantiles=c(0.5, 0.75))
}

#weighted mean

\dontrun{
calcStats(d=surveydata, var="income", stat="mean", 
          d.geo.var="state", by.var="year", wt.var="obs_weight")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }
\keyword{ dplot }
\keyword{ print }