\name{mapStats-package}
\alias{mapStats-package}
\docType{package}
\title{
Geographic display of survey data statistics
}
\description{
\code{mapStats} will calculate statistics (mean, total, and quantiles) for survey
variables, by geography level (e.g. state, county), and an optional class variable
(such as year, if multiple years are included).  The statistics for each variable
are then plotted on a shapefile with color codes.  Statistics may be calculated either
weighted or unweighted.
}
\details{
\tabular{ll}{
Package: \tab mapStats\cr
Type: \tab Package\cr
Version: \tab 1.11\cr
Date: \tab 2014-02-20\cr
License: \tab GPL (>=2)\cr
}
To use the package, one should have a survey \code{\link[base]{data.frame}} with a geographic variable, and a
shapefile that displays the boundaries of this geographic variable (such as a map of the 
United States).  The function \code{\link{mapStats}} is used to call functions \code{calcQuantiles} and
\code{calcStats} to calculate the desired statistics, and then calls \code{plotStats} to display these statistics
on the provided shapefile. A list of the calculated statistics is returned in \code{\link{mapStats}}.
}
\author{
Samuel Ackerman
Maintainer: Samuel Ackerman <ackerman@temple.edu>
}
\keyword{ package }
\keyword{ color }
\keyword{ dplot }
\keyword{ print }
%%\seealso{
%%}
%%\examples{
%%}
