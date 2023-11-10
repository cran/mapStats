\name{mapStats-package}
\alias{mapStats-package}
\docType{package}
\title{
Geographic display of survey data statistics
}
\description{

\code{mapStats} will calculate statistics (mean, total, quantiles, variance, and standard deviation) for survey data
variables, by geography level (e.g., state, county) and optional observation-level factor variables
(e.g., survey year, eduction level, race).  The statistics for each variable
are then plotted on a shapefile with color codes (choropleth map).  Easy control over visual 
elements such as cell censoring, color palettes, titles, plot layout, and variable renaming is enabled; see included \code{demo(map_examples)} for a 
full overview of options.  Statistics may be calculated either weighted or unweighted.  
The function \code{\link{calcStats}}, which is called by \code{\link{mapStats}}, can also be used on its own to calculate variable statistics.
}
\details{
\tabular{ll}{
Package: \tab mapStats\cr
Type: \tab Package\cr
Version: \tab 3.1\cr
Date: \tab 2023-11-6\cr
License: \tab GPL (>=2)\cr
} 
}
\author{
Samuel Ackerman
Maintainer: Samuel Ackerman <smackrmn@gmail.com>
}
\keyword{ package }
\keyword{ color }
\keyword{ dplot }
\keyword{ print }

