\name{usMap}
\alias{usMap}
\docType{data}
\title{
Shapefile of US state boundaries
}
\description{
Examples are provided using the \code{\link{usMap}} shapefile with a synthetic dataset.
}

\source{
\url{https://www.cdc.gov/brfss/gis/gis_maps.htm}
}

\examples{
#read the shapefile
#display first five rows of shapefile dataset
#plot the shapefile boundaries

usMap <- sf::as_Spatial(sf::st_read(system.file("shapes/usMap.shp", package="mapStats")[1]))
head(usMap@data)
# plot the outlines of the shapefile only
sp::plot(usMap)
# plot map colored according to one of the variables in @data
sp::spplot(usMap, zcol="A187_1")

}
