\name{jiggleClass}
\alias{jiggleClass}
\title{
Adjust class boundaries to protect from rounding errors
}
\description{
When using \code{\link[classInt]{classIntervals}} to compute classes, occasionally there are rounding errors so that
when you go to plot data and use the class breaks for colors, for instance, the rounding error may cause a value to 
not be plotted with the right color, or to not be plotted at all.  For this reason, we add a small value to each of the
break points to accomodate a possible rounding error.  When plotting, this correction will not be visible.
}
\usage{
jiggleClass(x)
}
\arguments{
  \item{x}{
an object of class \code{classIntervals} from the function \code{\link[classInt]{classIntervals}}.
}
}
\value{
an object of class \code{classIntervals}.
}
\examples{

library(classInt)
library(RColorBrewer)

y <- 100*rnorm(50)

#compute class intervals
class <- classIntervals(var=y, n=12, style="jenks")
class$brks

#there should be a slight difference now between class breaks
class <- jiggleClass(x=class)
class$brks

}