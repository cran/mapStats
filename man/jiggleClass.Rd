\name{jiggleClass}
\alias{jiggleClass}
\title{
Adjust class boundaries to protect from rounding errors
}
\description{
When using \code{\link[classInt]{classIntervals}} to compute classes, occasionally there are rounding errors so that
when the data is plotted and the class breaks are used for colors, for instance, the rounding error may cause a value to 
not be plotted with the right color, or to not be plotted at all.  For this reason, we add a small value to each of the
break points to accomodate a possible rounding error.  This correction is negligible and should not affect plotting.
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


y <- 100*rnorm(50)

#compute class intervals
class_list <- classInt::classIntervals(var=y, n=12, style="jenks")
class_list$brks

#there should be a slight difference now between class breaks
class_list <- jiggleClass(x=class_list)
class_list$brks

}