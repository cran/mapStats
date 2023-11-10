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
When both the variable values and all the interval breaks are integer-valued (e.g. for sums of integer values),
jiggleClass adjusts the interval breaks to be at the midpoints between interval endpoints.
Additionally, in case \code{ngroups} is high, resulting in empty groups (even though the number of unique values is higher than \code{ngroups}),
the function also eliminates the empty groups as part of the adjustment above.  In case there is such a change, the palettes are also changed.

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

#compute class intervals using either right or left interval closure

class_list_right <- classInt::classIntervals(var=y, n=12, intervalClosure="right")
class_list_right$brks
class_list_left <- classInt::classIntervals(var=y, n=12, intervalClosure="left")
class_list_left$brks

#there should be a slight difference now between class breaks from before, but should 
#have same number of observations per interval as before, and for both left and right closure

jiggleClass(x=class_list_right)
jiggleClass(x=class_list_left)


#example with discrete data, 7 groups but 9 unique values.
#classIntervals generates some empty intervals, so jiggleClass eliminates them and adjusts breaks
#in this example, with integer values, right/left closure matters more, and so the results
#will differ slightly depending on which is chosen
# here, both the observed values and the breaks under right and left are integers

y <- c(1, 1, 1, 1, 2, 3, 6, 7, 8, 9, 10, 10, 10, 10, 11)
class_list_right <- classInt::classIntervals(y, 7, intervalClosure="right")
class_list_right
#style: quantile
#  one of 28 possible partitions of this variable into 7 classes
#  [1,1]   (1,2]   (2,6]   (6,8]  (8,10] (10,10] (10,11] 
#      4       1       2       2       1       4       1 

class_list_left <- classInt::classIntervals(y, 7, intervalClosure="left")
class_list_left
#style: quantile
#  one of 28 possible partitions of this variable into 7 classes
#  [1,1)   [1,2)   [2,6)   [6,8)  [8,10) [10,10) [10,11] 
#      0       4       2       2       2       0       5 


#number of groups falls now for left closure, from 7 to 5, in this example

jiggleClass(x=class_list_right)
#style: quantile
#  one of 28 possible partitions of this variable into 7 classes
#  [0.5,1.5]   (1.5,2.5]   (2.5,6.5]   (6.5,8.5]   (8.5,9.5]  (9.5,10.5] (10.5,11.5] 
#          4           1           2           2           1           4           1 


jiggleClass(x=class_list_left)
#style: quantile
#  one of 70 possible partitions of this variable into 5 classes
# [0.5,1.5)  [1.5,4.5)  [4.5,7.5)  [7.5,9.5) [9.5,11.5] 
#         4          2          2          2          5 

}