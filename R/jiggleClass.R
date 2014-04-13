jiggleClass <- function(x) {
  
   ngroups <- length(x$brks) -1

   if (ngroups >=2) {
   
   #jiggle the end points
   r <- range(x$var, na.rm=TRUE)
   jiggle_end <- 0.001*(abs(r[2]-r[1]))

   x$brks[1] <- x$brks[1] - jiggle_end
   x$brks[ ngroups + 1] <- x$brks[ ngroups + 1 ] + jiggle_end

   #jiggle middle points by a bit for rounding error 
   s <- sort(x$var)

   #gives you the number of items in each class
   w <- cumsum(as.vector(table(classInt::findCols(x))))
   w <- w[ -length(w) ]
   d <- 0.001*(s[ w+1 ] - s[w])
       
   x$brks[ 2:ngroups ] <- x$brks[ 2:ngroups ] + d

   }

   x

}



   
