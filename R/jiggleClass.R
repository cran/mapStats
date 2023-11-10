


jiggleClass <- function(x) {

	# a few functions that we don't want to have to document
	remove_empty_intervals <- function(x) {
		nobs <- nobsEach(x)
		#eliminate any empty groups:
		nobs <- nobsEach(x)
	   
		if (any(nobs==0)) { 
		   warning("Some class intervals are empty. Number of groups will decrease.  Consider decreasing ngroups.") 
		
		   x$brks <- x$brks[ c(1, which(nobs !=0) +1 )]
		   
		   nobs <- nobsEach(x) 
	   }
	   ngroups <- length(x$brks) -1
	   list(x=x, ngroups=ngroups, nobs=nobs)

	}
	
	
	is.whole_number_vec <- function(x) {
		all(vapply(x, function(xx) ttutils::isInteger(n=xx, tol=1e-10), c(TRUE)))

	}


   ngroups <- length(x$brks) -1
   nobs <- nobsEach(x)

	if (ngroups >=2) {
   
		#eliminate any empty groups:
		tmp <- remove_empty_intervals(x)
		ngroups <- tmp$ngroups
		x <- tmp$x
		nobs <- tmp$nobs
		
	   # if original variable values and breaks are integers, then make breaks +- 0.5
	   if (is.whole_number_vec(x$brks) & is.whole_number_vec(x$var)) {
			
			# by now empty intervals have been removed
			brks <- x$brks
			brks[c(1, length(brks))] <- c(-Inf, Inf)
			# which interval each item belongs to
			belongsto <- findInterval(x=x$var, vec=brks, all.inside=TRUE, left.open=(attributes(x)$intervalClosure == "right"))
			# make into a list for each interval
			which_belongs_to <- lapply(unique(belongsto), function(xx) which(belongsto == xx))
			min_interval_val <- sapply(which_belongs_to, function(xx) min(x$var[xx])) - 0.5
			max_interval_val <- sapply(which_belongs_to, function(xx) max(x$var[xx])) + 0.5
			# new breaks are the midpoints of the mins/maxes and the overall endpoints
			# note that because the endpoints of each interval now do not overlap with any
			# observed value in x$var, the closure doesn't matter anymore
			# also, intervals won't be empty
			x$brks <- c(min_interval_val[1], 
						apply(rbind(max_interval_val[-ngroups], min_interval_val[-1]), 2, mean),
						max_interval_val[ngroups])
	
		}
		
		else {
	   
	  
		   #jiggle the end points
		   r <- range(x$var, na.rm=TRUE)
		   jiggle_end <- 0.01*(abs(r[2]-r[1]))

		   x$brks[1] <- x$brks[1] - jiggle_end
		   x$brks[ ngroups + 1] <- x$brks[ ngroups + 1 ] + jiggle_end


		   #jiggle middle points by a bit for rounding error 
		   s <- sort(x$var)
		   
		   ind <- cumsum(nobs)[ -ngroups ]
		   if (attributes(x)$intervalClosure =="right") {
			 
			 #increase the breaks a little bit
			 
			 d <- 0.001*(s[ind +1] -x$brks[2:ngroups])
			 x$brks[ 2:ngroups ] <- x$brks[ 2:ngroups ] + d
			}
		   else {
			 s <- c(x$brks[1], s)
			 d <- 0.001*(s[ind + 1] -x$brks[2:ngroups])
			 x$brks[ 2:ngroups ] <- x$brks[ 2:ngroups ] - d     
			}

		}     
	}
   x

}
