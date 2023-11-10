# legacy function
calcQuantiles <- function(...) calcStats(...)




nobsEach <- function(divs) {
      #number of observations in each class
	  # first expand divs to have inf on both ends
	  divs$brks[c(1, length(divs$brks))] <- c(-Inf, Inf)
	  # depends on intervalClosure
      nobs <- table( findInterval(x=divs$var, vec=divs$brks, all.inside=TRUE, left.open=ifelse(attributes(divs)$intervalClosure == "right", TRUE, FALSE)))
      ngroups <- length(divs$brks) - 1
      #if empty groups
      if (ngroups > length(nobs)) {
         empty_groups <- (1:ngroups)[ ! (1:ngroups) %in% as.numeric(names(nobs)) ]
         te <- as.table(rep(0, length(empty_groups)))
         names(te) <- empty_groups
         #merge
         nobs <- c(nobs, te)
	 #reorder
         nobs <- nobs[ order(as.numeric(names(nobs))) ]
        }
     as.vector(nobs)
   }
   
   
   

synthetic_US_dataset <- function() {

	# synthetic example dataset for map and calculation examples
	state_codes <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL',
					 'IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE',
					 'NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD',
					 'TN','TX','UT','VT','VA','WA','WV','WI','WY')

	surveydata <- data.frame(state=factor(rep(rep(state_codes, 
							 times=sample(x=c(30,40,60), size=length(state_codes), replace=TRUE)), 
							 times=2)))
	surveydata$year <- factor(rep(c(2009, 2010), each=nrow(surveydata)/2))
	surveydata$educ <- factor(sample(c("HS","BA","Grad"), prob=c(.4,.45,.15), replace=TRUE, size=nrow(surveydata)))
	surveydata$sex <- factor(sample(c("Male","Female"), replace=TRUE, size=nrow(surveydata)))

	surveydata$obs_weight <- stats::runif(n=nrow(surveydata), min=0.8, max=1.5)

	#two income distributions
	surveydata$income <- 100000*stats::rbeta(n=nrow(surveydata), 
									  shape1=ifelse(surveydata$year==2009, 2, 1.5),
									  shape2=ifelse(surveydata$year==2010, 10, 11))
	surveydata$income_ge30k <- ifelse(surveydata$income >=30000, 100, 0)

	surveydata$age <- round(100*stats::rbeta(n=nrow(surveydata), shape1=2, shape2=4))

	#these state and year combinations will not be shaded if they are missing entirely
	surveydata[ surveydata$state == "NV" & surveydata$year == 2009, c("income","income_ge30k")] <- NA
	surveydata[ surveydata$state == "OH" & surveydata$year == 2010, c("income","income_ge30k")] <- NA

	#create some missing values in other variables
	surveydata[ sample(1:nrow(surveydata), size=15), c("income","income_ge30k")] <- NA
	surveydata[ sample(1:nrow(surveydata), size=15), "obs_weight"] <- NA
	surveydata[ sample(1:nrow(surveydata), size=15), "year"] <- NA
	surveydata[ sample(1:nrow(surveydata), size=15), "state"] <- NA


	surveydata
}
