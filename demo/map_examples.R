#download shapefile
usMap <- readShapePoly(system.file("shapes/usMap.shp", package="mapStats")[1])

#create synthetic survey dataset


state_codes <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL',
                 'IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE',
                 'NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD',
                 'TN','TX','UT','VT','VA','WA','WV','WI','WY')

surveydata <- data.frame(state=factor(rep(rep(state_codes, 
                         times=sample(x=c(15,20,30), size=length(state_codes), replace=TRUE)), 
                         times=2)))
surveydata$year <- rep(c(2009, 2010), each=nrow(surveydata)/2)
surveydata$obs_weight <- runif(n=nrow(surveydata), min=0.8, max=1.5)

#two income distributions
surveydata$income <- 100000*rbeta(n=nrow(surveydata), 
                                  shape1=ifelse(surveydata$year==2009, 2, 1.5),
                                  shape2=ifelse(surveydata$year==2010, 10, 11))
surveydata$income_ge30k <- ifelse(surveydata$income >=30000, 100, 0)


#these state and year combinations will not be shaded if they are missing entirely
surveydata[ surveydata$state == "NV" & surveydata$year == 2009, c("income","income_ge30k")] <- NA
surveydata[ surveydata$state == "OH" & surveydata$year == 2010, c("income","income_ge30k")] <- NA

#create some missing values in other variables
surveydata[ sample(1:nrow(surveydata), 15), c("income","income_ge30k")] <- NA
surveydata[ sample(1:nrow(surveydata), 15), "obs_weight"] <- NA
surveydata[ sample(1:nrow(surveydata), 15), "year"] <- NA
surveydata[ sample(1:nrow(surveydata), 15), "state"] <- NA


head(surveydata)




#Calculate weighted mean and 40th, 50th, and 60th quantiles of variable income,
#by state and survey year.  Display using red sequential color palette with 6 groups.
#In the titles, rename 'income' by 'household income'.

#print each statistic on a separate page, using separate color breaks for each.  Will print 4 pages.       


mapStats(d=surveydata, var="income", by.var="year",
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean","quantile"), quantiles=c(.4, .5, .6),
         palette="Reds", ngroups=6, var.pretty="household income",
         geo.pretty="state", by.pretty="Year", map.label=TRUE)




#print statistics on 2 pages, with separate color breaks, with 1 column and 3 rows, 
#and use layout argument to position panels within each plot properly .
#eliminate labels because too crowded.     

mapStats(d=surveydata, var="income", by.var="year",
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean","quantile"), quantiles=c(.4, .5, .6),
         palette="Reds", ngroups=6, var.pretty="household income",
         geo.pretty="state", by.pretty="Year", map.label=FALSE,
         num.col=1, num.row=3, layout=c(2,1))

#plot all four statistics over all four years, on one page with 2 rows and 2 columns,
#using vertical order. Shrink labels so they fit better.  

mapStats(d=surveydata, var="income", 
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean","quantile"), quantiles=c(.4, .5, .6),
         palette="Reds", ngroups=6, var.pretty="household income",
         geo.pretty="state", by.pretty="Year", map.label=TRUE,
         cex.label=.5, num.col=2, num.row=2, horizontal.fill=FALSE)


#do the above example, except with a different palette for each to distinguish them 
#since separate breaks

mapStats(d=surveydata, var="income", 
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean","quantile"), quantiles=c(.4, .5, .6),
         palette=c("Reds","Greens","Blues","Greys"), ngroups=6, 
         var.pretty="household income", geo.pretty="state", 
         by.pretty="Year", map.label=TRUE, cex.label=.5, 
         num.col=2, num.row=2, horizontal.fill=FALSE)



#use the above with the same palette except with the same color breaks for all statistics,

mapStats(d=surveydata, var="income", 
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean","quantile"), quantiles=c(.4, .5, .6),
         palette="Reds", ngroups=6, var.pretty="household income",
         geo.pretty="state", by.pretty="Year", map.label=TRUE,
         cex.label=.5, num.col=2, num.row=2, horizontal.fill=FALSE, 
         separate=FALSE)


#do the same except use jenks style to calculate the breaks (classIntervals)

mapStats(d=surveydata, var="income", 
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean","quantile"), quantiles=c(.4, .5, .6),
         palette="Reds", ngroups=6, var.pretty="household income",
         geo.pretty="state", by.pretty="Year", map.label=TRUE,
         cex.label=.5, num.col=2, num.row=2, horizontal.fill=FALSE, 
         separate=FALSE, style="jenks")


#an example doing the above with HCL instead to override RColorBrewer
#differing numbers of groups, ngroups overridden
#three palettes will be reused over four plots

red_hcl <- rev(colorspace::sequential_hcl(n=5, h=0))
green_hcl <- rev(colorspace::sequential_hcl(n=7, h=120))
blue_hcl <- rev(colorspace::sequential_hcl(n=3, h=240))


mapStats(d=surveydata, var="income", 
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean","quantile"), quantiles=c(.4, .5, .6),
         col=list(red_hcl, green_hcl, blue_hcl),  
         var.pretty="household income", geo.pretty="state", 
         by.pretty="Year", map.label=TRUE, cex.label=.5, 
         num.col=2, num.row=2, horizontal.fill=FALSE)




#Example with additional parameters to control the spacing of the shapefile within the panel

#plot bounds
bounds <- attributes(usMap)$bbox
ranges <- abs(bounds[,2]-bounds[,1])
new_bounds <- bounds
new_bounds[,1] <- new_bounds[,1] - 0.1*ranges
new_bounds[,2] <- new_bounds[,2] + 0.1*ranges
xbox <- new_bounds[1,]
ybox <- new_bounds[2,]


mapStats(d=surveydata, var="income", by.var="year",
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean"), palette="Reds", ngroups=6, 
         var.pretty="household income", geo.pretty="state", 
         by.pretty="Year", map.label=TRUE, xlim=xbox, ylim=ybox)



#To calculate percentages of class variables, create an indicator variable, calculate its mean,
#and override the default titles to say you are calculating the percentage.  Here we illustrate by
#calculating the percent of respondents by state that have income above 30,000.


mapStats(d=surveydata, var="income_ge30k", 
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean"), col.pal="Reds", 
         titles="Percent of respondents with income\nat least $30,000")

#Do the same as above but add additional visual elements
#Normally you may want to overlay with a different shapefile showing other elements,
#such as rivers, or something else relevant to the question
#You can overlay with points to label specific areas on the map, or add a compass point

#subset the map, plot a few green
state_subset <- usMap[ usMap$STATE_ABBR %in% c("TX", "AK", "CA"), ]
shaded_green <- list("sp.polygons", state_subset, fill="green")

#add a red triangle on the map
attributes(usMap)$bbox

red_triangle <- list("sp.points", matrix(c(0, 2000000), ncol=2), 
                      pch=2, col="red", cex=4, lwd=5)

#combine the two things above in a list

map_overlay <- list(shaded_green, red_triangle)

mapStats(d=surveydata, var="income_ge30k", 
         wt.var="obs_weight", map.file=usMap,
         d.geo.var="state", map.geo.var="STATE_ABBR",
         stat=c("mean"), palette="Reds", 
         titles="Percent of respondents with income at least $30,000",
         sp_layout.pars=map_overlay)




#color multiple regions:  create code for Census region

west <- c("AK","AZ","WA","MT","OR","ID","WY","CA","NV","UT","CO","NM","HI")
midwest <- c("ND","MN","SD","NE","IA","WI","MI","OH","IN","IL","MO","KS")
neast <- c("PA","NY","NJ","CT","RI","MA","VT","NH","ME")
south <- c("TX","OK","AR","LA","MS","KY","TN","AL","FL","GA","SC","NC","VA","WV","DC","MD","DE")

surveydata$region[ surveydata$state %in% west ] <- "West"
surveydata$region[ surveydata$state %in% midwest ] <- "Midwest"
surveydata$region[ surveydata$state %in% neast ] <- "Northeast"
surveydata$region[ surveydata$state %in% south ] <- "South"

usMap$region[ usMap$STATE_ABBR %in% west ] <- "West"
usMap$region[ usMap$STATE_ABBR %in% midwest ] <- "Midwest"
usMap$region[ usMap$STATE_ABBR %in% neast ] <- "Northeast"
usMap$region[ usMap$STATE_ABBR %in% south ] <- "South"


#calculate mean and median by Census region, using a combined scale for 
#both statistics (separate = FALSE), and stack vertically

mapStats(d=surveydata, var="income", wt.var="obs_weight", map.file=usMap,
         d.geo.var="region", map.geo.var="region",
         stat=c("mean", "quantile"), quantiles=c(.5),
         palette="Reds", ngroups=4, var.pretty="household income",
         geo.pretty="region", map.label=TRUE, cex.label=.6, separate=FALSE,
         horizontal.fill=FALSE, num.row=2)
