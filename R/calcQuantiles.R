calcQuantiles <- function(d, var, quantiles=c(0.50, .75),
                          d.geo.var, by.var=NULL, wt.var=NULL, cell.min=2) {                          

    

    #list to contain the quantile stat matrices
    q.stats <- list()

  
   
    #quantile regression without the intercept and lower order interactions
    #if have two levels, need to create new vector because problems with rq and survey
 
    #need to eliminate NA values for geography
    d <- d[ ! is.na(d[, d.geo.var ]), ]
    
    if (! is.null(by.var)) {   
       d <- d[ ! is.na(d[, by.var ]), ]   
       d$level.combs <- paste(d[, d.geo.var], d[, by.var], sep="~!~")   
         }
    else  {   d$level.combs <- d[, d.geo.var]   }
 
    #weight vector

    if (! is.null(wt.var)) {
        d <- d[ ! is.na(d[, wt.var]), ]
        wt.var <- d[,wt.var]

      }
    else { 
        #create unit weights
  
        d$weight.vector <- 1    
        wt.var <- d$weight.vector
     }
   


    #eliminate ones with 0 or 1 elements

    t.c <- table( d$level.combs )
    
    small.cells <- names(t.c)[ t.c < max(2, cell.min) ]
    #set these as NA so don't get calculated
    d[ d$level.combs %in% small.cells , c( var, "level.combs") ] <- NA
    d$level.combs <- factor( d$level.combs ) 


    #quantile regression on combinations without intercept

    var.form <- as.formula(paste(
                 paste(var, "level.combs", sep="~"),
                 " -1", sep=""))
 
    stat.mat <- quantreg::rq(formula=var.form, tau=quantiles, weights=wt.var, data=d)
    stat.mat <- as.data.frame(as.matrix(stat.mat$coefficients))
    

    qnames <- paste("Q", round(100*quantiles), sep="")

    #make some modifications to names
    qnames[ qnames == "Q0" ] <- "Minimum"
    qnames[ qnames == "Q100" ] <- "Maximum"
    qnames[ qnames == "Q50" ] <- "Median"

    colnames(stat.mat) <- qnames  
    #split of level.comb text
    levelnames <- substr(rownames(stat.mat), 12, nchar(rownames(stat.mat)))
    levelnames <- strsplit(levelnames, split="~!~")
   
    #extract first
    stat.mat[, d.geo.var ] <- sapply(levelnames, function(y) y[1])
         
    if (! is.null(by.var)) {
       stat.mat[, by.var ] <- factor(sapply(levelnames, function(y) y[2]))
     }
      
    rownames(stat.mat) <- NULL


    #now we have the factor levels with stat.mat


      
    #loop over each 
    for (k in qnames) {

       if (is.null( by.var )) {
          q.stats[[ k ]] <- stat.mat[, c(d.geo.var, k) ]
       }

       else {
         cast.form <- as.formula(paste(d.geo.var, by.var, sep="~")) 
         q.mat <- reshape2::dcast(data=stat.mat, formula=cast.form, value.var=k)
 
         byvar.range <- names(table(stat.mat[, by.var ]))
         n <- ncol(q.mat)
         
         nlevels <- length(byvar.range) 
         plotcolumns <- paste(by.var, byvar.range, sep="_._")
       
         names(q.mat)[ (n-nlevels+1):n] <- plotcolumns       

         q.stats[[ k ]] <- q.mat
        
        }
    }
    
     attributes(q.stats)$variable <- var
     q.stats

  }

