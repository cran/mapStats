calcStats <-function(d, var, stat=c("mean", "total"),
                     d.geo.var, by.var=NULL, wt.var=NULL, cell.min=0) {
    
    
    #list to contain the stat matrices
    v.stats <- list()

    if (is.null(wt.var)) {     
        d$weight.vector <- 1   
        wt.var="weight.vector"
     }

    #preserve missing combinations	
    d[, d.geo.var ] <- as.factor( d[, d.geo.var ] ) 
    if (! is.null(by.var)) {  d[, by.var ] <- as.factor( d[, by.var ] )  }

    #formula for the weight variable

    wt.form <- as.formula(paste("~", wt.var, sep=""))
 
    #define formulas for variables
    var.form<-as.formula(paste("~",var))
    
    
    #eliminate missing values in the weight variable or variable or class
    #even if have na.rm, need to account for cases where weight variable is missing
    #or if class combinations are NA entirely (it's okay if removed but can't be NA)

    d <- d[ (! is.na(d[, wt.var ])) & (! is.na(d[, var ])) & (! is.na(d[, d.geo.var])), ] 
  
    #by variable formula
    by.form <- paste("~", d.geo.var)

    
    if( ! is.null(by.var)) {
       by.form <- paste(by.form, by.var, sep=" + ")
       d <- d[ ! is.na(d[, by.var ]), ]
      }
    by.form <- as.formula(by.form)


          		

    
    #define data design
    data.des <- svydesign(ids=~1, data=d, weights=wt.form) 
  
    for (k in stat) {

       #define the survey function to use
       svyfunc <- match.fun(paste("svy", k, sep=""))
  
  
       #variable statistic:
       stat.mat <- svyby( formula=var.form, by=by.form, design=data.des, 
			          FUN=svyfunc, keep.var=FALSE, na.rm=TRUE,
			          drop.empty.groups=FALSE)
      

       #now reshape the matrix

       if (! is.null(by.var)) {
         byvar.range <- names(table(stat.mat[, by.var ]))
         cast.form <- as.formula(paste(d.geo.var, by.var, sep="~")) 
	
         stat.mat <- reshape2::dcast(data=stat.mat, formula=cast.form, value.var="statistic")
         n <- ncol(stat.mat)
         nlevels <- length(byvar.range)
      

         #column names we will plot
         plotcolumns <- paste(by.var, byvar.range, sep="_._")     
         names(stat.mat)[ (n-nlevels+1):n] <- plotcolumns       

       }



       #now eliminate small cells:
       if (cell.min >0 )  {
            
           if (is.null(by.var)) { t.c <- as.matrix(table(d[, d.geo.var ])) }
           else { t.c <- as.matrix(table( d[, c(d.geo.var, by.var)]) ) }

           t.c <- t.c[ rowSums(t.c, na.rm=TRUE) >0, ]
           stat.mat[ , 2:ncol(stat.mat)][ t.c < cell.min ] <- NA
     
          }

        
       
         rownames(stat.mat) <- NULL
   

     v.stats[[ Hmisc::capitalize(k) ]] <- stat.mat

     }
 
  
  attributes(v.stats)$variable <- var
  v.stats  

 }

