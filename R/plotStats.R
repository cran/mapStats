plotStats <- function(statmats,
                      d.geo.var,
                      map.file,
                      map.geo.var,
                      ngroups,
                      separate,
                      palette,
                      col,
                      map.label,
                      map.label.names,
                      cex.label,
                      col.label,
                      titles,
                      cex.title,
                      wt.ind = FALSE,
                      wt.label,
                      var.pretty,
                      geo.pretty,
                      by.pretty,
                      sp_layout.pars,
                      num.col,
                      ...) {

    
    #sort id column
    map.file@data$sort.id <- 1:nrow(map.file@data)

    #list to contain trellis objects
    list_of_plots <- list()

    
    #do this in case someone wants to use their own titles,
    #such as for percents, etc.

    if (is.null(titles)) {

       wt <- ""    

       if (wt.label == TRUE) {      
         if (wt.ind == FALSE) { wt <- "(unwtd.)" }
         else { wt <- "(wtd.)" }
          }

       
       titles <- paste(names(statmats), "of" , 
                       ifelse(num.col >1 , paste(var.pretty,"\n", sep=""), var.pretty),
                       "by", geo.pretty, wt, sep=" ")
      }
   
     
    #if separate==FALSE then have the same scale for all statistics, and output them
    #on the same page, if possible
    #otherwise, class and colors are defined below in the loop


    
    #set up plotting parameters

    usebrewer <- is.null(col)

    #just in case both are null
    if( usebrewer==FALSE  & is.null(palette) ) { 
        palette <- "Reds" 
        usebrewer <- TRUE
       }

    #modify ngroups as necessary
    if( usebrewer==FALSE) {
        if( ! is.list(col) ) { col <- list(col) }
        
        #expand lengths
        col <- rep_len(col, length.out=length(statmats))
        ngroups <- rep_len( unlist(lapply(col, FUN=length)), length.out=length(statmats))     
    
      }    

     else {
        
        palette <- rep_len(palette, length.out=length(statmats))
        ngroups <- rep_len(pmin(pmax(ngroups,3), 9), length.out= length(statmats))
    
      }     
            
        
    #if want to maintain the same split for all        
     
    if( separate==FALSE ) {
      
      #use the first element of ngroups to choose number 
      class_div <- classInt::classIntervals(var=unlist(sapply(statmats, "[", i=2:ncol(statmats[[1]]))), n=ngroups[1], ...)
      class_div <- jiggleClass(x=class_div)

        if( usebrewer==TRUE ) {  plotclr <- RColorBrewer::brewer.pal(n=ngroups[1], name=palette[1])  }
        else { plotclr <- col[[1]] }
          

      }#end of separate

          


    #loop through output statistics   

     for (k in 1:length(statmats)) {

       map_tmp <- map.file

       #merge each time
       map_tmp@data <- merge(x=map_tmp, y=statmats[[ k ]], 
                             by.x=map.geo.var, by.y=d.geo.var,
                             all.x=TRUE, sort=FALSE)
       map_tmp@data <- map_tmp@data[ order(map_tmp@data$sort.id), ]
 

       #name of columns plotted   
       plotvars <- names(statmats[[ k ]])[2:ncol(statmats[[ k ]])]
      
       if (separate == TRUE ) {
        
           #generate new classes and colors for each statistic

           class_div <- classInt::classIntervals(var=as.vector(as.matrix(map_tmp@data[, plotvars])),
                               n=ngroups[k], ...)
           class_div <- jiggleClass(x=class_div)
    
           if ( usebrewer==TRUE ) { plotclr <- RColorBrewer::brewer.pal(n=ngroups[k], name=palette[k]) }
           else {  plotclr <- col[[k]]  }
    

        }#end of generating classes and colors
      
     
      

       prettynames <- plotvars
       if (! is.null(by.pretty)) { 
           prettynames <- sub(pattern="^(.+?)(?=_\\._)", replacement=by.pretty, x=plotvars, perl=TRUE)
         }
  
       prettynames <- sub(pattern="_._", replacement=" ", x=prettynames, perl=FALSE)
      

       #add labels for geography


       if (map.label == TRUE) {

          sl1 <- list('panel.text', coordinates(map_tmp)[,1], coordinates(map_tmp)[,2], 
                 labels=map_tmp@data[, map.label.names ], cex=cex.label, col=col.label, ...) 
       
               #this allows you to add extra polygons, etc. on top of   
               if( length(sp_layout.pars) >0 )  {
     
                sl2 <- sp_layout.pars
                sl2[[ length(sl2)+1 ]] <- sl1
                   
                }
                else{ sl2 <- sl1 }

            
           
          tmp_plot <- sp::spplot(obj=map_tmp, zcol=plotvars, col.regions=plotclr, 
                                 at=class_div$brks, names.attr=prettynames, 
                                 main=list(label=titles[k], cex=cex.title), sp.layout=sl2, ...)    

              }

       #if no labels
       else {
         
          if( length(sp_layout.pars) >0 ) {

          tmp_plot <- sp::spplot(obj=map_tmp, zcol=plotvars, col.regions=plotclr, 
                                 at=class_div$brks, names.attr=prettynames, 
                                 main=list(label=titles[k], cex=cex.title), sp.layout=sp_layout.pars, ...)
                      }
          else {
	 
         tmp_plot <- sp::spplot(obj=map_tmp, zcol=plotvars, col.regions=plotclr, 
                                 at=class_div$brks, names.attr=prettynames, main=titles[k], ...)
                      }
 
    	}
     

        list_of_plots[[ paste(d.geo.var, names(statmats)[k], sep="_") ]] <- tmp_plot
       

     

     }#finish looping over stat mats

    list_of_plots

}

