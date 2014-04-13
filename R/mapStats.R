mapStats <- function(d, 
                     var,
                     stat=c("mean", "quantile"),
                     quantiles=c(.5, .75), 
                     wt.var=NULL,
                     wt.label=TRUE,
                     d.geo.var,
                     by.var=NULL, 
 		     map.file, 
                     map.geo.var = d.geo.var,
		     makeplot=TRUE,
                     ngroups=4,
                     separate=TRUE,                    
                     cell.min=0, 
                     palette="Reds",
                     col=NULL,  
                     map.label=TRUE,
                     map.label.names=map.geo.var,
                     cex.label=.8,
                     col.label="black", 
                     titles=NULL,
                     cex.title=1,
                     var.pretty=var,
                     geo.pretty=d.geo.var,
                     by.pretty=by.var,
                     as.table=TRUE,
                     sp_layout.pars=list(),
                     between=list(y=1),
                     horizontal.fill=TRUE,
                     num.row=1,
                     num.col=1, 
                     ...
                     ) {
 
  
  #make a dummy weights vector if no weights.
  if (is.null(wt.var)) { weight.vector <-  1 }
  else { weight.vector  <- d[, wt.var ] }
  d <- cbind(d, weight.vector)   


  summary.stats<-list()
 
     stat <- tolower(stat)
     
     for (k in stat) {

       if (k %in% c("mean", "total")) {
 
        summary.stats <- append(summary.stats, 
                                 calcStats(d=d, var=var, stat=k, 
                                           d.geo.var=d.geo.var, by.var=by.var, 
                                           wt.var="weight.vector", cell.min=cell.min))                     
           }


       
       else if (k == "quantile") {
    
        summary.stats <- append(summary.stats,
                                calcQuantiles(d=d, var=var, 
                                              quantiles=unique(quantiles),        
                                              d.geo.var=d.geo.var, by.var=by.var,
                                              wt.var="weight.vector", cell.min=cell.min)) 
                            
           }

        }
         

   if (makeplot==TRUE) {

   list_of_plots <- plotStats(statmats=summary.stats,
                              map.file=map.file,
                              d.geo.var=d.geo.var,
                              map.geo.var=map.geo.var, 
                              ngroups=ngroups,
                              separate=separate,
                              palette=palette,
			      col=col,
                              map.label=map.label,
                              map.label.names=map.label.names,
                              cex.label=cex.label,
                              col.label=col.label,
                              titles=titles,
                              cex.title=cex.title,
                              wt.ind=!(is.null(wt.var)),
                              wt.label=wt.label,
                              var.pretty=var.pretty,
                              geo.pretty=geo.pretty,
                              by.pretty=by.pretty,
                              as.table=as.table,
                              sp_layout.pars=sp_layout.pars,
                              between=between,
                              num.col=num.col,
                              ...)

    class(list_of_plots) <- "plotStats"

    print.plotStats(x=list_of_plots, 
                    horizontal.fill=horizontal.fill, 
                    num.row=num.row, 
                    num.col=num.col,
                    ...)


  }

  
  attributes(summary.stats)$variable <- var
  
  summary.stats
}
