mapStats <- function(d=NULL, 
                     main.var,
                     stat=c("mean","quantile"),
                     quantiles=c(.5,.75), 
                     wt.var=NULL,
                     wt.label=TRUE,
                     d.geo.var,
                     by.var=NULL,
					 map.file=NULL, 
                     map.geo.var=d.geo.var,
					 makeplot=TRUE,
                     ngroups=4,
                     separate=1,                    
                     cell.min=0, 
                     paletteName="Reds",
                     colorVec=NULL,  
                     map.label=TRUE,
                     map.label.names=map.geo.var,
                     cex.label=.8,
                     col.label="black", 
                     titles=NULL,
                     cex.title=1,
                     var.pretty=main.var,
                     geo.pretty=map.geo.var,
                     by.pretty=by.var,
                     as.table=TRUE,
                     sp_layout.pars=list(),
                     between=list(y=1),
                     horizontal.fill=TRUE,
                     plotbyvar=ifelse( separate==1 & length(main.var)>1, FALSE, TRUE),
                     num.row=1,
                     num.col=1, 
                     ...
                     ) {
     
	# some argument checks
	if (is.null(map.file)) {
		if (makeplot) {
			message('no map.file was provided, so no plots will be drawn')
			makeplot <- FALSE
		}
	}
	else {
		if (! ("SpatialPolygonsDataFrame" %in% class(map.file))) {
			stop('map.file should be an object of class SpatialPolygonsDataFrame.  See map_examples')
		}
		if (is.null(d)) {
			message('using @data data.frame associated with map.file')
			d <- map.file@data
		}
	}
   
   stat <- match.arg(tolower(stat), c("mean", "quantile", "total", "var", "sd"), several.ok = TRUE)
   paletteName <- match.arg(paletteName, c("Reds","Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd","PuBu", "PuBuGn", 
                                   "PuRd","Purples","RdPu","YlGn","YlGnBu","YlOrBr","YlOrRd"), several.ok = TRUE)
   separate <- match.arg( as.character(separate), c("0","1","2","3","TRUE","FALSE"), several.ok=FALSE)   

   summary.stats <- calcStats(d=d, main.var=main.var, d.geo.var=d.geo.var, stat=tolower(stat), quantiles=quantiles,
                             by.var=by.var, wt.var=wt.var, cell.min=cell.min)

   if (makeplot==TRUE) {

      
	   #drop the freqs
	   list_of_plots <- plotStats(statmats=summary.stats,
								  map.file=map.file,
								  d.geo.var=d.geo.var,
								  map.geo.var=map.geo.var, 
								  ngroups=ngroups,
								  separate=separate,
								  paletteName=paletteName,
								  colorVec=colorVec,
								  map.label=map.label,
								  map.label.names=map.label.names,
								  cex.label=cex.label,
								  col.label=col.label,
								  titles=titles,
								  cex.title=cex.title,
								  wt.ind=!(missing(wt.var)),
								  wt.label=wt.label,
								  var.pretty=var.pretty,
								  geo.pretty=geo.pretty,
								  by.pretty=by.pretty,
								  as.table=as.table,
								  sp_layout.pars=sp_layout.pars,
								  plotbyvar=plotbyvar,
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
   invisible(summary.stats)
}
