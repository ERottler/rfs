#' @title DEM plotting function
#' @description Plots DEM as small inset map with desired stations highlighted
#' @return None
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
# @seealso \code{\link{help}}, \code{\link{help}}
#' @keywords hplot
#' @importFrom png readPNG
#' @importFrom berryFunctions checkFile smallPlot
#' @importFrom graphics plot rasterImage points
#' @export
#' @examples
#' load(seasFolder("data/dismeta.Rdata"))
#' plot(1:10)
#' minimap("Koeln")
#' minimap(0) # no stations
#' minimap("Cochem", allargs=list(pch=3,cex=0.5))
#' 
#' pdf("test.pdf", height=5)
#' hist(rnorm(100))
#' minimap(c("Koeln","Rheinfelden"), y1=0.1, y2=0.9, x2=0.9)
#' minimap() # all dots
#' dev.off()
#' berryFunctions::openFile("test.pdf")
#' unlink("test.pdf")
#' 
#' @param name        Station name(s) to be plotted with red crosses
#' @param metadf      Dataframe with metadata. DEFAULT: meta
#' @param x1,x2,y1,y2 Relative location of minimap
#' @param pch,lwd,col Point properties
#' @param allargs     List of arguments passed to all gauge locations first
#' @param \dots       Further arguments passed to \code{\link{points}}
#'
minimap = function(
 name,                # station name(s) to be plotted with red crosses
 metadf=meta,
 x1=0.1,x2=x1+0.2,    # relative location of minimap
 y1=0.7,y2=y1+0.27,
 pch=3,               # point properties
 lwd=3,
 col="red",
 allargs=NULL,        # list of arguments passed to all points first
 ...
 )
{
dempath <- system.file("extdata/dem_small.png", package="rfs")
checkFile(dempath)
meta <- metadf
smallPlot({
plot(x=c(5.11, 12.02), y=c(46.24, 52.04), type="n", yaxs="i", xaxs="i", axes=F, asp=1.4, ann=F)
rasterImage(png::readPNG(dempath),
                      xleft=5.11, ybottom=46.24-0.05,
                     xright=12.02,   ytop=52.04+0.07)
if(!is.null(allargs)) do.call(points, c(list(x=meta$lon, y=meta$lat), allargs))
points(lat~lon, data=meta[name,], pch=pch, lwd=lwd, col=col, ...)
},
x1=x1,x2=x2,y1=y1,y2=y2, mar=c(0,0,0,0), bg="transparent", border=NA)
}

