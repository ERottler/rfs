#' @title DEM plotting function
#' @description Plots DEM as (small inset) map with desired stations highlighted
#' @return None
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
# @seealso \code{\link{help}}, \code{\link{help}}
#' @keywords hplot
#' @importFrom png readPNG
#' @importFrom berryFunctions checkFile smallPlot owa
#' @importFrom graphics plot rasterImage points text
#' @export
#' @examples
#' plotmap("Koeln") ; axis(1) ; axis(2)
#' plotmap(c("Rees","Mainz","Worms","Seifen"), label=TRUE, col=1:4)
#' plot(1:10)
#' minimap("Koeln")
#' minimap(0) # no stations
#' minimap("Cochem", allargs=list(pch=3,cex=0.5))
#' 
#' stats <- gnames("app")[c(1,5,8,12)]
#' minimap(stats, expr=text(meta[name,"lon"], meta[name,"lat"], name, col=2, adj=c(0,1)))
#' 
#' op <- par(mfrow=c(2,2))
#' plot(1:10)
#' plot(1:10)
#' plot(1:10)
#' minimap("Koeln")
#' minimap("Mainz", spargs=list(outer=TRUE))
#' par(op)
#' 
#' pdf("test.pdf", height=5)
#' hist(rnorm(100))
#' minimap(c("Koeln","Rheinfelden"), y1=0.1, y2=0.9, x2=0.9)
#' minimap() # all dots
#' dev.off()
#' #berryFunctions::openFile("test.pdf")
#' unlink("test.pdf")
#' 
#' @param name        Station name(s) to be plotted with red crosses
#' @param metadf      Dataframe with metadata. DEFAULT: meta
#' @param x1,x2,y1,y2 Relative location of minimap
#' @param spargs      List of arguments passed to \code{\link{smallPlot}}. DEFAULT: NULL
#' @param pch,lwd,col Point properties
#' @param allargs     List of arguments passed to all gauge locations first
#' @param expr        Expression to be executed after points, see example. DEFAULT: NULL
#' @param label       Logical (vector): Label gauges? DEFAULT: FALSE
#' @param \dots       Further arguments passed from \code{minimap} to 
#'                    \code{plotmap} and then to \code{\link{points}}
#' 
minimap <- function(
 name,                # station name(s) to be plotted with red crosses
 x1=0.1,x2=x1+0.2,    # relative location of minimap
 y1=0.7,y2=y1+0.27,
 spargs=NULL,
 ...
 )
{
defargs <- list(expr=substitute(plotmap(name=name, ...)), x1=x1,x2=x2,y1=y1,y2=y2,
                mar=c(0,0,0,0), bg="transparent", border=NA)
do.call(smallPlot, owa(defargs, spargs))
}



#' @export
#' @rdname minimap
plotmap <- function(
 name,                # station name(s) to be plotted with red crosses
 metadf=meta,
 pch=3,               # point properties
 lwd=3,
 col="red",
 allargs=NULL,        # list of arguments passed to all points first
 expr=NULL,
 label=FALSE,         # Label gauges?
 ...
 )
{
dempath <- system.file("extdata/dem_small.png", package="rfs")
checkFile(dempath)
meta <- metadf
plot(x=c(5.11, 12.02), y=c(46.24, 52.04), type="n", yaxs="i", xaxs="i", axes=F, asp=1.4, ann=F)
rasterImage(png::readPNG(dempath),
                      xleft=5.11, ybottom=46.24-0.05,
                     xright=12.02,   ytop=52.04+0.07)
if(!is.null(allargs)) do.call(points, c(list(x=meta$lon, y=meta$lat), allargs))
points(lat~lon, data=meta[name,], pch=pch, lwd=lwd, col=col, ...)
lmeta <- meta[name,][label,]
if(any(label)) text(lmeta$lon, lmeta$lat, sub("_Rheinhalle","",lmeta$name),
                    col=col, adj=c(-0.1,1.1) )

eval(substitute(expr))
invisible()
}
