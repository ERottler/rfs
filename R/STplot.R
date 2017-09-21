#' @title Plot seasonality trend depending on threshold for "flood"
#' @description Plot seasonality trend depending on threshold for "flood"
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2017
#' @seealso \code{\link{seasTrend}}
#' @keywords hplot aplot
#' @importFrom graphics legend mtext
#' @export
#' @examples
#' load(seasFolder("data/ST.Rdata"))
#' STplot("Koeln")
#'
#' @param name   Character string: Gauge name
#' @param STlist List as created in \code{\link{rfs-package}.R}. DEFAULT: ST
#' @param ylim   Two numbers: Y axis limits. DEFAULT: NA (computed internally)
#' @param map    Logical. Should map be plotted? DEFAULT: TRUE
#'
STplot <- function(name, STlist=get("ST"), ylim=NA, map=TRUE)
{
if(!name %in% names(STlist)) stop(name, " is not in STlist object")
ST2 <- STlist[[name]]
par(mfrow=c(2,1), mar=c(0.5,4,0,0.5), mgp=c(1.7,0.6,0), oma=c(0,0,2,0), las=1)
if(all(is.na(ylim))) ylim <- range(-10*ST2[,c("slope_trex","slope_peak")])
plot(ST2$RP, ST2$slope_trex, type="n", ylab="", xlab="", ylim=ylim, las=1)
##yrects <- seq(0,40, by=5)
##for(yr in yrects) rect(xleft=0, ybottom=yr,   xright=10, ytop=yr+5, col=grey(1-yr/70))
##for(yr in yrects) rect(xleft=0, ybottom=-yr-5,xright=10, ytop=-yr , col=grey(1-yr/70))
abline(h=seq(-100,100,by=10), col="grey")
mtext("Slope of regression line\n flood DOY trend  [days per decade]", side=2, line=2, las=0)
abline(h=0, lwd=2, col="black")
legend("topright", "2010: floods occur earlier in the year than in 1960", cex=0.8, box.col=NA, bg="white")
legend("bottomright",       "below zero: floods occur later in the year", cex=0.8, box.col=NA, bg="white")
box()
title(main=paste0(name, ", ", meta[name, "river"]), outer=TRUE)
lines(ST2$RP, -10*ST2$slope_trex, lwd=2, col="orange")
lines(ST2$RP, -10*ST2$slope_peak, lwd=2, col="purple")
legend("topleft", c("All threshold exceedances", "Annual peaks only"), lwd=2, 
       col=c("orange","purple"), bg="white")
#
par(mar=c(3,5,2,if(map) 11.5 else 5))
plot(ST2$RP, ST2$threshold, type="l", las=1, lwd=2, col="forestgreen", 
     ylab="", xlab="Return Period for threshold  [Years]", yaxt="n")
mtext("Threshold  [m\U{00B3}/s]", side=2, line=3.5, col="forestgreen", las=0)
axis(2, col.axis="forestgreen", mgp=c(3,0.6,0), las=1)
par(new=TRUE)
#
plot(ST2$RP, ST2$n, type="p", col="blue", log="y", ann=FALSE, axes=FALSE)
axis(4, col.axis="blue", mgp=c(3,0.6,0), las=1)
mtext("Number of points\nabove threshold", side=4, line=3.5, col="blue", las=0)
if(map) minimap(name, x1=0.8, x2=1, y1=0, y2=1)
}
