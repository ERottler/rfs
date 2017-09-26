#' @title Seasonality trend 1960:2010
#' @description Seasonality trend 1960:2010
#' @return Vector with RP, threshold, lm coefficients both for 
#'         all threshold exceedances and annual peaks of those
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2017
#' @seealso \code{\link{rfsApp}("trend")}
#' @keywords aplot
#' @importFrom extremeStat distLextreme
#' @importFrom berryFunctions almost.equal seasonality between linReg colPointsLegend addAlpha
#' @importFrom stats coef lm
#' @export
#' @examples
#' load(seasFolder("data/dismeta.Rdata"))
#' seasTrend("Koeln")
#'
#' @param n      Character: Name of gauge to be analyzed, see \code{\link{gnames}}
#' @param disdf  Dataframe with columns "date" and \code{n}. DEFAULT: dis
#' @param RP     Return Period for threshold. DEFAULT: 1.2
#' @param plot   Logical: plot result? DEFAULT: TRUE
#' @param map    Logical: draw \code{\link{minimap}}? DEFAULT: FALSE
#' @param trex,peak Logical: Plot trendline for (annual peaks of) threshold excesses 
#                with \code{\link{linReg}}? DEFAULT: both TRUE
#' @param shift  Shift passed to \code{\link{seasonality}}. DEFAULT: 61
#' @param \dots  Further arguments passed to \code{\link{linReg}}
#' 
seasTrend <- function(
n, 
disdf=get("dis"),
RP=1.2, 
plot=TRUE,
map=FALSE, 
trex=TRUE,
peak=TRUE,
shift=61,
...
)
{
# Get threshold from RP:
threshold <- if(RP>=1) thresfuns[[n]](RP) else 0
if(RP>10) 
 {
 plot.new()
 text(0.5,0.5,paste0("RP is ", RP, ", but may not be larger than 10."))
 return()
 }
# Seasonality for all values > threshold:
large <- which(disdf[,n]>=threshold)
meta <- get("meta")
col <- seqPal(100)
if(!trex) col <- NA
s <- seasonality(disdf[large,"date"], disdf[large,n], shift=shift, nmax=1, pch=15,
            main=paste0(n, ", ", meta[n,"river"]), returnall=TRUE, col=col,
            maxargs=list(col=if(peak) "purple" else NA), adj=0.4,
            drange=1930:2012, legend=FALSE, plot=ifelse(plot,1,0), hlines=TRUE)
# Trend line:
s_trex <- s$data  [between(s$data$year,   1960, 2010),]
s_peak <- s$annmax[between(s$annmax$year, 1960, 2010),]
if(plot) 
  {
  if(peak) linReg(doy~year, data=s_peak, add=TRUE, plotrange=1960:2011, col="purple", 
         colband=addAlpha("purple", alpha=0.08), lwd=2, pos1=NA, ...)
  if(trex) linReg(doy~year, data=s_trex, add=TRUE, plotrange=1960:2011, col="orange", pos1=NA, lwd=2, ...)
  legend("left", c("All threshold exceedances", "Annual peaks above threshold"), lty=0, 
         text.col=c("orange","purple"), bg=addAlpha("white"), box.lty=0)
  # Legend and map:
  colPointsLegend(z=disdf[large,n], nlab=4, title=paste0("Streamflow > ",
                  round(min(disdf[large,n], na.rm=TRUE)),"  [m\U{00B3}/s]"), 
                  y1=0.89, y2=1, colors=col)
  if(map) minimap(n)
  }
c_trex <- as.vector(coef(lm(doy~year, data=s_trex)))
c_peak <- as.vector(coef(lm(doy~year, data=s_peak)))
# Output
out <- c(RP=RP, threshold=threshold, n=length(large),
         slope_trex=c_trex[2], slope_peak=c_peak[2], 
         inter_trex=c_trex[1], inter_peak=c_peak[1])
return(out)
}
