#' @title Seasonality trend 1960:2010
#' @description Seasonality trend 1960:2010
#' @return Vector with RP, threshold, lm coefficients both for 
#'         all threshold exceedances and annual peaks of those
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2017
#' @seealso \code{\link{rfsApp}("trend")}
#' @keywords aplot
#' @importFrom extremeStat distLextreme
#' @importFrom berryFunctions almost.equal seasonality between linReg colPointsLegend
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
#' @param trendpeakonly Logical: only use annual peaks of threshold excesses 
#'               for plotting with \code{\link{linReg}}? DEFAULT: FALSE
#' @param shift  Shift passed to \code{\link{seasonality}}. DEFAULT: 61
#' @param \dots  Further arguments passed to \code{\link{linReg}}
#' 
seasTrend <- function(
n, 
disdf=get("dis"),
RP=1.2, 
plot=TRUE,
map=FALSE, 
trendpeakonly=FALSE,
shift=61,
...
)
{
# Get threshold from RP:
annMax <- annualMax(disdf$date, disdf[,n], shift=0)
if(RP>1)
{
dle <- distLextreme(annMax$max, gpd=FALSE, sel="gev", RPs=RP, quiet=TRUE)
threshold <- dle$returnlev["gev",1]
} else
if(almost.equal(RP, 1)) threshold <- min(annMax$max, na.rm=TRUE)
else threshold <- 0
# Seasonality for all values > threshold:
large <- which(disdf[,n]>=threshold)
meta <- get("meta")
s <- seasonality(disdf[large,"date"], disdf[large,n], shift=shift, nmax=1, pch=15,
            main=paste0(n, ", ", meta[n,"river"]), returnall=TRUE, adj=0.4,
            drange=1930:2012, legend=FALSE, plot=ifelse(plot,1,0), hlines=TRUE)
# Trend line:
s_trex <- s$data  [between(s$data$year,   1960, 2010),]
s_peak <- s$annmax[between(s$annmax$year, 1960, 2010),]
if(plot) 
  {
  s_plot <- if(trendpeakonly) s_peak else s_trex
  linReg(doy~year, data=s_plot, add=TRUE, plotrange=1960:2011, pos1=NA, ...)
  # Legend and map:
  colPointsLegend(z=disdf[large,n], nlab=4, title="Streamflow  [m\U{00B3}/s]", 
                  y1=0.89, y2=1)
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
