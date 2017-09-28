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
#' @importFrom graphics plot.new
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
#' @param trex   Logical: Plot trend for threshold excesses? DEFAULT: TRUE
#' @param peak   Logical: Plot trend for annual peaks of threshold excesses? DEFAULT: TRUE
#' @param startmonth Integer between 1 and 12. To select which months
#'               are used in the trend analysis. DEFAULT: 1
#' @param nmonths Integer between 1 and 12. Hom many months are used? DEFAULT: 12
#' @param legpos Position of \code{\link{legend}}. DEFAULT: "left"
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
startmonth=1,
nmonths=12,
legpos="left",
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
if(!startmonth %in% 1:12) stop("startmonth must be between 1 and 12, not ", startmonth)
large <- disdf[,n]>=threshold
if(startmonth!=1 | nmonths!=12)
   large <- large & between(as.numeric(format(disdf$date, "%m")), 
                                       startmonth, startmonth+nmonths-1)
large <- which(large)
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
c_trex <- as.vector(coef(lm(doy~year, data=s_trex)))
c_peak <- as.vector(coef(lm(doy~year, data=s_peak)))
l_trex <- paste0(   "All threshold exceedances\nn = ",nrow(s_trex), 
                 ", trend slope = ", round(-10*c_trex[2],1), " days/decade\n")
l_peak <- paste0("Annual peaks above threshold\nn = ",nrow(s_peak), 
                 ", trend slope = ", round(-10*c_peak[2],1), " days/decade")
if(plot) 
  {
  abline(v=seq(1930, 2010, by=5), col=c(1,8)); box()
  if(peak) linReg(doy~year, data=s_peak, add=TRUE, plotrange=1960:2011, col="purple", 
                  colband=addAlpha("purple", alpha=0.08), lwd=2, pos1=NA, ...)
  if(trex) linReg(doy~year, data=s_trex, add=TRUE, plotrange=1960:2011, col="orange", pos1=NA, lwd=2, ...)
  legend(x=legpos, c(ifelse(trex, l_trex, " \n \n "), ifelse(peak, l_peak, " \n ")), 
         lty=0, text.col=c("orange","purple"), bg=addAlpha("white"), box.lty=0)
  # Legend and map:
  colPointsLegend(z=disdf[large,n], nlab=4, title=paste0("Streamflow > ",
                  round(min(disdf[large,n], na.rm=TRUE)),"  [m\U{00B3}/s]"), 
                  y1=0.89, y2=1, colors=col)
  if(map) minimap(n)
  }
# Output
out <- c(RP=RP, threshold=threshold, n=length(large),
         slope_trex=c_trex[2], slope_peak=c_peak[2], 
         inter_trex=c_trex[1], inter_peak=c_peak[1])
return(out)
}
