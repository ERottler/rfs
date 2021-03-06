#' @title Visualize seasonality of discharge
#' @description  Visualize seasonality of discharge via quantile per DOY.
#' @return Nothing, only plots
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2017
#' @seealso \code{\link{qdoyCompute}}, \code{\link{qdoyVisPeriods}}, \code{\link{rfs-package}}
#' @keywords aplot
#' @importFrom berryFunctions lim0 textField
#' @importFrom graphics lines plot
#' @export
#' @examples
#' # see   ?rfs-package
#' 
#' @param qd    Object returned by \code{\link{qdoyCompute}}
#' @param dist  Distribution function: gev or empirical. DEFAULT: "gev"
#' @param RPs   Return periods to be plotted (must be present in \code{qd}).
#'              DEFAULT: c(1.111,2,10,50,200)
#' @param cols  Colors for each line. Must have "RP.x" as names (or it is tried to set them).
#'              DEFAULT: \code{\link{RPcols}}
#' @param main  Plot title. DEFAULT: dist
#' @param ylim  Y axis limits. DEFAULT: NA (computed internally from qd)
#' @param add   Logical: add to existing plot?
#' @param shift Shift of year break, see \code{\link{qdoyCompute}}. DEFAULT: 0
#' @param labelindex Index along X axis at which to label lines.
#'              Can be vectorized for each RP. 0 to suppress. DEFAULT: 200
#' @param \dots Further arguments passed to \code{\link{plot}}
#' 
qdoyVis <- function(
qd,
dist="gev",
RPs=c(1.111,2,10,50,200),
cols=RPcols,
main=dist,
ylim=NA,
add=FALSE,
shift=0,
labelindex=200,
...)
{
RPs <- paste0("RP.",RPs)
if(all(is.na(ylim))) ylim <- lim0(qd[dist, RPs, ])
if(!add)
  {
  plot(1:366, 1:366, ylim=ylim, las=1, type="n", main=main, xaxt="n", xaxs="i",
       xlab="", ylab="discharge  [m\U{00B3}/s]", ...)
  seasAxis(shift=shift)
  }

if(is.null(names(cols))) names(cols) <- RPs

for(i in RPs) lines(1:366, qd[dist,i,], col=cols[i], lwd=2)
labelindex <- rep(labelindex, length(RPs))
names(labelindex) <- RPs
if(!all(labelindex==0))
for(i in RPs) textField(x=labelindex[i],
                       y=mean(qd[dist,i,labelindex[i]+(-10:10) ]),
                       labels=i, col=cols[i])
}
