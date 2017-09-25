#' Seasonality axis
#' 
#' Add monthly axis labels on seasonality plots
#' 
#' @return nothing
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{qdoyVis}}
#' @keywords aplot
#' @export
#' @importFrom berryFunctions owa monthLabs
#' @importFrom graphics axis abline
#' @examples
#' plot(1:366, xaxt="n", xaxs="i")
#' seasAxis(shift=117)
#' seasAxis(shift=117, targs=list(col.ticks="red"), lwd=3, col="purple")
#' 
#' plot(1:366, yaxt="n", ylim=c(366,0))
#' seasAxis(shift=117, side=2)
#' 
#' @param shift   Number of days to move the year-break to.
#'                E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param janline Logical: Should horizontal line be plotted at
#'                January 1st if \code{shift!=0}? DEFAULT: TRUE
#' @param mlabs   Labels for the months. DEFAULT: J,F,M,A,M,J,J,A,S,O,N,D
#' @param mline   Placement of labels (Distance from axis). DEFAULT: 0.7
#' @param xlab    Label for the axis. DEFAULT: "Month"
#' @param xline   Placement of axis label. DEFAULT: 2
#' @param side    Margin side for \code{\link{axis}}. DEFAULT: 1
#' @param targs   Optional list of arguments passed to \code{\link{axis}}
#'                for month separating ticks. DEFAULT: NULL
#' @param largs   Optional list of arguments passed to \code{\link{axis}}
#'                for mid-month labels. DEFAULT: NULL
#' @param xargs   Optional list of arguments passed to \code{\link{title}}
#'                for general axis label. DEFAULT: NULL
#' @param \dots   Further arguments passed to \code{\link{abline}} for janline
#'                (excluding \code{v}).
#' 
seasAxis <- function(
  shift=0,
  janline=TRUE,
  mlabs=substr(month.abb,1,1),
  mline=0.7,
  xlab="Month",
  xline=2,
  side=1,
  targs=NULL,
  largs=NULL,
  xargs=NULL,
  ...
)
{

shift <- checkShift(shift)
# Axis labelling positions:
tick <- monthLabs(2004,2004, npm=1) + shift
labs <- tick + 15
tdoy <- as.numeric(format(tick,"%j"))
ldoy <- as.numeric(format(labs,"%j"))

# monthly labels:
tdef <- list(side=side, at=tdoy, labels=FALSE, las=1)
ldef <- list(side=side, at=ldoy, labels=mlabs, tick=FALSE, mgp=c(3,mline,0), las=1)
do.call(axis, owa(tdef, targs))
do.call(axis, owa(ldef, largs))

ax1 <- side %in% c(1,3)
# vertical line at Jan 1:
if(janline & shift!=0) if(ax1) abline(v=shift+1, ...) else abline(h=shift+1, ...)

# Label for complete Axis:
xdef <- list(xlab=xlab, line=xline)
if(!ax1) names(xdef)[1] <- "ylab"
do.call(title, owa(xdef, xargs))
}

