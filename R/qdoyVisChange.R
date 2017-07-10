#' @title Visualize seasonality changes
#' @description Visualize seasonality changes: compare periods
#' @return Nothing?
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{qdoyPeriods}}, \code{\link{qdoyCompute}}
#' @keywords hplot
#' @importFrom graphics abline box lines par title
#' @importFrom berryFunctions seqPal lim0 pretty2 colPointsLegend monthLabs textField
#' @export
#' @examples
#' # see qdoyPeriods
#'
#' @param name        Gauge station name
#' @param seaslist    Named list as e.g. returned by \code{lapply(SOMENAMES, qdoyPeriods)}
#' @param qdp         Array with qualtiles per doy and period as retuned by
#'                    \code{\link{qdoyPeriods}}. DEFAULT: \code{seaslist[[name]]}
#' @param dist        Distribution function: gev or empirical. DEFAULT: "gev"
#' @param metadf      Dataframe with metadata. DEFAULT: meta
#' @param steps,RPs   Which of the periods and quantiles should be plotted?
#'                    DEFAULT: 1:3 (all of them), c(1.111,2,10,50,200)
#' @param map         Should \code{\link{minimap}} be added? DEFAULT: TRUE
#' @param axis        Should x axis with months be added? DEFAULT: TRUE
#' @param legend      Should Period legend be added in topright? DEFAULT: TRUE
#' @param x1,x2,y1,y2 Location of legend. See \code{\link{smallPlot}}
#' @param text        Should lines be labeled with quantiles? DEFAULT: TRUE
#' @param mar         Margins passed to \link{par}. DEFAULT: c(1.8,3.2,3,0.2)
#' @param cex.axis    Size of axis labels (also y axis) and text. DEFAULT: 1
#' @param boxcol,boxlwd Color and width of box around plot region. DEFAULTS: 1,1
#' @param maincex,mainline,mainadj,mainsep Arguments to customize the title.
#' @param xticks.lwd,x.line X axis tick width and line. DEFAULT. 1,0
#' @param \dots       Further arguments passed to \code{\link{qdoyCompute}}
#'
qdoyVisChange <- function(
name,
seaslist,
qdp=seaslist[[name]],
dist="gev",
metadf=get("meta"),
steps=1:3,
RPs=c(1.111,2,10,100),
map=TRUE,
axis=TRUE,
legend=TRUE,
x1=0.65, x2=0.95, y1=0.85, y2=1,
text=TRUE,
mar=c(1.8,3.2,3,0.2),
cex.axis=1,
boxcol=1,
boxlwd=1,
maincex=1,
mainline=NA,
mainadj=0.35,
mainsep="\n",
xticks.lwd=1,
x.line=0,
...)
{
RPs <- paste0("RP.",RPs)
par(mar=mar)
col <- seqPal(3,gb=T)
plot(1:366, type="n", xaxs="i", axes=FALSE, ylim=lim0(qdp[,RPs,,]), xlab="", ylab="", las=1, ...)
at <- pretty2(par("usr")[3:4], n=4)
at <- unique(c(0,at)) # sometimes zero is missing
axis(2, at=at, mgp=c(3,0.5,0), cex.axis=cex.axis, las=1)
for(p in RPs) for(s in steps) lines(qdp[dist,p,,s], col=col[s], lwd=3)
abline(v=117+1)
box(col=boxcol, lwd=boxlwd)
##if(box) box(col=boxcol[name], lwd=4)
pcex <- par("cex")
if(map) minimap(name, x1=0.16)
par(cex=pcex)
title(main=paste0(sub("_Rheinhalle","",name),mainsep,metadf[name,"river"]),
      adj=mainadj, cex=maincex, line=mainline)
if(legend) colPointsLegend(1920:2010, nbins=3, colors=col, title="", lines=FALSE,
                           at=c(1920,1950,1980,2010), density=FALSE, x1=x1, x2=x2, y1=y1, y2=y2)
if(axis)
   {
   tick <- monthLabs(2004,2004, npm=1) + 117
   labs <- tick + 15
   tdoy <- as.numeric(format(tick,"%j"))
   ldoy <- as.numeric(format(labs,"%j"))
   months=substr(month.abb,1,1)
   axis(1, ldoy, months, tick=FALSE, mgp=c(3,0.5,0), las=1, cex.axis=cex.axis)
   axis(1, tdoy, labels=FALSE, las=1, lwd=0, lwd.ticks=xticks.lwd, line=x.line, tcl=-1)
   }
if(text)
  {
  texti <- seq(from=200, to=40, length.out=length(RPs))
  names(texti) <- RPs
  texty <- apply(qdp[dist,,,], MARGIN=1:2, mean, na.rm=TRUE)[RPs,texti]
  if(length(RPs)!=1) texty <- diag(texty)
  textField(x=texti, y=texty, labels=RPs, quiet=TRUE, cex=cex.axis)
  }
}
