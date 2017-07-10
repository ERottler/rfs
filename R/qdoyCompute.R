#' @title Quantile estimation per DOY
#' @description Quantiles per Day of Year (doy), basically forked from
#' \url{https://github.com/brry/berryFunctions/blob/master/R/seasonality.R}
#' @return Matrix for each DOY and quantile
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{qdoyPeriods}}
#' @keywords ts chron
#' @importFrom berryFunctions l2array getColumn
#' @importFrom pbapply pblapply
#' @importFrom extremeStat distLextreme
#' @export
#' @examples
#' # to be abstracted from testing
#'
#' @param dates    Dates in ascending order.
#'                 Can be charater strings or \code{\link{strptime}} results,
#'                 as accepted (and coerced) by \code{\link{as.Date}}
#' @param values   Values to be analyzed
#' @param data     Optional: data.frame with the column names as given by dates and values
#' @param shift    Number of days prior to Jan 1 to move the year-break to.
#'                 E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param RPs      Return periods for \code{extremeStat::\link[extremeStat]{distLextreme}}
#'                 DEFAULT: \code{\link{RPs}}
#' @param \dots    Further arguments, currently ignored
#'
qdoyCompute <- function(
dates,
values,
data,
shift=0,
RPs=RPs,
...
)
{

# input columns or vectors
if(!missing(data)) # get vectors from data.frame
  {
    dates <- getColumn(substitute(dates),  data)
    values<- getColumn(substitute(values), data)
  }
#check input
if(length(dates)!=length(values)) stop("length of dates and values not equal (",
                                         length(dates),", ",length(values),").")
dates <- as.Date(dates)
# shift break to other month:
shift <- checkShift(shift)
dates <- dates + shift
# Day of Year and Return Level:
doy  <- as.numeric(format(dates,"%j"))
doy # stiffle rstudio warning about not being used
RL <- pblapply(1:366, function(day) distLextreme(values[doy==day], RPs=RPs, sel="gev",
                                      quiet=TRUE, gpd=FALSE, weight=FALSE)$quant)
# Transform into array:
RL <- l2array(RL)
names(dimnames(RL)) <- c("dist","rl","doy")
dimnames(RL)[[2]] <- c(paste0("RP.", RPs), "RMSE")
dimnames(RL)[[3]] <- 1:366
RL
}



# Testing ----
if(FALSE){


load(seasFolder("data/dismeta.Rdata"))
dd <- selectDates(1980,2020, df=dis)[,c("date","Koeln")]
qdoy   <- qdoyCompute("date", "Koeln", data=dis, shift=117) # 10 secs
qdoy30 <- qdoyCompute("date", "Koeln", data=dd,  shift=117) # 5 secs


plotqdoy <- function(dist, qd=qdoy, RPs=c(1,2,10,50,200), cols=RPcols, ylim=lim0(10900)  )
{
plot(1:366, 1:366, ylim=ylim, las=1, type="n", main=dist, xaxt="n", xlab="",
     ylab="discharge  [m\U{00B3}/s]")
seasAxis(shift=117)
RPs <- paste0("RP.",RPs)
for(i in RPs) lines(1:366, qd[dist,i,], col=cols[i], lwd=2)
textField(200, qd[dist,RPs,200], RPs, col=cols[RPs])
}
plotci <- function(dist, qd=qdoy, RPs=c(20,50,100), ...)
{
RPs <- paste0("RP.", sort(RPs))
ciBand(qd[dist,RPs[3],],qd[dist,RPs[1],],qd[dist,RPs[2],], ...)
}


# df <- names(sort(apply(qdoy[,"RMSE",], 1, mean, na.rm=TRUE), na.last=TRUE)) # quantile, probs=1:10/10

pdf("qdoy.pdf", height=5)
  par(mar=c(3,5,2,0.5), mgp=c(3.5,0.7,0))
  plotqdoy("empirical")
  plotqdoy("gev")
  #
  plotci("empirical", xaxt="n", xlab="", main="Q20, Q50 and Q100 in Cologne",
         ylab="discharge  [m\U{00B3}/s]")
  seasAxis(shift=117)
  plotci("gev", add=TRUE, colm=4)
  legend("topright", c("empirical", "gev"), fill=c(3,4))
  # for(d in df[1:22]) plotqdoy(d)
  # lines(1:366, qdoy["empirical","RP.100",], col=2)
dev.off()

# pdf("qdoy_30.pdf", height=5)
# for(d in df[1:22]) plotqdoy(d, qdoy30)
# #
# plotci("empirical", qdoy30)
# plotci("gev", qdoy30, add=TRUE, colm=4)
# #
# plotqdoy("gev", qdoy30)
# plotqdoy("empirical", qdoy30)
# plotqdoy("weighted2", qdoy30)
# dev.off()

}
