#' @title Quantile estimation per DOY
#' @description Quantiles per Day of Year (doy), basically forked from
#' \url{https://github.com/brry/berryFunctions/blob/master/R/seasonality.R}
#' @return Matrix for each DOY and quantile
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{qdoyPeriods}}
#' @keywords ts chron
#' @importFrom berryFunctions quantileMean l2df getColumn
#' @importFrom pbapply pblapply
#' @importFrom stats dnorm
#' @importFrom extremeStat quantGPD
#' @export
#' @examples
#' load(seasFolder("data/dismeta.Rdata"))
#' dd <- selectDates(1980,2010, df=dis)[,c("date","Koeln")]
#' RPs <- c(1,2,4,30,50,100)
#' qdoyemp3 <- qdoyCompute("date", "Koeln", data=dd, shift=117)
#' qdoypar3 <- qdoyCompute("date", "Koeln", data=dd, shift=117, RPs=RPs, truncate=0.8)
#' qdoyemp0 <- qdoyCompute("date", "Koeln", data=dd, shift=117, width=0)
#' qdoypar0 <- qdoyCompute("date", "Koeln", data=dd, shift=117, RPs=RPs, width=0, truncate=0)
#' head(qdoyemp3)
#' head(qdoypar3)
#'
#' empcols <- divPal(9, gp=T); empcols[5] <- 2
#' parcols <- divPal(7, gp=T)
#'
#' pdf("qdoy.pdf", height=5)
#' plot(1:366, 1:366, ylim=c(400,11500), las=1, type="n", lwd=2, main="width=0, par")
#' for(i in 1:6) lines(1:366, qdoypar0[,i], col=parcols[i], lwd=2)
#' textField(200, qdoypar0[200,], colnames(qdoypar0), col=parcols)
#' #
#' plot(1:366, 1:366, ylim=c(400,11500), las=1, type="n", lwd=2, main="width=0, emp")
#' for(i in 1:9) lines(1:366, qdoyemp0[,i], col=empcols[i])
#' textField(200, qdoyemp0[200,], colnames(qdoyemp0), col=empcols)
#' #
#' plot(1:366, 1:366, ylim=c(400,11500), las=1, type="n", lwd=2, main="width=3, emp")
#' for(i in 1:9) lines(1:366, qdoyemp3[,i], col=empcols[i])
#' textField(200, qdoyemp3[200,], colnames(qdoyemp3), col=empcols)
#' #
#' plot(1:366, 1:366, ylim=c(400,11500), las=1, type="n", lwd=2, main="width=3, par")
#' for(i in 1:6) lines(1:366, qdoypar3[,i], col=parcols[i], lwd=2)
#' textField(200, qdoypar3[200,], colnames(qdoypar3), col=parcols)
#' #
#' plot(1:366, 1:366, ylim=c(400,11500), las=1, type="n", lwd=2, main="width=0, par")
#' for(i in 1:6) lines(1:366, qdoypar0[,i], col=parcols[i], lwd=2)
#' textField(200, qdoypar0[200,], colnames(qdoypar0), col=parcols)
#' #
#' ciBand(qdoypar0[,"RP.100"],qdoypar0[,"RP.30"],qdoypar0[,"RP.50"])
#' lines(1:366, qdoyemp0[,"100%"], col=2)
#' #
#' ciBand(qdoypar0[,"RP.100"],qdoypar0[,"RP.30"],qdoypar0[,"RP.50"])
#' ciBand(qdoypar3[,"RP.100"],qdoypar3[,"RP.30"],qdoypar3[,"RP.50"], colm=4, add=T)
#' dev.off()
#'

#' lines(1:366, qdoypar0[,"RP.100"], lwd=2)
#'

#'
#'
#' sum(format(dd$date,"%m-%d")=="04-14")
#' qdoy2 <- qdoyCompute("date", "Koeln", data=dd)
#' head(qdoy2)
#' qdoypar2 <- qdoyCompute("date", "Koeln", data=dd, RPs=c(1,4,10,20,30))
#' head(qdoypar2)
#'
#' @param dates    Dates in ascending order.
#'                 Can be charater strings or \code{\link{strptime}} results,
#'                 as accepted (and coerced) by \code{\link{as.Date}}
#' @param values   Values to be analyzed
#' @param data     Optional: data.frame with the column names as given by dates and values
#' @param shift    Number of days prior to Jan 1 to move the year-break to.
#'                 E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param probs    Probabilities passed to \code{\link{quantileMean}} for plot=4.
#'                 DEFAULT: c(0,25,50,75,95,99)/100
#' @param RPs      Return periods. If given, \code{extremeStat::\link[extremeStat]{distLextreme}}
#'                 is used to deternime return levels. DEFAULT: NA
#' @param truncate If \code{!is.na(RPs)}, truncate is passed to distLextreme.
#'                 If \code{width=0}, set this to zero as well. DEFAULT: 0.9
#' @param width    Numeric: window width , used as sd in gaussian weighting.
#'                 Support (number of values around a DOY passed to
#'                 quantile funtion at least once) is ca 4.9*width.
#'                 The value at doy itself is used 10 times.
#'                 Larger values of width require more computing time.
#'                 DEFAULT: 3
#' @param quiet    Logical: suppress progress bar? DEFAULT: FALSE
#' @param \dots    Further arguments, currently ignored
#'
qdoyCompute <- function(
dates,
values,
data,
shift=0,
probs=c(0,25,50,75,90,95,99,99.9,100)/100,
RPs=NA,
truncate=0.9,
width=3,
quiet=FALSE,
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
# shift break to other month
if(shift<0) warning("'shift' was negative (",shift,"). Absolute value now used.")
shift <- abs(shift)
if(shift>366) stop("'shift' is", shift, ", but should be between 0 and 366.")
dates <- dates + shift
doy  <- as.numeric(format(dates,"%j")) # Day of Year

# empirical quantiles instead of GPD through return periods?
empirical <- all(is.na(RPs))

quantdoy1 <- function(day, emp=empirical)
{
 if(emp) return(quantileMean(values[which(doy==day)], probs=probs, na.rm=TRUE))
 quantGPD(values[which(doy==day)], probs=1 - 1/RPs, truncate=truncate)
 #plotLextreme(dle, log=TRUE)
 #plotLfit(dle)
 #dle$returnlev[c("gpa","weighted2"),]
 #hist(values[select])
 #out <- as.numeric(dle$returnlev[1,])
 #names(out) <- colnames(dle$returnlev)
 #out
}

quantdoy2 <- function(selection, emp=empirical, doyindex)
{
 if(emp) return(quantileMean(values[selection], probs=probs, na.rm=TRUE))
 #selection <- unique(selection)
 quantGPD(values[selection], probs=1-1/(RPs*npy), truncate=truncate)
}


if(width<1)
  {
  if(!empirical) if(truncate>0) message("Note in qdoyCompute: width=0, but truncate=0.9. ",
                         "Recommended to set to zero (Block Maxima approach).")
  Qp <- lapply(1:366, quantdoy1)
  }
else
{
xx4 <- unique(ceiling(  (-3*width):(3*width)  ))
w <- dnorm(xx4, sd=width)
w <- round(w*10/max(w))
npy <- 1#sum(w!=0)
# computing weighted quantile around DOYs
if(!quiet) lapply <- pbapply::pblapply
Qp <- lapply(1:366, function(day)
  {
  doyindex <- which(doy==day)
  select <- base::lapply(doyindex, function(i) rep(i+xx4, w) )
  select <- unlist(select)
  select <- select[select>0 & select<length(doy)]
  # output: weighted quantile
  quantdoy2(select, doyindex=doyindex)
  })
}
Qp <- as.matrix(l2df(Qp))
names(dimnames(Qp)) <- c("DOY","Quantile")
if(!empirical) colnames(Qp) <- c(paste0("RP.",RPs), "n")
Qp
}
