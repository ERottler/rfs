#' @title Quantile estimation per DOY
#' @description Quantiles per Day of Year (doy), basically forked from
#' \url{https://github.com/brry/berryFunctions/blob/master/R/seasonality.R}
#' @return Matrix for each DOY and quantile
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{help}}, \code{\link{help}}
#' @keywords ts chron
#' @importFrom berryFunctions quantileMean l2df getColumn
#' @importFrom pbapply pblapply
#' @importFrom stats dnorm
#' @export
#' @examples
#' load(seasFolder("data/dismeta.Rdata"))
#' qdoy <- qdoyCompute("date", "Koeln", data=dis)
#' str(qdoy)
#' plot(1:366, qdoy[,6], ylim=c(400,11000), type="l")
#' lines(1:366, qdoy[,5], col=2)
#'
#' @param dates    Dates in ascending order.
#'                 Can be charater strings or \code{\link{strptime}} results,
#'                 as accepted (and coerced) by \code{\link{as.Date}}
#' @param values   Values to be mapped in color with \code{\link{colPoints}}
#' @param data     Optional: data.frame with the column names as given by dates and values
#' @param shift    Number of days to move the year-break to.
#'                 E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param probs    Probabilities passed to \code{\link{quantileMean}} for plot=4.
#'                 DEFAULT: c(0,25,50,75,95,99)/100
#' @param width    Numeric: window width for plot=4. Used as sd in gaussian weighting.
#'                 Support (number of values around a DOY passed to
#'                 quantile funtion at least once) is ca 4.9*width.
#'                 The value at doy itself is used 10 times.
#'                 Larger values of width require more computing time.
#'                 DEFAULT: 3
#' @param quiet    Logical: suppress progress bar? DEFAULT: FALSE
#' @param \dots Further arguments passed to \code{\link{plot}}
#'
qdoyCompute <- function(
dates,
values,
data,
shift=0,
probs=c(0,25,50,75,95,99.9)/100,
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

if(width<1)
{
Qp <- lapply(1:366, function(day) quantileMean(values[which(doy==day)],
                                               probs=probs, na.rm=TRUE)  )
}
else
{
xx4 <- unique(ceiling(  (-3*width):(3*width)  ))
w <- dnorm(xx4, sd=width)
w <- round(w*10/max(w))
# computing weighted quantile around DOYs
if(!quiet) lapply <- pbapply::pblapply
Qp <- lapply(1:366, function(day)
  {
  select <- base::lapply(which(doy==day), function(i) rep(i+xx4, w) )
  select <- unlist(select)
  select <- select[select>0 & select<length(doy)]
  # output: weighted quantile
  quantileMean(values[select], probs=probs, na.rm=TRUE)
  })
}
Qp <- as.matrix(l2df(Qp))
names(dimnames(Qp)) <- c("DOY","Quantile")
Qp
}
