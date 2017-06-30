#' Annual maxima from time series
#'
#' Get annual maxima from time series
#'
#' @return named numeric vector
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{qdoyCompute}}
#' @export
#' @examples
#' load(seasFolder("data/dismeta.Rdata"))
#' annual_max <- annualMax("date","Koeln", dis)
#' head(data.frame(annual_max))
#' plot(as.numeric(names(annual_max)), annual_max, type="l", las=1, xlab="year")
#'
#' head(annualMax("date","Koeln", dis))
#' head(annualMax("date","Koeln", dis, shift=117)) # hydrological year
#'
#' @param dates    Dates in ascending order.
#'                 Can be charater strings or \code{\link{strptime}} results,
#'                 as accepted (and coerced) by \code{\link{as.Date}}
#' @param values   Values to be mapped in color with \code{\link{colPoints}}
#' @param data     Optional: data.frame with the column names as given by dates and values
#' @param shift    Number of days to move the year-break to.
#'                 E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param \dots    Further arguments passed to \code{\link{colPoints}}
#'
annualMax <- function(
  dates,
  values,
  data,
  shift=0,
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
# convert to date
dates <- as.Date(dates)

# shift break to other month
shift <- checkShift(shift)
dates <- dates + shift

# Annual maxima
# axis values
year <- as.numeric(format(dates,"%Y"))
mymax <- function(xx) if(all(is.na(xx))) NA else max(xx, na.rm=TRUE)
tapply(X=values, INDEX=year, FUN=mymax)
}

