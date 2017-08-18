#' @title Annual maxima from time series
#' @description Get annual maxima and more information from time series
#' @return data.frame with one row per (hydrological) year (h_year) with the columns:\cr
#' year, max, date, doy (day of h_year), n (number of non-NA values), 
#' days (number of days per h_year in input data) and rownr.\cr
#' The rownames of the output are character strings with the h_year.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{qdoyCompute}}
#' @importFrom berryFunctions getColumn
#' @importFrom utils head
#' @export
#' @examples
#' load(seasFolder("data/dismeta.Rdata"))
#' annmax <- annualMax("date","Koeln", dis)
#' head(annmax)
#' plot(annmax[,1:2], type="l", las=1, xlab="year")
#' plot(annmax$year, annmax$doy)
#' 
#' head(annualMax("date","Koeln", dis))
#' head(annualMax("date","Koeln", dis, shift=117)) # hydrological year
#' berryFunctions::seasonality("date","Koeln", dis, shift=117)
#' annmax <- annualMax("date","Koeln", dis, shift=117)
#' berryFunctions::linReg(doy~year, data=annmax); abline(h=150)
#' berryFunctions::linReg(doy~year, data=annmax[120:190,]); abline(h=150)
#' # No real trend in timing of annual streamflow maximum in Cologne
#' 
#' dis2 <- dis[dis$Koeln>9000,]
#' head(annualMax("date","Koeln", dis2))
#' head(annualMax("date","Koeln", dis2, missing2NA=FALSE))
#' #View(dis[,c("date","Koeln")])
#' plot(head(dis[,c("date","Koeln")], 500), type="l")
#' 
#' @param dates    Dates in ascending order.
#'                 Can be charater strings or \code{\link{strptime}} results,
#'                 as accepted (and coerced) by \code{\link{as.Date}}
#' @param values   Values to be mapped in color with \code{\link{colPoints}}
#' @param data     Optional: data.frame with the column names as given by dates and values
#' @param shift    Number of days to move the year-break to.
#'                 E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param missing2NA Logical. If years are completely missing in \code{dates},
#'                 insert NA rows for those? DEFAULT: TRUE
#' 
annualMax <- function(
  dates,
  values,
  data,
  shift=0,
  missing2NA=TRUE
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
year <- as.numeric(format(dates,"%Y"))
whichmax <- function(xx) if(all(is.na(xx))) NA else which.max(xx)
m_which <- tapply(X=values, INDEX=year, FUN=whichmax)
m_lengt <- tapply(X=values, INDEX=year, FUN=length)
m_nonNA <- tapply(X=values, INDEX=year, FUN=function(x)sum(!is.na(x)) )
m_index <- c(0,head(cumsum(m_lengt),-1)) + m_which

# output data.frame
out <- data.frame(year=year, max=values, date=dates-shift)
out <- out[m_index,]
out$year  <- as.numeric(names(m_which))
out$doy   <- as.numeric(m_which)
out$n     <- as.numeric(m_nonNA)
out$days  <- as.numeric(m_lengt)
out$rownr <- as.numeric(m_index)
if(missing2NA) out <- merge(data.frame(year=min(out$year):max(out$year)), out, all=TRUE)
rownames(out) <- out$year
return(out)
}

