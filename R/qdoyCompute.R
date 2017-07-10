#' @title Quantile estimation per DOY
#' @description Quantiles per Day of Year (doy), basically forked from
#' \url{https://github.com/brry/berryFunctions/blob/master/R/seasonality.R}
#' @return Matrix for each DOY and quantile
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{qdoyVis}}, \code{\link{qdoyPeriods}}
#' @keywords ts chron
#' @importFrom berryFunctions l2array getColumn
#' @importFrom pbapply pblapply
#' @importFrom extremeStat distLextreme
#' @export
#' @examples
#' # see qdoyVis
#'
#' @param dates    Dates in ascending order.
#'                 Can be charater strings or \code{\link{strptime}} results,
#'                 as accepted (and coerced) by \code{\link{as.Date}}
#' @param values   Values to be analyzed
#' @param data     Optional: data.frame with the column names as given by dates and values
#' @param shift    Number of days prior to Jan 1 to move the year-break to.
#'                 E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0
#' @param RPs      Return periods for \code{extremeStat::\link[extremeStat]{distLextreme}}
#'                 DEFAULT: \code{\link{RPvals}}
#' @param \dots    Further arguments, currently ignored
#'
qdoyCompute <- function(
dates,
values,
data,
shift=0,
RPs=RPvals,
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
