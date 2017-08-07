#' @title DOY quantiles for several periods
#' @description Quantiles per DOY for several time periods.
#' Enables analysis of seasonality changes over time.
#' @return 3d array for each doy, quantile and period
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{qdoyCompute}}, \code{\link{qdoyVis}}
#' @keywords ts chron
#' @importFrom berryFunctions l2array
#' @export
#' @examples
#' \dontrun{
#' load(seasFolder("data/dismeta.Rdata"))
#' qdoy <- qdoyPeriods("Koeln") # 3x3 seconds
#' str(qdoy)
#' qdoy <- list(Koeln=qdoy, Mainz=qdoyPeriods("Mainz", progbar=FALSE))
#' qdoyVisPeriods("Koeln", qdoy)
#' qdoyVisPeriods("Mainz", qdoy)
#' }
#' 
#' @param name       Character: Name of gauge to be analyzed, see \code{\link{gnames}}
#' @param disdf      Dataframe with columns "date" and \code{name}. DEFAULT: dis
#' @param sans1999   Logical: exclude the values from 1999? (Pentecost flood)
#'                   DEFAULT: FALSE
#' @param shift,RPs  Arguments passed to \code{\link{qdoyCompute}}
#' @param time       Period beginnings (and ending of last one).
#'                   DEFAULT: c(1920,1950,1980,2010)
#' @param \dots      Further arguments passed to \code{\link{qdoyCompute}}
#' 
qdoyPeriods <- function(
name,
disdf=get("dis"),
sans1999=FALSE,
shift=117,
RPs=RPvals,
time=c(1920,1950,1980,2010),
...)
{

seldat <- disdf[!is.na(disdf[,name]), c("date", name)]
seldat <- selectDates(1920,2011, df=seldat)
if(sans1999) seldat[format(seldat$date,"%Y")=="1999",2] <- NA

out <- lapply(seq_along(time[-1]), function(step){
  sdi <- selectDates(time[step], time[step+1], df=seldat)
  if(nrow(sdi)<1) sdi <- data.frame(date=Sys.Date()+1:10, emptystat=-1)
  qdoyCompute(sdi$date, sdi[,2], quiet=TRUE, shift=shift, RPs=RPs, ...)
  })
out <- l2array(out)
names(dimnames(out))[4] <- "Period"
dimnames(out)[[4]] <- paste(time[-length(time)], time[-1], sep="-")
out
}
