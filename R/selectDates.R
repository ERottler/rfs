#' @title title
#' @description description
#' @details detailsMayBeRemoved
#' @aliases aliasMayBeRemoved
#' @return ReturnValue
#' @section Warning: warningMayBeRemoved
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{help}}, \code{\link{help}}
#' @keywords aplot
# @importFrom package fun1 fun2
#' @export
#' @examples
#' load(seasFolder("data/dismeta.Rdata"))
#' str(selectDates(y1=2009)[,c(1,14:18)])
#' 
#' @param y1,m1,d1 Beginning year, month and day. DEFAULTS: 1920,1,1
#' @param y2,m2,d2 Ending year, month and day. DEFAULTS: 2010,1,1
#' @param df    Dataframe with discharge records in columns. DEFAULT: dis
#' @param logic Logical: return T/F vector: which rows of df are between a and b?
# @param \dots Further arguments passed to \code{\link{plot}}
#' 
selectDates <- function(
y1=1920,
y2=2010,
m1=1,d1=1,
m2=1,d2=1,
df=get("dis"),
logic=FALSE)
{
TF <- df[,"date"] >= as.Date(paste(y1,m1,d1, sep="-")) &
      df[,"date"] <  as.Date(paste(y2,m2,d2, sep="-"))
if(logic) return(TF)
df[TF,]
}
