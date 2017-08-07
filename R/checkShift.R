#' @title check shift values
#' @description internal function to check and correct shift values
#' @return corrected shift value
#' @importFrom berryFunctions almost.equal
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
#' @seealso \code{\link{seasAxis}}, \code{\link{annualMax}}, \code{\link{qdoyCompute}}
#' @examples
#' \dontrun{ # Excluded because it is internal
#' checkShift(-6)
#' checkShift(1:5)
#' checkShift(0)
#' checkShift(pi)
#' }
#' 
#' 
#' @param shift Shifting value to be checked
#' 
checkShift <- function(
shift
)
{
env <- deparse(sys.call(1))
if(length(shift)>1) warning("Length of 'shift' is ", length(shift),
                            ". Only first value will be used. ",
                            "Happened in: ", env, call.=FALSE)
shift <- shift[1]
if(length(shift)<1) stop("'shift' has zero length. Happened in: ", env, call.=FALSE)

if(shift<0) warning("'shift' was negative (",shift,"). Absolute value now used. ",
                    "Happened in: ", env, call.=FALSE)
shift <- abs(shift)

if(shift>366) stop("'shift' is", shift, ", but should be between 0 and 366. ",
                   "Happened in: ", env, call.=FALSE)

if(!almost.equal(shift, round(shift))) warning("'shift' should be an integer, is now rounded. ",
                                            "Happened in: ", env, call.=FALSE)
shift <- round(shift)
shift
}
