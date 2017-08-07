#' @title Return Periods
#' @description Common Return period values
#' @return Numerical (RPvals) or character string (RPcols) vector
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2017
#' @seealso \code{\link{qdoyCompute}}
#' @keywords data
#' @importFrom berryFunctions divPal
#' @export
#' @examples
#' RPvals
#' plot(RPvals, seq_along(RPvals), log="x", col=RPcols, pch=16, cex=3)
#' round((1-1/RPvals)*100,2)
#' 
RPvals <- c(1, 1.111, 1.2, 2, 5, 10, 20, 50, 100, 200)

#' @export
#' @rdname RPvals
RPcols <- divPal(length(RPvals), gp=T)
names(RPcols) <- paste0("RP.",RPvals)

