#' @title Return Periods
#' @description Common Return period values
#' @return Numerical (RPs) or character string (RPcols) vector
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2017
#' @seealso \code{\link{qdoyCompute}}
#' @keywords data
#' @importFrom berryFunctions divPal
#' @export
#' @examples
#' RPs
#' plot(RPs, seq_along(RPs), log="x", col=RPcols, pch=16, cex=3)
#' round((1-1/RPs)*100,2)
#'
RPs <- c(1, 1.111, 1.2, 2, 5, 10, 20, 50, 100, 200)

#' @export
#' @rdname RPs
RPcols <- divPal(length(RPs), gp=T)
names(RPcols) <- paste0("RP.",RPs)

