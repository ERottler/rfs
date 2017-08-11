#' @title smooth time series with FFT
#' @description smooth time series with FFT
#' @return Smoothed vector
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @seealso \code{\link{qdoyVis}}
#' @keywords smooth ts nonparametric
#' @importFrom fftw planFFT FFT IFFT
#' @importFrom stats dnorm
#' @importFrom berryFunctions approx2
#' @export
#' @examples
#' x <- cumsum(rnorm(200))
#' plot(x, type="l")
#' lines(smoothFFT(x, sd=1), col=4)
#' lines(smoothFFT(x, sd=4), col=2)
#' 
#' x[105:111] <- NA
#' plot(x, type="l")
#' lines(smoothFFT(x, sd=3), col=4, lwd=2)
#' lines(smoothFFT(x, sd=3, keepNA=FALSE), col=2, lty=3)
#' 
#' x <- seas$Koeln["gev","RP.50",,3]
#' plot(x, type="l")
#' lines(smoothFFT(x, sd=4), col=4)
#' lines(smooth.spline(x,spar=0.45), col=2)
#' 
#' x <- x[1:100]
#' plot(x, type="l")
#' lines(smoothFFT(x, sd=3), col=4)    # works better for seasonal data, I guess
#' lines(smooth.spline(x,spar=0.45), col=2)
#' 
#' smoothFFT(c(42,NA,NA,NA,NA)[-1], 2)
#' 
#' stopifnot(all(x == smoothFFT(x, -1)))
#' 
#' @param x      Numerical vector
#' @param sd     Standard deviation. Determines degree of smoothing.
#'               Use negative value to return original x.
#' @param keepNA Logical: keep NA values? For computation, NAs will be
#'               interpolated with
#'               \code{berryFunctions::\link[berryFunctions]{approx2}}.
#'               If keepNA=TRUE, the result will again have NAs.
#'               If keepNA=FALSE, they will remain interpolated. DEFAULT: TRUE
#' @param quiet  Logical. Suppress warnings about all-NA vectors? DEFAULT: FALSE
#' @param \dots  Further arguments passed to \code{fftw::\link[fftw]{IFFT}}
#' 
smoothFFT <- function(
x,
sd,
keepNA=TRUE,
quiet=FALSE,
...
)
{
# input checks:
isna <- is.na(x)
if(all(isna))
  {
  if(!quiet) warning("There are no non-NA values, returning all NAs.")
  return(x)
  }
if(!is.numeric(x)) stop("x must be numeric, not: ", class(x))
if(length(sd)!=1) stop("sd must have length 1, not: ", length(sd))
if(sd<0) return(x)
# actual computation
n <- length(x)
x <- berryFunctions::approx2(x)
pf <- fftw::planFFT(n)
gauss1 <- dnorm(1:n-n/2+0.5, mean=0, sd=sd)
gauss2 <- abs(fftw::FFT(gauss1, pf))
inverse <- fftw::IFFT(fftw::FFT(x, pf) * gauss2, ...)
out <- Re(inverse)
if(keepNA) out[isna] <- NA
out
}
