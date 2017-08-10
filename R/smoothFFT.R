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
#' x <- smoothFFTexampleData
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


#' @rdname smoothFFT
#' @export
smoothFFTexampleData <- c(2204.1, 2132.8, 2053.7, 2032.3, 2047, 2108.1, 2304.2,
2371.8, 2412.8, 2788.9, 3449.6, 3570, 3611.6, 3488.1, 3427.5, 3598, 3647.7,
3512.9, 3203.1, 2893.4, 2680.6, 2785.6, 2852, 2787, 2680.3, 2562.3, 2391.5,
2338.4, 2522.1, 3212.6, 3705.5, 3874.3, 3427.5, 3062.4, 2843, 2695, 2794.2,
2994.2, 3022.5, 3002.4, 2964.3, 2892.3, 2859.5, 2965.1, 3156.9, 3217.3, 3192.5,
3172.6, 3224.1, 3301.1, 3613.6, 4026, 4504.7, 4964.9, 5642.5, 5975.8, 6343.3,
6621.7, 6655.4, 6474.6, 6209.4, 6013.5, 5759.1, 5423.1, 5152.3, 5584.3, 5701.9,
5906.1, 6074.3, 6348.6, 6191.4, 5965.7, 6001, 6296.8, 6205.9, 5822.2, 5475.9,
5546.8, 5564.3, 5442.4, 5512.3, 5290.2, 4886.7, 4579.7, 4470.6, 4700, 5152.4,
5557.5, 5827.4, 6096.3, 6057.5, 5885.6, 5350.2, 5234.3, 5338, 5155.5, 5100.6,
5057.5, 5170, 5617.9, 5947.9, 5796.2, 5213.6, 4685.2, 4758.5, 5372.2, 6416.9,
8115, 9341.2, 9651.7, 9437.5, 8723.9, 8188.8, 7766.1, 7007, 6462.6, 6887.2,
7775, 8298.3, 8915.6, 9937.6, 10210.4, 10008.8, 10069.1, 9656.2, 8722.1, 7617.8,
6422.6, 5496.1, 5859.2, 6540.9, 6438.5, 6332.3, 6376.7, 5833.4, 5012.6, 5108.4,
5053.3, 4972.4, 4925.2, 6192.6, 6387, 6536.5, 7665.7, 9012.1, 10036.1, 10413.4,
10002.2, 9142.1, 8139.2, 7001.3, 6224.9, 5998.5, 6015.5, 6169, 5787.6, 5242.5,
4621.3, 4420.8, 4599.4, 4580.1, 5708.1, 6765.6, 7092.6, 7639.7, 7766.8, 7528.4,
6814.4, 6491.1, 7588.8, 8424.9, 8398.5, 8104, 8113.6, 8791, 9647.4, 9763.3,
9299.7, 8611.8, 8141.5, 7920.3, 7362.5, 6523.1, 5778.8, 5214.8, 4791.8, 5640.9,
6754.6, 7040.4, 6557.3, 6483.5, 6350.9, 6447.9, 6259.5, 5973.8, 5927.9, 6372.4,
7639.8, 8879.2, 9401.9, 9195.1, 8614.5, 7872.4, 7111.3, 6558.5, 6538.8, 6410.1,
6238.9, 6056.3, 5993.3, 5704.1, 5398.1, 5002.8, 4682.5, 4421.4, 4166.8, 3965,
4138.9, 4473.2, 4890.3, 5237.8, 5884.2, 6539.1, 6431.7, 6016.7, 5641.7, 5215,
4992.6, 4821.3, 4622, 4414.6, 4259.1, 4539.8, 4849.3, 4821.6, 4713.2, 4595,
4528.9, 4306.9, 3929.8, 3678.7, 3353.7, 3847.7, 4215.7, 4260.1, 3966.3, 3806.6,
3810.9, 3464.8, 3333.9, 3359, 3461.5, 3846, 4204.7, 4401.2, 4503.2, 4431.9,
4320.1, 4440.1, 4506, 4527.2, 4450.5, 4491.4, 4498.1, 4585.5, 4574.8, 4482.9,
4329.7, 4426.2, 4462.2, 4579.9, 4919.2, 5002, 4772.1, 4499.2, 4383.6, 4244.1,
3994.1, 3857.5, 3806.8, 3894.2, 4038.2, 4060.2, 3998.9, 4014.4, 3972.2, 3849.6,
3632.2, 3643.1, 3540.9, 3383, 3383.5, 3461.1, 3277.2, 3091.3, 3090.3, 3049.3,
2979, 2910.3, 2898.9, 2883.2, 2818.6, 2933.4, 3115, 3384.2, 3540.6, 3456.9,
3371.5, 3249.7, 3378.3, 3529.2, 3515.2, 3227.9, 3067, 3057.8, 3011.1, 3106.8,
3183.1, 3119.6, 3026.3, 2846.5, 2758.9, 2774.1, 2680.2, 2622.1, 2617.3, 2754.9,
2765.9, 2784.3, 2817.5, 2796.5, 2710.9, 2648.5, 2596.9, 2453.9, 2322.3, 2230.3,
2201.3, 2631.1, 3324.8, 3822.9, 3972.5, 3773.5, 3471.2, 3252.2, 3090.4, 2975.8,
2844.8, 2737.7, 2651, 2779.6, 3010.6, 3114.9, 3094.4, 3121.1, 2944.3, 2644,
2572.3, 2577.4, 2698.1, 2740.7, 2795.4, 2797.2, 2584.5, 2417.2, 1959.5)

