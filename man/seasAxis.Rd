% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasAxis.R
\name{seasAxis}
\alias{seasAxis}
\title{Seasonality axis}
\usage{
seasAxis(shift = 0, janline = TRUE, mlabs = substr(month.abb, 1, 1),
  mline = 0.7, xlab = "Month", xline = 2, side = 1, targs = NULL,
  largs = NULL, xargs = NULL, ...)
}
\arguments{
\item{shift}{Number of days to move the year-break to.
E.g. shift=61 for German hydrological year (Nov to Oct). DEFAULT: 0}

\item{janline}{Logical: Should horizontal line be plotted at
January 1st if \code{shift!=0}? DEFAULT: TRUE}

\item{mlabs}{Labels for the months. DEFAULT: J,F,M,A,M,J,J,A,S,O,N,D}

\item{mline}{Placement of labels (Distance from axis). DEFAULT: 0.7}

\item{xlab}{Label for the axis. DEFAULT: "Month"}

\item{xline}{Placement of axis label. DEFAULT: 2}

\item{side}{Margin side for \code{\link{axis}}. DEFAULT: 1}

\item{targs}{Optional list of arguments passed to \code{\link{axis}}
for month separating ticks. DEFAULT: NULL}

\item{largs}{Optional list of arguments passed to \code{\link{axis}}
for mid-month labels. DEFAULT: NULL}

\item{xargs}{Optional list of arguments passed to \code{\link{title}}
for general axis label. DEFAULT: NULL}

\item{\dots}{Further arguments passed to \code{\link{abline}} for janline
(excluding \code{v}).}
}
\value{
nothing
}
\description{
Add monthly axis labels on seasonality plots
}
\examples{
plot(1:366, xaxt="n", xaxs="i")
seasAxis(shift=117)
seasAxis(shift=117, targs=list(col.ticks="red"), lwd=3, col="purple")

plot(1:366, yaxt="n", ylim=c(366,0))
seasAxis(shift=117, side=2)

}
\seealso{
\code{\link{qdoyVis}}
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Jun 2017
}
\keyword{aplot}
