#' @title Launch interactive data analysis
#' @description Launch interactive analysis of flood seasonality changes 
#'              along the Rhine river and tributaries.
#'              The R session is blocked during usage, close the app to re-enable
#'              console usage.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2017
#' @seealso \code{shiny::\link[shiny]{runApp}}, \code{\link{qdoyVisPeriods}}
#' @keywords iplot
#' @importFrom shiny runApp
#' @importFrom sp plot
#' @export
#' @examples
#' # rfsApp()
#' @param \dots Arguments passed to \code{shiny::\link[shiny]{runApp}}
#'
rfsApp <- function(...)
{
appDir <- system.file("shinyapp", package="rfs")
if(appDir=="") stop("Could not find shinyapp directory. Try re-installing `rfs`.", call.=FALSE)
shiny::runApp(appDir, ...)
}
