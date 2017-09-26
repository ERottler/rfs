#' @title Get path to folder with seasonality analysis + data
#' @description Get path to (files in) the folder with seasonality analysis + data
#' @return Path(s)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2017
# @seealso \code{\link{help}}, \code{\link{help}}
#' @keywords file
#' @importFrom berryFunctions checkFile
#' @export
#' @examples
#' seasFolder()
#' seasFolder("dummy.txt", warnonly=TRUE)
#' stopifnot(length(seasFolder("dummy.txt", "dude.xyz", warnonly=TRUE))==2)
#' 
#' @param \dots Optional filename(s) that will be appended.
#' @param warnonly Logical: only warn instead of stopping if files are not found. DEFAULT: FALSE
#' 
seasFolder <- function(
...,
warnonly=FALSE
)
{
path <- "C:/Users/boessenkool/Dropbox/Promotion/6_Seasonality"
# home PC path change:
if(!file.exists(path)) path <- gsub("C:/Users/boessenkool", "S:", path)
# laptop linux path change:
if(!file.exists(path)) path <- gsub("S:", "/home/berry", path)
if(!file.exists(path)) stop("Cannot find project path. Last try:", path)
files <- list(...)
if(length(files)>0) path <- file.path(path, files, fsep="/")
checkFile(path, warnonly=warnonly)
path
}
