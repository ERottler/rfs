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
#' @param warnonly Logical: only warn instead of stopping if files are not found. 
#'                 NA to not check file existence. DEFAULT: FALSE
#' @param path     Character: path to search in. DEFAULT: NA
#' 
seasFolder <- function(
...,
warnonly=FALSE,
path=NA
)
{
if(is.na(path)) path <- "S:/Dropbox/6_Seasonality"
# laptop linux path change:
if(!file.exists(path)) path <- gsub("S:", "/home/berry", path)
if(!file.exists(path)) stop("Cannot find project path. Last try:", path)
files <- list(...)
if(length(files)>0) path <- file.path(path, files, fsep="/")
if(!is.na(warnonly)) checkFile(path, warnonly=warnonly)
path
}
