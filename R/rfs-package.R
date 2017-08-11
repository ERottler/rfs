#' @title Rhine flood seasonality
#' 
#' @description Rhine flood seasonality analysis.
#' PhD project by Berry Boessenkool.
#' ToDo: add more info here
#' 
#' @name rfs
#' @aliases rfs-package rfs
#' @docType package
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2017
#' @keywords package documentation
NULL


#' @title seas - streamflow aggregates
#' @description seas: A dataset with streamflow aggregates for seasonality analysis
#' @format A list with 55 elements for 55 gauges (along large rivers, see \code{\link{gnames}}).
#' Each element has an array with 2 distributions, 11 return periods, 366 days and 3 periods.\cr
#' Included as a dataset in the package with code in R/rfs-package.R
#' @docType data
#' @examples
#' str(seas[[1]])
"seas"

#' @title meta data about gauges
#' @description meta: A dataset with meta data about the streamflow gauges
#' @format A data.frame with 196 rows for 28 variables
#' @docType data
#' @examples
#' colnames(meta)
#' head(meta[,c(3,4,6,22:28)])
"meta"


# Data inclusion in package ----
if(FALSE){

# load data from local computer (original data is not allowed to be public):
load(seasFolder("data/dismeta.Rdata"))

# set up parallelization:
library(pbapply); library(parallel) # for parallel lapply execution
cl <- makeCluster( detectCores()-1 )
clusterExport(cl, c("dis", "meta"))

# compute seasonality changes:
seas <- pbsapply(gnames(large=TRUE), cl=cl, FUN=qdoyPeriods, progbar=FALSE, 
                 negative2NA=TRUE, simplify=FALSE) # 2 mins
stopCluster(cl); rm(cl); gc()

# checks:
x <- seas[[1]]
x[,1:2,1,1]
summary(x[1,,,])
summary(x)
rm(x)

# remove unneeded information (reduce size from 31.3 to 11.2 MB):
seas <- sapply(seas, function(x) x[1:2,,,], simplify=FALSE)
summary(seas[[1]])
str(seas[[1]])

# save into data directory
setwd(devtools::as.package(".")$path) # go to main package level
dir.create("data")
save(seas,       file="data/seas.rda")
tools::resaveRdaFiles("data/seas.rda") # from 5.7 to 4.8 MB

# meta data:
save(meta,       file="data/meta.rda")
tools::resaveRdaFiles("data/meta.rda") 

} # end if(FALSE)
