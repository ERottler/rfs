#' @title Rhine flood seasonality
#' @description Rhine flood seasonality analysis.
#' PhD project by Berry Boessenkool.
#' The main functions are introduced in the examples below.
#' @name rfs
#' @aliases rfs-package rfs
#' @docType package
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2017
#' @keywords package documentation
#' @examples 
#' 
#' # qdoyCompute aggregates streamflow values per day of the year (doy)
#' # The first examples require the original data to be in seasFolder.
#' # Unfortunately, we are not allowed to make it publicly available.
#' # Please contact berry-b@gmx.de if you need the raw data. 
#' 
#' load(seasFolder("data/dismeta.Rdata"))
#' dd <- selectDates(1990,2010, df=dis)[,c("date","Koeln")]
#' qdoy <- qdoyCompute("date", "Koeln", data=dd, shift=117) # 2 secs
#' str(qdoy)
#' 
#' # Visualize the result:
#' qdoyVis(qdoy, shift=117)
#' qdoyVis(qdoy, main="Cologne 1990-2010", RPs=50, cols=4, ylim=c(2e3,10e3), shift=117, lab=0)
#' qdoyVis(qdoy, dist="empirical", RPs=50, cols=3, add=TRUE, lab=0)
#' legend("topright", c("empirical", "gev"), col=3:4, lwd=3)
#' 
#' # qdoyPeriods computes the aggregates for separate periods. 
#' # This procedure was applied to 55 stations at large streams and rivers.
#' # (see source code rfs-package.R)
#' # The result is stored in this package, see   ?seas
#' 
#' # Elements from that list can be visualized as follows:
#' qdoyVisPeriods("Rekingen")
#' 
#' # You can visualize your own data split up in periods with
#' qdoy <- qdoyPeriods("Koeln") # 3x3 seconds
#' str(qdoy)
#' qdoyVisPeriods("Koeln", list(Koeln=qdoy))
#' 
#' # By default, the package dataset "seas" will be used:
#' qdoyVisPeriods("Mainz")
#' qdoyVisPeriods("Mainz", sd=3) # for smoothing
#' 
#' qdoyVisPeriods("Oberriet_Blatten") # can handle NAs
#' 
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

#' @title Europe borders map
#' @description map: A dataset with borders of European countries used in the 
#'              \code{\link{rfsApp}}
#' @format SpatialPolygonsDataFrame
#' @docType data
#' @examples
#' str(map@data)
"map"



if(FALSE){

# load data from local computer (original data is not allowed to be public):
load(seasFolder("data/dismeta.Rdata"))

# save seasTrend ST data ----
ST <- pblapply(gnames("trend"), function(n) # 14 mins
 {
 RPs <- seq(1, 5, len=50)
 ST2 <- sapply(RPs, seasTrend, n=n, disdf=dis, plot=FALSE) # ca 30 secs per stat
 as.data.frame(t(ST2))
 })
names(ST) <- gnames("trend")
save(ST, file=paste0(seasFolder(), "/data/ST.Rdata"))
# This is saved only locally
 
 
# Data inclusion in package ----

# set up parallelization:
library(pbapply); library(parallel) # for parallel lapply execution
cl <- makeCluster( detectCores()-1 )
clusterExport(cl, c("dis", "meta"))

# compute seasonality changes:
seas <- pbsapply(gnames("large"), cl=cl, FUN=qdoyPeriods, progbar=FALSE, 
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


# install.packages(c("rworldmap", "rworldxtra"))
map <- rworldmap::getMap("high")
map <- map[map$ISO3 %in% c("CZE","POL","DEU","NLD","BEL","LUX","FRA","CHE",
                           "LIE","ITA","AUT","SVN","SVK","HUN","HRV","BIH"),]
save(map,        file="data/map.rda")
tools::resaveRdaFiles("data/map.rda") 


devtools::install_github("brry/OSMscale")
devtools::install_github("brry/berryFunctions")
devtools::install_github("brry/extremeStat")
devtools::install_github("brry/rfs")

rsconnect::deployApp('inst/shinyapps/rhine', appName="rhine")
#
#Error: Unable to retrieve package records for the following packages:
#- "OSMscale", "berryFunctions", "extremeStat", "rfs"
# https://github.com/rstudio/rsconnect/issues/189 # use install_github first


# Preparing to deploy application...DONE
# Uploading bundle for application: 205333...
# Detecting system locale ... de_DE
# DONE
# Deploying bundle: 943172 for application: 205333 ...
# Waiting for task: 476281349
#   building: Parsing manifest
#   building: Building image: 936907
#   building: Installing system dependencies
#   building: Fetching packages
#   building: Building package: Renext
#   building: Building package: evir
#   building: Building package: berryFunctions
#   building: Building package: fExtremes
#   building: Building package: ismev
#   building: Building package: extremeStat
#   building: Building package: rfs
#   building: Building package: OpenStreetMap
#   building: Building package: OSMscale
#   building: Installing files
#   building: Pushing image: 936907
#   deploying: Starting instances
#   rollforward: Activating new instances
#   success: Stopping old instances
# Application successfully deployed to https://brry.shinyapps.io/rhine/

} # end if(FALSE)
