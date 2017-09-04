# rfs
Rhine Flood Seasonality  
PhD project by Berry Boessenkool, <berry-b@gmx.de>.

Flood regimes along the Rhine river appear to be changing. 
In the last decades, some snow-melt dominated floods have occured relatively early in the year, 
for example in the Pentecost flood in 1999. 
In tributary rivers where floods mostly stem from long rainfall events, 
streamflow levels seem to be increasing.
If these trends continue into the future, there may be a temporal overlap in 
snow and rain floods, which may bring larger floods than ever in the middle Rhine, 
where both regimes mix.

This repository stores the code used for analyzing streamflow data with regard to the hypothesis outlined above.  
The current status

### install

The code comes in a standard [R](https://github.com/brry/course#slides) package. 

On Linux, you may first need to manually install `fftw3` for the `fftw` package in the terminal:
```
sudo apt-get install fftw3 fftw3-dev pkg-config
```
For the actual installation of `rfs`, use (within R):
```R
install.packages("berryFunctions")
berryFunctions::instGit("brry/berryFunctions")
berryFunctions::instGit("brry/rfs")
library(rfs)
?rfs
```

### use

To be expanded soon


### science

Here's the [project layout](https://github.com/brry/rfs/raw/master/document/PhDay_Poster.png), for which I won a [poster prize](https://www.uni-potsdam.de/natriskchange/activities/publications/contributions-at-conferences-workshops.html).  
The [first results](https://github.com/brry/rfs/raw/master/document/EGU17_Poster.png) were presented at EGU 2017.  
The current method is presented for [interactive analysis](https://brry.shinyapps.io/rhine/).
This app can also be run offline after installing this package (see above) with
```R
library(rfs)
rfsApp()
```
The [source code for the app](https://github.com/brry/rfs/blob/master/inst/shinyapps/rhine/app.R) is available within the package as well (in the `inst/shinyapp/` folder).

