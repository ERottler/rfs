# rfs
Rhine Flood Seasonality  
PhD project by Berry Boessenkool, <berry-b@gmx.de>.

### install

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
The [source code for the app](https://github.com/brry/rfs/blob/master/inst/shinyapp/app.R) is available within the package as well (in the `inst/shinyapp/` folder).

