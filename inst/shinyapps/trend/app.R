# TREND APP
# Load packages and data ----
library(extremeStat) ; library(rfs) ; library(berryFunctions)
load(seasFolder("data/dismeta.Rdata"))

seasTrend <- function(n, RPs=1.2, map=FALSE, trendpeakonly=FALSE)
{
# Get threshold from RPs:
annMax <- annualMax(dis$date, dis[,n], shift=0)
if(RPs>1)
{
dle <- distLextreme(annMax$max, gpd=FALSE, sel="gev", RPs=RPs, quiet=TRUE)
threshold <- dle$returnlev["gev",1]
} else
if(almost.equal(RPs, 1)) threshold <- min(annMax$max, na.rm=TRUE)
else threshold <- 0
# Seasonality for all values > threshold:
large <- which(dis[,n]>=threshold)
s <- seasonality(dis[large,"date"], dis[large,n], shift=61, nmax=1, pch=15,
            main=paste0(n, ", ", meta[n,"river"]), returnall=TRUE, adj=0.4,
            drange=1930:2012, legend=FALSE, hlines=TRUE)
# Trend line:
s2 <- if(trendpeakonly)  s$annmax[between(s$annmax$year, 1960, 2010),]else
                         s$data[  between(  s$data$year, 1960, 2010),] 
linReg(doy~year, data=s2, add=TRUE, plotrange=1960:2011, pos1=NA)
# Legend and map:
colPointsLegend(z=dis[large,n], nlab=4, title="Streamflow  [m\U{00B3}/s]", 
                y1=0.89, y2=1)
if(map) minimap(n)
}



server <- function(input, output) {

# Reactive selection ----

loc_sel <- reactive({
m <- meta[gnames(trend=TRUE),]
loc <- "Lobith"
if(!is.null(input$location)) loc <- input$location
if(!is.null(input$plot_click)){
  pdist <- berryFunctions::distance(x=m$lon, xref=input$plot_click$x, 
                                    y=m$lat, yref=input$plot_click$y)
  loc <- m[which.min(pdist), "name"]
  }
loc
})

output$location <- renderUI({selectInput("location", "Choose a location, or click on the map", 
                choices=gnames(trend=TRUE), selected=loc_sel())
})


# Main plot ----

output$seasplot <- renderPlot({
par(bg="grey96")
seasTrend(loc_sel(), RPs=input$RPs, map=FALSE, trendpeakonly=input$tpeak)
#box("figure", col=4)
})


# Map data ----

xlim <- c(4,14.5)
ylim <- c(46.24, 52.04)
omap <- list(tiles=list(list(projection=OSMscale::pll())),
             #bbox=list(p1=par("usr")[c(1,4)], p2=par("usr")[2:3]))
             bbox=list(p1=c(xlim[1],ylim[2]), p2=c(xlim[2],ylim[1])))
countries <- read.table(header=TRUE, text="
    x     y country
15.8 51.8 PO
14.5 50.0 CZ
11.2 48.5 DE
 5.4 52.0 NL
 4.7 50.6 BE
6.13 49.7 LU
 5.0 47.5 FR
 8.4 46.7 CH
11.2 46.3 IT
14.0 47.3 AT
14.8 46.2 SL
# 9.53 47.16 LI
#10.6 51.2 DE")
dem <- png::readPNG(system.file("extdata/dem_small.png", package="rfs"))

# Map plot ----

output$map <- renderPlot({
par(mar=rep(0,4), bg="lightblue")
sp::plot(map, xlim=xlim, ylim=ylim, asp=1.4, col="grey80", border="white")
rasterImage(dem, xleft= 5.11, ybottom=46.24-0.05,
                xright=12.02,    ytop=52.04+0.07)
OSMscale::scaleBar(omap, abslen=300, ndiv=3, x=0.5, y=0.95)
sp::plot(map, add=TRUE, border=addAlpha("white"))
text(countries$x, countries$y, countries$country, cex=1.5, col="white")
points(lat~lon, data=meta[gnames(trend=TRUE),], pch=3, lwd=2, col="red")
points(lat~lon, data=meta[loc_sel(),], cex=3, lwd=2, col="orange")
})
}

### User interface layout ####

ui <- fixedPage(
  titlePanel("Streamflow seasonality changes along the Rhine river"),
  sidebarLayout(
    sidebarPanel(
      "App by Berry Boessenkool,", a("berry-b@gmx.de", href="mailto:berry-b@gmx.de"), 
      br(), "Flood seasonality", a("project homepage", href="https://github.com/brry/rfs#rfs"), br(),
      "App", a("source code", href="https://github.com/brry/rfs/blob/master/inst/shinyapps/trend/app.R"), 
      br(),br(),
      "All streamflow observations above a certain threshold are displayed as ",
      "day of the year (DOY) depending on the hydrological year.",
      "The color specifies the streamflow value as noted in the legend.", 
      br(),
      "The threshold is determined as the GEV return level for the entire time ",
      "series of observations for a given return period.",
      br(), br(),
      "A linear regression line is added for all the doy-year pairs of the high ",
      "streamflow values in the 50 years between 1960 and 2010. ",
      "The crosses mark the doy with the annual maximum.", 
      "If desired, only those are used for the trendline.", br(), br(),
      uiOutput("location"),
      numericInput("RPs", "Return period for threshold", value=1.2, min=0.9, max=200, step=0.1),
      checkboxInput("tpeak", strong("Use only annual peaks for trend line"))
    ),
    # Show seasonality change plot
    mainPanel(plotOutput("seasplot"),
              plotOutput("map", click="plot_click"))
    # plotOutput click # for map
    ,
    fluid=FALSE
    )
)
shinyApp(ui = ui, server = server)
