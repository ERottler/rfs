# RHINE SEASONALITY CHANGE APP
# Load package and data ----
library(rfs)

server <- function(input, output) {

# Reactive selection ----

sd_sel <- reactive(if(input$smooth) input$sd else -1)

loc_sel <- reactive({
m <- meta[gnames(app=TRUE),]
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
                choices=gnames(app=TRUE), selected=loc_sel())
})


# Main plot ----

output$seasplot <- renderPlot({
par(bg="grey96")
qdoyVisPeriods(loc_sel(), seas, sd=sd_sel(), map=FALSE, mainsep=", ",
               mar=c(1.8,3.2,1.8,0.2), dist=input$dist, RPs=input$RPs,
               ylim=input$ylim)
mtext(" Streamflow  [m\U{00B3}/s]", adj=0, line=-1.4, outer=TRUE)
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
sp::plot(map, add=TRUE, border=berryFunctions::addAlpha("white"))
text(countries$x, countries$y, countries$country, cex=1.5, col="white")
points(lat~lon, data=meta[gnames(app=TRUE),], pch=3, lwd=2, col="red")
points(lat~lon, data=meta[loc_sel(),], cex=3, lwd=2, col="orange")
})
}

### User interface layout ####

ui <- fixedPage(
  titlePanel("Streamflow seasonality changes along the Rhine river"),
  sidebarLayout(
    sidebarPanel(
      a("App", href="https://github.com/brry/rfs/blob/master/inst/shinyapps/rhine/app.R"),
      "by Berry Boessenkool,", a("berry-b@gmx.de", href="mailto:berry-b@gmx.de"),
      br(),br(),
      "Flood regimes along the Rhine river are changing, as described on the ", 
      a("project homepage.", href="https://github.com/brry/rfs#rfs"), 
      "This app enables an interactive analysis of long term records of several river flow gauges.",
      br(),
      "For each day of the year in certain time periods (see the top right legend),",
      "the 30 values are aggregated with empirical or parametrical quantile estimators. ",
      "For the latter, the general extreme value distribution (GEV) is fitted via L-moments.",
      br(),br(),
      uiOutput("location"),
      checkboxInput("smooth", strong("Apply FFT smoothing"), value=FALSE),
      sliderInput("sd", "Degree of smoothing", min=1, max=10, value=3, step=0.1),
      radioButtons("dist", "Quantile estimator", choices=dimnames(seas[[1]])$dist[1:2]),
      numericInput("ylim", "y axis limit (NA or empty for automatic)", value=NA, min=0, max=20e3, step=500),
      checkboxGroupInput("RPs", "Return periods", choices=RPvals, selected=c(1.111, 2, 10, 100))
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
