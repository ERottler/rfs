# TREND APP
# Load packages and data ----

server <- function(input, output) {

if(!exists("dis")) {library(rfs) ; library(berryFunctions)
                    load(seasFolder("data/dismeta.Rdata"), envir=globalenv())}
# Reactive selection ----

loc_sel <- reactive({
m <- meta[gnames("trend"),]
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
                choices=gnames("trend"), selected=loc_sel())
})


# Main plot ----

output$seasplot <- renderPlot({
par(bg="grey96")
seasTrend(loc_sel(), RP=input$RPs, shift=input$shift, map=FALSE,
          trex=input$trex, peak=input$peak, legpos=input$legpos)
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
points(lat~lon, data=meta[gnames("trend"),], pch=3, lwd=2, col="red")
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
      "A linear regression line (orange) is added for all the doy-year pairs of the high ",
      "streamflow values in the 50 years between 1960 and 2010. ",
      "The crosses mark the doy with the annual maximum of those threshold exceedances.", 
      "For these doys, the regression is plotted in purple.", br(), br(),
      uiOutput("location"),
      numericInput("RPs", "Return period for threshold", value=1.2, min=0.9, max=10, step=0.1),
      strong("Show trend line for:"),
      checkboxInput("trex", "all doys above threshold", value=TRUE),
      checkboxInput("peak", "annual peaks of threshold exceedances", value=TRUE),
      sliderInput("shift", strong("Yearbreak shift"), min=0, max=200, value=61, step=1),
      selectInput("legpos", strong("Legend position"), 
                  choices=c("left", "topleft", "top", "topright", "right", 
                            "bottomright", "bottom", "bottomleft"))
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
