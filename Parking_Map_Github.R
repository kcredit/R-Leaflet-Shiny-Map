#install.packages("rgdal")
#install.packages("RColorBrewer")
#install.packages("leaflet", dep = T)
#install.packages("htmlwidgets")
#install.packages("shiny")
#install.packages('rsconnect')
#install.packages("Rcpp")
#install.packages("shinyjs")
library(shiny)
library(rsconnect)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library(Rcpp)
library(shinyjs)

#set your working directory
#setwd(.....)

#Read in shapefile - in this case, we are using a polyline shapefile
#for parking restrictions on various streets
streets <- readOGR("Parking Streets.shp",
                  layer = "Parking Streets")
#Project the shapeifle into coordinates that Leaflet can use (WGS84)
streets <- spTransform(streets, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(streets)
streets$Level <- as.numeric(streets$Level)

# Create a continuous palette function for visualizing parking restrictions
#(green = fewer restrctions,  red = more)
pal <- colorNumeric(
  palette = c("green", "yellow", "red"),
  domain = streets$Level)

#Create the leaflet map
m <- leaflet(streets) %>%
  addTiles(options = tileOptions(opacity=.5)) %>%  # Add default OpenStreetMap map tiles
  addPolylines(color= ~pal(Level), label= ~Details) %>%
  addMarkers(lng=-87.624450, lat=41.863938, label="1400 S. Michigan") # Add marker showing home address
  
m  # Print the map

# ==== function in javascript allowing geolocalisation on Shiny app
jsCode <- '
shinyjs.geoloc = function() {
navigator.geolocation.getCurrentPosition(onSuccess, onError);
function onError (err) {
Shiny.onInputChange("geolocation", false);
}
function onSuccess (position) {
setTimeout(function () {
var coords = position.coords;
console.log(coords.latitude + ", " + coords.longitude);
Shiny.onInputChange("geolocation", true);
Shiny.onInputChange("lat", coords.latitude);
Shiny.onInputChange("long", coords.longitude);
}, 5)
}
};
'

##### Create shiny app
server <- function(input, output) {
  
  # Basic map
  map <- m
  output$map <- renderLeaflet(map)
  
  # Find geolocalisation coordinates when user clicks
  observeEvent(input$geoloc, {
    js$geoloc()
  })
  
  
  # zoom on the corresponding area
  observe({
    if(!is.null(input$lat)){
      map <- leafletProxy("map")
      dist <- 0.0035
      lat <- input$lat
      lng <- input$long
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    }
  })
  
}

# ==== UI
ui <- fluidPage(
  
  # Tell shiny we will use some Javascript
  useShinyjs(),
  extendShinyjs(text = jsCode),
  
  # One button and one map
  br(),
  actionButton("geoloc", "Localize me", class="btn btn-primary", onClick="shinyjs.geoloc()"),
  leafletOutput("map", height="600px")
)

#Creates and launches the Shiny App
shinyApp(ui = ui, server = server)


#method for manually exporting the html for personal use
#saveWidget(widget = m, file="m.html", selfcontained = FALSE)
