## using split view

library(shinydashboard)
library(googleway)
library(sp)

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
   
      box(google_mapOutput(outputId = "map", width = "100%", height="800px"), width = "100%", height="800px")
    
    #box(plotOutput("plot"), width = "100%", height="800px")
    
    
  )
)

server <- function(input, output) {
  
  
  
  
# library(sp)
 
  #change the crimes data into a SpatialPointsDataFrame
  #coords <- cbind(Longitude = as.numeric(as.character(crimes$Longitude)), Latitude = as.numeric(as.character(crimes$Latitude)))
  #crimes <- SpatialPointsDataFrame(coords, crimes[, -(97:98)], proj4string = CRS("+init=epsg:4326"))
  
  # output$plot <- renderPlot({
  # 
  # library(ggmap)
  # map_key <- 'AIzaSyCiWLYufriPNDL7DIiWKyeYeKYVSDCPoZ0'
  # register_google(key=map_key)
  # # markers and paths are easy to access
  # d <- function(x=-95.36, y=29.76, n,r,a){
  #   round(data.frame(
  #     lon = jitter(rep(x,n), amount = a),
  #     lat = jitter(rep(y,n), amount = a)
  #   ), digits = r)
  # }
  # df <- d(n=50,r=3,a=.3)
  # map <- get_googlemap(markers = df, path = df, scale = 2)
  # ggmap(map)
  # 
  # })
  
  output$map <- renderGoogle_map({

    crimes <- read.csv("TESTMAP.csv", stringsAsFactors = F)
    map_key <- 'AIzaSyCiWLYufriPNDL7DIiWKyeYeKYVSDCPoZ0'
    ## different colour palettes
    # lstPalette <- list(fill_colour = colorRampPalette(c("red","blue")),
    #                    stroke_colour = viridisLite::plasma)
    
    
    set_key(key=map_key, api = c("default", "map", "directions", "distance", "elevation",
                                 "geocode", "places", "find_place", "place_autocomplete", "places_details",
                                 "roads", "streetview", "timezone"))
     google_map(key = map_key, data = crimes, zoom = 14) %>%
      add_circles(lat = "Latitude", lon = "Longitude", fill_colour = "Site_no",
                  stroke_weight = 1, stroke_colour = "Site_no", info_window ="Site_no")
     
     



  })
  
  
}

shinyApp(ui, server)









