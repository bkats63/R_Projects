## using split view

library(shinydashboard)
library(googleway)


ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    
    box(google_mapOutput(outputId = "map", width = "100%", height="800px"), width = "100%", height="800px")
    
    #box(plotOutput("plot"), width = "100%", height="800px")
    
    
  )
)

server <- function(input, output) {
  

  
  
  output$map <- renderGoogle_map({

    ## the same polygon, specified using coordinates, and with a second independent
    ## polygon
    df <- data.frame(myId = c(1,1,1,1,1,1,2,2,2),
                     lineId = c(1,1,1,2,2,2,1,1,1),
                     lat = c(26.774, 18.466, 32.321, 28.745, 29.570, 27.339, 22, 23, 22),
                     lon = c(-80.190, -66.118, -64.757, -70.579, -67.514, -66.668, -50, -49, -51),
                     colour = c(rep("#00FF0F", 6), rep("#FF00FF", 3)),
                     stringsAsFactors = FALSE)


map_key <- 'AIzaSyCiWLYufriPNDL7DIiWKyeYeKYVSDCPoZ0'



set_key(key=map_key, api = c("default", "map", "directions", "distance", "elevation",
                     "geocode", "places", "find_place", "place_autocomplete", "places_details",
                     "roads", "streetview", "timezone"))

google_map(key = map_key) %>%
  add_polygons(data = df, lat = 'lat', lon = 'lon', id = 'myId', pathId = 'lineId',
               fill_colour = 'colour')


  })
  
  
}

shinyApp(ui, server)
