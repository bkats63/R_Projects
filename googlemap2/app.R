library(shinydashboard)
library(googleway)

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody( 
   
        google_mapOutput(outputId = "map") #, width = "100%", height = "90%")
  )
)

server <- function(input, output) {
  set_key("AIzaSyCiWLYufriPNDL7DIiWKyeYeKYVSDCPoZ0")
  
  
  map_key <- 'AIzaSyCiWLYufriPNDL7DIiWKyeYeKYVSDCPoZ0'
  
  output$map <- renderGoogle_map({
    
  google_map(key = map_key, data = tram_stops) %>%
    add_circles(lat = "stop_lat", lon = "stop_lon", fill_colour = "stop_name",
                stroke_weight = 1, stroke_colour = "stop_name", info_window ="stop_id")
  
  # ## different colour palettes
  # lstPalette <- list(fill_colour = colorRampPalette(c("red","blue")),
  #                    stroke_colour = viridisLite::plasma)
  # 
  # ## set the key via set_key()
  # set_key(key = map_key)
  # google_map(data = tram_stops) %>%
  #   add_circles(lat = "stop_lat", lon = "stop_lon", fill_colour = "stop_lat",
  #               stroke_weight = 2, stroke_colour = "stop_name", palette = lstPalette, legend = T)
  # 
  # ## controlling the legend
  # google_map(data = tram_stops) %>%
  #   add_circles(lat = "stop_lat", lon = "stop_lon", fill_colour = "stop_lat",
  #               stroke_weight = 2, stroke_colour = "stop_name",
  #               legend = c(fill_colour = T, stroke_colour = F),
  #               legend_options = list(position = "TOP_RIGHT", css = "max-height: 100px;"))
  # 
  # google_map(data = tram_stops) %>%
  #   add_circles(lat = "stop_lat", lon = "stop_lon", fill_colour = "stop_lat",
  #               stroke_weight = 2, stroke_colour = "stop_name",
  #               legend = T,
  #               legend_options = list(
  #                 fill_colour = list(position = "TOP_RIGHT", css = "max-height: 100px;"),
  #                 stroke_colour = list(position = "LEFT_BOTTOM", title = "Stop Name")
  #               ))
  # 
  # 
  # 
  
  
  })
}

shinyApp(ui, server)

