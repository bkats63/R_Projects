

library(googleway)

map_key <- 'AIzaSyCiWLYufriPNDL7DIiWKyeYeKYVSDCPoZ0'

renderGoogle_map({
  
  ## different colour palettes
  lstPalette <- list(fill_colour = colorRampPalette(c("red","blue")),
                     stroke_colour = viridisLite::plasma)
  
  google_map(key = map_key, data = tram_stops) %>%
    add_circles(lat = "stop_lat", lon = "stop_lon", fill_colour = "stop_name",
                stroke_weight = 0.3, stroke_colour = "stop_name", palette = lstPalette, info_window ="stop_id")
  
})


