# Load libraries
library("leaflet")

ui <- shinyUI(fluidPage(sidebarLayout(
  sidebarPanel(
    hr(),
    h4("Remove points"),
    checkboxGroupInput(
      inputId = "removeFromMap",
      label = "",
      choices = c(1:6),
      selected = c(1:6)
    ),
 
    hr(),
    p("Checked = removed"),
    p("Unchecked = present")
  ),
  mainPanel(leafletOutput("map"))
)))

# ID must be a character
df <- data.frame(
  id = as.character(1:6),
  lng = rnorm(6, -106.1039361, 0.5),
  lat = rnorm(6, 50.543981, 0.5)
)

server <- shinyServer(function(input, output, session) {
  output$map <- renderLeaflet(
    leaflet() %>%
      setView(-106.1039361, 50.543981, zoom = 5) %>%
      addTiles() %>%
   
      
      addCircleMarkers(
        layerId = df$id,
        df$lng,
        df$lat,
        group = 'marker',
        radius = 3,
        fill = TRUE,
        color = '#ff0000'
      )
  )
  
  # Global ID vector
  ids <- df$id
  allids<-df$id
  
  # Remove points
  observe({
    checkedPoints <- input$removeFromMap
    #checkedPoints <- checkedPoints[which(checkedPoints %in% allids)]
    
    proxy <- leafletProxy('map')
    # 
    if (length(checkedPoints) != length(df$id)) {
    #  
         pointsToKeep <- checkedPoints[which(checkedPoints %in% allids)]
         pointsC1<-setdiff(allids,pointsToKeep)
         proxy %>% removeMarker(layerId = pointsC1)
         
        
         
        proxy %>% addCircleMarkers(
          layerId = df$id[df$id %in% pointsToKeep],
          df$lng[df$id %in% pointsToKeep],
          df$lat[df$id %in% pointsToKeep],
          group = 'marker',
          radius = 3,
          fill = TRUE,
          color = '#ff0000'
        )
      
        
       #ids <<- ids[-which(ids == pointsC1)]
    #     
    # 
    }
    
    else
    {
      proxy %>% addCircleMarkers(
        layerId = df$id,
        df$lng,
        df$lat,
        group = 'marker',
        radius = 3,
        fill = TRUE,
        color = '#ff0000'
      )
      
      
    }
    
    
  })
  
})

shinyApp(ui, server)