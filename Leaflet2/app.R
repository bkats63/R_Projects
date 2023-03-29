library(shiny)
library(leaflet)
library(RColorBrewer)
library(RSQLite)
library(DBI)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  #tags$script("L_PREFER_CANVAS = true; setTimeout(function(){ map.invalidateSize()}, 500);"),
  tags$p(),
  actionButton("PCTSubmit","LOAD USER PLOTS"),
  leafletOutput("map", width = "100%", height = "100%")
  
  # ,
  # absolutePanel(top = 10, right = 10,
  #               sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
  #                           value = range(quakes$mag), step = 0.1
  #               ),
  #               selectInput("colors", "Color Scheme",
  #                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
  #               ),
  #               checkboxInput("legend", "Show legend", TRUE)
  #)
)

server <- function(input, output, session) {
  
  
  
  
  match_data <- reactiveValues(matches = NULL)
  d1<-NULL
  
  observeEvent(input$PCTSubmit,{
    match_data$matches<-1
    
  })
  
  
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
      read.csv("SiteXSpeciesMatrix_DummyEnvData.csv", stringsAsFactors = F)
  })
  
  
  
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    if (!is.null(match_data$matches)){
          leaflet(data=filteredData() )
    }else{
      leaflet(data=d1 )
    }
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    #pal <- colorpal()
    
    # Initialize a temporary in memory database and copy a data.frame into it
    con <- dbConnect(RSQLite::SQLite(), dbname="data/pctdatadb.sqlite")
    
    if (!is.null(match_data$matches)){
    
      rs <- dbSendQuery(con, paste0("SELECT * FROM fsdata WHERE (lat>=",min(filteredData()$Latitude)-0.1,
                                     " AND lat<=",max(filteredData()$Latitude)+0.1,") 
                                  AND (long>=",min(filteredData()$Longitude)-0.1," AND long<=",max(filteredData()$Longitude)+0.1,")  ORDER BY pctid"))
    }else{
      
      rs <- dbSendQuery(con, paste0("SELECT * FROM fsdata WHERE lat is not NULL ORDER BY pctid"))
    }
    d1 <- dbFetch(rs)
    dbHasCompleted(rs)
    dbClearResult(rs)
    # clean up
    dbDisconnect(con)
    
    colfunc <- colorRampPalette(c("AliceBlue", "AntiqueWhite",  "Aquamarine", "Azure", "Beige", "Bisque", "Black", "BlanchedAlmond", 
                                  "Blue", "BlueViolet", "Brown", "BurlyWood", "CadetBlue", "Chartreuse", "Chocolate", "Coral", "CornflowerBlue", 
                                  "Cornsilk",  "Cyan", "DarkBlue", "DarkCyan", "DarkGoldenRod", "DarkGray", "DarkGrey", "DarkGreen", 
                                  "DarkKhaki", "DarkMagenta", "DarkOliveGreen", "DarkOrange", "DarkOrchid", "DarkRed", "DarkSalmon", "DarkSeaGreen", 
                                  "DarkSlateBlue", "DarkSlateGray", "DarkSlateGrey", "DarkTurquoise", "DarkViolet", "DeepPink", "DeepSkyBlue", "DimGray", 
                                  "DimGrey", "DodgerBlue", "FireBrick", "FloralWhite", "ForestGreen",  "Gainsboro", "GhostWhite", "Gold", 
                                  "GoldenRod", "Gray", "Grey", "Green", "GreenYellow", "HoneyDew", "HotPink", "IndianRed",  "Ivory", "Khaki", 
                                  "Lavender", "LavenderBlush", "LawnGreen", "LemonChiffon", "LightBlue", "LightCoral", "LightCyan", "LightGoldenRodYellow", 
                                  "LightGray", "LightGrey", "LightGreen", "LightPink", "LightSalmon", "LightSeaGreen", "LightSkyBlue", "LightSlateGray", 
                                  "LightSlateGrey", "LightSteelBlue", "LightYellow",  "LimeGreen", "Linen", "Magenta", "Maroon", "MediumAquaMarine", 
                                  "MediumBlue", "MediumOrchid", "MediumPurple", "MediumSeaGreen", "MediumSlateBlue", "MediumSpringGreen", "MediumTurquoise", 
                                  "MediumVioletRed", "MidnightBlue", "MintCream", "MistyRose", "Moccasin", "NavajoWhite", "Navy", "OldLace",  "OliveDrab", 
                                  "Orange", "OrangeRed", "Orchid", "PaleGoldenRod", "PaleGreen", "PaleTurquoise", "PaleVioletRed", "PapayaWhip", "PeachPuff", 
                                  "Peru", "Pink", "Plum", "PowderBlue", "Purple", "RosyBrown", "RoyalBlue", "SaddleBrown", "Salmon", 
                                  "SandyBrown", "SeaGreen", "SeaShell", "Sienna",  "SkyBlue", "SlateBlue", "SlateGray", "SlateGrey", "Snow", 
                                  "SpringGreen", "SteelBlue", "Tan", "Thistle", "Tomato", "Turquoise", "Violet", "Wheat", "White", "WhiteSmoke", "Yellow", "YellowGreen"))
    
    
   
    
    categories<-d1$pctid
    RdYlBu <- colorFactor(colfunc(3000), domain = categories)
    
    if (!is.null(match_data$matches)){
      
            leafletProxy("map", data = filteredData()) %>%
              addTiles() %>%
              addProviderTiles(providers$Esri.WorldTopoMap) %>%
              clearShapes() %>%
              #setView(lat = mean(filteredData()$Latitude), lng = mean(filteredData()$Longitude), zoom = 12) %>%
              fitBounds(~min(filteredData()$Longitude), ~min(filteredData()$Latitude), ~max(filteredData()$Longitude), ~max(filteredData()$Latitude)) %>%
              
              clearMarkers()%>%
              
              addMarkers(lat = ~filteredData()$Latitude,lng = ~filteredData()$Longitude,layerId = ~filteredData()$Site_no,
                           popup = ~paste("Site no:",filteredData()$Site_no," lat:",filteredData()$Latitude," lng:",filteredData()$Longitude)) %>%
            
            
              addCircles(radius= 100, lat = ~d1$lat, lng = ~d1$long, layerId = ~d1$siteno, color = ~RdYlBu(d1$pctid),  fillOpacity = 1,
                         data = d1, popup = ~paste("Site no:",d1$siteno," lat:",d1$lat," lng:",d1$long," pct:", d1$pctname))
    }else{
      
      leafletProxy("map", data = d1) %>%
        addTiles() %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        clearShapes() %>%
        setView(lat = mean(d1$lat), lng = mean(d1$long), zoom = 11) %>%
        #fitBounds(~min(d1$long), ~min(d1$lat), ~max(d1$long), ~max(d1$lat)) %>%
        
        clearMarkers()%>%
        
        # addCircles(radius= 800, lat = ~filteredData()$Latitude,lng = ~filteredData()$Longitude,
        #               layerId = ~filteredData()$Site_no,
        #            fillColor ='red', opacity = 0.5,color = 'red',fillOpacity = 1, data = d1,
        #            popup = ~paste("Site no:",filteredData()$Site_no," lat:",filteredData()$Latitude," lng:",filteredData()$Longitude)) %>%
        
        # addMarkers(lat = ~filteredData()$Latitude,lng = ~filteredData()$Longitude,layerId = ~filteredData()$Site_no,
        #            popup = ~paste("Site no:",filteredData()$Site_no," lat:",filteredData()$Latitude," lng:",filteredData()$Longitude)) %>%
        # 
        # addCircleMarkers(lat = ~filteredData()$Latitude,lng = ~filteredData()$Longitude,layerId = ~filteredData()$Site_no,fillColor ='green',
        #                  opacity = 0.5,color = 'red',fillOpacity = 1,
        #                  popup = ~paste("Site no:",filteredData()$Site_no," lat:",filteredData()$Latitude," lng:",filteredData()$Longitude)) %>%
        
        
        
        addCircles(radius= 100, lat = ~d1$lat, lng = ~d1$long, layerId = ~d1$siteno, color = ~RdYlBu(d1$pctid),fillColor =~RdYlBu(d1$pctid),   fillOpacity = 1,
                   data = d1, popup = ~paste("Site no:",d1$siteno," lat:",d1$lat," lng:",d1$long," pct:", d1$pctname))
      
    }
   
                  
      
  })
  
  # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = quakes)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = pal, values = ~mag
  #     )
  #   }
  # })
}

shinyApp(ui, server)