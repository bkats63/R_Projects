library(shiny)
library(shinydashboard)
library(leaflet)
library(jsonlite)
library(tidyverse)
library(leaflet.extras)
library(lubridate)

# Define UI for data upload app ----
ui <- dashboardPage(skin = "red",
                    title = "Google Location Map",
                    dashboardHeader(title = "Google Location Map", titleWidth = 300),
                    
                    # interactive sidebar with menu and widgets
                    dashboardSidebar(width = 300,
                                     
                                     tags$div(
                                       tags$blockquote("Use this app to see where Google has tracked you!"),
                                       tags$h4("How to get your Google location data:"),
                                       tags$p("Visit ", tags$a(href="https://takeout.google.com/", "Google Takeout")," to see and download any of the data Google holds on you."),
                                       tags$p("Click on SELECT NONE, then scroll down to Location History and click on the slider to select it."),
                                       tags$p("Scroll to the bottom and click NEXT, then CREATE ARCHIVE, and finally DOWNLOAD when it is ready. You will need to verify by logging into your Google account."),
                                       tags$p("This will download a ZIP file to your downloads directory. Extract this ZIP file which will create a directory called Takeout"),
                                       tags$p("Upload the JSON file found in Takeout/Location History using the selector below..."),
                                       style = "padding: 10px;"
                                       
                                     ),
                                     
                                     tags$hr(),
                                     
                                     # Input: Select a file ----
                                     fileInput("file1", "Upload Location History.json",
                                               multiple = FALSE,
                                               accept = ".json",
                                               placeholder = "Max file size 100Mb"),
                                     
                                     tags$hr(),
                                     
                                     tags$div(
                                       p("Once data is loaded, select a maptile of your preference and whether you want to see a heatmap, clustered point data, or both at the top right of the map. Point data is capped at the most recent 50,000 locations."),
                                       tags$i("Your data will only be used for the purpose of rendering this visualisation and will not be stored permamently by this application!"),
                                       style = "padding: 10px;"
                                     ),
                                     
                                     # CoI credit tag
                                     div(style = "padding: 10px;",
                                         helpText("Powered by", a("Culture of Insight", 
                                                                  href = "https://cultureofinsight.com", 
                                                                  target = "_blank")))
                                     
                    ),
                    
                    # Main panel for displaying outputs ----
                    dashboardBody(
                      
                      tags$head(tags$style("#myMap{height:90vh !important;}")),
                      
                      leafletOutput("myMap")
                      
                    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  options(shiny.maxRequestSize = 100*1024^2)
  
  output$myMap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Default Maptile") %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
      setView(24, 27, zoom = 2) %>% 
      addLayersControl(
        baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    
    withProgress(message = 'Please wait...',
                 value = 0/4, {
                   
                   req(input$file1)
                   
                   incProgress(1/4, detail = "reading data")
                   
                   locationdata <- fromJSON(input$file1$datapath, simplifyVector = TRUE, simplifyDataFrame = TRUE)
                   
                   newIcons <- iconList(
                     stand = makeIcon("stand.png", "stand.png", 36, 36),
                     drive = makeIcon("drive.png", "drive.png", 36, 36)
                   )
                   
                   incProgress(1/4, detail = "cleaning data")
                   
                   myData <- locationdata$locations %>% 
                     select(latitudeE7, longitudeE7, `timestampMs`, velocity) %>% 
                     mutate(lat = latitudeE7 / 1E7, lon = longitudeE7 / 1E7) %>% 
                     mutate(timestampMs = as.numeric(timestampMs)) %>%
                     mutate(Date = as.POSIXct(timestampMs/1000, origin="1970-01-01")) %>%
                     select(-latitudeE7, -longitudeE7) %>% 
                     mutate(image = case_when(
                       velocity > 10 ~ "drive",
                       TRUE ~ "stand"
                     )) %>% 
                     mutate(image = factor(image, levels = c("drive","stand")))
                   
                   incProgress(1/4, detail = "rendering map")
                   
                   leafletProxy("myMap", data = myData) %>%
                     fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%  
                     addHeatmap(lng = ~lon, lat = ~lat, group = "HeatMap", blur = 20, max = 0.01, radius = 15) %>%
                     addMarkers(data = head(myData, 50000), ~lon, ~lat, icon = ~newIcons[image], clusterOptions = markerClusterOptions(), 
                                label = ~ format(Date, format = "%H:%M %d-%b-%Y"), group = "Points") %>% 
                     addLayersControl(
                       baseGroups = c("Default Maptile", "Dark Maptile", "Satellite Maptile"),
                       overlayGroups = c("HeatMap", "Points"),
                       options = layersControlOptions(collapsed = FALSE)
                     )
                   
                   incProgress(1/4)
                   
                 })
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)