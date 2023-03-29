#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("google maps test"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
       
        
        
        
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  crimes <- read.csv("data/police-uk-2014-04-merseyside-street.csv")
  #load library
  library(sp)
  #change the crimes data into a SpatialPointsDataFrame
  coords <- cbind(Longitude = as.numeric(as.character(crimes$Longitude)), Latitude = as.numeric(as.character(crimes$Latitude)))
  crime.pts <- SpatialPointsDataFrame(coords, crimes[, -(5:6)], proj4string = CRS("+init=epsg:4326"))
   
  #plot the  hybrid Google Maps basemap
  map <- qmap('Liverpool', zoom = 12, maptype = 'hybrid')
  #plot the crime points on top
  map + geom_point(data = crimes, aes(x = Longitude, y = Latitude), color="red", size=3, alpha=0.5)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

