#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#





library(shiny)
library(raster)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
       
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
         ,
         plotOutput("distPlot22")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
  
  
  library(rgdal)
  max_temp <- readGDAL('maxann.grid')
  ##summary(max_temp)
  
  ##plot(max_temp)
  
  library(inlmisc)
  spdf_1 <- Grid2Polygons(max_temp)
  
  
  spdf_2 <- as(max_temp,'SpatialPolygonsDataFrame')
  
  sf_data <- st_as_sf(spdf_2)
  
  
  
  library(raster)
  r <- raster(file.path('G:\\GISdata\\ASCII\\DEM', 'dems1s.asc'), crs='+proj=longlat')
  pts<-rasterToPoints(r,spatial = T)
  
  #poly<-rasterToPolygons(r)
  
  #x <- rasterToContour(r)
  
  shapefile(pts, 'data\\DEMs1s.shp')
  
  plot(pts)
  
  
  plot(x)
  
  plot(poly)
  
  spplot(poly)
  
  spplot(pts)
  
  
  library(rgdal)
  writeOGR(poly, dsn = getwd(), layer = "polyrainann", driver = "ESRI Shapefile")
  
  library(spex)
  
  psf <- qm_rasterToPolygons(r, na.rm = TRUE)
  
  psf <- polygonize(r, na.rm = TRUE)
  
  library(rgdal)
  writeOGR(qm, dsn = getwd(), layer = "temppoly1", driver = "ESRI Shapefile")
  
  

  library(quadmesh)
  qm <- quadmesh(r)
  
  
  
  ######-----------------------------------------------------------
  
  
  library(raster)
 
  r <- raster(file.path(getwd(), 'maxann.grid'), crs='+proj=longlat')

  x <- rasterToContour(r)
  class(x)
  
  
  plot(r)
  plot(x, add=TRUE)

  
  library(rgdal)
  writeOGR(x, dsn = getwd(), layer = "RNOZAN", driver = "ESRI Shapefile")
  
  
  
  library(raster)
  r <- raster(file.path(getwd(), 'rnozan.grid'), crs='+proj=longlat')
  pts<-rasterToPoints(r,spatial = T)
  
  plot(pts)
  
  shapefile(pts, 'rnozan.shp')
  
  library(rgdal)
  writeOGR(pts, dsn = getwd(), layer = "rnozan2", driver = "ESRI Shapefile")
  
  
 shp <- readOGR(dsn = file.path(getwd(), "rnozan.shp"), layer ='rnozan' )
 plot(shp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

