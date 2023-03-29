

library(shiny)

ui <- htmlTemplate("main.html",  sliderValue=33)




server <- function(input, output) {
  
 
  
  
}


# Create Shiny app ----
shinyApp(ui, server)