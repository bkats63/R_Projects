
library(shiny)

## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    titlePanel("Hello Shiny!")
  )
  shinyApp(ui, server = function(input, output) { })
}

