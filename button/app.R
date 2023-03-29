
library(shinydashboard)

ui<-shiny::fluidRow(
  shinydashboard::box(title = "Intro Page", "Some description...", 
                      shiny::actionButton(inputId='ab1', label="Learn More", 
                                          icon = icon("fas fa-external-link-alt"), 
                                          onclick ="window.open('http://google.com', '_blank')")
  )

)

shinyApp(ui, server = function(input, output) { })


