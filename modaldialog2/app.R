library("shiny")
library("datasets")
library("DT")
library("shinyBS")

ui = shinyUI(fluidPage(
  DT::dataTableOutput("mtcarsTable"),
  bsModal("mtCarsModal", "My Modal", "",textOutput('mytext'), size = "small")
))

on_click_js = "
Shiny.onInputChange('mydata', '%s');
$('#mtCarsModal').modal('show')
"

convert_to_link = function(x) {
  as.character(tags$a(href = "#", onclick = sprintf(on_click_js,x), x))
}

shinyApp(
  ui = ui,
  server = function(input, output, session) {
    
    mtcarsLinked <- reactive({   
      mtcars$mpg <- sapply(
        datasets::mtcars$mpg,convert_to_link)
      return(mtcars)
    })
    
    output$mtcarsTable <- DT::renderDataTable({
      DT::datatable(mtcarsLinked(), 
                    class = 'compact',
                    escape = FALSE, selection=list(mode='single',target='cell')
      )
    })
    output$mytext = renderText(sprintf('mpg value is %s',input$mydata))
  }
)