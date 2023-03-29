lemon <- read.table(text="Response, Factor 1, Factor 2, Factor 3
5, 2, 5, 2        
                    7, 1, 4, 3", header=T, sep=",")
csvuploaded<-FALSE

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        radioButtons("csvuploaded", "uploaded", c(T, F)),  # change csvuploaded
        uiOutput(outputId="factorcheckboxes")
      ),
      mainPanel()
    )), 
  server = function(input, output) {    
    output$factorcheckboxes <- renderUI({
      if(input$csvuploaded) {
        checkboxGroupInput(inputId="variable",
                           label="Variable:",
                           choices=colnames(lemon), selected=NULL, inline=FALSE)
      } else { NULL }
    })
  }
)