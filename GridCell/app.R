library(shiny)

ui <- fluidPage(
  #numericInput("x", "how many cells to the left/right?", min=-5, max=5, value=0),
  #numericInput("y", "how many cells to the top/bottom?", min=-5, max=5, value=0),
  DT::dataTableOutput("dt"),
  uiOutput("headline")
  #verbatimTextOutput("shifted_cell")
)

server <- function(input, output) {
  
  output$headline <- renderUI({
    
    if (!is.null(input$dt_cell_clicked$col))
    {
    conn <-colnames(mtcars[input$dt_cell_clicked$col])
    
    if (conn=="mpg"){
      h3(paste0("You clicked value ", input$dt_cell_clicked$value, ".    cc ",colnames(mtcars[input$dt_cell_clicked$col])))
     
      
              #input$x, " cells to the ", ifelse(input$x > 0, "right", "left"), " and ",
              #input$y, " cells to the ", ifelse(input$y > 0, "bottom", "top"), " is:"))
    }else{ h3("")}
    }else{
      
      h3("")
    }
    
  })
  
  # the value of the shifted cell
  # output$shifted_cell <- renderPrint({
  #   mtcars[input$dt_cell_clicked$row + input$y, # clicked plus y to the bottom/top
  #          input$dt_cell_clicked$col + input$x] # clicked plus x to the left/right 
  #})
  
  # the datatable
  output$dt <- DT::renderDataTable({
    DT::datatable(mtcars, select="none")})
}

shinyApp(ui, server)






