library(DT)
library(shiny)

library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  uiOutput("headline"),
  actionButton("Submit","Submit"),
  DT::dataTableOutput('Table')
)

server <- function(input, output, session) {
  
  
  observe({
    
    if (!is.null(input$Table_cell_clicked$col))
    {
      conn <-colnames(mtcars[input$Table_cell_clicked$col])       
      if (conn=="mpg"){
        shinyjs::show("Submit")
      } else {
        shinyjs::hide("Submit")
      }
    }else{
      
      shinyjs::hide("Submit")
    }
    
  })
  
  
  output$Table <- renderDataTable({datatable(mtcars, selection=list(mode="single",target="cell"))})
  
  Clicked <- eventReactive(input$Table_cell_clicked,{
    #input$Table_cell_clicked$value
    
    if (!is.null(input$Table_cell_clicked$col))
    {
      conn <-colnames(mtcars[input$Table_cell_clicked$col])
      
      if (conn=="mpg"){
        
        
        h3(paste0("You clicked value ", input$Table_cell_clicked$value, ".    cc ",colnames(mtcars[input$Table_cell_clicked$col])))
       
      }else{ h3("")}
    }else{
      
      h3("")
    }
  })
  
  
  
  output$selected <- renderText({paste0("You Selected cell value: ",Clicked())})
  
  observeEvent(input$Submit,{
    if (!is.null(input$Table_cell_clicked$col))
    {
      conn <-colnames(mtcars[input$Table_cell_clicked$col])
      
      if (conn=="mpg"){
            showModal(modalDialog( h2("Row Selection Example"),size = "l",br(),textOutput("selected")))
      }
    }
  })
  
  output$headline <- renderUI({
    
    if (!is.null(input$Table_cell_clicked$col))
    {
      conn <-colnames(mtcars[input$Table_cell_clicked$col])
      
      if (conn=="mpg"){
        h3(paste0("You clicked value ", input$Table_cell_clicked$value, ".    cc ",colnames(mtcars[input$Table_cell_clicked$col])))
     
      }else{ h3("")}
    }else{
      
      h3("")
    }
    
  })
  
  
  
}

shinyApp(ui, server)


##row selection code
# server <- function(input, output, session) {
#   
#   output$Table <- renderDataTable({datatable(mtcars,  selection=list(mode='single',target='row'))})
#   
#   Clicked <- eventReactive(input$Table_rows_selected,{
#     rownames(mtcars[input$Table_rows_selected,])
#   })
#   
#   output$selected <- renderText({paste0("You Selected Row: ",Clicked())})
#   
#   observeEvent(input$Submit,{
#     showModal(modalDialog( h2("Row Selection Example"),size = "l",br(),textOutput("selected")))
#   })
# }