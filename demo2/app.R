
library(shiny)
library(ggplot2)

ui<-(pageWithSidebar(
  headerPanel("Use case - Change the side bar panel elements based on the selected tab."),
  sidebarPanel(
    
    ## conditionalPanel() functions for selected tab
    conditionalPanel(condition="input.tabselected==1",h4("Demo conditionalPanel()")),
    conditionalPanel(condition="input.tabselected==2",
                     selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'), 
                                 selected = "mtcars"),
                     radioButtons("choice","Choose an option", choices=c("Dataset" = 1, "Structure" = 2,
                                                                         "Summary" = 3 ))
                     
    ),
    
    conditionalPanel(condition="input.tabselected==3",uiOutput("varx"),uiOutput("vary"))
    
  ),
  mainPanel(
    # recommend review the syntax for tabsetPanel() & tabPanel() for better understanding
    # id argument is important in the tabsetPanel()
    # value argument is important in the tabPanle()
    tabsetPanel(
      tabPanel("About", value=1, helpText("conditionalPanel(condition, ...) creates a panel that is visible or hidden, 
                                          depending on the condition given. The condition is evaluated once at 
                                          startup and whenever Shiny detects a relevant change in input/output.
                                          ")),
      tabPanel("Data", value=2, conditionalPanel(condition="input.choice==1", verbatimTextOutput("dat")),
               conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
               conditionalPanel(condition="input.choice==3", verbatimTextOutput("summary"))),
      tabPanel("Plot", value=3, plotOutput("plot")), 
      id = "tabselected"
      )
    )
  ))



server <-(function(input,output)({
  
  ## Get the value of the dataset that is selected by user from the list of datasets
  data <- reactive({
    get(input$dataset)
  })
  
  ## to output the dataset
  output$dat <- renderPrint({
    data()
  })
  
  # Pulling the list of variable for choice of variable x
  output$varx <- renderUI({
    selectInput("variablex", "select the X variable", choices=names(data()))
  })
  
  # Pulling the list of variable for choice of variable y
  output$vary <- renderUI({
    selectInput("variabley", "select the Y variable", choices=names(data()))
    
  })
  
  # to output the structure of the dataset
  output$struct <- renderPrint({
    str(get(input$dataset))
  })
  
  # for summary
  output$summary <- renderPrint({
    summary(get(input$dataset))
  })
  
  # For plot
  output$plot <- renderPlot({
    ggplot(data(),aes_string(x=input$variablex, y=input$variabley)) +
      geom_point() 
  }) 
  
}))


shinyApp(ui, server)
