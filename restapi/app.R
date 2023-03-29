
library(httr)
library(jsonlite)
library(DT)
library(shiny)

ui <- fluidPage(
  headerPanel("calling api service test"),
  
  fluidRow(
    column(12, DT::dataTableOutput("apidata_table"))
  )

)

server <- function(input, output) {
  
  base <- "https://biodiversity.my.opendatasoft.com/api/odata/"
  endpoint <- "testdata"
  odataapikey <- "apikey=e37a9d51f1cf91617f70085f3c0be6882dd589b497742ce2df604dde"
  query <- "&$filter=pctid%20eq%202"


  callapi <- paste(base,endpoint,"?",odataapikey,query,sep="")

  get_pdata <- GET(callapi)
  get_pdata_text <- content(get_pdata, "text")

  get_pdata_json <- fromJSON(get_pdata_text, flatten = TRUE)

  get_pdata_df <- as.data.frame(get_pdata_json)

  output$apidata_table <- renderDataTable({get_pdata_df})


 
  
}

shinyApp(ui, server)
