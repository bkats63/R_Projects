library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    DT::dataTableOutput("mtcarsTable"),
    DT::dataTableOutput("irisss")
  ),
  
  
  server = function(input, output) {
    
    output$mtcarsTable <- DT::renderDataTable({
      DT::datatable(datasets::mtcars[,1:3], 
                    options = list(
                      
                      columnDefs = list(list(
                        targets = 0,
                        render = JS(
                          "function(data, type, row, meta) {",
                          "return type === 'display' && data.length > 6 ?",
                          "'<a href=\"http://www.google.com.au?'+data.substr(0, 6)+'\" target=_blank>' + data.substr(0, 6) + '...</a>' : data;",
                          "}")
                      )),
                      
                    
                      rowCallback = JS(
                      "function(nRow, aData) {",
                      "var full_text = aData[0] + ','+ aData[1] + ',' + aData[2] + ','+ aData[3];",
                      "$('td:eq(0)', nRow).attr('title', full_text);}"
                      )
                  
                      
                    ), selection=list(mode='single',target='cell')
                    , callback = JS("table.order([3, 'asc']).draw();")
      )
      
    })
    
    output$irisss <- DT::renderDataTable({datatable(iris[c(1:20, 51:60, 101:120), ],
              options = list(columnDefs = list(list(
      targets = 5,
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 6 ?",
        "'<a href=\"http://www.google.com.au?'+data.substr(0, 6)+'\" target=_blank>' + data.substr(0, 6) + '...</a>' : data;",
        "}")
    ))) , selection=list(mode='single',target='cell'))
    })
    
  }
)