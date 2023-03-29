#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# library(shiny)
# 
# # Define server logic required to draw a histogram
# shinyServer(function(input, output) {
# 
#   output$distPlot <- renderPlot({
# 
#     tryCatch({
#       
#           
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     
#       }, warning = function(w) {      
#         showNotification(paste0("Warning: ",w),duration = 10,type = c("warning"))
#         # Choose a return value in case of warning
#         return(NULL)
#       }, error = function(e) {
#         showNotification(paste0("Error: ",e),duration = 10,type = c("error"))
#         # Choose a return value in case of error
#         return(NULL)
#       }, finally = {
#         #message("Some other message at the end")
#       })
# 
#   })
#     
#     
# 
# })




library(shiny)

library(futile.logger)
library(tryCatchLog)

# options(keep.source = TRUE)        # source code file name and line number tracking
# #options("tryCatchLog.write.error.dump.file" = TRUE) # dump for post-mortem analysis
# 
# flog.appender(appender.file("my_app.log"))  # to log into a file instead of console
# flog.threshold(ERROR)    # TRACE, DEBUG, INFO, WARN, ERROR, FATAL
# tryCatchLog(source("server.R"))

# Define server logic for random distribution application
shinyServer(function(input, output) {

  
  
  
  

  # Reactive expression to generate the requested distribution. This is
  # called whenever the inputs change. The output renderers defined
  # below then all used the value computed from this expression
  data <- reactive({

    tryCatch({
      
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

   


    dist(input$n)

    # }
    # , warning = function(w) {
    # 
    #   showNotification(paste0("Warning: ",w),duration = 10,type = c("warning"))
    #   # Choose a return value in case of warning
    #   return(NULL)
    }, error = function(e) {
      showNotification(paste0("Error: ",e),duration = 10,type = c("error"))
      errmsg <- paste(Sys.time(), ", Error:",e)
      write(errmsg, paste0(file.path(getwd(), "www"),"/errorlog.txt"), append = TRUE) 
      # Choose a return value in case of error
       return(NULL)
    }, finally = {
      #message("Some other message at the end")
    })


  })



  # Generate a plot of the data. Also uses the inputs to build the
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n

    hist(data(),
         main=paste('r', dist, '(', n, ')', sep=''))
  })

  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })

  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })





})