
##-------------------##
##-- LIBRARY CALLS --##
##-------------------##
library(processx)
library(orca)
library(shiny)

#library('stringr')
#library('DT')
#library('shinyjs')
#suppressPackageStartupMessages(library(EpiModel))  # param.dcm, init.dcm


##------------------##
##-- SOURCE FILES --##
##------------------##

source("R/plot_resources.R")
source("R/plot_shiny_interface.R")
source('R/server_helpers.R')
source('R/styling_helpers.R')
source('R/server_utils.R')

source("R/server.routes.docs.R")
source("R/server.routes.runModel.R")
source("R/model_code/plot_simulations.R")


##----------------------##
##-- SET UP THE CACHE --##
##----------------------##
shinyOptions(cache=diskCache(file.path(dirname(tempdir()), "myapp-cache")))
# Constants / initiliazers
# CACHE = memoryCache(size = 20e6)
CACHE = diskCache(max_size = 20e6)


##------------------------------##
##-- THE MAIN SERVER FUNCTION --##
##------------------------------##
server <- function(input, output, session) {
  data.table <- reactiveVal()
  data.plot <- reactiveVal()
  
  # Print an initial message - useful for debugging on shinyapps.io servers
  print(paste0("Launching server() function - ", Sys.time()))

  # Make our session cache the cache available to all sessions
  cache = CACHE

  # Page: RunModel - Def ####  
  #server.routes.runModel = server.routes.runModel.get(
  #  input, control, init, param)
  #output$ui_main = server.routes.runModel[['ui_main']]

  output$ui_main = server.routes.runModel.get(input, session)
  
  # Page: Docs (#page-docs): output$introductionText ####
  output$introductionText = server.routes.docs
  output[['design-interventions']] = server.routes.designInterventions
  output[['help-and-feedback']] = server.routes.helpAndFeedback

  # Events: Simulate & plot ####
  # Plot: Pass to plot event handler function
  # - Alternative method: ggplotly
  # `# output$mainPlot = renderPlotly({ p = ggplot(); ggplotly(p) })``
  
  # Plot at start:
  
 # plot.and.cache = plotAndCache(input, cache)
 # cache = plot.and.cache$cache
 # output$mainPlot = plot.and.cache$plot
  
  # Plot when clicking 'Run':
  reset.handler = function(input) {
      # Plot & cache
      plot.and.cache = generate.plot.and.table(input, cache)
      # This is not needed for diskCache; only mem cache:
      # cache = plot.and.cache$cache
      
      # Update the plot
      data.plot(plot.and.cache$plot)
      output$mainPlot = renderPlotly(plot.and.cache$plot)
      
      # Update the table
      pretty.table = make.pretty.change.data.frame(
          plot.and.cache$change.df, data.type.names=DATA.TYPES)
      data.table(pretty.table)
      output$mainTable = renderDataTable(pretty.table)
      output$mainTable_message = NULL
  }
  
  observeEvent(input$reset_main, {reset.handler(input)})
  observeEvent(input$reset_main_sidebar, {
#      shinyjs::runjs("window.scrollTo(0, 0)")
      shinyjs::runjs("window.scrollTo({ top: 0, behavior: 'smooth' })")
      reset.handler(input)
      })
  

##------------------------------------##  
##-- INTERVENTION SELECTOR HANDLERS --##
##------------------------------------##
  
  # Demographic dimensions: select all
  observeEvent(input$demog.selectAll, {
    if (input$demog.selectAll == 'TRUE') {
      dims.namesAndChoices = map(
        get.dimension.value.options(
          version=version,
          location=input[['geographic_location']]), 
        function(dim) {
          list(
            'choices'=names(dim[['choices']]),
            'name'=dim[['name']] )
        })
      for (dim in dims.namesAndChoices) {
        updateCheckboxGroupInput(
          session, 
          inputId=dim[['name']], 
          selected=dim[['choices']])
      }
    }
  })
  
  ##-- LOCATION HANDLER --##
  observeEvent(input$geographic_location, {
    output$mainPlot = renderPlotly(make.plotly.message(BLANK.MESSAGE))
    message.df = data.frame(BLANK.MESSAGE)
    names(message.df) = NULL
    # matrix(BLANK.MESSAGE,nrow=1,ncol=1))
    output$mainTable = renderDataTable(message.df)  
    output$mainTable_message = renderText(BLANK.MESSAGE)
  })
  
  # Download buttons ##
  output$downloadButton.table <- downloadHandler(
    filename=function() {
      paste("Ending-HIV-in-the-US - DataTable", Sys.Date(), ".csv", sep="") },
    content=function(filepath) {
      write.csv(data.table(), filepath) } )
  
  output$downloadButton.plot <- downloadHandler(
    filename=function() {
      paste("Ending-HIV-in-the-US - Plot", Sys.Date(), ".png", sep="") },
    content=function(filepath) {
      # TODO: @Joe: Issues trying to save plot. Some possibilities:
      # - Does plotly have a way to take a class 'plotly + htmlObject'
      #   of which data.plot() is by this point, and save it?
      # - If not, can we hack a process together?
      # - Maybe we can update the `data.plot` reactiveVal within one of the 
      #   nested plot functions, and save whatever that intermediate plot
      #   object is. But it should appear the same. Otherwise, will have to
      #   think of something else.
      # - Do custom implementation of saving file to disk and sending to user
      # Resources:
      # - https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
      browser()
      
      plot = data.plot()
      # plot2 = plot_ly(plot)  # attempts to save & quits
      # plot3 = plot_ly(plot$x)  # attempts to save & quits
      # orca(plot$x, filepath)
      # orca(renderPlotly(plot), filepath)
      # orca(plot, filepath)
      # ggsave(fiepath, plot$x)
      # ggsave(...) also tried with other 'device' attrs
      # ggsave(fiepath, plot)
  } )
  
  # for now
  output$custom_int_msg_1 = renderText(NO.CUSTOM.INTERVENTIONS.MESSAGE)
  
}
