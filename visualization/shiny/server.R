
##-------------------##
##-- LIBRARY CALLS --##
##-------------------##

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
  
  # Print an initial message - useful for debugging on shinyapps.io servers
  print(paste0("Launching server() function - ", Sys.time()))
  
  # Make our session cache the cache available to all sessions
  cache = CACHE

  # Page: RunModel - Def ####  
  #server.routes.runModel = server.routes.runModel.get(
  #  input, control, init, param)
  #output$ui_main = server.routes.runModel[['ui_main']]

  output$ui_main = server.routes.runModel.get(input)
  
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
  observeEvent(input$reset_main, {
    plot.and.cache = generate.plot.and.table(input, cache)
    
    # This is not needed for diskCache
    # cache = plot.and.cache$cache

    # Update the plot
    output$mainPlot = renderPlotly(plot.and.cache$plot)
    
    # Update the table
    pretty.table = make.pretty.change.data.frame(
      plot.and.cache$change.df, data.type.names=DATA.TYPES)
    output$mainTable = renderDataTable(pretty.table)
    output$mainTable_message = NULL
  })
  

##------------------------------------##  
##-- INTERVENTION SELECTOR HANDLERS --##
##------------------------------------##
  
  observeEvent(input$intervention1, {
      if (input$intervention1=='none')
          output$intervention1_description = NULL
      else
      {
          int = intervention.from.short.name(input$intervention1)
          description = get.intervention.html.description(int)
          output$intervention1_description = renderText(description)
      }
  })
  
  observeEvent(input$intervention2, {
      if (input$intervention2=='none')
          output$intervention2_description = NULL
      else
      {
          int = intervention.from.short.name(input$intervention2)
          description = get.intervention.html.description(int)
          output$intervention2_description = renderText(description)
      }
  })
  
  
##-- LOCATION HANDLER --##
  observeEvent(input$geographic_location, {
    # TODO: temp put here for now and then put on plot:
    session$sendCustomMessage(
      type='resetInputValue', 
      message="show_download")
    
    output$mainPlot = renderPlotly(make.plotly.message(BLANK.MESSAGE))
      message.df = data.frame(BLANK.MESSAGE)
      names(message.df) = NULL
      output$mainTable = renderDataTable(message.df)#matrix(BLANK.MESSAGE,nrow=1,ncol=1))
      output$mainTable_message = renderText(BLANK.MESSAGE)
  })
  
  # TODO: https://shiny.rstudio.com/reference/shiny/0.14/downloadHandler.html
  output$downloadDataLink <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      browser()
      write.csv(pretty.table, file)
    })
}
