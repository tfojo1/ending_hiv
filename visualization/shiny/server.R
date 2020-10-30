
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
  
  # Print an initial message - useful for debugging on shinyapps.io servers
  print(paste0("Launching server() function - ", Sys.time()))

  # Make our session cache the cache available to all sessions
  cache = CACHE

  # Page: RunModel - Def ####  
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
  
  ##-- URL query params --##
  # to-do: @JEF/TF: this has been set up in the UI reactively,
  # but this imperative style is also possible.
  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if (!is.null(query[['name']])) {
  #     updateTextInput(session, "name", value = query[['name']])
  # })
  
  # Demographic dimensions: select all
  # to-do: @Todd/@TF: I left everything unselected by default. But when 
  # selected and deselected again, the app doesn't maintain
  # memory of what the values were before, and does not restore them.
  observeEvent(input$demog.selectAll, {
    if (input$demog.selectAll == TRUE) {
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
    # Download button (try 1/2)
    # TODO: @Joe/@JEF: temp put here for now and then put on plot:
    session$sendCustomMessage(
      type='resetInputValue', 
      message="show_download")
    
    output$mainPlot = renderPlotly(make.plotly.message(BLANK.MESSAGE))
      message.df = data.frame(BLANK.MESSAGE)
      names(message.df) = NULL
      output$mainTable = renderDataTable(message.df)  # matrix(BLANK.MESSAGE,nrow=1,ncol=1))
      
      output$mainTable_message = renderText(BLANK.MESSAGE)
  })
  
  # Download button (try 2/2) ###
  # TODO: @Joe/@JEF: Not yet working
  # can download: table 
  # can download: plot
  # (can do: 1 button that can do both, or 2 diff buttons)
  # todd knows how to write the files. how to write the table file, 
  # temp directory, and delete it afterwards
  # https://shiny.rstudio.com/reference/shiny/0.14/downloadHandler.html
  # https://shiny.rstudio.com/articles/communicating-with-js.html
  # https://shiny.rstudio.com/articles/js-send-message.html
  # https://stackoverflow.com/questions/37883046/shiny-dashboard-reset-the-conditional-panel-state-when-we-navigate-across-diffe
  # https://shiny.rstudio.com/reference/shiny/0.11/conditionalPanel.html
  # https://shiny.rstudio.com/reference/shiny/1.4.0/conditionalPanel.html
  output$downloadDataButton <- downloadHandler(
    filename=function() {
      paste("data-", Sys.Date(), ".csv", sep="") },
    content=function(file) {
      write.csv(pretty.table, file) }
  )
  
  #for now
  output$custom_int_msg_1 = renderText(NO.CUSTOM.INTERVENTIONS.MESSAGE)
}
