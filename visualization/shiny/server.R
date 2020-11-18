# Library calls & Source files ####
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

# Cache ####
##----------------------##
##-- SET UP THE CACHE --##
##----------------------##
shinyOptions(cache=diskCache(file.path(dirname(tempdir()), "myapp-cache")))
# Constants / initiliazers
# TODO: @jef/@tf: Add a 2nd diskCache that is dedicated to the necessary
# datasets we want to lazy load on app start for all sessions. For every,
# city, pulls those 2 files from the diskCache.
# CACHE = memoryCache(size = 20e6)
CACHE = diskCache(max_size = 20e6)

# Main ####
##------------------------------##
##-- THE MAIN SERVER FUNCTION --##
##------------------------------##
server <- function(input, output, session) {
  config.contents <- list(
    'customInterventions.groups.max'=5)
  # to-do: turn this into a function:
  # to-do: dynamically create:
  state.contents <- list(
    'customIntervention_box_switch1'=TRUE,
    'customIntervention_box_switch2'=TRUE,
    'customIntervention_box_switch3'=TRUE,
    'customIntervention_box_switch4'=TRUE,
    'customIntervention_box_switch5'=TRUE
  )
  ci.defaults = customInterventions.demog.defaults()
  for (i in config.contents[['customInterventions.groups.max']])
    for (dim in names(ci.defaults))
      state.contents[[paste0(dim, i)]] = ci.defaults[[dim]]
  
  state <- reactiveVal(state.contents)
  config <- reactiveVal(config.contents)
  data.table <- reactiveVal()
  data.plot <- reactiveVal()
  plot.and.cache = NULL
  
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
  output[['design-interventions']] = 
    server.routes.designInterventions.get(input, session, config, state)
  output[['help-and-feedback']] = 
    server.routes.helpAndFeedback.get(input)

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
      plot.and.cache <<- generate.plot.and.table(input, cache)
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
      
      shinyjs::enable('downloadButton.table')
      shinyjs::enable('downloadButton.plot')
  }
  
  observeEvent(input$run_custom_interventions, {
    # TODO: @tf
  })
  
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
  # - Visualize projections
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
  
  # Demographic dimensions: select all
  # - Custom interventions
  # TODO: How to implement?
  # a. static: 5 blocks of these, w/ id suffixes 1-5
  # b. for loop? for (i in 1:5) input[[paste0('custom...', i)]]
  # c. reactiveVal? of some sort w/ all the logic inside the UI? if poss?
  
  # observeEvent(input$customInterventions.demog.selectAll, {
  #   if (input$customInterventions.demog.selectAll == 'TRUE') {
  
  # to-do: Haven't updated this w/ correct id's, etc yet:
  #     dims.namesAndChoices = map(
  #       get.dimension.value.options(
  #         version=version,
  #         location=input[['geographic_location']]), 
  #       function(dim) {
  #         list(
  #           'choices'=names(dim[['choices']]),
  #           'name'=dim[['name']] )
  #       })
  #     for (dim in dims.namesAndChoices) {
  #       updateCheckboxGroupInput(
  #         session, 
  #         inputId=dim[['name']], 
  #         selected=dim[['choices']])
  #     }
  #   }
  # })
  
  ##-- LOCATION HANDLER --##
  observeEvent(input$geographic_location, {
    output$mainPlot = renderPlotly(make.plotly.message(BLANK.MESSAGE))
    message.df = data.frame(BLANK.MESSAGE)
    names(message.df) = NULL
    # matrix(BLANK.MESSAGE,nrow=1,ncol=1))
    output$mainTable = renderDataTable(message.df)  
    output$mainTable_message = renderText(BLANK.MESSAGE)
  })
  
  # Select All Subgroups #
  observeEvent(input$demog.selectAll, {
      
      checked = input$demog.selectAll
      
      dim.value.options = get.dimension.value.options()
      subgroup.checkbox.ids = unname(sapply(dim.value.options, function(elem){elem$name}))
      
      if (checked)
      {
          for (i in 1:length(dim.value.options))
          {
              id = subgroup.checkbox.ids[i]
              shinyjs::disable(id)
        #      updateCheckboxGroupInput(session, inputId=id,
         #                              selected=dim.value.options[[i]]$choices)
          }
      }
      else
      {
          for (id in subgroup.checkbox.ids)
              shinyjs::enable(id)
      }
  })
  
  #This does not seem to be working - take it out?
  observeEvent(input$plot_format, {
      updateKnobInput(session, 
                      inputId='interval_coverage',
                      options = list(readOnly = input$plot_format=='individual.simulations'))
  })
  
  # Download buttons ##
  output$downloadButton.table <- downloadHandler(
    filename=function() {get.default.download.filename(input, ext='csv')},
    content=function(filepath) {
      write.csv(plot.and.cache$change.df, filepath) 
      } )
  
  observeEvent(input$downloadButton.plot, {
      width = 1000 #we can fill in a user-specified width in pixels later
      height = 650 #ditto
      shinyjs::runjs(paste0("plot=document.getElementById('mainPlot');
                                    Plotly.downloadImage(plot, {format: 'png', width: ", width, ", height: ", height, ", 
                            filename: '", get.default.download.filename(input),"'})"))
   })
  
  observeEvent(input[['n-custom-interventions-btn']], {
    state.temp = state()
    state.temp[['n-custom-interventions']] = 
      input[['n-custom-interventions']]
    state(state.temp)
  })

  observeEvent(input[['createPresetId']], {
    queryStr = presets.urlQueryParamString.create(input)
    presetId = db.write.queryString(queryStr)
    msg = paste0('Preset ID created: ', as.character(presetId))
    output[['createPresetId_msg']] = renderText(msg)
  })
  
  observeEvent(input[['feedback_submit']], {
    name = input[['feedback_name']]
    email = input[['feedback_email']]
    contents = input[['feedback_contents']]
    # TODO: @Joe: send email (currently in server.utils)
  })
  
  # @tf/todd: didnt work: 
  for (i in 1:5) {
    key = paste0('intervention_save', i)
    observeEvent(input[[key]], {
      browser()
    })
  }
  # TODO: @Joe
  observeEvent(input[['intervention_save1']], {
      
  })
  observeEvent(input[['intervention_save2']], {
    
  })
  observeEvent(input[['intervention_save3']], {
    
  })
  observeEvent(input[['intervention_save4']], {
    
  })
  observeEvent(input[['intervention_save5']], {
    
  })
  
  # TODO: @Todd/Joe: 
  # Need 4x5=20 blocks for: dim[['name']], '_switch', i)
  observeEvent(input[['age_switch1']], {
    
  })
  #... and so on
  
  # for now
  output$custom_int_msg_1 = renderText(NO.CUSTOM.INTERVENTIONS.MESSAGE)
  
}
