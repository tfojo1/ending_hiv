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
    # Unused
    'sidebarItemExpanded'='',
    'sidebarCollapsed'=FALSE,
    'side_menu'='main',    
    'plotly_afterplot-A'='"mainPlot"',
    
    # Page: run model
    # Runmodel 1/6: Projections
    'toggle_main'='Figure',
    
    # Runmodel 2/6: Location
    'geographic_location'=invert.keyVals(
      get.location.options(version))[1],
    
    # Runmodel 3/6: Potential Interventions
    'no_intervention_checkbox'=TRUE,
    
    'preset_tpop_1'='none',
    'intervention_1_selector'='prerun',
    
    
    # Runmodel 4/6: Epidemiological Indicators
    'epidemiological-indicators'=c('incidence', 'new'),
    
    # Runmodel 5/6: Demographic Subgroups
    'demog.selectAll'=FALSE,
    # 'sex'=c('male', 'female'),
    # 'racial-groups'=c('black', 'hispanic', 'other'),
    # 'age-groups'=c('age1', 'age2', 'age3', 'age4', 'age5'),
    # 'risk-groups'=c('msm', 'idu', 'msm_idu', 'heterosexual'),
    'sex'=names(DIMENSION.VALUES[['sex']][['choices']]),
    'racial-groups'=names(DIMENSION.VALUES[['race']][['choices']]),
    'age-groups'=names(DIMENSION.VALUES[['age']][['choices']]),
    'risk-groups'=names(DIMENSION.VALUES[['risk']][['choices']]),    
    'split'='',
    'facet'='',
    'color_by_split_1'=FALSE,    
    
    # Runmodel 6/6: Figure Options
    'plot_format'='individual.simulations',
    'interval_coverage'=95,
    'label_change'=TRUE,
    'change_years'=c(2020, 2030),
    'color_by'='Intervention',
    
    # Page: custom interventions
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

  # Page definitions ####  
  output$ui_main = server.routes.runModel.get(input, session, state)
  output$introductionText = server.routes.docs
  output[['design-interventions']] = 
#    renderUI({includeHTML(
#      'tempCustomHolder.html'
#    )}) #temporary placeholder
    renderUI({includeMarkdown('tempCustomHolder.Rmd')})
#    server.routes.designInterventions.get(input, session, config, state)
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
      shinyjs::enable('createPresetId1')
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
  
  ##-- LOCATION HANDLER --##
  observeEvent(input$geographic_location, {
    output$mainPlot = renderPlotly(make.plotly.message(BLANK.MESSAGE))
    message.df = data.frame(BLANK.MESSAGE)
    names(message.df) = NULL
    # matrix(BLANK.MESSAGE,nrow=1,ncol=1))
    output$mainTable = renderDataTable(message.df)  
    output$mainTable_message = renderText(BLANK.MESSAGE)
  })
  
  # Select All Subgroups: RunModel: selections #
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
  
  # Select All Subgroups: RunModel: enable/disable #
  observeEvent(input$demog.selectAll, {
      checked = input$demog.selectAll
      dim.value.options = get.dimension.value.options()
      subgroup.checkbox.ids = unname(
        sapply(dim.value.options, function(elem){elem$name}))
      
      if (checked) {
        for (i in 1:length(dim.value.options)) {
            id = subgroup.checkbox.ids[i]
            shinyjs::disable(id)
           # updateCheckboxGroupInput(session, inputId=id,
           #                         selected=dim.value.options[[i]]$choices)
        }
      } else
        for (id in subgroup.checkbox.ids)
            shinyjs::enable(id)
  })
  
  # Select All Subgroups: Custom interventions #
  # get dims
  customInts.namesAndChoices = map(
    get.dimension.value.options(
      version=version,
      location=input[['geographic_location']],
      msm_idu_mode=TRUE), 
    function(dim) {
      list(
        'choices'=names(dim[['choices']]),
        'name'=dim[['name']],
        'shortName'=dim[['shortName']] )
    })
  
  customInts.checkboxIds = c()
  customInts.switchIds = c()
  customInts.dimNames = c()
  for (i in 1:5) {
    for (dim in customInts.namesAndChoices) {
      customInts.switchIds <- c(
        customInts.switchIds, 
        paste0(dim[['name']], '_switch', i))
      customInts.checkboxIds <- c(
        customInts.checkboxIds, 
        paste0(dim[['name']], i))
      customInts.dimNames <- c(
        customInts.dimNames, 
        dim[['shortName']])
    }
  }
  
  observe({
    lapply(1:length(customInts.switchIds), function(i) {
      shortName = customInts.dimNames[i]
      switchId = customInts.switchIds[i]
      checkboxGroupId = customInts.checkboxIds[i]
        
      observeEvent(input[[switchId]], {
          checked = input[[switchId]]
          # Update checkboxes
          if (checked)
            updateCheckboxGroupInput(
              session, 
              inputId=checkboxGroupId, 
              selected=customInts.namesAndChoices[[shortName]][['choices']])
          # Enable / disable
          if (checked)
            shinyjs::disable(checkboxGroupId)
          else
            shinyjs::enable(checkboxGroupId)
        })
    })
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
  
  observeEvent(input[['createPresetId1']], {
    handleCreatePreset(input)
  })
  observeEvent(input[['createPresetId2']], {
    handleCreatePreset(input)
  })
  
  observeEvent(input[['feedback_submit']], {
    name = input[['feedback_name']]
    email = input[['feedback_email']]
    contents = input[['feedback_contents']]
    db.write.contactForm(
      name=name, email=email, message=contents)
    showMessageModal('Your message has been received.')
  })
  

  # for now
  output$custom_int_msg_1 = renderText(NO.CUSTOM.INTERVENTIONS.MESSAGE)
  
}
