'EndingHIV RShiny Server process'
library('stringr')
#library('DT')
#library('shinyjs')

# Source files
#source("R/ui.tools.R")
source("R/plot_shiny_interface.R")
source("R/server.routes.docs.R")
# source("R/server.routes.designInterventions.R")
# source("R/server.routes.helpAndFeedback.R")
source("R/server.routes.runModel.R")
source("R/plot_manager.R")
source("R/model_code/plot_simulations.R")

suppressPackageStartupMessages(library(EpiModel))  # param.dcm, init.dcm
shinyOptions(cache=diskCache(file.path(dirname(tempdir()), "myapp-cache")))

# Constants / initiliazers
# CACHE = memoryCache(size = 20e6)
CACHE = diskCache(max_size = 20e6)

# Functional components
plotAndCache <- function(input, cache) {
  version = names(get.version.options())[1]
  
  # Pull Intervention Names from Input
  if (input[['no_intervention_checkbox']])
    intervention.names = get.intervention.name(NO.INTERVENTION)
  else
    intervention.names = character()
  intervention.names = c(intervention.names,
                         input[['intervention1']],
                         input[['intervention2']])
  intervention.names = intervention.names[intervention.names != 'none']
  
  
  # Get the filenames to pre-cache
  filenames = get.sim.filenames.to.load(
    version,
    location=input[['geographic_location']],
    intervention.names=intervention.names)
  
  filenames = filenames[!is.sim.cached(filenames, cache=cache)]
  
  # Pre-fetch them simsets
  if (length(filenames)>0)
  {
    if (length(filenames)==1)
      msg = "Fetching 1 Simulation File from Remote Server:"
    else
      msg = paste0("Fetching ", length(filenames), " Simulation Files from Remote Server:")
    withProgress(
      message=msg, min=0, max=1, value=0.2/length(filenames),
      detail=paste("Fetching file 1 of ", length(filenames)),
      {
        for (i in 1:length(filenames))
        {
            if (i>1)
              setProgress((i-1)/length(filenames),
                          detail = paste("Fetching file ", i, " of ", length(filenames)))
          filename = filenames[i]
          cache = update.sims.cache(
            filenames=filename,
            cache=cache)
          
          setProgress(1, detail='Done')
        }
    })
  }
  
    # Make the plot
    plot.results = plot.simulations(
      cache=cache,
      version=version,
      location=input[['geographic_location']],
      intervention.names=intervention.names,
      # years=input[['years']][1]:input[['years']][2],
      years=get.year.options(
        version,
        get.location.options(version)[1]),
      data.types=input[['epidemiological-indicators']],
      facet.by=input[['facet']],
      split.by=input[['split']],
      dimension.subsets=list(  # TODO
        'age'=input[['age-groups']],
        'race'=input[['racial-groups']],
        'sex'=input[['sex']],  # aka gender
        'risk'=input[['risk-groups']]),
      plot.format=input[['aggregation-of-simulations-ran']] )
    
  plot.results$cache = cache
  plot.results
}


BLANK.MESSAGE = "Select intervention(s) and click 'Generate Projections'"
make.plotly.message <- function(message=BLANK.MESSAGE)
{
    plot = plotly_empty()
    plot = add_text(plot, text=message, x=0, y=0)
    #plot = layout(plot, xaxis=list(range = c(-0.5,0.5)))
    #plot = layout(plot, uniformtext=list(minsize=8, mode='hide'))
    
    plot
}

# Server
server <- function(input, output, session) {
  cache = CACHE

  # Page: RunModel - Def ####  
  server.routes.runModel = server.routes.runModel.get(
    input, control, init, param)
  output$ui_main = server.routes.runModel[['ui_main']]
  # Page: RunModel - Components####  
  # output$mainDL = server.routes.runModel[['mainDL']]
  # output$mainTable = server.routes.runModel[['mainTable']]

  # TODO: need this?
  output$mainPlot = server.routes.runModel[['mainPlot']]
  
  # res_main = server.routes.runModel[['res_main']]
  
  # Page: Docs (#page-docs): output$introductionText ####
  output$introductionText = server.routes.docs
  output[['design-interventions']] = server.routes.designInterventions
  output[['help-and-feedback']] = server.routes.helpAndFeedback
  # output$introductionText <- renderUI({includeMarkdown(
  #   "introductionText.Rmd")})


  
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
      print('responding to reset main')
      print(paste0("Selected location = ", input[['geographic_location']]))
    plot.and.cache = plotAndCache(input, cache)
    
    cache = plot.and.cache$cache
    
    output$mainPlot = renderPlotly(plot.and.cache$plot)
    
    pretty.table = make.pretty.change.data.frame(plot.and.cache$change.df, data.type.names=DATA.TYPES)
   # last.two.cols = (dim(pretty.table)[2]-1):dim(pretty.table)[2]
    #pretty.table = datatable(pretty.table) %>% formatStyle(last.two.cols,"white-space"="nowrap")
    output$mainTable = renderDataTable(pretty.table)
    output$mainTable_message = NULL
  })
  
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
  
  observeEvent(input$geographic_location, {
        
      output$mainPlot = renderPlotly(make.plotly.message(BLANK.MESSAGE))
      
      message.df = data.frame(BLANK.MESSAGE)
      names(message.df) = NULL
      output$mainTable = renderDataTable(message.df)#matrix(BLANK.MESSAGE,nrow=1,ncol=1))
      output$mainTable_message = renderText(BLANK.MESSAGE)
  })
  
}
