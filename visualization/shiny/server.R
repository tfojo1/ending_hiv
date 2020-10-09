'EndingHIV RShiny Server process'
library('stringr')

# Source files
source("R/ui.tools.R")
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
    location=input[['geographic-location']],
    intervention.names=intervention.names)
  
  filenames = filenames[!is.sim.cached(filenames, cache=cache)]
  
  # Pre-fetch them simsets
  if (length(filenames)>0)
  {
    if (length(filenames)==1)
      msg = "Loading 1 Simulation File from Remote Server:"
    else
      msg = paste0("Loading ", length(filenames), " Simulations Files from Remote Server:")
    withProgress(
      message=msg, min=0, max=length(filenames), value=0,
      {
        for (i in 1:length(filenames))
        {
          setProgress((i-1),
                      detail = paste("Loading file ", i, " of ", length(filenames)))
          filename = filenames[i]
          cache = update.sims.cache(
            filenames=filename,
            cache=cache)
          
        }
        setProgress(length(filenames), detail='Done')
    })
  }
  
    # Make the plot
    plot.results = plot.simulations(
      cache=cache,
      version=version,
      location=input[['geographic-location']],
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
    p = plot.results$plot

  
  list(
    'cache'=cache,
    'plot'= do.render.plot(p) )
}

# Server
server <- function(input, output, session) {
  cache = CACHE
  # TODO: @jef: does this section 'defaults' apply  to all pages, 
  # or is this  all for the 'Parameters' page
  # defaults ####
  param <- reactive({
    # TODO @jef: put what I expect here; rather, get these from Todd's
    #  plot interface constant.
    beta_student_to_student <- 
      input$R0_student_to_student / input$infectious
    beta_on_to_on <- input$R0_on_to_on / input$infectious
    beta_saf <- input$R0_saf / input$infectious
    N <- input$N_on + input$N_off + input$N_saf

    # TODO: get rid of stuff i don't need
# https://www.rdocumentation.org/
# packages/EpiModel/versions/1.2.8/topics/param.dcm
    param.dcm(  # EpiModel::param.dcm
      latent                  = input$latent,
      infectious              = input$infectious,
      isolation               = input$isolation,

      R0_on_to_on             = input$R0_on_to_on,
      R0_student_to_student   = input$R0_student_to_student,
      R0_saf                  = input$R0_saf,
      beta_student_to_student = beta_student_to_student,
      beta_on_to_on           = beta_on_to_on,
      beta_saf                = beta_saf,

      community               = input$community,
      p_asympt_stu            = input$p_asympt_stu,
      p_asympt_saf            = input$p_asympt_saf,

      p_hosp_stu              = input$p_hosp_stu,
      p_hosp_saf              = input$p_hosp_saf,

      p_death_stu             = input$p_death_stu,
      p_death_saf             = input$p_death_saf,

      contacts                = input$contacts,
      p_contacts_reached      = input$p_contacts_reached,
      sensitivity             = input$sensitivity,
      testing                 = 0,#input$testing,
      screening               = 0,#input$screening,
      p_asympt_stu            = input$p_asympt_stu,
      p_asympt_saf            = input$p_asympt_saf,
      ili                     = input$ili,
      N                       = N
    )
  })

  # TODO @jef: put what I expect here
  init <- reactive({  # rshiny::reactive
    N_off <- input$N_stu - input$N_on # Based on number on campus
    
# https://www.rdocumentation.org/
# packages/EpiModel/versions/1.2.8/topics/init.dcm
    init.dcm(  # EpiModel::init.dcm
      # S_on must be input that updates with E I R N
      # # number initially susceptible
      S_on = input$N_on - (input$E_on + input$I_on + input$R_on), 
      E_on = input$E_on, # number initially incubating
      I_on = input$I_on, # number initially infectious
      P_on = input$P_on, # number initially isolated
      R_on = input$R_on, # initially immune
      Icum_on = 0, # cumulative cases -- for counting incidence
      Pcum_on = 0,
      Q_on = input$Q_on,
      Qcum_on = 0,
      Hcum_on = 0,
      Dcum_on = 0,

      S_off = input$N_off - (input$E_off + input$I_off + input$R_off),
      E_off = input$E_off,
      I_off = input$I_off,
      P_off = input$P_off,
      R_off = input$R_off,
      Icum_off = 0,
      Pcum_off = 0,
      Q_off = input$Q_off,
      Qcum_off = 0,
      Hcum_off = 0,
      Dcum_off = 0,

      S_saf = input$N_saf - (input$E_saf + input$I_saf + input$R_saf),
      E_saf = input$E_saf,
      I_saf = input$I_saf,
      P_saf = input$P_saf,
      R_saf = input$R_saf,
      Icum_saf = 0,
      Pcum_saf = 0,
      Q_saf = input$Q_saf,
      ## Qcum_saf = input$Qcum_saf,
      Hcum_saf = 0,
      Dcum_saf = 0,

      Test = input$Test
    )
  })

  control <- reactive(control.dcm(nsteps = input$nsteps, new.mod = model))

  observeEvent(input$btn_reload, {
    session$reload()
  })

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

  # observe({
  #   res_main()
  #   update_init_vals_pattern("baseIni_", input, session)
  #   update_init_vals_pattern("basePar_", input, session)
  #   updateSliderInput(session, "baseCon_nsteps", value = input$nsteps)
  # })
  
  # output$rawParamText  ####
  output$rawParamText <- renderUI({
    includeMarkdown("rawParamText.Rmd")
  })
  output[['intervention1-description']] <- renderText({
    options = get.intervention.options(
      version=input[['version']],
      location=input[['location']])
    # description = ''
    description = options[[input[['intervention1']]]]$description
    description
  })
  output[['intervention2-description']] <- renderText({ 
    options = get.intervention.options(
      version=input[['version']], 
      location=input[['location']])
    # description = ''
    description = options[[input[['intervention2']]]]$description
    description
  })
  
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
    # %>%
    # output$mainPlot = withSpinner(color="#0dc5c1")
    plot.and.cache = plotAndCache(input, cache)
    cache = plot.and.cache$cache
    output$mainPlot = plot.and.cache$plot
  })
}
