'EndingHIV RShiny Server process'

source("R/ui.tools.R")
source("R/plot_shiny_interface.R")
source("R/server.routes.docs.R")
source("R/server.routes.runModel.R")

suppressPackageStartupMessages(library(EpiModel))  # param.dcm, init.dcm

# Components
rp <- function(input) {
  renderPlotly({
    version = names(get.version.options())[1]
    
    #update the sims cache
    filenames = get.sim.filenames.to.load(version,
                                          location=input[['geographic-location']],
                                          intervention.names=input[['public-health-interventions']])
    CACHE = update.sims.cache(filenames=filenames,
                              cache=CACHE)

    
    p = plot.simulations(
        cache=CACHE,
      version=version,
      location=input[['geographic-location']],
      intervention.names=input[['public-health-interventions']],
      years=input[['years']][1]:input[['years']][2],
      data.types=input[['epidemiological-indicators']],
      facet.by=input[['facet']],
      split.by=input[['split']],
      dimension.subsets=list(  # TODO
        'age'=input[['age-groups']],
        'race'=input[['racial-groups']],
        'sex'=input[['sex']],  # aka gender
        'risk'=input[['risk-groups']]),
      plot.format=input[['aggregation-of-simulations-ran']]
    )$plot
    ggplotly(p)
})}

server <- function(input, output, session) {
  
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
  
  res_main = server.routes.runModel[['res_main']]
  
  # Page: Docs (#page-docs): output$introductionText ####
  output$introductionText = server.routes.docs
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
  
  # Events: Simulate & plot ####
  # Plot: Pass to plot event handler function
  # - Alternative method: ggplotly
  # `# output$mainPlot = renderPlotly({ p = ggplot(); ggplotly(p) })``
  
  # TODO: re-enable once plot works
  rp(input)  # Plots at start
  
  observeEvent(input$reset_main, {
    output$mainPlot = rp(input)
  })
  
  # TODO: check simset
  # filename = 'No_Intervention.Rdata'
  # sims.load(filename)
  # s3load(filename,'endinghiv.sims')
  # browser()
}
