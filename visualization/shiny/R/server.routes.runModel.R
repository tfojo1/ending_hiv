source("R/ui.tools.R")  # plot.simulations
source("R/plot_shiny_interface.R")  # plot.simulations

# Page: Run Model (#page-run-model): output$ui_main
# boilerplate = TRUE
boilerplate = FALSE
server.routes.runModel = list()

server.routes.runModel.get <- function(
  input,
  control,
  init,
  param
) {
  # Set-up ####
  res_main <- reactiveVal()
  
  # Components ####
  param_base <- reactive({
    params <- param()
    ids <- keep(names(input), ~ grepl("basePar_", .x))
    names <- map_chr(ids, ~ strsplit(.x, "basePar_")[[1]][2])
    params[names] <- map(ids, ~ input[[.x]])
    
    params$beta_student_to_student <- 
      params$R0_student_to_student / params$infectious
    params$beta_on_to_on <- params$R0_on_to_on / params$infectious
    params$beta_saf <- params$R0_saf / params$infectious
    params$N <- input$baseIni_N_on + input$baseIni_N_off + input$baseIni_N_saf
    
    params
  })
  
  init_base <- reactive({
    inits <- init()
    ids <- keep(names(input), ~ grepl("baseIni_", .x))
    names <- map_chr(ids, ~ strsplit(.x, "baseIni_")[[1]][2])
    inits[names] <- map(ids, ~ input[[.x]])
    
    inits$S_on <- inits$N_on - (inits$E_on + inits$I_on + inits$R_on)
    inits$S_off <- inits$N_off - (inits$E_off + inits$I_off + inits$R_off)
    inits$S_saf <- inits$N_saf - (inits$E_saf + inits$I_saf + inits$R_saf)
    
    inits[c("N_on", "N_off", "N_saf")] <- NULL
    inits
  })
  
  control_base <- reactive({
    controls <- control()
    controls$nsteps <- input$baseCon_nsteps
    
    controls
  })
  
  # TODO: Intermediate stuff for 'base'?
  df_base <- reactive({
    df <- dcm(param_base(), init_base(), control_base()) %>%
      as_tibble() %>%
      mutate(scenario = base_scenario_name)
    
    df
  })
  
  df_interv <- reactive({
    if (input$mainPlot_test_int == 0 && input$mainPlot_screen_int == 0) {
      df <- tibble()
    } else {
      param <- param()
      param$testing <- interval2rate(input$mainPlot_test_int)
      param$screening <- interval2rate(input$mainPlot_screen_int)
      
      df <- dcm(param, init_base(), control_base()) %>%
        as_tibble() %>%
        mutate(scenario = "Intervention Model")
    }
    
    df
  })
  
  df_both <- reactive(bind_rows(df_base(), df_interv()))
  
  df_summ <- reactive({
    df_cum <- df_both() %>%
      group_by(scenario) %>%
      filter(time == max(time)) %>%
      summarize(
        student_n = S_on + E_on + I_on +  P_on + R_on + Q_on - Dcum_on +
          S_off + E_off + I_off +  P_off + R_off + Q_off - Dcum_off,
        student_cases = Icum_on + Icum_off,
        student_hosps = Hcum_on + Hcum_off,
        student_isos = Pcum_on + Pcum_off,
        student_quas = Qcum_on + Qcum_off,
        student_deaths = Dcum_on + Dcum_off,
        saf_n = S_saf + E_saf + I_saf +  P_saf + R_saf + Q_saf - Dcum_saf,
        saf_cases = Icum_saf,
        saf_hosps = Hcum_saf,
        saf_deaths = Dcum_saf,
        tests = Test
      ) %>%
      mutate(tests_pc = tests / (student_n + saf_n))
    
    df_peak <- df_both() %>%
      group_by(scenario) %>%
      summarize(
        student_cases_peak = max(I_on + I_off, na.rm = TRUE),
        student_isos_peak = max(P_on + P_off, na.rm = TRUE),
        student_isos_days = sum(P_on + P_off, na.rm = TRUE),
        student_quas_peak = max(Q_on + Q_off, na.rm = TRUE),
        student_quas_days = sum(Q_on + Q_off, na.rm = TRUE),
        saf_cases_peak = max(I_saf, na.rm = TRUE),
      )
    
    df_out <- full_join(df_cum, df_peak, by = "scenario") %>%
      pivot_longer(-scenario, names_to = "measure", values_to = "value") %>%
      mutate(value = format_nb(value)) %>%
      pivot_wider(id_cols = measure, names_from = scenario, values_from = value)
    
    df_out <- left_join(
      tibble(measure = names(summ_labs)),
      df_out,
      by = "measure"
    ) %>%
      mutate(measure = summ_labs[measure])
    
    df_out
  })
  
  df_clean <- reactive({
    df_both() %>%
      pivot_longer(-c(time, scenario)) %>%
      separate(name, c("measure", "pop"), sep = "_", fill = "right") %>%
      replace_na(list(pop = "all")) %>%
      pivot_wider(names_from = pop, values_from = value) %>%
      mutate(
        stu = on + off ,
        all = if_else(is.na(all), on + off + saf, all)
      ) %>%
      pivot_longer(- c(time, measure, scenario), names_to = "pop")
  })
  
  df_plot <- reactive({
    df_clean() %>%
      filter(
        measure %in% input$mainPlot_measures,
        pop %in% input$mainPlot_pop
      ) %>%
      mutate(
        measure = all_labs[measure],
        pop = all_labs[pop]
      )
  })
  
  # Components.mainPlot ####
  if (boilerplate == T) {
    mainPlot <- renderPlotly({
      p <- ggplot(df_plot(), aes(x = time, y = value,
                                 col = scenario, label = measure)) +
        geom_line() +
        facet_grid(rows = vars(measure), cols = vars(pop), scales = "free_y") +
        theme(panel.border = element_rect(color = "black", fill = NA)) +
        xlab("Days") +
        ylab("Value")
      
      
      ggplotly(p, tooltip = c("y", "label", "colour", "x")) %>%
        layout(legend = list(
          orientation = "h",
          y = 1.1
        ))
    })
  }
  if (boilerplate == F) {
    # TODO: figure out how to render mainPlot
    # - Is it supposed to be a ggplotly() obj?
    # - These are  placeholder values; these should be selected from the UI.
    
    # Attempt 1
    # p.version = names(get.version.options())[1]
    # p.location = names(get.location.options(p.version))[1]
    # p.interventions = get.intervention.options(p.version, p.location)
    # p = plot.simulations(
    #   version=p.version,
    #   location=p.location,
    #   intervention.names=names(p.interventions)[1],
    #   years=c(2000:2020),
    #   data.types=get.data.type.options(p.version, p.location),
    #   facet.by=get.facet.by.options(p.version, p.location),
    #   split.by=get.split.by.options(p.version, p.location),
    #   dimension.subsets=get.dimension.value.options(p.version, p.location),
    #   plot.format=get.plot.format.options(p.version, p.location)[1])$plot
    
    # Attempt 2
    version = names(get.version.options())[1]
    location = names(get.location.options(version))[1]
    interventions = get.intervention.options(version, location)
    p = plot.simulations(
      version=version,
      location=location,
      intervention.names = names(interventions)[1],
      years = get.year.options(version, location),
      data.types = get.data.type.options(version, location)[1:2],
      facet.by = names(get.facet.by.options(version, location))[1],
      split.by = names(get.split.by.options(version, location))[2],
      dimension.subsets=get.dimension.value.options(version, location),
      plot.format = names(get.plot.format.options(version, location))[1],
      plot.interval.coverage=0.95,
      summary.statistic=get.summary.statistic.options(version, location)[1],
      summary.statistic.interval.coverage=0.95,
      baseline.color='blue',
      intervention.colors='red',
      plot.interval.alpha=0.2,
      simulation.alpha=0.2,
      simulation.line.size=0.1,
      show.truth=T,
      truth.point.size=5
    )$plot
    
    mainPlot <- renderPlotly({
      ggplotly(
        p
        # tooltip=c("y", "label", "colour", "x")) %>%
        # layout(legend = list(
        #   orientation = "h",
        #   y = 1.1
        # )
      )
    })
  }
  
  mainDL = downloadHandler(
    filename = function() {
      paste0("simulation_data.tsv")
    },
    content = function(file) {
      df <- df_clean() %>%
        mutate(
          measure = all_labs[measure],
          pop = all_labs[pop]
        )
      
      write.table(df, file=file,
                  quote=FALSE, sep='\t', row.names = FALSE)
    }
  )
  
  mainTable <- function() {
    req(df_summ())
    
    df_summ() %>%
      kable_sum()
  }

  
  # Page Def ####
  res_main <- reactiveVal()
  ui_main = renderUI({
    reset <- input$reset_main
    res_main(runif(1))
    
    # List
    list(
      
      # Row 1
      fluidRow(
        # Row 1, Box 1
        column(          width = 12,
          box(
            width = NULL, title = name2lab("model_plots", all_labs),
            status = "primary", solidHeader = TRUE,
            
            column(
              width = 10,
              
              plotlyOutput("mainPlot", height = 500)
            ),
            column(
              width = 2,
              checkboxGroupInput(
                "mainPlot_pop", name2lab("Plot_pop", all_labs),
                choiceValues = names(pop_labs),
                choiceNames = unname(pop_labs),
                selected = c("stu", "saf")
              ),
              checkboxGroupInput(
                "mainPlot_measures",
                name2lab("Plot_measures", all_labs),
                choiceValues = names(cp_labs),
                choiceNames = unname(cp_labs),
                selected = c("I", "Icum")
              )
            )
          )
        )
        
      # # Attempt 1
      # ))})
    
      # Attempt 2
      ),

      # Row 2
      fluidRow(
        column(
          width = 6,

          # Row 2, Box 1
          box(
            width = NULL, title = name2lab("model_summary", all_labs),
            status = "primary", solidHeader = TRUE,

            tableOutput("mainTable")
          ),
        ),

    column(
      width = 6,
      # Row 2, Box 2
      box(
        width = NULL, title = name2lab("model_scenario", all_labs),
        status = "primary", solidHeader = TRUE,

        column(
          width = 6,
          sliderInput(
            "mainPlot_test_int",
            name2lab("test_int", all_labs),
            0, 28, 4
          )
        ),
        column(
          width = 6,
          sliderInput(
            "mainPlot_screen_int",
            name2lab("screen_int", all_labs),
            0, 180, 30
          )
        )
      ),

      # Row 2, Box 3
      box(
        width = NULL, title = name2lab("model_opts", all_labs),
        status = "primary", solidHeader = TRUE,

        column(
          width = 6,
          numericInput("baseIni_N_off", name2lab("N_off", all_labs), 0),
          numericInput("baseIni_N_saf", name2lab("N_saf", all_labs), 0)
        ),
        column(
          width = 6,
          numericInput("baseIni_N_on", name2lab("N_on", all_labs), 0),
          sliderInput(
            "baseCon_nsteps",
            name2lab("nsteps", all_labs),
            0, 365, 180
          )
        ),

        # action buttons
        fluidRow(
          column(
            width = 6,
            downloadButton("mainDL", name2lab("dl_btn", all_labs))
          ),
          column(
            width = 6,
            actionButton("reset_main", name2lab("reset_button", all_labs))
          )
        )
      ),

      # Row 2, Box 4
      box(
        width = NULL, title = name2lab("model_opts_trans", all_labs),
        status = "primary", solidHeader = TRUE,

        column(
          width = 6,
          numericInput("basePar_R0_student_to_student",
                       name2lab("R0_student_to_student", all_labs), 0),
          numericInput("basePar_R0_saf", name2lab("R0_saf", all_labs), 0)
        ),
        column(
          width = 6,
          numericInput("basePar_R0_on_to_on",
                       name2lab("R0_on_to_on", all_labs), 0),
          numericInput("basePar_community",
                       name2lab("community", all_labs), 0)
        )
      ) #
        )
      )
    )
  })
  
  # Export ####
  server.routes.runModel[['ui_main']] = ui_main
  server.routes.runModel[['mainPlot']] = mainPlot
  server.routes.runModel[['mainDL']] = mainDL
  server.routes.runModel[['mainTable']] = mainTable
  server.routes.runModel[['res_main']] = res_main
  server.routes.runModel
}
