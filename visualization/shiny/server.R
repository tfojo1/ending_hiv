'EndingHIV RShiny Server process'

source("R/plot_shiny_interface.R")

suppressPackageStartupMessages(library(EpiModel))  # param.dcm, init.dcm

# Code ####
# TODO @jef: Still in the middle of understanding this function. It's organized 
# a little messily. Really I'd like to see 3 different files / distinct areas 
# for each of the 3 pages/routes. But it looks like all of it is here, with also
# what appear to be some intermediate calculations blotched in between. I dont' 
# know which module/page/routes need which objects in this function, so I will 
# not split it up at the moment. - jef 2020/08/01
server <- function(input, output, session) {

  # TODO: @jef: does this section 'defaults' apply  to all pages, or is this 
  # all for the 'Parameters' page
  # defaults ----------------------------------------------------------- ####
  param <- reactive({
    # TODO @jef: put what I expect here; rather, get these from Todd's plot interface constant.
    beta_student_to_student <- input$R0_student_to_student / input$infectious
    beta_on_to_on <- input$R0_on_to_on / input$infectious
    beta_saf <- input$R0_saf / input$infectious
    N <- input$N_on + input$N_off + input$N_saf

    # https://www.rdocumentation.org/packages/EpiModel/versions/1.2.8/topics/param.dcm
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
    
    # https://www.rdocumentation.org/packages/EpiModel/versions/1.2.8/topics/init.dcm
    init.dcm(  # EpiModel::init.dcm
      # S_on must be input that updates with E I R N
      S_on = input$N_on - (input$E_on + input$I_on + input$R_on), # number initially susceptible
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

  # TODO @jef: Is this the 'Run Model' tab?
  # Page: Run Model (#page-run-model)
  res_main <- reactiveVal()
  output$ui_main <- renderUI({
    reset <- input$reset_main
    res_main(runif(1))
    list(
      fluidRow(
        column(
          width = 12,
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
      ),
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL, title = name2lab("model_summary", all_labs),
            status = "primary", solidHeader = TRUE,

            tableOutput("mainTable")
          ),
        ),
        column(
          width = 6,
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
          )
        )
      )
    )

  })

  param_base <- reactive({
    params <- param()
    ids <- keep(names(input), ~ grepl("basePar_", .x))
    names <- map_chr(ids, ~ strsplit(.x, "basePar_")[[1]][2])
    params[names] <- map(ids, ~ input[[.x]])

    params$beta_student_to_student <- params$R0_student_to_student / params$infectious
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

  output$mainPlot <- renderPlotly({
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

  output$mainDL <- downloadHandler(
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

  output$mainTable <- function() {
    req(df_summ())

    df_summ() %>%
      kable_sum()
  }
  
  # Page: Docs (#page-docs)
  output$introductionText <- renderUI({includeMarkdown(
    "introductionText.Rmd")})

  observe({
    res_main()
    update_init_vals_pattern("baseIni_", input, session)
    update_init_vals_pattern("basePar_", input, session)
    updateSliderInput(session, "baseCon_nsteps", value = input$nsteps)
  })
  
  # Output 2 ####
  output$rawParamText <- renderUI({
    includeMarkdown("rawParamText.Rmd")
  })
}
