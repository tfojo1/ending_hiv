# # EndingHiv; Page: Run Model (#page-run-model): output$ui_main
# Settings & Imports ####
library('shiny')
library('shinyWidgets')

source("R/ui.tools.R")  # plot.simulations
source("R/plot_shiny_interface.R")  # plot.simulations

# Options
server.options.boilerplate.on = FALSE

# Variables ####
# Constants
page.width = 12

demog.choiceNames = c(
  'Age',
  'Race',
  'Sex',
  'HIV Risk Factor')
demog.choiceValues = c(
  'age',
  'race',
  'sex',
  'risk')

sex.choiceNames = c(
  'Male',
  'Female')
sex.choiceValues = sex.choiceNames

race.choiceNames = c(
  'Other',
  'Black',
  'Hispanic')
race.choiceValues = race.choiceNames

age.choiceNames = c(
  '13-24 years',
  '25-34 years',
  '35-44 years',
  '45-54 years',
  '55+ years')
age.choiceValues = age.choiceNames

risk.choiceNames = c(
  'MSM',
  'IDU',
  'MSM+IDU',
  'Heterosexual')
risk.choiceValues=risk.choiceNames

intervention.choiceNames = c(
  'No intervention',
  'Young black MSM testing1py 0.8 suppressed 0.25 prep',
  'All MSM IDU testing1py 0.9 suppressed_0.5 prep')
intervention.choiceValues = c(
  'no_intervention',
  'young_black_msm_testing_1py_0.8_suppressed_0.25_prep',
  'all_msm_idu_testing_1py_0.9_suppressed_0.5_prep')

indicator.choiceNames = c(
  'Estimated Prevalence',
  'Reported Diagnoses',
  'HIV Mortality',
  'Viral HIV Suppression',
  'Awareness of HIV Diagnosis',
  'HIV Incidence')
indicator.choiceValues = c(
  'prevalence',
  'new',
  'mortality',
  'suppression',
  'awareness',
  'incidence')

# TODO: Test dynamic loading
# TODO: really return everything
# TODO: hide or fix warnings
version = 1
location = 21205
get.intervention.options.df <- function(version, location)
{
  no.int = list(target.groups='character()',
                testing.frequency=0,
                suppressed.proportion=0.0,
                prep.coverage=0.0,
                intervention.start.year=2021,
                intervention.implemented.year=2022)
  
  int.1 = list(target.groups='Black MSM <35yo',
               testing.frequency=1,
               suppressed.proportion=0.8,
               prep.coverage=0.25,
               intervention.start.year=2021,
               intervention.implemented.year=2022)
  
  int.2 = list(target.groups='All MSM and IDU',
               testing.frequency=1,
               suppressed.proportion=0.9,
               prep.coverage=0.5,
               intervention.start.year=2021,
               intervention.implemented.year=2022)
  
  ints = list('no_intervention'=no.int,
              'young_black_msm_testing_1py_0.8_suppressed_0.25_prep'=int.1,
              'all_msm_idu_testing_1py_0.9_suppressed_0.5_prep'=int.2)
  
  bind_rows(as.data.frame(int.1), as.data.frame(int.2))
}
intervention.options = get.intervention.options.df()

# Calculated variables
page.width.half = round(page.width / 2)

# Implementation: Boilerplate ####
server.routes.runModel.get.boilerplate <- function(
  input,
  control,
  init,
  param
) {
  # Boilerplate.components ####
  res_main <- reactiveVal()
  
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
      mutate(scenario=base_scenario_name)
    
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
        mutate(scenario="Intervention Model")
    }
    
    df
  })
  
  df_both <- reactive(bind_rows(df_base(), df_interv()))
  
  df_summ <- reactive({
    df_cum <- df_both() %>%
      group_by(scenario) %>%
      filter(time == max(time)) %>%
      summarize(
        student_n=S_on + E_on + I_on +  P_on + R_on + Q_on - Dcum_on +
          S_off + E_off + I_off +  P_off + R_off + Q_off - Dcum_off,
        student_cases=Icum_on + Icum_off,
        student_hosps=Hcum_on + Hcum_off,
        student_isos=Pcum_on + Pcum_off,
        student_quas=Qcum_on + Qcum_off,
        student_deaths=Dcum_on + Dcum_off,
        saf_n=S_saf + E_saf + I_saf +  P_saf + R_saf + Q_saf - Dcum_saf,
        saf_cases=Icum_saf,
        saf_hosps=Hcum_saf,
        saf_deaths=Dcum_saf,
        tests=Test
      ) %>%
      mutate(tests_pc=tests / (student_n + saf_n))
    
    df_peak <- df_both() %>%
      group_by(scenario) %>%
      summarize(
        student_cases_peak=max(I_on + I_off, na.rm=TRUE),
        student_isos_peak=max(P_on + P_off, na.rm=TRUE),
        student_isos_days=sum(P_on + P_off, na.rm=TRUE),
        student_quas_peak=max(Q_on + Q_off, na.rm=TRUE),
        student_quas_days=sum(Q_on + Q_off, na.rm=TRUE),
        saf_cases_peak=max(I_saf, na.rm=TRUE))
    
    df_out <- full_join(df_cum, df_peak, by="scenario") %>%
      pivot_longer(-scenario, names_to="measure", values_to="value") %>%
      mutate(value=format_nb(value)) %>%
      pivot_wider(id_cols=measure, names_from=scenario, values_from=value)
    
    df_out <- left_join(
      tibble(measure=names(summ_labs)),
      df_out,
      by="measure"
    ) %>%
      mutate(measure=summ_labs[measure])
    
    df_out
  })
  
  df_clean <- reactive({
    df_both() %>%
      pivot_longer(-c(time, scenario)) %>%
      separate(name, c("measure", "pop"), sep="_", fill="right") %>%
      replace_na(list(pop="all")) %>%
      pivot_wider(names_from=pop, values_from=value) %>%
      mutate(
        stu=on + off ,
        all=if_else(is.na(all), on + off + saf, all)
      ) %>%
      pivot_longer(- c(time, measure, scenario), names_to="pop")
  })
  
  df_plot <- reactive({
    df_clean() %>%
      filter(
        measure %in% input$mainPlot_measures,
        pop %in% input$mainPlot_pop
      ) %>%
      mutate(
        measure=all_labs[measure],
        pop=all_labs[pop]
      )
  })
  
  # Boilerplate.components.mainPlot ####
  mainPlot <- renderPlotly({  # TODO: This is actually boiler plate atm
    p <- ggplot(
      df_plot(), 
      aes(x=time, y=value, col=scenario, label=measure)) +
      geom_line() +
      facet_grid(rows=vars(measure), cols=vars(pop), scales="free_y") +
      theme(panel.border=element_rect(color="black", fill=NA)) +
      xlab("Days") +
      ylab("Value")
    
    ggplotly(p, tooltip=c("y", "label", "colour", "x")) %>%
      layout(legend=list(
        orientation="h",
        y=1.1))
  })
  
  mainDL=downloadHandler(
    filename=function() {
      paste0("simulation_data.tsv")
    },
    content=function(file) {
      df <- df_clean() %>%
        mutate(
          measure=all_labs[measure],
          pop=all_labs[pop]
        )
      
      write.table(df, file=file,
                  quote=FALSE, sep='\t', row.names=FALSE)
    }
  )
  
  mainTable <- function() {
    req(df_summ())
    
    df_summ() %>%
      kable_sum()
  }
  
  res_main <- reactiveVal()
  # Boilerplate.pageDef ####
  ui_main.boilerplate = renderUI({
    reset <- input$reset_main  # could be used later
    res_main(runif(1))  # not sure why r-unifrom here; always returns 1
    
    # List
    list(
      
      # Row 1
      fluidRow(
        # Row 1, Box 1
        column(
          width=page.width,
         box(
           width=NULL, 
           title=name2lab("model_plots", all_labs),
           status="primary", solidHeader=TRUE,
           
           column(
             width=page.width,
             
             plotlyOutput(
               "mainPlot"
                # height=500
               )
           ),
           column(
             width=2,
             checkboxGroupInput(
               "mainPlot_pop", name2lab("Plot_pop", all_labs),
               choiceValues=names(pop_labs),
               choiceNames=unname(pop_labs),
               selected=c("stu", "saf")
             ),
             checkboxGroupInput(
               "mainPlot_measures",
               name2lab("Plot_measures", all_labs),
               choiceValues=names(cp_labs),
               choiceNames=unname(cp_labs),
               selected=c("I", "Icum")
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
          width=page.width.half,
          # Row 2, Box 1
          box(
            width=NULL, title=name2lab("model_summary", all_labs),
            status="primary", solidHeader=TRUE,
            tableOutput("mainTable"))),
        
        column(
          width=page.width.half,
          # Row 2, Box 2
          box(
            width=NULL, title=name2lab("model_scenario", all_labs),
            status="primary", solidHeader=TRUE,
            
            column(
              width=page.width.half,
              sliderInput(
                "mainPlot_test_int",
                name2lab("test_int", all_labs),
                0, 28, 4)
            ),
            column(
              width=page.width.half,
              sliderInput(
                "mainPlot_screen_int",
                name2lab("screen_int", all_labs),
                0, 180, 30
              )
            )
          ),
          
          # Row 2, Box 3
          box(
            width=NULL, title=name2lab("model_opts", all_labs),
            status="primary", solidHeader=TRUE,
            
            column(
              width=page.width.half,
              numericInput("baseIni_N_off", name2lab("N_off", all_labs), 0),
              numericInput("baseIni_N_saf", name2lab("N_saf", all_labs), 0)
            ),
            column(
              width=page.width.half,
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
                width=page.width.half,
                downloadButton("mainDL", name2lab("dl_btn", all_labs))
              ),
              column(
                width=page.width.half,
                actionButton("reset_main", name2lab("reset_button", all_labs))
              )
            )
          ),
          
          # Row 2, Box 4
          box(
            width=NULL, title=name2lab("model_opts_trans", all_labs),
            status="primary", solidHeader=TRUE,
            
            column(
              width=page.width.half,
              numericInput("basePar_R0_student_to_student",
                           name2lab("R0_student_to_student", all_labs), 0),
              numericInput("basePar_R0_saf", name2lab("R0_saf", all_labs), 0)
            ),
            column(
              width=page.width.half,
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
  server.routes.runModel[['ui_main']] = ui_main.boilerplate
  server.routes.runModel[['mainPlot']] = mainPlot
  server.routes.runModel[['mainDL']] = mainDL
  server.routes.runModel[['mainTable']] = mainTable
  server.routes.runModel[['res_main']] = res_main
  server.routes.runModel
}

# Implementation: HIV ####
server.routes.runModel.get.hiv <- function(
  input, control, init, param
) {
  # Other components ####
  # Plot support components
  # used for boilerplate
  df_clean <- reactive({
    
  })
  
  # used for boilerplate
  df_plot <- reactive({
    # df_clean() %>%
    #   filter(
    #     measure %in% input$mainPlot_measures,
    #     pop %in% input$mainPlot_pop
    #   ) %>%
    #   mutate(
    #     measure=all_labs[measure],
    #     pop=all_labs[pop]
    #   )
  })
  
  # Component: Download #mainDL
  mainDL = downloadHandler(
    filename=function() {
      paste0("simulation_data.tsv")
    },
    content=function(file) {
      df <- df_clean() %>%
        mutate(
          measure=all_labs[measure],
          pop=all_labs[pop])
      write.table(
        df, 
        file=file,
        quote=FALSE, 
        sep='\t', 
        row.names=FALSE)
    })
  
  # Component: Main Table #mainTable
  mainTable <- function() {
    req(df_summ())
    
    df_summ() %>%
      kable_sum()
  }
  
  # Component: res_main
  # Not really sure what this one is yet
  res_main = reactiveVal()
  
  # Component: Plot #mainPlot[renderPlotly] ####
  mainPlot = renderPlotly({
    p = ggplot()
    ggplotly(p)
  })
  
  # Component: PageDef #ui_main[renderUI] ####
  ui_main = renderUI({
    # Imperative pre-processing ####
    reset <- input$reset_main
    res_main(runif(1))
    list(  # returns-->list
      # Declartative UI
      # - Resources:
      # https://shiny.rstudio.com/reference/shiny/0.12.2/sliderInput.html
      # https://shiny.rstudio.com/articles/sliders.html
      
      # Button & Plot ####
      # #plot #button
      fluidRow(
        column(
          width=page.width,
          box(
            width=NULL, 
            title="Output",
            status="primary", 
            solidHeader=TRUE,
            
            # #button
            fluidRow(
              column(
                width=page.width.half,
                actionButton(
                  "reset_main", 
                  "Run"))),
            # #plot
            fluidRow(
              column(
                width=page.width,
                plotlyOutput(
                  "mainPlot",
                  # height="0%",
                  width="0%"
                  # height=500
                  )))
      ))), 
      
      # #options
      # Spatiotemporal dimensions ####
      # to-do: expand/collapse feature
      fluidRow(
        column(
          width=page.width,
          box(
            width=NULL, title="Spatiotemporal dimensions",
            status="primary", solidHeader=TRUE,
            
            column(
              width=page.width,
              selectInput(
                inputId="geographic-location", 
                label="Geographic location",
                choices=c(
                  'Baltimore-Columbia-Towson, MD'='12580', 
                  'Miami-Fort Lauderdale-Pompano Beach, FL'='33100'), 
                selected='Baltimore-Columbia-Towson, MD', 
                multiple=FALSE,
                selectize=TRUE, 
                width=NULL, 
                size=NULL) ),
            
            column(
              width=page.width,
              sliderInput(
                  "years", 
                  "Year range",
                min=1970, 
                max=2030,
                value=c(1970, 2030)) )
            
          ))),
      
      # Demographic dimensions ####
      # to-do: expand/collapse feature
      fluidRow(
        column(
          width=page.width,
          box(
            width=NULL, title="Demographic dimensions",
            status="primary", solidHeader=TRUE,
            
            column(
              width=page.width.half,
              checkboxGroupInput(
                inputId='age', 
                label='Age', 
                choiceNames=age.choiceNames,
                choiceValues=age.choiceValues,
                selected=age.choiceValues ),
              
              checkboxGroupInput(
                inputId='race', 
                label='Race', 
                choiceNames=race.choiceNames,
                choiceValues=race.choiceValues,
                selected=race.choiceNames )
            ),  # </column>
            
            column(
              width=page.width.half,  
              
              checkboxGroupInput(
                inputId='sex', 
                label='Sex', 
                choiceNames=sex.choiceNames,
                choiceValues=sex.choiceValues,
                selected=sex.choiceNames ),
              
              checkboxGroupInput(
                inputId='hiv-risk-factor', 
                label='HIV Risk Factor', 
                choiceNames=risk.choiceNames,
                choiceValues=risk.choiceValues,
                selected=risk.choiceNames )
            )  # </column>
              
      ))),
      
      
      # Epidemiological dimensions ####
      # to-do: expand/collapse feature
      fluidRow(
        column(
          width=page.width,
          box(
            width=NULL, title="Epidemiological dimensions",
            status="primary", solidHeader=TRUE,
            
            checkboxGroupInput(
              inputId='public-health-interventions', 
              label='Public health interventions', 
              choiceNames=intervention.choiceNames,
              choiceValues=intervention.choiceValues,
              selected=intervention.choiceValues ),
            
            checkboxGroupInput(
              inputId='epidemiological-indicators', 
              label='Epidemiological indicators', 
              choiceNames=indicator.choiceNames,
              choiceValues=indicator.choiceValues,
              selected=indicator.choiceValues )
            
      ))),
      # Aggregation options ####
      # to-do: expand/collapse feature
      fluidRow(
        column(
          width=page.width,
          box(
            width=NULL, title="Aggregation options",
            status="primary", solidHeader=TRUE,
            
            column(
              width=page.width,
              selectInput(
                inputId='aggregation-of-simulations-ran', 
                label='Aggregation of simulations ran', 
                choices=c(
                  'Mean and Prediction Interval'='mean.and.interval',
                  'Median and Prediction Interval'='median.and.interval',
                  'Individual Simulations'='individual.simulations'),
                selected=NULL, 
                multiple=FALSE,
                selectize=TRUE, 
                width=NULL, 
                size=NULL) ),
            
            column(
              width=page.width.half,
              checkboxGroupInput(
                inputId='facet', 
                label='Multi-panel dis-aggregation', 
                choiceNames=demog.choiceNames,
                choiceValues=demog.choiceValues,
                selected=demog.choiceValues )),
            
            column(
              width=page.width.half,
              checkboxGroupInput(
                inputId='split', 
                label='Multi-line dis-aggregation', 
                choiceNames=demog.choiceNames,
                choiceValues=demog.choiceValues,
                selected=demog.choiceValues )),
    
      # [Testing dynamic loading] ####
      
      fluidRow(
        column(
          width=page.width,
          box(
            width=NULL, title="[Testing dynamic loading]",
            status="primary", solidHeader=TRUE,
            column(
              width=page.width,
              intervention.options %>%
                selectInput(
                  inputId='xxx', 
                  # label='xxx', 
                  label=intervention.options$target.groups,
                  choices=c('xxx')
      ))))) 
    ))))  # </list>  #returns
  })
  
  # Export ####
  server.routes.runModel = list(
    'ui_main'=ui_main,
    'mainPlot'=mainPlot,
    'mainDL'=mainDL,
    'mainTable'=mainTable,
    'res_main'=res_main)
  
  # TODO: @Todd: This method looks like it will work. I just need to 
  # full utilize it for all of the code. This is just an example: 
  server.routes.runModel[['ui_main']] = renderUI({
    iterable = get.intervention.options(version='1.0', location='12580')
    map(
      iterable, ~ textInput(
        inputId=.x$name[1], 
        label=.x$name[1], 
        value=.x$name[1]) ) 
  })
  
  server.routes.runModel
}

# Export ####
server.routes.runModel.get = server.routes.runModel.get.hiv
if (server.options.boilerplate.on == T)
  server.routes.runModel.get = server.routes.runModel.get.boilerplate

# Scratch ####
# library(dataRetrieval)
# library(tidyverse)
# library(lubridate)
# library(shiny)
# library(shinyjs)
# library(plotly)
# library(here)
# interface = paste(
#   here(),"/visualization/shiny/R/plot_shiny_interface.R", sep='')
# source(interface)
# df = data.frame(a=1:1000, b=1:1000); View(df)
# xxx = df %>% { df * df }
# 
# # inputId='aggregation-of-simulations-ran', 
# # label='Aggregation of simulations ran', 
# # choices=c(
# #   'Mean and Prediction Interval'='mean.and.interval',
# #   'Median and Prediction Interval'='median.and.interval',
# #   'Individual Simulations'='individual.simulations'
# 
# #
# xxx = data %>% { data$target.groups }
