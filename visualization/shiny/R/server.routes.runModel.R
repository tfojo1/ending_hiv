# # EndingHiv; Page: Run Model (#page-run-model): output$ui_main
# Settings & Imports ####
library('shiny')
library('shinyWidgets')

source("R/ui.tools.R")  # plot.simulations
source("R/plot_shiny_interface.R")  # plot.simulations


# Variables ####
# Constants
version = '1.0'
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

# Calculated variables
page.width.half = round(page.width / 2)
year.ticks = get.year.options(
    version,
    get.location.options(version)[1])

# Implementation: Static ####
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
                  "Run")))
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
                selected=demog.choiceValues ) )
  
    ))))  # </list>  #returns
  })
  
  # Export ####
  server.routes.runModel = list(
    'ui_main'=ui_main,
    'mainPlot'=mainPlot,
    'mainDL'=mainDL,
    'mainTable'=mainTable,
    'res_main'=res_main)
  
  server.routes.runModel
}

# Implementation: Dynamic ####
server.routes.runModel.get.hiv.dynamic <- function(
  input, control, init, param
) {
  # Component: PageDef #ui_main[renderUI]
  ui_main = renderUI({
    list(  # returns-->list
    # Button & Plot ####
    # #plot #button
    'output'=fluidRow(
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
                outputId="mainPlot",
                height="auto",
                width="auto",
                inline=T)))
          
        ))), 
    
    # #options
    # Spatiotemporal dimensions ####
    # to-do: expand/collapse feature
    
    'spatiotemporal-dimensions'=fluidRow(
      column(
        width=page.width,
        box(
          title="Spatiotemporal dimensions",
          status="primary",
          width=NULL, 
          solidHeader=TRUE,
          
          # #button
          fluidRow(
            column(
              width=page.width.half,
                selectInput(
                  inputId="geographic-location", 
                  label="Geographic location",
                  choices=get.location.options(
                    version=version),
                  selected=get.location.options(
                    version=version)[1],
                  multiple=FALSE,
                  selectize=TRUE, 
                  width=NULL, 
                  size=NULL )
          )),
          
          column(
            width=page.width,
            sliderInput(
              inputId="years", 
              label="Year range",
              min=min(year.ticks),
              max=max(year.ticks),
              value=c(
                min(year.ticks), 
                max(year.ticks)) )
            
    )))),
    
    # Demographic dimensions ####
    # to-do: expand/collapse feature
    'demographic-dimensions'=fluidRow(
      column(
        width=page.width,
        box(
          title="Demographic dimensions",
          status="primary",
          width=NULL, 
          solidHeader=TRUE,

          map(
            get.dimensions(
              version=version,
              location=input[['geographic-location']]), 
            function(dim) {
              column(
                width=page.width.half,
              checkboxGroupInput(
                inputId=dim[['name']],
                label=dim[['label']],
                choiceNames=unname(dim[['choices']]),
                choiceValues=names(dim[['choices']]),
                selected=names(dim[['choices']])
              ) )
            })
      
    ))),
    
    # TODO: #now
    # Epidemiological dimensions ####
    # to-do: expand/collapse feature
    'epidemiological-dimensions'=fluidRow(
      column(
        width=page.width,
        box(
          width=NULL, title="Epidemiological dimensions",
          status="primary", solidHeader=TRUE,
          
          checkboxGroupInput(
            inputId='public-health-interventions', 
            label='Public health interventions', 
            choiceNames=unname(map(
              get.intervention.options(
                version=version, 
                location=input[['geographic-location']]),
              ~ .x$label )),
            choiceValues=names(map(
              get.intervention.options(
                version=version, 
                location=input[['geographic-location']]),
              ~ .x$name )),
            selected=unname(map(
              get.intervention.options(
                version=version, 
                location=input[['geographic-location']]),
              ~ .x$name )) ),
          
          checkboxGroupInput(
            inputId='epidemiological-indicators', 
            label='Epidemiological indicators', 
            choiceNames=unname(map(
              get.data.type.options(
                version=version, 
                location=input[['geographic-location']]),
              ~ .x )),
            choiceValues=names(map(
              get.data.type.options(
                version=version, 
                location=input[['geographic-location']]),
              ~ .x )),
            selected=names(map(
              get.data.type.options(
                version=version, 
                location=input[['geographic-location']]),
              ~ .x )) ),
          
    ))),
    # Aggregation options ####
    # to-do: expand/collapse feature
    'aggregation-options'=fluidRow(
      column(
        width=page.width,
        box(
          width=NULL, title="Aggregation options",
          status="primary", solidHeader=TRUE,
          
          # TODO:
          column(
            width=page.width,
            selectInput(
              inputId='aggregation-of-simulations-ran', 
              label='Aggregation of simulations ran', 
              choices=get.plot.format.options(
                version=version,
                location=input[['geographic-location']]),
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
              choiceNames=unname(get.facet.by.options(
                version=version,
                location=input[['geographic-location']])),
              choiceValues=names(get.facet.by.options(
                version=version,
                location=input[['geographic-location']])),
              selected=names(get.facet.by.options(
                version=version,
                location=input[['geographic-location']])) )),
          
          column(
            width=page.width.half,
            checkboxGroupInput(
              inputId='split', 
              label='Multi-line dis-aggregation', 
              choiceNames=unname(get.split.by.options(
                version=version,
                location=input[['geographic-location']])),
              choiceValues=names(get.split.by.options(
                version=version,
                location=input[['geographic-location']])),
              selected=names(get.split.by.options(
                version=version,
                location=input[['geographic-location']])) ))
          
          # column(
          #   width=page.width.half,
          #   checkboxGroupInput(
          #     inputId='split', 
          #     label='Multi-line dis-aggregation', 
          #     choiceNames=demog.choiceNames,
          #     choiceValues=demog.choiceValues,
          #     selected=demog.choiceValues ) )
          
        ))))  # </list>  #returns
  })
  # Export ####
  server.routes.runModel = list(
    'ui_main'=ui_main,
    'mainPlot'=renderUI({})
    # 'mainDL'=mainDL,
    # 'mainTable'=mainTable,
    # 'res_main'=res_main,
    )
  server.routes.runModel
}

# Export ####
# server.routes.runModel.get = server.routes.runModel.get.hiv
server.routes.runModel.get = server.routes.runModel.get.hiv.dynamic

# Scratch ####
