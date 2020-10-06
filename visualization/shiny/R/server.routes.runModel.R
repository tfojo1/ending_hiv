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

# Implementation ####
server.routes.runModel.get <- function(
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
          title="Location",
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
                  choices=invert.keyVals(get.location.options(
                    version=version)),
                  selected=invert.keyVals(get.location.options(
                    version=version))[1],
                  multiple=FALSE,
                  selectize=TRUE, 
                  width=NULL, 
                  size=NULL ) ))
          
          # column(
          #   width=page.width,
          #   sliderInput(
          #     inputId="years", 
          #     label="Year range",
          #     min=min(year.ticks),
          #     max=max(year.ticks),
          #     value=c(
          #       min(year.ticks), 
          #       max(year.ticks)) ))
            
    ))),
    # Interventions ####
    'epidemiological-interventions'=fluidRow(
      column(
        width=page.width,
        box(
          width=NULL, 
          title="Interventions",
          status="primary", 
          solidHeader=TRUE,
          
          fluidRow(
            column(
              width=page.width,
              checkboxInput(
                inputId='no_intervention_checkbox', 
                label='Include "No intervention" in results', 
                value=TRUE,  
                width='100%' )),
          ),  # </fluidRow>
          
          fluidRow(
            column(
              width=page.width.half,
              selectInput(
                inputId="intervention1", 
                label="Intervention 1",
                choices=invert.keyVals(
                  get.interventions.simpleList(
                    version=version, 
                    location=input[['geographic-location']])),
                selected=invert.keyVals(get.interventions.simpleList(
                  version=version, input[['geographic-location']]))[1],
                multiple=FALSE,
                selectize=TRUE, 
                width='auto', 
                size=NULL ),
              textOutput(outputId='intervention1-description')
            ),
            column(
              width=page.width.half,
              selectInput(
                inputId="intervention2", 
                label="Intervention 2",
                choices=invert.keyVals(
                  get.interventions.simpleList(
                    version=version, 
                    location=input[['geographic-location']])),
                selected=invert.keyVals(get.interventions.simpleList(
                  version=version, input[['geographic-location']]))[1],
                multiple=FALSE,
                selectize=TRUE, 
                width='auto', 
                size=NULL ),
              textOutput(outputId='intervention2-description')
            ),
          ),  # </fluidRow>
          
        ))),
    # Epidemiological dimensions ####
    'epidemiological-dimensions'=fluidRow(
      column(
        width=page.width,
        box(
          width=NULL, title="Epidemiological dimensions",
          status="primary", solidHeader=TRUE,
          
          checkboxGroupInput(
            inputId='epidemiological-indicators', 
            label='Indicators', 
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
              ~ .x ))[1:2] ),
          
        ))),
    
    
    # Aggregation options ####
    # to-do: expand/collapse feature
    'aggregation-options'=fluidRow(
      column(
        width=page.width,
        box(
          width=NULL, 
          title="Aggregation",
          status="primary", 
          solidHeader=TRUE,
          
          column(
            width=page.width,
            selectInput(
              inputId='aggregation-of-simulations-ran', 
              label='Aggregation of simulations ran', 
              choices=invert.keyVals(get.plot.format.options(
                version=version,
                location=input[['geographic-location']])),
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
              selected=NULL)),
          
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
              selected=NULL))
          
          # column(
          #   width=page.width.half,
          #   checkboxGroupInput(
          #     inputId='split', 
          #     label='Multi-line dis-aggregation', 
          #     choiceNames=demog.choiceNames,
          #     choiceValues=demog.choiceValues,
          #     selected=demog.choiceValues ) )
          
      ))),  # </list>  #returns
    
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
            get.dimension.value.options(
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
      
    )))
    
  )})
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

# Scratch ####
