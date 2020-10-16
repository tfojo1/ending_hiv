# # EndingHiv; Page: Run Model (#page-run-model): output$ui_main

##-------------------------------##
##-- LIBRARIES and SOURCE CODE --##
##-------------------------------##

library('shiny')
library('shinycssloaders')
library('shinyWidgets')
library('purrr')

# This sourcing will be done by the parent server.R file
#source("R/plot_shiny_interface.R")  # plot.simulations

##--------------##
##- CONSTANTS --##
##--------------##

#version = '1.0'
page.width = 12
page.width.half = round(page.width / 2)

##----------------------------------------------------------##
##-- SOME HELPERS (that abstract away the 'input' object) --##
##----------------------------------------------------------##

#Here for future-proofing. For now, just one version possible
get.version <- function(input)
{
   '1.0' 
}

get.location <- function(input)
{
    input$geographic_location
}

##-----------------------------------------------------##
##-- THE FUNCTION THAT GENERATES THE UI FOR THE PAGE --##
##-----------------------------------------------------##


#reutrns 
server.routes.runModel.get <- function(input) 
{
  
  # Component: PageDef #ui_main[renderUI]
  ui_main = renderUI({
    
    location.choice = input[['geographic_location']]
    if (is.null(location.choice))
        location.choice = invert.keyVals(get.location.options(version))[1]
    
    
    list(  # returns-->list
    
    #This code sets the position and style for the progress bar when loading simulations
    tags$head(tags$style(".shiny-notification {position: fixed; top: 10% ;left: 25%; color: black;font-size: 20px;font-style: normal; padding-left: 50px; padding-right: 50px")),

    #The panel for 
    'output'=fluidRow(
      column(
        width=page.width,
        box(
          width=NULL, 
          title="Figure",
          collapsible=T,
          collapsed=F,
          status="primary", 
          solidHeader=TRUE,
          
          # #button
          fluidRow(
            column(
              width=page.width.half,
              actionButton(
                "reset_main", 
                "Generate Projections")),
            column(
                width=page.width.half,
                radioGroupButtons(
                    inputId="toggle_main", 
                    selected='Figure',
                    choices=c('Figure','Table')))
            ),
          
          # #plot
          fluidRow(
              tags$head(tags$style("#tbl {white-space: nowrap;}")),
            column(
              width=page.width,
              
              conditionalPanel(
                  condition = "input.toggle_main == 'Figure'",
                  plotlyOutput(outputId="mainPlot",
                      height="auto",
                      width='100%',#"auto",
                      inline=T)  %>% withSpinner(color="#0dc5c1")
                  ),
              
              conditionalPanel(
                  condition = "input.toggle_main == 'Table'",
                  verbatimTextOutput('mainTable_message', placeholder = FALSE),
                  div(style = 'overflow-x: scroll', 
                      dataTableOutput(outputId="mainTable")
                      )
                  )
              
              ))
            )
          
         )), 
    
    # #options
    # Spatiotemporal dimensions ####
    # to-do: expand/collapse feature
    
    'spatiotemporal-dimensions'=fluidRow(
      column(
        width=page.width,
        box(
          title="Location",
          collapsible=T,
          collapsed=F,
          status="primary",
          width=NULL, 
          solidHeader=TRUE,
          
          # #button
          fluidRow(
            column(
              width=page.width,
                selectInput(
                  inputId="geographic_location", 
                  label=NULL,#"Metropolitan Statistical Area (MSA)",
                  choices=invert.keyVals(get.location.options(
                    version=version)),
                  selected=location.choice,#get.location.options(version=version)[1],
                  multiple=FALSE,
                  selectize=TRUE, 
                  width=NULL, 
                  size=NULL ) ))
            
    ))),
    # Interventions ####
    'epidemiological-interventions'=fluidRow(
      column(
        width=page.width,
        box(
          width=NULL, 
          title="Potential Interventions",
          collapsible=T,
          collapsed=F,
          status="primary", 
          solidHeader=TRUE,
          
          fluidRow(
            column(
              width=page.width,
              checkboxInput(
                inputId='no_intervention_checkbox', 
                label='Display "No intervention"', 
                value=TRUE,  
                width='100%' )),
          ),  # </fluidRow>
          
          #div(HTML("<HR>")),
          div(style = "font-size: 1.2em; padding: 0px 0px; margin-bottom:-20px",
              HTML("<b>Intervention 1:</b>")),
          create.intervention.selector.panel(1, input)
  #        box(title='Intervention 1:', solidHeader=T, width=12,
   #           create.intervention.selector.panel(1, input))
          
          ))),  # </fluidRow>
          
    # Epidemiological dimensions ####
    'epidemiological-dimensions'=fluidRow(
      column(
        width=page.width,
        box(
          width=NULL, title="Epidemiological Indicators",
          collapsible=T,
          collapsed=T,
          status="primary", solidHeader=TRUE,
          
          checkboxGroupInput(
            inputId='epidemiological-indicators', 
            label=NULL,#'Indicators', 
            choiceNames=unname(map(
              get.data.type.options(
                version=version, 
                location=input[['geographic_location']]),
              ~ .x )),
            choiceValues=names(map(
              get.data.type.options(
                version=version, 
                location=input[['geographic_location']]),
              ~ .x )),
            selected=names(map(
              get.data.type.options(
                version=version, 
                location=input[['geographic_location']]),
              ~ .x ))[1:2] ),
          
        ))),
    
    
    # Aggregation options ####
    # to-do: expand/collapse feature
    'aggregation-options'=fluidRow(
      column(
        width=page.width,
        box(
          width=NULL, 
          title="Plot Options (how to slice the projections)",
          collapsible=T,
          collapsed=T,
          status="primary", 
          solidHeader=TRUE,
          
          column(
            width=page.width,
            radioGroupButtons(
#            selectInput(
              inputId='aggregation-of-simulations-ran', 
              label='What to Plot', 
              choices=invert.keyVals(get.plot.format.options(
                version=version,
                location=input[['geographic_location']])),
              selected=NULL, 
              #multiple=FALSE,
              #selectize=TRUE, 
              width=NULL, 
              size=NULL) ),
          
          column(
            width=page.width.half,
            checkboxGroupInput(
              inputId='facet', 
              label='Make Separate Panels for Each:', 
              choiceNames=unname(get.facet.by.options(
                version=version,
                location=input[['geographic_location']])),
              choiceValues=names(get.facet.by.options(
                version=version,
                location=input[['geographic_location']])),
              selected=NULL)),
          
          column(
            width=page.width.half,
            checkboxGroupInput(
              inputId='split', 
              label='Within a Panel, Plot Separate Lines for Each:', 
              choiceNames=unname(get.split.by.options(
                version=version,
                location=input[['geographic_location']])),
              choiceValues=names(get.split.by.options(
                version=version,
                location=input[['geographic_location']])),
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
          title="Demographic Subgroups",
          collapsible=T,
          collapsed=T,
          status="primary",
          width=NULL, 
          solidHeader=TRUE,

          map(
            get.dimension.value.options(
              version=version,
              location=input[['geographic_location']]), 
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
  
  ui_main
}

# Scratch ####
