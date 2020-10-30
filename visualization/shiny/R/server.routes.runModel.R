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

tipBox <- function(message) {
  fluidRow(
    column(
      width=page.width, 
      tags$div(
        background='#FFF3CD', 
        class="yellow-box", 
        { message }
  )))
}

##-----------------------------------------------------##
##-- THE FUNCTION THAT GENERATES THE UI FOR THE PAGE --##
##-----------------------------------------------------##

#returns
server.routes.runModel.get <- function(input, session) 
{
  # Component: PageDef #ui_main[renderUI]
  ui_main = renderUI({
    
    
    
    
    
    
    
    
    
    # TODO: add ability to fetch from query param
    # Location.choice
    # to-do: @Todd/TF: want me to change 'location' to say 'locationId',
    # 'locationCode', etc? To be more clear? This will be a lot easier
    # than parsing special characters (especially any tranasformed 
    # ones, e.g. from whitespace) from the URL query string. Also,
    # if we did this we'd probably want it to be case insensitive.
    if ('location' %in% names(
      parseQueryString(session$clientData$url_search))) {
      loc = parseQueryString(
        session$clientData$url_search)[['location']]
      
      # to-do: validate
      # @Todd/TF: Right now if what they type is invalid, it will 
      # thankfully rever to a default selection / first item in the
      # list, rather than erroring out.
      valid = TRUE  # placeholder
      
      # to-do: find out how to get: arrayOfValidLocationIds, and
      # re-activate this code block:
      # 
      # by using this?:
      # invert.keyVals(get.location.options(
      # version=version))
      # 
      # arrayOfValidLocationIds = c(
      #   'nothing', 'valid', 'here')  # placeholder
      # if (!(loc %in% arrayOfValidLocationIds))
      #   valid = FALSE  
      
      # TODO: try 33100
      if (valid == TRUE)
        location.choice = loc
      
      # to-do: Then update URL bar to fix conflict if user changes?
      # to-do: Show warning message if invalid?
    } else
      location.choice = input[['geographic_location']]
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    if (is.null(location.choice))
      location.choice = invert.keyVals(
        get.location.options(version))[1]
    
    list(  # returns-->list
      # Header & styles ####
      #This code sets the position and style for the progress bar when
      # loading simulations
      tags$head(
        tags$style(
          ".shiny-notification {
           position: fixed;
           top: 10%;
           left: 25%;
           color: black;
           font-size: 20px;
           font-style: normal;
           padding-left: 50px;
           padding-right: 50px;
        }
        .yellow-box {
          background: #FFF3CD;
          color: #856405;
          margin-top: 10px;
          margin-bottom: 10px;
          padding-top: 5px;
          padding-bottom: 5px;
          padding-left: 5px;
          padding-right: 5px;
        }
        ")),  # TODO: yellow box: (1) rounded corners, (2) black border
      
      # Info box ####
      
      # Output panel ####
      'output'=fluidRow(
        column(
          width=page.width,

          verticalSpacer(40),
          
          tags$table(
            tags$tr(
              tags$td(style='padding-right: 20px',
                actionButton(
                  style="background: #204C73; color: white; font-size:150%; margin: 0 auto;",
                  #          style='material-flat', size='md', color='primary',
                  "reset_main", 
                  HTML("Generate<BR>Projections"))),
              tags$td(
                
          tipBox(width=12,
                 'To make projections:<ol>
           <li> Select a location from the "Locations" tab </li>
           <li> Select interventions from the "Potential Interventions" tab </li>
           <li> Click "Generate Projections" </li>
           </ol>')
              )
            )
          ),
          
          box(
            width=NULL, 
            title="Projections",
            collapsible=T,
            collapsed=F,
            status="primary", 
            solidHeader=TRUE,
            
            
            
            # TODO: Download button: Not yet working
            fluidRow(
              column(
                width=12,
                conditionalPanel(
                  condition="(input.show_download  !== undefined && input.show_download !== null)",
                  downloadLink(
                    "downloadDataLink",
                    actionButton(
                      "downloadDataButton", 
                      "Download"))) ),
            ),
            
            # plot and table
            fluidRow(
              #   tags$head(tags$style("#tbl {white-space: nowrap;}")),
              #
              #             ),
              column(
                width=page.width,
                
                tabsetPanel(
                  id='toggle_main',
                  selected='Figure',
                  type='tabs',
                  
                  #Figure
                  tabPanel(title='Figure',
                           value='Figure',
                           plotlyOutput(outputId="mainPlot",
                                        height="auto",
                                        width='100%',#"auto",
                                        inline=T)  %>% withSpinner(color="#0dc5c1")
                  ),
                  
                  #Table
                  tabPanel(title='Table',
                           value='Table',
                           verbatimTextOutput(
                             placeholder=FALSE,
                             'mainTable_message'
                           ),
                           div(style = 'overflow-x: scroll', 
                               dataTableOutput(outputId="mainTable")
                           ))
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
                  #get.location.options(version=version)[1],
                  selected=location.choice,
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
            div(style = "font-size: 1.2em; padding: 0px 0px; margin-bottom:0px",
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
            
            fluidRow(
              column(
                width=3,
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
                    ~ .x ))[1:2] )
              ),
              
            
                tags$div(
                  background='#FFF3CD', 
                  class="yellow-box", 
                  { '[placeholder]'}
                ))
            ),
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
          fluidRow(
            column(
              width=page.width, 
              tags$div(
                background='#FFF3CD', 
                class="yellow-box", 
                { '[placeholder]'}
              ))
          ),
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
          fluidRow(
            column(
              width=page.width,
              checkboxInput(
                inputId='demog.selectAll', 
                label='Select all', 
                value=F) )),
          fluidRow(
            map(
              get.dimension.value.options(
                version=version,
                location=input[['geographic_location']]), 
              function(dim) {
                column(
                  width=page.width / length(
                    get.dimension.value.options(
                    version=version, 
                    location=input[['geographic_location']])),
                checkboxGroupInput(
                  inputId=dim[['name']],
                  label=dim[['label']],
                  choiceNames=unname(dim[['choices']]),
                  choiceValues=names(dim[['choices']]),
                  # selected=names(dim[['choices']])
                  # selected=ifelse(
                  #   input[['demog.selectAll']] == F, 
                  #   names(dim[['choices']]), 
                  #   rep('', length(names(dim[['choices']]))) 
                ))
            })
          ),
          fluidRow(
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
              }),
            fluidRow(
              column(
                width=page.width, 
                tags$div(
                  background='#FFF3CD', 
                  class="yellow-box", 
                  { '[placeholder]'}
                ))
            ),
            
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
