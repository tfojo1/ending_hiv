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

#returns
server.routes.runModel.get <- function(input, session)
{
  # Component: PageDef #ui_main[renderUI]
  ui_main = renderUI({
    urlParams = parseQueryString(
      session$clientData$url_search)
    # Pre-processing: URL Params: presetId ####
    presetKeyUsed = NULL
    presetPermutations = c(
      'preset', 'presetId', 'presetID', 'presetID', 'presetid', 
      'PRESETID', 'preset_id', 'preset_ID', 'PRESET_ID')
    for (permu in presetPermutations)
      if (permu %in% names(urlParams)) {
        presetKeyUsed = permu
        break }
    
    # TODO: @Joe: continue from here
    if (!(is.null(presetKeyUsed))) {
      presetId = parseQueryString(
        session$clientData$url_search)[[presetKeyUsed]]
      # 1. fetch query string from db
      presetStr = db.presets.read.all()
      
      # 1.5 filter by id==presetId
      # to-do
      
      # 2. parse it into a list
      presets = presets.urlQueryParamString.parse(presetStr)
      
      # 3. for each key in list set input[[key]]=val
      # #to-do
    }
    
    
    # Pre-processing: URL params: location ####
    # Location.choice
    # TODO: @Todd: From our meeting on Fri 10/30/2020:
    #   I think I've added what I need here. - Joe
    # ---
    # "What we actually want to do is have 2 different options. 
    # one is 'session=sessionId'. Todd will
    # add a mapping. And it will return a vector of location codes. And 
    # then Todd will do some work afterwards
    # to invert, and check if the location passed is a valid location name.
    
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
      
      if (valid == TRUE)
        location.choice = loc
      
      # to-do: Then update URL bar to fix conflict if user changes?
      # to-do: Show warning message if invalid?
    } else
      location.choice = input[['geographic_location']]
    
    if (is.null(location.choice))
      location.choice = invert.keyVals(
        get.location.options(version))[1]
    
    shinyjs::disable("reset_main_sidebar")
    
    
    # UI ####
    rv = list(  # returns-->list
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
        .modebar-container: {
          background: 'black';
          border-style: solid;
          border-color: black
        }
        .modebar: {
          orientation: 'v';
          position: 'right'
      }
      ")),
      
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
                  "reset_main", 
                  HTML("Generate<BR>Projections"))
              ),
              tags$td(
                tipBox(
                  width=12,
                  'To make projections:<ol>
           <li> Select a location from the "Locations" tab </li>
           <li> Select interventions from the "Potential Interventions" tab </li>
           <li> Click "Generate Projections" </li>
           </ol>')
              )
            )
          ),
          
          # TODO: @TF/@Todd: I did not think much about placement of this.
          fluidRow(
            column(
              width=page.width.half,
              actionButton(
                inputId='createPresetId',
                label='Create unique preset based on selections') ),
            column(
              width=page.width.half,
              textOutput(
                outputId='createPresetId_msg') )
          ),
          verticalSpacer(20),
          
          box(
            width=NULL, 
            title="Projections",
            collapsible=T,
            collapsed=F,
            status="primary", 
            solidHeader=TRUE,

            # plot and table
            fluidRow(
              #   tags$head(tags$style("#tbl {white-space: nowrap;}")),
              # ),
              column(
                width=page.width,
                div(style='padding: 10px',
                  tabsetPanel(
                    id='toggle_main',
                    selected='Figure',
                    type='tabs',
                    
                    #Figure
                    tabPanel(
                      title='Figure',
                      value='Figure',
                      
                      fluidRow(
                        plotlyOutput(
                          outputId="mainPlot",
                          height="auto",
                          width='100%',#"auto",
                          inline=T)  %>% withSpinner(color="#0dc5c1") ),
                      
                      fluidRow(
                        # to-do: Would be nice to only appear or not be
                        # greyed out on some condition.
                        # conditionalPanel(
                        #   condition="(input.show_download  !== undefined && input.show_download !== null)",
                        column(
                          width=(page.width * 11/12),
                          actionButton("downloadButton.plot",
                                       label="Download Figure",
                                       icon=icon('download'),
                                       disabled=T)
                             ))
                        
                    ),
                  
                  #Table
                  tabPanel(
                    title='Table',
                    value='Table',
                    
                    fluidRow( 
                    verbatimTextOutput(
                      placeholder=FALSE,
                      'mainTable_message'),
                    div(
                       style='overflow-x: scroll', 
                       dataTableOutput(outputId="mainTable")) ),
                    fluidRow(
                      # to-do: Would be nice to only appear or not be
                      # greyed out on some condition.
                      # conditionalPanel(
                      #   condition="(input.show_download  !== undefined && input.show_download !== null)",
                      column(
                        width=(page.width * 11/12),
                        downloadButton(
                          "downloadButton.table", 
                          "Download Table",
                          disabled=T)
                        ))
                    # )
                  )
                )  # /tabsetPanel
            )))
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
            div(style = "font-size: 1.2em; padding: 0px 0px; margin-bottom:0px",
                HTML("<b>Intervention 1:</b>")),
            create.intervention.selector.panel(1, input),
<<<<<<< HEAD
            materialSwitch(inputId = 'use_intervention_2',
                          label = "Include a Second Intervention",
                          value=F,
                          right=T,
                          status='primary'),
            conditionalPanel(condition="(input.use_intervention_2)",
                             div(style = "font-size: 1.2em; padding: 0px 0px; margin-bottom:0px",
                                              HTML("<b>Intervention 2:</b>")),
                             create.intervention.selector.panel(2, input),
                             )
=======
            checkboxInput(
              inputId = 'use_intervention_2',
              label = "Include a Second Intervention",
              value=F),
            conditionalPanel(
              condition="(input.use_intervention_2)",
              div(
                style="font-size: 1.2em; padding: 0px 0px; margin-bottom:0px",
                HTML("<b>Intervention 2:</b>")),
              create.intervention.selector.panel(2, input), 
            )
>>>>>>> ff424bf93323c30445ca326e428539d7323d9252
            
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
            
            tableRow(inner.padding='25px',
                fluidRow(
                    column(
                        width=12,
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
                    )),
                tipBox("Select the Epidemiological Indicators to be displayed as outcomes in the plot. Each indicator will be plotted on a separate panel",
                       left.arrow = T, width=6)
                
                
              ),
          ),
        )),
      
      
     
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
            
            fluidRow(column(width=12, wellPanel(
                tableRow(inner.padding = '30px',
                    fluidRow(column(width=12,
                        HTML("<b>Subgroups to Include in Plots:</b>"),
                        materialSwitch(
                            inputId='demog.selectAll', 
                            label='Select All Subgroups', 
                            value=F,
                            right=T,
                            status='primary'
                        )
                    )),
                    tipBox("Only demographic subgroups whose characteristics are checked below will be included in the figures",
                           left.arrow = T, left.arrow.direction = 'down', left.arrow.align = 'bottom',
                           right.arrow = T, right.arrow.direction = 'down', right.arrow.align = 'bottom')
                ),
        
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
                      selected=names(dim[['choices']])
                    ))
                })
            )
            ))),
           
            fluidRow(
            column(
                width=page.width.half,
                wellPanel(fluidRow(
                    
                    column(width=6,
                           tipBox("Any checked boxes here will result in outcomes for the corresponding subgroups being displayed in separate panels. For example, checking 'Race' will yield separate panels for 'Black', 'Hispanic', and 'Other'",
                                  right.arrow = T, right.arrow.align = 'middle')
                    ),
                    
                    column(width=6,
                        checkboxGroupInput(
                            inputId='facet', 
                            label='Make Separate Panels for Each:', 
                            choiceNames=unname(get.facet.by.options(
                                version=version,
                                location=input[['geographic_location']])),
                            choiceValues=names(get.facet.by.options(
                                version=version,
                                location=input[['geographic_location']])),
                            selected=NULL)
                    )
                ))),
            
            column(
                width=page.width.half,
                wellPanel(fluidRow(
                    column(width=6,
                           tipBox("Any checked boxes here will result in outcomes for the corresponding subgroups being represented by separate LINES within the same panel. For example, checking 'Race' will plot separate lines for 'Black', 'Hispanic', and 'Other' instead of one line for the total",
                                  right.arrow = T, right.arrow.align = 'middle')
                    ),
                    
                    column(width=6,
                        checkboxGroupInput(
                            inputId='split', 
                            label='Within a Panel, Plot Separate Lines for Each:', 
                            choiceNames=unname(get.split.by.options(
                                version=version,
                                location=input[['geographic_location']])),
                            choiceValues=names(get.split.by.options(
                                version=version,
                                location=input[['geographic_location']])),
                            selected=NULL),
                        
                        tableRow(inner.padding = '5px',
                            materialSwitch(
                                inputId = 'color_by_split_1',
                                value=F,
                                right=T,
                                status='primary'
                            ),
                            HTML('<b>Color Lines by Subgroup</b><BR>(instead of by Intervention)')
                        )
                    )
                ))
            )
      )))
      ),  # /fluidrow
      
      # Aggregation options ####
      # to-do: expand/collapse feature
      'plot-options'=fluidRow(
          column(
              width=page.width,
              box(
                  width=NULL, 
                  title="Figure Options",
                  collapsible=T,
                  collapsed=T,
                  status="primary", 
                  solidHeader=TRUE,
                  
                  fluidRow(
                      column(width=12,
                             tableRow(nestedWellPanel(title='What to Plot:',
                                 tableRow(inner.padding = '25px',
                                          column(width=12,
                                                 radioGroupButtons(
                                                     inputId='plot_format', 
                                                     size='normal',
                                                     direction='vertical',
                                                     status = 'primary',
                                                     choices=invert.keyVals(get.plot.format.options(
                                                         version=version,
                                                         location=input[['geographic_location']])),
                                                     selected=NULL)
                                          ),
                                          
                                          flowLayout(column(width=12,
                                                            tipBox("If 'Individual Simulations' is chosen, a separate line will be plotted for each individual simulation.",
                                                                   left.arrow = T),
                                                            tipBox("Otherwise, set the coverage for the plotted prediction interval",
                                                                   right.arrow = T)
                                                            )),
                                          
                                          knobInput(
                                              inputId = 'interval_coverage',
                                              label = "Prediction Interval Coverage",
                                              min=0,
                                              max=100,
                                              step=5,
                                              value=95,
                                              lineCap = 'default',
                                              post='%')
                                 )
                             )),
                             #),
                             verticalSpacer(10),
                             tableRow(nestedWellPanel(title='Labels:',
                                 tableRow(
                                     flowLayout(tipBox("Indicate whether the figure should include labels indicating the change in the outcome for each intervention. By default, this denotes the change from 2020 to 2030; drag the slider to change the years.",
                                                       right.arrow = T)),
                                     column(width=12,
                                            materialSwitch(
                                                inputId='label_change', 
                                                label=HTML('<b>Show Labels for the Change in Outcome</b>'), 
                                                value=T,
                                                right=T,
                                                status='primary'
                                            ),
                                            sliderInput(
                                                inputId='change_years',
                                                label="From year to year",
                                                min=2020,
                                                max=2030,
                                                step = 1,
                                                value=c(2020,2030),
                                                sep = ''
                                            )
                                     )
                                 ))),
                             
                             verticalSpacer(10),
                             tableRow(nestedWellPanel(
                                 title='Colors:',
                                 fluidRow(column(width=12,
                                        radioGroupButtons(
                                            inputId='color_by',
                                            label='Color Lines By',
                                            choices=c('Intervention','Demographic Subgroup'),
                                            status='primary'
                                        ),
                                        
                                        flowLayout(tipBox("If 'Interventions' is selected, lines representing the same intervention will have the same color, distinct from other interventions. If 'Demographic Subgroups' is selected, one color will be assigned to each demographic subgroup for which a separate line is plotted",
                                                          up.arrow = T)),
                                 )
                             )))
                      )
                  )
                  
                  
              )))
    )
    
    shinyjs::enable('reset_main_sidebar')
    
    rv})  # </list>  #returns
 
  
  ui_main
}
