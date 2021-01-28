# # EndingHiv; Page: Run Model (#page-run-model): output$ui_main

##-------------------------------##
##-- LIBRARIES and SOURCE CODE --####
##-------------------------------##

library('shiny')
library('shinycssloaders')
library('shinyWidgets')
library('purrr')
source('R/server_utils.R')

# This sourcing will be done by the parent server.R file
#source("R/plot_shiny_interface.R")  # plot.simulations

##--------------###
##- CONSTANTS --####
##--------------##

#version = '1.0'
page.width = 12
page.width.half = round(page.width / 2)
createPresetLabel = "Create a URL for these Projections"
state.ignoreList = c(
  'reset_main_sidebar',
  'sidebarItemExpanded',
  'sidebarCollapsed',
  'side_menu',
  'plotly_afterplot-A',
  '.clientValue-default-plotlyCrosstalkOpts',
  'plotly_relayout-A'
)

##----------------------------------------------------------##
##-- SOME HELPERS (that abstract away the 'input' object) --####
##----------------------------------------------------------##

#Here for future-proofing. For now, just one version possible
get.version <- function(input)
{
  '1.0' 
}

##-----------------------------------------------------##
##-- THE FUNCTION THAT GENERATES THE UI FOR THE PAGE --####
##-----------------------------------------------------##

#returns
server.routes.runModel.old.get <- function(input, session, state)
{
  # Todd: ON THE FIRST CALL (done once), set state() to defaults
  # TODO: @Todd: Wouldn't work here because outside reactive context. I put it
  # inside observe() in server.R. You can erase this note when you see it. - JEF
  
  # Component: PageDef #ui_main[renderUI]
  ui_main = renderUI({
    # Pre-processing ####
    # Todd: ON EVERY RENDER:set state to what's in input, EXCEPT for the location
    # TODO: @Todd: You can erase these notes when you see them: - JEF
    # 1. Can't do that inside of here because it triggers an endless cycle of 
    # recursive page re-renders every time state is changed. For some reason, this
    # was happening even if on my end, even with a lot of debugging, it appeared
    #  that state was not actually updating. I'm not sure why, but something
    #  was causing this to happen just by the presence of code that was merely
    #  just checking to see if anything needed to be updated.
    # 2. Under the context of presets, I'm not sure i can do what's asked
    #  for loc, because its in preset and needs to be set. And I don't think this
    #   is interfering w/ anything else.
    presetId = getPresetIdFromUrl(session)
    if (!(is.null(presetId)) && is.null(state()['presetId'])) {
      # 1. fetch query string from db
      presetTable.df = db.presets.read.all()
      presetRecord = presetTable.df[presetTable.df$id==presetId,]
      presetStr = presetRecord$urlQueryParamString
      # 2. parse it into a list
      presets = presets.urlQueryParamString.parse(presetStr)  # list
      # 3. set state
      tempstate = state()
      tempstate['presetId'] = presetId
      for (key in names(presets))
        tempstate[key] = presets[key]
      state(tempstate)
    }
    
    # Pre-processing: URL params: location
    if ('location' %in% names(
      parseQueryString(session$clientData$url_search))) {
      loc = parseQueryString(
        session$clientData$url_search)[['location']]
      # to-do: validate
      valid = TRUE  # placeholder
      if (valid == TRUE)
        location.choice = loc
      # to-do: Then update URL bar to fix conflict if user changes?
      # to-do: Show warning message if invalid?
    } else
      location.choice = invert.keyVals(
        get.location.options(version))[1]
    #   location.choice = input[['geographic_location']]
    # if (is.null(location.choice))
    #   location.choice = state()[['geographic_location']]
    
    # UI ####
    shinyjs::disable("reset_main_sidebar")
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
                  'Welcome to the Johns Hopkins HIV Epidemiological and Economic Model (JHEEM) Web Tool. This tool allows you to make projections of HIV Incidence, Prevalence, Diagnoses, and other HIV outcomes for local epidemics in 32 Metropolitan Statistical Areas (MSAs).
                  In particular, you can make projections for specific intervention that combine increases in HIV Testing, Pre-Exposure Prophylaxis (PrEP), and/or Viral Suppression among PWH.<BR>
                  To get started: <ol>
           <li> Select a Metropolitan Statistical Area from the "Location" box below </li>
           <li> Select interventions from the "Potential Interventions" box </li>
           <li> Click "Generate Projections" and look at the resulting Figure and Table</li>
           <li> (You can also select more HIV Outcomes to visualize, split up demographic subgroups, or customize the figures)
           </ol>')
              )
            )
          ),
          
          verticalSpacer(20),
          
          box(
            width=NULL, 
            title=tags$div(icon("chart-line"), "Projections"),
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
                    selected=state()[['toggle_main']],
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
                          width=(page.width / 4),
                          actionButton(
                            "downloadButton.plot",
                            label="Download Figure",
                            icon=icon('download'),
                            disabled=T) ),
                          column(
                            width=(page.width * 3 / 4),
                            actionButton(
                              inputId='createPresetId1',
                              label=createPresetLabel,
                              disabled=T,
                              icon = icon("globe", lib='font-awesome')) )
                        )
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
                        width=(page.width / 4),
                        downloadButton(
                          "downloadButton.table", 
                          "Download Table",
                          disabled=T)
                        ),
                      column(
                        width=(page.width * 3 / 4),
                        actionButton(
                          inputId='createPresetId2',
                          label=createPresetLabel) )
                      )
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
            title=tags$div(icon('map-marked-alt'), "Location"),
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
            title=tags$div(icon('toolbox'), "Potential Interventions"),
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
                  value=state()[['no_intervention_checkbox']],
                  width='100%' )),
            ),  # </fluidRow>
            
            #div(HTML("<HR>")),
            div(style = "font-size: 1.2em; padding: 0px 0px; margin-bottom:0px",
                HTML("<b>Intervention 1:</b>")),
            create.intervention.selector.panel(
              1, input, state),
            materialSwitch(
              inputId='use_intervention_2',
              label="Include a Second Intervention",
              value=state()[['use_intervention_2']],
              right=T,
              status='primary'),
            conditionalPanel(
              condition="(input.use_intervention_2)",
              div(
                style="font-size: 1.2em; padding: 0px 0px; margin-bottom:0px",
                HTML("<b>Intervention 2:</b>")),
              create.intervention.selector.panel(
                2, input, state),
                             )
          ))),  # </fluidRow>
      
      # Epidemiological dimensions ####
      'epidemiological-dimensions'=fluidRow(
        column(
          width=page.width,
          box(
            width=NULL, 
            title=tags$div(icon('table'), "HIV Outcomes (Incidence, Prevalence, etc)"),
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
                                    location=input$geographic_location),
                                ~ .x )),
                            choiceValues=names(map(
                                get.data.type.options(
                                    version=version, 
                                    location=input$geographic_location),
                                ~ .x )),
                            selected=state()[['epidemiological-indicators']] )
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
            title=tags$div(icon('user-friends'), "Demographic Subgroups"),
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
                            value=state()[['demog.selectAll']],
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
                  location=input$geographic_location), 
                function(dim) {
                  column(
                    width=page.width / length(
                      get.dimension.value.options(
                        version=version, 
                        location=input$geographic_location)),
                    checkboxGroupInput(
                      inputId=dim[['name']],
                      label=dim[['label']],
                      choiceNames=unname(dim[['choices']]),
                      choiceValues=names(dim[['choices']]),
                      # selected=names(dim[['choices']])
                      selected=state()[[dim[['name']]]]
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
                                location=input$geographic_location)),
                            choiceValues=names(get.facet.by.options(
                                version=version,
                                location=input$geographic_location)),
                            selected=state()[['facet']])
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
                                location=input$geographic_location)),
                            choiceValues=names(get.split.by.options(
                                version=version,
                                location=input$geographic_location)),
                            selected=state()[['split']]),
                        
                        tableRow(inner.padding = '5px',
                            materialSwitch(
                                inputId='color_by_split_1',
                                value=state()[['color_by_split_1']],
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
                  title=tags$div(icon('palette'), "Figure Options"),
                  collapsible=T,
                  collapsed=T,
                  status="primary", 
                  solidHeader=TRUE,
                  
                  fluidRow(
                    column(
                      width=12,
                     tableRow(
                       nestedWellPanel(title='What to Plot:',
                       tableRow(
                         inner.padding = '25px',
                        
                         column(width=12,
                         radioGroupButtons(
                           inputId='plot_format', 
                           size='normal',
                           direction='vertical',
                           status = 'primary',
                           choices=invert.keyVals(
                             get.plot.format.options(
                               version=version, 
                               location=input$geographic_location)),
                           selected=state()[['plot_format']])
                        ),
                                          
                        flowLayout(
                          column(
                            width=12,
                              tipBox("If 'Individual Simulations' is chosen, a separate line will be plotted for each individual simulation.",
                                     left.arrow = T),
                              tipBox("Otherwise, set the coverage for the plotted prediction interval",
                                     right.arrow = T)
                              )),
                        
                        knobInput(
                            inputId='interval_coverage',
                            label="Prediction Interval Coverage",
                            min=0,
                            max=100,
                            step=5,
                            value=state()[['interval_coverage']],
                            lineCap = 'default',
                            post='%')
                      )
                      )),
                             #),
                     verticalSpacer(10),
                     
                     tableRow(
                       nestedWellPanel(
                         title='Labels:',
                         tableRow(
                           flowLayout(
                             tipBox(
                               "Indicate whether the figure should include labels indicating the change in the outcome for each intervention. By default, this denotes the change from 2020 to 2030; drag the slider to change the years.",
                               right.arrow = T)),
                             column(
                               width=12,
                               materialSwitch(
                                 inputId='label_change', 
                                  label=HTML(
                                    '<b>Show Labels for the Change in Outcome</b>'), 
                                  value=state()[['label_change']],
                                  right=T,
                                  status='primary'),
                               sliderInput(
                                inputId='change_years',
                                label="From year to year",
                                min=2020,
                                max=2030,
                                step=1,
                                value=state()[['change_years']],
                                sep='')
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
                          status='primary',
                          selected=state()[['color_by']]),
                        flowLayout(
                          tipBox(
                            "If 'Interventions' is selected, lines representing the same intervention will have the same color, distinct from other interventions. If 'Demographic Subgroups' is selected, one color will be assigned to each demographic subgroup for which a separate line is plotted",
                            up.arrow = T)),
                        
                       ))))))
              )))
    )
    
    shinyjs::enable('reset_main_sidebar')
    
    rv})  # </list>  #returns
  
  ui_main
}
