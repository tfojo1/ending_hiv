##-------------------------------##
##-- LIBRARIES and SOURCE CODE --####
##-------------------------------##

library('shiny')
# library('shinycssloaders')
# library('shinyWidgets')
# library('purrr')
source('R/server_utils.R')

# UI ####
##-----------------------------------------------------##
##-- THE FUNCTION THAT GENERATES THE UI FOR THE PAGE --##
##-----------------------------------------------------##

#returns
server.routes.runModel.get <- function(input, session, state) {
  ui_main = renderUI({
    # Pre-processing ####
    shinyjs::disable("reset_main_sidebar")

    # Declarative UI ####
    rv <- dashboardPage(
      # Header ####
      # add.style.to.tag(
      #   #this keeps it pegged to the top/not scrollable:
      #   style='position: fixed; width:100%; z-index:99999',
      dashboardHeader(
        title="JHEEM: Ending HIV in the US", 
        disable=T
        # )
      ),  # </dashboardHeader>
      
      # Sidebar ####
      
      # add.style.to.tag(
        # this keeps it pegged to the top/not scrollable: 
        # style='position: fixed;', 
        dashboardSidebar(
          tags$head(
            # tags$style(HTML(".sidebar .left-side .main-sidebar {
            # tags$style(HTML("#sidebarCollapsed .sidebar .main-sidebar {
            tags$style(HTML("
            aside#sidebarCollapsed.main-sidebar {
              /* height: 90vh !important; 
              background-color: #ecf0f5; */
              background-color: #f8f8f8;
              border-color: #e7e7e7;
              height: 90vh;
              min-height: 0 !important;
              padding-top: 0 !important;
              overflow-y: auto !important;
              overscroll-behavior: contain;
              color: black !important;
              padding-left: 0;
            }
            .sidebar {
              color: black;
          }"))),
          
          # Widgets
          selectInput(
            inputId="geographic_location", 
            # label=NULL,
            label="Metropolitan Statistical Area (MSA)",
            choices=invert.keyVals(get.location.options(
              version=version)),
            # selected=location.choice,
            selected=get.location.options(version=version)[1],
            multiple=FALSE,
            selectize=TRUE, 
            width=NULL, 
            size=NULL ),
                    
          # radioGroupButtons(
          radioButtons(
            inputId='Target Populations',
            label='Target Populations',
            choices=c(
              'Black and Hispanic MSM <35yo',
              'All MSM and All IDU',
              'All MSM, All IDU, and All Heterosexuals'
            ),
            # direction = 'vertical',
            # justified=T,
            # individual=F,
            # size='lg',
            # status='primary'
          ),
          
          # radioGroupButtons(
          radioButtons(
            inputId='Intervention Aspects',
            label='Intervention Aspects',
            choices=c(
              'PrEP Only',
              'Testing Only',
              'Suppression Only',
              'PrEP, Testing, and Suppression'
            ),
            # direction = 'vertical',
            # justified=T,
            # individual=F,
            # size='lg',
            # status='primary'
          ),
          
          # radioGroupButtons(
          radioButtons(
            inputId='Intervention Intensity',
            label='Intervention Intensity',
            choices=c(
              'PrEP 25%',
              'PrEP 50%'
            ),
            # direction = 'vertical',
            # justified=T,
            # individual=F,
            # size='lg',
            # status='primary'
          ),
          
          # radioGroupButtons(
          radioButtons(
            inputId='Ramp-up',
            label='Ramp-up',
            choices=c(
              '2021-2022',
              '2021-2024'
            ),
            # direction = 'vertical',
            # justified=T,
            # individual=F,
            # size='lg',
            # status='primary'
          ),
          
          # 'Run' Button
          verticalSpacer(10),
          useShinyjs(),
          actionButton(
            style="
              background: #204C73; 
              color: white; 
              font-size:120%; 
              margin: 0 auto;",
            "reset_main_sidebar", 
            HTML("Run"),
            disabled=T)
        ),  # </dashboardSidebar>
        # ),  # </dashboardSidebar/style>
      
      # Content area ####
      add.style.to.tag(
        style='
          /* position: fixed; 
          padding: 0;
          padding-left: 0; */
          background: white;
          overflow: auto;
          overscroll-behavior: contain;
          padding-top: 0;
          padding-bottom: 0;
          padding-right: 0;
        ',
        dashboardBody(
          id='runmodel_content',
          tags$head(
            # tags$style(HTML("#runmodel_content section.content {
            tags$style(HTML("#runmodel_content {
              /* 
              position: fixed; 
              padding: 0;
              padding-left: 0; 
              width: 90vw;
              overscroll-behavior-x: contain;
              */
              background: white;
              padding-top: 0;
              padding-bottom: 0;
              /* Need or else get x scroll bar: */
              padding-right: 20px;
              overflow: auto;
              overflow-x: auto;
              overflow-y: auto;
              overscroll-behavior: contain;
              overscroll-behavior-y: contain;
          }"))),
          # TODO: #now
          # Header & styles ####
          # This code sets the position and style for the progress bar when
          # loading simulations
          tags$head(
            tags$style("
        .shiny-notification {
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
        }")),
          
          # Output ####
          fluidRow(
            # column(
            #   width=page.width,
              
              # div(
              #   class="sticky_footer", 
              #   p("test footer")),
              
              navbarPage(
                id='runmodel_nav',
                title=NULL,
                # collapsible=TRUE,
                
                # header=tags$div(
                #   'header'
                # ),
                
                # footer=tags$div(
                #   'footer'
                # ),
                
                # Menu
                tabPanel(
                  title="Figure",
                  HTML('This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.')
                ),
                tabPanel(
                  title="Table",
                  HTML('This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.')
                ),
                tabPanel(
                  title="(Hide)",
                  ''
                ),
                tabPanel(
                  title="Run",
                  ''
                ),
                navbarMenu(
                  title="Share",
                  # Error when using icon:
                  # Warning: Error in : $ operator is invalid for atomic vectors
                  # icon='share-alt',
                  tabPanel(
                    title="Download figure",
                    # 'hello figure'
                  ),
                  tabPanel(
                    title="Download table",
                    # 'hello table'
                  ),
                  tabPanel(
                    title="Share link",
                    # 'hello share link'
                  )
                )  # </navbarMenu>
              ),  # </navbarPage>
            ),
          # ),
          
          # Options ####
          fluidRow(
            column(
              width=12,
              box(
                id='runmodel_options_box_body',
                width=12,
                column(
                  width=4,
                  # checkboxGroupInput(
                  #   inputId='epidemiological-indicators', 
                  #   label='Indicators', 
                  #   choiceNames=unname(map(
                  #     get.data.type.options(
                  #       version=version, 
                  #       location=invert.keyVals(
                  #         get.location.options(version))[1]),
                  #     ~ .x )),
                  #   choiceValues=names(map(
                  #     get.data.type.options(
                  #       version=version, 
                  #       location=invert.keyVals(
                  #         get.location.options(version))[1]),
                  #     ~ .x )),
                  #   # selected=state()[['epidemiological-indicators']] 
                  #   )
                checkboxGroupInput(
                  inputId='outcomes',
                  label='Outcomes', 
                  choices=c(
                    'Incidence',
                    'Reported Diagnoses',
                    'Prevalence',
                    'Mortality'
                  )
                )
                ),
                column(
                  width=4,
                  checkboxGroupInput(
                    inputId='visualize_subgroups',
                    label='Visualize Subgroups', 
                    choices=c(
                      'Age',
                      'Race',
                      'Sex',
                      'Risk Factor'
                    )
                  )
                ),
                column(
                  width=4,
                  sliderInput(
                    inputId='change_years',
                    label="From year to year",
                    min=2020,
                    max=2030,
                    step=1,
                    value=c(2020, 2030),
                    sep='')
                ),
              )
            )
          ),
          
          # Collapsible panel for more Options ####
          fluidRow(
            # column(
            #   width=12,
            #   box(
            #     id='runmodel_more_options_box_body',
            #     width=12,
            #     title='More options',
            #     collapsible=T,
            #     collapsed=T,
            #     'hello'
            #   )
            # )
          ),
      ))  # </style/dashboardBody)>
      
    )  # </UI (dashboardPage)>
    
    # Post-processing ####
    shinyjs::enable('reset_main_sidebar')
    
    rv
  })  # </list>  #returns
  
  ui_main
}
  