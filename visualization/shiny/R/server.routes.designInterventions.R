# EndingHiv; Page: Custom interventions
# Libs & source ####
##-------------------------------##
##-- LIBRARIES and SOURCE CODE --##
##-------------------------------##
library('shiny')
library('shinycssloaders')
library('shinyWidgets')
library('purrr')

# This sourcing will be done by the parent server.R file
#source("R/plot_shiny_interface.R")  # plot.simulations

# Constants ####
##--------------##
##- CONSTANTS --##
##--------------##
page.width = 12
page.width.half = round(page.width / 2)

# Auxiliary functional components ####
metricBox <- function(
  i,
  inputId.prefix,
  box.title,
  sliderInput.label,
  knobInput.label,
  knobInput.min,
  knobInput.max,
  knobInput.value,
  materialSwitch.label,
  knobInput.post='',
  column.width=page.width * 1/2
) {
  column(
    width=column.width,
    box(
      width=NULL, 
      title=box.title,
      collapsible=TRUE,
      collapsed=FALSE,
      status='info', 
      solidHeader=TRUE,
      
      wellPanel(
        
        fluidRow(
          materialSwitch(
            inputId=paste0(inputId.prefix, '_switch', i), 
            label=materialSwitch.label, 
            value=T,
            right=T,
            status='primary')),  # </rowInner1/3>
        
        fluidRow(conditionalPanel(
          condition=paste0(
            'input.', inputId.prefix, '_switch', i, ' == true'),
          tableRow(
            vertical.align='top',
            inner.padding='25px',
            column(
              width=12,
              align='center',
              tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}"),
              knobInput(
                inputId=paste0(inputId.prefix, '_value', i),
                label=knobInput.label,
                min=knobInput.min,
                max=knobInput.max,
                step=1,
                value=knobInput.value,
                pre=NULL,
                post=knobInput.post,
                cursor=FALSE,
                lineCap=c("default", "round")[1],
                rotation=c("clockwise", "anticlockwise")[1],
                # px only; doesn't seem to work
                # width='100px',  
                # height='100px', 
                angleOffset=0,
                angleArc=360,
                thickness=NULL,
                displayInput=TRUE,
                displayPrevious=FALSE,
                fgColor=NULL,
                inputColor=NULL,
                bgColor=NULL,
                fontSize=NULL,
                readOnly=FALSE,
                immediate=TRUE )) ),  # </rowInner2/3>
          
          tableRow(
            vertical.align='top',
            inner.padding='25px',
            sliderInput(
              inputId=paste0(inputId.prefix, '_years', i),
              label=sliderInput.label,
              value=c(2020, 2030),
              min=2020,
              max=2030,
              step=1))  # </rowInner3/3>
        ))  # </conditionalPanel /fluidRow>
  )))  # </wellPanel /box /column>            
}

customInterventionBox <- function(i, state) {
  collapse = TRUE
  customIntervention_box_switch.value = TRUE
  if (i == 1) {
    collapse = FALSE
    customIntervention_box_switch.value = TRUE
  }
  
  dimension.value.options = get.dimension.value.options(
    version=version,
    location=input$geographic_location,
    msm_idu_mode=TRUE)
  dimension.value.col.width = 12 / length(dimension.value.options)
    
  # conditionalPanel(
  #   condition=paste0(
  #     '(', as.character(i), ' == 1)',
  #     ' || ',
  #     '(input.group_addition_checkbox_', 
  #     as.character(as.numeric(i) - 1), 
  #     '== true)'), 
    fluidRow(
      column(
        width=page.width,
        box(
          title=paste0("Custom intervention ", i),
          collapsible=TRUE,
          collapsed=collapse,
          status="primary",
          width=NULL,
          solidHeader=TRUE,
          
          # Row 0.1/2: Activate 
          tableRow(
            inner.padding='25px',
            materialSwitch(
              inputId=paste0(
                'customIntervention_box_switch', i), 
              label=paste0('Use custom intervention ', i, ' ?'), 
              value=customIntervention_box_switch.value,
              right=F,
              status='primary')
          ),
          
          conditionalPanel(
            # condition=state()[[paste0(
            #   'customIntervention_box_switch', i)]] == TRUE,
            condition=paste0(
              '(input.customIntervention_box_switch', i, ' == true)'),
          # Row 1/2: Demog & 1 widget ####
          fluidRow(
            # Col 1/4: Demog ####
            column(
              width=page.width,
              box(
                width=NULL, 
                title=tags$div(icon('user-friends'), "Targeted Subgroup(s)"),
                collapsible=F,
                collapsed=F,
                status="success", 
                solidHeader=TRUE,
                
                tableRow(
                  lapply(dimension.value.options,
                         function(dim) {
                           column(
                             # align='center',
                             # tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}"),  
                             tags$style(type="text/css", "#string { height: 50px; width: 100%; display: block; padding-left: 20px; }"),
                             width=dimension.value.col.width,
                             fluidRow(
                               HTML(paste0('<b>', dim[['label']], '</b>'))
                             ),  # </fluidRow>
                             fluidRow( 
                               # tableRow(
                               #   vertical.align='top',
                               #   inner.padding='25px',
                               
                               materialSwitch(
                                 inputId=paste0(
                                   dim[['name']], '_switch', i),
                                 label='Select all',
                                 value=FALSE,
                                 right=TRUE,
                                 status='primary'),
                               # checkboxInput(
                               #   inputId=paste0(
                               #     dim[['name']], '_switch', i),
                               #   label='Select all',
                               #   value=FALSE),
                               
                               # ),  # </fluidRow>
                               # fluidRow(
                               checkboxGroupInput(
                                 inputId=paste0(dim[['name']], i),
                                 # label=dim[['label']],
                                 label=NULL,
                                 selected=state()[[
                                   paste0(dim[['name']], i)]],
                                 # selected=names(dim[['choices']]),
                                 choiceNames=unname(dim[['choices']]),
                                 choiceValues=names(dim[['choices']]) )
                             )  # </fluidRow>
                           )  # </column>
                         })
                )
              ))
          ),  # </row(1/2)>
          
          
          
          
          # Row 2/2: 2 widgets ####
          fluidRow(
            # Col 2/4: Test frequency ####
            metricBox(
              i=i,
              inputId.prefix='test_freq_months',
              box.title=div(icon('vial'), "HIV Testing"),
              materialSwitch.label=HTML(
                '<b>Include testing in intervention</b>'),
              knobInput.label='Frequency of testing 
            (individuals in targeted subgroups are tested,
            on average, once every so many months)',
              knobInput.min=1,
              knobInput.max=24,
              knobInput.value=12,
              knobInput.post='mo',
              # knobInput.post='',
              sliderInput.label='Years from when testing
            intervention begins to when it is fully
            implemented',
              column.width=page.width * 1/3),
            
            # Col 3/4: PrEP ####
            metricBox(
              i=i,
              inputId.prefix='prep',
              box.title=div(icon('pills'), 'Pre-Exposure Prophylaxis (PrEP)'), #alt icon: prescription-bottle-alt
              materialSwitch.label=HTML(
                '<b>Include PrEP in Intervention</b>'),
              knobInput.label='Proportion on PrEP: (this percentage of
              individuals in targeted subgroups are prescribed and 
              adherent to PrEP with HIV screening every 3mo)',
              knobInput.min=1,
              knobInput.max=100,
              knobInput.value=25,
              knobInput.post='%',
              sliderInput.label='Years from when PrEP intervention
              begins to when it is fully implemented',
              column.width=page.width * 1/3),
            
            # Col 4/4: Viral suppression ####
            metricBox(
              i=i,
              inputId.prefix='viral_suppression',
              box.title=div(icon('virus'), 'Viral Suppression'),
              materialSwitch.label=HTML(
                '<b>Include viral suppression in Intervention</b>'),
              knobInput.label='Suppressed Proportion: (this percentage of 
              PWH with diagnosed HIV in the targeted subgroups are virally 
              suppressed)',
              knobInput.min=1,
              knobInput.max=100,
              knobInput.value=80,
              knobInput.post='%',
              sliderInput.label='Years from when suppression intervention
              begins to when it is fully implemented',
              column.width=page.width * 1/3)
            
          )  # </row 2/2>
        # )  # </conditionalPanel>
        ),  # </conditionalPanel>
      )))  # </box/column/row/
}

# Main functional component ####
##-----------------------------------------------------##
##-- THE FUNCTION THAT GENERATES THE UI FOR THE PAGE --##
##-----------------------------------------------------##
server.routes.designInterventions.get <- function(
  input, session, config, state
) {
  # Component: PageDef #ui_main[renderUI]
  ui_main = renderUI({
    # Pre-processing: n/a ####
    # n/a
    
    # UI ####
    rv = list(  # returns-->list
      # Header & styles ####
      # This code sets the position and style for the progress bar when
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
      
      verticalSpacer(40),

      # Run Interventions button ####
      tags$table(
        tags$tr(
          
          tags$td(style='padding-right: 20px',
            actionButton(
              style="background: #204C73; color: white; font-size:150%;
              margin: 0 auto;",
              'run_custom_interventions', 
              HTML("Run<BR>Interventions")) ),
          
          tags$td(
            tipBox(
              width=12,
              'To make custom interventions:<ol>
           <li> First, tap your feet </li>
           <li> Then, wave your hands around </li>
           <li> Click "Run Interventions" </li>
           </ol>') )
          
      )),  # </tr/table>
      
      verticalSpacer(10),
      
      fluidRow(
      # tableRow(
      #   vertical.align='top',
      #   inner.padding='25px',
        column(
          width=page.width,
            textInput(
              inputId='intervention_name', 
              label='Intervention name', 
              # value='My intervention', 
              placeholder='My intervention') )),
      
      # Custom interventions boxes ####
      map(
          1:config()[['customInterventions.groups.max']],
          function(i) customInterventionBox(i, state) )
    )  # </list>
    
    # Post-processing ####
    # shinyjs::enable('reset_main_sidebar')
    rv
  })  #returns
  
  ui_main
}
