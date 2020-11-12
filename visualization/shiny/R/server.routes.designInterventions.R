# # EndingHiv; Page: Custom interventions

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

page.width = 12
page.width.half = round(page.width / 2)

##-----------------------------------------------------##
##-- THE FUNCTION THAT GENERATES THE UI FOR THE PAGE --##
##-----------------------------------------------------##

#returns
server.routes.designInterventions.get <- function(input, session, state)
{
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

      fluidRow(
        column(
          width=page.width,
          box(
            title="Number of custom interventions",
            collapsible=T,
            collapsed=F,
            status="info",
            width=NULL,
            solidHeader=TRUE,
            
            # numericInput(
            fluidRow(
              column(
                width=page.width,
                sliderInput(
                  inputId='n-custom-interventions',
                  label='',
                  # label=paste(
                  #   input[['n-custom-interventions']], ' interventions'),
                  # value=1,
                  value=state()[['n-custom-interventions']],
                  min=1,
                  max=5,
                  # step=1
            ))),
            fluidRow(
              column(
                width=page.width,
                actionButton(
                  inputId='n-custom-interventions-btn', 
                  label='Update')
            ))
            
      ))),
      
      # Map ####
      map(
          1:state()[['n-custom-interventions']],
          # c(1:input[['n-custom-interventions']])  # got a 'length 0' error
        function(i) {

          # Row
          fluidRow(
            column(
              width=page.width,
              box(
                title=paste("Custom intervention groups", i, sep=' '),
                collapsible=T,
                collapsed=T,
                status="primary",
                width=NULL,
                solidHeader=TRUE,

                column(
                  width=page.width * 2/3,
                  wellPanel(
                    fluidRow(
                      map(
                        get.dimension.value.options(
                          version=version,
                          location=input[['geographic_location']],
                          msm_idu_mode=TRUE),
                        function(dim) {
                          column(
                            width=page.width / length(
                              get.dimension.value.options(
                                version=version,
                                location=input[['geographic_location']],
                                msm_idu_mode=TRUE)),
                            checkboxGroupInput(
                              inputId=paste(
                                dim[['name']], i, sep=' '),
                              # inputId=dim[['name']],
                              label=dim[['label']],
                              choiceNames=unname(dim[['choices']]),
                              choiceValues=names(dim[['choices']]),
                              selected=names(dim[['choices']])
                            ))
                        })
                    ))),

                column(
                  width=page.width * 1/3,
                  wellPanel(
                    fluidRow(
                      # numericInput(
                      sliderInput(
                        inputId=paste(
                          "test-freq-months", i, sep=' '),
                        # inputId='test-freq-months',
                        label='Test frequency (in months)',
                        value=6,
                        min=0,
                        max=12,
                        step=1),
                      sliderInput(
                        inputId=paste(
                          "prep-pct", i, sep=' '),
                        # inputId='prep-pct',
                        label='% individuals on PrEP',
                        value=50,
                        min=0,
                        max=100,
                        step=1),
                      sliderInput(
                        inputId=paste(
                          "virally-suppresssed-pct", i, sep=' '),
                        # inputId='virally-suppresssed-pct',
                        label='% HIV positive individuals virally suppressed',
                        value=50,
                        min=0,
                        max=100,
                        step=1)
                    )))
              )))
        })
    
    )
    
    #   # Row ####
    #   fluidRow(
    #     column(
    #       width=page.width,
    #       box(
    #         title="Custom intervention groups",
    #         collapsible=T,
    #         collapsed=F,
    #         status="primary",
    #         width=NULL, 
    #         solidHeader=TRUE,
    #         
    #         column(
    #           width=page.width * 2/3, 
    #           wellPanel(
    #             fluidRow(
    #               map(
    #                 get.dimension.value.options(
    #                   version=version,
    #                   location=input[['geographic_location']],
    #                   msm_idu_mode=TRUE), 
    #                 function(dim) {
    #                   column(
    #                     width=page.width / length(
    #                       get.dimension.value.options(
    #                         version=version, 
    #                         location=input[['geographic_location']],
    #                         msm_idu_mode=TRUE)),
    #                     checkboxGroupInput(
    #                       inputId=dim[['name']],
    #                       label=dim[['label']],
    #                       choiceNames=unname(dim[['choices']]),
    #                       choiceValues=names(dim[['choices']]),
    #                       selected=names(dim[['choices']])
    #                     ))
    #                 })
    #         ))),
    #         
    #         column(
    #             width=page.width * 1/3,
    #             wellPanel(
    #               fluidRow(
    #                 # numericInput(
    #                 sliderInput(
    #                   inputId='test-freq-months',
    #                   label='Test frequency (in months)',
    #                   value=6,
    #                   min=0,
    #                   max=12,
    #                   step=1),
    #                 sliderInput(
    #                   inputId='prep-pct',
    #                   label='% individuals on PrEP',
    #                   value=50,
    #                   min=0,
    #                   max=100,
    #                   step=1),
    #                 sliderInput(
    #                   inputId='virally-suppresssed-pct',
    #                   label='% HIV positive individuals virally suppressed',
    #                   value=50,
    #                   min=0,
    #                   max=100,
    #                   step=1)
    #         )))
    #   
    # ))))  # </list>
    # 
    # 
    
    # Post-processing ####
    # shinyjs::enable('reset_main_sidebar')
    rv
  })  #returns
  
  
  ui_main
}
