'EndingHIV RShiny web front-end process: UI spec'

##-------------------##
##-- LIBRARY CALLS --##
##-------------------##

library('shinydashboard')
library(shinyjs)

source('R/styling_helpers.R')


##------------------##
##-- DEFINE the UI--##
##------------------##

ui <- dashboardPage(
  
  ## Header
  add.style.to.tag(dashboardHeader(title="JHEEM: Ending HIV in the US", disable=F),
                        style='position: fixed; width:100%; z-index:99999'), #this keeps it pegged to the top/not scrollable
  
  ## Sidebar
  #  - Appear in app in order shown below
  add.style.to.tag(style='position: fixed;', #this keeps it pegged to the top/not scrollable
  dashboardSidebar(
    sidebarMenu(#style='position: fixed',
                id = 'side_menu',
      menuItem("Visualize Projections",  
               
               tabName="main", icon=icon("chart-line")),
     
      # menuItem("Parameters", tabName="raw_params", icon=icon("th")),
      menuItem("Custom Interventions", tabName="design-interventions", 
               icon=icon("wrench")),
      menuItem("About the Model", tabName="text", icon=icon("info-circle")),
#      menuItem("Frequently Asked Questions", tabName="faq", icon=icon("question-circle"))
      menuItem("Contact Us", tabName="help-and-feedback", icon=icon("envelope"))
    ),
    
    
    conditionalPanel(
      condition=paste0("input.side_menu == 'main'"),
      verticalSpacer(10),
      useShinyjs(),
      actionButton(
        style="background: #204C73; color: white; font-size:120%; margin: 0 auto;",
        "reset_main_sidebar", 
        HTML("Generate<BR>Projections"),
        disabled=T),
      href="#top"
  )

)),
  
  # Body
  dashboardBody(
    tabItems(
      tabItem(
        tabName="main",
        uiOutput("ui_main")),
      tabItem(
        tabName="design-interventions",
        uiOutput("design-interventions")),
      tabItem(
        tabName="text",
        uiOutput("introductionText")),
      tabItem(
        tabName="help-and-feedback",
        uiOutput("help-and-feedback"))
      
    )  # </Body (tabItems)>
  )  # </Body (dashboardBody)>

)  # </UI (dashboardPage)>

ui
