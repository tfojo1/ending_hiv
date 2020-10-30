'EndingHIV RShiny web front-end process: UI spec'

##-------------------##
##-- LIBRARY CALLS --##
##-------------------##

library('shinydashboard')
library(shinyjs)


##------------------##
##-- DEFINE the UI--##
##------------------##

ui <- dashboardPage(
  
  ## Header
  add.style.to.top.level.tag(dashboardHeader(title="Ending HIV in the US", disable=F),
                        style='position: fixed; width:100%; z-index:99999'), #this keeps it pegged to the top/not scrollable
  
  ## Sidebar
  #  - Appear in app in order shown below
  add.style.to.top.level.tag(style='position: fixed;', #this keeps it pegged to the top/not scrollable
  dashboardSidebar(
    sidebarMenu(#style='position: fixed',
                id = 'side_menu',
      menuItem("Visualize projections", tabName="main", icon=icon("chart-line")),
      # menuItem("Parameters", tabName="raw_params", icon=icon("th")),
      menuItem("Custom interventions", tabName="design-interventions", 
               icon=icon("wrench")),
      menuItem("About the model", tabName="text", icon=icon("info-circle")),
      menuItem("Help and feedback", tabName="help-and-feedback", icon=icon("question-circle"))
      ),
    
    
    conditionalPanel(
      condition=paste0("input.side_menu == 'main'"),
      verticalSpacer(10),
      useShinyjs(),
      actionButton(
        style="background: #204C73; color: white; font-size:120%; margin: 0 auto;",
        "reset_main_sidebar", 
        HTML("Generate<BR>Projections")),
      href="#top"
      
      # menuItem(
      #   "Sensitivity Analysis", tabName="sens_ana", 
      #   icon=icon("bar-chart"))
  ))),
  
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
