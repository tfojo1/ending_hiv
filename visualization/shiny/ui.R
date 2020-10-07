'EndingHIV RShiny web front-end process: UI spec'

source("R/ui.tools.R")
source("R/ui.pages.params.R")

# UI
ui <- dashboardPage(
  
  ## Header
  dashboardHeader(title="Ending HIV"),
  
  ## Sidebar
  #  - Appear in app in order shown below
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualize projections", tabName="main", icon=icon("dashboard")),
      # menuItem("Parameters", tabName="raw_params", icon=icon("th")),
      menuItem("Design interventions", tabName="design-interventions", 
               icon=icon("dashboard")),
      menuItem("About the model", tabName="text", icon=icon("info")),
      menuItem("Help and feedback", tabName="help-and-feedback", icon=icon("info"))
      
      # menuItem(
      #   "Sensitivity Analysis", tabName="sens_ana", 
      #   icon=icon("bar-chart"))
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
