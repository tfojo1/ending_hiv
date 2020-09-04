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
      menuItem("Run Model", tabName="main", icon=icon("dashboard")),
      # menuItem("Parameters", tabName="raw_params", icon=icon("th")),
      menuItem("Documentation", tabName="text", icon=icon("info"))
      # menuItem(
      #   "Sensitivity Analysis", tabName="sens_ana", 
      #   icon=icon("bar-chart"))
  )),
  
  # Body
  dashboardBody(
    tabItems(
      # Page: Docs
      # - Dynamic page; rendered in server.R under "#page-run-model"
      tabItem(
        tabName="text",
        uiOutput("introductionText")),
      
      # Page: Run model
      # - Dynamic page; rendered in server.R under "#page-docs"
      tabItem(
        tabName="main",
        uiOutput("ui_main")),
      
      # Page: Params
      # - Static page; defined here
      ui.pages.params  # tabItem
    )  # </Body (tabItems)>
  )  # </Body (dashboardBody)>

)  # </UI (dashboardPage)>

ui
