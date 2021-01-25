# Imports  ####
'EndingHIV RShiny web front-end process: UI spec'

##-------------------##
##-- LIBRARY CALLS --##
##-------------------##

library('shinydashboard')
library(shinyjs)

source('R/styling_helpers.R')

# Variables ####
app.title = "JHEEM: Ending HIV in the US"

##------------------##
##-- DEFINE the UI--##
##------------------##
# Navbar Approach ####
ui.navbar = tagList(
  tags$head(
    tags$style(HTML("
     .navbar-brand {
       /* Was -15px, but that eliminated centering 'padding' */
       margin-left: 0 !important;
     }
     .box {
       margin-bottom: 0 !important;
     }
     .col-sm-12 {
       /* Box and column w/ width=12 */
       padding-left: 0;
       padding-right: 0;       
     }
     .dropdown-menu {
       /* ideally, customize this to the one in runModel page */
       left: -83px;   
     }
     .container-fluid {
       padding-left: 0;
       padding-right: 0;
     }
     body > div.container-fluid {
       padding-left: 0;
       padding-right: 0;
     }
     .navbar > div.container-fluid {
       /* This doesn't work, so just did '.container-fluid' */
       padding-left: 0;
       padding-right: 0;
     }
     #runmodel_nav {
       float: none !important;
     }
     #runmodel_nav > li:nth-child(4) {
       float: right;
       /* right: 50px; */
     }
     .navbar {
       margin-bottom: 0;
     }
     .sticky_footer { 
       position:fixed; bottom:0; right:0; left:0;
       background:#3c8dbc; z-index: 1000; opacity: 0.9; 
     }
     
    /* Static scrollbar: Sets only for right side of screen:
    ::-webkit-scrollbar {
      -webkit-appearance: none;
      width: 7px;
    }
    ::-webkit-scrollbar-thumb {
      border-radius: 4px;
      background-color: rgba(0, 0, 0, .5);
      box-shadow: 0 0 1px rgba(255, 255, 255, .5);
    } */
  "))),  # </tags$head>
  
  navbarPage(
    id='main_nav',
    title=app.title,
    # header=tags$div(),
    # footer=tags$div(),
    collapsible=T,
    tabPanel(
      "Make Projections",
      uiOutput("ui_main")),
    tabPanel(
      "Design Interventions",
      uiOutput("design-interventions")),
    tabPanel(
      "About the Model",
      uiOutput("text")),
    tabPanel(
      "FAQ",
      uiOutput("faq")),
    tabPanel(
      "Contact Us",
      uiOutput("help-and-feedback"))
  )  # </navbarPage>
)

# Dashboard approach ####
ui.dashboard <- dashboardPage(
  # Header ####
  add.style.to.tag(
    #this keeps it pegged to the top/not scrollable:
    style='position: fixed; width:100%; z-index:99999',
  dashboardHeader(
    title=app.title, 
    disable=F
    
    # Custom navbar attempts ####
    # Temp note: Failed attempt to have a custom navbar:
    # https://rdrr.io/cran/shinyjs/man/classFuncs.html
    # useShinyjs(),
    # tags$li(
    #   id='test-li',
    #   addClass("test-li", "dropdown"),
    #   'hello')
    
    # Temp note: This actually works:
    # tags$li(
    #   id='test-li',
    #   'hello') %>% 
    #   tagAppendAttributes(class='dropdown')
    
  )),  # </dashboardHeader>
  
  # Sidebar ####
  #  - Appear in app in order shown below
  
  add.style.to.tag(
    # this keeps it pegged to the top/not scrollable: 
    style='position: fixed;', 
  dashboardSidebar(
    sidebarMenu(
      id='side_menu',
      menuItem("Visualize Projections",  
        tabName="main", icon=icon("chart-line")),
      menuItem("Custom Interventions", tabName="design-interventions", 
               icon=icon("wrench")),
      menuItem("About the Model", tabName="text", icon=icon("info-circle")),
      menuItem("Contact Us", tabName="help-and-feedback", icon=icon("envelope"))
    ),
    conditionalPanel(
      condition=paste0("input.side_menu == 'main'"),
      verticalSpacer(10),
      useShinyjs(),
      actionButton(
        style="
          background: #204C73; 
          color: white; 
          font-size:120%; 
          margin: 0 auto;",
        "reset_main_sidebar", 
        HTML("Generate<BR>Projections"),
        disabled=T),
      href="#top")  # </conditionalPanel>

  )),  # </dashboardSidebar>
  
  # Body ####
  dashboardBody(
    tabItems(
      tabItem(
        tabName="main",
        uiOutput("ui_main")),
      tabItem(
        tabName="main_old",
        uiOutput("ui_main_old")),
      tabItem(
        tabName="design-interventions",
        uiOutput("design-interventions")),
      tabItem(
        tabName="text",
        uiOutput("introductionText")),
      tabItem(
        tabName="help-and-feedback",
        uiOutput("help-and-feedback"))
      
  ))  # </dashboardBody)>

)  # </UI (dashboardPage)>


# Post-processing ####
# n/a

# Exports ####
ui = ui.navbar
ui
