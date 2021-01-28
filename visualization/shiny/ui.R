##-------------------##
##-- LIBRARY CALLS --####
##-------------------##
'EndingHIV RShiny web front-end process: UI spec'


library('shinydashboard')
library('shinyjs')

source('R/styling_helpers.R')


##------------------##
##-- DEFINE the UI--####
##------------------##
# Variables
app.title = "JHEEM: Ending HIV in the US"

# UI
ui = tagList(
  tags$head(
    tags$style(HTML("
     .wrapper {
       background-color: #f8f8f8 !important;
     }
     .content-wrapper {
       background-color: #f8f8f8 !important;
     }
     #runmodel_options_box_body {
       background-color: #f8f8f8 !important;
     }
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
     /* Not sure why 'Run' appears 4th, even though listed 5th in the UI. */
     #runmodel_nav > li:nth-child(4) {
       /* color: white; 
       background: #1d7cd0; light blue
       background: #107ad4; vibrant blue
       background: #337ab7; ui consistent blue
       background: #204C73; same as other run button but darker; dk why 
       */
       float: right;
       background: #107ad4;
     }
     #runmodel_nav > li:nth-child(4) > a {
       color: white
     }
     #runmodel_nav > li:nth-child(5) {
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
