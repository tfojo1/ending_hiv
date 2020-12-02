# server.routes.helpAndFeedback <- renderUI({includeMarkdown(
#   "help-and-feedback.Rmd")})

# Send email functionality ####
# TODO: send email
# https://www.google.com/search?q=r+how+to+send+an+email&rlz=1C5CHFA_enUS711US711&oq=r+how+to+send+an+email&aqs=chrome..69i57j0i22i30i457j69i64l2.5847j0j7&sourceid=chrome&ie=UTF-8
# https://blog.mailtrap.io/r-send-email/

# try 1
# https://www.rdocumentation.org/packages/mailR/versions/0.4.1/
# topics/send.mail
# library('mailR')
# sender <- "joeflack4@gmail.com"
# recipients <- c("joeflack4@gmail.com")
# email <- send.mail(
#   from = sender,
#   to = recipients,
#   subject="Subject of the email",
#   body = "Body of the email",
#   smtp = list(host.name = "aspmx.l.google.com", port = 25),
#   authenticate = FALSE,
#   send = FALSE)
# email$send()
# 

# Form ####
# TODO: create form
server.routes.helpAndFeedback.get <- function(input) {
  ui_main = renderUI({
    
    list(
      verticalSpacer(40),
      
      box(
        width=NULL, 
        title=tags$div(icon('envelope'), "Contact Form"),
        collapsible=F,
        collapsed=F,
        status="primary", 
        solidHeader=TRUE,
        
        tableRow(
          inner.padding='25px',
          column(
            width=page.width,
            fluidRow(
              # tableRow(
              # vertical.align='top',
              # inner.padding='25px',
              textInput(
                inputId='feedback_name', 
                label='Your name') ),
            fluidRow(
              textInput(
                inputId='feedback_email', 
                label='Your email') )
          ),
          column(
            width=page.width,
            textAreaInput(
              inputId='feedback_contents', 
              label='Your message',
              height='250px',
              width='375px',
              # cols=80,
              # rows=6,
              )
          )
        ),
        tableRow(
          inner.padding='25px',
          actionButton(
            inputId='feedback_submit',
            label='Submit')
        )
      )

      
  )})
  ui_main
}

