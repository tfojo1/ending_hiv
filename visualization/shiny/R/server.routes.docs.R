#server.routes.docs <- renderUI({includeHTML(
#    'introductionText.html'
#)})

#server.routes.docs <- renderUI({
#    HTML(markdown::markdownToHTML(knitr::knit('introductionText.Rmd', quiet = TRUE)))
#})

#server.routes.docs <- renderUI({HTML(renderMarkdown(
#    "introductionText.Rmd"))})

server.routes.docs <- renderUI({includeMarkdown(
  "introductionText.Rmd")})