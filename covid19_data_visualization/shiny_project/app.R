library(shiny)


source('myUI.R', local = TRUE)
source('myServer.R')


shinyApp(
  ui = myUI,
  server = myServer
)
