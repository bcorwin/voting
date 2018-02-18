library(shiny)

PEOPLE <- sort(c("Ben", "Kevin", "Hannah", "AJ", "PJ", "Brandon"))

shinyUI(fluidPage(

  titlePanel("The Watch Voting"),

  uiOutput("all_tabs")

))
