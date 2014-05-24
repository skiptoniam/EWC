library(shiny)
library(deSolve)
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Holland VS Spain EWC10 Final."),
  # Sidebar with two slider inputs
  sidebarPanel(
    sliderInput("Aslice",
                "Dutch Bossche Bol Consumption:",
                min = 1,
                max = 10,
                value = 5),
    sliderInput("Rslice",
                "Spanish Magdalena Consumption:",
                min = 1,
                max = 10,
                value = 5)
  ),
  # Show plot
  mainPanel(
    plotOutput("wcPlot")
  )
))
