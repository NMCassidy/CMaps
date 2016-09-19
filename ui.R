library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("united"),
              # titlePanel("CPOP Mapping Feature"), 
  conditionalPanel("input.CPP != 'Select a CPP'",fluidRow(
    splitLayout(cellWidths = c("33%", "33%", "33%"),
      h5("Percentage of Children in Poverty", style = "text-align:center; margin-top:2; margin-bottom:0"), h5("S4 Average Tariff Score", style = "text-align:center; margin-top:2; margin-bottom:0"), h5("% School Leavers Entering Positive Destinations", style = "text-align:center; margin-top:2; margin-bottom:0"))
  )
  )
  ,   fluidRow(
        splitLayout(cellWidths = c("33%", "33%", "33%"),
           leafletOutput("newplot"), leafletOutput("newplot2"), leafletOutput("newplot3")))
        , 
    conditionalPanel("input.CPP != 'Select a CPP'",fluidRow(
    splitLayout(cellWidths = c("33%", "33%", "33%"),
    h5("% Aged 16-64 Receiving Out of Work Benefits", style = "text-align:center; margin-top: 3; margin-bottom:0"), h5("Number of SIMD Crimes per 10,000 People", style = "text-align:center; margin-top:3; margin-bottom:0"), h5("Emergency Admissions (65+) per 100,000 People", style = "text-align:center; margin-top:3; margin-bottom:0"))
  )
    )
  ,  fluidRow(
     splitLayout(cellWidths = c("33%", "33%", "33%"),
     leafletOutput("newplot4"), leafletOutput("newplot5"), leafletOutput("newplot6"))),
     absolutePanel(fixed = TRUE,
            draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
        width = 320, height = "auto", style = "opacity:0.9",
        tags$style(".well {background-color:azure2;}"),
     wellPanel(
       selectInput("CPP", "Select a CPP", 
       c("Select a CPP", unique(CPPdta$council)), selected = "Select a CPP")
       ,
       uiOutput("IZ"))
     )
))