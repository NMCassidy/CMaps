library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("readable"),
              # titlePanel("CPOP Mapping Feature"), 
  conditionalPanel("input.CPP != 'Select a CPP'",fluidRow(
    splitLayout(cellWidths = c("33%", "33%", "34%"),
      h3("map1", style = "text-align:center; margin-top:0"), h3("map2", style = "text-align:center; margin-top:0"), h3("map3", style = "text-align:center; margin-top:0"))
  )
  )
  ,   fluidRow(
        splitLayout(cellWidths = c("33%", "33%", "33%"),
           leafletOutput("newplot"), leafletOutput("newplot2"), leafletOutput("newplot3")))
        , 
    conditionalPanel("input.CPP != 'Select a CPP'",fluidRow(
    splitLayout(cellWidths = c("33%", "33%", "34%"),
    h3("map4", style = "text-align:center; margin-top:0"), h3("map5", style = "text-align:center; margin-top:0"), h3("map6", style = "text-align:center; margin-top:0"))
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