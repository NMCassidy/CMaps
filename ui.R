library(shinythemes)

shinyUI(navbarPage(title = "CPOP Mapping Feature",theme = shinytheme("readable"),
                  
    #CSS to make sure that there is no scroll even on smaller screens
                  tags$head(tags$style("#newplot{height:46vh !important;}",
                                       "#newplot2{height:46vh !important;}",
                                       "#newplot3{height:46vh !important;}",
                                       "#newplot4{height:46vh !important;}",
                                       "#newplot5{height:46vh !important;}",
                                       "#newplot6{height:46vh !important;}",
                                       HTML("
                                            h5 {
                                            height: 1.5vh;
                                            margin-top:1vh;
                                            margin-bottom:1vh;
                                            text-align:center;
                                            font-weight: bold
                                            }"))),
    tabPanel("Data Zone Level Maps", 
  conditionalPanel("input.CPP != 'Select a CPP'",fluidRow(
    splitLayout(cellWidths = c("33%", "33%", "33%"),
      tags$h5("Percentage of Children in Poverty"), h5("S4 Average Tariff Score"), h5("% School Leavers Entering Positive Destinations"))
  )
  )
  ,
  fluidRow(
        splitLayout(cellWidths = c("33%", "33%", "33%"),
           leafletOutput("newplot"), leafletOutput("newplot2"), leafletOutput("newplot3")))
        , 
  hr(style = "margin-bottom:0.3vh; margin-top:0.5vh"),
    conditionalPanel("input.CPP != 'Select a CPP'",fluidRow(
    splitLayout(cellWidths = c("33%", "33%", "33%"),
    h5("% Aged 16-64 Receiving Out of Work Benefits"), h5("Number of SIMD Crimes per 10,000 People"), h5("Emergency Admissions (65+) per 100,000 People"))
  )
    )
  ,  fluidRow(
     splitLayout(cellWidths = c("33%", "33%", "33%"),
     leafletOutput("newplot4"), leafletOutput("newplot5"), leafletOutput("newplot6"))),
     absolutePanel(fixed = FALSE,
            draggable = FALSE, top = 70, left = "auto", right = 20, bottom = "auto",
        width = 320, height = "auto", style = "opacity:0.9",
        tags$style(".well {background-color:azure2;}"),
     wellPanel(
       selectInput("CPP", "Select a CPP", 
       c("Select a CPP", unique(CPPdta$council)), selected = "Select a CPP")
       ,
       uiOutput("IZ"))
     )
    )
))