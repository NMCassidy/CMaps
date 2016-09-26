library(shinythemes)

shinyUI(navbarPage(title = "CPOP Mapping Feature",id = "nav", theme = "bootstrap.css",
    #CSS to make sure that there is no scroll even on smaller screens
                  tags$head(tags$style("#newplot{height:39vh !important;border-style: solid;border-width:1px}",
                                       "#newplot2{height:39vh !important;border-style: solid;border-width:1px}",
                                       "#newplot3{height:39vh !important;border-style: solid;border-width:1px}",
                                       "#newplot4{height:39vh !important;border-style: solid;border-width:1px}",
                                       "#newplot5{height:39vh !important;border-style: solid;border-width:1px}",
                                       "#newplot6{height:39vh !important;border-style: solid;border-width:1px}",
                                       ".well {background-color:white; padding-bottom:0px; height:0vh;}",
                                       ".row-fluid {padding-top:7vh;}",
                                       HTML("
                                            h5 {
                                            height: 1.5vh;
                                            margin-top:0.6vh;
                                            margin-bottom:1vh;
                                            text-align:center;
                                            font-weight: bold
                                            }"))),   
   tabPanel("Data Zone Level Maps", 
            fluidPage(
            absolutePanel(fixed = FALSE,
                           draggable = FALSE, top = "30px", left = 0, right = 0, bottom = 0,
                           width = "100%", height = "auto", style = "opacity:1",
                           wellPanel(
                              div(class = "row",
                                              div(class = "span4",style = "display: inline-block; vertical-align: text-top; padding-right:6vh; padding-left:6vh", 
                                                  selectInput("CPP", h5("Select a CPP"), 
                                                 c("Select a CPP", unique(CPPdta$council)), selected = "Select a CPP", width = "350px")
                                                       ),
                                    div(class = "span4", style = "display: inline-block; vertical-align: text-top; width: 450px",
                             uiOutput("IZUI")))
                           )
             ),
  conditionalPanel("input.CPP != 'Select a CPP'", div(class = "row-fluid",
    fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
      h5("Percentage of Children in Poverty"), h5("S4 Average Tariff Score"), h5("% School Leavers Entering Positive Destinations"))
  )
  ,
      fluidRow(splitLayout(cellWidths = c("33%", "33%", "33%"),
           leafletOutput("newplot"), leafletOutput("newplot2"), leafletOutput("newplot3")))
        ), 
  hr(style = "margin-bottom:0.3vh; margin-top:0.5vh")),
    conditionalPanel("input.CPP != 'Select a CPP'",fluidRow(
    splitLayout(cellWidths = c("33%", "33%", "33%"),
    h5("% Aged 16-64 Receiving Out of Work Benefits"), h5("Number of SIMD Crimes per 10,000 People"), h5("Emergency Admissions (65+) per 100,000 People"))
  )
  ,  fluidRow(
     splitLayout(cellWidths = c("33%", "33%", "33%"),
     leafletOutput("newplot4"), leafletOutput("newplot5"), leafletOutput("newplot6")))
    )
)
)))