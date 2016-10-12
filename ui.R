library(shinythemes)

ui <- navbarPage(title = "CPOP Mapping Feature",selected = "Data Zone Level Maps",theme = "bootstrap.css",
   tabPanel("Data Zone Level Maps", 
            #CSS to make sure that there is no scroll even on smaller screens
            tags$head(tags$style(".leaflet{height:38vh !important;border-style: solid;border-width:1px}",
                                 ".well {background-color:white; padding-bottom:0px; height:0vh;}",
                                 ".row-fluid {padding-top:7vh;}",
                                 ".span4 {display: inline-block; vertical-align: text-top; width: 35vh;}",
                                 "#communityMap{height:85vh !important;border-style:solid;border-width:1px}",
                                 HTML("
                                            h5 {
                                            height: 18px;
                                            margin-top:2px;
                                            margin-bottom:0px;
                                            text-align:center;
                                            font-weight: bold
                                            }"))), 
            fluidPage(
            absolutePanel(fixed = FALSE,
                           draggable = FALSE, top = "28px", left = 0, right = 0, bottom = 0,
                           width = "100%", height = "0px", style = "opacity:1",
                           wellPanel(
                              div(class = "row",
                                              div(class = "span4",style = "padding-right:6vh; padding-left:6vh", 
                                                  selectizeInput("CPP", h5("Select a CPP"), 
                                                 choices = unique(CPPdta$council), options = list(placeholder = "Select a CPP",
                                                     onInitialize = I('function() { this.setValue(""); }')))
                                                       ),
                                    div(class = "span4", style = "width: 45vh",
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
),
  tabPanel("Community Ranking",
           fluidPage(
             absolutePanel(fixed = FALSE, draggable = FALSE, top = "28px", left = 0, right = 0,
                           bottom = 0, width = "100%", height = "0px", 
                           wellPanel(div(class = "span4",style = "padding-left:6vh", selectInput("CPPIZ", h5("Select a CPP"), unique(CPPdta$council), width = "350px")))),
           fluidRow(div(class = "row-fluid",id = "comMap", leafletOutput("communityMap")))
           ))

)