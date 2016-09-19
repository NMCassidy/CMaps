require(dplyr)        #Data manipulation
require(RColorBrewer) #For colour scheme
require(sp)           #To deal with shapefiles
library(shiny)

shinyServer(
  function(input, output, session){
    
    output$IZ <- renderUI({
      opts <- selectInput("IZ", "Select a Community", unique(CPPdta[CPPdta$council == input$CPP, 10]))
    })
    
    clrs<-brewer.pal(7, "RdYlGn")
    povPal <- colorBin(rev(clrs), SpPolysDF@data$`% of children in poverty`)
    tariffPal <- colorBin(clrs, SpPolysDF@data$`S4 Average tariff score`)
    posPal <- colorBin(clrs, SpPolysDF@data$`Percentage of school leavers entering positive destinations`)
    benPal <- colorBin(rev(clrs), SpPolysDF@data$` % of population (aged 16-64) in receipt of out of work benefits`)
    crimePal <- colorBin(rev(clrs), SpPolysDF@data$`Number of SIMD crimes per 10,000 of the population`)
    admisPal <- colorBin(rev(clrs), SpPolysDF@data$`Emergency admissions (65+) per 100,000 population`)
    
    plydata<-reactive({
        desIZ<- which(CPPdta$INTZONE_NAME %in% input$IZ)
        IZ_dzs<-SpPolysDF[desIZ,]
    })
    
    #create the map
    output$newplot<-renderLeaflet({
       p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~povPal(`% of children in poverty`), color = "black")
      return(p)
    })
    output$newplot2<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~tariffPal(`S4 Average tariff score`),  color = "black")
      return(p)
    })
    output$newplot3<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~posPal(`Percentage of school leavers entering positive destinations`), color = "black")
      return(p)
    })
    output$newplot4<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~benPal(` % of population (aged 16-64) in receipt of out of work benefits`), color = "black")
      return(p)
    })
    output$newplot5<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~crimePal(`Number of SIMD crimes per 10,000 of the population`), color = "black")
      return(p)
    })
    output$newplot6<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~admisPal(`Emergency admissions (65+) per 100,000 population`), color = "black")
      return(p)
    })
    
    ##Clickable popups for map1
      showDZPopup <- function(group, lat, lng) {
      selectedDZ <- CPPdta[CPPdta$group == group,]
      content <- as.character(tagList(
        tags$h4(as.character(unique(selectedDZ$group))),
        sprintf("%s: %s",
                "Children in Poverty (%)", round(unique(selectedDZ[11]),2)), tags$br()
            ))
      leafletProxy("newplot") %>% addPopups(lng, lat, content, layerId = group)
      }
      
      #Makes the popups appear and clears old popups
      observe({
        leafletProxy("newplot") %>% clearPopups()
        event <- input$newplot_shape_click
        if (is.null(event))
          return()
        isolate({
          showDZPopup(event$id, event$lat, event$lng)
        })
      })
      
      ##Clickable popups for map2
      showDZPopup2 <- function(group, lat, lng) {
        selectedDZ <- CPPdta[CPPdta$group == group,]
        content <- as.character(tagList(
          tags$h4(as.character(unique(selectedDZ$group))),
          sprintf("%s: %s\n",
                  "Tariff Score", round(unique(selectedDZ[12]),2)), tags$br()
        ))
        leafletProxy("newplot2") %>% addPopups(lng, lat, content, layerId = group)
      }
      
      #Makes the popups appear and clears old popups
      observe({
        leafletProxy("newplot2") %>% clearPopups()
        event <- input$newplot2_shape_click
        if (is.null(event))
          return()
        isolate({
          showDZPopup2(event$id, event$lat, event$lng)
        })
      })
      ##Clickable popups for map2
      showDZPopup3 <- function(group, lat, lng) {
        selectedDZ <- CPPdta[CPPdta$group == group,]
        content <- as.character(tagList(
          tags$h4(as.character(unique(selectedDZ$group))),
          sprintf("%s: %s\n",
                  "Positive Destinations (%)", round(unique(selectedDZ[13]),2)), tags$br()
        ))
        leafletProxy("newplot3") %>% addPopups(lng, lat, content, layerId = group)
      }
      
      #Makes the popups appear and clears old popups
      observe({
        leafletProxy("newplot3") %>% clearPopups()
        event <- input$newplot3_shape_click
        if (is.null(event))
          return()
        isolate({
          showDZPopup3(event$id, event$lat, event$lng)
        })
      })
      ##Clickable popups for map2
      showDZPopup4 <- function(group, lat, lng) {
        selectedDZ <- CPPdta[CPPdta$group == group,]
        content <- as.character(tagList(
          tags$h4(as.character(unique(selectedDZ$group))),
          sprintf("%s: %s\n",
                  "Out of Work Benefits (%)", round(unique(selectedDZ[14]),2)), tags$br()
        ))
        leafletProxy("newplot4") %>% addPopups(lng, lat, content, layerId = group)
      }
      
      #Makes the popups appear and clears old popups
      observe({
        leafletProxy("newplot4") %>% clearPopups()
        event <- input$newplot4_shape_click
        if (is.null(event))
          return()
        isolate({
          showDZPopup4(event$id, event$lat, event$lng)
        })
      })
      ##Clickable popups for map2
      showDZPopup5 <- function(group, lat, lng) {
        selectedDZ <- CPPdta[CPPdta$group == group,]
        content <- as.character(tagList(
          tags$h4(as.character(unique(selectedDZ$group))),
          sprintf("%s: %s\n",
                  "SIMD Crimes per 10,000", round(unique(selectedDZ[15]),2)), tags$br()
        ))
        leafletProxy("newplot5") %>% addPopups(lng, lat, content, layerId = group)
      }
      
      #Makes the popups appear and clears old popups
      observe({
        leafletProxy("newplot5") %>% clearPopups()
        event <- input$newplot5_shape_click
        if (is.null(event))
          return()
        isolate({
          showDZPopup5(event$id, event$lat, event$lng)
        })
      })
      ##Clickable popups for map2
      showDZPopup6 <- function(group, lat, lng) {
        selectedDZ <- CPPdta[CPPdta$group == group,]
        content <- as.character(tagList(
          tags$h4(as.character(unique(selectedDZ$group))),
          sprintf("%s: %s\n",
                  "Emergency Admissions per 100,000", round(unique(selectedDZ[16]),2)), tags$br()
        ))
        leafletProxy("newplot6") %>% addPopups(lng, lat, content, layerId = group)
      }
      
      #Makes the popups appear and clears old popups
      observe({
        leafletProxy("newplot6") %>% clearPopups()
        event <- input$newplot6_shape_click
        if (is.null(event))
          return()
        isolate({
          showDZPopup6(event$id, event$lat, event$lng)
        })
      })
      
  })