require(RColorBrewer) #For colour scheme
require(sp)           #To deal with shapefiles

server <-  function(input, output, session){
    
    output$IZUI <- renderUI({
        selectInput("IZ", h5("Select a Community"), unique(CPPdta[CPPdta$council == input$CPP, 10]))
    })
    
    clrs<-brewer.pal(7, "RdYlGn")
    povPal <- colorBin(rev(clrs), SpPolysDF@data$povDecs)
    tariffPal <- colorBin(clrs, SpPolysDF@data$tariffDecs)
    posPal <- colorBin(clrs, SpPolysDF@data$posDecs)
    benPal <- colorBin(rev(clrs), SpPolysDF@data$benDecs)
    crimePal <- colorBin(rev(clrs), SpPolysDF@data$crimeDecs)
    admisPal <- colorBin(rev(clrs), SpPolysDF@data$admisDecs)
    
    plydata<-reactive({
        desIZ<- which(CPPdta$INTZONE_NAME %in% input$IZ)
        IZ_dzs<-SpPolysDF[desIZ,]
    })
    
    #create the map
    output$newplot<-renderLeaflet({
       p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~povPal(`povDecs`), color = "black")
      return(p)
    })
    output$newplot2<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~tariffPal(`tariffDecs`),  color = "black")
      return(p)
    })
    output$newplot3<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~posPal(`posDecs`), color = "black")
      return(p)
    })
    output$newplot4<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~benPal(`benDecs`), color = "black")
      return(p)
    })
    output$newplot5<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~crimePal(`crimeDecs`), color = "black")
      return(p)
    })
    output$newplot6<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~admisPal(`admisDecs`), color = "black")
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
      
      #Colours for Community Map
      communityPal <- colorBin(rev(clrs), SpPolysIZ@data$rank_decs)
      
      #Subset IZ Data
      IZPlys <- reactive({
        sbst <- which(SpPolysIZ@data$council %in% input$CPPIZ)
        dt <- SpPolysIZ[sbst,]
      })
      
      #Create Community Map
      output$communityMap <- renderLeaflet({
        cp <- leaflet(IZPlys()) %>%
          addTiles() %>%
          addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                  layerId = ~IZ_CODE, fillColor = ~communityPal(`rank_decs`), color = "black")
      })
      #Add click function
        showIZPopup <- function(group, lat, lng){
        selectedIZ <- SpPolysIZ@data[SpPolysIZ@data$IZ_CODE == group,]
        content <- as.character(tagList(
          tags$h4(as.character(unique(selectedIZ$IZ_NAME))),
          paste("Intermediate Geography Ranking:", as.character(unique(selectedIZ[8]))),
          tags$br()
        ))
        leafletProxy("communityMap") %>% addPopups(lng, lat, content, layerId = group)
        }
        #Make popup appear and clear old popups
        observe({
          leafletProxy("communityMap") %>% clearPopups()
          event <- input$communityMap_shape_click
          if(is.null(event)){
            return()}
          isolate({
            showIZPopup(event$id, event$lat, event$lng)
          })
          
        })
  }