require(dplyr)        #Data manipulation
require(RColorBrewer) #For colour scheme
require(sp)           #To deal with shapefiles
library(shiny)

shinyServer(
  function(input, output){
 
    output$IZ <- renderUI({
      CPPchoice <- input$CPP
      IZs <- unique(CPPdta[CPPdta$council %in% CPPchoice, 2])
      opts <- selectInput("IZ", "Select a Community", IZs)
    })

    clrs<-brewer.pal(10, "RdYlGn")
    pal<- colorNumeric(clrs, SpPolysDF@data$scsimd2012decile)
   
    plydata <- reactive({
        IZselect <- as.character(input$IZ)
        LAselect <- as.character(input$CPP)
        IZ_dzs<-SpPolysDF[SpPolysDF@data$INTZONE_NAME == IZselect & SpPolysDF@data$council == LAselect,]
    })
    
  
    #create the maps
    output$newplot<-renderLeaflet({
      if(!is.null(plydata())){
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~pal(scsimd2012decile), color = "black")
        return(p)
      } else{
        p<-leaflet(plydata())%>%
          addTiles()%>%
          setView(lng = -4.13, lat = 57, zoom = 7)
        return(p)
      }
    })
    output$newplot2 <- renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~pal(scsimd2012decile),  color = "black")
      if(!is.null(p)){
        return(p)
      }
    })
    output$newplot3<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~pal(scsimd2012decile), color = "black")
      if(!is.null(p)){
        return(p)
      }
    })
    output$newplot4<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~pal(scsimd2012decile), color = "black")
      if(!is.null(p)){
        return(p)
      }
    })
    output$newplot5<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~pal(scsimd2012decile), color = "black")
      if(!is.null(p)){
        return(p)
      }
    })
    output$newplot6<-renderLeaflet({
      p<-leaflet(plydata())%>%
        addTiles()%>%
        addPolygons(smoothFactor = 0.5, weight = 1.5, fillOpacity = 0.7,
                    layerId = ~group, fillColor = ~pal(scsimd2012decile), color = "black")
      if(!is.null(p)){
        return(p)
      }
    })
  })