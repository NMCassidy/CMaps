require(leaflet)      #To make the Maps
library(readr)
library(dplyr)
library(shiny)
SpPolysDF<-read_rds("Shapes.rds")
SpPolysIZ <- read_rds("IZShapes.rds")

CPPdta <- SpPolysDF@data

#Calculate percentiles for colours
povDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 11], 7)
  povDecs <-c(povDecs,x)
}

tariffDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 12], 7)
  tariffDecs <-c(tariffDecs,x)
}

posDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 13], 7)
  posDecs <-c(posDecs,x)
}

benDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 14], 7)
  benDecs <-c(benDecs,x)
}

crimeDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 15], 7)
  crimeDecs <-c(crimeDecs,x)
}

admisDecs <- c()
for(i in unique(SpPolysDF@data$council)){
  x <- ntile(SpPolysDF@data[SpPolysDF@data$council == i, 16], 7)
  admisDecs <-c(admisDecs,x)
}

SpPolysDF@data <- cbind(SpPolysDF@data, povDecs, tariffDecs, posDecs,benDecs,crimeDecs, admisDecs)
rm(i, povDecs, tariffDecs, posDecs,benDecs,crimeDecs, admisDecs)