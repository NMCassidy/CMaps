require(leaflet)      #To make the Maps
library(readr)
SpPolysDF<-read_rds("Q:/CMaps/Shapes.rds")

CPPdta <- SpPolysDF@data