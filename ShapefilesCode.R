#------------------Create shapefiles for Cpop maps ---------------
library(readr)
library(readxl)
library(dplyr)
#read shapefiles
SpPolysDF<-read_rds("Q:/Codes/Shiny -Leaflet/LeafletPolygons.rds")

#read higher geography data
dtaGeoHigher <- read_csv(file.path("S:","G - Governance & Performance Mngmt",
        "Research Team","Salaries-Analysis","RStudio-Salaries", 
        "data", "csv", "DZs to Higher Geos.csv"))[c(1,3)]
#merge
SpPolysDF@data <- left_join(SpPolysDF@data, dtaGeoHigher, by = c("group" = "DATAZONE"))

#If IZs begin with IZ then add Council name to the end
SpPolysDF@data[grep("IZ",SpPolysDF$INTZONE_NAME), 10] <- paste(SpPolysDF@data[grep("IZ",SpPolysDF$INTZONE_NAME), 10], SpPolysDF@data[grep("IZ",SpPolysDF$INTZONE_NAME), 7], sep = " ")
#Search for other duplicates
dups <- unique(SpPolysDF@data[c(7,10)])
dups <- dups[duplicated(dups$INTZONE_NAME),2]
#Where duplicated add abbreviated Council Name
SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10] <- paste(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10], abbreviate(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 7],6), sep = " ")

#Read the data zone indicator data
indDta <- read_excel("Q:/CMaps/datazone data for maps.xlsx")
SpPolysDF@data <- left_join(SpPolysDF@data, indDta[c(1,4,5,6,7,8,9)], by = c("group" = "Datazone"))
  
#save
saveRDS(SpPolysDF, file = "Q:/CMaps/Shapes.rds")
