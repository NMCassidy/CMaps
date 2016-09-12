#------------------Create shapefiles for Cpop maps ---------------
library(readr)
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
#save
saveRDS(SpPolysDF, file = "Q:/CMaps/Shapes.rds")
