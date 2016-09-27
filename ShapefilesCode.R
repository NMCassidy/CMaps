#------------------Create shapefiles for Cpop maps ---------------
library(readr)
library(readxl)
library(dplyr)
library(rgdal)
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


## Intermediate Geography Shapes
#Download links are available at http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/StatisticalUnits/SG_StatisticalUnits.atom.en.xml
#download, read, sort projections
download.file("http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_IntermediateZoneBdry_2001.zip", "Q:/CMaps/IZBounds.zip")
###unzipped manually
SpPolysIZ <- readOGR(file.path("Q:", "CMaps", "IZBounds"), "SG_IntermediateZone_Bdry_2001")
proj4string(SpPolysIZ)
SpPolysIZ <- spTransform(SpPolysIZ, CRS("+proj=longlat +datum=WGS84 +no_defs"))
#Read CPOP data, merge with shapes, clean the ranks into LA "sevenths" (septiles?)
CPdta <- read_excel("Q:/CMaps/ranks for igzs.xlsx", sheet = 1)
SpPolysIZ@data <- left_join(SpPolysIZ@data, CPdta, by = c("IZ_CODE" = "IGZ code"))
colnames(SpPolysIZ@data)[7] <- "council"
decs <- c()
for(i in unique(SpPolysIZ@data$council)){
  x <- ntile(SpPolysIZ@data[SpPolysIZ@data$council == i, 8], 7)
  decs <- c(decs, x)
}
SpPolysIZ@data$rank_decs <- decs

SpPolysIZ@data[SpPolysIZ@data$council == "Edinburgh, City of", 7] <- "Edinburgh"

#Save 
saveRDS(SpPolysIZ, "Q:/CMaps/IZshapes.rds")
