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


##Search for other duplicates
#dups <- unique(SpPolysDF@data[c(7,10)])
#dups <- dups[duplicated(dups$INTZONE_NAME),2]
##Where duplicated add abbreviated Council Name
#SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10] <- paste(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10], abbreviate(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 7],6), sep = " ")

#Read the data zone indicator data
indDta <- read_excel("Q:/CMaps/datazone data for maps.xlsx")
SpPolysDF@data <- left_join(SpPolysDF@data, indDta[c(1,3,4,5,6,7,8,9)], by = c("group" = "Datazone"))
#remove old Intzone name column
SpPolysDF@data$INTZONE_NAME <- SpPolysDF@data$IGZ
SpPolysDF@data <- select(SpPolysDF@data, -IGZ)
#If IZs begin with IZ then add Council name to the end
SpPolysDF@data[grep("IZ",SpPolysDF$INTZONE_NAME), 10] <- paste(SpPolysDF@data[grep("IZ",SpPolysDF$INTZONE_NAME), 10], SpPolysDF@data[grep("IZ",SpPolysDF$INTZONE_NAME), 7], sep = " ")
#Look for duplicates
dups <- unique(SpPolysDF@data[c(7,10)])
dups <- dups[duplicated(dups$INTZONE_NAME),2]
#Where duplicated add abbreviated Council Name
SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10] <- paste(SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 10], SpPolysDF@data[SpPolysDF@data$INTZONE_NAME %in% dups, 7], sep = " ")

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

##Just doing this here to be lazy
shps <- readRDS("IZshapes.rds")
shps@data$IZ_NAME <- as.character(shps@data$IZ_NAME)
shps@data[grep("IZ ",shps@data$IZ_NAME),2] <- shps@data[grep("IZ ",shps@data$IZ_NAME),6]
saveRDS(shps,"IZshapes.rds")
#Also need to do it for datazones
shps <- readRDS("Shapes.rds")
shps@data[shps@data$INTZONE_NAME == "IZ Seventeen East Lothian", 10] <- "Longniddry and Aberlady"
shps@data[shps@data$INTZONE_NAME == "IZ One East Lothian", 10] <- "Haddington Rural"
shps@data[shps@data$INTZONE_NAME == "IZ Two East Lothian", 10] <- "Ormiston"
shps@data[shps@data$INTZONE_NAME == "IZ Three East Lothian", 10] <- "Whitecraig"
shps@data[shps@data$INTZONE_NAME == "IZ Four East Lothian", 10] <- "Tranent South"
shps@data[shps@data$INTZONE_NAME == "IZ Five East Lothian", 10] <- "Musselburgh South"
shps@data[shps@data$INTZONE_NAME == "IZ Six East Lothian", 10] <- "Musselburgh West"
shps@data[shps@data$INTZONE_NAME == "IZ Seven East Lothian", 10] <- "Musselburgh East"
shps@data[shps@data$INTZONE_NAME == "IZ Eight East Lothian", 10] <- "Tranent North"
shps@data[shps@data$INTZONE_NAME == "IZ Nine East Lothian", 10] <- "Musselburgh North"
shps@data[shps@data$INTZONE_NAME == "IZ Ten East Lothian", 10] <- "Wallyford"
shps@data[shps@data$INTZONE_NAME == "IZ Eleven East Lothian", 10] <- "Haddington South"
shps@data[shps@data$INTZONE_NAME == "IZ Twelve East Lothian", 10] <- "Haddington North"
shps@data[shps@data$INTZONE_NAME == "IZ Thirteen East Lothian", 10] <- "Prestonpans South"
shps@data[shps@data$INTZONE_NAME == "IZ Fourteen East Lothian", 10] <- "Prestonpans North"
shps@data[shps@data$INTZONE_NAME == "IZ Fifteen East Lothian", 10] <- "East Linton"
shps@data[shps@data$INTZONE_NAME == "IZ Sixteen East Lothian", 10] <- "Cockenzie and Port Seton"
shps@data[shps@data$INTZONE_NAME == "IZ Eighteen East Lothian", 10] <- "Dunbar East"
shps@data[shps@data$INTZONE_NAME == "IZ Twenty Two East Lothian", 10] <- "North Berwick North"
shps@data[shps@data$INTZONE_NAME == "IZ Nineteen East Lothian", 10] <- "Dunbar West"
shps@data[shps@data$INTZONE_NAME == "IZ Twenty East Lothian", 10] <- "Gullane and Drem"
shps@data[shps@data$INTZONE_NAME == "IZ Twenty One East Lothian", 10] <- "North Berwick South"
saveRDS(shps,"shapes.rds")
