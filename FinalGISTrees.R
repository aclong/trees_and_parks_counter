#Here is the entire tool. If you want to use the API to calculate all the IVE Scores you can
#I have also save a copy as a csv that can be picked up later in the sript.

#I have also saved a full copy of the global environment should you wich to use that
#though it doesn't need it

wd <- getwd()

DataFolder <- paste(wd, "/data", sep = "")

graphopper_API <- Sys.getenv("ghAPI")

#libraries
library(tidyverse)
library(maptools)
library(RColorBrewer)
library(classInt)
library(OpenStreetMap)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(spatstat)
library(GISTools)
library(readr)
library(stplanr)

#load in BNG
UKBNG <- "+init=epsg:27700"

#Tree Data
TreeData <- read.csv(paste(DataFolder, "/Borough_tree_list_may16.csv", sep = ""))
#Turn csv into datapoints
TreeDataSP <- SpatialPointsDataFrame(TreeData[,6:7], TreeData, proj4string = CRS(UKBNG))

#Green Open Space Data
PublicOpenSpace <- readOGR(paste(DataFolder, "/POS_and_AoD/London_POS_region.shp", sep = ""))
#transform to BNG
PublicOpenSpaceBNG <- spTransform(PublicOpenSpace,UKBNG)

#Access to open space data
AccesstoOpenSpace <- read.csv(paste(DataFolder, "/Access to Public Space/public-open-space-nature-ward_access-to-nature_general.csv", sep = ""))
AccesstoOpenSpaceComplex <- read.csv(paste(DataFolder, "/Access to Public Space/access-public-open-space-nature-ward_each_by_POS-type.csv", sep = ""))
CamdenAccessPOSComplex <- AccesstoOpenSpaceComplex[grep("E09000007", AccesstoOpenSpaceComplex$Borough.Code), ]
CamdenAccessPOS <- AccesstoOpenSpace[grep("E09000007", AccesstoOpenSpace$Borough.Code), ]

#Ward shape files
LondonWards <- readOGR(paste(DataFolder, "/LondonWardsBoundaries/LondonWardsNew.shp", sep = ""))
#transform to BNG
LondonWardsBNG <- spTransform(LondonWards,UKBNG)

#Borough Boundaries
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")

#Choose only Camden Borough boundary
CamdenMap <- EW[grep("^E09000007",EW@data$lad15cd),]
#transform to BNG
CamdenMapBNG <- spTransform(CamdenMap,UKBNG)

#Use Camden Borough boundary to clip other data sets
#clip all london wards outside of camden
CamdenWards <- LondonWardsBNG[grep("^00AGG", LondonWardsBNG@data$WD11CDO),]

#need a WGS84 version for the API
CamdenWardsWGS84 <- LondonWards[grep("^00AGG", LondonWardsBNG@data$WD11CDO),]

CamdenWardsWGS84 <-spTransform(CamdenWards, CRS("+proj=longlat +datum=WGS84"))

#clip all trees outside of camden
CamdenTreeData <- TreeDataSP[CamdenMapBNG, ]

#clip all open space data outside of camden
CamdenPublicOpenSpace <- PublicOpenSpaceBNG[CamdenMapBNG, ]

#set tm_view to interactive
tmap_mode("view")

#view the map
tm_shape(CamdenWards) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(CamdenTreeData) +
  tm_dots(col = "green") +
  tm_shape(CamdenPublicOpenSpace) +
  tm_polygons(col = "blue", alpha = 0.5)

#functions for working out
#for CalculateNewRoute the StartCoords and EndCoords must be WGS84 coords in the format "lon, lat" (string)
CalculateNewRoute <- function(StartCoords, EndCoords) {
  NewRoute <- route_graphhopper(StartCoords, EndCoords, vehicle = "foot", pat = graphopper_API, base_url = "https://graphhopper.com")
  NewRoute <- spTransform(NewRoute, UKBNG)
  return(NewRoute)
}

#for CalculateIVE the route must be a linestring in BNG coordinates, ideally it should all be within the camden wards as tree/openspace data for outside of this area is not used
CalculateIVE <- function(NewRoute){
  NewRouteBuffer <- gBuffer(NewRoute, width = 2)
  CamdenTreeDataSF <- st_as_sf(CamdenTreeData)
  CamdenPublicOpenSpaceSF <- st_as_sf(CamdenPublicOpenSpace)
  NewRouteBufferSF <- st_as_sf(NewRouteBuffer)
  NewRouteSF <- st_as_sf(NewRoute)
  TreeCount <- st_intersects(CamdenTreeDataSF[NewRouteBufferSF, ], NewRouteBufferSF, sparse = FALSE)
  ParkCount <- st_intersects(CamdenPublicOpenSpaceSF[NewRouteBufferSF, ], NewRouteBufferSF, sparse = FALSE)
  IVETotal <- length(TreeCount) + 3*(length(ParkCount))
  IVEPerM <- IVETotal/line_length(NewRoute)
  print(class(IVEPerM))
  return(IVEPerM)
}

#create dataframe to store results of route/IVE calculation
AverageIVEList <- data.frame("id" = 128:145)

#testroute to ensure API is working
TestRoute <- CalculateNewRoute("51.5738, -0.1859", "51.53394, -0.13711")

#for loop to created walking routes that cross each of the wards in camdenwards

for(j in 1:length(CamdenWardsWGS84@data$OBJECTID)){
  print(j)

  #use OBJECTID to select the ward to be used
  i <- CamdenWardsWGS84@data$OBJECTID[j]
  print(i)
  ThisWard <- CamdenWardsWGS84[CamdenWardsWGS84@data$OBJECTID==i,]

  #creat start and end coordinates from the bbox of each ward
  #also paste them together in the format needed for the graphhopper API
  CoordsNW <- paste(ThisWard@bbox[2,2],",", ThisWard@bbox[1,1], sep = "")
  CoordsSE <- paste(ThisWard@bbox[2,1],",", ThisWard@bbox[1,2], sep = "")

  CoordsSW <- paste(ThisWard@bbox[2,2],",",ThisWard@bbox[1,2], sep = "")
  CoordsNE <- paste(ThisWard@bbox[2,1],",",ThisWard@bbox[1,1], sep = "")

  #Calculate each of the new routes
  NewRouteA <- CalculateNewRoute(CoordsNW, CoordsSE)

  NewRouteB <- CalculateNewRoute(CoordsSW, CoordsNE)

  #use the CalculateIVE function to find a score for each route
  NewIVEA <- CalculateIVE(NewRouteA)

  NewIVEB <- CalculateIVE(NewRouteB)

  #average the two routes to find a vague IVE score for each ward
  AverageIVE <- ((NewIVEA + NewIVEB)/2)

  #fill the dataframe created earlier with these results to attach to the wards later
  AverageIVEList$Average_IVE[j] <- AverageIVE
}

#I have saved a previous list as csv in case you don't want to create a new one with the API
write.csv(AverageIVEList, paste(DataFolder, "/AverageIVE.csv", sep = ""))

#load in saved csv of AverageIVE here if you haven't created a new one
AverageIVEList <- read.csv(paste(DataFolder, "/AverageIVE.csv", sep = ""))

#Find min, max and avg IVE score to compare to user input route

MinIVE <- min(AverageIVEList[ ,"Average_IVE"])

MaxIVE <- max(AverageIVEList[ ,"Average_IVE"])

MeanIVE <- mean(AverageIVEList[ ,"Average_IVE"])

#Append list to CamdenWards
CamdenWards@data <- data.frame(CamdenWards@data,AverageIVEList[match(CamdenWards@data[,"OBJECTID"],AverageIVEList[,"id"]),])

#Add Access to open sapce data to CamdenWards
CamdenWards@data <- data.frame(CamdenWards@data, CamdenAccessPOS[match(CamdenWards@data[,"WD11CD"], CamdenAccessPOS[,"Ward"]),])
CamdenWards@data <- data.frame(CamdenWards@data, CamdenAccessPOSComplex[match(CamdenWards@data[,"WD11CD"], CamdenAccessPOSComplex[,"Ward"]),])

#User Input Route

UserStartCoordsA <- "51.522622, -0.136346"
UserEndCoordsA <- "51.555999, -0.145973"

UserStartCoordsB <- "51.548249, -0.177642"
UserEndCoordsB <- "51.537714, -0.134746"

UserStartCoordsC <- "51.535494, -0.157181"
UserEndCoordsC <- "51.527622, -0.130775"

#Change the valuse in CalculateNew Route to any of the above to calculate different routes and IVE Scores
TestRoute <- CalculateNewRoute("51.5738, -0.1859", "51.53394, -0.13711")

#Calculate IVE Score
TestRouteIVE <- CalculateIVE(TestRoute)

#Add IVE to TestRoute@data
TestRoute@data$IVE_Score <- TestRouteIVE

#plot map of CamdenWards with TestRoute
TestRouteMap <- tm_shape(CamdenWards) +
                  tm_polygons(col = NA, alpha = 0.5) +
                tm_shape(CamdenTreeData) +
                  tm_dots(col = "green") +
                tm_shape(CamdenPublicOpenSpace) +
                  tm_polygons(col = "blue", alpha = 0.3) +
                tm_shape(TestRoute) +
                  tm_lines(col = "red")

tm_view(TestRouteMap)

#showing IVE rating for TestRoute, alongside average, min and max

#Print out information regarding the map
print(paste("The IVE score for this route is ", TestRoute@data$IVE_Score, ", the mean for the borough is ", MeanIVE, ", the smallest is ", MinIVE, ", and the largest is ", MaxIVE, ".", sep = ""))
