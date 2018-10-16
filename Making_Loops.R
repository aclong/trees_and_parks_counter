AverageIVEList <- data.frame("id" = 128:145)

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
  print(AverageIVE)
  print(1)
  
  #fill the dataframe created earlier with these results to attach to the wards later 
  AverageIVEList$Average_IVE[j] <- AverageIVE
}


