require(sp)
require(rgeos)

DataMatching <- function(CrimeData,PolygonData){
  # matching the old beat records and the new one by finding which new beat polygon the point falls in
  # and then re-assign that beat number to that crime record. 
  # Therefore all crime beat records have an unified reference which is the new beat map.
  
  PolygonData.temp <- PolygonData
  PolygonData.temp@data$BEAT_NUMBE <- as.factor(as.integer(as.character(PolygonData.temp@data$BEAT_NUMBE)))
  
  # Remove instances that have NA values (beat-wise)
  CrimeData <- CrimeData[!is.na(CrimeData$BEAT),]
  
  BeatList <- sort(unique(PolygonData.temp@data$BEAT_NUMBE))
  Nbeat <- length(PolygonData.temp@polygons)
  Ncrime <- nrow(CrimeData)
  
  # check if a point is inside a given polygon
  PtsIn <- rep(NA,Ncrime)
  Inside <- matrix(NA,nrow=Ncrime,ncol=Nbeat)
  for (i in 1:Nbeat){
    poly.coords <- PolygonData.temp@polygons[[i]]@Polygons[[1]]@coords
    Inside[,i] <- point.in.polygon(CrimeData$X_COORD, CrimeData$Y_COORD,
                                   poly.coords[,1], poly.coords[,2], mode.checked=FALSE)
  }
  
  for (i in 1:Ncrime){
    inBeat <- PolygonData.temp@data$BEAT_NUMBE[Inside[i,]!=0]
    if (length(inBeat)==0){
      # can not find a beat which the point is located in
      if (any(BeatList==as.character(CrimeData$BEAT[i]))){
        PtsIn[i] <- BeatList[BeatList==as.character(CrimeData$BEAT[i])]
      }
      else{
        # assign it the nearest (in terms of centroid) beat
        # ??????
      }
    }
    # a point fallen into more than one beat (e.g. on the edges)
    else if (length(inBeat) > 1){   
      # no beat found matches the one in the data frame, 
      # pick the first one as the beat number, otherwise use the one in the data frame
      matchIdx <- which(inBeat == as.character(CrimeData$BEAT[i]))
      if (length(matchIdx)==0){
        PtsIn[i] <- inBeat[1]
      }
      else{
        PtsIn[i] <- inBeat[matchIdx]
      }
    }
    else {
      PtsIn[i] <- inBeat
    }
  }
  
  CrimeData$BEAT<- factor(PtsIn)
  levels(CrimeData$BEAT) <- levels(PolygonData@data$BEAT_NUMBE)
  
  return(list(CrimeData=CrimeData,PolygonData=PolygonData.temp))
}



DataMatching2 <- function(CrimeData,PolygonData,area=c("BEAT","DISTRICT")){
  # matching the old beat/district records and the new one by finding which new beat/district polygon the point falls in
  # and then re-assign that beat/district number to that crime record. 
  # Therefore all crime beat/district records have an unified reference which is the new beat/district map.
  
  if (area=="BEAT"){areaNo <- "BEAT_NUMBE"}
  else if (area=="DISTRICT"){areaNo <- "DISTRICT"}
  
  PolygonData.temp <- PolygonData
  PolygonData.temp@data[[areaNo]] <- as.factor(as.integer(as.character(PolygonData.temp@data[[areaNo]])))
  
#   # Remove instances that have NA values (beat/district-wise)
#   CrimeData <- CrimeData[!is.na(CrimeData[[area]]),]
  
  AreaList <- sort(unique(PolygonData.temp@data[[areaNo]]))
  Narea<- length(PolygonData.temp@polygons)
  Ncrime <- nrow(CrimeData)
  Crd <- coordinates(PolygonData.temp)  

  # check if a point is inside a given polygon
  PtsIn <- rep(NA,Ncrime)
  Inside <- matrix(NA,nrow=Ncrime,ncol=Narea)
  for (i in 1:Narea){
    poly.coords <- PolygonData.temp@polygons[[i]]@Polygons[[1]]@coords
    Inside[,i] <- point.in.polygon(CrimeData$X_COORD, CrimeData$Y_COORD,
                                   poly.coords[,1], poly.coords[,2], mode.checked=FALSE)
  }
  
  for (i in 1:Ncrime){
    inArea <- PolygonData.temp@data[[areaNo]][Inside[i,]!=0]
    if (length(inArea)==0){
      # can not find an area which the point is located in
      if ( !is.na(CrimeData[[area]][i]) & any(AreaList==as.character(CrimeData[[area]][i])) ){
        PtsIn[i] <- AreaList[AreaList==as.character(CrimeData[[area]][i])]
      }
      else if (area=="DISTRICT"){
        # District numbers match the first 2 digit of beat numbers
        PtsIn[i] <- AreaList[AreaList==as.numeric(substr(as.character(CrimeData$BEAT[i]),1,2))]
      }
      else{ 
        # assign it the nearest (in terms of centroid) beat/district
        loc <- CrimeData[i,c("X_COORD","Y_COORD")]
        P2Cdist <- rep(NA,nrow(Crd))
        for (j in 1:nrow(Crd)){
          P2Cdist[j] <- dist(rbind(as.vector(loc),Crd[j,])) 
        }
        PtsIn[i] <- PolygonData.temp@data[[areaNo]][which.min(P2Cdist)]
      }
    }
    # a point fallen into more than one beat (e.g. on the edges)
    else if (length(inArea) > 1){   
      # no beat found matches the one in the data frame, 
      # pick the first one as the area number, otherwise use the one in the data frame
      matchIdx <- which(inArea == as.character(CrimeData[[area]][i]))
      if (length(matchIdx)==0){
        PtsIn[i] <- inArea[1]
      }
      else{
        PtsIn[i] <- inArea[matchIdx]
      }
    }
    else {
      PtsIn[i] <- inArea
    }
  }
  
  CrimeData[[area]]<- factor(PtsIn)
  levels(CrimeData[[area]]) <- levels(PolygonData@data[[areaNo]])
  
  return(list(CrimeData=CrimeData,PolygonData=PolygonData.temp))
}


DataMatching3 <- function(CrimeData,PolygonData,area=c("BEAT","DISTRICT")){
  if (area=="BEAT"){areaNo <- "BEAT_NUMBE"}
  else if (area=="DISTRICT"){areaNo <- "DISTRICT"}
  
  CrimeData.sp <- SpatialPoints(coords=CrimeData[,c("X_COORD","Y_COORD")],proj4string=CRS(proj4string(PolygonData)))
  NewAreaIdx <- over(CrimeData.sp,as(PolygonData,"SpatialPolygons"))
  
  NewArea <- PolygonData@data[[areaNo]][NewAreaIdx]
  
  # deal with NA by assiging to the neareat region
  DistMat <- gDistance(CrimeData.sp[is.na(NewArea),], PolygonData, byid=TRUE)      
  closeIdx <- apply(DistMat, 2, which.min)    
  NewArea[is.na(NewArea)] <- PolygonData@data[[areaNo]][closeIdx]
    
  CrimeData[[area]] <- NewArea
  
  return(CrimeData)
}