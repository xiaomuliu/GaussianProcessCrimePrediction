require(spacetime)

ConstructArealSTData <- function(CrimeData,spData,Crd,area=c("BEAT","DISTRICT")){
  # Construct a spatio-temporal data frame (of form STFDF) that is derived from a spatial polygon data frame template (spData)
  # and contains the crime data info according to the area numbers
  
  if (area=="BEAT"){areaNo <- "BEAT_NUMBE"}
  else if (area=="DISTRICT"){areaNo <- "DISTRICT"}
  # *************************************************************** #
  # Resolve the mismatch issue by removing all the mismatched records.
  # If the crime data has already been matched the new distirct/beat map, this part should make no change.
  #   aNo1 <- as.numeric(sort(as.character(unique(spData@data[[areaNo]]))))
  #   aNo2 <- as.numeric(as.character(unique(CrimeData[[area]])))
  #   miss <- numeric()
  #   for (i in aNo2){
  #     if (length(which(aNo1==i)) == 0){
  #       miss <- c(miss,i)
  #     }
  #   }
  #   
  #   rm_idx <- numeric()
  #   for (i in miss){
  #     k = which(as.numeric(as.character(CrimeData[[area]])) == i)
  #     rm_idx = c(rm_idx,k)
  #   }
  #   if (length(rm_idx)!=0){
  #     CrimeData <- CrimeData[-rm_idx,]
  #   } 
  #   CrimeData[[area]] <- as.factor(CrimeData[[area]] )
  #   CrimeData[[area]] <- factor(as.character(CrimeData[[area]]),levels=levels(spData@data[[areaNo]]))
  # *************************************************************** #
  
  # fill in the records (set them to be zero) for the case that no incidents in a area for a centain day. 
  # Expand the corresponding column (DOW and Month) so that the data becomes 
  # a "full grid" panel data (for every time unit (day), it has incidents for all the areas)
  # which will be tranform to STFDF
  
  AreaList <- sort(unique(spData@data[[areaNo]]))
  DateList <- aggregate(.~DATEOCC,data=subset(CrimeData,select=c("DATEOCC","YEAR","MONTH","DOW","HOLIDAY")),FUN=function(x){x[1]})
  DateList$DOW <- factor(DateList$DOW)
  levels(DateList$DOW) <- levels(CrimeData$DOW)
  #   levels(DateList$DOW) <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
  DateList$HOLIDAY <- factor(DateList$HOLIDAY)
  levels(DateList$HOLIDAY) <- levels(CrimeData$HOLIDAY)
  AREAfull.df <- data.frame(do.call("rbind", replicate(length(AreaList), DateList, simplify=FALSE)),AREA=rep(AreaList,times=nrow(DateList)))
  names(AREAfull.df)[names(AREAfull.df)=="AREA"] <- area  
  CrimeData <- merge(AREAfull.df,CrimeData,all=TRUE)
  CrimeData <- CrimeData[order(CrimeData$DATEOCC),]
  CrimeData$INC_CNT[is.na(CrimeData$INC_CNT)] = 0
  CrimeData$MONTH <- factor(CrimeData$MONTH)
  
  
  area.loc <- data.frame(AREA=spData@data[[areaNo]], X_COORD=Crd[,1], Y_COORD=Crd[,2])
  names(area.loc)[names(area.loc)=="AREA"] <- area
  area.loc <- area.loc[match(CrimeData[[area]],area.loc[[area]]),]
  
  areaCentroids <- coordinates(area.loc[,c("X_COORD","Y_COORD")])
  rownames(areaCentroids) <- area.loc[[area]]
  
  areaCentroids <- SpatialPoints(areaCentroids,CRS(proj4string(spData)))
  
  CrimeData.stfdf <- stConstruct(subset(CrimeData,select=c(area,"DATEOCC","INC_CNT")),space=area,
                                 time="DATEOCC", SpatialObj=areaCentroids)
  return(list(CrimeData.stfdf=CrimeData.stfdf,CrimeData=CrimeData))
}

ConstructSTData <- function(CrimeData,spData,Crd,area=c("BEAT","DISTRICT")){
  # create a spatio-temporal data frame of which the spatial locations are centroids 
  # and the data is from CrimeData
  if (area=="BEAT"){areaNo <- "BEAT_NUMBE"}
  else if (area=="DISTRICT"){areaNo <- "DISTRICT"}
  
  area.loc <- data.frame(AREA=spData@data[[areaNo]], X_COORD=Crd[,1], Y_COORD=Crd[,2])
  names(area.loc)[names(area.loc)=="AREA"] <- area
  area.loc <- area.loc[match(CrimeData[[area]],area.loc[[area]]),]
  
  areaCentroids <- coordinates(area.loc[,c("X_COORD","Y_COORD")])
  rownames(areaCentroids) <- area.loc[[area]]
  
  areaCentroids <- SpatialPoints(areaCentroids,CRS(proj4string(spData)))
  
  CrimeData.stfdf <- stConstruct(CrimeData,select,space=area,time="DATEOCC", SpatialObj=areaCentroids)
  return(CrimeData.stfdf)
}