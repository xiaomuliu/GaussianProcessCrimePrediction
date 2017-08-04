HitRate <- function(value,RasterIm,prob,CrimeActualPts){
  threshold <- quantile(value,probs=prob)
  r2p <- rasterToPolygons(clump(RasterIm>=threshold), dissolve=TRUE)
  proj4string(r2p) <- proj4string(CrimeActualPts)
  
  CrimeActualPts.inPoly_poly <- aggregate(CrimeActualPts, by=r2p, FUN=sum)
  CrimeActualPts.inPoly_pts <- CrimeActualPts[CrimeActualPts.inPoly_poly, ]
  HitRate <- nrow(CrimeActualPts.inPoly_pts@data)/nrow(CrimeActualPts)
  return(list(HitRate=HitRate,inPoly_poly=CrimeActualPts.inPoly_poly,inPoly_pts=CrimeActualPts.inPoly_pts))
}

HitRate2 <- function(Pred,prob,CrimeActualPts,r,isInPoly){
  threshold <- quantile(Pred$VALUE,probs=prob)
  selPix <- Pred$VALUE>=threshold
  
  CrimeActualPts.raster <- rasterize(CrimeActualPts[,c("X_COORD","Y_COORD")], r, CrimeActualPts$INC_CNT, fun=sum)
  
  CrimeRaster.df <- as.data.frame(CrimeActualPts.raster,xy=TRUE)
  names(CrimeRaster.df) <- c("X_COORD","Y_COORD","VALUE")
  CrimeRaster.df <- CrimeRaster.df[isInPoly,]
  CrimeRaster.df$VALUE[is.na(CrimeRaster.df$VALUE)] <- 0

  HitRate <- sum(CrimeRaster.df$VALUE[selPix])/nrow(CrimeActualPts)
  return(HitRate)
}

HitRate3 <- function(Pred,threshold,CrimeActualPts,r,isInPoly){
  selPix <- Pred$VALUE>=threshold
  
  CrimeActualPts.raster <- rasterize(CrimeActualPts[,c("X_COORD","Y_COORD")], r, CrimeActualPts$INC_CNT, fun=sum)
  
  CrimeRaster.df <- as.data.frame(CrimeActualPts.raster,xy=TRUE)
  names(CrimeRaster.df) <- c("X_COORD","Y_COORD","VALUE")
  CrimeRaster.df <- CrimeRaster.df[isInPoly,]
  CrimeRaster.df$VALUE[is.na(CrimeRaster.df$VALUE)] <- 0
  
  HitRate <- sum(CrimeRaster.df$VALUE[selPix])/nrow(CrimeActualPts)
  return(HitRate)
}


ConstrainedKDE <- function(CrimePts,grd,polygon,bandwidth,raster){
  # KDE bounded in the polygon boundary
  kernSm <- bkde2D(data.matrix(CrimePts[,c("X_COORD","Y_COORD")]), bandwidth=bandwidth, 
                   gridsize=c(grd@grid@cells.dim[1], grd@grid@cells.dim[2]), 
                   range.x=list(grd@bbox[1,],grd@bbox[2,]))
  grd.kde <- expand.grid(list(X_COORD=kernSm$x1, Y_COORD=kernSm$x2)) # a rectangular full grid
  value <- rep(NA,nrow(grd.kde))
  for (j in 1:nrow(grd.kde)){
    idx.x <- kernSm$x1==grd.kde$X_COORD[j] 
    idx.y <- kernSm$x2==grd.kde$Y_COORD[j]
    value[j] <- kernSm$fhat[idx.x,idx.y] 
  }
  
  KDE.df_full <- data.frame(X_COORD=grd.kde$X_COORD,Y_COORD=grd.kde$Y_COORD,VALUE=value)
  coordinates(KDE.df_full) = c("X_COORD", "Y_COORD") 
  proj4string(KDE.df_full) <- proj4string(polygon)
  KDE.df_full <- as(KDE.df_full,"SpatialPointsDataFrame")
  
  # bounded KDE values in the boundary
  BoundedOverFullGrd <- over(KDE.df_full, as(polygon,"SpatialPolygons"))
  KDE.sp_inPoly <- KDE.df_full
  KDE.sp_inPoly@data <- data.frame(VALUE=KDE.df_full@data[!is.na(BoundedOverFullGrd),])
  KDE.sp_inPoly@coords <- KDE.df_full@coords[!is.na(BoundedOverFullGrd),]
  KDE.df_inPoly <- as.data.frame(KDE.sp_inPoly)
  KDE.raster_inPoly <- rasterize(KDE.df_inPoly[,c("X_COORD","Y_COORD")], raster, KDE.df_inPoly$VALUE, fun=sum)
  
  return(list(KDE.df=KDE.df_inPoly,KDE.raster=KDE.raster_inPoly,KDE.sp=KDE.sp_inPoly))
}

ConstrainedKDE2 <- function(CrimePts,grd,isInBound,bw,r){
  # KDE bounded in the boundary
  M <- r@nrows
  N <- r@ncols
  KDE.df_full <- data.frame(X_COORD=grd$X_COORD,Y_COORD=grd$Y_COORD,VALUE=rep(NA,N*M))
  kernSm <- bkde2D(data.matrix(CrimePts[,c("X_COORD","Y_COORD")]), bandwidth=bw, 
                   gridsize=c(N, M), range.x=list(range(grd$X_COORD),range(grd$Y_COORD)))
  KDE.df_full$VALUE <- as.vector(kernSm$fhat)     
  KDE.df_inPoly <- KDE.df_full[isInBound,]
  
  KDE.raster_inPoly <- rasterize(KDE.df_inPoly[,c("X_COORD","Y_COORD")], r, KDE.df_inPoly$VALUE, fun=sum)
  
  return(list(KDE.df=KDE.df_inPoly,KDE.raster=KDE.raster_inPoly))
}