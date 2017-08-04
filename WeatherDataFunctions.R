RetrieveWeatherData <- function(startDate, endDate, Path='./', Unit='day', API.id ='9D8C53D1-976B-4866-9AE5-C5A2C77EE7B0',
                                LONG='-87.750',LAT='41.783',Format='csv',site='KMDW'){
  #   Pull and download weather data using weatheranalytics.com API
  if(!is.character(startDate)){
    startDate = as.character(as.Date(as.character(startDate), "%m/%d/%Y")) 
  }
  if(!is.character(endDate)){
    endDate = as.character(as.Date(as.character(endDate), "%m/%d/%Y")) 
  }
  
  if(Unit=='day'){Req='davg'}
  else if(Unit=='month'){Req='mavg'}
  else {Req='standard'} # hour
  
  con = url(paste('http://data.weatheranalytics.com/wawebdataservices/wawebservice/?ID=', API.id,
                  '&LONG=',LONG,'&Lat=',LAT,'&Req=',Req,'&StartDate=',startDate,'&EndDate=',endDate,
                  '&TS=LST&Format=',Format,'&site=KMDW', sep=''))
  filelink <- readLines(con, warn=FALSE)
  close(con)
  
  parseStr = strsplit(filelink,'.csv')
  filelink = paste(parseStr[[1]][1],'.csv',sep='')
  
  filename <- paste(Path,'WeatherData_',as.character(as.Date(startDate, "%m/%d/%Y")),'_',
                    as.character(as.Date(endDate, "%m/%d/%Y")),'.csv',sep='')
  status <- download.file(filelink, destfile = filename, method= "curl")
  
  invisible(status) #0 for success and non-zero for failure
}


ConvertToDailyWeatherData <- function(filename){
  
  WeatherData.raw = read.csv(filename)
  
  # convert to daily data
#   # remove irrelevent attributes
#   irrelevent <- c('WsWaIdx','DateHrGmt','FcastHr','Twet_F','Twc_F','Spd_KTS',
#                   'Dir_DEG','DnSol_WsqM','DiffHorz_WsqM','DirNormIr_WsqM')
  
  keep <- c('DateHrLwt','Tsfc_F','Tdew_F','Rh_PCT','Psfc_MB','CldCov_PCT','Tapp_F','PcpPrevHr_IN','Spd_MPH')
  WeatherData.raw <- subset(WeatherData.raw, select=keep)
  WeatherData.raw$DateHrLwt <- as.Date(WeatherData.raw$DateHrLwt)  

  sub.df <- subset(WeatherData.raw,select=-c(PcpPrevHr_IN,DateHrLwt))
  WeatherMean <- aggregate(sub.df,by=list(WeatherData.raw$DateHrLwt),FUN=mean,na.rm=TRUE)
  WeatherMax <- aggregate(sub.df,by=list(WeatherData.raw$DateHrLwt),FUN=max,na.rm=TRUE)
  WeatherMin <- aggregate(sub.df,by=list(WeatherData.raw$DateHrLwt),FUN=min,na.rm=TRUE)
  # calculate the total amount of daily precipitation
  Tot_pcp <- aggregate(PcpPrevHr_IN~DateHrLwt,data=WeatherData.raw,FUN=sum,na.rm=TRUE)
  names(Tot_pcp)[1]<-'Date'
  
  names(WeatherMean) <- c('Date',sapply(names(sub.df),paste,'_avg',sep=''))
  names(WeatherMax) <- c('Date',sapply(names(sub.df),paste,'_max',sep=''))
  names(WeatherMin) <- c('Date',sapply(names(sub.df),paste,'_min',sep=''))
  
  WeatherData.daily <- merge(merge(merge(WeatherMean,WeatherMax,all=TRUE),WeatherMin,all=TRUE),Tot_pcp,all=TRUE)
  return(WeatherData.daily)              
}

DailyWeatherDiff<- function(WeatherData.daily){
  # calculate one-day and two-day differences of each weather variables
  diff1 <- apply(WeatherData.daily[,-1],2,FUN=diff,lag=1)
  diff2 <- apply(WeatherData.daily[,-1],2,FUN=diff,lag=2)
  colnames(diff1) <- sapply(names(WeatherData.daily[,-1]),paste,'_diff1',sep='')
  colnames(diff2) <- sapply(names(WeatherData.daily[,-1]),paste,'_diff2',sep='')
  WeatherData.daily_diff <- cbind(Date=WeatherData.daily$Date[3:length(WeatherData.daily$Date)],
                                  as.data.frame(cbind(diff1[2:nrow(diff1),],diff2)))
}