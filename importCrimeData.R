importCrimeData <- function(filePath="./",fileName){
  file <- paste(filePath,fileName,sep="")
  CrimeData <- read.csv(file)
  
  # convert dateocc to date class
  CrimeData$DATEOCC <- as.Date(strptime(CrimeData$DATEOCC,'%d-%B-%y'))
  # order the data by date
  CrimeData <- CrimeData[order(CrimeData$DATEOCC),]
  # add attribute 'day of week'
  DOW <- weekdays(CrimeData$DATEOCC,abbreviate=T)
  
  CrimeData <- cbind(CrimeData[,c("DATEOCC","YEAR","MONTH","DAY")],DOW=DOW,
                     CrimeData[,!names(CrimeData) %in% c("DATEOCC","YEAR","MONTH","DAY")])
  
  if ("AREA" %in% names(CrimeData)){
    CrimeData$AREA <- factor(CrimeData$AREA)
  } 
  CrimeData$BEAT <- factor(CrimeData$BEAT)
  CrimeData$DISTRICT <- factor(CrimeData$DISTRICT)  
  CrimeData$DOW <- factor(CrimeData$DOW, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
  CrimeData$CURR_IUCR <- factor(CrimeData$CURR_IUCR)
  CrimeData$FBI_CD <- factor(CrimeData$FBI_CD)
  
  return(CrimeData)
}