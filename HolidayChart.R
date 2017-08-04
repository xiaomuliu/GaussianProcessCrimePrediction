holidays <- function(dateocc){
  Month <- as.numeric(format(dateocc, "%m"))
  Day <- as.numeric(format(dateocc,"%d"))
  DOW <- weekdays(dateocc,abbreviate=T) #level "Sun","Mon","Tue","Wed","Thu","Fri","Sat"
  
  if ((Month==12 & Day==31 & DOW=="Fri") | (Month==01 & Day==02 & DOW=="Mon") | (Month==01 & Day==01)){
    Holiday = 1 # New Year's Day
  } 
  else if (Month==01 & Day>=15 & Day<=21 & DOW=="Mon") {
    Holiday = 2 # M.L.K's day
  }
  else if (Month==02 & Day>=15 & Day<=21 & DOW=="Mon") {    
    Holiday = 3 # Washington's Birthday
  }         
  else if (Month==05 & Day>=25 & Day<=31 & DOW=="Mon") {   
    Holiday = 4 #Memorial Day
  } 
  else if ((Month==07 & Day==03 & DOW=="Fri") | (Month==07 & Day==05 & DOW=="Mon") | (Month==07 & Day==04)){
    Holiday = 5 # 4th of July
  }  
  else if (Month==09 & Day>=01 & Day<=07 & DOW=="Mon"){
    Holiday = 6 # Labor Day
  } 
  else if (Month==10 & Day>=08 & Day<=14 & DOW=="Mon"){   
    Holiday = 7 # Columbus Day
  }   
  else if (Month==10 & Day==31){
    Holiday = 8 # Halloween
  }
  else if ((Month==11 & Day==10 & DOW=="Fri") | (Month==11 & Day==12 & DOW=="Mon") | (Month==11 & Day==11)){
    Holiday = 9 #Veterans Day
  }
  else if (Month==11 & Day>=22 & Day<=28 & DOW=="Thu"){
    Holiday = 10 # Thanksgiving
  }     
  else if ((Month==12 & Day==24 & DOW=="Fri") | (Month==12 & Day==26 & DOW=="Mon") | (Month==12 & Day==25)){
    Holiday = 11 # Christmas
  } 
  else {
    Holiday = 0 # not a holiday
  }
  return(Holiday)
}
