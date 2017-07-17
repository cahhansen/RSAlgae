#Read in reflectance data for calibration

FormatHistoricReflectanceData <- function(data){
    
  #Format dates
  data$ImageDate=as.Date(data$ImageDate,format="%Y/%m/%d")
  #Remove cloud pixels
  data=data[(data$CloudMask<=1),]
  #Remove missing observations
  data=data[(!is.na(data$Blue)),]
  #Remove data with negative reflectance values
  data=data[(data$SWIR1>0 & data$SWIR2>0),]
  
  
  #Remove duplicated data
  temp.df=data[!duplicated(data[,c("ImageDate","StationID")]),]
  temp.df$Dataset="Estimated"
  
  return(temp.df)
}

