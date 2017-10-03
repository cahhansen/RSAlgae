#' formatSRdata
#'
#' Format surface reflectance data (used for application, not calibration)
#'
#' @param data dataframe of surface reflectance data. Designed to work with reflectance values for bands from Landsat surface reflectance products (Blue, Red, Green, NIR, SWIR1, SWIR2, CloudMask) at specific point locations (StationID)
#' @param date string, name of column for dates
#' @return dataframe with formatted data
#' @export

formatSRdata <- function(data,date){
  #Format dates
  data$ImageDate=as.Date(data[,date],format="%Y/%m/%d")
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

