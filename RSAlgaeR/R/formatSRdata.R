#' formatSRdata
#'
#' Format surface reflectance data
#'
#' @param data dataframe of surface reflectance data. Designed to work with reflectance values for bands from Landsat surface reflectance products (Blue, Red, Green, NIR, SWIR1, SWIR2, CloudMask) at specific point locations
#' @param imagerydate string, name of column for imagery dates
#' @param samplingdate string, name of column for sampling dates (only required if dataset is used for calibration)
#' @param location string, name of column for location identifier
#' @param datatype string, "Calibration" or "Estimated"
#' @param qaband string, name of column for QA (such as a cloudmask) rating
#' @param qa_accept vector, QA classes which are acceptable
#' @return dataframe with formatted data
#' @export

formatSRdata <- function(data,imagerydate,samplingdate="",location,datatype,qaband,qa_accept){
  #Format dates
  data$ImageDate <- as.Date(data[,imagerydate],format="%m/%d/%Y")

  #Remove cloud pixels
  data$QA <- data[,qaband]
  data <- data[(data$QA %in% qa_accept),]
  #Remove missing observations
  data <- data[(!is.na(data$Blue)),]
  #Remove data with negative reflectance values
  data <- data[(data$SWIR1>0 & data$SWIR2>0),]

  if(samplingdate!=""){
    data$SamplingDate <- as.Date(data[,samplingdate],format="%m/%d/%Y")
    data$AbsDiffInDays <- abs(data$ImageDate-data$SamplingDate)
    #Remove erroneous field data
    data <- data[(data$FieldValue>0),]

    #Find the images with least difference in near coincident matches
    dates <- unique(data$SamplingDate)
    temp.df <- data.frame()
    #Limit to the closest match
    for (i in dates){
      temp <- data[(data$SamplingDate==i),]
      sites <- unique(temp$StationID)

      for (j in sites){
        temp2 <- temp[(temp$StationID==j),]
        closesttemp <- temp2[(temp2$AbsDiffInDays==min(temp2$AbsDiffInDays)),]
        temp.df <- rbind(temp.df,closesttemp)
      }
    }
    #Remove data samples are matched to multiple dates or images matched to multiple samples (default to keep earlier image)
    temp.df <- temp.df[!duplicated(temp.df[,c(samplingdate,location)]),]
    temp.df <- temp.df[!duplicated(temp.df[,c(imagerydate,location)]),]

  }
  #Remove duplicated data
  temp.df=data[!duplicated(data[,c("ImageDate",location)]),]
  temp.df$Dataset=datatype

  return(temp.df)
}

