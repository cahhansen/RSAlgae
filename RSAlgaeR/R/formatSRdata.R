#' formatSRdata
#'
#' Format surface reflectance data
#'
#' @param data dataframe of surface reflectance data. Designed to work with reflectance values for bands from Landsat surface reflectance products (Blue, Red, Green, NIR, SWIR1, SWIR2, QA Band) at specific point locations
#' @param value string, name of column with water quality parameter values
#' @param imagerydate string, name of column for imagery dates (must be date format)
#' @param samplingdate string, name of column for sampling dates (only required if dataset is used for calibration, must be date format)
#' @param location string, name of column for location identifier
#' @param datatype string, "Calibration" or "Estimated"
#' @param qaband string, name of column for QA (such as a cloudmask) rating
#' @param qa_accept vector, QA classes which are acceptable
#' @return dataframe with formatted data
#' @examples
#' data(srdata)
#' formattedsrdata <- formatSRdata(data=srdata,value="FieldValue",imagerydate="ImageDate",
#' samplingdate="SamplingDate",location="StationID",
#' datatype="Calibration",qaband="CloudMask",qa_accept=c(0,1))
#' @export

formatSRdata <- function(data,value,imagerydate,samplingdate="",location,datatype,qaband,qa_accept){
  data$value <- data[[value]]
  data$imagedate <- data[[imagerydate]]
  data$location <- data[[location]]


  #Remove cloud pixels
  data$QA <- data[[qaband]]
  data <- data[(data$QA %in% qa_accept),]
  #Remove missing observations
  data <- data[(!is.na(data$Blue)),]
  #Remove data with negative reflectance values
  data <- data[(data$SWIR1>0 & data$SWIR2>0),]

  if(samplingdate!=""){
    data$samplingdate <- data[[samplingdate]]
    data$AbsDiffInDays <- abs(data$imagedate-data$samplingdate)
    #Remove erroneous field data
    data <- data[(data$value>0),]

    #Find the images with least difference in near coincident matches
    dates <- unique(data$samplingdate)
    temp.df <- data.frame()
    #Limit to the closest match
    for (i in dates){
      temp <- data[(data$samplingdate==i),]
      sites <- unique(temp$location)

      for (j in sites){
        temp2 <- temp[(temp$location==j),]
        closesttemp <- temp2[(temp2$AbsDiffInDays==min(temp2$AbsDiffInDays)),]
        temp.df <- rbind(temp.df,closesttemp)
      }
    }


  }
  #Remove data samples that are matched to multiple dates or images matched to multiple samples (default to keep earlier image)
  temp.df <- temp.df[!duplicated(temp.df[,c("samplingdate","location")]),]
  temp.df <- temp.df[!duplicated(temp.df[,c("imagedate","location")]),]
  temp.df$Dataset=datatype

  return(temp.df)
}

