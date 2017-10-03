#' create.model.vars
#'
#' Create model variables used in model calibration and application
#'
#' @param filename CSV file with formatted surface reflectance data
#' @param rowIndex True or False, Indicates whether the first column in the formatted data file is a row index
#' @return dataframe with variables used in model development or application
#' @export

create.model.vars <- function(filename,rowIndex=TRUE){
  dataframe <- read.csv(filename)
  if(rowIndex==TRUE){
    dataframe=dataframe[,-1]
  }
  #Create interaction variables
  dataframe$Green_Blue=dataframe$Green/dataframe$Blue
  dataframe$Red_Blue=dataframe$Red/dataframe$Blue
  dataframe$Red_Green=dataframe$Red/dataframe$Green
  dataframe$Red_NIR=dataframe$Red/dataframe$NIR
  dataframe$Red_SWIR1=dataframe$Red/dataframe$SWIR1
  dataframe$Green_SWIR1=dataframe$Green/dataframe$SWIR1
  dataframe$Blue_SWIR1=dataframe$Blue/dataframe$SWIR1
  dataframe$Red_SWIR2=dataframe$Red/dataframe$SWIR2
  dataframe$Green_SWIR2=dataframe$Green/dataframe$SWIR2
  dataframe$Blue_SWIR2=dataframe$Blue/dataframe$SWIR2
  dataframe$NIR_SWIR1=dataframe$NIR/dataframe$SWIR1
  dataframe$NIR_SWIR2=dataframe$NIR/dataframe$SWIR2
  dataframe$NIR_Blue=dataframe$NIR/dataframe$Blue
  dataframe$NIR_Green=dataframe$NIR/dataframe$Green
  dataframe$NDVI=(dataframe$NIR-dataframe$Red)/(dataframe$NIR+dataframe$Red)
  dataframe$avgRGB=(dataframe$Red+dataframe$Blue+dataframe$Green)/3
  dataframe$avgSWIR=(dataframe$SWIR1+dataframe$SWIR2)/2

  return(dataframe)
}

