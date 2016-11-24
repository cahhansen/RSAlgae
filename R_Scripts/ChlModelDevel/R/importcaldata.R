#'Import Reflectance Data for calibration
#'
#'Reads in Reflectance Data from MySQL Database. It is important that the database have columns for RowID, Blue, Green, Red, NIR, SWIR1, SWIR2,
#'StationID, ImageDate, Lake, Sensor, and CloudMask. (It may have more columns).
#'@param database Name of Database
#'@param password Password for 'root' user
#'@param tablename Name of table to import data from
#'@param lake Body of Water of Interest
#'@return Dataframe of reflectance values
#'@export


#Load data for calibration---------------------------------------------------------------------------------------------
importcaldata=function(database,password,tablename,lake){
  library(RMySQL)
  library(lubridate)
  #Return only cloud-free imagery that has been averaged for the imagery that is closest to the sampling date
  d<-dbDriver("MySQL");
  con<-dbConnect(d,user='root',password=password,host='localhost');
  #Select database
  db=paste0("Use ",database,";")
  sqlstmtdb <-dbSendQuery(con,db)
  query=paste0("SELECT * FROM ",tablename,";")
  sqlstmt<-dbSendQuery(con, query)
  srdata<- dbFetch(sqlstmt, n = -1)
  srdata=srdata[(srdata$Lake==lake),]

  #Format
  srdata$Date=as.Date(srdata$ImageDate,format="%Y-%m-%d")
  srdata$SamplingDate=as.Date(srdata$SamplingDate,format="%Y-%m-%d")
  srdata$Month=factor(months(srdata$SamplingDate))
  srdata$Method=factor(srdata$Method)
  srdata$Sensor=factor(srdata$Sensor)
  srdata$Organization=factor(srdata$Organization)
  #Limit to detected chlorophyll (do not include where there were errors or non-detect measurements)
  srdatapos=srdata[which(srdata$FieldValue>0),]
  #Generally, interest is in measurements that have been corrected for accessory pigments. This is not the case for Utah Lake, which consists largely of uncorrected measurements (historically)
  if(lake!='Utah'){
    srdatapos=srdatapos[which(srdatapos$Method!="Chlorophyll-A Uncorrected for pheophytin"& srdatapos$Method!="Water Grab Sampling" & srdatapos$Method!="Chlorophyll-A (Colorimet)"),]
  }else{
    srdatapos=srdatapos[which(srdatapos$Method!="Chlorophyll-A (Colorimet), Corrected for pheophytin"),]
  }
  #Limit it to the timewindow
  srdatapos=srdatapos[which(srdatapos$AbsDiffInDays<=timewindow),]
  #Eliminate any data where the SWIR data is negative (over-corrected)
  srdatapos=srdatapos[which(srdatapos$SWIR1>0 & srdatapos$SWIR2>0),]
  srdatapos$Green_Blue=srdatapos$Green/srdatapos$Blue
  srdatapos$Red_Blue=srdatapos$Red/srdatapos$Blue
  srdatapos$Red_Green=srdatapos$Red/srdatapos$Green
  srdatapos$Red_NIR=srdatapos$Red/srdatapos$NIR
  srdatapos$Red_SWIR1=srdatapos$Red/srdatapos$SWIR1
  srdatapos$Green_SWIR1=srdatapos$Green/srdatapos$SWIR1
  srdatapos$Blue_SWIR1=srdatapos$Blue/srdatapos$SWIR1
  srdatapos$Red_SWIR2=srdatapos$Red/srdatapos$SWIR2
  srdatapos$Green_SWIR2=srdatapos$Green/srdatapos$SWIR2
  srdatapos$Blue_SWIR2=srdatapos$Blue/srdatapos$SWIR2
  srdatapos$NIR_SWIR1=srdatapos$NIR/srdatapos$SWIR1
  srdatapos$NIR_SWIR2=srdatapos$NIR/srdatapos$SWIR2
  srdatapos$NIR_Blue=srdatapos$NIR/srdatapos$Blue
  srdatapos$NIR_Green=srdatapos$NIR/srdatapos$Green
  srdatapos$NDVI=(srdatapos$NIR-srdatapos$Red)/(srdatapos$NIR+srdatapos$Red)
  srdatapos$avgRGB=(srdatapos$Red+srdatapos$Blue+srdatapos$Green)/3
  srdatapos$avgSWIR=(srdatapos$SWIR1+srdatapos$SWIR2)/2
  srdatapos$DOY=(yday(srdatapos$Date))
  #Put columns in order
  srdatapos=srdatapos[,c("Date","Month","DOY","StationID","FieldValue","Blue","Green","Red","NIR","SWIR1","SWIR2","Green_Blue","Red_Blue",
                     "Red_Green","Red_NIR","Red_SWIR1","Green_SWIR1","Blue_SWIR1","Red_SWIR2","Green_SWIR2",
                     "Blue_SWIR2","NIR_SWIR1","NIR_SWIR2","NIR_Blue","NIR_Green","NDVI","avgRGB","avgSWIR","Sensor","Lake")]
  return(srdatapos)

}
