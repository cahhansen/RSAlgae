#'Import Reflectance Data
#'
#'Reads in Reflectance Data from MySQL Database. It is important that the database have columns for RowID, Blue, Green, Red, NIR, SWIR1, SWIR2,
#'StationID, ImageDate, Lake, Sensor, and CloudMask. (It may have more columns).
#'@param database Name of Database
#'@param password Password for 'root' user
#'@param lake Body of Water of Interest
#'@return Dataframe of reflectance values
#'@export



#Load Historic Reflectance Data from SQL Database and Format------------------------------------------------------------
importrefldata=function(database,password,tablename,lake){
  library(RMySQL)
  library(lubridate)
  d<-dbDriver("MySQL");
  con<-dbConnect(d,user='root',password=password,host='localhost');
  #Select database
  db=paste0("Use ",database,";")
  sqlstmtdb <-dbSendQuery(con,db)
  query=paste0("SELECT min(RowID),avg(Blue) AS Blue, avg(Green) AS Green, avg(Red) AS Red, avg(NIR) AS NIR, avg(SWIR1) AS SWIR1, avg(SWIR2) AS SWIR2, StationID,ImageDate,Lake,Sensor FROM ",tablename," WHERE CloudMask<=1 GROUP BY ImageDate, StationID;")
    sqlstmt<-dbSendQuery(con, query)
  sr_hist<- dbFetch(sqlstmt, n = -1)
  sr_hist=sr_hist[(sr_hist$Lake==lake),]
  #Drop RowID Column
  sr_hist=sr_hist[,-1]
  #Drop rows with erroneous reflectance measurements
  sr_hist=sr_hist[(sr_hist$Blue<=10000 & sr_hist$SWIR1>0 & sr_hist$SWIR2>0 & sr_hist$SWIR1<1000 &sr_hist$SWIR2<1000),]
  #Format historical reflectance data to create interaction variables
  sr_hist$Green_Blue=sr_hist$Green/sr_hist$Blue
  sr_hist$Red_Blue=sr_hist$Red/sr_hist$Blue
  sr_hist$Red_Green=sr_hist$Red/sr_hist$Green
  sr_hist$Red_NIR=sr_hist$Red/sr_hist$NIR
  sr_hist$Red_SWIR1=sr_hist$Red/sr_hist$SWIR1
  sr_hist$Green_SWIR1=sr_hist$Green/sr_hist$SWIR1
  sr_hist$Blue_SWIR1=sr_hist$Blue/sr_hist$SWIR1
  sr_hist$Red_SWIR2=sr_hist$Red/sr_hist$SWIR2
  sr_hist$Green_SWIR2=sr_hist$Green/sr_hist$SWIR2
  sr_hist$Blue_SWIR2=sr_hist$Blue/sr_hist$SWIR2
  sr_hist$NIR_SWIR1=sr_hist$NIR/sr_hist$SWIR1
  sr_hist$NIR_SWIR2=sr_hist$NIR/sr_hist$SWIR2
  sr_hist$NIR_Blue=sr_hist$NIR/sr_hist$Blue
  sr_hist$NIR_Green=sr_hist$NIR/sr_hist$Green
  sr_hist$NDVI=(sr_hist$NIR-sr_hist$Red)/(sr_hist$NIR+sr_hist$Red)
  sr_hist$avgRGB=(sr_hist$Red+sr_hist$Blue+sr_hist$Green)/3
  sr_hist$avgSWIR=(sr_hist$SWIR1+sr_hist$SWIR2)/2
  #Format and create variables for potential sub-seasonal model development
  sr_hist$Date=as.Date(sr_hist$ImageDate)
  sr_hist$Month=(month(sr_hist$Date))
  sr_hist$DOY=(yday(sr_hist$Date))
  #Put columns in order
  sr_hist=sr_hist[,c("Date","Month","DOY","StationID","Blue","Green","Red","NIR","SWIR1","SWIR2","Green_Blue","Red_Blue",
                     "Red_Green","Red_NIR","Red_SWIR1","Green_SWIR1","Blue_SWIR1","Red_SWIR2","Green_SWIR2",
                     "Blue_SWIR2","NIR_SWIR1","NIR_SWIR2","NIR_Blue","NIR_Green","NDVI","avgRGB","avgSWIR","Sensor","Lake")]
  return(sr_hist)
}
