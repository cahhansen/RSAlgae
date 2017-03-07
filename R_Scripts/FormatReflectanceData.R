#Read in reflectance data for calibration
setwd('C:/Users/carly/Google Drive/University of Utah - Research/Dissertation Research/RemoteSensingPaper/ReflectanceValues/SurfaceReflectanceData')
data=read.csv('LS5-7EarthEngineExport_Utah.csv')



#Format dates
data$ImageDate=as.Date(data$ImageDate,format="%m/%d/%Y")
data$SamplingDate=as.Date(data$SamplingDate,format="%m/%d/%Y")
#Specify Methods to be used
data=data[!(data$Method=="Chlorophyll-A (Colorimet), Corrected for pheophytin"),]
#Remove erroneous field data
data=data[(data$FieldValue>0),]
#Remove cloud pixels
data=data[(data$CloudMask<=1),]
#Remove missing observations
data=data[(!is.na(data$Blue)),]
#Remove data with negative reflectance values
data=data[(data$SWIR1>0 & data$SWIR2>0),]

#Find the images with least difference in near coincident matches
dates=unique(data$SamplingDate)
temp.df=data.frame()
#Limit to the closest match
for (i in dates){
  temp=data[(data$SamplingDate==i),]
  sites=unique(temp$StationID)
  
  for (j in sites){
    temp2=temp[(temp$StationID==j),]
    closesttemp=temp2[(temp2$AbsDiffInDays==min(temp2$AbsDiffInDays)),]
    temp.df=rbind(temp.df,closesttemp)
  }
}
#Remove data samples are matched to multiple dates or images matched to multiple samples (default to keep earlier image)
temp.df=temp.df[!duplicated(temp.df[,c("SamplingDate","StationID")]),]
temp.df=temp.df[!duplicated(temp.df[,c("ImageDate","StationID")]),]


write.csv(temp.df,file="UtahLakeLS57CalibrationFormat.csv")
