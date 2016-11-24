library(RMySQL)
library(ggplot2)
library(lubridate)
library(scales)

Lake='Utah'
TimeWindow=1
Threshold=1000


#Load Historic Reflectance Data from SQL Database and Format------------------------------------------------------------
importrefldata=function(database,password,Lake){
	d<-dbDriver("MySQL");
	con<-dbConnect(d,user='root',password=password,host='localhost');
	#Select database
	sqlstmtdb <-dbSendQuery(con,paste0("Use ",database,";"))
	#Create temporary table of historic, cloudfree data
	sqlstmt<-dbSendQuery(con, "SELECT min(RowID),avg(Blue) AS Blue, avg(Green) AS Green, avg(Red) AS Red, 
                     avg(NIR) AS NIR, avg(SWIR1) AS SWIR1, avg(SWIR2) AS SWIR2, 
                     StationID,ImageDate,Lake, Sensor FROM historic_sr WHERE CloudMask<=1 GROUP BY ImageDate, StationID;")
	sr_hist<- dbFetch(sqlstmt, n = -1)
	sr_hist=sr_hist[(sr_hist$Lake==Lake),]
	#Drop RowID Column
	sr_hist=sr_hist[,-1]
	#Drop rows with erroneous reflectance measurements
	sr_hist=sr_hist[(sr_hist$Blue<=10000 & sr_hist$SWIR1>0 & sr_hist$SWIR2>0),]
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
	return(sr_hist)
}

#Load data for calibration---------------------------------------------------------------------------------------------
importcaldata=function(database,password,Lake){
#Return only cloud-free imagery that has been averaged for the imagery that is closest to the sampling date
sqlstmt<-dbSendQuery(con, paste0("SELECT * FROM landsat_sr WHERE Lake=",Lake))
srdata<- dbFetch(sqlstmt, n = -1)

#Load surface reflectance data and Format
srdata$ImageDate=as.Date(srdata$ImageDate,format="%Y-%m-%d")
srdata$SamplingDate=as.Date(srdata$SamplingDate,format="%Y-%m-%d")
srdata$Month=factor(months(srdata$SamplingDate),levels=month.name)
srdata$Method=factor(srdata$Method)
srdata$Sensor=factor(srdata$Sensor)
srdata$Organization=factor(srdata$Organization)
#Limit to detected chlorophyll (do not include where there were errors or non-detect measurements)
srdatapos=srdata[which(srdata$FieldValue>0),]
#Generally, interest is in measurements that have been corrected for accessory pigments. This is not the case for Utah Lake, which consists largely of uncorrected measurements (historically)
	if(Lake!='Utah'){
	srdatapos=srdatapos[which(srdatapos$Method!="Chlorophyll a, uncorrected for pheophytin"& srdatapos$Method!="Water Grab Sampling" & srdatapos$Method!="Chlorophyll-A (Colorimet)"),]
	}


summary(utah)


#september----------------------------------------------

#Subset for seasonal model
sr_histSept=sr_hist[(sr_hist$Month==9),]
sr_histSept=sr_histSept[(sr_histSept$SWIR1<800 & sr_histSept$SWIR2>=0 & sr_histSept$SWIR1>=0),]
modcalSept=modcal[(modcal$Month==9),]
summary(modcalSept$FieldValue)
#Calibrate model
mod=glm(FieldValue~Blue+SWIR2,data=modcalSept,family=gaussian(link="log"))
predicted=(mod$fitted.values)
summary(predicted)
compare=data.frame(predicted)
compare$actual=modcalSept$FieldValue
compare$diff=compare$actual-compare$predicted
#Compute RMSE
rmse=sqrt(mean(compare$diff)^2)
print(paste("RMSE is: ",rmse))
print(summary(lm(compare$actual~compare$predicted))$r.squared)
summary(mod)
#Apply model
predvals=predict(mod,newdata=sr_histSept,type="response",se.fit=TRUE)
modeledvals=predvals$fit
limits=aes(ymax =predvals$fit+1.96*predvals$se.fit,ymin=predvals$fit-1.96*predvals$se.fit)
sr_histSept$Value=modeledvals
ggplot(sr_histSept, aes(colour=as.factor(StationID), y=Value, x=Date))+ 
  geom_point() + geom_errorbar(limits, width=0.2)+theme_bw()+
  scale_color_discrete(name="Station ID")+ggtitle("Predicted Values with Confidence Intervals: September")+
  ylab("Predicted Chl-a (ug/L)")
summary(sr_histSept$Value)
summary(as.Date(sr_histSept$ImageDate))


estimatedrecord=rbind(sr_histJune,sr_histJuly,sr_histAug,sr_histSept)
estimatedrecord=estimatedrecord[,c(11,7,31,10,30)]
estimatedrecord$Dataset='Estimates'
modcal_format=modcal
modcal_format$Date=modcal_format$SamplingDate
modcal_format$Value=modcal_format$FieldValue
modcal_format=modcal_format[,c(41,9,42,18,40,22)]
completerecord=rbind(estimatedrecord,modcal_format)

write.csv(completerecord,'RemoteSensingPaper/UtahLakeEstimatedRecord.csv')


#Plot Estimated values (with Calibration data)
ggplot(completerecord)+geom_point(aes(x=Date,y=Value,col=as.factor(Dataset)))+
  xlab("Date")+ylab("Estimated Value of Surface Chlorophyll, ug/L")+theme_bw()+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year"))+
  scale_y_continuous(limits=c(0,350))+
  theme(legend.position="bottom",axis.title.x = element_text(face="bold", size=14),
        axis.text.x  = element_text(angle=45, vjust=0.5, size=12),axis.title.y = element_text(face="bold", size=14),
        axis.text.y  = element_text(size=12))+scale_color_discrete(name  ="Dataset")+
  scale_y_continuous(limits=c(0,350))+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 year"),limits=c(as.Date("1984-01-01"),as.Date("2016-12-31")))


fieldrecord=read.csv('Field Data/GSLSWSData.csv')
Utah=fieldrecord[(fieldrecord$Lake=='Utah'),]
Utah=Utah[(Utah$Value>0),]
Utah$Date=as.Date(Utah$Date,format="%m/%d/%Y")
Utah$Month=month(Utah$Date)
UtahJune=Utah[(Utah$Month==6),]
summary(UtahJune$Value)
summary(UtahJune$Date)
UtahJuly=Utah[(Utah$Month==7),]
summary(UtahJuly$Value)
summary(UtahJuly$Date)
UtahAug=Utah[(Utah$Month==8),]
summary(UtahAug$Value)
summary(UtahAug$Date)
UtahSept=Utah[(Utah$Month==9),]
summary(UtahSept$Value)
summary(UtahSept$Date)
#Plot Historical Field Record
ggplot(Utah)+geom_point(aes(x=Date,y=Value,col=as.factor(StationID)))+
  xlab("Date")+ylab("Field-Measured Value \nof Surface Chlorophyll, ug/L")+theme_bw()+
  theme(legend.position="bottom",axis.title.x = element_text(face="bold", size=14),
        axis.text.x  = element_text(angle=45, vjust=0.5, size=12),axis.title.y = element_text(face="bold", size=14),
        axis.text.y  = element_text(size=12))+scale_color_discrete(name  ="Station")+
  scale_y_continuous(limits=c(0,350))+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 year"),limits=c(as.Date("1984-01-01"),as.Date("2016-12-31")))


#Plot Historical Field Record with Estimates on same plot
ggplot()+geom_point(aes(x=Utah$Date,y=Utah$Value))+geom_point(aes(x=completerecord$Date,y=completerecord$Value,col=as.factor(completerecord$StationID)))+
  xlab("Date")+ylab("Field-Measured or Estimated Value \nof Surface Chlorophyll, ug/L")+theme_bw()+
  theme(legend.position="bottom",axis.title.x = element_text(face="bold", size=14),
        axis.text.x  = element_text(angle=45, vjust=0.5, size=12),axis.title.y = element_text(face="bold", size=14),
        axis.text.y  = element_text(size=12))+scale_color_discrete(name  ="Station")+
  scale_y_continuous(limits=c(0,350))+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 year"),limits=c(as.Date("1984-01-01"),as.Date("2016-12-31")))

Utah$Sensor=NA
Utah$Month=month(Utah$Date)
Utah$DOY=yday(Utah$Date)
Utah$Dataset="Measurement"
complete.record=rbind(estimatedrecord,Utah[,c(1,3,2,13,15,16)])

write.csv(complete.record,'RemoteSensingPaper/UtahLakeCompleteRecord.csv')
