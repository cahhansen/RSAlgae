#Load in packages
library(ggplot2)
library(lubridate)

setwd("C:/Users/carly/Google Drive/University of Utah - Research/Dissertation Research/RemoteSensingPaper")
#May-June----------------------------------------------------------------------------------------------------------------------------
#Load dataframe
load('ReflectanceValues/SurfaceReflectanceData/UtahLakeLS57ModelData.Rda')
dataframe=dataframe[which(dataframe$Dataset=="Calibration"),]
dataframe$Month=as.factor(month(dataframe$SamplingDate))
dataframe=dataframe[(dataframe$Month=="5"|dataframe$Month=="6"),]
timewindow=2
subdataframe=dataframe[which(dataframe$AbsDiffInDays<=timewindow),]

modelvariables=c("Blue","Green","NIR","Red","SWIR1","SWIR2","Green_Blue","Red_Blue","Red_Green",
                 "Red_NIR","Red_SWIR1","Green_SWIR1","Blue_SWIR1","Red_SWIR2","Green_SWIR2","Blue_SWIR2",
                 "NIR_SWIR1","NIR_SWIR2","NIR_Blue","NIR_Green","NDVI","avgRGB","avgSWIR","FieldValue")
moddataframe=subset(subdataframe,select=modelvariables)
modearly=glm(FieldValue~Green_Blue+Blue,data=moddataframe,family=gaussian(link="log"))
predicted=(modearly$fitted.values)
compare=data.frame(predicted)
compare$actual=moddataframe$FieldValue
compare$diff=compare$actual-compare$predicted
#Compute RMSE
rmse=sqrt(mean(compare$diff)^2)
print(paste("RMSE is: ",rmse))
summary(lm(compare$actual~compare$predicted)$r.squared)
print(summary(lm(compare$actual~compare$predicted))$r.squared)
summary(modearly)

shapiro.test(moddataframe$FieldValue)
print(paste("Min chl: ",min(moddataframe$FieldValue)," Max chl: ",max(moddataframe$FieldValue)))
print(paste("N:",nrow(moddataframe)))
print(summary(moddataframe$FieldValue))
ggplot()+geom_point(aes(x=compare$actual,y=compare$predicted))+
  xlab(expression(paste("Measured Chl (",mu,"g/L)")))+ylab(expression(paste("Modeled Chl (",mu,"g/L)")))+
  theme_bw()+ggtitle("Measured vs. Modeled Chlorophyll, Early Summer Model")+
  theme(plot.title = element_text(hjust = 0.5))+geom_abline()

#July-September-------------------------------------------------------------------------------------------------------------------------
#Load dataframe
load('ReflectanceValues/SurfaceReflectanceData/UtahLakeLS57ModelData.Rda')
dataframe=dataframe[which(dataframe$Dataset=="Calibration"),]
dataframe$Month=as.factor(month(dataframe$SamplingDate))
dataframe=dataframe[(dataframe$Month=="7"|dataframe$Month=="8"|dataframe$Month=="9"|dataframe$Month=="10"),]
timewindow=1
subdataframe=dataframe[which(dataframe$AbsDiffInDays<=timewindow),]

modelvariables=c("Blue","Green","NIR","Red","SWIR1","SWIR2","Green_Blue","Red_Blue","Red_Green",
                 "Red_NIR","Red_SWIR1","Green_SWIR1","Blue_SWIR1","Red_SWIR2","Green_SWIR2","Blue_SWIR2",
                 "NIR_SWIR1","NIR_SWIR2","NIR_Blue","NIR_Green","NDVI","avgRGB","avgSWIR","FieldValue")
moddataframe=subset(subdataframe,select=modelvariables)
modmid=glm(FieldValue~Blue+Green_SWIR2+Red_SWIR1,data=moddataframe,family=gaussian(link="log"))
predicted=(modmid$fitted.values)
compare=data.frame(predicted)
compare$actual=moddataframe$FieldValue
compare$diff=compare$actual-compare$predicted
#Compute RMSE
rmse=sqrt(mean(compare$diff)^2)
print(paste("RMSE is: ",rmse))
summary(lm(compare$actual~compare$predicted)$r.squared)
print(summary(lm(compare$actual~compare$predicted))$r.squared)
summary(modmid)

print(paste("Min chl: ",min(moddataframe$FieldValue)," Max chl: ",max(moddataframe$FieldValue)))
print(paste("N:",nrow(moddataframe)))
ggplot()+geom_point(aes(x=subdataframe$FieldValue,y=compare$predicted))+
  xlab(expression(paste("Measured Chl (",mu,"g/L)")))+ylab(expression(paste("Modeled Chl (",mu,"g/L)")))+
  theme_bw()+ggtitle("Measured vs. Modeled Chlorophyll, Summer Model")+
  theme(plot.title = element_text(hjust = 0.5))+geom_abline()

#July-----------------------------------------------------------------------------------------------------------------------------------
#Load dataframe
load('ReflectanceValues/SurfaceReflectanceData/UtahLakeLS57ModelData.Rda')
dataframe=dataframe[which(dataframe$Dataset=="Calibration"),]
dataframe$Month=as.factor(month(dataframe$SamplingDate))
dataframe=dataframe[(dataframe$Month=="7"),]
timewindow=2
subdataframe=dataframe[which(dataframe$AbsDiffInDays<=timewindow),]

modelvariables=c("Blue","Green","NIR","Red","SWIR1","SWIR2","Green_Blue","Red_Blue","Red_Green",
                 "Red_NIR","Red_SWIR1","Green_SWIR1","Blue_SWIR1","Red_SWIR2","Green_SWIR2","Blue_SWIR2",
                 "NIR_SWIR1","NIR_SWIR2","NIR_Blue","NIR_Green","NDVI","avgRGB","avgSWIR","FieldValue")
moddataframe=subset(subdataframe,select=modelvariables)
modlate=glm(FieldValue~Red+Red_SWIR2+Blue_SWIR2+NIR_Green+NDVI+avgSWIR,data=moddataframe,family=gaussian(link="log"))
predicted=(modlate$fitted.values)
compare=data.frame(predicted)
compare$actual=moddataframe$FieldValue
compare$diff=compare$actual-compare$predicted
#Compute RMSE
rmse=sqrt(mean(compare$diff)^2)
print(paste("RMSE is: ",rmse))
summary(lm(compare$actual~compare$predicted)$r.squared)
print(summary(lm(compare$actual~compare$predicted))$r.squared)
summary(modlate)

print(paste("Min chl: ",min(moddataframe$FieldValue)," Max chl: ",max(moddataframe$FieldValue)))
print(paste("N:",nrow(moddataframe)))
ggplot()+geom_point(aes(x=subdataframe$FieldValue,y=compare$predicted))+
  xlab(expression(paste("Measured Chl (",mu,"g/L)")))+ylab(expression(paste("Modeled Chl (",mu,"g/L)")))+
  theme_bw()+ggtitle("Measured vs. Modeled Chlorophyll, Mid Summer Model")+
  theme(plot.title = element_text(hjust = 0.5))+geom_abline()

#August-Sept-----------------------------------------------------------------------------------------------------------------------------------
#Load dataframe
load('ReflectanceValues/SurfaceReflectanceData/UtahLakeLS57ModelData.Rda')
dataframe=dataframe[which(dataframe$Dataset=="Calibration"),]
dataframe$Month=as.factor(month(dataframe$SamplingDate))
dataframe=dataframe[(dataframe$Month=="8"|dataframe$Month=="9"),]
timewindow=1
subdataframe=dataframe[which(dataframe$AbsDiffInDays<=timewindow),]

modelvariables=c("Blue","Green","NIR","Red","SWIR1","SWIR2","Green_Blue","Red_Blue","Red_Green",
                 "Red_NIR","Red_SWIR1","Green_SWIR1","Blue_SWIR1","Red_SWIR2","Green_SWIR2","Blue_SWIR2",
                 "NIR_SWIR1","NIR_SWIR2","NIR_Blue","NIR_Green","NDVI","avgRGB","avgSWIR","FieldValue")
moddataframe=subset(subdataframe,select=modelvariables)
modlate=glm(FieldValue~Blue+Red_SWIR2,data=moddataframe,family=gaussian(link="log"))
predicted=(modlate$fitted.values)
compare=data.frame(predicted)
compare$actual=moddataframe$FieldValue
compare$diff=compare$actual-compare$predicted
#Compute RMSE
rmse=sqrt(mean(compare$diff)^2)
print(paste("RMSE is: ",rmse))
summary(lm(compare$actual~compare$predicted)$r.squared)
print(summary(lm(compare$actual~compare$predicted))$r.squared)
summary(modlate)

print(paste("Min chl: ",min(moddataframe$FieldValue)," Max chl: ",max(moddataframe$FieldValue)))
print(paste("N:",nrow(moddataframe)))
ggplot()+geom_point(aes(x=subdataframe$FieldValue,y=compare$predicted))+
  xlab(expression(paste("Measured Chl (",mu,"g/L)")))+ylab(expression(paste("Modeled Chl (",mu,"g/L)")))+
  theme_bw()+ggtitle("Measured vs. Modeled Chlorophyll, Late Summer Model")+
  theme(plot.title = element_text(hjust = 0.5))+geom_abline()
