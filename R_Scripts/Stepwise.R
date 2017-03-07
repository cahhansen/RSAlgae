#Load in packages
library(ggplot2)
#Load data and process-------------------------------------------------------------------------------
setwd("C:/Users/carly/Google Drive/University of Utah - Research/Dissertation Research/RemoteSensingPaper")
#Load dataframe
load('ReflectanceValues/SurfaceReflectanceData/UtahLakeLS57ModelData.Rda')
timewindow=10
subdataframe=dataframe[which(dataframe$AbsDiffInDays<=timewindow),]
print(nrow(subdataframe))
summary(subdataframe$Method)
summary(subdataframe$FieldValue)

modelvariables=c("Blue","Green","NIR","Red","SWIR1","SWIR2","Green_Blue","Red_Blue","Red_Green",
                       "Red_NIR","Red_SWIR1","Green_SWIR1","Blue_SWIR1","Red_SWIR2","Green_SWIR2","Blue_SWIR2",
                       "NIR_SWIR1","NIR_SWIR2","NIR_Blue","NIR_Green","NDVI","avgRGB","avgSWIR","FieldValue")
modsubdataframe=subset(subdataframe,select=modelvariables)
if(nrow(modsubdataframe)<30){
  print("Insufficient Samples for k-fold validation")
  mod1=glm(FieldValue~.,data=modsubdataframe,family=gaussian(link="log"))
  mod0=glm(FieldValue~1,data=modsubdataframe,family=gaussian(link="log"))
  mod=step(mod0,scope=formula(mod1),trace=0)
  predicted=(mod$fitted.values)
  compare=data.frame(predicted)
  compare$actual=modsubdataframe$FieldValue
  compare$diff=compare$actual-compare$predicted
  #Compute RMSE
  rmse=sqrt(mean(compare$diff)^2)
  print(paste("RMSE is: ",rmse))
  summary(lm(compare$actual~compare$predicted)$r.squared)
  print(paste("R2 is: ",summary(lm(compare$actual~compare$predicted))$r.squared))
  summary(mod)
  plot(compare$predicted~compare$actual)
}else{
  #k-fold validation (for checking which parameters consistently are significant)
  k=10
  modsubdataframe$id <- sample(1:k, nrow(modsubdataframe), replace = TRUE)
  rmse.test=list()
  rmse.train=list()
  predictiver2=list()
  trainr2=list()
  coeff.df=data.frame(Variable=character(0),PValue=numeric(0))
  
  for (n in c(1,2,3,4,5,6,7,8,9,10)){
    print(paste("K is ", n))
    testset = subset(modsubdataframe, id %in% n)
    trainingset = subset(modsubdataframe, !(id %in% n))
    trainingset = trainingset[,-25]
    testset = testset[,-25]
    #Stepwise build GLM
      mod1=glm(FieldValue~.,data=trainingset,family=gaussian(link="log"))
      mod0=glm(FieldValue~1,data=trainingset,family=gaussian(link="log"))
      mod=step(mod0,scope=formula(mod1),trace=0)
    
    #Look at training dataset
    predtrain=exp(predict(mod))
    comparetrain=data.frame(predtrain)
    comparetrain$actual=trainingset$FieldValue
    comparetrain$diff=comparetrain$actual-comparetrain$predtrain
    #Compute RMSE and R2
    rmsetrain=sqrt(mean(comparetrain$diff)^2)
    rmse.train=append(rmse.train,rmsetrain)
    trainr2=append(trainr2,summary(lm(comparetrain$actual~comparetrain$predtrain))$r.squared)
    
    #Look at testing dataset
    predicted=exp(predict(mod,newdata=testset))
    compare=data.frame(predicted)
    compare$actual=testset$FieldValue
    compare$diff=compare$actual-compare$predicted
    #Compute RMSE and R2
    rmse=sqrt(mean(compare$diff)^2)
    rmse.test=append(rmse.test,rmse)
    predictiver2=append(predictiver2,summary(lm(compare$actual~compare$predicted))$r.squared)
    
    #Retrieve significant coefficients
    coeff=data.frame(names(mod$coefficients),summary(mod)$coef[,4],row.names=NULL)
    names(coeff)=c("Parameter","PValue")
    coeff.df=rbind(coeff.df,coeff)
    coeff.df=coeff.df[(coeff.df$PValue<=0.15),]
  }
}

#Summary of dependent variable
print(paste("Min chl: ",min(modsubdataframe$FieldValue)," Max chl: ",max(modsubdataframe$FieldValue)))
print(paste("N:",nrow(modsubdataframe)))
print(summary(modsubdataframe$FieldValue))
shapiro.test(modsubdataframe$FieldValue)
#Summary of model fit
table(coeff.df$Parameter)
print(paste("Mean R2 for training sets is:", mean(unlist(trainr2))))
print(paste("Mean RMSE for all fold training sets is:", mean(unlist(rmse.train))))
print(paste("Mean predictive R2 is:", mean(unlist(predictiver2))))
print(paste("Mean RMSE for all folds is:", mean(unlist(rmse.test))))

plot(comparetrain$actual,comparetrain$predtrain,main=paste("Predicted vs. Measured for time window:",timewindow))


