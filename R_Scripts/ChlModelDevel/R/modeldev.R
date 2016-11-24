#'Model Development
#'
#'Performs a stepwise Regression and regression subset for a given calibration dataset
#'@param data Dataframe of reflectance data and field data
#'@param season Vector of months included in seasonal model
#'@return Summary of stepwise regression results and plots of parameters that produce the best R2 and BIC
#'@export
#Stepwise Model------------------------------------------------------------------------
modeldev=function(data,season){
  library(leaps)
  data=data[(is.element(data$Month,season)==TRUE),]
  data=data[,c(5,6:28)]
  #Stepwise Regression
  mod1=glm(FieldValue~.,data=data,family=gaussian(link="log"))
  mod0=glm(FieldValue~1,data=data,family=gaussian(link="log"))
  mod=step(mod0,scope=formula(mod1),trace=0)
  print(summary(mod))
  #Regression Subset
  models=regsubsets(log(FieldValue)~.,data=data)
  p1=plot(models,scale="adjr2",main=paste('Regression Subset for ',toString(season)))
  p2=plot(models,scale="bic",main=paste('Regression Subset for ',toString(season)))
  return(list(p1,p2))
}
