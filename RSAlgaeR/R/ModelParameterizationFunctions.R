#' step.model
#'
#' Use stepwise regression to parameterize model
#'
#' @param data dataframe, formatted calibration data (model variables: field data and surface reflectance values)
#' @param imagerydate string, name of column with dates of imagery
#' @param value string, name of column with water quality values
#' @param timewindow numeric, number of days to allow for near coincidence
#' @param season vector, months to include in model
#' @param stepdirection string, direction for stepwise regression ("backward","both","forward")
#' @param print.on boolean, option to print the resutls of the model (default is TRUE)
#' @return list with the stepwise model and the modeled values
#' @import hydroGOF
#' @export




step.model <- function(data,imagerydate,value,timewindow,season,stepdirection,print.on=TRUE){

  modelvariables <- c("Blue","Green","NIR","Red","SWIR1","SWIR2","Green_Blue","Red_Blue","Red_Green",
                   "Red_NIR","Red_SWIR1","Green_SWIR1","Blue_SWIR1","Red_SWIR2","Green_SWIR2","Blue_SWIR2",
                   "NIR_SWIR1","NIR_SWIR2","NIR_Blue","NIR_Green","NDVI","avgRGB","avgSWIR","value")

  data$value <- data[,value]
  #Limit timewindow to certain number of days (max=10)
  datasub <- data[(data$AbsDiffInDays<=timewindow),]
  #Limit based on season
  datasub$Month <- months(as.Date(datasub[,imagerydate]))
  datasubseason <- datasub[(datasub$Month %in% season),]
  #Select model variables
  data <- subset(datasubseason,select=modelvariables)
  #Build model
  mod1 <- glm(value~.,data=data,family=gaussian(link="log"))
  mod0 <- glm(value~1,data=data,family=gaussian(link="log"))
  stepmod <- step(mod0,scope=formula(mod1),trace=0,direction=stepdirection)
  #Calculate Model Results
  predicted <- (stepmod$fitted.values)
  compare <- data.frame(predicted)
  compare$actual <- data$value
  compare$diff <- compare$actual-compare$predicted
  stepmodsummary <- summary(stepmod)
  if(print.on==TRUE){
    #Compute RMSE, R2, and AIC
    print("Full stepwise regression:")
    print(paste("Number of observations for timewindow: ",nrow(data)))
    print(paste("RMSE is: ",sqrt(mean((compare$diff)^2))))
    print(paste("PBIAS is: ", pbias(compare$predicted,compare$actual)))
    print(paste("R2 is: ",summary(lm(compare$actual~compare$predicted))$r.squared))
    print(paste("AIC is: ",stepmodsummary$aic))
    print(stepmodsummary$coefficients)
    plot(compare$predicted~compare$actual, main="Modeled vs. Observed",ylab="Modeled",xlab="Observed")
    abline(lm(compare$predicted~compare$actual), col="black")
  }

  return(list(StepwiseModel=stepmod,StepwiseResults=compare))
}



#' cv.model
#'
#' Use k-fold cross validation to evaluate the goodness of fit for a model
#'
#' @param data dataframe, limited to independent/dependent variables
#' @param value string, name of column with water quality values
#' @param k numeric, number of folds (will not be used if there are fewer observations than folds)
#' @param model formula
#' @param gof string, measure of the goodness of fit (PBIAS, R2, RMSE)
#' @return list of training and testing goodness of fit
#' @import cvTools
#' @import hydroGOF
#' @export

#Calcluate avg R2, RMSE, and Parameter Estimates (coefficients)
cv.model <- function(data,value,k,model,gof){
  data$value <- data[,value]
  formula <- model$formula
  params <- length(model$model)
  #Cross validation (k-fold, if at least k observations in calibration set), LOOCV if <k observations
  folds <- cvTools::cvFolds(n=nrow(data),K=k,R=1,type="random")
  trainmodelsum <- data.frame(R2=rep(NA,k),RMSE=rep(NA,k),PBIAS=rep(NA,k))
  testmodelsum <- data.frame(R2=rep(NA,k),RMSE=rep(NA,k),PBIAS=rep(NA,k))
  for (n in seq(1,k,1)){
    foldindex <- folds$subsets[folds$which==n]
    traindata <- data[-foldindex,]
    testdata <- data[foldindex,]
    trainmod <- glm(formula,data=traindata,family=gaussian(link="log"))
    #Calculate Model Results
    testpredict <- exp(predict(trainmod,newdata=testdata))
    traincompare <- data.frame(predicted=trainmod$fitted.values)
    traincompare$actual <- traindata$value
    traincompare$diff <- traincompare$actual-traincompare$predicted
    testcompare <- data.frame(predicted=testpredict)
    testcompare$actual <- testdata$value
    testcompare$diff <- testcompare$actual-testcompare$predicted
    #Summarize Model Results
    trainmodelsum[n,'R2'] <- summary(lm(traincompare$actual~traincompare$predicted))$r.squared
    trainmodelsum[n,'RMSE'] <- sqrt(mean((traincompare$diff)^2))
    trainmodelsum[n,'PBIAS'] <- pbias(traincompare$predicted,traincompare$actual)
    testmodelsum[n,'R2'] <- summary(lm(testcompare$actual~testcompare$predicted))$r.squared
    testmodelsum[n,'RMSE'] <- sqrt(mean((testcompare$diff)^2))
    testmodelsum[n,'PBIAS'] <- pbias(testcompare$predicted,testcompare$actual)

  }
  if(gof=='R2'){
    trainavgR2 <- mean(trainmodelsum[,'R2'],na.rm=TRUE)
    trainval <- trainavgR2
    testavgR2 <- mean(testmodelsum[,'R2'],na.rm=TRUE)
    testval <- testavgR2
  }
  if(gof=='RMSE'){
    trainavgRMSE <- mean(trainmodelsum[,'RMSE'],na.rm=TRUE)
    trainval <- trainavgRMSE
    testavgRMSE <- mean(testmodelsum[,'RMSE'],na.rm=TRUE)
    testval <- testavgRMSE
  }
  if(gof=='PBIAS'){
    trainavgPBIAS <- mean(trainmodelsum[,'PBIAS'],na.rm=TRUE)
    trainval <- trainavgPBIAS
    testavgPBIAS <- mean(testmodelsum[,'PBIAS'],na.rm=TRUE)
    testval <- testavgPBIAS
  }
  return(list(TrainingVal=trainval,TestingVal=testval))
}

#' modresults
#'
#' Evaluate Model Performance (R2 and RMSE)
#'
#' @param model GLM model
#' @param data data for calibration/evaluation
#' @param value tring, name of column with water quality values
#' @param title string, optional suffix for the title of the plot
#' @import hydroGOF
#' @export


modresults <- function(model, data, value, title=""){
  data$value <- data[,value]
  compare <- data.frame(predicted=model$fitted.values)
  compare$actual <- data$value
  compare$diff <- compare$actual-compare$predicted
  modsummary <- summary(model)
  #Compute RMSE, R2, and AIC
  print("GLM Performance:")
  print(paste("Number of observations: ",nrow(data)))
  print(paste("Range:",min(data$value),"-",max(data$value)))
  print(paste("RMSE is: ",sqrt(mean((compare$diff)^2))))
  print(paste("PBIAS is: ", pbias(compare$predicted,compare$actual)))
  print(paste("R2 is: ",summary(lm(compare$actual~compare$predicted))$r.squared))
  print(paste("AIC is: ",modsummary$aic))
  print(modsummary$coefficients)

  plot(compare$predicted~compare$actual, main=paste("Modeled vs. Observed"," ",title),ylab="Modeled",xlab="Observed")
  abline(lm(compare$predicted~compare$actual), col="black")
  abline(0,1,col="red",lty=2)
}


