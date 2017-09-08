#' Perform Stepwise Regression
#'
#' Use stepwise regression to parameterize initial model
#'
#' @param caldata .Rda with field data and surface reflectance values
#' @param timewindow Number of days to allow for near coincidence
#' @param season Vector of months to include in model
#' @param stepdirection Direction for stepwise regression ("backward","both","forward")
#' @param print.on option to print the resutls of the model (default is TRUE)
#' @return list with the stepwise model and the modeled values
#' @import hydroGOF
#' @export




stepChlModel <- function(caldata,timewindow,season,stepdirection,print.on=TRUE){

  modelvariables <- c("Blue","Green","NIR","Red","SWIR1","SWIR2","Green_Blue","Red_Blue","Red_Green",
                   "Red_NIR","Red_SWIR1","Green_SWIR1","Blue_SWIR1","Red_SWIR2","Green_SWIR2","Blue_SWIR2",
                   "NIR_SWIR1","NIR_SWIR2","NIR_Blue","NIR_Green","NDVI","avgRGB","avgSWIR","FieldValue")

  #Limit timewindow to certain number of days (max=10)
  caldatasub <- caldata[(caldata$AbsDiffInDays<=timewindow),]
  #Limit based on season
  caldatasub$Month <- months(as.Date(caldatasub$ImageDate))
  caldatasubseason <- caldatasub[(caldatasub$Month %in% season),]
  #Select model variables
  caldf <- subset(caldatasubseason,select=modelvariables)
  #Build model
  mod1 <- glm(FieldValue~.,data=caldf,family=gaussian(link="log"))
  mod0 <- glm(FieldValue~1,data=caldf,family=gaussian(link="log"))
  stepmod <- step(mod0,scope=formula(mod1),trace=0,direction=stepdirection)
  #Calculate Model Results
  predicted <- (stepmod$fitted.values)
  compare <- data.frame(predicted)
  compare$actual <- caldf$FieldValue
  compare$diff <- compare$actual-compare$predicted
  stepmodsummary <- summary(stepmod)
  if(print.on==TRUE){
    #Compute RMSE, R2, and AIC
    print("Full stepwise regression:")
    print(paste("Number of observations for timewindow: ",nrow(caldf)))
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


#' Limit Parameters
#'
#' Create formula limited to parameters above a confidence level
#'
#' @param form Formula
#' @param pval P-value (confidence level)
#' @return formula with significant parameters
#' @export


#Create model limited to statistically significant parameters
limitparams <- function(form,pval){
  #Limit to coefficients with p-value <0.05
  formdf <- data.frame(summary(form)$coefficients)
  formdf$params <- names(form$coefficients)
  finalparams <- formdf$params[stepmoddf$Pr...t..<=pval]
  limitedform <- as.formula(paste("FieldValue ~ ",paste(finalparams[-1],collapse="+")))
  return(limitedform)
}


#' Cross Validation
#'
#' Use k-fold or LOOCV to evaluate the R2, RMSE, and parameter estimates for a model
#'
#' @param caldf dataframe limited to independent/dependent variables
#' @param k Number of folds (will not be used if there are fewer observations than folds)
#' @param model formula
#' @param gof character string, measure of the goodness of fit (PBIAS, R2, RMSE)
#' @return parameter estimates for specified model formula
#' @import cvTools
#' @import hydroGOF
#' @export

#Calcluate avg R2, RMSE, and Parameter Estimates (coefficients)
cvChlModel <- function(caldf,k,model,gof){
  formula <- model$formula
  params <- length(model$model)
  #Cross validation (k-fold, if at least k observations in calibration set), LOOCV if <k observations
  folds <- cvTools::cvFolds(n=nrow(data),K=k,R=1,type="random")
  trainmodelsum <- data.frame(R2=rep(NA,k),RMSE=rep(NA,k),PBIAS=rep(NA,k))
  testmodelsum <- data.frame(R2=rep(NA,k),RMSE=rep(NA,k),PBIAS=rep(NA,k))
  for (n in seq(1,k,1)){
    foldindex <- folds$subsets[folds$which==n]
    traindata <- caldf[-foldindex,]
    testdata <- caldf[foldindex,]
    trainmod <- glm(formula,data=traindata,family=gaussian(link="log"))
    #Calculate Model Results
    testpredict <- exp(predict(trainmod,newdata=testdata))
    traincompare <- data.frame(predicted=trainmod$fitted.values)
    traincompare$actual <- traindata$FieldValue
    traincompare$diff <- traincompare$actual-traincompare$predicted
    testcompare <- data.frame(predicted=testpredict)
    testcompare$actual <- testdata$FieldValue
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
    val1 <- trainavgR2
    testavgR2 <- mean(testmodelsum[,'R2'],na.rm=TRUE)
    val2 <- testavgR2
  }
  if(gof=='RMSE'){
    trainavgRMSE <- mean(trainmodelsum[,'RMSE'],na.rm=TRUE)
    val1 <- trainavgRMSE
    testavgRMSE <- mean(testmodelsum[,'RMSE'],na.rm=TRUE)
    val2 <- testavgRMSE
  }
  if(gof=='PBIAS'){
    trainavgPBIAS <- mean(trainmodelsum[,'PBIAS'],na.rm=TRUE)
    val1 <- trainavgPBIAS
    testavgPBIAS <- mean(testmodelsum[,'PBIAS'],na.rm=TRUE)
    val2 <- testavgPBIAS
  }
  return(list(val1,val2))
}

#' Model Results
#'
#' Evaluate Model Performance (R2 and RMSE)
#'
#' @param model GLM model
#' @param data data for calibration/evaluation
#' @param title character string, suffix for the title of the plot
#' @import hydroGOF
#' @export


modresults <- function(model, data, title){
  compare <- data.frame(predicted=model$fitted.values)
  compare$actual <- data$FieldValue
  compare$diff <- compare$actual-compare$predicted
  modsummary <- summary(model)
  #Compute RMSE, R2, and AIC
  print("GLM Performance:")
  print(paste("Number of observations: ",nrow(data)))
  print(paste("Range:",min(data$FieldValue),"-",max(data$FieldValue)))
  print(paste("RMSE is: ",sqrt(mean((compare$diff)^2))))
  print(paste("PBIAS is: ", pbias(compare$predicted,compare$actual)))
  print(paste("R2 is: ",summary(lm(compare$actual~compare$predicted))$r.squared))
  print(paste("AIC is: ",modsummary$aic))
  print(modsummary$coefficients)

  plot(compare$predicted~compare$actual, main=paste("Modeled vs. Observed",": ",title),ylab="Modeled",xlab="Observed")
  abline(lm(compare$predicted~compare$actual), col="black")
  abline(0,1,col="red",lty=2)
}


