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

