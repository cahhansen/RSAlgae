#' Perform Stepwise Regression
#'
#' Use stepwise regression to parameterize initial model
#'
#' @param caldata .Rda with field data and surface reflectance values
#' @param timewindow Number of days to allow for near coincidence
#' @param season Vector of months to include in model
#' @param stepdirection Direction for stepwise regression ("backward","both","forward")
#' @return list with the stepwise model and the modeled values
#' @export
#'



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
    print(paste("RMSE is: ",sqrt(mean(compare$diff)^2)))
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
#'

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
#' @param form Formula for GLM
#' @param params Number of independent variables in the specified formula
#' @return parameter estimates for specified model formula
#' @export
#'
#Calcluate avg R2, RMSE, and Parameter Estimates (coefficients)
cvChlModel <- function(caldf,k,form,params){
  #Cross validation (k-fold, if at least k observations in calibration set), LOOCV if <k observations
  caldf$id <- sample(1:k, nrow(caldf), replace = FALSE)
  coeffsum <- data.frame(matrix(nrow=length(params+1),ncol=length(unique(caldf$id))))
  pvaluesum <- data.frame(matrix(nrow=length(params+1),ncol=length(unique(caldf$id))))
  modelsum <- data.frame(matrix(nrow=length(unique(caldf$id)),ncol=2))
  for (n in unique(caldf$id)){
    traindata <- caldf[(caldf$id!=n),]
    traindata <- subset(traindata,select=-c(id))
    trainmod <- glm(form,data=traindata,family=gaussian(link="log"))

    #Calculate Model Results
    kpredicted <- (trainmod$fitted.values)
    kcompare <- data.frame(predicted)
    kcompare$actual <- traindata$FieldValue
    kcompare$diff <- kcompare$actual-kcompare$predicted
    #Summarize Model Results
    coeffsum[,n] <- trainmod$coefficients
    pvaluesum[,n] <- summary(trainmod)$coefficients[,4]
    modelsum[n,1] <- summary(lm(kcompare$actual~kcompare$predicted))$r.squared
    modelsum[n,2] <- sqrt(mean(kcompare$diff)^2)
  }
  avgR2 <- mean(modelsum[,1],na.rm=TRUE)
  minR2 <- min(modelsum[,1])
  maxR2 <- max(modelsum[,1])
  print(paste("Avg R2 for all folds: ",avgR2))
  print(paste("Summary of R2",minR2,"-",maxR2))
  avgRMSE <- mean(modelsum[,2],na.rm=TRUE)
  minRMSE <- min(modelsum[,2])
  maxRMSE <- max(modelsum[,2])
  print(paste("Avg RMSE for all folds: ",avgRMSE))
  print(paste("Summary of RMSE",minRMSE,"-",maxRMSE))

  return(rowMeans(coeffsum))
}

