#' step.model
#'
#' Use stepwise regression to parameterize model
#'
#' @param data dataframe, formatted calibration data (model variables: field data and surface reflectance values)
#' @param imagerydate string, name of column with dates of imagery
#' @param value string, name of column with water quality values
#' @param modelvariables vector of strings with the names of columns for bands to consider
#' @param timewindow numeric, number of days to allow for near coincidence
#' @param season vector, months to include in model
#' @param stepdirection string, direction for stepwise regression ("backward","both","forward")
#' @param print.on boolean, option to print the resutls of the model (default is TRUE)
#' @return list with the stepwise model and the modeled values
#' @import hydroGOF
#' @export




step.model <- function(data,imagerydate,value,modelvariables,timewindow,season,stepdirection,print.on=TRUE){

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
