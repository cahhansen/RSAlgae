#' modresults
#'
#' Evaluate Model Performance (R2 and RMSE)
#'
#' @param model GLM model
#' @param data data for calibration/evaluation
#' @param value string, name of column with water quality values
#' @param title string, optional suffix for the title of the plot
#' @return prints summary of model and plot of modeled vs. observed
#' @import hydroGOF
#' @import graphics
#' @export


modresults <- function(model, data, value, title=""){
  data$value <- data[[,value]]
  compare <- data.frame(predicted=model$fitted.values)
  compare$actual <- data$value
  compare$diff <- compare$actual-compare$predicted
  modsummary <- summary(model)
  #Compute RMSE, PBIAS R2, and AIC
  print("GLM Performance:")
  print(paste("Number of observations: ",nrow(data)))
  print(paste("Range:",min(data$value),"-",max(data$value)))
  print(paste("RMSE is: ",sqrt(mean((compare$diff)^2))))
  print(paste("PBIAS is: ", hydroGOF::pbias(compare$predicted,compare$actual)))
  print(paste("R2 is: ",summary(lm(compare$actual~compare$predicted))$r.squared))
  print(paste("AIC is: ",modsummary$aic))
  print(modsummary$coefficients)

  plot(compare$predicted~compare$actual, main=paste("Modeled vs. Observed"," ",title),ylab="Modeled",xlab="Observed")
  abline(lm(compare$predicted~compare$actual), col="black")
  abline(0,1,col="red",lty=2)
}


