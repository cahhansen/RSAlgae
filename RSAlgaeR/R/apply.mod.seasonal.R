#' apply.mod.seasonal
#'
#' Apply GLM to remotely sensed record
#'
#' @param data dataframe with reflectance values
#' @param date string, name of column with imagery dates
#' @param model calibrated GLM
#' @param season vector of months to include in the season
#' @param threshold numeric value above which is considered unreasonable/noise
#' @return dataframe of predicted values and confidence intervals
#' @export


apply.mod.seasonal <- function(data, date, model, season, threshold){
  data$ImageDate <- as.Date(data[,date])
  data$Month <- months(data$ImageDate)
  datasub <- data[(data$Month %in% season),]
  subpredicted <- predict(model,newdata=datasub,type='response',se.fit=T)
  datasub$value <- subpredicted$fit
  datasub$lower <- datasub$value-1.96*subpredicted$se.fit
  datasub$upper <- datasub$value+1.96*subpredicted$se.fit
  datasub <- datasub[(datasub$value<threshold),]
  return(datasub)
}

