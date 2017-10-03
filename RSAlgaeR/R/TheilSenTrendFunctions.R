#' Explore long term annual trends with Theil-Sen Estimator
#'
#' Calculates annual linear trend of average values and significance of  with Theil-Sen Estimator (used for robust to non-normal data)
#'
#' @param record dataframe with record of estimated water quality
#' @param value string, name of column with water quality values
#' @param date string, name of column with dates
#' @param var string, aggregator (e.g. mean, max)
#' @import lubridate
#' @import mblm
#' @export


annualtrend.ts <- function(record,value,date,var){
  record$year.norm <- year(record[,date])-min(year(record[,date]))
  record$value <- record[,value]
  yearlyrecord <- aggregate(data=record,value~year.norm,FUN=var)
  fit <- with(yearlyrecord,mblm(value~year.norm))
  summary(fit)
}


#' Explore long term monthly trends with Theil-Sen Estimator
#'
#' Calculates annual linear trend of average monthly values and significance of  with Theil-Sen Estimator (used for robust to non-normal data)
#'
#' @param record dataframe with record of estimated water quality
#' @param value string, name of column with values
#' @param date string, name of column with dates
#' @param months list of months
#' @param var string, aggregator (e.g. mean, max)
#' @import lubridate
#' @import mblm
#' @export

monthlytrend.ts <- function(record,value,date,months,var){
  monthlyresults <- data.frame(Month=months,Trend=rep(NA,length(months)),PValue=rep(NA,length(months)))
  record$value <- record[,value]
  for(i in seq(1,length(months),1)){
    record.sub <- record[(record$Month==months[i]),]
    record.sub$year.norm <- year(record.sub[,date])-min(year(record.sub[,date]))
    yearlyrecord <- aggregate(data=record.sub,value~year.norm,FUN=var)
    fit <- summary(with(yearlyrecord,mblm(value~year.norm)))
    monthlyresults[i,"Trend"] <- fit$coefficients[2,1]
    monthlyresults[i,"PValue"] <- fit$coefficients[2,4]
  }

  return(monthlyresults)
}
