#' Explore long term annual trends with Theil-Sen Estimator
#'
#' Calculates annual linear trend of average values and significance of  with Theil-Sen Estimator (used for robust to non-normal data)
#'
#' @param record dataframe with record of estimated water quality
#' @param valuecol string, name of column with water quality values
#' @param datecol string, name of column with dates
#' @param var string, aggregator (e.g. mean, max)
#' @param monthlybias, calculates annual average using monthly averages (in case of differing numbers of samples for each month)
#' @return summary of the Theil-Sen estimator
#' @import lubridate
#' @import mblm
#' @import stats
#' @examples
#' data(estimatedrecord)
#' annualtrend.ts(record=estimatedrecord,valuecol="EstChlValue",
#' datecol="ImageDate",var="mean",monthlybias="TRUE")
#' @export


annualtrend.ts <- function(record,valuecol,datecol,var,monthlybias=FALSE){
  record$Date <- record[,datecol]
  record$value <- record[,valuecol]
  if(monthlybias==TRUE){
    record$year.norm <- lubridate::year(record$Date)-min(lubridate::year(record$Date))
    record$Month <- lubridate::month(record$Date)
    yearlymonthlyrecord <- stats::aggregate(data=record,value~year.norm+Month, var, na.rm=TRUE)
    yearlyrecord <- stats::aggregate(data=yearlymonthlyrecord,value~year.norm,FUN=var)
  }else{
    record$year.norm <- lubridate::year(record$Date)-min(lubridate::year(record$Date))
    yearlyrecord <- stats::aggregate(data=record,value~year.norm,FUN=var)
  }
  fit <- with(yearlyrecord,mblm::mblm(value~year.norm))
  summary(fit)
}


#' Explore long term monthly trends with Theil-Sen Estimator
#'
#' Calculates annual linear trend of average monthly values and significance of  with Theil-Sen Estimator (used for robust to non-normal data)
#'
#' @param record dataframe with record of estimated water quality
#' @param valuecol string, name of column with values
#' @param datecol string, name of column with dates
#' @param months list of months
#' @param var string, aggregator (e.g. mean, max)
#' @return summary of the Theil-Sen estimator
#' @import lubridate
#' @import mblm
#' @import stats
#' @export

monthlytrend.ts <- function(record,valuecol,datecol,months,var){
  monthlyresults <- data.frame(Month=months,Trend=rep(NA,length(months)),PValue=rep(NA,length(months)))
  record$value <- record[,valuecol]
  record$Date <- record[,datecol]
  for(i in seq(1,length(months),1)){
    record.sub <- record[(record$Month==months[i]),]
    record.sub$year.norm <- lubridate::year(record.sub$Date)-min(lubridate::year(record.sub$Date))
    yearlyrecord <- stats::aggregate(data=record.sub,value~year.norm,FUN=var)
    fit <- summary(with(yearlyrecord,mblm::mblm(value~year.norm)))
    monthlyresults[i,"Trend"] <- fit$coefficients[2,1]
    monthlyresults[i,"PValue"] <- fit$coefficients[2,4]
  }

  return(monthlyresults)
}


