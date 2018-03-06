#' summarize water quality conditions on an annual basis
#'
#'
#' @param data  with estimated values, dates, location identifiers
#' @param value string, name of column with water quality values
#' @param date string, name of column with dates
#' @param location string, name of column with location identifiers
#' @import plyr
#' @import lubridate
#' @export

annual.summary.wq <- function(data,date,value,location){
  data$date <- data[,date]
  data$value <- data[,value]
  data$location <- data[,location]
  data$Year <- (year(data$date))

  data$Year <- as.factor(data$Year)
  annualmaxdata <- ddply(data,c('Year'),function(x) x[which(x$value==max(x$value)),])
  annualmax <- data.frame(DOYmax=yday(annualmaxdata$date),
                          Year=year(annualmaxdata$date),
                          MaxValue=annualmaxdata$value,
                          LocationID=as.factor(annualmaxdata$location))
  annualmean <- ddply(data,c('Year'),function(x) mean(x$value))
  colnames(annualmean) <- c("Year","MeanValue")
  summarydata <- merge(annualmax,annualmean,by="Year")
  return(summarydata)
}



#' summarize climate conditions on an annual basis
#'
#'
#' @param data  with estimated values, dates, location identifiers
#' @param value string, name of column with climate parameter values
#' @param date string, name of column with dates
#' @param parameter string, name of parameter ("Precipitation","Temperature")
#' @import plyr
#' @import lubridate
#' @export

annual.summary.climate <- function(data,date,value,parameter){
  data$date <- data[,date]
  data$value <- data[,value]
  data$Year <- as.factor(year(data$date))
  data$Month <- as.factor(month(data$date))

  if(parameter=="Precipitation"){
    data <- data[(data$value<0),]
    janfebprecip <- ddply(data[(data$Month %in% c(1,2)),],c('Year'),function(x) sum(x$value))
    janfebprecip$Year <- as.numeric(levels(factor(janfebprecip$Year)))
    decprecip <- ddply(data[(data$Month==12),],c('Year'),function(x) sum(x$value))
    janfebprecip <- janfebprecip[-1,]
    winterprecipsum <- data.frame(TotalPrecip=janfebprecip$V1+decprecip$V1)
    winterprecipsum$Year <- janfebprecip$Year

    springprecipsum <- ddply(data[(data$Month %in% c(3,4,5,6)),],c('Year'),function(x) sum(x$value))
    springprecipcount <- ddply(data[(data$Month %in% c(3,4,5,6)),],c('Year'),function(x) sum(x$value>0))
    colnames(winterprecipsum) <- c("Year","winterTotalPrecip")
    colnames(springprecipsum) <- c("Year","springTotalPrecip")
    colnames(springprecipcount) <- c("Year","springCountPrecip")
    avgtotalwinterprecip <- mean(winterprecipsum$winterTotalPrecip)
    avgtotalspringprecip <- mean(springprecipsum$springTotalPrecip)
    avgspringprecipcount <- mean(springprecipcount$CountPrecip)
    return(list(winterprecipsum,springprecipsum,springprecipcount,avgWinterPrecip=avgtotalwinterprecip,avgSpringPrecip=avgtotalspringprecip,avgNumSpringPrecip=avgspringprecipcount))
  }else if(parameter=="Temperature"){
    springtemp <- ddply(data[(data$Month %in% c(3,4,5,6)),],c('Year'),function(x) mean(x$value))
    summertemp <- ddply(data[(data$Month %in% c(7,8,9)),],c('Year'),function(x) mean(x$value))
    colnames(springtemp) <- c("Year","MeanTemp")
    colnames(summertemp) <- c("Year","MeanTemp")
    avgspringtemp <- mean(springtemp$MeanTemp)
    avgsummertemp <- mean(summertemp$MeanTemp)
    return(list(avgspringtemp=avgspringtemp,avgsummertemp=avgsummertemp))
  }
}
