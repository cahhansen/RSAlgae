#' summarize water quality conditions on an annual basis
#'
#'
#' @param data  data frame with estimated values, dates, location identifiers
#' @param value string, name of column with water quality values
#' @param date string, name of column with dates
#' @param location string, name of column with location identifiers
#' @return dataframe of annual summaries
#' @importFrom plyr ddply
#' @importFrom lubridate year
#' @export

annual.summary.wq <- function(data,value,date,location){
  data$date <- df[,date]
  data$value <- df[,value]
  data$location <- data[,location]
  data$Year <- (lubridate::year(data$date))

  data$Year <- as.factor(data$Year)
  annualmaxdata <- plyr::ddply(data,c('Year'),function(x) x[which(x$value==max(x$value)),])
  annualmax <- data.frame(DOYmax=yday(annualmaxdata$date),
                          Year=year(annualmaxdata$date),
                          MaxValue=annualmaxdata$value,
                          LocationID=as.factor(annualmaxdata$location))
  annualmean <- plyr::ddply(data,c('Year'),function(x) mean(x$value))
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
#' @return list of annual (seasonal) summaries
#' @importFrom plyr ddply
#' @importFrom lubridate year
#' @export

annual.summary.climate <- function(data,date,value,parameter){
  data$date <- data[,date]
  data$value <- data[,value]
  data$Year <- as.factor(lubridate::year(data$date))
  data$Month <- as.factor(lubridate::month(data$date))

  if(parameter=="Precipitation"){
    data <- data[(data$value>=0),]
    janfebprecip <- plyr::ddply(data[(data$Month %in% c(1,2)),],c('Year'),function(x) sum(x$value))
    janfebprecip$Year <- as.numeric(levels(factor(janfebprecip$Year)))
    decprecip <- plyr::ddply(data[(data$Month==12),],c('Year'),function(x) sum(x$value))
    janfebprecip <- janfebprecip[-1,]
    winterprecipsum <- data.frame(TotalPrecip=janfebprecip$V1+decprecip$V1)
    winterprecipsum$Year <- janfebprecip$Year

    springprecipsum <- plyr::ddply(data[(data$Month %in% c(3,4,5,6)),],c('Year'),function(x) sum(x$value))
    springprecipcount <- plyr::ddply(data[(data$Month %in% c(3,4,5,6)),],c('Year'),function(x) sum(x$value>0))
    colnames(winterprecipsum) <- c("Year","winterTotalPrecip")
    colnames(springprecipsum) <- c("Year","springTotalPrecip")
    colnames(springprecipcount) <- c("Year","springCountPrecip")
    avgtotalwinterprecip <- mean(winterprecipsum$winterTotalPrecip)
    avgtotalspringprecip <- mean(springprecipsum$springTotalPrecip)
    avgspringprecipcount <- mean(springprecipcount$springCountPrecip)
    return(list(winterprecipsum,springprecipsum,springprecipcount,avgWinterPrecip=avgtotalwinterprecip,avgSpringPrecip=avgtotalspringprecip,avgNumSpringPrecip=avgspringprecipcount))
  }else if(parameter=="Temperature"){
    springtemp <- plyr::ddply(data[(data$Month %in% c(3,4,5,6)),],c('Year'),function(x) mean(x$value))
    summertemp <- plyr::ddply(data[(data$Month %in% c(7,8,9)),],c('Year'),function(x) mean(x$value))
    colnames(springtemp) <- c("Year","MeanTemp")
    colnames(summertemp) <- c("Year","MeanTemp")
    avgspringtemp <- mean(springtemp$MeanTemp)
    avgsummertemp <- mean(summertemp$MeanTemp)
    return(list(avgspringtemp=avgspringtemp,avgsummertemp=avgsummertemp))
  }
}
