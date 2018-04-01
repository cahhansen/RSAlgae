#' summarize max and mean water quality conditions on an annual basis
#'
#'
#' @param df  data frame with estimated values, dates, location identifiers
#' @param valuecol string, name of column with water quality values
#' @param datecol string, name of column with dates (must be date format)
#' @param locationcol string, name of column with location identifiers
#' @return dataframe of annual summaries
#' @importFrom plyr ddply
#' @importFrom lubridate year yday
#' @import mblm
#' @import ggplot2
#' @examples
#' data(estimatedrecord)
#' sumdata <- annual.summary.wq(df=estimatedrecord,valuecol="EstChlValue",datecol="ImageDate",
#' locationcol="StationID")
#' @export

annual.summary.wq <- function(df,valuecol,datecol,locationcol){
  #Format data frame
  df$Date <- df[,datecol]
  df$Value <- df[,valuecol]
  df$Location <- df[,locationcol]
  df$Year <- as.factor(lubridate::year(df$Date))

  df <- df[,c("Date","Value","Location","Year")]

  #Calculate max and mean by year
  annualmaxdata <- plyr::ddply(df,c('Year'),function(x) x[which(x$Value==max(x$Value)),])
  annualmax <- data.frame(DOYmax=lubridate::yday(annualmaxdata$Date),
                          Year=lubridate::year(annualmaxdata$Date),
                          MaxValue=annualmaxdata$Value,
                          LocationID=as.factor(annualmaxdata$Location))
  annualmean <- plyr::ddply(df,c('Year'),function(x) mean(x$Value))
  colnames(annualmean) <- c("Year","MeanValue")
  summarydata <- merge(annualmax,annualmean,by="Year")

  #Calculate trend in maximum timing
  annualmax$Yearnorm <- annualmax$Year-min(annualmax$Year)
  fit <- with(annualmax,mblm::mblm(DOYmax~Yearnorm))

  #Plot doy of max vs. year
  doyplot <- ggplot2::ggplot(annualmax,aes(x=Yearnorm,y=DOYmax))+
    geom_point(aes(x=Yearnorm,y=DOYmax))+
    geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], col='red')+
    theme_bw()+
    ylab("Day of Year")+
    xlab("Year in record")+
    ggtitle(paste("Timing of Maximum Chl-Levels ",min(annualmax$Year),"-",max(annualmax$Year)))+
    theme(legend.position="none")

  return(list(summarydata,summary(fit),doyplot))
}



#' summarize climate conditions on an annual basis
#'
#'
#' @param df  with estimated values, dates, location identifiers
#' @param valuecol string, name of column with climate parameter values
#' @param datecol string, name of column with dates
#' @param parameter string, name of parameter ("Precipitation","Temperature")
#' @return list of annual (seasonal) summaries
#' @importFrom plyr ddply
#' @importFrom lubridate year month
#' @examples
#' data(climatedata)
#' sumdata <- annual.summary.climate(df=climatedata,valuecol="PRCP",datecol="DATE",
#' parameter="Precipitation")
#' @export

annual.summary.climate <- function(df,datecol,valuecol,parameter){
  #Format data frame
  df$Date <- df[,datecol]
  df$Value <- df[,valuecol]
  df$Month <- as.factor(lubridate::month(df$Date))
  df$Year <- as.factor(lubridate::year(df$Date))

  df <- df[,c("Date","Value","Month","Year")]
  df <- df[(df$Value>=0),]

  if(parameter=="Precipitation"){
    janfebprecip <- plyr::ddply(df[(df$Month %in% c(1,2)),],c('Year'),function(x) sum(x$Value))
    janfebprecip$Year <- as.numeric(levels(factor(janfebprecip$Year)))
    janfebprecipcount <- plyr::ddply(df[(df$Month %in% c(1,2)),],c('Year'),function(x) sum(x$Value>0))
    decprecip <- plyr::ddply(df[(df$Month==12),],c('Year'),function(x) sum(x$Value))
    decprecip$Year <- as.numeric(levels(factor(decprecip$Year)))
    decprecip$Year <- decprecip$Year+1
    decprecipcount <- plyr::ddply(df[(df$Month %in% c(12)),],c('Year'),function(x) sum(x$Value>0))
    decprecipcount$Year <- as.numeric(levels(factor(decprecipcount$Year)))
    decprecipcount$Year <- decprecipcount$Year+1

    winterprecipsum <- merge(decprecip,janfebprecip,by="Year")
    winterprecipcount <- merge(decprecipcount,janfebprecipcount,by="Year")
    winterprecipcount$winterCountPrecip <- winterprecipcount$V1.x+winterprecipcount$V1.y
    winterprecipsum$winterTotalPrecip <- winterprecipsum$V1.x+winterprecipsum$V1.y
    winterprecipsum <- winterprecipsum[,c("Year","winterTotalPrecip")]
    winterprecipcount <- winterprecipcount[,c("Year","winterCountPrecip")]
    springprecipcount <- plyr::ddply(df[(df$Month %in% c(3,4,5,6)),],c('Year'),function(x) sum(x$Value>0))

    springprecipsum <- plyr::ddply(df[(df$Month %in% c(3,4,5,6)),],c('Year'),function(x) sum(x$Value))
    springprecipcount <- plyr::ddply(df[(df$Month %in% c(3,4,5,6)),],c('Year'),function(x) sum(x$Value>0))
    springprecipsum$Year <- as.numeric(levels(factor(springprecipsum$Year)))
    colnames(springprecipsum) <- c("Year","springTotalPrecip")
    colnames(springprecipcount) <- c("Year","springCountPrecip")

    precipsummarydf <- merge(winterprecipsum,springprecipsum,by="Year")
    precipsummarydf <- merge(precipsummarydf,winterprecipcount,by="Year")
    precipsummarydf <- merge(precipsummarydf,springprecipcount,by="Year")
    avgtotalwinterprecip <- mean(winterprecipsum$winterTotalPrecip)
    avgtotalspringprecip <- mean(springprecipsum$springTotalPrecip)
    avgspringprecipcount <- mean(springprecipcount$springCountPrecip)
    return(list(precipsummarydf,avgWinterPrecip=avgtotalwinterprecip,avgSpringPrecip=avgtotalspringprecip,avgNumSpringPrecip=avgspringprecipcount))
  }else if(parameter=="Temperature"){
    springtemp <- plyr::ddply(df[(df$Month %in% c(3,4,5,6)),],c('Year'),function(x) mean(x$Value))
    summertemp <- plyr::ddply(df[(df$Month %in% c(7,8,9)),],c('Year'),function(x) mean(x$Value))
    colnames(springtemp) <- c("Year","MeanSpringTemp")
    colnames(summertemp) <- c("Year","MeanSummerTemp")
    avgspringtemp <- mean(springtemp$MeanSpringTemp)
    avgsummertemp <- mean(summertemp$MeanSummerTemp)
    tempsummarydf <- merge(springtemp,summertemp,by="Year")
    return(list(tempsummarydf,avgspringtemp=avgspringtemp,avgsummertemp=avgsummertemp))
  }
}
