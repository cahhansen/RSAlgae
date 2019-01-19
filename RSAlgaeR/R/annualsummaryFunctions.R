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
#' @param season vector, names of months to include in analysis
#' @return list of annual (seasonal) summaries
#' @importFrom plyr ddply
#' @importFrom lubridate year month
#' @examples
#' data(climatedata)
#' sumdata <- annual.summary.climate(df=climatedata,valuecol="PRCP",datecol="DATE",
#' parameter="Precipitation",season=c("January","February"))
#' @export

annual.summary.climate <- function(df,datecol,valuecol,parameter,season){
  #Format data frame
  df$Date <- df[,datecol]
  df$Value <- df[,valuecol]
  df$Month <- as.factor(months(df$Date))
  df$Year <- as.factor(lubridate::year(df$Date))

  df <- df[,c("Date","Value","Month","Year")]
  df <- df[(df$Value>=0),]

  if(parameter=="Precipitation"){
    if("December" %in% season){
      decprecip <- plyr::ddply(df[(df$Month=='December'),],c('Year'),function(x) sum(x$Value))
      decprecip$Year <- as.numeric(levels(factor(decprecip$Year)))
      decprecip$Year <- decprecip$Year+1
      decprecipcount <- plyr::ddply(df[(df$Month=='December'),],c('Year'),function(x) sum(x$Value>0))
      decprecipcount$Year <- as.numeric(levels(factor(decprecipcount$Year)))
      decprecipcount$Year <- decprecipcount$Year+1

      subseason <- season [! season %in% "December"]
      precipsub <- plyr::ddply(df[(df$Month %in% subseason),],c('Year'),function(x) sum(x$Value))
      precipsub$Year <- as.numeric(levels(factor(precipsub$Year)))
      precipsubcount <- plyr::ddply(df[(df$Month %in% season),],c('Year'),function(x) sum(x$Value>0))

      precipsum <- merge(decprecip,precipsub,by="Year")
      precipsum$TotalPrecip <- precipsum$V1.x+precipsum$V1.y
      precipsum <- precipsum[,c("Year","TotalPrecip")]

      precipcount <- merge(decprecipcount,precipsubcount,by="Year")
      precipcount$CountPrecip <- precipcount$V1.x+precipcount$V1.y
      precipcount <- precipcount[,c("Year","CountPrecip")]

    }else{
      precipsum <- plyr::ddply(df[(df$Month %in% season),],c('Year'),function(x) sum(x$Value))
      precipsum$Year <- as.numeric(levels(factor(precipsum$Year)))
      precipsum$TotalPrecip <- precipsum$V1
      precipsum <- precipsum[,c("Year","TotalPrecip")]

      precipcount <- plyr::ddply(df[(df$Month %in% season),],c('Year'),function(x) sum(x$Value>0))
      precipcount$CountPrecip <- precipcount$V1
      precipcount <- precipcount[,c("Year","CountPrecip")]
    }

    return(list(avgPrecip=mean(precipsum$TotalPrecip),
                avgPrecipCount=mean(precipcount$CountPrecip),
                annualprecipsum=precipsum,
                annualprecipcount=precipcount))

  }else if(parameter=="Temperature"){
    tempsub <- plyr::ddply(df[(df$Month %in% season),],c('Year'),function(x) mean(x$Value))
    colnames(tempsub) <- c("Year","MeanTemp")
    avgtemp <- mean(tempsub$MeanTemp)
    return(list(avgTemp=avgtemp,
                annualavgTemp=tempsub))
  }
}
